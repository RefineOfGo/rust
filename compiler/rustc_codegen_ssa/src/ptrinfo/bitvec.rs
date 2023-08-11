use std::ops::BitOr;

use smallvec::SmallVec;

use super::{Enum, Niche};

/// First-byte encodes pointer info for simple types or `sizeof(type)` for
/// more complex types:
///     0nnn_nnnn   Varint size `nnn_nnnn` (in bytes) of types that have no
///                 pointers.
///     10ww_pppp   Types which contains at most 4 pointer slots and is pointer
///                 size aligned, it's size is encoded as `(ww + 1) * sizeof(pointer)`.
///                 `pppp` indicates which slot is a pointer. `pppp` == 0 is
///                 invalid in this encoding.
///     11nn_nnnn   Varint size `nn_nnnn` (in bytes) of types that cannot be
///                 encoded with the methods above. Right after this byte are
///                 all the components encoded back to back.
///
/// Encoding for each component:
///     0xxx_xxxx   Raw bitmap with at most 7 bits
///     10xx_xxxx   Varint + 1 lots of int bits
///     110x_xxxx   Varint + 1 lots of ptr bits
///     1110_xxxx   Varint + 1 lots of enum cases
///     1111_xxxx   Varint + 1 lots of niche cases
///
/// Direct enums are encoded as follows:
///     Enum {
///         header   : Identifier described as above (1110_xxxx).
///         tag_offs : Varint, offset to the tag field.
///         tag_size : Varint, size of the tag field.
///         variants ...
///     }
///
/// Niche enums are encoded as follows:
///     Niche {
///         header            : Identifier described as above (1110_xxxx).
///         tag_offs          : Varint, offset to the tag field.
///         tag_size          : Varint, size of the tag field.
///         niche_start       : Varint, offset of the tag value.
///         niche_range_start : Varint, start of the niche value, inclusive.
///         niche_range_end   : Varint, start of the niche value, inclusive.
///         untagged_variant  : Varint, discriminant value of the untagged niche.
///         variants ...
///     }
///
/// A general algorithm to extract the discriminant from the tag is:
///     relative_tag = tag - niche_start
///     relative_max = niche_range_end - niche_range_start
///     is_niche = relative_tag <= (ule) relative_max
///     discr = if is_niche {
///         cast(relative_tag) + niche_range_start
///     } else {
///         untagged_variant
///     }

const NOPTR: usize = 0b0;
const NOPTR_LEN: usize = 1;

const SIMPLE: usize = 0b10;
const COMPLEX: usize = 0b11;
const COMPLEX_LEN: usize = 2;

const SEQ_INT: usize = 0b10;
const SEQ_INT_LEN: usize = 2;

const SEQ_PTR: usize = 0b110;
const SEQ_PTR_LEN: usize = 3;

const ENUM_CASE: usize = 0b1110;
const ENUM_CASE_LEN: usize = 4;

const ENUM_NICHE: usize = 0b1111;
const ENUM_NICHE_LEN: usize = 4;

pub type BitmapData = SmallVec<[u8; 16]>;

pub struct CompressedBitVec {
    buf: u128,
    size: usize,
    bytes: BitmapData,
}

impl CompressedBitVec {
    pub fn new() -> CompressedBitVec {
        Self { buf: 0, size: 0, bytes: SmallVec::new() }
    }
}

impl CompressedBitVec {
    fn commit(&mut self) {
        let mut size = self.size;
        self.size = 0;
        assert!(size <= 128, "Bit buffer overflow");

        /* right align the buffer */
        if size != 128 {
            self.buf >>= 128 - size;
        }

        /* encode the last few bits with raw bitmap if possible */
        while size > 7 {
            let trailing_one = (self.buf.trailing_ones() as usize).min(size);
            let trailing_zero = (self.buf.trailing_zeros() as usize).min(size);

            /* lots of ones, encode them as "varint lots of ptr bits" */
            if trailing_one > 7 {
                self.emit_varint(SEQ_PTR, SEQ_PTR_LEN, trailing_one - 1);
                self.buf >>= trailing_one;
                size -= trailing_one;
                continue;
            }

            /* lots of zeros, encode them as "varint lots of int bits" */
            if trailing_zero > 7 {
                self.emit_varint(SEQ_INT, SEQ_INT_LEN, trailing_zero - 1);
                self.buf >>= trailing_zero;
                size -= trailing_zero;
                continue;
            }

            /* mixed ones and zeros, encode as sequence of raw bitmaps */
            self.bytes.push((self.buf & 0x7f) as u8);
            self.buf >>= 7;
            size -= 7;
        }

        /* the remaining bits */
        if size != 0 {
            assert!(self.buf < 0x80, "Pointer map size desynced");
            self.bytes.push((self.buf & 0x7f) as u8);
        }
    }

    fn emit_rep(&mut self, bit: u8, mut rep: usize) {
        assert!((bit & 0xfe) == 0, "Invalid bit value");
        while rep > 0 {
            let len = rep.min(128 - self.size);
            self.buf >>= len;

            if bit != 0 {
                self.buf |= !((1 << (128 - len)) - 1);
            }

            rep -= len;
            self.size += len;

            if self.size >= 128 {
                self.commit();
            }
        }
    }

    fn emit_varint<T: TryInto<u128>>(&mut self, tag: usize, mut tag_size: usize, value: T) {
        assert!(tag_size < 7, "Invalid prefix length");
        if self.size != 0 {
            self.commit();
        }
        let mut tag = tag as u128;
        let mut value: u128 = value.try_into().ok().unwrap();
        while value >= 1 << (7 - tag_size) {
            let v_tag = tag << (8 - tag_size);
            let v_flag = 1u128 << (7 - tag_size);
            self.bytes.push((v_tag | v_flag | (value & (v_flag - 1))) as u8);
            value >>= 7 - tag_size;
            tag_size = 0;
            tag = 0;
        }
        self.bytes.push(((tag << (8 - tag_size)) | value) as u8);
    }
}

impl CompressedBitVec {
    pub fn finish(mut self) -> BitmapData {
        self.commit();
        self.bytes
    }
}

impl CompressedBitVec {
    pub fn add_int(&mut self, rep: usize) {
        self.emit_rep(0, rep);
    }

    pub fn add_ptr(&mut self, rep: usize) {
        self.emit_rep(1, rep);
    }

    pub fn add_noptr(&mut self, len: usize) {
        self.emit_varint(NOPTR, NOPTR_LEN, len);
    }

    pub fn add_simple(&mut self, map: &[bool]) {
        assert!(map.len() > 0, "Empty simple type");
        assert!(map.len() <= 4, "Too many slots for a simple type");
        assert!(map.iter().any(|v| *v), "Simple type without pointer slots");
        if self.size != 0 {
            self.commit();
        }
        self.bytes.push(
            map.iter()
                .enumerate()
                .map(|(i, b)| (*b as usize) << i)
                .fold((SIMPLE << 6) | (map.len() - 1) << 4, usize::bitor) as u8,
        );
    }

    pub fn add_complex(&mut self, len: usize) {
        self.emit_varint(COMPLEX, COMPLEX_LEN, len);
    }

    pub fn add_enum_tag(&mut self, tag: u128) {
        self.emit_varint(0, 0, tag);
    }

    pub fn add_enum_header(&mut self, value: &Enum, niche: Option<Niche>) {
        if niche.is_none() {
            self.emit_varint(ENUM_CASE, ENUM_CASE_LEN, value.variants.len() - 1);
        } else {
            self.emit_varint(ENUM_NICHE, ENUM_NICHE_LEN, value.variants.len() - 1);
        }

        self.emit_varint(0, 0, value.tag_offs.bytes());
        self.emit_varint(0, 0, value.tag_size.bytes());

        if let Some(nx) = niche {
            self.emit_varint(0, 0, nx.niche_start);
            self.emit_varint(0, 0, nx.niche_variants.start().as_usize());
            self.emit_varint(0, 0, nx.niche_variants.end().as_usize());
            self.emit_varint(0, 0, nx.untagged_variant.as_usize());
        }
    }
}
