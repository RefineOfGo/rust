use std::ops::BitOr;

use smallvec::SmallVec;

use super::Enum;

/// First-byte encodes pointer info for simple types or `sizeof(type)` for
/// more complex types:
///     0000_0000   Types that don't have any pointers, trimmed for root types.
///     00ww_pppp   Types which contains at most 4 pointer slots and is pointer
///                 size aligned, it's size is encoded as `(ww + 1) * sizeof(pointer)`.
///                 `pppp` indicates which slot is a pointer. `pppp` == 0 is
///                 invalid in this encoding.
///     1nnn_nnnn   Varint size `nnn_nnnn` (in bytes) of types that cannot be
///                 encoded with the first method. Right after this byte are
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
/// Niche enums are encoded as follows (like direct enum, but with an
/// extra untagged variant):
///     Niche {
///         header            : Identifier described as above (1110_xxxx).
///         tag_offs          : Varint, offset to the tag field.
///         tag_size          : Varint, size of the tag field.
///         variants ...
///         untagged_variant
///     }

pub type BitmapData = SmallVec<[u8; 16]>;

#[derive(Clone, Debug, Default)]
pub struct CompressedBitVec {
    buf: u128,
    size: usize,
    bytes: BitmapData,
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
                self.emit_varint(0b110, 3, trailing_one - 1);
                self.buf >>= trailing_one;
                size -= trailing_one;
                continue;
            }

            /* lots of zeros, encode them as "varint lots of int bits" */
            if trailing_zero > 7 {
                self.emit_varint(0b10, 2, trailing_zero - 1);
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
        if self.size != 0 {
            self.commit();
        }
        match self.bytes.as_slice() {
            &[0] => BitmapData::new(),
            _ => self.bytes,
        }
    }
}

impl CompressedBitVec {
    pub fn add_int(&mut self, rep: usize) {
        self.emit_rep(0, rep);
    }

    pub fn add_ptr(&mut self, rep: usize) {
        self.emit_rep(1, rep);
    }

    pub fn add_noptr(&mut self) {
        self.commit();
        self.bytes.push(0x00);
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
                .fold((map.len() - 1) << 4, usize::bitor) as u8,
        );
    }

    pub fn add_complex(&mut self, len: usize) {
        self.emit_varint(1, 1, len);
    }

    pub fn add_enum_tag(&mut self, tag: u128) {
        self.emit_varint(0, 0, tag);
    }

    pub fn add_enum_header(&mut self, value: &Enum) {
        if value.untagged_variant.is_none() {
            self.emit_varint(0b1110, 4, value.variants.len() - 1);
        } else {
            self.emit_varint(0b1111, 4, value.variants.len() - 1);
        }

        self.emit_varint(0, 0, value.tag_offs.bytes());
        self.emit_varint(0, 0, value.tag_size.bytes());
    }
}
