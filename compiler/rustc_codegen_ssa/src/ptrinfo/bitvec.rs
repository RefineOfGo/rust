use std::ops::BitOr;

use rustc_target::abi::Size;
use smallvec::SmallVec;

use super::Enum;

/// First-byte encodes pointer info for simple types or `sizeof(type)` for
/// more complex types:
///     00ww_pppp   Types which contains at most 4 pointer slots and is pointer
///                 size aligned, it's size is encoded as `(ww + 1) * sizeof(pointer)`.
///                 `pppp` indicates which slot is a pointer. `pppp` == 0 is
///                 invalid in this encoding.
///     1nnn_nnnn   Types with size `varint(nnn_nnnn) + 1` that cannot be encoded with
///                 the first method. Right after this byte are all the components
///                 encoded back to back.
///
/// Types that don't have any pointers encode to nothing.
///
/// Encoding for each component:
///     0000_001x   Raw bitmap with a single bit.
///     0000_01xx   Raw bitmap with 2 bits.
///     0000_1xxx   Raw bitmap with 3 bits.
///     0001_xxxx   Raw bitmap with 4 bits.
///     001x_xxxx   Raw bitmap with 5 bits.
///     01xx_xxxx   Raw bitmap with 6 bits.
///     10xx_xxxx   Varint encoded number of repeating int bits minus one.
///     110x_xxxx   Varint encoded number of repeating ptr bits minus one.
///     1110_xxxx   Varint encoded number of enum cases minus one.
///     1111_xxxx   Varint encoded number of niche cases minus one.
///
/// Direct enums are encoded as follows:
///     Enum {
///         header          Identifier described as above (1110_xxxx).
///         max_size        Varint encoded maximum size (in bytes) of this enum, minus one.
///         tag_field       Encoded tag field, see below.
///         variants ...
///     }
///
///     `tag_field` is encoded as `bbbx_xxxx`:
///         `bbb._....`     log2(size of the tag field) within range 0..=4.
///         `...x_xxxx`     Varint encoded offset to the tag field.
///
/// Niche enums are encoded as follows (almost the same as direct enum, but with an
/// extra untagged variant):
///     Niche {
///         header          Identifier described as above (1110_xxxx).
///         max_size        Varint, maximum size (in bytes) of this enum, minus one.
///         tag_field       Encoded tag field, same as direct enum.
///         variants ...
///         untagged_variant
///     }
///
/// All variants are encoded as follows:
///     Variant {
///         flags_and_tag   Encoded flags and discriminant value, see below.
///         submap          Optional pointer map for this variant.
///     }
///
///     `flags_and_tag` is encoded as `sudd_dddd`:
///         `s..._....`     Set to 1 if it contains a submap, 0 otherwise.
///         `.u.._....`     Set to 1 if this is the untagged variant, 0 otherwise.
///         `..dd_dddd`     Varint encoded discriminant value of this variant, must be
///                         `00_0000` for untagged variant.

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
        while size > 6 {
            let trailing_one = (self.buf.trailing_ones() as usize).min(size);
            let trailing_zero = (self.buf.trailing_zeros() as usize).min(size);

            /* lots of ones, encode them as "varint lots of ptr bits" */
            if trailing_one > 6 {
                self.emit_varint(0b110, 3, trailing_one - 1);
                self.buf >>= trailing_one;
                size -= trailing_one;
                continue;
            }

            /* lots of zeros, encode them as "varint lots of int bits" */
            if trailing_zero > 6 {
                self.emit_varint(0b10, 2, trailing_zero - 1);
                self.buf >>= trailing_zero;
                size -= trailing_zero;
                continue;
            }

            /* mixed ones and zeros, encode as sequence of raw bitmaps */
            self.bytes.push(0x40 | ((self.buf & 0x3f) as u8));
            self.buf >>= 6;
            size -= 6;
        }

        /* the remaining bits */
        if size != 0 {
            assert!(self.buf < 0x40, "Pointer map size desynced");
            self.bytes.push(((1 << size) | (self.buf & ((1 << size) - 1))) as u8);
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

        let v_tag = tag << (8 - tag_size);
        self.bytes.push((v_tag | value) as u8);
    }
}

impl CompressedBitVec {
    pub fn finish(mut self) -> BitmapData {
        if self.size != 0 {
            self.commit();
        }
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

    pub fn add_simple(&mut self, map: &[bool]) {
        assert!(map.len() > 0, "Simple type with no slots");
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
        assert!(len > 0, "Complex type with zero size");
        self.emit_varint(1, 1, len - 1);
    }

    pub fn add_enum_tag(&mut self, tag: Option<u128>, has_submap: bool) {
        self.emit_varint(
            ((has_submap as usize) << 1) | (tag.is_none() as usize),
            2,
            tag.unwrap_or(0),
        );
    }

    pub fn add_enum_header(&mut self, value: &Enum) {
        assert!(!value.variants.is_empty(), "Empty enum");
        assert_ne!(value.max_size, Size::ZERO, "Zero-sized enum");
        assert_ne!(value.tag_size, Size::ZERO, "Zero-sized tag field");

        let tag_size = value.tag_size.bytes();
        assert!(tag_size.is_power_of_two(), "Tag size is not a power of two");

        let tag_log2 = tag_size.trailing_zeros();
        assert!(tag_log2 <= 4, "Oversized tag field");

        if value.untagged_variant.is_none() {
            self.emit_varint(0b1110, 4, value.variants.len() - 1);
        } else {
            self.emit_varint(0b1111, 4, value.variants.len() - 1);
        }

        self.emit_varint(0, 0, value.max_size.bytes() - 1);
        self.emit_varint(tag_log2 as usize, 3, value.tag_offs.bytes());
    }
}
