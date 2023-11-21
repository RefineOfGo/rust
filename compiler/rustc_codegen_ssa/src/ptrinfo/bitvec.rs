use rustc_target::abi::Size;
use smallvec::SmallVec;

use super::Enum;

/// Pointer maps are encoded as components back to back, types that don't have any
/// pointers encode to nothing.
///
/// Encoding for each component:
///
///     0000_001x   Raw bitmap with a single bit.
///     0000_01xx   Raw bitmap with 2 bits.
///     0000_1xxx   Raw bitmap with 3 bits.
///     0001_xxxx   Raw bitmap with 4 bits.
///     001x_xxxx   Raw bitmap with 5 bits.
///     01xx_xxxx   Raw bitmap with 6 bits.
///     10xx_xxxx   Varint encoded number of repeating int bits minus one.
///     110x_xxxx   Varint encoded number of repeating pointer bits minus one.
///     1110_xxxx   Varint encoded number of enum cases minus one.
///     1111_xxxx   Varint encoded number of niche cases (not including the untagged
///                 variant) minus one.
///
/// Direct enums are encoded as follows:
///
///     Enum {
///         header          Identifier described as above (1110_xxxx).
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
///
///     Niche {
///         header          Identifier described as above (1111_xxxx).
///         tag_field       Encoded tag field, same as direct enum.
///         variants ...
///         untagged_variant
///     }
///
/// All variants are encoded as follows:
///
///     Variant {
///         discriminant?   Optional varint encoded discriminant value of this variant, the
///                         untagged variant does not have this field.
///         submap_size     Varint encoded size of the encoded submap, in bytes, 0 if the
///                         submap does not exist.
///         submap?         Optional pointer map for this variant.
///     }
///
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

    pub fn add_submap(&mut self, mut submap: CompressedBitVec) {
        if self.size != 0 {
            self.commit();
        }
        if submap.size != 0 {
            submap.commit();
        }
        self.emit_varint(0, 0, submap.bytes.len());
        self.bytes.extend_from_slice(&submap.bytes);
    }

    pub fn add_enum_tag(&mut self, tag: u128) {
        self.emit_varint(0, 0, tag);
    }

    pub fn add_enum_header(&mut self, value: &Enum) {
        assert!(!value.variants.is_empty(), "Empty enum");
        assert_ne!(value.max_size, Size::ZERO, "Zero-sized enum");
        assert_ne!(value.tag_size, Size::ZERO, "Zero-sized tag field");

        let tag_size = value.tag_size.bytes();
        assert!(tag_size.is_power_of_two(), "Tag size is not a power of two");

        let tag_log2 = tag_size.trailing_zeros();
        assert!(tag_log2 <= 4, "Oversized tag field");

        let niche_bit = value.untagged_variant.is_some() as usize;
        self.emit_varint(0b1110 | niche_bit, 4, value.variants.len() - 1);
        self.emit_varint(tag_log2 as usize, 3, value.tag_offs.bytes());
    }
}
