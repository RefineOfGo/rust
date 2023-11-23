use std::{
    fmt::{Debug, Formatter, Result as FmtResult},
    ops::RangeInclusive,
};

use smallvec::{smallvec, SmallVec};

pub type BitmapData = SmallVec<[u8; 16]>;

#[derive(Clone, PartialEq, Eq)]
pub struct BitVec {
    size: usize,
    bits: BitmapData,
}

impl BitVec {
    pub fn new(size: usize) -> Self {
        let bits = smallvec![0; (size + 7) / 8 + 1];
        Self { size, bits }
    }
}

impl BitVec {
    pub fn set(&mut self, index: usize) {
        assert!(index < self.size);
        self.bits[index / 8] |= 1 << (index % 8);
    }

    pub fn merge(&mut self, start: usize, other: BitVec) -> Option<RangeInclusive<usize>> {
        assert!(start + other.size <= self.size);
        let idx = start / 8;
        let offs = start % 8;

        /* check for alignment */
        if offs == 0 {
            self.merge_aligned(idx, other)
        } else {
            self.merge_unaligned(idx, offs, other)
        }
    }
}

impl BitVec {
    #[inline(always)]
    fn bytes(&self) -> &[u8] {
        &self.bits[..self.data_len()]
    }

    #[inline(always)]
    fn indexed_bytes(&self) -> impl Iterator<Item = (usize, u8)> + '_ {
        self.bits[..self.data_len()].iter().copied().enumerate()
    }
}

impl BitVec {
    fn merge_aligned(&mut self, idx: usize, other: BitVec) -> Option<RangeInclusive<usize>> {
        let mut low = usize::MAX;
        let mut high = 0;

        /* scan & merge every bit, word by word */
        for (i, v) in other.indexed_bytes() {
            let data = self.bits[idx + i];
            let diff = data ^ v;

            /* they are the same */
            if diff == 0 {
                continue;
            }

            /* update bits */
            low = low.min((idx + i) * 8 + diff.trailing_zeros() as usize);
            high = high.max((idx + i) * 8 + (7 - diff.leading_zeros()) as usize);
            self.bits[idx + i] |= v;
        }

        /* create range if valid */
        if low <= high {
            Some(low..=high)
        } else {
            None
        }
    }

    fn merge_unaligned(
        &mut self,
        idx: usize,
        offs: usize,
        other: BitVec,
    ) -> Option<RangeInclusive<usize>> {
        let mut low = usize::MAX;
        let mut high = 0;

        /* scan & merge every bit */
        for (i, v) in other.indexed_bytes() {
            let lsb = v << offs;
            let msb = v >> (8 - offs);
            let lsd = lsb ^ (self.bits[idx + i] & !((1 << offs) - 1));
            let msd = msb ^ (self.bits[idx + i + 1] & ((1 << (8 - offs)) - 1));

            /* merging same bits */
            if lsd == 0 && msd == 0 {
                continue;
            }

            /* merge bits by logical or */
            self.bits[idx + i] |= lsb;
            self.bits[idx + i + 1] |= msb;

            /* difference lower bounds */
            low = low.min(if lsd != 0 {
                (idx + i) * 8 + lsd.trailing_zeros() as usize
            } else {
                (idx + i + 1) * 8 + msd.trailing_zeros() as usize
            });

            /* difference upper bounds */
            high = high.max(if msd == 0 {
                (idx + i) * 8 + (7 - lsd.leading_zeros()) as usize
            } else {
                (idx + i + 1) * 8 + (7 - msd.leading_zeros()) as usize
            });
        }

        /* create range if valid */
        if low <= high {
            Some(low..=high)
        } else {
            None
        }
    }
}

impl BitVec {
    pub fn encode_into(&self, buf: &mut BitmapData) {
        let mut offs = 0;
        let mut data = self.size - 1;

        /* encode length as varint */
        while data >= 0x80 {
            buf[offs] = 0x80 | (data & 0x7f) as u8;
            data >>= 7;
            offs += 1;
        }

        /* last bits */
        buf[offs] = data as u8;
        offs += 1;

        /* actual bitmap data */
        let src = self.bytes();
        let end = self.encoded_len();

        /* copy bitmap data */
        assert!(offs == self.prefix_len());
        buf[offs..end].copy_from_slice(src);
    }
}

impl BitVec {
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.bits.iter().all(|v| *v == 0)
    }

    #[inline(always)]
    pub fn bits_len(&self) -> usize {
        self.size
    }

    #[inline(always)]
    pub fn data_len(&self) -> usize {
        (self.size + 7) / 8
    }

    #[inline(always)]
    pub fn prefix_len(&self) -> usize {
        (64 - self.size.leading_zeros() as usize + 6) / 7
    }

    #[inline(always)]
    pub fn encoded_len(&self) -> usize {
        self.data_len() + self.prefix_len()
    }
}

impl Debug for BitVec {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if self.size == 0 {
            write!(f, "BitVec(0) {{}}")
        } else {
            write!(f, "BitVec({}) {{ ", self.size)
                .and_then(|_| {
                    (0..self.size)
                        .rev()
                        .map(|i| (i, self.bits[i / 8] & (1 << (i % 8)) != 0))
                        .try_for_each(|(i, b)| {
                            if i % 8 != 7 || i == self.size - 1 {
                                write!(f, "{}", b as u8)
                            } else {
                                write!(f, "_{}", b as u8)
                            }
                        })
                })
                .and_then(|_| write!(f, " }}"))
        }
    }
}
