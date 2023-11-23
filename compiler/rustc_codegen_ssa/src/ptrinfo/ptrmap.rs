use std::{
    fmt::{Debug, Formatter, Result as FmtResult},
    ops::RangeInclusive,
};

use rustc_middle::ty::layout::TyAndLayout;
use rustc_target::abi::{Abi, FieldsShape, Primitive, Scalar, Size, VariantIdx, Variants};
use smallvec::{smallvec, SmallVec};

use super::bitvec::{BitVec, BitmapData};
use crate::traits::CodegenMethods;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct InexactRange {
    left: usize,
    right: usize,
}

impl InexactRange {
    pub fn max(self, other: Self) -> Self {
        Self {
            left: self.left.min(other.left),
            right: self.left.max(other.right),
        }
    }
}

impl InexactRange {
    fn to_range(self) -> RangeInclusive<usize> {
        self.left..=self.right
    }
}

impl Debug for InexactRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(&self.to_range(), f)
    }
}

impl From<RangeInclusive<usize>> for InexactRange {
    fn from(value: RangeInclusive<usize>) -> Self {
        let (left, right) = value.into_inner();
        Self { left, right }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PointerMap {
    bits: BitVec,
    inexact: SmallVec<[InexactRange; 16]>,
}

impl PointerMap {
    pub fn has_pointers(&self) -> bool {
        !self.bits.is_empty()
    }
}

impl PointerMap {
    pub fn encode(self) -> BitmapData {
        if self.bits.is_empty() {
            smallvec![]
        } else if self.inexact.is_empty() {
            self.serialize_exact()
        } else {
            self.serialize_inexact()
        }
    }
}

impl PointerMap {
    fn serialize_exact(self) -> BitmapData {
        let mut ret = smallvec![0; self.bits.encoded_len()];
        self.bits.encode_into(&mut ret);
        ret
    }

    fn serialize_inexact(mut self) -> BitmapData {
        let mut pos = self.bits.encoded_len();
        let mut ret = smallvec![0; self.bits.data_len() + pos];

        /* encode the bitmap, then sort the inexact ranges */
        self.bits.encode_into(&mut ret);
        self.inexact.sort_by_key(|v| v.left);

        /* encode all the inexact ranges as bitmap */
        let mut bits = 0;
        let mut size = 0;
        let mut last = 0;

        /* bit appending routine */
        let mut add_bits = |bit: i8, mut count: usize| {
            let rem = 8 - size;
            let len = count.min(rem);
            let idx = self.bits.data_len();

            /* flush pending bits if any */
            if size != 0 {
                bits |= ((-bit as u8) & ((1 << len) - 1)) << size;
                size += len;
                count -= len;

                /* flush the buffer */
                if size == 8 {
                    ret[pos] = bits & ret[pos - idx];
                    size = 0;
                    pos += 1;
                }
            }

            /* add in groups of 8 bits */
            while count >= 8 {
                ret[pos] = (-bit as u8) & ret[pos - idx];
                count -= 8;
                pos += 1;
            }

            /* remaining bits */
            if count != 0 {
                size = count;
                bits = (-bit as u8) & ((1 << count) - 1);
            }
        };

        /* encode every range */
        for r in self.inexact {
            add_bits(0, r.left - last);
            add_bits(1, r.right - r.left + 1);
            last = r.right + 1;
        }

        /* remaining bits, they are exact */
        if last != self.bits.bits_len() {
            add_bits(0, self.bits.bits_len() - last);
        }

        /* remaining bits */
        if size != 0 {
            ret[pos] = bits & ret[pos - self.bits.data_len()];
            pos += 1;
        }

        /* buffer size must be exact */
        assert!(pos == ret.len());
        ret
    }
}

impl PointerMap {
    #[inline(always)]
    fn index<'tcx, Cx: CodegenMethods<'tcx>>(cx: &Cx, offset: Size) -> usize {
        offset.bytes_usize() / (cx.data_layout().pointer_align.abi.bytes() as usize)
    }
}

impl PointerMap {
    fn set_submap<'tcx, Cx: CodegenMethods<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        submap: PointerMap,
    ) -> Option<InexactRange> {
        self.bits
            .merge(Self::index(cx, offset), submap.bits)
            .map(InexactRange::from)
    }

    fn set_scalar<'tcx, Cx: CodegenMethods<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        scalar: Scalar,
    ) {
        // Unions don't have deterministic runtime type information, user must
        // take care of their pointer variants, ROG GC won't handle those pointers.
        if let Scalar::Initialized {
            value: Primitive::Pointer(_),
            ..
        } = scalar
        {
            if offset.is_aligned(cx.data_layout().pointer_align.abi) {
                self.bits.set(Self::index(cx, offset));
            }
        }
    }

    fn set_layout<'tcx, Cx: CodegenMethods<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        layout: TyAndLayout<'tcx>,
    ) {
        match layout.abi {
            Abi::Uninhabited => {}
            Abi::Scalar(scalar) => self.set_scalar(cx, offset, scalar),
            Abi::ScalarPair(a, b) => {
                self.set_scalar(cx, offset, a);
                self.set_scalar(cx, offset + a.size(cx).align_to(b.align(cx).abi), b);
            }
            Abi::Vector { element, count } => {
                let size = element.size(cx);
                let align = element.align(cx).abi;
                for i in 0..count {
                    self.set_scalar(cx, offset + (size * i).align_to(align), element)
                }
            }
            Abi::Aggregate { .. } => match layout.fields {
                // Treat unions as opaque data, as described above.
                FieldsShape::Primitive | FieldsShape::Union(_) => {}
                FieldsShape::Array { count, .. } => {
                    let elem = layout.field(cx, 0);
                    let submap = Self::resolve(cx, layout.field(cx, 0), None);
                    for i in 0..count {
                        self.set_submap(cx, offset + elem.size * i, submap.clone());
                    }
                }
                FieldsShape::Arbitrary { .. } => match layout.variants {
                    Variants::Single { .. } => {
                        for i in layout.fields.index_by_increasing_offset() {
                            self.set_submap(
                                cx,
                                offset + layout.fields.offset(i),
                                Self::resolve(cx, layout.field(cx, i), None),
                            );
                        }
                    }
                    Variants::Multiple { ref variants, .. } => {
                        variants
                            .indices()
                            .map(|i| Self::resolve(cx, layout, Some(i)))
                            .flat_map(|m| self.set_submap(cx, offset, m))
                            .skip(1)
                            .reduce(InexactRange::max)
                            .into_iter()
                            .for_each(|r| self.inexact.push(r));
                    }
                },
            },
        }
    }
}

impl PointerMap {
    pub fn resolve<'tcx, Cx: CodegenMethods<'tcx>>(
        cx: &Cx,
        layout: TyAndLayout<'tcx>,
        variant_idx: Option<VariantIdx>,
    ) -> Self {
        let ty = layout.ty;
        if let Some(map) = cx.get_pointer_map(ty, variant_idx) {
            return map;
        }
        let layout = match variant_idx {
            Some(idx) => layout.for_variant(cx, idx),
            None => layout,
        };
        let align = cx.data_layout().pointer_align.abi;
        let size = layout.size.align_to(align).bytes() / align.bytes();
        let mut ret = Self {
            bits: BitVec::new(size as usize),
            inexact: smallvec![],
        };
        if layout.size != Size::ZERO {
            ret.set_layout(cx, Size::ZERO, layout);
        }
        cx.add_pointer_map(ty, variant_idx, ret.clone());
        ret
    }
}

pub fn may_contain_heap_ptr<'tcx, Cx: CodegenMethods<'tcx>>(
    cx: &Cx,
    layout: TyAndLayout<'tcx>,
) -> bool {
    PointerMap::resolve(cx, layout, None).has_pointers()
}
