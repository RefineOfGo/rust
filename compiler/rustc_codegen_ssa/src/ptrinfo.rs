use std::fmt::Debug;

use rustc_middle::ty::layout::TyAndLayout;
use rustc_target::abi::{Abi, FieldsShape, Primitive, Scalar, Size, VariantIdx, Variants};
use smallvec::{smallvec, SmallVec};

use crate::traits::CodegenMethods;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PointerMap {
    size: usize,
    used: SmallVec<[u8; 16]>,
    bits: SmallVec<[u8; 16]>,
    mask: SmallVec<[u8; 16]>,
}

impl PointerMap {
    fn new(size: usize) -> Self {
        Self {
            size,
            used: smallvec![0; Self::bytes(size)],
            bits: smallvec![0; Self::bytes(size)],
            mask: smallvec![0; Self::bytes(size)],
        }
    }
}

impl PointerMap {
    #[inline(always)]
    pub fn has_pointers(&self) -> bool {
        self.bits.iter().any(|v| *v != 0)
    }
}

impl PointerMap {
    pub fn encode(self) -> Vec<u8> {
        let mut len = self.size - 1;
        let mut ret = Vec::with_capacity(self.bits.len() * 2 + 1);

        /* sanity check */
        assert!(
            self.bits
                .iter()
                .zip(self.mask.iter())
                .zip(self.used.iter())
                .all(|((b, m), u)| (*b | *m) & !*u == 0),
            "found bits within unused area"
        );

        /* encode length as varint */
        while len >= 0x80 {
            ret.push(0x80 | (len & 0x7f) as u8);
            len >>= 7;
        }

        /* last bits */
        ret.push(len as u8);
        ret.extend_from_slice(&self.bits);

        /* check if the encoding is exact */
        if self.mask.iter().all(|v| *v == 0) {
            return ret;
        }

        /* add the inexact map if any */
        ret.extend_from_slice(&self.mask);
        ret
    }
}

impl PointerMap {
    #[inline(always)]
    const fn bytes(bits: usize) -> usize {
        (bits + 7) / 8
    }

    #[inline(always)]
    fn index<'tcx, Cx: CodegenMethods<'tcx>>(cx: &Cx, offset: Size) -> usize {
        offset.bytes_usize() / cx.data_layout().pointer_align.abi.bytes_usize()
    }
}

impl PointerMap {
    fn set_zero(&mut self, idx: usize, len: usize) {
        (idx..idx + len).for_each(|i| self.mask[i] |= self.bits[i]);
        self.used[idx..idx + len].fill(0xff);
        self.bits[idx..idx + len].fill(0);
    }

    fn set_bits(&mut self, idx: usize, mask: u8, bit: i8) {
        self.mask[idx] |= (self.bits[idx] ^ (-bit as u8)) & mask;
        self.bits[idx] &= !mask;
        self.bits[idx] |= (-bit as u8) & mask;
        self.used[idx] |= mask;
    }

    fn set_noptr<'tcx, Cx: CodegenMethods<'tcx>>(&mut self, cx: &Cx, offset: Size, size: Size) {
        let start = offset.bytes_usize();
        let align = cx.data_layout().pointer_align.abi.bytes_usize();

        /* empty layout */
        if size == Size::ZERO {
            return;
        }

        /* slot index of the first & last byte of this object */
        let pos = start / align;
        let end = (start + size.bytes_usize() - 1) / align;

        /* slot count & bitmap offset */
        let mut len = end - pos + 1;
        let (mut idx, offs) = (pos / 8, pos % 8);

        /* fast-path: a single bit */
        if len == 1 {
            self.set_bits(idx, 1 << offs, 0);
            return;
        }

        /* unaligned leading bits */
        if offs != 0 {
            let rem = (8 - offs).min(len);
            let mask = ((1 << rem) - 1) << offs;
            self.set_bits(idx, mask, 0);
            len -= rem;
            idx += 1;
        }

        /* consecutive 8-bit groups */
        if len >= 8 {
            let num = len / 8;
            self.set_zero(idx, num);
            idx += num;
            len %= 8;
        }

        /* remaining bits */
        if len != 0 {
            let mask = (1 << len) - 1;
            self.set_bits(idx, mask, 0);
        }
    }

    fn set_scalar<'tcx, Cx: CodegenMethods<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        scalar: Scalar,
    ) {
        /* unions don't have deterministic runtime type information, user must
         * take care of their pointer variants, ROG GC won't handle those pointers. */
        match scalar {
            Scalar::Initialized { value: Primitive::Pointer(_), .. }
                if offset.is_aligned(cx.data_layout().pointer_align.abi) =>
            {
                let pos = Self::index(cx, offset);
                self.set_bits(pos / 8, 1 << (pos % 8), 1);
            }
            _ => self.set_noptr(cx, offset, scalar.size(cx)),
        }
    }

    fn set_layout<'tcx, Cx: CodegenMethods<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        layout: TyAndLayout<'tcx>,
    ) {
        match layout.abi {
            Abi::Uninhabited => self.set_noptr(cx, offset, layout.size),
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
                FieldsShape::Primitive | FieldsShape::Union(_) => {
                    self.set_noptr(cx, offset, layout.size);
                }
                FieldsShape::Array { count, .. } => {
                    let elem = layout.field(cx, 0);
                    for i in 0..count {
                        self.set_layout(cx, offset + elem.size * i, elem);
                    }
                }
                FieldsShape::Arbitrary { .. } => match layout.variants {
                    Variants::Single { .. } => {
                        for i in layout.fields.index_by_increasing_offset() {
                            let offs = offset + layout.fields.offset(i);
                            self.set_layout(cx, offs, layout.field(cx, i));
                        }
                    }
                    Variants::Multiple { ref variants, .. } => {
                        for i in variants.indices() {
                            self.set_layout(cx, offset, layout.for_variant(cx, i));
                        }
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
        let size = layout.size;
        if let Some(map) = cx.get_pointer_map(ty, variant_idx) {
            return map;
        }
        let layout = match variant_idx {
            Some(idx) => layout.for_variant(cx, idx),
            None => layout,
        };
        let align = cx.data_layout().pointer_align.abi;
        let count = size.align_to(align).bytes_usize() / align.bytes_usize();
        let mut ret = Self::new(count);
        ret.set_layout(cx, Size::ZERO, layout);
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
