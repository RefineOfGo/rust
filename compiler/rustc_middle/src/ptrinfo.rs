use std::fmt::Debug;

use rustc_target::abi::{Abi, FieldsShape, HasDataLayout, Primitive, Scalar, Size, Variants};
use smallvec::{smallvec, SmallVec};

use crate::{
    managed::{ManagedChecker, ManagedSelf},
    ty::{
        layout::{HasParamEnv, HasTyCtxt, TyAndLayout},
        Ty,
    },
};

pub trait PointerMapMethods<'tcx> {
    fn compute_pointer_map<R>(
        &self,
        ty: Ty<'tcx>,
        map_fn: impl FnOnce(&PointerMap) -> R,
        compute_fn: impl FnOnce() -> PointerMap,
    ) -> R;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PointerMap {
    used: SmallVec<[u8; 16]>,
    bits: SmallVec<[u8; 16]>,
    mask: SmallVec<[u8; 16]>,
}

impl PointerMap {
    fn new(size: usize) -> Self {
        Self {
            used: smallvec![0; (size + 7) / 8],
            bits: smallvec![0; (size + 7) / 8],
            mask: smallvec![0; (size + 7) / 8],
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
    pub fn encode(&self) -> Vec<u8> {
        let size = self.bits.iter().rposition(|v| *v != 0).map(|n| n + 1).unwrap_or(0);
        let bits = size * (u8::BITS as usize);

        /* empty bitmap */
        if size == 0 {
            return Vec::new();
        }

        /* sanity check */
        assert!(
            self.bits
                .iter()
                .zip(self.mask.iter())
                .zip(self.used.iter())
                .all(|((b, m), u)| (*b | *m) & !*u == 0),
            "found bits within unused area"
        );

        /* allocate space for encoded bitmap */
        let mut len = bits - self.bits[size - 1].leading_zeros() as usize - 1;
        let mut ret = Vec::with_capacity(size * 2 + 1);

        /* encode length as varint */
        while len >= 0x80 {
            ret.push(0x80 | (len & 0x7f) as u8);
            len >>= 7;
        }

        /* add the actual bitmap */
        ret.push(len as u8);
        ret.extend_from_slice(&self.bits[..size]);

        /* check if the encoding is exact */
        if self.mask[..size].iter().all(|v| *v == 0) {
            return ret;
        }

        /* add the inexact map if any */
        ret.extend_from_slice(&self.mask[..size]);
        ret
    }
}

impl PointerMap {
    fn set_zero(&mut self, idx: usize, len: usize) {
        (idx..idx + len).for_each(|i| self.mask[i] |= self.bits[i] & self.used[i]);
        self.used[idx..idx + len].fill(0xff);
    }

    fn set_bits(&mut self, idx: usize, mask: u8, bit: i8) {
        self.mask[idx] |= (self.bits[idx] ^ (-bit as u8)) & self.used[idx] & mask;
        self.bits[idx] |= (-bit as u8) & mask;
        self.used[idx] |= mask;
    }

    fn set_noptr<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasParamEnv<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        size: Size,
    ) {
        let start = offset.bytes_usize();
        let align = cx.data_layout().pointer_size.bytes_usize();

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

    fn set_scalar<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasParamEnv<'tcx>>(
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
                let pos = offset.bytes_usize() / cx.data_layout().pointer_size.bytes_usize();
                self.set_bits(pos / 8, 1 << (pos % 8), 1);
            }
            _ => self.set_noptr(cx, offset, scalar.size(cx)),
        }
    }

    fn set_layout<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasParamEnv<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        layout: TyAndLayout<'tcx>,
    ) {
        match layout.fields {
            FieldsShape::Primitive => match layout.abi {
                Abi::Uninhabited => self.set_noptr(cx, offset, layout.size),
                Abi::Scalar(scalar) => self.set_scalar(cx, offset, scalar),
                Abi::ScalarPair(..) => unreachable!("conflict: Primitive & Scalar Pair"),
                Abi::Vector { .. } => unreachable!("conflict: Primitive & Vector"),
                Abi::Aggregate { .. } => unreachable!("conflict: Primitive & Aggregate"),
            },
            FieldsShape::Union(_) => self.set_noptr(cx, offset, layout.size),
            FieldsShape::Array { count, .. } => {
                let elem = layout.field(cx, 0);
                let item_size = elem.size.align_to(elem.align.abi);
                for i in 0..count {
                    self.set_layout(cx, offset + item_size * i, elem);
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
                    let mut min_size = Size::from_bytes(u64::MAX);
                    for i in variants.indices() {
                        let variant = layout.for_variant(cx, i);
                        min_size = min_size.min(variant.size);
                        self.set_layout(cx, offset, variant);
                    }
                    assert!(min_size <= layout.size);
                    self.set_noptr(cx, offset + min_size, layout.size - min_size);
                }
            },
        }
    }
}

impl PointerMap {
    pub fn resolve_and<
        'cx,
        'tcx: 'cx,
        R,
        Cx: HasDataLayout + HasTyCtxt<'tcx> + HasParamEnv<'tcx> + PointerMapMethods<'tcx>,
    >(
        cx: &'cx Cx,
        layout: TyAndLayout<'tcx>,
        map_fn: impl FnOnce(&Self) -> R,
    ) -> R {
        cx.compute_pointer_map(layout.ty, map_fn, move || {
            if ManagedChecker::new(cx.tcx()).is_managed_self(layout.ty) != ManagedSelf::Yes {
                Self::new(0)
            } else {
                let unit = cx.data_layout().pointer_size;
                let size = layout.size.align_to(cx.data_layout().pointer_align.abi);
                let mut ret = Self::new(size.bytes_usize() / unit.bytes_usize());
                ret.set_layout(cx, Size::ZERO, layout);
                ret
            }
        })
    }
}

pub fn encode<
    'cx,
    'tcx: 'cx,
    Cx: HasDataLayout + HasTyCtxt<'tcx> + HasParamEnv<'tcx> + PointerMapMethods<'tcx>,
>(
    cx: &'cx Cx,
    layout: TyAndLayout<'tcx>,
) -> Vec<u8> {
    PointerMap::resolve_and(cx, layout, PointerMap::encode)
}

pub fn has_pointers<
    'cx,
    'tcx: 'cx,
    Cx: HasDataLayout + HasTyCtxt<'tcx> + HasParamEnv<'tcx> + PointerMapMethods<'tcx>,
>(
    cx: &'cx Cx,
    layout: TyAndLayout<'tcx>,
) -> bool {
    PointerMap::resolve_and(cx, layout, PointerMap::has_pointers)
}
