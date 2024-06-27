use std::{borrow::Cow, fmt::Debug, sync::Arc};

use rustc_target::abi::{Abi, FieldsShape, HasDataLayout, Primitive, Scalar, Size, Variants};
use smallvec::{smallvec, SmallVec};

use crate::{
    managed::ManagedChecker,
    ty::layout::{HasParamEnv, HasTyCtxt, TyAndLayout},
};

struct BitIter {
    i: usize,
    b: u64,
}

impl Iterator for BitIter {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.b == 0 {
            None
        } else {
            let p = self.b.trailing_zeros() as usize;
            self.b &= !(1 << p);
            Some(self.i * 64 + p)
        }
    }
}

impl From<(usize, ((u64, u64), u64))> for BitIter {
    fn from((i, ((b, m), u)): (usize, ((u64, u64), u64))) -> Self {
        let b = b & !m & u;
        BitIter { i, b }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct PointerMapData {
    used: SmallVec<[u64; 4]>,
    bits: SmallVec<[u64; 4]>,
    mask: SmallVec<[u64; 4]>,
}

impl PointerMapData {
    fn new(size: usize) -> Self {
        Self {
            used: smallvec![0; (size + 63) / 64],
            bits: smallvec![0; (size + 63) / 64],
            mask: smallvec![0; (size + 63) / 64],
        }
    }
}

impl PointerMapData {
    fn set_zero(&mut self, idx: usize, len: usize) {
        (idx..idx + len).for_each(|i| self.mask[i] |= self.bits[i] & self.used[i]);
        self.used[idx..idx + len].fill(u64::MAX);
    }

    fn set_bits(&mut self, idx: usize, mask: u64, bit: i64) {
        self.mask[idx] |= (self.bits[idx] ^ (-bit as u64)) & self.used[idx] & mask;
        self.bits[idx] |= (-bit as u64) & mask;
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
        let (mut idx, offs) = (pos / 64, pos % 64);

        /* fast-path: a single bit */
        if len == 1 {
            self.set_bits(idx, 1 << offs, 0);
            return;
        }

        /* unaligned leading bits */
        if offs != 0 {
            let rem = (64 - offs).min(len);
            let mask = ((1 << rem) - 1) << offs;
            self.set_bits(idx, mask, 0);
            len -= rem;
            idx += 1;
        }

        /* consecutive 64-bit groups */
        if len >= 64 {
            let num = len / 64;
            self.set_zero(idx, num);
            idx += num;
            len %= 64;
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
                self.set_bits(pos / 64, 1 << (pos % 64), 1);
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

#[derive(Clone, Debug)]
pub struct EncodedPointerMap(Box<[u64]>);

impl EncodedPointerMap {
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl From<EncodedPointerMap> for Cow<'_, [u8]> {
    fn from(value: EncodedPointerMap) -> Self {
        unsafe {
            let len = value.0.len() * 8;
            let ptr = Box::into_raw(value.0);
            Cow::Owned(Vec::from_raw_parts(ptr as *mut u8, len, len))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PointerMap {
    data: Arc<PointerMapData>,
}

impl PointerMap {
    #[inline(always)]
    pub fn has_pointers(&self) -> bool {
        self.data.bits.iter().any(|v| *v != 0)
    }
}

impl PointerMap {
    pub fn encode(&self, force_inexact: bool) -> EncodedPointerMap {
        let size = self.data.bits.iter().rposition(|v| *v != 0).map_or(0, |n| n + 1);
        let total = size * (u64::BITS as usize);

        /* empty bitmap */
        if size == 0 {
            return EncodedPointerMap(vec![].into_boxed_slice());
        }

        /* sanity check */
        assert!(
            self.data
                .bits
                .iter()
                .zip(self.data.mask.iter())
                .zip(self.data.used.iter())
                .all(|((b, m), u)| (*b | *m) & !*u == 0),
            "found bits within unused area"
        );

        /* figure out if the bitmap is exact */
        let inexact = self.data.mask[..size].iter().any(|v| *v != 0) || force_inexact;
        let overflow = self.data.bits[size - 1].leading_zeros() as usize;

        /* allocate space for encoded bitmap */
        let mut mask = &self.data.mask[..size];
        let mut rbuf = Vec::with_capacity((size << (inexact as usize)) + 1);

        /* encode length and bitmap */
        rbuf.push((total - overflow) as u64);
        rbuf.extend_from_slice(&self.data.bits[..size]);

        /* check if the encoding is exact */
        if !inexact {
            return EncodedPointerMap(rbuf.into_boxed_slice());
        }

        /* mark all pointer bits as inexact if forced */
        if force_inexact {
            mask = &self.data.bits[..size];
        }

        /* add the inexact map if any */
        rbuf.extend_from_slice(mask);
        EncodedPointerMap(rbuf.into_boxed_slice())
    }

    pub fn exact_pointer_slots(&self) -> SmallVec<[usize; 16]> {
        self.data
            .bits
            .iter()
            .copied()
            .zip(self.data.mask.iter().copied())
            .zip(self.data.used.iter().copied())
            .enumerate()
            .flat_map(BitIter::from)
            .collect()
    }
}

impl PointerMap {
    pub fn resolve<'cx, 'tcx: 'cx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasParamEnv<'tcx>>(
        cx: &'cx Cx,
        layout: TyAndLayout<'tcx>,
    ) -> Self {
        let unit = cx.data_layout().pointer_size;
        let size = layout.size.align_to(cx.data_layout().pointer_align.abi);
        let mut data = PointerMapData::new(size.bytes_usize() / unit.bytes_usize());
        data.set_layout(cx, Size::ZERO, layout);
        Self { data: Arc::new(data) }
    }
}

pub trait HasPointerMap<'tcx> {
    fn pointer_map(&self, layout: TyAndLayout<'tcx>) -> PointerMap;
    fn encoded_pointer_map(&self, layout: TyAndLayout<'tcx>) -> EncodedPointerMap;
}

impl<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasParamEnv<'tcx>> HasPointerMap<'tcx> for Cx {
    fn pointer_map(&self, layout: TyAndLayout<'tcx>) -> PointerMap {
        self.tcx()
            .pointer_maps
            .borrow_mut()
            .entry(layout.ty)
            .or_insert_with(|| PointerMap::resolve(self, layout))
            .clone()
    }

    fn encoded_pointer_map(&self, layout: TyAndLayout<'tcx>) -> EncodedPointerMap {
        self.pointer_map(layout).encode(!ManagedChecker::new(self.tcx()).is_managed(layout.ty))
    }
}
