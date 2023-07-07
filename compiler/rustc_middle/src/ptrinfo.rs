use std::borrow::Cow;
use std::fmt::Debug;
use std::sync::Arc;

use rustc_abi::{BackendRepr, FieldsShape, HasDataLayout, Primitive, Scalar, Size, Variants};
use smallvec::{SmallVec, smallvec};

use crate::ty::layout::{HasTyCtxt, HasTypingEnv, TyAndLayout};

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

    fn set_noptr<Cx: HasDataLayout>(&mut self, cx: &Cx, offset: Size, size: Size) {
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

    fn set_scalar<Cx: HasDataLayout>(&mut self, cx: &Cx, offset: Size, scalar: Scalar) {
        match scalar {
            Scalar::Union { value: Primitive::Pointer(_) }
            | Scalar::Initialized { value: Primitive::Pointer(_), .. }
                if offset.is_aligned(cx.data_layout().pointer_align.abi) =>
            {
                let pos = offset.bytes_usize() / cx.data_layout().pointer_size.bytes_usize();
                self.set_bits(pos / 64, 1 << (pos % 64), 1);
            }
            _ => {
                self.set_noptr(cx, offset, scalar.size(cx));
            }
        }
    }

    fn set_layout<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        layout: TyAndLayout<'tcx>,
    ) {
        if layout.is_uninhabited() {
            self.set_noptr(cx, offset, layout.size);
            return;
        }
        match layout.fields {
            FieldsShape::Primitive => match layout.backend_repr {
                BackendRepr::Scalar(scalar) => self.set_scalar(cx, offset, scalar),
                BackendRepr::ScalarPair(..) => unreachable!("conflict: Primitive & Scalar Pair"),
                BackendRepr::Vector { .. } => unreachable!("conflict: Primitive & Vector"),
                BackendRepr::Memory { .. } => unreachable!("conflict: Primitive & Memory"),
            },
            FieldsShape::Array { stride, count } => {
                let elem = layout.field(cx, 0);
                for i in 0..count {
                    self.set_layout(cx, offset + stride * i, elem);
                }
            }
            FieldsShape::Union(..) | FieldsShape::Arbitrary { .. } => {
                let min_field = layout
                    .fields
                    .index_by_increasing_offset()
                    .map(|i| {
                        let offs = layout.fields.offset(i);
                        let field = layout.field(cx, i);
                        self.set_layout(cx, offset + offs, field);
                        field.size
                    })
                    .min()
                    .unwrap_or(Size::ZERO);
                if layout.ty.is_union() && min_field != layout.size {
                    assert!(min_field < layout.size);
                    self.set_noptr(cx, offset + min_field, layout.size - min_field);
                }
                if let Variants::Multiple { ref variants, .. } = layout.variants {
                    let min_variant = variants
                        .indices()
                        .map(|i| {
                            let variant = layout.for_variant(cx, i);
                            self.set_layout(cx, offset, variant);
                            variant.size
                        })
                        .min()
                        .unwrap_or_else(|| bug!("multiple variant without any variants"));
                    if min_variant != layout.size {
                        assert!(min_variant < layout.size);
                        self.set_noptr(cx, offset + min_variant, layout.size - min_variant);
                    }
                }
            }
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
    pub fn encode(&self) -> EncodedPointerMap {
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
        let inexact = self.data.mask[..size].iter().any(|v| *v != 0);
        let overflow = self.data.bits[size - 1].leading_zeros() as usize;

        /* allocate space for encoded bitmap */
        let slots = (total - overflow) as u64;
        let mut rbuf = Vec::with_capacity((size << (inexact as usize)) + 1);

        /* encode flags, length and bitmap */
        rbuf.push(((inexact as u64) << 63) | slots);
        rbuf.extend_from_slice(&self.data.bits[..size]);

        /* check if the encoding is exact */
        if !inexact {
            return EncodedPointerMap(rbuf.into_boxed_slice());
        }

        /* add the inexact map if any */
        rbuf.extend_from_slice(&self.data.mask[..size]);
        EncodedPointerMap(rbuf.into_boxed_slice())
    }
}

impl PointerMap {
    pub fn resolve<'cx, 'tcx: 'cx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
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
}

impl<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>> HasPointerMap<'tcx> for Cx {
    fn pointer_map(&self, layout: TyAndLayout<'tcx>) -> PointerMap {
        self.tcx()
            .pointer_maps
            .borrow_mut()
            .entry(layout.ty)
            .or_insert_with(|| PointerMap::resolve(self, layout))
            .clone()
    }
}
