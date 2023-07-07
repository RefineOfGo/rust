use std::borrow::Cow;
use std::fmt::Debug;
use std::sync::Arc;

use rustc_abi::{
    BackendRepr, FieldsShape, HasDataLayout, Primitive, Scalar, Size, TagEncoding, VariantIdx,
    Variants,
};
use smallvec::{SmallVec, smallvec};

use crate::ty::layout::{HasTyCtxt, HasTypingEnv, TyAndLayout};

#[derive(Debug, Default, PartialEq, Eq)]
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
    fn is_no_ptr<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
        cx: &Cx,
        layout: TyAndLayout<'tcx>,
        variant_idx: Option<VariantIdx>,
    ) -> bool {
        if let Some(value) = cx.tcx().noptr_cache.lock().get(&(layout.ty, variant_idx)) {
            return *value;
        }
        if layout.is_uninhabited() {
            cx.tcx().noptr_cache.lock().insert((layout.ty, variant_idx), true);
            return true;
        }
        let no_ptr = match layout.fields {
            FieldsShape::Primitive => match layout.backend_repr {
                BackendRepr::Scalar(scalar) => !matches!(
                    scalar,
                    Scalar::Union { value: Primitive::Pointer(_) }
                        | Scalar::Initialized { value: Primitive::Pointer(_), .. }
                ),
                BackendRepr::ScalarPair(..) => unreachable!("conflict: Primitive & Scalar Pair"),
                BackendRepr::SimdVector { .. } => unreachable!("conflict: Primitive & SimdVector"),
                BackendRepr::Memory { .. } => unreachable!("conflict: Primitive & Memory"),
            },
            FieldsShape::Array { .. } => {
                let elem = layout.field(cx, 0);
                Self::is_no_ptr(cx, elem, None)
            }
            FieldsShape::Union(..) | FieldsShape::Arbitrary { .. } => {
                let mut is_no_ptr = layout
                    .fields
                    .index_by_increasing_offset()
                    .all(|index| Self::is_no_ptr(cx, layout.field(cx, index), None));
                if let Variants::Multiple { ref variants, .. } = layout.variants {
                    is_no_ptr &= variants.indices().all(|index| {
                        Self::is_no_ptr(cx, layout.for_variant(cx, index), Some(index))
                    });
                }
                is_no_ptr
            }
        };
        cx.tcx().noptr_cache.lock().insert((layout.ty, variant_idx), no_ptr);
        no_ptr
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
        let align = cx.data_layout().pointer_size().bytes_usize();

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

    fn set_exact<Cx: HasDataLayout>(&mut self, cx: &Cx, offset: Size) {
        let pos = offset.bytes_usize() / cx.data_layout().pointer_size().bytes_usize();
        let (idx, mask) = (pos / 64, 1 << (pos % 64));
        assert!(self.used[idx] & mask != 0 && self.bits[idx] & mask != 0);
        self.mask[idx] &= !mask;
    }

    fn set_scalar<Cx: HasDataLayout>(&mut self, cx: &Cx, offset: Size, scalar: Scalar) {
        match scalar {
            Scalar::Union { value: Primitive::Pointer(_) }
            | Scalar::Initialized { value: Primitive::Pointer(_), .. }
                if offset.is_aligned(cx.data_layout().pointer_align().abi) =>
            {
                let pos = offset.bytes_usize() / cx.data_layout().pointer_size().bytes_usize();
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
        variant_idx: Option<VariantIdx>,
    ) {
        if Self::is_no_ptr(cx, layout, variant_idx) {
            self.set_noptr(cx, offset, layout.size);
            return;
        }
        match layout.fields {
            FieldsShape::Primitive => match layout.backend_repr {
                BackendRepr::Scalar(scalar) => self.set_scalar(cx, offset, scalar),
                BackendRepr::ScalarPair(..) => unreachable!("conflict: Primitive & Scalar Pair"),
                BackendRepr::SimdVector { .. } => unreachable!("conflict: Primitive & SimdVector"),
                BackendRepr::Memory { .. } => unreachable!("conflict: Primitive & Memory"),
            },
            FieldsShape::Array { stride, count } => {
                let elem = layout.field(cx, 0);
                for i in 0..count {
                    self.set_layout(cx, offset + stride * i, elem, None);
                }
            }
            FieldsShape::Union(..) | FieldsShape::Arbitrary { .. } => {
                let min_field = layout
                    .fields
                    .index_by_increasing_offset()
                    .map(|i| {
                        let offs = layout.fields.offset(i);
                        let field = layout.field(cx, i);
                        self.set_layout(cx, offset + offs, field, None);
                        field.size
                    })
                    .min()
                    .unwrap_or(Size::ZERO);
                if layout.ty.is_union() && min_field != layout.size {
                    assert!(min_field < layout.size);
                    self.set_noptr(cx, offset + min_field, layout.size - min_field);
                }
                if let Variants::Multiple {
                    tag, ref tag_encoding, tag_field, ref variants, ..
                } = layout.variants
                {
                    let min_variant = variants
                        .indices()
                        .map(|i| {
                            let variant = layout.for_variant(cx, i);
                            self.set_layout(cx, offset, variant, Some(i));
                            variant.size
                        })
                        .min()
                        .unwrap_or_else(|| bug!("multiple variant without any variants"));
                    if min_variant != layout.size {
                        assert!(min_variant < layout.size);
                        self.set_noptr(cx, offset + min_variant, layout.size - min_variant);
                    }
                    if let TagEncoding::Niche { niche_start, .. } = tag_encoding
                        && let Scalar::Initialized { value: Primitive::Pointer(..), .. } = tag
                        && *niche_start == 0
                        && variants.len() <= 2
                    {
                        let index = tag_field.index();
                        assert_eq!(layout.field(cx, index).size, cx.data_layout().pointer_size());
                        self.set_exact(cx, offset + layout.fields.offset(index));
                    }
                }
            }
        }
    }
}

impl PointerMapData {
    fn validate_and_trim(&mut self) {
        assert!(
            self.bits
                .iter()
                .zip(self.mask.iter())
                .zip(self.used.iter())
                .all(|((&b, &m), &u)| (b | m) & !u == 0),
            "found pointer or mask bits within unused area"
        );
        let valid_len = self
            .bits
            .iter()
            .copied()
            .rposition(|slot| slot != 0)
            .map(|index| index + 1)
            .unwrap_or(0);
        self.bits.truncate(valid_len);
        self.mask.truncate(valid_len);
        self.used.truncate(valid_len);
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

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PointerMap {
    data: Arc<PointerMapData>,
}

impl PointerMap {
    pub fn is_exact(&self) -> bool {
        self.data.mask.iter().all(|v| *v == 0)
    }
}

impl PointerMap {
    pub fn encode(&self) -> EncodedPointerMap {
        assert_ne!(self.data.bits.last(), Some(&0));
        EncodedPointerMap(self.data.bits.clone().into_boxed_slice())
    }
}

impl PointerMap {
    pub fn resolve<'cx, 'tcx: 'cx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
        cx: &'cx Cx,
        layout: TyAndLayout<'tcx>,
    ) -> Self {
        if PointerMapData::is_no_ptr(cx, layout, None) {
            return Self::default();
        }
        let ptr_size = cx.data_layout().pointer_size();
        let ptr_align = cx.data_layout().pointer_align().abi;
        assert!(layout.align.abi >= ptr_align && layout.size.is_aligned(layout.align.abi));
        let num_slots = layout.size.bytes_usize() / ptr_size.bytes_usize();
        let mut ptrmap = PointerMapData::new(num_slots);
        ptrmap.set_layout(cx, Size::ZERO, layout, None);
        ptrmap.validate_and_trim();
        Self { data: Arc::new(ptrmap) }
    }
}

pub trait HasPointerMap<'tcx> {
    fn pointer_map(&self, layout: TyAndLayout<'tcx>) -> PointerMap;
    fn has_pointers(&self, layout: TyAndLayout<'tcx>) -> bool;
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

    fn has_pointers(&self, layout: TyAndLayout<'tcx>) -> bool {
        !PointerMapData::is_no_ptr(self, layout, None)
    }
}
