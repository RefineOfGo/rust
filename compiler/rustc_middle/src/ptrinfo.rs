use std::fmt::Debug;
use std::sync::Arc;

use rustc_abi::{
    BackendRepr, Endian, FieldsShape, HasDataLayout, Primitive, Scalar, Size, TagEncoding,
    VariantIdx, Variants,
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
        let key = (cx.typing_env(), layout.ty, variant_idx);
        if let Some(value) = cx.tcx().noptr_cache.lock().get(&key) {
            return *value;
        }
        if layout.is_uninhabited() {
            cx.tcx().noptr_cache.lock().insert(key, true);
            return true;
        }
        let no_ptr = match layout.fields {
            FieldsShape::Primitive => match layout.backend_repr {
                BackendRepr::Scalar(scalar) => !matches!(
                    scalar,
                    Scalar::Union { value: Primitive::Pointer(_) }
                        | Scalar::Initialized { value: Primitive::Pointer(_), .. }
                ),
                BackendRepr::ScalarPair { .. } => bug!("conflict: Primitive & Scalar Pair"),
                BackendRepr::SimdScalableVector { .. } => {
                    bug!("conflict: Primitive & SimdScalableVector")
                }
                BackendRepr::SimdVector { .. } => bug!("conflict: Primitive & SimdVector"),
                BackendRepr::Memory { .. } => bug!("conflict: Primitive & Memory"),
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
        cx.tcx().noptr_cache.lock().insert(key, no_ptr);
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
        let pointer_size = cx.data_layout().pointer_size().bytes_usize();
        if !offset.bytes_usize().is_multiple_of(pointer_size) {
            return;
        }
        let pos = offset.bytes_usize() / pointer_size;
        let (idx, mask) = (pos / 64, 1 << (pos % 64));
        assert!(self.used[idx] & mask != 0 && self.bits[idx] & mask != 0);
        self.mask[idx] &= !mask;
    }

    fn set_scalar<Cx: HasDataLayout>(&mut self, cx: &Cx, offset: Size, scalar: Scalar) {
        if let Primitive::Pointer(address_space) = scalar.primitive() {
            let slot_size = cx.data_layout().pointer_size().bytes_usize();
            let scalar_size = scalar.size(cx).bytes_usize();
            let start = offset.bytes_usize();
            if address_space == cx.data_layout().default_address_space
                && scalar_size == slot_size
                && start.is_multiple_of(slot_size)
            {
                let pos = start / slot_size;
                self.set_bits(pos / 64, 1 << (pos % 64), 1);
            } else {
                let first = start / slot_size;
                let last = (start + scalar_size - 1) / slot_size;
                for pos in first..=last {
                    let (idx, mask) = (pos / 64, 1 << (pos % 64));
                    self.set_bits(idx, mask, 1);
                    self.mask[idx] |= mask;
                }
            }
        } else {
            self.set_noptr(cx, offset, scalar.size(cx));
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
                BackendRepr::ScalarPair { .. } => bug!("conflict: Primitive & Scalar Pair"),
                BackendRepr::SimdScalableVector { .. } => {
                    bug!("conflict: Primitive & SimdScalableVector")
                }
                BackendRepr::SimdVector { .. } => bug!("conflict: Primitive & SimdVector"),
                BackendRepr::Memory { .. } => bug!("conflict: Primitive & Memory"),
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
                        && let Scalar::Initialized {
                            value: Primitive::Pointer(address_space), ..
                        } = tag
                        && address_space == cx.data_layout().default_address_space
                        && tag.size(cx) == cx.data_layout().pointer_size()
                        && *niche_start == 0
                        && variants.len() <= 2
                    {
                        let index = tag_field.index();
                        if layout.field(cx, index).size == cx.data_layout().pointer_size() {
                            self.set_exact(cx, offset + layout.fields.offset(index));
                        }
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

    pub fn into_target_bytes(self, endian: Endian) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(self.0.len() * size_of::<u64>());
        for word in self.0 {
            match endian {
                Endian::Little => bytes.extend_from_slice(&word.to_le_bytes()),
                Endian::Big => bytes.extend_from_slice(&word.to_be_bytes()),
            }
        }
        bytes
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
        let num_slots = layout.size.bytes().div_ceil(ptr_size.bytes()) as usize;
        let mut ptrmap = PointerMapData::new(num_slots);
        ptrmap.set_layout(cx, Size::ZERO, layout, None);
        ptrmap.validate_and_trim();
        Self { data: Arc::new(ptrmap) }
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
            .entry((self.typing_env(), layout.ty))
            .or_insert_with(|| PointerMap::resolve(self, layout))
            .clone()
    }
}
