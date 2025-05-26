use std::borrow::Cow;
use std::fmt::Debug;
use std::sync::Arc;

use num_integer::Integer;
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
    ) -> bool {
        Self::is_variant_no_ptr(cx, layout, None)
    }

    fn is_variant_no_ptr<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
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
                Self::is_no_ptr(cx, elem)
            }
            FieldsShape::Union(..) | FieldsShape::Arbitrary { .. } => {
                let mut is_no_ptr = layout
                    .fields
                    .index_by_increasing_offset()
                    .all(|index| Self::is_no_ptr(cx, layout.field(cx, index)));
                if let Variants::Multiple { ref variants, .. } = layout.variants {
                    is_no_ptr &= variants.indices().all(|index| {
                        Self::is_variant_no_ptr(cx, layout.for_variant(cx, index), Some(index))
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

    fn set_exact<Cx: HasDataLayout>(&mut self, cx: &Cx, offset: Size) {
        let pos = offset.bytes_usize() / cx.data_layout().pointer_size.bytes_usize();
        let (idx, mask) = (pos / 64, 1 << (pos % 64));
        assert!(self.used[idx] & mask != 0 && self.bits[idx] & mask != 0);
        self.mask[idx] &= !mask;
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
        if layout.is_uninhabited() || Self::is_no_ptr(cx, layout) {
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
                if let Variants::Multiple {
                    tag, ref tag_encoding, tag_field, ref variants, ..
                } = layout.variants
                {
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
                    if let TagEncoding::Niche { niche_start, .. } = tag_encoding
                        && let Scalar::Initialized { value: Primitive::Pointer(..), .. } = tag
                        && *niche_start == 0
                        && variants.len() <= 2
                    {
                        assert_eq!(layout.field(cx, tag_field).size, cx.data_layout().pointer_size);
                        self.set_exact(cx, offset + layout.fields.offset(tag_field));
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

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PointerMap {
    data: Arc<PointerMapData>,
    num_slots: usize,
}

impl PointerMap {
    fn find_period(data: &[u64], size: usize) -> usize {
        let mut p = vec![0; size];
        let bit_at = |i: usize| data[i / 64] & (1 << (i % 64)) != 0;

        /* build the prefix function by KMP */
        for i in 1..size {
            let mut j = p[i - 1];
            while j > 0 && bit_at(i) != bit_at(j) {
                j = p[j - 1];
            }
            if bit_at(i) == bit_at(j) {
                j += 1;
            }
            p[i] = j;
        }

        /* calculate the period */
        let period = size - p[size - 1];
        if size % period == 0 { period } else { size }
    }

    fn encode_bitmap(bits: &[u64], mask: &[u64], num_slots: usize) -> Vec<u64> {
        let is_exact = mask.iter().all(|v| *v == 0);
        let mut data = Vec::with_capacity(bits.len() + mask.len() + 3);

        /* fits into a single slot */
        if num_slots <= 64 {
            if is_exact {
                assert!(bits[0] != 0);
                data.extend([0u64, num_slots as u64, 1u64, bits[0]]);
                return data;
            } else {
                assert!(bits[0] != 0 && mask[0] != 0);
                data.extend([4u64, num_slots as u64, 1u64, bits[0], 1u64, mask[0]]);
                return data;
            }
        }

        /* find the period of bits & mask */
        let bits_period = Self::find_period(bits, num_slots);
        let mask_period = Self::find_period(mask, num_slots);

        /* calculate the final period & repeat count */
        let mut period = bits_period.lcm(&mask_period);
        let (mut repeat, tails) = num_slots.div_rem(&period);

        /* sanity check */
        assert!(tails == 0, "irregular period: {num_slots}:{period}");
        assert!(repeat != 0, "repeats zero times: {num_slots}:{period}");
        assert!(period <= u32::MAX as usize, "struct is too large: {num_slots}:{period}");

        /* the maximum possible factor */
        let mut new_bits;
        let mut new_mask;
        let mut div_factor = 64 / period;

        /* find the max factor of repeat count */
        while div_factor != 0 && repeat % div_factor != 0 {
            div_factor -= 1;
        }

        /* pack as much as possible into a single slot */
        let (bits, mask) = {
            if div_factor > 1 {
                new_bits = bits[0] & ((1 << period) - 1);
                new_mask = if is_exact { 0 } else { mask[0] & ((1 << period) - 1) };
                assert!(period * div_factor <= 64);

                /* expand the bits & masks */
                for i in 0..div_factor {
                    new_bits |= new_bits << (i * period);
                    new_mask |= new_mask << (i * period);
                }

                /* scale both factors */
                period *= div_factor;
                repeat /= div_factor;
                (std::slice::from_ref(&new_bits), std::slice::from_ref(&new_mask))
            } else {
                (bits, mask)
            }
        };

        /* encode the map header & pointer bits */
        data.push(0);
        data.push((((repeat - 1) << 32) | period) as u64);
        Self::compress_bitmap(&mut data, bits, period);

        /* exact pointer map does not have the inexactness map */
        if is_exact {
            return data;
        }

        /* encode the inexactness map */
        data[0] = data.len() as u64;
        Self::compress_bitmap(&mut data, mask, period);
        data
    }

    fn compress_bitmap(dest: &mut Vec<u64>, data: &[u64], mut num_bits: usize) {
        let first_run = dest.len();
        let initial_zero = data[0] == 0;

        /* RLE encoder state */
        let mut i = 0;
        let mut j = first_run;
        let mut zero = initial_zero;

        /* add the initial run length */
        assert!(num_bits != 0);
        dest.push(0);

        /* this is a variant of the run-length encoding which only compresses consecutive zeros */
        while num_bits != 0 {
            let m = if num_bits < 64 { (1 << num_bits) - 1 } else { u64::MAX };
            let v = data[i] & m;

            /* count up the run */
            if (v == 0) != zero {
                j = dest.len();
                zero = !zero;
                dest.push(1);
            } else {
                assert!(dest[j] < i64::MAX as u64);
                dest[j] += 1;
            }

            /* add non-zero blocks to the output */
            if v != 0 {
                dest.push(v);
            }

            /* advance to the next bits */
            i += 1;
            num_bits -= num_bits.min(64);
        }

        /* mark the initial run type, we don't really need a terminator,
         * because the pointer map header already tells the total bit count */
        assert!(dest[first_run] <= i64::MAX as u64);
        dest[first_run] |= (initial_zero as u64) << 63;
    }
}

impl PointerMap {
    pub fn encode(&self) -> EncodedPointerMap {
        assert!(
            self.data
                .bits
                .iter()
                .zip(self.data.mask.iter())
                .zip(self.data.used.iter())
                .all(|((b, m), u)| (*b | *m) & !*u == 0),
            "found bits within unused area"
        );
        let data = {
            if self.data.bits.iter().any(|v| *v != 0) {
                Self::encode_bitmap(&self.data.bits, &self.data.mask, self.num_slots)
            } else {
                vec![]
            }
        };
        EncodedPointerMap(data.into_boxed_slice())
    }
}

impl PointerMap {
    pub fn resolve<'cx, 'tcx: 'cx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
        cx: &'cx Cx,
        layout: TyAndLayout<'tcx>,
    ) -> Self {
        if PointerMapData::is_no_ptr(cx, layout) {
            return Self::default();
        }
        let ptr_size = cx.data_layout().pointer_size;
        let ptr_align = cx.data_layout().pointer_align.abi;
        assert!(layout.align.abi >= ptr_align && layout.size.is_aligned(layout.align.abi));
        let num_slots = layout.size.bytes_usize() / ptr_size.bytes_usize();
        let mut ptrmap = PointerMapData::new(num_slots);
        ptrmap.set_layout(cx, Size::ZERO, layout);
        Self { data: Arc::new(ptrmap), num_slots }
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
        !PointerMapData::is_no_ptr(self, layout)
    }
}
