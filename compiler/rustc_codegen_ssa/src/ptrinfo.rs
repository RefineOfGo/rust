use std::ops::RangeInclusive;

use rustc_index::IndexVec;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_target::abi::{
    Abi, FieldsShape, Primitive, Scalar, Size, TagEncoding, VariantIdx, Variants,
};
use smallvec::{smallvec, SmallVec};

use crate::traits::CodegenMethods;

#[derive(Clone, Debug)]
pub enum Slot {
    Ptr,
    Int,
    Enum {
        tag_offs: Size,
        tag_size: Size,
        variants: IndexVec<VariantIdx, PointerMap>,
    },
    Niche {
        tag_offs: Size,
        tag_size: Size,
        variants: IndexVec<VariantIdx, PointerMap>,
        niche_start: u128,
        niche_variants: RangeInclusive<VariantIdx>,
        untagged_variant: VariantIdx,
    },
}

#[derive(Clone, Debug)]
pub struct PointerMap {
    pub slots: SmallVec<[Slot; 32]>,
}

impl PointerMap {
    pub fn int() -> Self {
        Self { slots: smallvec![Slot::Int] }
    }

    pub fn ptr() -> Self {
        Self { slots: smallvec![Slot::Ptr] }
    }
}

impl PointerMap {
    pub fn array(self, count: usize) -> Self {
        let size = self.slots.len() * count;
        let slots = self.slots.into_iter().cycle().take(size).collect();
        Self { slots }
    }
}

impl PointerMap {
    pub fn has_pointers(&self) -> bool {
        self.slots.iter().any(|slot| match slot {
            Slot::Ptr => true,
            Slot::Int => false,
            Slot::Enum { variants, .. } => variants.iter().any(Self::has_pointers),
            Slot::Niche { variants, .. } => variants.iter().any(Self::has_pointers),
        })
    }
}

impl PointerMap {
    fn from_layout_uncached<'tcx, Cx: CodegenMethods<'tcx>>(
        cx: &Cx,
        layout: TyAndLayout<'tcx>,
    ) -> Self {
        match layout.abi {
            // Unions does not have deterministic runtime type information, so
            // user must take care of their pointer variants. ROG GC won't deal
            // with those pointers.
            Abi::Scalar(Scalar::Initialized { value: Primitive::Pointer(_), .. })
            | Abi::ScalarPair(Scalar::Initialized { value: Primitive::Pointer(_), .. }, _)
            | Abi::ScalarPair(_, Scalar::Initialized { value: Primitive::Pointer(_), .. })
            | Abi::Vector {
                element: Scalar::Initialized { value: Primitive::Pointer(_), .. },
                ..
            } => Self::ptr(),
            Abi::Aggregate { .. } => match layout.fields {
                // Don't deal with unions.
                FieldsShape::Primitive | FieldsShape::Union(_) => Self::int(),
                FieldsShape::Array { count, .. } => {
                    let elem = layout.field(cx, 0);
                    if elem.size < cx.data_layout().pointer_size {
                        Self::int()
                    } else {
                        Self::from_layout(cx, elem).array(count as usize)
                    }
                }
                FieldsShape::Arbitrary { .. } => match &layout.variants {
                    Variants::Single { .. } => {
                        let unit = cx.data_layout().pointer_size.bytes_usize();
                        let align = cx.data_layout().pointer_align.abi;

                        let mut slots = smallvec![
                            Slot::Int;
                            layout.size.align_to(align).bytes_usize() / unit
                        ];

                        for i in layout.fields.index_by_increasing_offset() {
                            let offs = layout.fields.offset(i);
                            if !offs.is_aligned(align) {
                                continue;
                            }

                            let base = offs.bytes_usize() / unit;
                            Self::from_layout(cx, layout.field(cx, i))
                                .slots
                                .iter()
                                .enumerate()
                                .for_each(|(idx, slot)| slots[base + idx] = slot.clone());
                        }
                        Self { slots }
                    }
                    Variants::Multiple {
                        tag: Scalar::Initialized { value, .. },
                        tag_encoding: TagEncoding::Direct,
                        tag_field,
                        variants,
                        ..
                    } => Self {
                        slots: smallvec!(Slot::Enum {
                            tag_offs: layout.fields.offset(*tag_field),
                            tag_size: value.size(cx),
                            variants: variants
                                .indices()
                                .map(|i| Self::from_layout(cx, layout.for_variant(cx, i)))
                                .collect(),
                        }),
                    },
                    Variants::Multiple {
                        tag: Scalar::Initialized { value, .. },
                        tag_encoding:
                            TagEncoding::Niche { untagged_variant, niche_variants, niche_start },
                        tag_field,
                        variants,
                        ..
                    } => Self {
                        slots: smallvec!(Slot::Niche {
                            tag_offs: layout.fields.offset(*tag_field),
                            tag_size: value.size(cx),
                            variants: variants
                                .indices()
                                .map(|i| Self::from_layout(cx, layout.for_variant(cx, i)))
                                .collect(),
                            niche_start: *niche_start,
                            niche_variants: niche_variants.clone(),
                            untagged_variant: *untagged_variant,
                        }),
                    },
                    Variants::Multiple { tag, .. } => {
                        unreachable!("Union discriminant value: {:?}", tag)
                    }
                },
            },
            _ => Self::int(),
        }
    }
}

impl PointerMap {
    pub fn from_layout<'tcx, Cx: CodegenMethods<'tcx>>(cx: &Cx, layout: TyAndLayout<'tcx>) -> Self {
        if layout.size < cx.data_layout().pointer_size {
            return PointerMap { slots: smallvec![] };
        }
        if let Some(map) = cx.get_pointer_map(layout.ty) {
            return map;
        }
        let map = Self::from_layout_uncached(cx, layout);
        cx.add_pointer_map(layout.ty, map.clone());
        map
    }
}

pub fn may_contain_heap_ptr<'tcx, Cx: CodegenMethods<'tcx>>(
    cx: &Cx,
    layout: TyAndLayout<'tcx>,
) -> bool {
    PointerMap::from_layout(cx, layout).has_pointers()
}
