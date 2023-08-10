use std::{
    fmt::{Debug, Formatter, Result as FmtResult},
    ops::RangeInclusive,
};

use rustc_data_structures::fx::FxHashMap;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_target::abi::{
    Abi, FieldsShape, Primitive, Scalar, Size, TagEncoding, VariantIdx, Variants,
};
use smallvec::{smallvec, SmallVec};

use crate::traits::CodegenMethods;

#[derive(Clone)]
pub enum Slot {
    Void,
    Ptr,
    Int(Size),
    Enum {
        max_size: Size,
        tag_offs: Size,
        tag_size: Size,
        variants: FxHashMap<u128, PointerMap>,
    },
    Niche {
        max_size: Size,
        tag_offs: Size,
        tag_size: Size,
        variants: FxHashMap<u128, PointerMap>,
        niche_start: u128,
        niche_variants: RangeInclusive<VariantIdx>,
        untagged_variant: VariantIdx,
    },
}

impl Debug for Slot {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Void => write!(f, "_"),
            Self::Ptr => write!(f, "ptr"),
            Self::Int(size) => write!(f, "i{}", size.bits_usize()),
            Self::Enum { max_size, tag_offs, tag_size, variants } => f
                .debug_struct("Enum")
                .field("max_size", &max_size.bytes_usize())
                .field("tag_offs", &tag_offs.bytes_usize())
                .field("tag_size", &tag_size.bytes_usize())
                .field("variants", variants)
                .finish(),
            Self::Niche {
                max_size,
                tag_offs,
                tag_size,
                variants,
                niche_start,
                niche_variants,
                untagged_variant,
            } => f
                .debug_struct("Niche")
                .field("max_size", &max_size.bytes_usize())
                .field("tag_offs", &tag_offs.bytes_usize())
                .field("tag_size", &tag_size.bytes_usize())
                .field("variants", variants)
                .field("niche_start", niche_start)
                .field("niche_variants", niche_variants)
                .field("untagged_variant", untagged_variant)
                .finish(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PointerMap {
    pub slots: SmallVec<[Slot; 32]>,
}

impl PointerMap {
    pub fn encode(&self) -> Vec<u8> {
        todo!();
    }
}

impl PointerMap {
    pub fn has_pointers(&self) -> bool {
        self.slots.iter().any(|slot| match slot {
            Slot::Ptr => true,
            Slot::Void | Slot::Int(_) => false,
            Slot::Enum { variants, .. } => variants.values().any(Self::has_pointers),
            Slot::Niche { variants, .. } => variants.values().any(Self::has_pointers),
        })
    }
}

impl PointerMap {
    fn tag_of<'tcx, Cx: CodegenMethods<'tcx>>(
        cx: &Cx,
        layout: TyAndLayout<'tcx>,
        variant_idx: VariantIdx,
    ) -> u128 {
        layout
            .ty
            .discriminant_for_variant(cx.tcx(), variant_idx)
            .map_or(variant_idx.as_u32() as u128, |d| d.val)
    }
}

impl PointerMap {
    fn set(&mut self, offset: Size, idx: usize, slot: Slot) {
        self.slots[offset.bytes_usize() + idx] = slot;
    }

    fn set_submap(&mut self, offset: Size, submap: PointerMap) {
        submap.slots.iter().enumerate().for_each(|(idx, slot)| {
            self.set(offset, idx, slot.clone());
        });
    }

    fn set_scalar<'tcx, Cx: CodegenMethods<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        scalar: Scalar,
    ) {
        match scalar {
            Scalar::Initialized { value, .. } => {
                if matches!(value, Primitive::Pointer(_)) {
                    self.set(offset, 0, Slot::Ptr);
                } else {
                    self.set(offset, 0, Slot::Int(value.size(cx)));
                }
            }
            // Unions does not have deterministic runtime type information, so
            // user must take care of their pointer variants. ROG GC won't deal
            // with those pointers.
            Scalar::Union { value } => {
                self.set(offset, 0, Slot::Int(value.size(cx)));
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
                for i in 0..count {
                    self.set_scalar(
                        cx,
                        offset + (element.size(cx) * i).align_to(element.align(cx).abi),
                        element,
                    )
                }
            }
            Abi::Aggregate { .. } => match layout.fields {
                // Treat unions as opaque data, as described above.
                FieldsShape::Primitive | FieldsShape::Union(_) => {
                    self.set(offset, 0, Slot::Int(layout.size));
                }
                FieldsShape::Array { count, .. } => {
                    let elem = Self::resolve(cx, layout.field(cx, 0), None);
                    let size = Size::from_bytes(elem.slots.len());
                    for i in 0..count {
                        self.set_submap(offset + size * i, elem.clone());
                    }
                }
                FieldsShape::Arbitrary { .. } => match layout.variants {
                    Variants::Single { .. } => {
                        for i in layout.fields.index_by_increasing_offset() {
                            Self::resolve(cx, layout.field(cx, i), None)
                                .slots
                                .iter()
                                .enumerate()
                                .for_each(|(idx, slot)| {
                                    self.set(offset + layout.fields.offset(i), idx, slot.clone())
                                });
                        }
                    }
                    Variants::Multiple {
                        tag: Scalar::Initialized { value, .. },
                        tag_encoding: TagEncoding::Direct,
                        tag_field,
                        ref variants,
                        ..
                    } => self.set(
                        offset,
                        0,
                        Slot::Enum {
                            max_size: layout.size,
                            tag_offs: layout.fields.offset(tag_field),
                            tag_size: value.size(cx),
                            variants: variants
                                .indices()
                                .map(|i| {
                                    (
                                        Self::tag_of(cx, layout, i),
                                        Self::resolve(cx, layout, Some(i)),
                                    )
                                })
                                .collect(),
                        },
                    ),
                    Variants::Multiple {
                        tag: Scalar::Initialized { value, .. },
                        tag_encoding:
                            TagEncoding::Niche { untagged_variant, ref niche_variants, niche_start },
                        tag_field,
                        ref variants,
                        ..
                    } => self.set(
                        offset,
                        0,
                        Slot::Niche {
                            max_size: layout.size,
                            tag_offs: layout.fields.offset(tag_field),
                            tag_size: value.size(cx),
                            variants: variants
                                .indices()
                                .map(|i| {
                                    (
                                        Self::tag_of(cx, layout, i),
                                        Self::resolve(cx, layout, Some(i)),
                                    )
                                })
                                .collect(),
                            niche_start,
                            niche_variants: niche_variants.clone(),
                            untagged_variant,
                        },
                    ),
                    Variants::Multiple { tag, .. } => {
                        unreachable!("Union discriminant value: {:#?}", tag)
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
        let mut ret = Self {
            slots: smallvec![
                Slot::Void;
                layout.size.bytes_usize()
            ],
        };
        ret.set_layout(cx, Size::ZERO, layout);
        eprintln!("ty({:?}) {:#?} :: {:?} -> PointerMap {:#?}", layout.size, ty, variant_idx, &ret);
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
