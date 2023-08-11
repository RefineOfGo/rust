use std::{
    fmt::{Debug, DebugStruct, Formatter, Result as FmtResult},
    ops::RangeInclusive,
};

use itertools::Itertools;
use rustc_data_structures::{fx::FxHashMap, stack::ensure_sufficient_stack};
use rustc_middle::ty::layout::TyAndLayout;
use rustc_target::abi::{
    Abi, FieldsShape, Primitive, Scalar, Size, TagEncoding, VariantIdx, Variants,
};
use smallvec::{smallvec, SmallVec};

use crate::traits::CodegenMethods;

use super::bitvec::{BitmapData, CompressedBitVec};

#[derive(Clone, PartialEq, Eq)]
pub struct Enum {
    pub max_size: Size,
    pub tag_offs: Size,
    pub tag_size: Size,
    pub variants: FxHashMap<u128, PointerMap>,
}

impl Enum {
    fn debug_fields(&self, f: &mut DebugStruct<'_, '_>) {
        f.field("max_size", &self.max_size.bytes_usize())
            .field("tag_offs", &self.tag_offs.bytes_usize())
            .field("tag_size", &self.tag_size.bytes_usize())
            .field("variants", &self.variants);
    }
}

impl Debug for Enum {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut dx = f.debug_struct("Enum");
        self.debug_fields(&mut dx);
        dx.finish()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Niche {
    pub niche_start: u128,
    pub niche_variants: RangeInclusive<VariantIdx>,
    pub untagged_variant: VariantIdx,
}

impl Niche {
    fn debug_fields(&self, f: &mut DebugStruct<'_, '_>) {
        f.field("niche_start", &self.niche_start)
            .field("niche_variants", &self.niche_variants)
            .field("untagged_variant", &self.untagged_variant);
    }
}

impl Debug for Niche {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut dx = f.debug_struct("Niche");
        self.debug_fields(&mut dx);
        dx.finish()
    }
}

/// Pointer Map Calculation

#[derive(Clone, PartialEq, Eq)]
pub enum Slot {
    Void(Size),
    Ptr(usize),
    Int(Size),
    Enum(Enum, Option<Niche>),
}

impl Debug for Slot {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Void(size) => match size.bytes() {
                0 => bug!("Void slot with zero size"),
                1 => write!(f, "void"),
                n => write!(f, "void*{}", n),
            },
            Self::Ptr(rep) => match *rep {
                0 => bug!("Pointer slot with zero size"),
                1 => write!(f, "ptr"),
                n => write!(f, "ptr*{}", n),
            },
            Self::Int(size) => write!(f, "i{}", size.bits_usize()),
            Self::Enum(ex, None) => {
                let mut dx = f.debug_struct("Enum");
                ex.debug_fields(&mut dx);
                dx.finish()
            }
            Self::Enum(ex, Some(nx)) => {
                let mut dx = f.debug_struct("Niche");
                ex.debug_fields(&mut dx);
                nx.debug_fields(&mut dx);
                dx.finish()
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PointerMap {
    pub size: Size,
    pub slots: SmallVec<[Slot; 32]>,
}

impl PointerMap {
    pub fn has_pointers(&self) -> bool {
        self.slots.iter().any(|slot| match slot {
            Slot::Ptr(_) => true,
            Slot::Int(_) | Slot::Void(_) => false,
            Slot::Enum(ex, ..) => ex.variants.values().any(Self::has_pointers),
        })
    }
}

impl PointerMap {
    pub fn encode<'tcx, Cx: CodegenMethods<'tcx>>(&self, cx: &Cx) -> BitmapData {
        self.clone().into_encoded(cx)
    }

    pub fn into_encoded<'tcx, Cx: CodegenMethods<'tcx>>(mut self, cx: &Cx) -> BitmapData {
        ensure_sufficient_stack(|| {
            let mut out = CompressedBitVec::new();
            self.legalize(cx);
            self.simplify();
            self.compress();
            // TODO: remove this
            eprintln!("compressed pointer map: {:#?}", self);
            self.monolize();
            self.serialize_into(cx, &mut out, Size::ZERO);
            out.finish()
        })
    }
}

impl PointerMap {
    fn serialize_into<'tcx, Cx: CodegenMethods<'tcx>>(
        self,
        cx: &Cx,
        out: &mut CompressedBitVec,
        mut offs: Size,
    ) {
        let ptr_size = cx.data_layout().pointer_size;
        let ptr_align = cx.data_layout().pointer_align.abi;

        /* common case 1: type without pointers encodes to a single byte */
        if !self.has_pointers() {
            out.add_noptr(self.size.bytes_usize());
            return;
        }

        /* common case 2: types that with at most 4 pointer slots and is
         * pointer-size aligned encodes into a single byte */
        if self.size <= ptr_size * 4
            && self.size.is_aligned(ptr_align)
            && self.slots.iter().all(|slot| !matches!(slot, Slot::Enum(_, _)))
        {
            assert!(self.slots.len() <= 4, "Uncompressed pointer map");
            let unit = ptr_size.bytes_usize();
            let slots: SmallVec<[bool; 4]> = self
                .slots
                .into_iter()
                .map(|slot| -> SmallVec<[bool; 4]> {
                    match slot {
                        Slot::Void(_) => bug!("Illegal pointer map with void slots: {:?}", slot),
                        Slot::Ptr(rep) => smallvec![true; rep],
                        Slot::Int(size) => smallvec![false; size.bytes_usize() / unit],
                        Slot::Enum(_, _) => unreachable!(),
                    }
                })
                .flatten()
                .collect();
            out.add_simple(&slots);
            return;
        }

        /* other more complex types */
        let start = offs;
        out.add_complex(self.size.bytes_usize());

        for slot in self.slots {
            match slot {
                Slot::Void(_) => {
                    bug!("Illegal pointer map with void slots: {:?}", slot);
                }
                Slot::Ptr(rep) => {
                    assert_ne!(rep, 0, "Pointer slot with zero size");
                    assert!(offs.is_aligned(ptr_align), "Unaligned pointer slot");
                    out.add_ptr(rep);
                    offs += ptr_size * (rep as u64);
                }
                Slot::Int(size) => {
                    assert_ne!(size, Size::ZERO, "Integer slot with zero size");
                    out.add_int(size.bytes_usize() / ptr_size.bytes_usize());
                    offs += size;
                }
                Slot::Enum(value, niche) => {
                    assert!(!value.variants.is_empty(), "Empty enum");
                    assert_ne!(value.max_size, Size::ZERO, "Enum slot with zero size");
                    out.add_enum_header(&value, niche);
                    value.variants.into_iter().sorted_by_key(|(tag, _)| *tag).for_each(
                        |(tag, map)| {
                            out.add_enum_tag(tag);
                            map.serialize_into(cx, out, offs);
                        },
                    );
                    offs += value.max_size;
                }
            }
        }
        assert_eq!(offs - start, self.size, "Size mismatch for encoded pointer map");
    }
}

impl PointerMap {
    fn slot_pair(&mut self, i: usize) -> (&mut Slot, &mut Slot) {
        assert!(i < self.slots.len(), "First slot index out of bounds");
        assert!(i + 1 < self.slots.len(), "Second slot index out of bounds");
        unsafe {
            let p = self.slots.as_mut_ptr();
            (&mut *p.add(i), &mut *p.add(i + 1))
        }
    }

    fn void_slots(&self, pos: usize, slot_count: usize) -> Option<usize> {
        if pos + slot_count > self.slots.len() {
            None
        } else if slot_count == 1 {
            Some(1)
        } else {
            self.slots
                .iter()
                .skip(pos + 1)
                .take(slot_count - 1)
                .all(|slot| match slot {
                    Slot::Void(size) if size.bytes() == 1 => true,
                    Slot::Void(_) => bug!("Legalizing compressed pointer maps: {:?}", self),
                    _ => false,
                })
                .then_some(slot_count)
        }
    }

    fn legal_prefix<'tcx, Cx: CodegenMethods<'tcx>>(&self, cx: &Cx, pos: usize) -> Option<usize> {
        match self.slots[pos] {
            Slot::Void(size) => {
                assert_ne!(size, Size::ZERO, "Void slot with zero size");
                Some(1)
            }
            Slot::Ptr(rep) => {
                assert_ne!(rep, 0, "Pointer slot with zero size");
                self.void_slots(pos, rep * cx.data_layout().pointer_size.bytes_usize())
            }
            Slot::Int(size) => {
                assert_ne!(size, Size::ZERO, "Integer slot with zero size");
                self.void_slots(pos, size.bytes_usize())
            }
            Slot::Enum(ref ex, _) => {
                assert_ne!(ex.max_size, Size::ZERO, "Enum slot with zero size");
                self.void_slots(pos, ex.max_size.bytes_usize())
            }
        }
    }
}

impl PointerMap {
    fn compress(&mut self) {
        for i in (1..self.slots.len()).rev() {
            match self.slot_pair(i - 1) {
                (Slot::Void(a), Slot::Void(b)) => *a += std::mem::replace(b, Size::ZERO),
                (Slot::Ptr(a), Slot::Ptr(b)) => *a += std::mem::replace(b, 0usize),
                (Slot::Int(a), Slot::Int(b)) => *a += std::mem::replace(b, Size::ZERO),
                (_, Slot::Enum(ex, _)) => ex.variants.values_mut().for_each(Self::compress),
                _ => {}
            }
        }
        if let Some(Slot::Enum(ex, _)) = self.slots.first_mut() {
            ex.variants.values_mut().for_each(Self::compress);
        }
        self.slots.retain(|slot| {
            !matches!(slot, Slot::Ptr(0) | Slot::Int(Size::ZERO) | Slot::Void(Size::ZERO))
        });
    }

    fn monolize(&mut self) {
        let mut merge_list: SmallVec<[usize; 8]> = smallvec![];
        for (i, slot) in self.slots.iter_mut().enumerate() {
            if let Slot::Enum(ex, _) = slot {
                for value in ex.variants.values_mut() {
                    value.monolize();
                }
                if ex.variants.values().all_equal() {
                    merge_list.push(i);
                }
            }
        }
        // TODO: remove this
        let printmerge = !merge_list.is_empty();
        // TODO: remove this
        if printmerge {
            eprintln!("can merge variants: {:#?}", self);
        }
        for i in merge_list.into_iter().rev() {
            let Slot::Enum(enum_data, _) = self.slots.remove(i) else { unreachable!() };
            let submap = enum_data.variants.into_values().next().expect("Empty variants");
            self.slots.insert_many(i, submap.slots);
        }
        // TODO: remove this
        if printmerge {
            eprintln!("merged variants: {:#?}", self);
        }
    }

    fn simplify(&mut self) -> bool {
        let mut has_pointers = false;
        for slot in self.slots.iter_mut() {
            match slot {
                Slot::Ptr(_) => {
                    has_pointers = true;
                }
                Slot::Enum(ex, _) => {
                    for value in ex.variants.values_mut() {
                        if value.simplify() {
                            has_pointers = true;
                        }
                    }
                }
                _ => {}
            }
        }
        if !has_pointers {
            self.slots = smallvec![Slot::Int(self.size)];
        }
        has_pointers
    }

    fn legalize<'tcx, Cx: CodegenMethods<'tcx>>(&mut self, cx: &Cx) {
        let len = self.slots.len();
        let mut del = 0;
        let mut pos = 0;
        while pos < len {
            if let Slot::Enum(ref mut ex, _) = self.slots[pos] {
                ex.variants.values_mut().for_each(|m| m.legalize(cx));
            }
            if let Some(pfx) = self.legal_prefix(cx, pos) {
                if del > 0 {
                    self.slots.swap(pos, pos - del);
                }
                del += pfx - 1;
                pos += pfx;
            } else {
                bug!("Illegal prefix at pointer map index {}: {:?}", pos, self);
            }
        }
        self.slots.truncate(len - del);
        self.slots.iter_mut().for_each(|slot| {
            if let Slot::Void(size) = slot {
                *slot = Slot::Int(*size);
            }
        });
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
    fn set<'tcx, Cx: CodegenMethods<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        idx: usize,
        slot: Slot,
    ) {
        let pos = offset + Size::from_bytes(idx);
        let size = cx.data_layout().pointer_size;
        let align = cx.data_layout().pointer_align.abi;

        /* unaligned pointer, it's technically invalid, but some FFI related
         * code do need these (e.g. libc::unix::bsd::apple::shmid_ds), marking
         * as integers to ignore these slots */
        if let Slot::Ptr(n) = slot && !pos.is_aligned(align) {
            self.slots[pos.bytes_usize()] = Slot::Int((n as u64) * size);
        } else {
            self.slots[pos.bytes_usize()] = slot;
        }
    }

    fn set_submap<'tcx, Cx: CodegenMethods<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        submap: PointerMap,
    ) {
        submap.slots.iter().enumerate().for_each(|(idx, slot)| {
            self.set(cx, offset, idx, slot.clone());
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
                    self.set(cx, offset, 0, Slot::Ptr(1));
                } else {
                    self.set(cx, offset, 0, Slot::Int(value.size(cx)));
                }
            }
            /* unions does not have deterministic runtime type information, so
             * user must take care of their pointer variants, ROG GC won't deal
             * with those pointers. */
            Scalar::Union { value } => {
                self.set(cx, offset, 0, Slot::Int(value.size(cx)));
            }
        }
    }

    fn set_layout<'tcx, Cx: CodegenMethods<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        layout: TyAndLayout<'tcx>,
    ) {
        if layout.size < cx.data_layout().pointer_size {
            self.set(cx, offset, 0, Slot::Int(layout.size));
            return;
        }
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
                /* treat unions as opaque data, as described above. */
                FieldsShape::Primitive | FieldsShape::Union(_) => {
                    self.set(cx, offset, 0, Slot::Int(layout.size));
                }
                FieldsShape::Array { count, .. } => {
                    let elem = Self::resolve(cx, layout.field(cx, 0), None);
                    let size = Size::from_bytes(elem.slots.len());
                    for i in 0..count {
                        self.set_submap(cx, offset + size * i, elem.clone());
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
                                    self.set(
                                        cx,
                                        offset + layout.fields.offset(i),
                                        idx,
                                        slot.clone(),
                                    )
                                });
                        }
                    }
                    Variants::Multiple {
                        tag: Scalar::Initialized { value, .. },
                        ref tag_encoding,
                        tag_field,
                        ref variants,
                        ..
                    } => {
                        self.set(
                            cx,
                            offset,
                            0,
                            Slot::Enum(
                                Enum {
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
                                match tag_encoding {
                                    TagEncoding::Direct => None,
                                    TagEncoding::Niche {
                                        untagged_variant,
                                        niche_variants,
                                        niche_start,
                                    } => Some(Niche {
                                        niche_start: *niche_start,
                                        niche_variants: niche_variants.clone(),
                                        untagged_variant: *untagged_variant,
                                    }),
                                },
                            ),
                        );
                    }
                    Variants::Multiple { tag, .. } => {
                        bug!("Union discriminant value: {:#?}", tag)
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
            size: layout.size,
            slots: smallvec![Slot::Void(Size::from_bytes(1)); layout.size.bytes_usize()],
        };
        if layout.size != Size::ZERO {
            ret.set_layout(cx, Size::ZERO, layout);
        }
        cx.add_pointer_map(ty, variant_idx, ret.clone());
        // TODO: remove this block
        if variant_idx.is_none() {
            eprintln!(
                "ty({:?}) {:#?} -> [ {} ]\n",
                layout.size,
                ty,
                ret.encode(cx)
                    .into_iter()
                    .map(|v| format!("{:08b}", v))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
        }
        ret
    }
}

pub fn may_contain_heap_ptr<'tcx, Cx: CodegenMethods<'tcx>>(
    cx: &Cx,
    layout: TyAndLayout<'tcx>,
) -> bool {
    PointerMap::resolve(cx, layout, None).has_pointers()
}
