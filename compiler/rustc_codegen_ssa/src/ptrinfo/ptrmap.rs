use std::fmt::{Debug, DebugStruct, Formatter, Result as FmtResult};

use itertools::Itertools;
use rustc_data_structures::{fx::FxHashMap, stack::ensure_sufficient_stack};
use rustc_middle::ty::layout::TyAndLayout;
use rustc_target::abi::{
    Abi, FieldIdx, FieldsShape, Primitive, Scalar, Size, TagEncoding, VariantIdx, Variants,
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
    pub untagged_variant: Option<PointerMap>,
}

impl Enum {
    pub fn iter(&self) -> impl Iterator<Item = &PointerMap> {
        self.variants.values().chain(self.untagged_variant.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut PointerMap> {
        self.variants.values_mut().chain(self.untagged_variant.iter_mut())
    }
}

impl Enum {
    pub fn into_values(self) -> impl Iterator<Item = PointerMap> {
        self.variants.into_values().chain(self.untagged_variant.into_iter())
    }
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

/// Pointer Map Calculation

#[derive(Clone, PartialEq, Eq)]
pub enum Slot {
    Void(Size),
    Ptr(usize),
    Int(Size),
    Enum(Box<Enum>),
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
            Self::Enum(ex) => {
                if let Some(ref v) = ex.untagged_variant {
                    let mut dx = f.debug_struct("Niche");
                    ex.debug_fields(&mut dx);
                    dx.field("untagged_variant", v);
                    dx.finish()
                } else {
                    let mut dx = f.debug_struct("Enum");
                    ex.debug_fields(&mut dx);
                    dx.finish()
                }
            }
        }
    }
}

enum EnumTag {
    None,
    Tagged(u128),
    Untagged,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PointerMap {
    pub size: Size,
    pub slots: SmallVec<[Slot; 32]>,
}

impl PointerMap {
    pub fn is_static(&self) -> bool {
        self.slots.iter().all(|slot| !matches!(slot, Slot::Enum(_)))
    }

    pub fn has_pointers(&self) -> bool {
        self.slots.iter().any(|slot| match slot {
            Slot::Ptr(_) => true,
            Slot::Int(_) | Slot::Void(_) => false,
            Slot::Enum(ex, ..) => ex.iter().any(Self::has_pointers),
        })
    }
}

impl PointerMap {
    pub fn encode<'tcx, Cx: CodegenMethods<'tcx>>(mut self, cx: &Cx) -> BitmapData {
        ensure_sufficient_stack(|| {
            let mut out = CompressedBitVec::default();
            self.legalize(cx);
            self.simplify();
            self.compress();
            self.monolize();
            self.compress();
            self.resizing();
            self.trimming(cx);
            self.serialize_into(cx, &mut out, Size::ZERO, EnumTag::None);
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
        enum_tag: EnumTag,
    ) {
        let has_submap = self.has_pointers();
        assert!(has_submap || self.slots.is_empty(), "Untrimmed pointer map");

        /* encode the enum tag, if any */
        match enum_tag {
            EnumTag::None => {}
            EnumTag::Tagged(tag) => out.add_enum_tag(Some(tag), has_submap),
            EnumTag::Untagged => out.add_enum_tag(None, has_submap),
        }

        /* common case 1: type without pointers encodes into nothing */
        if !has_submap {
            return;
        }

        let ptr_size = cx.data_layout().pointer_size;
        let ptr_align = cx.data_layout().pointer_align.abi;

        /* common case 2: static types that with at most 4 pointer slots and is
         * pointer-size aligned encodes into a single byte */
        if self.size <= ptr_size * 4 && self.size.is_aligned(ptr_align) && self.is_static() {
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
                        Slot::Enum(_) => unreachable!(),
                    }
                })
                .flatten()
                .collect();
            out.add_simple(&slots);
            return;
        }

        /* other more complex types */
        let len = self.size.bytes_usize();
        out.add_complex(len);

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
                Slot::Enum(box value) => {
                    out.add_enum_header(&value);
                    value.variants.into_iter().sorted_by_key(|(tag, _)| *tag).for_each(
                        |(tag, map)| {
                            map.serialize_into(cx, out, offs, EnumTag::Tagged(tag));
                        },
                    );
                    if let Some(map) = value.untagged_variant {
                        map.serialize_into(cx, out, offs, EnumTag::Untagged);
                    }
                    offs += value.max_size;
                }
            }
        }
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
            Slot::Enum(ref ex) => {
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
                (_, Slot::Enum(ex)) => ex.iter_mut().for_each(Self::compress),
                _ => {}
            }
        }
        if let Some(Slot::Enum(ex)) = self.slots.first_mut() {
            ex.iter_mut().for_each(Self::compress);
        }
        self.slots.retain(|slot| {
            !matches!(slot, Slot::Ptr(0) | Slot::Int(Size::ZERO) | Slot::Void(Size::ZERO))
        });
    }

    fn monolize(&mut self) {
        let mut merge_list: SmallVec<[usize; 8]> = smallvec![];
        for (i, slot) in self.slots.iter_mut().enumerate() {
            if let Slot::Enum(ex) = slot {
                for map in ex.iter_mut() {
                    map.monolize();
                }
                if ex.iter().all_equal() {
                    merge_list.push(i);
                }
            }
        }
        for i in merge_list.into_iter().rev() {
            let Slot::Enum(ex) = self.slots.remove(i) else { unreachable!() };
            let size = ex.max_size;
            let submap = ex.into_values().next().expect("Empty variants");
            if size != submap.size {
                assert!(size > submap.size, "Submap does not fit into this slot");
                self.slots.insert(i, Slot::Int(size - submap.size));
            }
            self.slots.insert_many(i, submap.slots);
        }
    }

    fn simplify(&mut self) -> bool {
        let mut has_pointers = false;
        for slot in self.slots.iter_mut() {
            match slot {
                Slot::Ptr(_) => {
                    has_pointers = true;
                }
                Slot::Enum(ex) => {
                    let mut enum_has_pointers = false;
                    for value in ex.iter_mut() {
                        if value.simplify() {
                            has_pointers = true;
                            enum_has_pointers = true;
                        }
                    }
                    if !enum_has_pointers {
                        *slot = Slot::Int(ex.max_size);
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

    fn resizing(&mut self) -> bool {
        let mut keep = self.size;
        let mut slots = self.slots.len();
        let mut has_pointers = false;

        for slot in self.slots.iter_mut().rev() {
            match slot {
                Slot::Void(_) => {
                    bug!("Illegal pointer map with void slots: {:?}", slot);
                }
                Slot::Ptr(_) => {
                    has_pointers = true;
                    break;
                }
                Slot::Int(size) => {
                    keep -= *size;
                    slots -= 1;
                }
                Slot::Enum(data) => {
                    let old_size = data.max_size;
                    let mut new_size = Size::ZERO;

                    for map in data.iter_mut() {
                        if map.resizing() {
                            has_pointers = true;
                        }
                        new_size = new_size.max(map.size);
                    }

                    assert!(new_size <= old_size, "Enum grows after resizing");
                    keep -= old_size - new_size;
                    data.max_size = new_size;

                    if has_pointers {
                        break;
                    }

                    assert_eq!(new_size, Size::ZERO, "Non-empty noptr map");
                    slots -= 1;
                }
            }
        }

        self.size = keep;
        self.slots.truncate(slots);
        self.slots.shrink_to_fit();
        has_pointers
    }

    fn trimming<'tcx, Cx: CodegenMethods<'tcx>>(&mut self, cx: &Cx) -> bool {
        let mut keep = 0;
        let mut has_pointers = false;

        for (i, slot) in self.slots.iter_mut().enumerate() {
            match slot {
                Slot::Void(_) => {
                    bug!("Illegal pointer map with void slots: {:?}", slot);
                }
                Slot::Ptr(_) => {
                    keep = i + 1;
                    has_pointers = true;
                }
                Slot::Int(_) => {}
                Slot::Enum(data) => {
                    for map in data.iter_mut() {
                        if map.trimming(cx) {
                            keep = i + 1;
                            has_pointers = true;
                        }
                    }
                }
            }
        }

        self.slots.truncate(keep);
        self.slots.shrink_to_fit();
        has_pointers
    }

    fn legalize<'tcx, Cx: CodegenMethods<'tcx>>(&mut self, cx: &Cx) {
        let len = self.slots.len();
        let mut del = 0;
        let mut pos = 0;
        while pos < len {
            if let Slot::Enum(ref mut ex) = self.slots[pos] {
                for map in ex.iter_mut() {
                    map.legalize(cx);
                }
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
                FieldsShape::Arbitrary { ref offsets, .. } => match layout.variants {
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
                        let (untagged_idx, adjust) = match tag_encoding {
                            TagEncoding::Direct => (None, 0u128),
                            TagEncoding::Niche {
                                untagged_variant,
                                niche_variants,
                                niche_start,
                            } => {
                                let start = niche_variants.start().as_usize();
                                let adjust = niche_start.wrapping_sub(start as u128);
                                (Some(*untagged_variant), adjust)
                            }
                        };
                        self.set(
                            cx,
                            offset,
                            0,
                            Slot::Enum(Box::new(Enum {
                                max_size: layout.size,
                                tag_offs: offsets[FieldIdx::from_usize(tag_field)],
                                tag_size: value.size(cx),
                                variants: variants
                                    .indices()
                                    .filter(|i| untagged_idx.map_or(true, |v| v != *i))
                                    .map(|i| {
                                        (
                                            Self::tag_of(cx, layout, i).wrapping_add(adjust),
                                            Self::resolve(cx, layout, Some(i)),
                                        )
                                    })
                                    .collect(),
                                untagged_variant: untagged_idx
                                    .map(|i| Self::resolve(cx, layout, Some(i))),
                            })),
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
        ret
    }
}

pub fn may_contain_heap_ptr<'tcx, Cx: CodegenMethods<'tcx>>(
    cx: &Cx,
    layout: TyAndLayout<'tcx>,
) -> bool {
    PointerMap::resolve(cx, layout, None).has_pointers()
}
