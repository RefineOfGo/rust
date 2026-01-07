use rustc_abi::{
    Align, BackendRepr, FieldsShape, Float, HasDataLayout, Integer, Primitive, Reg, RegKind,
    Scalar, Size, Variants,
};
use smallvec::SmallVec;

use crate::ty::layout::{HasTyCtxt, HasTypingEnv, TyAndLayout};

#[derive(Debug, Clone, Copy)]
struct Value {
    reg: Reg,
    offs: Size,
    align: Align,
}

impl Value {
    fn is_adjacent_to(&self, next: &Self) -> bool {
        (self.offs + self.reg.size).align_to(next.align) == next.offs
    }
}

pub struct RegisterMap {
    regs: SmallVec<[Value; 8]>,
}

impl RegisterMap {
    fn new() -> Self {
        Self { regs: SmallVec::new() }
    }
}

impl RegisterMap {
    fn set_scalar<Cx: HasDataLayout>(
        &mut self,
        cx: &Cx,
        offset: Size,
        scalar: Scalar,
    ) -> Option<()> {
        let offs = offset;
        let align = scalar.align(cx).abi;

        /* don't use registers for unaligned field */
        if !offset.is_aligned(align) {
            return None;
        }

        /* map the scalar to register */
        let reg = match scalar.primitive() {
            Primitive::Int(Integer::I8, _) => Reg::i8(),
            Primitive::Int(Integer::I16, _) => Reg::i16(),
            Primitive::Int(Integer::I32, _) => Reg::i32(),
            Primitive::Int(Integer::I64, _) => Reg::i64(),
            Primitive::Int(Integer::I128, _) => Reg::i128(),
            Primitive::Float(Float::F16) => Reg::f16(),
            Primitive::Float(Float::F32) => Reg::f32(),
            Primitive::Float(Float::F64) => Reg::f64(),
            Primitive::Float(Float::F128) => Reg::f128(),
            Primitive::Pointer(_) => Reg::ptr(cx),
        };

        /* add the register */
        self.regs.push(Value { reg, offs, align });
        Some(())
    }

    fn set_layout<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        layout: TyAndLayout<'tcx>,
    ) -> Option<()> {
        if layout.is_uninhabited() {
            return None;
        }
        match layout.fields {
            FieldsShape::Primitive => match layout.backend_repr {
                BackendRepr::Scalar(scalar) => self.set_scalar(cx, offset, scalar)?,
                BackendRepr::ScalarPair(..) => bug!("conflict: Primitive & Scalar Pair"),
                BackendRepr::ScalableVector { .. } => bug!("conflict: Primitive & ScalableVector"),
                BackendRepr::SimdVector { .. } => bug!("conflict: Primitive & SimdVector"),
                BackendRepr::Memory { .. } => bug!("conflict: Primitive & Memory"),
            },
            FieldsShape::Array { stride, count } => {
                let elem = layout.field(cx, 0);
                for i in 0..count {
                    self.set_layout(cx, offset + stride * i, elem)?;
                }
            }
            FieldsShape::Union(..) | FieldsShape::Arbitrary { .. } => {
                for i in layout.fields.index_by_increasing_offset() {
                    self.set_layout(cx, offset + layout.fields.offset(i), layout.field(cx, i))?;
                }
                if let Variants::Multiple { ref variants, .. } = layout.variants {
                    for i in variants.indices() {
                        self.set_layout(cx, offset, layout.for_variant(cx, i))?;
                    }
                }
            }
        }
        Some(())
    }
}

impl RegisterMap {
    fn finalize(mut self) -> Option<Vec<Reg>> {
        let mut i = 0usize;
        let mut j = 1usize;

        /* ensure pointer sorts before integer for the same offset */
        self.regs.sort_by_key(|v| match v.reg.kind {
            RegKind::Integer => (v.offs, 1),
            RegKind::Pointer => (v.offs, 0),
            RegKind::Float => (v.offs, 2),
            RegKind::Vector => (v.offs, 3),
        });

        /* coalesce identical slots, while pointer trumps integer, and check compactness along the way */
        while j < self.regs.len() {
            if self.regs[i].offs == self.regs[j].offs {
                if self.regs[i].reg.size != self.regs[j].reg.size {
                    return None;
                }
                match (self.regs[i].reg.kind, self.regs[j].reg.kind) {
                    (RegKind::Integer, RegKind::Integer)
                    | (RegKind::Pointer, RegKind::Integer | RegKind::Pointer)
                    | (RegKind::Float, RegKind::Float) => {
                        j += 1;
                        continue;
                    }
                    (RegKind::Integer | RegKind::Pointer, RegKind::Float) => return None,
                    (RegKind::Vector, _) | (_, RegKind::Vector) => bug!("invalid reg kind"),
                    (RegKind::Float, _) | (_, RegKind::Pointer) => bug!("invalid reg order"),
                }
            }
            if !self.regs[i].is_adjacent_to(&self.regs[j]) {
                return None;
            }
            if j != i + 1 {
                self.regs.swap(i + 1, j);
            }
            i += 1;
            j += 1;
        }

        /* remove dead slots */
        self.regs.truncate(i + 1);
        Some(self.regs.into_iter().map(|v| v.reg).collect::<Vec<_>>())
    }
}

impl RegisterMap {
    pub fn resolve<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
        cx: &Cx,
        layout: TyAndLayout<'tcx>,
    ) -> Option<Vec<Reg>> {
        cx.tcx()
            .register_maps
            .borrow_mut()
            .entry(layout.ty)
            .or_insert_with(|| {
                let mut ret = Self::new();
                ret.set_layout(cx, Size::ZERO, layout)?;
                ret.finalize()
            })
            .clone()
    }
}
