use rustc_abi::{
    BackendRepr, FieldsShape, Float, HasDataLayout, Primitive, Reg, RegKind, Scalar, Size, Variants,
};

use crate::ty::layout::{HasTyCtxt, HasTypingEnv, TyAndLayout};

#[derive(Debug, Clone, Copy)]
struct Value {
    reg: Reg,
    offs: Size,
}

impl Value {
    #[inline(always)]
    fn end(&self) -> Size {
        self.offs + self.reg.size
    }
}

pub struct RegisterMap {
    vals: Vec<Value>,
}

impl RegisterMap {
    fn set_at(&mut self, offset: Size, reg: Reg) {
        let end = offset + reg.size;
        let index = self.vals.binary_search_by_key(&offset, |v| v.offs);

        /* handle pointer slots & non-overlapping values */
        match index {
            Ok(idx) => {
                let val = self.vals[idx].reg;
                let rkind = reg.kind;

                /* both value have identical type & size */
                if rkind == val.kind && reg.size == val.size {
                    return;
                } else {
                    assert!(rkind != RegKind::Pointer || val.kind != RegKind::Pointer);
                }
            }
            Err(idx) => {
                let is_overlapping = {
                    (idx > 0 && offset < self.vals[idx - 1].end())
                        || (idx < self.vals.len() && end > self.vals[idx].offs)
                };
                if !is_overlapping {
                    self.vals.insert(idx, Value { reg, offs: offset });
                    return;
                }
            }
        }

        /* affected value range */
        let mut upper = Size::ZERO;
        let mut lower = Size::from_bytes(u64::MAX);
        let mut have_pointers = false;

        /* partially overlapping, remove all values that it touches, except pointers */
        self.vals.retain(|v| {
            end <= v.offs || v.end() <= offset || {
                lower = lower.min(v.offs);
                upper = upper.max(v.end());

                /* record pointer info */
                v.reg.kind == RegKind::Pointer && {
                    assert!(reg.kind != RegKind::Pointer, "overlapping pointers");
                    have_pointers = true;
                    true
                }
            }
        });

        /* if the overlapping range contains no pointers, just overwrite the entire range with a big integer */
        let mut idx = {
            match self.vals.binary_search_by_key(&lower, |v| v.offs) {
                Ok(idx) => {
                    assert!(have_pointers);
                    assert!(self.vals[idx].reg.kind == RegKind::Pointer);
                    lower += self.vals[idx].reg.size;
                    idx + 1
                }
                Err(idx) if !have_pointers => {
                    let reg = Reg { kind: RegKind::Integer, size: upper - lower };
                    self.vals.insert(idx, Value { reg, offs: lower });
                    return;
                }
                Err(idx) => idx,
            }
        };

        /* fill gaps with padding */
        while idx < self.vals.len() {
            let val = self.vals[idx];
            let offs = val.offs;

            /* last item */
            if val.end() > upper {
                break;
            }

            /* add paddings if needed */
            if lower < offs {
                let reg = Reg { kind: RegKind::Integer, size: offs - lower };
                self.vals.insert(idx, Value { reg, offs: lower });
                lower = offs;
                idx += 1;
            }

            /* move to next value */
            assert!(lower == offs);
            lower += val.reg.size;
            idx += 1;
        }

        /* trailing paddings */
        if lower < upper {
            let reg = Reg { kind: RegKind::Integer, size: upper - lower };
            self.vals.insert(idx, Value { reg, offs: lower });
        }
    }

    fn set_int(&mut self, offset: Size, size: Size) {
        if size != Size::ZERO {
            let reg = Reg { kind: RegKind::Integer, size };
            self.set_at(offset, reg);
        }
    }

    fn set_scalar<Cx: HasDataLayout>(&mut self, cx: &Cx, offset: Size, scalar: Scalar) {
        if offset.is_aligned(scalar.align(cx).abi) {
            match scalar.primitive() {
                Primitive::Int(v, _) => self.set_int(offset, v.size()),
                Primitive::Float(Float::F16) => self.set_at(offset, Reg::f16()),
                Primitive::Float(Float::F32) => self.set_at(offset, Reg::f32()),
                Primitive::Float(Float::F64) => self.set_at(offset, Reg::f64()),
                Primitive::Float(Float::F128) => self.set_at(offset, Reg::f128()),
                Primitive::Pointer(_) => self.set_at(offset, Reg::ptr(cx)),
            }
        } else {
            self.set_int(offset, scalar.size(cx));
        }
    }

    fn set_layout<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
        &mut self,
        cx: &Cx,
        offset: Size,
        layout: TyAndLayout<'tcx>,
    ) {
        if layout.is_uninhabited() {
            self.set_int(offset, layout.size);
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
                for i in layout.fields.index_by_increasing_offset() {
                    let offs = offset + layout.fields.offset(i);
                    self.set_layout(cx, offs, layout.field(cx, i));
                }
                if let Variants::Multiple { ref variants, .. } = layout.variants {
                    let mut min_size = Size::from_bytes(u64::MAX);
                    for i in variants.indices() {
                        let variant = layout.for_variant(cx, i);
                        min_size = min_size.min(variant.size);
                        self.set_layout(cx, offset, variant);
                    }
                    assert!(min_size <= layout.size);
                    self.set_int(offset + min_size, layout.size - min_size);
                }
            }
        }
    }
}

impl RegisterMap {
    fn finalize<'tcx>(self, layout: TyAndLayout<'tcx>) -> Vec<Reg> {
        let mut pos = Size::ZERO;
        let mut idx = 0usize;
        let mut ret = Vec::with_capacity(self.vals.len());

        /* process each value, insert paddings as needed */
        for val in &self.vals {
            if pos < val.offs {
                ret.push(Reg { kind: RegKind::Integer, size: val.offs - pos });
                pos = val.offs;
            }
            assert!(pos == val.offs);
            pos += val.reg.size;
            ret.push(val.reg);
        }

        /* append paddings if needed */
        if pos < layout.size {
            ret.push(Reg { kind: RegKind::Integer, size: layout.size - pos });
            pos = layout.size;
        }

        /* size check */
        assert!(
            pos == layout.size,
            "register size mismatch. size={pos:?}, regs={ret:#?}, layout={layout:#?}",
        );

        /* merge consecutive integers */
        for i in (1..ret.len()).rev() {
            if ret[i].kind == RegKind::Integer && ret[i - 1].kind == RegKind::Integer {
                let size = ret[i].size;
                ret[i - 1].size += size;
                ret[i].size = Size::ZERO;
            }
        }

        /* split large integer register into at most 64-bit ones */
        while idx < ret.len() {
            if ret[idx].kind == RegKind::Integer && ret[idx].size > Size::from_bits(64) {
                let size = ret[idx].size - Size::from_bits(64);
                ret.insert(idx + 1, Reg { kind: RegKind::Integer, size });
                ret[idx].size = Size::from_bits(64);
            }
            idx += 1;
        }

        /* remove zero-sized registers */
        ret.retain(|v| v.size != Size::ZERO);
        ret
    }
}

impl RegisterMap {
    pub fn resolve<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasTypingEnv<'tcx>>(
        cx: &Cx,
        layout: TyAndLayout<'tcx>,
    ) -> Vec<Reg> {
        cx.tcx()
            .register_maps
            .borrow_mut()
            .entry(layout.ty)
            .or_insert_with(|| {
                let unit = cx.data_layout().pointer_size().bytes_usize();
                let vals = Vec::with_capacity(layout.size.bytes_usize() / unit + 1);
                let mut ret = Self { vals };
                ret.set_layout(cx, Size::ZERO, layout);
                ret.finalize(layout)
            })
            .clone()
    }
}
