use rustc_data_structures::fx::FxHashSet;

use crate::ty::{self, AdtDef, FieldDef, GenericArg, List, ParamEnv, Ty, TyCtxt};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ManagedSelf {
    No,
    Yes,
    Unsure,
}

#[derive(Clone, Copy)]
pub struct ManagedChecker<'tcx> {
    tcx: TyCtxt<'tcx>,
}

impl<'tcx> ManagedChecker<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self { tcx }
    }
}

impl<'tcx> ManagedChecker<'tcx> {
    pub fn is_managed(self, ty: Ty<'tcx>) -> bool {
        self.is_managed_impl(ty, &mut FxHashSet::default())
    }

    pub fn is_managed_self(self, ty: Ty<'tcx>) -> ManagedSelf {
        if Self::is_unmanaged_fast(ty) {
            ManagedSelf::No
        } else if self.tcx.is_managed_raw(ParamEnv::reveal_all().and(ty)) {
            ManagedSelf::Yes
        } else {
            ManagedSelf::Unsure
        }
    }

    pub fn find_managed_field(
        self,
        adt: AdtDef<'tcx>,
        args: &'tcx List<GenericArg<'tcx>>,
    ) -> Option<&'tcx FieldDef> {
        self.find_managed_field_impl(adt, args, &mut FxHashSet::default())
    }
}

impl<'tcx> ManagedChecker<'tcx> {
    fn is_unmanaged_fast(ty: Ty<'_>) -> bool {
        match ty.kind() {
            ty::Bool
            | ty::Char
            | ty::Int(_)
            | ty::Uint(_)
            | ty::Float(_)
            | ty::Foreign(_)
            | ty::Str
            | ty::FnDef(..)
            | ty::FnPtr(_)
            | ty::Dynamic(..)
            | ty::Never
            | ty::Error(_) => true,
            ty::Adt(adt, _) => adt.is_union(),
            ty::Array(elem_ty, _)
            | ty::Pat(elem_ty, _)
            | ty::Slice(elem_ty)
            | ty::RawPtr(elem_ty, _)
            | ty::Ref(_, elem_ty, _) => Self::is_unmanaged_fast(*elem_ty),
            ty::Closure(..)
            | ty::CoroutineClosure(..)
            | ty::Coroutine(..)
            | ty::CoroutineWitness(..) => false,
            ty::Tuple(fields) => fields.iter().all(Self::is_unmanaged_fast),
            ty::Alias(..) | ty::Param(_) | ty::Bound(..) | ty::Placeholder(_) | ty::Infer(_) => {
                tracing::debug!("suspicious type, assuming unmanaged {:#?}", ty);
                true
            }
        }
    }
}

impl<'tcx> ManagedChecker<'tcx> {
    fn is_managed_impl(self, ty: Ty<'tcx>, seen: &mut FxHashSet<Ty<'tcx>>) -> bool {
        if let Some(is_managed) = self.tcx.managed_cache.lock().get(&ty) {
            return *is_managed;
        }
        let is_managed = match self.is_managed_self(ty) {
            ManagedSelf::No => false,
            ManagedSelf::Yes => true,
            ManagedSelf::Unsure => {
                if !seen.insert(ty) {
                    return false;
                }
                let result = match *ty.kind() {
                    ty::Adt(adt, args) => {
                        let field = self.find_managed_field_impl(adt, args, seen);
                        field.is_some()
                    }
                    ty::Array(elem_ty, _)
                    | ty::Pat(elem_ty, _)
                    | ty::Slice(elem_ty)
                    | ty::RawPtr(elem_ty, _)
                    | ty::Ref(_, elem_ty, _) => self.is_managed_impl(elem_ty, seen),
                    ty::Closure(_, args) => args
                        .as_closure()
                        .upvar_tys()
                        .iter()
                        .any(|upvar_ty| self.is_managed_impl(upvar_ty, seen)),
                    ty::CoroutineClosure(_, args) => args
                        .as_coroutine_closure()
                        .upvar_tys()
                        .iter()
                        .any(|upvar_ty| self.is_managed_impl(upvar_ty, seen)),
                    ty::Coroutine(_, args) => args
                        .as_coroutine()
                        .upvar_tys()
                        .iter()
                        .chain([args.as_coroutine().witness()])
                        .any(|upvar_ty| self.is_managed_impl(upvar_ty, seen)),
                    ty::CoroutineWitness(def_id, args) => self
                        .tcx
                        .coroutine_hidden_types(def_id)
                        .map(|bty| bty.instantiate(self.tcx, args))
                        .collect::<Vec<_>>()
                        .into_iter()
                        .any(|bty| self.is_managed_impl(bty, seen)),
                    ty::Tuple(fields) => {
                        fields.iter().any(|field_ty| self.is_managed_impl(field_ty, seen))
                    }
                    _ => unreachable!("checking trivially unmanaged type in slow path"),
                };
                seen.remove(&ty);
                result
            }
        };
        self.tcx.managed_cache.lock().insert(ty, is_managed);
        is_managed
    }

    fn find_managed_field_impl(
        self,
        adt: AdtDef<'tcx>,
        args: &'tcx List<GenericArg<'tcx>>,
        seen: &mut FxHashSet<Ty<'tcx>>,
    ) -> Option<&'tcx FieldDef> {
        if adt.is_union() {
            None
        } else {
            assert!(adt.is_enum() || adt.is_struct());
            adt.variants().iter().flat_map(|def| def.fields.iter()).find(|field| {
                let ty = field.ty(self.tcx, args);
                let ty = self.tcx.normalize_erasing_regions(ParamEnv::reveal_all(), ty);
                self.is_managed_impl(ty, seen)
            })
        }
    }
}
