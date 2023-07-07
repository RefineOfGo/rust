use rustc_data_structures::fx::{FxHashMap, FxHashSet};

use crate::ty::{self, AdtDef, FieldDef, GenericArg, List, ParamEnv, Ty, TyCtxt};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ManagedSelf {
    No,
    Yes,
    Unsure,
}

pub struct ManagedChecker<'tcx> {
    tcx: TyCtxt<'tcx>,
    seen: FxHashSet<Ty<'tcx>>,
    cache: FxHashMap<Ty<'tcx>, bool>,
}

impl<'tcx> ManagedChecker<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self { tcx, seen: FxHashSet::default(), cache: FxHashMap::default() }
    }
}

impl<'tcx> ManagedChecker<'tcx> {
    pub fn is_managed(&mut self, ty: Ty<'tcx>) -> bool {
        assert!(self.seen.is_empty());
        self.is_managed_impl(ty)
    }

    pub fn is_managed_self(&mut self, ty: Ty<'tcx>) -> ManagedSelf {
        if Self::is_unmanaged_fast(ty) {
            ManagedSelf::No
        } else if self.tcx.is_managed_raw(ParamEnv::reveal_all().and(ty)) {
            ManagedSelf::Yes
        } else {
            ManagedSelf::Unsure
        }
    }

    pub fn find_managed_field(
        &mut self,
        adt: AdtDef<'tcx>,
        args: &'tcx List<GenericArg<'tcx>>,
    ) -> Option<&'tcx FieldDef> {
        assert!(self.seen.is_empty());
        self.find_managed_field_impl(adt, args)
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
    fn is_managed_impl(&mut self, ty: Ty<'tcx>) -> bool {
        if let Some(is_managed) = self.cache.get(&ty) {
            return *is_managed;
        }
        let is_managed = match self.is_managed_self(ty) {
            ManagedSelf::No => false,
            ManagedSelf::Yes => true,
            ManagedSelf::Unsure => {
                if !self.seen.insert(ty) {
                    return false;
                }
                let result = match *ty.kind() {
                    ty::Adt(adt, args) => self.find_managed_field_impl(adt, args).is_some(),
                    ty::Array(elem_ty, _)
                    | ty::Pat(elem_ty, _)
                    | ty::Slice(elem_ty)
                    | ty::RawPtr(elem_ty, _)
                    | ty::Ref(_, elem_ty, _) => self.is_managed_impl(elem_ty),
                    ty::Closure(_, args) => args
                        .as_closure()
                        .upvar_tys()
                        .iter()
                        .any(|upvar_ty| self.is_managed_impl(upvar_ty)),
                    ty::CoroutineClosure(_, args) => args
                        .as_coroutine_closure()
                        .upvar_tys()
                        .iter()
                        .any(|upvar_ty| self.is_managed_impl(upvar_ty)),
                    ty::Coroutine(_, args) => args
                        .as_coroutine()
                        .upvar_tys()
                        .iter()
                        .chain([args.as_coroutine().witness()])
                        .any(|upvar_ty| self.is_managed_impl(upvar_ty)),
                    ty::CoroutineWitness(def_id, args) => self
                        .tcx
                        .coroutine_hidden_types(def_id)
                        .map(|bty| bty.instantiate(self.tcx, args))
                        .collect::<Vec<_>>()
                        .into_iter()
                        .any(|bty| self.is_managed_impl(bty)),
                    ty::Tuple(fields) => {
                        fields.iter().any(|field_ty| self.is_managed_impl(field_ty))
                    }
                    _ => unreachable!("checking trivially unmanaged type in slow path"),
                };
                self.seen.remove(&ty);
                result
            }
        };
        self.cache.insert(ty, is_managed);
        is_managed
    }

    fn find_managed_field_impl(
        &mut self,
        adt: AdtDef<'tcx>,
        args: &'tcx List<GenericArg<'tcx>>,
    ) -> Option<&'tcx FieldDef> {
        if adt.is_union() {
            None
        } else {
            assert!(adt.is_enum() || adt.is_struct());
            adt.variants().iter().flat_map(|def| def.fields.iter()).find(|field| {
                self.is_managed_impl(
                    self.tcx.normalize_erasing_regions(
                        ParamEnv::reveal_all(),
                        field.ty(self.tcx, args),
                    ),
                )
            })
        }
    }
}
