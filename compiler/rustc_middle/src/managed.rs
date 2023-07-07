use crate::ty::{AdtDef, FieldDef, GenericArg, List, ParamEnv, Ty, TyCtxt};

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
        ty.is_managed(self.tcx, ParamEnv::reveal_all())
    }

    pub fn find_managed_field(
        self,
        adt: AdtDef<'tcx>,
        args: &'tcx List<GenericArg<'tcx>>,
    ) -> Option<&'tcx FieldDef> {
        self.find_managed_field_impl(adt, args)
    }
}

impl<'tcx> ManagedChecker<'tcx> {
    fn find_managed_field_impl(
        self,
        adt: AdtDef<'tcx>,
        args: &'tcx List<GenericArg<'tcx>>,
    ) -> Option<&'tcx FieldDef> {
        adt.variants().iter().flat_map(|def| def.fields.iter()).find(|field| {
            let ty = field.ty(self.tcx, args);
            let ty = self.tcx.normalize_erasing_regions(ParamEnv::reveal_all(), ty);
            self.is_managed(ty)
        })
    }
}
