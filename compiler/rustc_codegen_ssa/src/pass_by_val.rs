use rustc_middle::ty::layout::{HasParamEnv, HasTyCtxt, TyAndLayout};
use rustc_target::abi::{Abi, AbiAndPrefAlign, HasDataLayout, Size, Variants};

pub fn can_pass_by_value<'tcx, Cx: HasDataLayout + HasTyCtxt<'tcx> + HasParamEnv<'tcx>>(
    cx: &Cx,
    layout: TyAndLayout<'tcx>,
) -> bool {
    let check_size_and_align = |size: Size, align: AbiAndPrefAlign| {
        align.abi >= cx.data_layout().pointer_align.abi
            && (size.bytes() & (cx.data_layout().pointer_size.bytes() - 1)) == 0
    };
    if !check_size_and_align(layout.size, layout.align) {
        return false;
    }
    if layout.is_zst() {
        return true;
    }
    match layout.abi {
        Abi::Uninhabited => false,
        Abi::Scalar(v) => check_size_and_align(v.size(cx), v.align(cx)),
        Abi::ScalarPair(a, b) => {
            check_size_and_align(a.size(cx), a.align(cx))
                && check_size_and_align(b.size(cx), b.align(cx))
        }
        Abi::Vector { element, .. } => check_size_and_align(element.size(cx), element.align(cx)),
        Abi::Aggregate { sized: false } => false,
        Abi::Aggregate { sized: true } => match layout.variants {
            Variants::Single { .. } => layout
                .fields
                .index_by_increasing_offset()
                .all(|i| can_pass_by_value(cx, layout.field(cx, i))),
            Variants::Multiple { ref variants, .. } => {
                variants.indices().all(|i| can_pass_by_value(cx, layout.for_variant(cx, i)))
            }
        },
    }
}
