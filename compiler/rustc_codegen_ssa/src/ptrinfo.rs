use rustc_data_structures::fx::FxHashSet;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_target::abi::{Abi, FieldsShape, Primitive, Scalar, Variants};

use crate::traits::BuilderMethods;

fn may_contain_heap_ptr_impl<'a, 'tcx, Bx: BuilderMethods<'a, 'tcx>>(
    bx: &mut Bx,
    cache: &mut FxHashSet<TyAndLayout<'tcx>>,
    layout: TyAndLayout<'tcx>,
) -> bool {
    match layout.abi {
        // Unions does not have deterministic runtime type information, so
        // user must take care of their pointer variants. ROG GC won't take
        // care those pointers.
        Abi::Scalar(Scalar::Initialized { value: Primitive::Pointer(_), .. }) => true,
        Abi::ScalarPair(Scalar::Initialized { value: Primitive::Pointer(_), .. }, _) => true,
        Abi::ScalarPair(_, Scalar::Initialized { value: Primitive::Pointer(_), .. }) => true,
        Abi::Vector {
            element: Scalar::Initialized { value: Primitive::Pointer(_), .. }, ..
        } => true,
        Abi::Aggregate { .. } => match layout.fields {
            // Don't deal with unions.
            FieldsShape::Primitive | FieldsShape::Union(_) => false,
            FieldsShape::Array { .. } => {
                may_contain_heap_ptr_impl(bx, cache, layout.field(bx.cx(), 0))
            }
            FieldsShape::Arbitrary { .. } => match layout.variants {
                Variants::Single { .. } => layout
                    .fields
                    .index_by_increasing_offset()
                    .any(|i| may_contain_heap_ptr_impl(bx, cache, layout.field(bx.cx(), i))),
                Variants::Multiple { ref variants, .. } => variants
                    .indices()
                    .any(|i| may_contain_heap_ptr_impl(bx, cache, layout.for_variant(bx.cx(), i))),
            },
        },
        _ => false,
    }
}

pub fn may_contain_heap_ptr<'a, 'tcx, Bx: BuilderMethods<'a, 'tcx>>(
    bx: &mut Bx,
    layout: TyAndLayout<'tcx>,
) -> bool {
    let mut cache = FxHashSet::default();
    may_contain_heap_ptr_impl(bx, &mut cache, layout)
}
