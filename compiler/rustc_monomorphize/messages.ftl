monomorphize_couldnt_dump_mono_stats =
    unexpected error occurred while dumping monomorphization stats: {$error}

monomorphize_encountered_error_while_instantiating =
    the above error was encountered while instantiating `{$formatted_item}`

monomorphize_large_assignments =
    moving {$size} bytes
    .label = value moved from here
    .note = The current maximum size is {$limit}, but it can be customized with the move_size_limit attribute: `#![move_size_limit = "..."]`

monomorphize_no_optimized_mir =
    missing optimized MIR for an item in the crate `{$crate_name}`
    .note = missing optimized MIR for this item (was the crate `{$crate_name}` compiled with `--emit=metadata`?)

monomorphize_recursion_limit =
    reached the recursion limit while instantiating `{$shrunk}`
    .note = `{$def_path_str}` defined here

monomorphize_start_not_found = using `fn main` requires the standard library
    .help = use `#![no_main]` to bypass the Rust generated entrypoint and declare a platform specific entrypoint yourself, usually with `#[no_mangle]`

monomorphize_symbol_already_defined = symbol `{$symbol}` is already defined

monomorphize_unknown_cgu_collection_mode =
    unknown codegen-item collection mode '{$mode}', falling back to 'lazy' mode

monomorphize_unused_generic_params = item has unused generic parameters

monomorphize_written_to_path = the full type name has been written to '{$path}'

monomorphize_managed_union_field =
    unions cannot hold managed value of type `{$field_ty}`
    .note = unions does not have a deterministic layout at runtime, which is impossible for GC to manage

monomorphize_managed_field_in_unmanaged_adt =
    unmanaged structs or enums cannot hold managed value of type `{$field_ty}`
    .note = add `#[derive(Managed)]` to the containing struct/enum in order to hold this field

monomorphize_dyn_trait_points_to_managed_value =
    managed value of type `{$value_ty}` referenced by `{$target_ty}`
    .note = dynamic values are always unmanaged, thus cannot hold any managed values
