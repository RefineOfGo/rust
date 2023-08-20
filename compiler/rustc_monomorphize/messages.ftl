monomorphize_consider_type_length_limit =
    consider adding a `#![type_length_limit="{$type_length}"]` attribute to your crate

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

monomorphize_symbol_already_defined = symbol `{$symbol}` is already defined

monomorphize_type_length_limit = reached the type-length limit while instantiating `{$shrunk}`

monomorphize_unknown_cgu_collection_mode =
    unknown codegen-item collection mode '{$mode}', falling back to 'lazy' mode

monomorphize_unknown_partition_strategy = unknown partitioning strategy

monomorphize_unused_generic_params = item has unused generic parameters

monomorphize_written_to_path = the full type name has been written to '{$path}'

monomorphize_managed_union_field =
    unions cannot hold `Managed` value of type `{$field_ty}`
    .note = union fields cannot be managed by ROG, which should not hold any managed values

monomorphize_managed_field_in_unmanaged_adt =
    unmanaged structs or enums cannot hold `Managed` value of type `{$field_ty}`
    .note = add `#[derive(Managed)]` to the containing struct/enum in order to hold this field

monomorphize_closure_captures_managed_value =
    `Managed` value of type `{$value_ty}` captured by closure
    .note = to ROG developers: maybe implement a `KeepAlive<T>` container?

monomorphize_closure_captures_managed_value_span =
    the captured `Managed` value

monomorphize_closure_captures_managed_capture_span =
    referenced here
