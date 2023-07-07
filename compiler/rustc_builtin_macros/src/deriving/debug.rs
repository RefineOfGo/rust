use crate::deriving::generic::ty::*;
use crate::deriving::generic::*;
use crate::deriving::path_std;

use ast::EnumDef;
use rustc_ast::{self as ast, MetaItem};
use rustc_expand::base::{Annotatable, ExtCtxt};
use rustc_span::symbol::sym;
use rustc_span::symbol::Ident;
use rustc_span::Span;
use thin_vec::{thin_vec, ThinVec};

pub fn expand_deriving_debug(
    cx: &mut ExtCtxt<'_>,
    span: Span,
    mitem: &MetaItem,
    item: &Annotatable,
    push: &mut dyn FnMut(Annotatable),
    is_const: bool,
) {
    // &mut ::std::fmt::Formatter
    let fmtr = Ref(Box::new(Path(path_std!(fmt::Formatter))), ast::Mutability::Mut);

    let trait_def = TraitDef {
        span,
        path: path_std!(fmt::Debug),
        skip_path_as_bound: false,
        needs_copy_as_bound_if_packed: true,
        additional_bounds: Vec::new(),
        supports_unions: false,
        methods: vec![MethodDef {
            name: sym::fmt,
            generics: Bounds::empty(),
            explicit_self: true,
            nonself_args: vec![(fmtr, sym::f)],
            ret_ty: Path(path_std!(fmt::Result)),
            attributes: thin_vec![cx.attr_word(sym::inline, span)],
            fieldless_variants_strategy:
                FieldlessVariantsStrategy::SpecializeIfAllVariantsFieldless,
            combine_substructure: combine_substructure(Box::new(|a, b, c| {
                show_substructure(a, b, c)
            })),
        }],
        associated_types: Vec::new(),
        is_const,
    };
    trait_def.expand(cx, mitem, item, push)
}

fn show_substructure(cx: &mut ExtCtxt<'_>, span: Span, substr: &Substructure<'_>) -> BlockOrExpr {
    // We want to make sure we have the ctxt set so that we can use unstable methods
    let span = cx.with_def_site_ctxt(span);

    let (ident, vdata, fields) = match substr.fields {
        Struct(vdata, fields) => (substr.type_ident, *vdata, fields),
        EnumMatching(_, _, v, fields) => (v.ident, &v.data, fields),
        AllFieldlessEnum(enum_def) => return show_fieldless_enum(cx, span, enum_def, substr),
        EnumTag(..) | StaticStruct(..) | StaticEnum(..) => {
            cx.dcx().span_bug(span, "nonsensical .fields in `#[derive(Debug)]`")
        }
    };

    let name = cx.expr_str(span, ident.name);
    let fmt = substr.nonselflike_args[0].clone();

    // Struct and tuples are similar enough that we use the same code for both,
    // with some extra pieces for structs due to the field names.
    let is_struct = match vdata {
        ast::VariantData::Unit(..) => {
            // Special fast path for unit variants.
            assert!(fields.is_empty());
            false
        }
        ast::VariantData::Tuple(..) => false,
        ast::VariantData::Struct { .. } => true,
    };

    fn expr_for_field(
        cx: &ExtCtxt<'_>,
        field: &FieldInfo,
        index: usize,
        len: usize,
    ) -> ast::ptr::P<ast::Expr> {
        if index < len - 1 {
            field.self_expr.clone()
        } else {
            // Unsized types need an extra indirection, but only the last field
            // may be unsized.
            cx.expr_addr_of(field.span, field.self_expr.clone())
        }
    }

    if fields.is_empty() {
        // Special case for no fields.
        let fn_path_write_str = cx.std_path(&[sym::fmt, sym::Formatter, sym::write_str]);
        let expr = cx.expr_call_global(span, fn_path_write_str, thin_vec![fmt, name]);
        BlockOrExpr::new_expr(expr)
    } else {
        // `let dbg_expr = fmt::Formatter::debug_struct(fmt, name)` or
        // `let dbg_expr = fmt::Formatter::debug_tuple(fmt, name)`
        let mut dbg_expr = cx.expr_call_global(
            span,
            cx.std_path(&[
                sym::fmt,
                sym::Formatter,
                if is_struct { sym::debug_struct } else { sym::debug_tuple },
            ]),
            thin_vec![fmt, name],
        );

        for i in 0..fields.len() {
            let field = &fields[i];
            let mut args = ThinVec::with_capacity(2);

            if is_struct {
                args.push(cx.expr_str(field.span, field.name.unwrap().name));
            }

            let field = expr_for_field(cx, field, i, fields.len());
            args.push(field);

            dbg_expr = cx.expr(
                span,
                ast::ExprKind::MethodCall(Box::new(ast::MethodCall {
                    seg: ast::PathSegment::from_ident(Ident::with_dummy_span(sym::field)),
                    receiver: dbg_expr,
                    args,
                    span,
                })),
            )
        }

        BlockOrExpr::new_expr(cx.expr_call_global(
            span,
            cx.std_path(&[
                sym::fmt,
                if is_struct { sym::DebugStruct } else { sym::DebugTuple },
                sym::finish,
            ]),
            thin_vec![dbg_expr],
        ))
    }
}

/// Special case for enums with no fields. Builds:
/// ```text
/// impl ::core::fmt::Debug for A {
///     fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
///          ::core::fmt::Formatter::write_str(f,
///             match self {
///                 A::A => "A",
///                 A::B() => "B",
///                 A::C {} => "C",
///             })
///     }
/// }
/// ```
fn show_fieldless_enum(
    cx: &mut ExtCtxt<'_>,
    span: Span,
    def: &EnumDef,
    substr: &Substructure<'_>,
) -> BlockOrExpr {
    let fmt = substr.nonselflike_args[0].clone();
    let arms = def
        .variants
        .iter()
        .map(|v| {
            let variant_path = cx.path(span, vec![substr.type_ident, v.ident]);
            let pat = match &v.data {
                ast::VariantData::Tuple(fields, _) => {
                    debug_assert!(fields.is_empty());
                    cx.pat_tuple_struct(span, variant_path, ThinVec::new())
                }
                ast::VariantData::Struct { fields, .. } => {
                    debug_assert!(fields.is_empty());
                    cx.pat_struct(span, variant_path, ThinVec::new())
                }
                ast::VariantData::Unit(_) => cx.pat_path(span, variant_path),
            };
            cx.arm(span, pat, cx.expr_str(span, v.ident.name))
        })
        .collect::<ThinVec<_>>();
    let name = cx.expr_match(span, cx.expr_self(span), arms);
    let fn_path_write_str = cx.std_path(&[sym::fmt, sym::Formatter, sym::write_str]);
    BlockOrExpr::new_expr(cx.expr_call_global(span, fn_path_write_str, thin_vec![fmt, name]))
}
