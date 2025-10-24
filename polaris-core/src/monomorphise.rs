use rustc_hash::FxHashMap as HashMap;

use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    symbol::SymbolId,
    types::{Scheme, Substitution, Ty, TypeVar},
};

#[derive(Clone, Debug, Default)]
pub struct MonomorphiseInfo {
    pub monomorphised_asts: HashMap<SymbolId, Node>,
}

pub fn monomorphise_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let monomorphisations = ctx.type_info.monomorphised_fns.clone();

    if monomorphisations.is_empty() {
        return Ok(());
    }

    let mut poly_fn_defs: HashMap<SymbolId, Node> = HashMap::default();

    for (_module_id, module) in ctx.dependencies.modules.iter() {
        collect_poly_fn_defs(&module.ast, &ctx.type_info.type_env, &mut poly_fn_defs);
    }

    let mut new_fn_nodes: Vec<(String, Node)> = Vec::new();

    for (new_id, (orig_id, concrete_type)) in &monomorphisations {
        if let Some(orig_fn) = poly_fn_defs.get(orig_id) {
            let mut new_fn = orig_fn.clone();

            if let Some(scheme) = ctx.type_info.type_env.get(orig_id) {
                let subst = build_substitution(scheme, concrete_type)?;
                apply_type_substitution(&mut new_fn, &subst);
                new_fn.symbol_id = Some(*new_id);

                for (module_id, module) in ctx.dependencies.modules.iter() {
                    if contains_fn_def(&module.ast, *orig_id) {
                        new_fn_nodes.push((module_id.clone(), new_fn));
                        break;
                    }
                }
            }
        }
    }

    for (module_id, new_fn) in new_fn_nodes {
        if let Some(module) = ctx.dependencies.modules.get_mut(&module_id) {
            add_fn_to_module(&mut module.ast, new_fn);
        }
    }

    Ok(())
}

fn collect_poly_fn_defs(
    node: &Node,
    type_env: &HashMap<SymbolId, Scheme>,
    poly_fns: &mut HashMap<SymbolId, Node>,
) {
    match &node.kind {
        NodeKind::Module { children } => {
            for child in children {
                collect_poly_fn_defs(child, type_env, poly_fns);
            }
        }
        NodeKind::FnDecl { .. } => {
            if let Some(symbol_id) = node.symbol_id {
                if let Some(scheme) = type_env.get(&symbol_id) {
                    if !scheme.bound_vars.is_empty() {
                        poly_fns.insert(symbol_id, node.clone());
                    }
                }
            }
        }
        _ => {}
    }
}

fn contains_fn_def(node: &Node, target_id: SymbolId) -> bool {
    match &node.kind {
        NodeKind::Module { children } => children
            .iter()
            .any(|child| contains_fn_def(child, target_id)),
        NodeKind::FnDecl { .. } => node.symbol_id == Some(target_id),
        _ => false,
    }
}

fn add_fn_to_module(module_node: &mut Node, fn_node: Node) {
    if let NodeKind::Module { children } = &mut module_node.kind {
        children.push(fn_node);
    }
}

fn build_substitution(scheme: &Scheme, concrete_type: &Ty) -> Result<Substitution, ()> {
    let mut subst_map = HashMap::default();
    collect_type_var_mappings(&scheme.body, concrete_type, &mut subst_map);
    Ok(Substitution(subst_map))
}

fn collect_type_var_mappings(
    scheme_ty: &Ty,
    concrete_ty: &Ty,
    mappings: &mut HashMap<TypeVar, Ty>,
) {
    use crate::types::TyKind;

    match (scheme_ty.kind(), concrete_ty.kind()) {
        (TyKind::Var(tv), _) => {
            mappings.insert(*tv, concrete_ty.clone());
        }
        (TyKind::Fn(s_arg, s_ret), TyKind::Fn(c_arg, c_ret)) => {
            collect_type_var_mappings(s_arg.as_ref(), c_arg.as_ref(), mappings);
            collect_type_var_mappings(s_ret.as_ref(), c_ret.as_ref(), mappings);
        }
        (TyKind::Tuple(s_elems), TyKind::Tuple(c_elems)) => {
            for (s_elem, c_elem) in s_elems.iter().zip(c_elems.iter()) {
                collect_type_var_mappings(s_elem, c_elem, mappings);
            }
        }
        (TyKind::Ctor(s_id, s_args), TyKind::Ctor(c_id, c_args)) if s_id == c_id => {
            for (s_arg, c_arg) in s_args.iter().zip(c_args.iter()) {
                collect_type_var_mappings(s_arg, c_arg, mappings);
            }
        }
        (TyKind::Nocrypt(s_inner), TyKind::Nocrypt(c_inner)) => {
            collect_type_var_mappings(s_inner.as_ref(), c_inner.as_ref(), mappings);
        }
        _ => {}
    }
}

//ast-centric type substitution
//pretty similar to some code in the typechecker,
//but different enough that I'm leaving for now

fn apply_type_substitution(node: &mut Node, subst: &Substitution) {
    if let Some(ty) = &node.ty {
        node.ty = Some(subst.apply(ty));
    }
    match &mut node.kind {
        NodeKind::Module { children } => {
            for child in children {
                apply_type_substitution(child, subst);
            }
        }
        NodeKind::FnDecl {
            args,
            return_type,
            expr,
            ..
        } => {
            for (arg_pattern, arg_type, _) in args.iter_mut() {
                apply_type_substitution(arg_pattern, subst);
                if let Some(type_node) = arg_type {
                    apply_type_substitution(type_node, subst);
                }
            }

            if let Some(ret_type) = return_type {
                apply_type_substitution(ret_type, subst);
            }

            if let Some(body) = expr {
                apply_type_substitution(body, subst);
            }
        }
        NodeKind::ConstDecl {
            const_type, expr, ..
        } => {
            if let Some(type_node) = const_type {
                apply_type_substitution(type_node, subst);
            }
            apply_type_substitution(expr, subst);
        }
        NodeKind::TypeDecl { variants, .. } => {
            for variant in variants {
                apply_type_substitution(variant, subst);
            }
        }
        NodeKind::TypeConstructor { fields, .. } => {
            for (_, field_type, _) in fields {
                apply_type_substitution(field_type, subst);
            }
        }
        NodeKind::TypeAlias { alias, actual, .. } => {
            apply_type_substitution(alias, subst);
            apply_type_substitution(actual, subst);
        }
        NodeKind::Expr { expr } => {
            apply_type_substitution_expr(expr, subst);
        }
        NodeKind::Type { .. } => {
            //
        }
        NodeKind::Import { .. } => {
            //
        }
        NodeKind::FnType { args, return_type } => {
            for arg in args {
                apply_type_substitution(arg, subst);
            }
            if let Some(ret) = return_type {
                apply_type_substitution(ret, subst);
            }
        }
        NodeKind::TupleType { elements } => {
            for elem in elements {
                apply_type_substitution(elem, subst);
            }
        }
    }
}

fn apply_type_substitution_expr(expr: &mut ExprKind, subst: &Substitution) {
    match expr {
        ExprKind::Block(nodes) => {
            for child in nodes {
                apply_type_substitution(child, subst);
            }
        }
        ExprKind::LetBinding {
            symbol_type,
            expr,
            symbols,
        } => {
            if let Some(type_node) = symbol_type {
                apply_type_substitution(type_node, subst);
            }
            apply_type_substitution(expr, subst);
            apply_type_substitution(symbols, subst);
        }
        ExprKind::Closure {
            args, expr: body, ..
        } => {
            for (arg_pattern, arg_type, _) in args.iter_mut() {
                apply_type_substitution(arg_pattern, subst);
                if let Some(type_node) = arg_type {
                    apply_type_substitution(type_node, subst);
                }
            }
            apply_type_substitution(body, subst);
        }
        ExprKind::Match {
            expr: match_expr,
            arms,
        } => {
            apply_type_substitution(match_expr, subst);
            for (patterns, arm_expr) in arms {
                for pattern in patterns {
                    apply_type_substitution(pattern, subst);
                }
                apply_type_substitution(arm_expr, subst);
            }
        }
        ExprKind::BinaryOp { left, right, .. } => {
            apply_type_substitution(left, subst);
            apply_type_substitution(right, subst);
        }
        ExprKind::UnaryOp { expr, .. } => {
            apply_type_substitution(expr, subst);
        }
        ExprKind::FnCall { callee, args } => {
            apply_type_substitution(callee, subst);
            for (_, arg) in args {
                apply_type_substitution(arg, subst);
            }
        }
        ExprKind::FieldAccess { expr, .. } => {
            apply_type_substitution(expr, subst);
        }
        ExprKind::IndexAccess { expr, index } => {
            apply_type_substitution(expr, subst);
            apply_type_substitution(index, subst);
        }
        ExprKind::IfElse {
            condition,
            then_branch,
            else_branch,
        } => {
            apply_type_substitution(condition, subst);
            apply_type_substitution(then_branch, subst);
            if let Some(else_expr) = else_branch {
                apply_type_substitution(else_expr, subst);
            }
        }
        ExprKind::For {
            binding,
            start,
            end,
            body,
        } => {
            apply_type_substitution(binding, subst);
            apply_type_substitution(start, subst);
            apply_type_substitution(end, subst);
            apply_type_substitution(body, subst);
        }
        ExprKind::TupleLit(elements) | ExprKind::ListLit(elements) => {
            for elem in elements {
                apply_type_substitution(elem, subst);
            }
        }
        ExprKind::MapLit(entries) => {
            for (key, value) in entries {
                apply_type_substitution(key, subst);
                apply_type_substitution(value, subst);
            }
        }
        ExprKind::ListPattern(elements) => {
            use crate::ast::ListPatternElement;
            for elem in elements {
                match elem {
                    ListPatternElement::Element(node) => {
                        apply_type_substitution(node, subst);
                    }
                    ListPatternElement::Rest(Some(node)) => {
                        apply_type_substitution(node, subst);
                    }
                    ListPatternElement::Rest(None) => {}
                }
            }
        }
        ExprKind::IntLit(_)
        | ExprKind::RealLit { .. }
        | ExprKind::StringLit(_)
        | ExprKind::Symbol { .. }
        | ExprKind::Discard => {}
    }
}
