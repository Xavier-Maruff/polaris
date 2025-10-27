use rustc_hash::FxHashMap as HashMap;
use std::collections::HashSet;

use crate::{
    ast::{Node, NodeKind},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    symbol::SymbolId,
    types::{Scheme, Substitution, Ty, TyKind, TypeVar},
    visitor::visit_ast_mut,
};

const WARN_MONOMORPH_MAX: usize = 50;

pub fn monomorphise_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    collect_type_instantiations(ctx)?;
    monomorphise_types(ctx)?;
    monomorphise_fns(ctx)?;
    Ok(())
}

fn collect_type_instantiations(ctx: &mut CompileContext) -> Result<(), ()> {
    let module_ids: Vec<String> = ctx
        .dependencies
        .modules
        .keys()
        .cloned()
        .collect();

    for module_id in module_ids {
        let mut tys: Vec<Ty> = Vec::new();
        if let Some(module) = ctx.dependencies.modules.get_mut(&module_id) {
            visit_ast_mut(&mut module.ast, &mut |node: &mut Node| {
                if let Some(ty) = &node.ty {
                    tys.push(ty.clone());
                }
                Ok(())
            })?;
        }

        for ty in tys {
            collect_from_type(&ty, ctx)?;
        }
    }

    Ok(())
}

fn collect_from_type(ty: &Ty, ctx: &mut CompileContext) -> Result<(), ()> {
    match ty.kind() {
        TyKind::Ctor(type_id, args) => {
            if let Some(scheme) = ctx.type_info.type_env.get(type_id) {
                if !scheme.bound_vars.is_empty() && !args.is_empty() {
                    let key = (*type_id, args.clone());
                    if !ctx.type_info.type_instantiation_ids.contains_key(&key) {
                        let new_type_id = ctx.symbols.symbol_idx;
                        ctx.symbols.symbol_idx += 1;

                        ctx.type_info
                            .type_instantiation_ids
                            .insert(key, new_type_id);
                        ctx.type_info
                            .monomorphised_types
                            .insert(new_type_id, (*type_id, args.clone()));

                        if let Some(orig_name) = ctx.symbols.symbol_names.get(type_id) {
                            let args_str = args
                                .iter()
                                .map(|arg| format_type_for_name(arg, ctx))
                                .collect::<Vec<_>>()
                                .join("_");
                            ctx.symbols
                                .symbol_names
                                .insert(new_type_id, format!("{}__{}", orig_name, args_str));
                        }
                    }
                }
            }

            args.iter()
                .try_for_each(|arg| collect_from_type(arg, ctx))?;
        }
        TyKind::Fn(arg, ret) => {
            collect_from_type(arg, ctx)?;
            collect_from_type(ret, ctx)?;
        }
        TyKind::Tuple(elements) => {
            elements
                .iter()
                .try_for_each(|elem| collect_from_type(elem, ctx))?;
        }
        TyKind::Nocrypt(inner) => collect_from_type(inner, ctx)?,
        _ => {}
    }

    Ok(())
}

fn format_type_for_name(ty: &Ty, ctx: &CompileContext) -> String {
    match ty.kind() {
        TyKind::Concrete(id) => ctx
            .symbols
            .symbol_names
            .get(id)
            .cloned()
            .unwrap_or_else(|| format!("T{}", id)),
        TyKind::IntLiteral(n) => n.to_string(),
        TyKind::Ctor(id, args) => {
            let base = ctx
                .symbols
                .symbol_names
                .get(id)
                .cloned()
                .unwrap_or_else(|| format!("T{}", id));
            if args.is_empty() {
                base
            } else {
                let args_str = args
                    .iter()
                    .map(|arg| format_type_for_name(arg, ctx))
                    .collect::<Vec<_>>()
                    .join("_");
                format!("{}_{}", base, args_str)
            }
        }
        _ => "T".to_string(),
    }
}

fn monomorphise_types(ctx: &mut CompileContext) -> Result<(), ()> {
    let type_monomorphisations = ctx.type_info.monomorphised_types.clone();
    if type_monomorphisations.is_empty() {
        return Ok(());
    }

    let polymorphic_type_ids: HashSet<SymbolId> = ctx
        .type_info
        .type_env
        .iter()
        .filter_map(|(id, scheme)| (!scheme.bound_vars.is_empty()).then_some(*id))
        .collect();

    let mut poly_type_defs: HashMap<SymbolId, Node> = HashMap::default();
    for module in ctx.dependencies.modules.values_mut() {
        let mut f = |node: &mut Node| {
            if matches!(node.kind, NodeKind::TypeDecl { .. }) {
                if let Some(symbol_id) = node.symbol_id {
                    if polymorphic_type_ids.contains(&symbol_id) {
                        poly_type_defs.insert(symbol_id, node.clone());
                    }
                }
            }
            Ok(())
        };
        visit_ast_mut(&mut module.ast, &mut f)?;
    }

    warn_excessive("Type", &type_monomorphisations, &poly_type_defs, ctx);

    let mut new_type_nodes: Vec<(String, Node)> = Vec::new();

    for (new_type_id, (orig_type_id, concrete_args)) in &type_monomorphisations {
        if let Some(orig_type) = poly_type_defs.get(orig_type_id) {
            if let Some(scheme) = ctx.type_info.type_env.get(orig_type_id) {
                let mut new_type = orig_type.clone();
                let subst = build_type_substitution(scheme, concrete_args)?;

                visit_ast_mut(&mut new_type, &mut |node| {
                    if let Some(ty) = &node.ty {
                        node.ty = Some(subst.apply(ty));
                    }
                    Ok(())
                })?;

                new_type.symbol_id = Some(*new_type_id);

                if let NodeKind::TypeDecl { variants, .. } = &mut new_type.kind {
                    for variant in variants {
                        if let Some(orig_ctor_id) = variant.symbol_id {
                            let new_ctor_id = ctx.symbols.symbol_idx;
                            ctx.symbols.symbol_idx += 1;
                            variant.symbol_id = Some(new_ctor_id);
                            ctx.symbols
                                .type_constructors
                                .insert(new_ctor_id, *new_type_id);
                            if let Some(name) = ctx.symbols.symbol_names.get(&orig_ctor_id) {
                                ctx.symbols.symbol_names.insert(new_ctor_id, name.clone());
                            }
                        }
                    }
                }

                if let Some((module_id, _)) = ctx
                    .dependencies
                    .modules
                    .iter()
                    .find(|(_, m)| contains_def(&m.ast, *orig_type_id, true))
                {
                    new_type_nodes.push((module_id.clone(), new_type));
                }
            }
        }
    }

    for (module_id, new_type) in new_type_nodes {
        if let Some(module) = ctx.dependencies.modules.get_mut(&module_id) {
            if let NodeKind::Module { children } = &mut module.ast.kind {
                children.push(new_type);
            }
        }
    }

    Ok(())
}

fn monomorphise_fns(ctx: &mut CompileContext) -> Result<(), ()> {
    let fn_monomorphisations = ctx.type_info.monomorphised_fns.clone();
    if fn_monomorphisations.is_empty() {
        return Ok(());
    }

    let polymorphic_fn_ids: HashSet<SymbolId> = ctx
        .type_info
        .type_env
        .iter()
        .filter_map(|(id, scheme)| (!scheme.bound_vars.is_empty()).then_some(*id))
        .collect();

    let mut poly_fn_defs: HashMap<SymbolId, Node> = HashMap::default();
    for module in ctx.dependencies.modules.values_mut() {
        let mut f = |node: &mut Node| {
            if matches!(node.kind, NodeKind::FnDecl { .. }) {
                if let Some(symbol_id) = node.symbol_id {
                    if polymorphic_fn_ids.contains(&symbol_id) {
                        poly_fn_defs.insert(symbol_id, node.clone());
                    }
                }
            }
            Ok(())
        };
        visit_ast_mut(&mut module.ast, &mut f)?;
    }

    warn_excessive("Function", &fn_monomorphisations, &poly_fn_defs, ctx);

    let mut new_fn_nodes: Vec<(String, Node)> = Vec::new();

    for (new_fn_id, (orig_fn_id, concrete_type)) in &fn_monomorphisations {
        if let Some(orig_fn) = poly_fn_defs.get(orig_fn_id) {
            if let Some(scheme) = ctx.type_info.type_env.get(orig_fn_id) {
                let mut new_fn = orig_fn.clone();
                let subst = build_fn_substitution(scheme, concrete_type)?;

                visit_ast_mut(&mut new_fn, &mut |node| {
                    if let Some(ty) = &node.ty {
                        node.ty = Some(subst.apply(ty));
                    }
                    Ok(())
                })?;

                new_fn.symbol_id = Some(*new_fn_id);

                if let Some((module_id, _)) = ctx
                    .dependencies
                    .modules
                    .iter()
                    .find(|(_, m)| contains_def(&m.ast, *orig_fn_id, false))
                {
                    new_fn_nodes.push((module_id.clone(), new_fn));
                }
            }
        }
    }

    for (module_id, new_fn) in new_fn_nodes {
        if let Some(module) = ctx.dependencies.modules.get_mut(&module_id) {
            if let NodeKind::Module { children } = &mut module.ast.kind {
                children.push(new_fn);
            }
        }
    }

    Ok(())
}

fn warn_excessive<T>(
    kind: &str,
    monomorphisations: &HashMap<SymbolId, (SymbolId, T)>,
    poly_defs: &HashMap<SymbolId, Node>,
    ctx: &mut CompileContext,
) {
    let mut counts: HashMap<SymbolId, usize> = HashMap::default();
    for (_, (orig_id, _)) in monomorphisations {
        *counts.entry(*orig_id).or_insert(0) += 1;
    }

    for (orig_id, count) in &counts {
        if *count > WARN_MONOMORPH_MAX {
            if let Some(orig_node) = poly_defs.get(orig_id) {
                let name = match &orig_node.kind {
                    NodeKind::FnDecl { symbol, .. } | NodeKind::TypeDecl { symbol, .. } => {
                        symbol.clone()
                    }
                    _ => "<unknown>".to_string(),
                };

                let file = ctx
                    .dependencies
                    .modules
                    .values()
                    .find(|m| contains_def(&m.ast, *orig_id, kind == "Type"))
                    .map(|m| m.file.clone())
                    .unwrap_or_else(|| "<unknown>".to_string());

                ctx.warnings.push(Diagnostic::new(DiagnosticMsg {
                    message: format!(
                        "{} '{}' has been monomorphised {} times (threshold: {}). Your binary may be mega large.",
                        kind, name, count, WARN_MONOMORPH_MAX
                    ),
                    span: orig_node.span.clone(),
                    file,
                    err_type: DiagnosticMsgType::ExcessiveMonomorphisation,
                }));
            }
        }
    }
}

fn contains_def(node: &Node, target_id: SymbolId, is_type: bool) -> bool {
    if node.symbol_id == Some(target_id) {
        return if is_type {
            matches!(node.kind, NodeKind::TypeDecl { .. })
        } else {
            matches!(node.kind, NodeKind::FnDecl { .. })
        };
    }

    if let NodeKind::Module { children } = &node.kind {
        return children
            .iter()
            .any(|child| contains_def(child, target_id, is_type));
    }

    false
}

fn build_fn_substitution(scheme: &Scheme, concrete_type: &Ty) -> Result<Substitution, ()> {
    let mut subst_map = HashMap::default();
    collect_type_var_mappings(&scheme.body, concrete_type, &mut subst_map);
    Ok(Substitution(subst_map))
}

fn build_type_substitution(scheme: &Scheme, concrete_args: &[Ty]) -> Result<Substitution, ()> {
    let mut subst_map = HashMap::default();
    for (&bound_var, arg) in scheme.bound_vars.iter().zip(concrete_args.iter()) {
        subst_map.insert(bound_var, arg.clone());
    }
    Ok(Substitution(subst_map))
}

fn collect_type_var_mappings(
    scheme_ty: &Ty,
    concrete_ty: &Ty,
    mappings: &mut HashMap<TypeVar, Ty>,
) {
    match (scheme_ty.kind(), concrete_ty.kind()) {
        (TyKind::Var(tv), _) | (TyKind::SizeVar(tv), _) => {
            mappings.insert(*tv, concrete_ty.clone());
        }
        (TyKind::Fn(s_arg, s_ret), TyKind::Fn(c_arg, c_ret)) => {
            collect_type_var_mappings(s_arg, c_arg, mappings);
            collect_type_var_mappings(s_ret, c_ret, mappings);
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
            collect_type_var_mappings(s_inner, c_inner, mappings);
        }
        _ => {}
    }
}
