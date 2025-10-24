use rustc_hash::FxHashSet as HashSet;

use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    symbol::SymbolId,
};

pub fn closure_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    for (_module_id, module) in ctx.dependencies.modules.iter_mut() {
        analyze_node(&mut module.ast, &mut HashSet::default());
    }
    Ok(())
}

fn analyze_node(node: &mut Node, scope: &mut HashSet<SymbolId>) -> HashSet<SymbolId> {
    match &mut node.kind {
        NodeKind::Module { children } => {
            for child in children.iter_mut() {
                analyze_node(child, scope);
            }
            HashSet::default()
        }
        NodeKind::FnDecl { args, expr, .. } => {
            let mut fn_scope = scope.clone();
            for (arg_pattern, _arg_type, _span) in args.iter() {
                if let Some(symbol_id) = arg_pattern.symbol_id {
                    fn_scope.insert(symbol_id);
                }
            }
            if let Some(body) = expr {
                analyze_node(body, &mut fn_scope);
            }
            HashSet::default()
        }
        NodeKind::ConstDecl { expr, .. } => {
            analyze_node(expr, scope);
            HashSet::default()
        }
        NodeKind::Expr { .. } => analyze_expr(node, scope),
        _ => HashSet::default(),
    }
}

fn analyze_expr(node: &mut Node, scope: &mut HashSet<SymbolId>) -> HashSet<SymbolId> {
    let expr = match &mut node.kind {
        NodeKind::Expr { expr } => expr,
        _ => return HashSet::default(),
    };

    match expr {
        ExprKind::Symbol { .. } => HashSet::default(),

        ExprKind::LetBinding { symbols, expr, .. } => {
            analyze_expr(expr, scope);

            let mut defined = HashSet::default();
            if let Some(symbol_id) = symbols.symbol_id {
                scope.insert(symbol_id);
                defined.insert(symbol_id);
            }
            defined
        }

        ExprKind::Closure {
            args, expr: body, ..
        } => {
            let mut closure_scope = scope.clone();
            let mut closure_params = HashSet::default();

            for (arg_pattern, _arg_type, _span) in args.iter_mut() {
                if let Some(symbol_id) = arg_pattern.symbol_id {
                    closure_scope.insert(symbol_id);
                    closure_params.insert(symbol_id);
                }
            }

            let referenced = collect_references(body);

            let mut captures = Vec::new();
            for symbol_id in referenced {
                if scope.contains(&symbol_id) && !closure_params.contains(&symbol_id) {
                    captures.push(symbol_id);
                }
            }

            node.capture_list = Some(captures);

            analyze_expr(body, &mut closure_scope);

            HashSet::default()
        }

        ExprKind::Match { expr, arms } => {
            analyze_expr(expr, scope);
            for (patterns, arm_expr) in arms.iter_mut() {
                let mut arm_scope = scope.clone();
                for pattern in patterns.iter() {
                    collect_pattern_bindings(pattern, &mut arm_scope);
                }
                analyze_expr(arm_expr, &mut arm_scope);
            }
            HashSet::default()
        }

        ExprKind::Block(nodes) => {
            let mut block_scope = scope.clone();
            for child in nodes.iter_mut() {
                analyze_expr(child, &mut block_scope);
            }
            HashSet::default()
        }

        ExprKind::BinaryOp { left, right, .. } => {
            analyze_expr(left, scope);
            analyze_expr(right, scope);
            HashSet::default()
        }

        ExprKind::UnaryOp { expr, .. } => {
            analyze_expr(expr, scope);
            HashSet::default()
        }

        ExprKind::FnCall { callee, args } => {
            analyze_expr(callee, scope);
            for (_name, arg) in args.iter_mut() {
                analyze_expr(arg, scope);
            }
            HashSet::default()
        }

        ExprKind::FieldAccess { expr, .. } => {
            analyze_expr(expr, scope);
            HashSet::default()
        }

        ExprKind::IndexAccess { expr, index } => {
            analyze_expr(expr, scope);
            analyze_expr(index, scope);
            HashSet::default()
        }

        ExprKind::IfElse {
            condition,
            then_branch,
            else_branch,
        } => {
            analyze_expr(condition, scope);
            analyze_expr(then_branch, scope);
            if let Some(else_expr) = else_branch {
                analyze_expr(else_expr, scope);
            }
            HashSet::default()
        }

        ExprKind::For {
            binding,
            start,
            end,
            body,
        } => {
            analyze_expr(start, scope);
            analyze_expr(end, scope);

            let mut for_scope = scope.clone();
            if let Some(symbol_id) = binding.symbol_id {
                for_scope.insert(symbol_id);
            }
            analyze_expr(body, &mut for_scope);
            HashSet::default()
        }

        ExprKind::TupleLit(elements) | ExprKind::ListLit(elements) => {
            for elem in elements.iter_mut() {
                analyze_expr(elem, scope);
            }
            HashSet::default()
        }

        ExprKind::MapLit(entries) => {
            for (key, value) in entries.iter_mut() {
                analyze_expr(key, scope);
                analyze_expr(value, scope);
            }
            HashSet::default()
        }

        ExprKind::IntLit(_)
        | ExprKind::RealLit { .. }
        | ExprKind::StringLit(_)
        | ExprKind::Discard
        | ExprKind::ListPattern(_) => HashSet::default(),
    }
}

fn collect_references(node: &Node) -> HashSet<SymbolId> {
    let mut refs = HashSet::default();
    collect_references_helper(node, &mut refs);
    refs
}

fn collect_references_helper(node: &Node, refs: &mut HashSet<SymbolId>) {
    if let NodeKind::Expr {
        expr: ExprKind::Symbol { .. },
    } = &node.kind
    {
        if let Some(symbol_id) = node.symbol_id {
            refs.insert(symbol_id);
        }
    }

    match &node.kind {
        NodeKind::Expr { expr } => match expr {
            ExprKind::LetBinding {
                symbols: _, expr, ..
            } => {
                collect_references_helper(expr, refs);
            }
            ExprKind::Closure { expr: body, .. } => {
                collect_references_helper(body, refs);
            }
            ExprKind::Match { expr, arms } => {
                collect_references_helper(expr, refs);
                for (_patterns, arm_expr) in arms {
                    collect_references_helper(arm_expr, refs);
                }
            }
            ExprKind::Block(nodes) => {
                for child in nodes {
                    collect_references_helper(child, refs);
                }
            }
            ExprKind::BinaryOp { left, right, .. } => {
                collect_references_helper(left, refs);
                collect_references_helper(right, refs);
            }
            ExprKind::UnaryOp { expr, .. } => {
                collect_references_helper(expr, refs);
            }
            ExprKind::FnCall { callee, args } => {
                collect_references_helper(callee, refs);
                for (_name, arg) in args {
                    collect_references_helper(arg, refs);
                }
            }
            ExprKind::FieldAccess { expr, .. } => {
                collect_references_helper(expr, refs);
            }
            ExprKind::IndexAccess { expr, index } => {
                collect_references_helper(expr, refs);
                collect_references_helper(index, refs);
            }
            ExprKind::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                collect_references_helper(condition, refs);
                collect_references_helper(then_branch, refs);
                if let Some(else_expr) = else_branch {
                    collect_references_helper(else_expr, refs);
                }
            }
            ExprKind::For {
                start, end, body, ..
            } => {
                collect_references_helper(start, refs);
                collect_references_helper(end, refs);
                collect_references_helper(body, refs);
            }
            ExprKind::TupleLit(elements) | ExprKind::ListLit(elements) => {
                for elem in elements {
                    collect_references_helper(elem, refs);
                }
            }
            ExprKind::MapLit(entries) => {
                for (key, value) in entries {
                    collect_references_helper(key, refs);
                    collect_references_helper(value, refs);
                }
            }
            _ => {}
        },
        _ => {}
    }
}

fn collect_pattern_bindings(pattern: &Node, scope: &mut HashSet<SymbolId>) {
    match &pattern.kind {
        NodeKind::Expr { expr } => match expr {
            ExprKind::Symbol { .. } => {
                if let Some(symbol_id) = pattern.symbol_id {
                    scope.insert(symbol_id);
                }
            }
            ExprKind::TupleLit(elements) => {
                for elem in elements {
                    collect_pattern_bindings(elem, scope);
                }
            }
            ExprKind::FnCall { args, .. } => {
                for (_name, arg) in args {
                    collect_pattern_bindings(arg, scope);
                }
            }
            ExprKind::ListPattern(elements) => {
                use crate::ast::ListPatternElement;
                for elem in elements {
                    match elem {
                        ListPatternElement::Element(node) => {
                            collect_pattern_bindings(node, scope);
                        }
                        ListPatternElement::Rest(Some(node)) => {
                            collect_pattern_bindings(node, scope);
                        }
                        ListPatternElement::Rest(None) => {}
                    }
                }
            }
            _ => {}
        },
        _ => {}
    }
}
