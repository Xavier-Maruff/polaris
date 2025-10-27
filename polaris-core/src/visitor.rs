use crate::ast::{ExprKind, ListPatternElement, Node, NodeKind};

pub fn visit_ast_mut<F>(node: &mut Node, f: &mut F) -> Result<(), ()>
where
    F: FnMut(&mut Node) -> Result<(), ()>,
{
    f(node)?;
    visit_ast_mut_children(node, f)
}

pub fn visit_ast_mut_children<F>(node: &mut Node, f: &mut F) -> Result<(), ()>
where
    F: FnMut(&mut Node) -> Result<(), ()>,
{
    match &mut node.kind {
        NodeKind::Module { children } => {
            for child in children {
                visit_ast_mut(child, f)?;
            }
        }
        NodeKind::FnDecl {
            args,
            return_type,
            expr,
            ..
        } => {
            for (arg_pattern, arg_type, _) in args {
                visit_ast_mut(arg_pattern, f)?;
                if let Some(t) = arg_type {
                    visit_ast_mut(t, f)?;
                }
            }
            if let Some(ret) = return_type {
                visit_ast_mut(ret, f)?;
            }
            if let Some(body) = expr {
                visit_ast_mut(body, f)?;
            }
        }
        NodeKind::ConstDecl {
            const_type, expr, ..
        } => {
            if let Some(t) = const_type {
                visit_ast_mut(t, f)?;
            }
            visit_ast_mut(expr, f)?;
        }
        NodeKind::TypeDecl { variants, .. } => {
            for variant in variants {
                visit_ast_mut(variant, f)?;
            }
        }
        NodeKind::TypeConstructor { fields, .. } => {
            for (_, field_type, _) in fields {
                visit_ast_mut(field_type, f)?;
            }
        }
        NodeKind::TypeAlias { alias, actual, .. } => {
            visit_ast_mut(alias, f)?;
            visit_ast_mut(actual, f)?;
        }
        NodeKind::FnType { args, return_type } => {
            for arg in args {
                visit_ast_mut(arg, f)?;
            }
            if let Some(ret) = return_type {
                visit_ast_mut(ret, f)?;
            }
        }
        NodeKind::TupleType { elements } => {
            for elem in elements {
                visit_ast_mut(elem, f)?;
            }
        }
        NodeKind::Expr { expr } => visit_expr_mut(expr, f)?,
        _ => {}
    }
    Ok(())
}

pub fn visit_expr_mut<F>(expr: &mut ExprKind, f: &mut F) -> Result<(), ()>
where
    F: FnMut(&mut Node) -> Result<(), ()>,
{
    match expr {
        ExprKind::Block(ns) | ExprKind::TupleLit(ns) | ExprKind::ListLit(ns) => {
            for n in ns {
                visit_ast_mut(n, f)?;
            }
        }
        ExprKind::LetBinding {
            symbol_type,
            expr,
            symbols,
        } => {
            if let Some(t) = symbol_type {
                visit_ast_mut(t, f)?;
            }
            visit_ast_mut(expr, f)?;
            visit_ast_mut(symbols, f)?;
        }
        ExprKind::Closure {
            args,
            return_type,
            expr: body,
        } => {
            for (arg_pattern, arg_type, _) in args {
                visit_ast_mut(arg_pattern, f)?;
                if let Some(t) = arg_type {
                    visit_ast_mut(t, f)?;
                }
            }
            if let Some(ret) = return_type {
                visit_ast_mut(ret, f)?;
            }
            visit_ast_mut(body, f)?;
        }
        ExprKind::Match {
            expr: match_expr,
            arms,
        } => {
            visit_ast_mut(match_expr, f)?;
            for (patterns, arm_expr) in arms {
                for pattern in patterns {
                    visit_ast_mut(pattern, f)?;
                }
                visit_ast_mut(arm_expr, f)?;
            }
        }
        ExprKind::BinaryOp { left, right, .. } => {
            visit_ast_mut(left, f)?;
            visit_ast_mut(right, f)?;
        }
        ExprKind::UnaryOp { expr, .. } | ExprKind::FieldAccess { expr, .. } => {
            visit_ast_mut(expr, f)?;
        }
        ExprKind::IndexAccess { expr, index } => {
            visit_ast_mut(expr, f)?;
            visit_ast_mut(index, f)?;
        }
        ExprKind::FnCall { callee, args } => {
            visit_ast_mut(callee, f)?;
            for (_, arg) in args {
                visit_ast_mut(arg, f)?;
            }
        }
        ExprKind::IfElse {
            condition,
            then_branch,
            else_branch,
        } => {
            visit_ast_mut(condition, f)?;
            visit_ast_mut(then_branch, f)?;
            if let Some(e) = else_branch {
                visit_ast_mut(e, f)?;
            }
        }
        ExprKind::For {
            binding,
            start,
            end,
            body,
        } => {
            visit_ast_mut(binding, f)?;
            visit_ast_mut(start, f)?;
            visit_ast_mut(end, f)?;
            visit_ast_mut(body, f)?;
        }
        ExprKind::MapLit(entries) => {
            for (key, value) in entries {
                visit_ast_mut(key, f)?;
                visit_ast_mut(value, f)?;
            }
        }
        ExprKind::ListPattern(elements) => {
            for elem in elements {
                if let ListPatternElement::Element(n) | ListPatternElement::Rest(Some(n)) = elem {
                    visit_ast_mut(n, f)?;
                }
            }
        }
        _ => {}
    }
    Ok(())
}
