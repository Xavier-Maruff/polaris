use rustc_hash::FxHashMap as HashMap;
use rustc_hash::FxHashSet as HashSet;

use crate::{
    ast::{AllocationStrategy, ExprKind, Node, NodeKind, ParameterSemantics, UseSemantics},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    parse::CodeSpan,
    symbol::SymbolId,
    types::Ty,
};

#[derive(Clone, Debug, Default)]
pub struct LifetimeInfo {
    pub escapes: HashSet<SymbolId>,
}

pub fn lifetime_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let module_ids: Vec<String> = ctx.dependencies.modules.keys().cloned().collect();

    for module_id in module_ids {
        if let Some(module) = ctx.dependencies.modules.get_mut(&module_id) {
            let file = module.file.clone();
            let diagnostics = analyse_module(&mut module.ast, &file, &ctx.symbols);
            ctx.warnings.extend(diagnostics);
        }
    }

    Ok(())
}

fn analyse_module(
    node: &mut Node,
    file: &str,
    symbols: &crate::symbol::SymbolContext,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let mut arena_counter = 0;
    visit_node(node, file, symbols, &mut diagnostics, &mut arena_counter);
    diagnostics
}

fn visit_node(
    node: &mut Node,
    file: &str,
    symbols: &crate::symbol::SymbolContext,
    diagnostics: &mut Vec<Diagnostic>,
    arena_counter: &mut usize,
) {
    match &mut node.kind {
        NodeKind::Module { children } => {
            for child in children {
                visit_node(child, file, symbols, diagnostics, arena_counter);
            }
        }
        NodeKind::FnDecl {
            expr: Some(body),
            args,
            ..
        } => {
            //base arena ID for func
            let arena_id = *arena_counter;
            *arena_counter += 1;

            let mut ctx = AnalysisContext::new(arena_id);

            for (param_pattern, _, _) in args.iter() {
                if let Some(symbol_id) = param_pattern.symbol_id {
                    ctx.record_binding(symbol_id, param_pattern.ty.clone());
                }
            }

            //last expr is in return position
            visit_expr(body, &mut ctx, true);

            ctx.compute_arena_assignments(arena_id);
            emit_diagnostics(&ctx, file, symbols, diagnostics);

            annotate_params(args, &ctx);
            annotate(body, &ctx);
        }
        _ => {}
    }
}

#[derive(Clone)]
struct AnalysisContext {
    bindings: HashMap<SymbolId, BindingData>,
    use_sites: Vec<UseSite>,
    current_order: usize,
    closure_escapes: HashMap<usize, bool>,
    symbol_spans: HashMap<SymbolId, CodeSpan>,
    current_arena_id: usize,
}

#[derive(Clone, Debug)]
struct BindingData {
    escapes_function: bool,
    captured_by_closure: bool,
    captured_by_escaping_closure: bool,
    shared_across_branches: bool,
    has_multiple_uses: bool,
    ty: Option<Ty>,
    birth_order: usize,
    arena_id: Option<usize>,
}

#[derive(Clone, Debug)]
struct UseSite {
    symbol_id: SymbolId,
    order: usize,
    context: UseContext,
    node_ptr: usize,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum UseContext {
    Read,
    Consumed,
    Returned,
    Escaped,
}

impl AnalysisContext {
    fn new(arena_id: usize) -> Self {
        Self {
            bindings: HashMap::default(),
            use_sites: Vec::new(),
            current_order: 0,
            closure_escapes: HashMap::default(),
            symbol_spans: HashMap::default(),
            current_arena_id: arena_id,
        }
    }

    fn record_binding(&mut self, symbol_id: SymbolId, ty: Option<Ty>) {
        let birth_order = self.current_order;
        self.current_order += 1;

        self.bindings.insert(
            symbol_id,
            BindingData {
                escapes_function: false,
                captured_by_closure: false,
                captured_by_escaping_closure: false,
                shared_across_branches: false,
                has_multiple_uses: false,
                ty,
                birth_order,
                arena_id: None,
            },
        );
    }

    fn record_binding_span(&mut self, symbol_id: SymbolId, span: CodeSpan) {
        self.symbol_spans.insert(symbol_id, span);
    }

    fn record_use(&mut self, symbol_id: SymbolId, context: UseContext, node: &Node) {
        self.use_sites.push(UseSite {
            symbol_id,
            order: self.current_order,
            context,
            node_ptr: node as *const Node as usize,
        });

        if let Some(data) = self.bindings.get_mut(&symbol_id) {
            let existing_uses = self
                .use_sites
                .iter()
                .filter(|u| u.symbol_id == symbol_id)
                .count();

            if existing_uses > 1 {
                data.has_multiple_uses = true;
            }
        }

        self.current_order += 1;
    }

    fn mark_escaped_function(&mut self, symbol_id: SymbolId) {
        if let Some(data) = self.bindings.get_mut(&symbol_id) {
            data.escapes_function = true;
        }
    }

    fn mark_captured_by_closure(
        &mut self,
        symbol_id: SymbolId,
        _closure_ptr: usize,
        closure_escapes: bool,
    ) {
        if let Some(data) = self.bindings.get_mut(&symbol_id) {
            data.captured_by_closure = true;
            if closure_escapes {
                data.captured_by_escaping_closure = true;
                data.escapes_function = true;
            }
        }
    }

    fn mark_shared_across_branches(&mut self, symbol_id: SymbolId) {
        if let Some(data) = self.bindings.get_mut(&symbol_id) {
            data.shared_across_branches = true;
        }
    }

    fn merge_branch(&mut self, branch_ctx: &AnalysisContext) {
        for (symbol_id, branch_data) in &branch_ctx.bindings {
            if let Some(data) = self.bindings.get_mut(symbol_id) {
                data.escapes_function = data.escapes_function || branch_data.escapes_function;
                data.captured_by_closure =
                    data.captured_by_closure || branch_data.captured_by_closure;
                data.captured_by_escaping_closure =
                    data.captured_by_escaping_closure || branch_data.captured_by_escaping_closure;
                data.shared_across_branches =
                    data.shared_across_branches || branch_data.shared_across_branches;
                data.has_multiple_uses = data.has_multiple_uses || branch_data.has_multiple_uses;
            }
        }

        for use_site in &branch_ctx.use_sites {
            let existing_count = self
                .use_sites
                .iter()
                .filter(|u| u.symbol_id == use_site.symbol_id)
                .count();
            let branch_count = branch_ctx
                .use_sites
                .iter()
                .filter(|u| u.symbol_id == use_site.symbol_id)
                .count();

            if existing_count > 0 && branch_count > 0 {
                self.mark_shared_across_branches(use_site.symbol_id);
            }
        }

        self.use_sites.extend(branch_ctx.use_sites.clone());
    }

    // bindings with non-overlapping lifetimes can share the same arena
    fn compute_arena_assignments(&mut self, base_arena_id: usize) {
        #[derive(Debug)]
        struct LifetimeInterval {
            symbol_id: SymbolId,
            birth: usize,
            death: usize,
        }

        let mut intervals: Vec<LifetimeInterval> = Vec::new();

        for (symbol_id, data) in &self.bindings {
            //skip escaping bindings - no arena for you
            if data.escapes_function || data.captured_by_escaping_closure {
                continue;
            }

            let birth = data.birth_order;

            let death = self
                .use_sites
                .iter()
                .filter(|u| u.symbol_id == *symbol_id)
                .map(|u| u.order)
                .max()
                .unwrap_or(birth);

            intervals.push(LifetimeInterval {
                symbol_id: *symbol_id,
                birth,
                death,
            });
        }

        intervals.sort_by_key(|i| i.birth);

        //greedy coloring
        // arena id -> [symbol_ids]
        let mut arena_assignments: Vec<Vec<usize>> = Vec::new(); // arena_id -> [symbol_ids]

        for interval in intervals {
            //currently occupied arenas at birth
            let mut available_arena: Option<usize> = None;

            for (arena_idx, arena_symbols) in arena_assignments.iter().enumerate() {
                let is_free = arena_symbols.iter().all(|&other_symbol_id| {
                    if let Some(other_data) = self.bindings.get(&other_symbol_id) {
                        let other_death = self
                            .use_sites
                            .iter()
                            .filter(|u| u.symbol_id == other_symbol_id)
                            .map(|u| u.order)
                            .max()
                            .unwrap_or(other_data.birth_order);

                        //no overlap
                        other_death < interval.birth || other_data.birth_order > interval.death
                    } else {
                        true
                    }
                });

                if is_free {
                    available_arena = Some(arena_idx);
                    break;
                }
            }

            let arena_idx = if let Some(idx) = available_arena {
                idx
            } else {
                //need a new arena if none available
                let idx = arena_assignments.len();
                arena_assignments.push(Vec::new());
                idx
            };

            arena_assignments[arena_idx].push(interval.symbol_id);

            if let Some(data) = self.bindings.get_mut(&interval.symbol_id) {
                data.arena_id = Some(base_arena_id + arena_idx);
            }
        }
    }
}

fn visit_expr(node: &Node, ctx: &mut AnalysisContext, is_return_position: bool) {
    match &node.kind {
        NodeKind::Expr { expr } => match expr {
            ExprKind::Symbol { .. } => {
                if let Some(symbol_id) = node.symbol_id {
                    let context = if is_return_position {
                        UseContext::Returned
                    } else {
                        UseContext::Read
                    };
                    ctx.record_use(symbol_id, context, node);

                    if is_return_position {
                        ctx.mark_escaped_function(symbol_id);
                    }
                }
            }

            ExprKind::LetBinding {
                symbols,
                expr: value_expr,
                ..
            } => {
                visit_expr(value_expr, ctx, false);

                if let Some(symbol_id) = symbols.symbol_id {
                    ctx.record_binding(symbol_id, value_expr.ty.clone());
                    ctx.record_binding_span(symbol_id, symbols.span.clone());
                }
            }

            ExprKind::FnCall { callee, args } => {
                visit_expr(callee, ctx, false);

                for (_label, arg) in args {
                    visit_expr_with_context(arg, ctx, UseContext::Consumed);
                }
            }

            ExprKind::BinaryOp { left, right, .. } => {
                visit_expr(left, ctx, false);
                visit_expr(right, ctx, false);
            }

            ExprKind::UnaryOp { expr, .. } => {
                visit_expr(expr, ctx, false);
            }

            ExprKind::Block(nodes) => {
                for (i, child) in nodes.iter().enumerate() {
                    let is_last = i == nodes.len() - 1;
                    visit_expr(child, ctx, is_return_position && is_last);
                }
            }

            ExprKind::Match {
                expr: match_expr,
                arms,
            } => {
                visit_expr(match_expr, ctx, false);

                let mut arm_ctxs = Vec::new();
                for (patterns, arm_expr) in arms {
                    let mut arm_ctx = ctx.clone();

                    for pattern in patterns {
                        visit_pattern(pattern, &mut arm_ctx);
                    }

                    visit_expr(arm_expr, &mut arm_ctx, is_return_position);
                    arm_ctxs.push(arm_ctx);
                }

                for arm_ctx in &arm_ctxs {
                    ctx.merge_branch(arm_ctx);
                }
            }

            ExprKind::Closure {
                args, expr: body, ..
            } => {
                //inherit current arena ID
                let mut closure_ctx = AnalysisContext::new(ctx.current_arena_id);
                let closure_ptr = node as *const Node as usize;

                for (arg_pattern, _, _) in args {
                    if let Some(symbol_id) = arg_pattern.symbol_id {
                        closure_ctx.record_binding(symbol_id, arg_pattern.ty.clone());
                    }
                }

                visit_expr(body, &mut closure_ctx, true);

                let closure_escapes = is_return_position;
                ctx.closure_escapes.insert(closure_ptr, closure_escapes);

                if let Some(capture_list) = &node.capture_list {
                    for &captured_id in capture_list {
                        //record capture use
                        let use_context = if closure_escapes {
                            UseContext::Escaped
                        } else {
                            UseContext::Read
                        };
                        ctx.record_use(captured_id, use_context, node);
                        ctx.mark_captured_by_closure(captured_id, closure_ptr, closure_escapes);
                    }
                }
            }

            ExprKind::TupleLit(elements) | ExprKind::ListLit(elements) => {
                for elem in elements {
                    let elem_context = if is_return_position {
                        UseContext::Escaped
                    } else {
                        UseContext::Read
                    };
                    visit_expr_with_context(elem, ctx, elem_context);
                }
            }

            ExprKind::MapLit(entries) => {
                for (key, value) in entries {
                    let elem_context = if is_return_position {
                        UseContext::Escaped
                    } else {
                        UseContext::Read
                    };
                    visit_expr_with_context(key, ctx, elem_context);
                    visit_expr_with_context(value, ctx, elem_context);
                }
            }

            ExprKind::FieldAccess { expr, .. } => {
                visit_expr(expr, ctx, false);
            }

            ExprKind::IndexAccess { expr, index } => {
                visit_expr(expr, ctx, false);
                visit_expr(index, ctx, false);
            }

            ExprKind::For {
                binding,
                start,
                end,
                body,
            } => {
                visit_expr(start, ctx, false);
                visit_expr(end, ctx, false);

                let mut for_ctx = ctx.clone();
                if let Some(symbol_id) = binding.symbol_id {
                    for_ctx.record_binding(symbol_id, binding.ty.clone());
                }
                visit_expr(body, &mut for_ctx, false);
                ctx.merge_branch(&for_ctx);
            }

            _ => {}
        },
        _ => {}
    }
}

fn visit_expr_with_context(node: &Node, ctx: &mut AnalysisContext, use_context: UseContext) {
    if let NodeKind::Expr {
        expr: ExprKind::Symbol { .. },
    } = &node.kind
    {
        if let Some(symbol_id) = node.symbol_id {
            ctx.record_use(symbol_id, use_context, node);
            if use_context == UseContext::Escaped {
                ctx.mark_escaped_function(symbol_id);
            }
        }
    } else {
        visit_expr(node, ctx, false);
    }
}

fn visit_pattern(pattern: &Node, ctx: &mut AnalysisContext) {
    match &pattern.kind {
        NodeKind::Expr { expr } => match expr {
            ExprKind::Symbol { .. } => {
                if let Some(symbol_id) = pattern.symbol_id {
                    ctx.record_binding(symbol_id, pattern.ty.clone());
                }
            }
            ExprKind::TupleLit(elements) => {
                for elem in elements {
                    visit_pattern(elem, ctx);
                }
            }
            ExprKind::FnCall { args, .. } => {
                for (_name, arg) in args {
                    visit_pattern(arg, ctx);
                }
            }
            ExprKind::ListPattern(elements) => {
                use crate::ast::ListPatternElement;
                for elem in elements {
                    match elem {
                        ListPatternElement::Element(node) => {
                            visit_pattern(node, ctx);
                        }
                        ListPatternElement::Rest(Some(node)) => {
                            visit_pattern(node, ctx);
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

fn emit_diagnostics(
    ctx: &AnalysisContext,
    file: &str,
    symbols: &crate::symbol::SymbolContext,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (symbol_id, data) in &ctx.bindings {
        let symbol_name = symbols
            .symbol_names
            .get(symbol_id)
            .cloned()
            .unwrap_or_else(|| format!("<id:{}>", symbol_id));

        let span = ctx
            .symbol_spans
            .get(symbol_id)
            .cloned()
            .unwrap_or(CodeSpan { start: 0, end: 0 });

        let alloc_strategy = determine_allocation_strategy(*symbol_id, &data.ty, ctx);

        if let AllocationStrategy::RefCounted = alloc_strategy {
            if !data.captured_by_escaping_closure {
                let mut diag = Diagnostic::new(DiagnosticMsg {
                    message: format!(
                        "Value '{}' uses reference counting, preventing in-place mutation. Consider restructuring to avoid closure capture.",
                        symbol_name
                    ),
                    span: span.clone(),
                    file: file.to_string(),
                    err_type: DiagnosticMsgType::PerformanceWarning,
                });
                diag.add_hint("Reference counted values require cloning before mutation, which is expensive for encrypted data in FHE.".to_string());
                diagnostics.push(diag);
            }
        }

        if data.shared_across_branches && data.has_multiple_uses {
            let uses: Vec<&UseSite> = ctx
                .use_sites
                .iter()
                .filter(|u| u.symbol_id == *symbol_id)
                .collect();

            if uses.len() > 3 {
                let mut diag = Diagnostic::new(DiagnosticMsg {
                    message: format!(
                        "Value '{}' is shared across branches and used {} times, preventing move optimization.",
                        symbol_name,
                        uses.len()
                    ),
                    span: span.clone(),
                    file: file.to_string(),
                    err_type: DiagnosticMsgType::PerformanceWarning,
                });
                diag.add_hint("Consider restructuring branches to enable move semantics, which avoids expensive copies in FHE.".to_string());
                diagnostics.push(diag);
            }
        }

        if data.captured_by_escaping_closure && data.has_multiple_uses {
            let use_count = ctx
                .use_sites
                .iter()
                .filter(|u| u.symbol_id == *symbol_id)
                .count();

            let mut diag = Diagnostic::new(DiagnosticMsg {
                message: format!(
                    "Value '{}' is captured by an escaping closure and used {} times, preventing move/in-place mutation.",
                    symbol_name, use_count
                ),
                span: span.clone(),
                file: file.to_string(),
                err_type: DiagnosticMsgType::PerformanceWarning,
            });
            diag.add_hint("Captured values require reference counting, which prevents in-place mutation of encrypted data.".to_string());
            diag.add_hint(
                "Consider extracting only the needed fields before creating the closure to reduce the amount of data that must be cloned.".to_string(),
            );
            diagnostics.push(diag);
        }
    }
}

fn annotate_params(
    args: &mut Vec<(Box<Node>, Option<Box<Node>>, crate::parse::CodeSpan)>,
    ctx: &AnalysisContext,
) {
    for (param_pattern, _, _) in args {
        if let Some(symbol_id) = param_pattern.symbol_id {
            let semantics = determine_parameter_semantics(symbol_id, ctx);
            param_pattern.parameter_semantics = Some(semantics);
        }
    }
}

fn annotate(node: &mut Node, ctx: &AnalysisContext) {
    if let Some(symbol_id) = node.symbol_id {
        if let NodeKind::Expr {
            expr: ExprKind::Symbol { .. },
        } = &node.kind
        {
            let node_ptr = node as *const Node as usize;
            let semantics = determine_semantics(symbol_id, node_ptr, ctx);
            node.use_semantics = Some(semantics);
        }
    }

    let alloc_for_expr = if let NodeKind::Expr { expr } = &node.kind {
        if let ExprKind::LetBinding { symbols, .. } = expr {
            if let Some(symbol_id) = symbols.symbol_id {
                Some(determine_allocation_strategy(symbol_id, &node.ty, ctx))
            } else {
                determine_expr_allocation(node, ctx)
            }
        } else {
            determine_expr_allocation(node, ctx)
        }
    } else {
        None
    };

    if node.alloc_strategy.is_none() {
        node.alloc_strategy = alloc_for_expr;
    }

    match &mut node.kind {
        NodeKind::Expr { expr } => match expr {
            ExprKind::LetBinding { expr, .. } => {
                annotate(expr, ctx);
            }
            ExprKind::FnCall { callee, args } => {
                annotate(callee, ctx);
                for (_label, arg) in args {
                    annotate(arg, ctx);
                }
            }
            ExprKind::BinaryOp { left, right, .. } => {
                annotate(left, ctx);
                annotate(right, ctx);
            }
            ExprKind::UnaryOp { expr, .. } => {
                annotate(expr, ctx);
            }
            ExprKind::Block(nodes) => {
                for child in nodes {
                    annotate(child, ctx);
                }
            }
            ExprKind::Match { expr, arms, .. } => {
                annotate(expr, ctx);
                for (_patterns, arm_expr) in arms {
                    annotate(arm_expr, ctx);
                }
            }
            ExprKind::Closure { expr: body, .. } => {
                annotate(body, ctx);
            }
            ExprKind::TupleLit(elements) | ExprKind::ListLit(elements) => {
                for elem in elements {
                    annotate(elem, ctx);
                }
            }
            ExprKind::MapLit(entries) => {
                for (key, value) in entries {
                    annotate(key, ctx);
                    annotate(value, ctx);
                }
            }
            ExprKind::FieldAccess { expr, .. } => {
                annotate(expr, ctx);
            }
            ExprKind::IndexAccess { expr, index } => {
                annotate(expr, ctx);
                annotate(index, ctx);
            }
            ExprKind::For {
                start, end, body, ..
            } => {
                annotate(start, ctx);
                annotate(end, ctx);
                annotate(body, ctx);
            }
            _ => {}
        },
        NodeKind::FnDecl {
            expr: Some(body), ..
        } => {
            annotate(body, ctx);
        }
        _ => {}
    }
}

fn determine_parameter_semantics(symbol_id: SymbolId, ctx: &AnalysisContext) -> ParameterSemantics {
    let binding_data = ctx.bindings.get(&symbol_id);

    if let Some(data) = binding_data {
        let uses_of_param: Vec<&UseSite> = ctx
            .use_sites
            .iter()
            .filter(|u| u.symbol_id == symbol_id)
            .collect();

        if uses_of_param.is_empty() {
            return ParameterSemantics::Borrowed;
        }

        let has_move = uses_of_param.iter().any(|u| {
            let is_last = u.order == uses_of_param.iter().map(|s| s.order).max().unwrap_or(0);
            is_last && !data.shared_across_branches
        });

        if has_move {
            return ParameterSemantics::Owned;
        }
    }

    ParameterSemantics::Borrowed
}

fn determine_semantics(
    symbol_id: SymbolId,
    node_ptr: usize,
    ctx: &AnalysisContext,
) -> UseSemantics {
    let binding_data = ctx.bindings.get(&symbol_id);

    if let Some(data) = binding_data {
        let uses_of_symbol: Vec<&UseSite> = ctx
            .use_sites
            .iter()
            .filter(|u| u.symbol_id == symbol_id)
            .collect();

        if uses_of_symbol.is_empty() {
            return UseSemantics::Borrow;
        }

        let max_order = uses_of_symbol.iter().map(|u| u.order).max().unwrap_or(0);

        let this_use = uses_of_symbol.iter().find(|u| u.node_ptr == node_ptr);

        if let Some(use_site) = this_use {
            let is_last_use = use_site.order == max_order;

            if use_site.context == UseContext::Escaped || use_site.context == UseContext::Returned {
                return UseSemantics::Copy;
            }

            if data.captured_by_escaping_closure {
                return UseSemantics::Copy;
            }

            if is_last_use && !data.shared_across_branches {
                return UseSemantics::Move;
            }
        }
    }

    UseSemantics::Borrow
}

fn determine_allocation_strategy(
    symbol_id: SymbolId,
    ty: &Option<Ty>,
    ctx: &AnalysisContext,
) -> AllocationStrategy {
    let binding_data = ctx.bindings.get(&symbol_id);

    //encrypted type -> cannot stack allocate (memory footprint will be bigfoot size)
    let is_encrypted = ty.as_ref().map(|t| t.requires_secret()).unwrap_or(true);

    if let Some(data) = binding_data {
        if data.captured_by_escaping_closure {
            return AllocationStrategy::RefCounted;
        }

        if data.shared_across_branches && data.escapes_function {
            return AllocationStrategy::RefCounted;
        }

        if data.escapes_function {
            return AllocationStrategy::StaticFree;
        }

        //default to function level arena
        let arena_id = data.arena_id.unwrap_or(ctx.current_arena_id);

        if data.has_multiple_uses {
            return AllocationStrategy::Arena(arena_id);
        }

        if !is_encrypted {
            return AllocationStrategy::Stack;
        } else {
            return AllocationStrategy::Arena(arena_id);
        }
    }

    AllocationStrategy::Arena(ctx.current_arena_id)
}

fn determine_expr_allocation(node: &Node, ctx: &AnalysisContext) -> Option<AllocationStrategy> {
    let is_encrypted = node
        .ty
        .as_ref()
        .map(|t| t.requires_secret())
        .unwrap_or(true);

    match &node.kind {
        NodeKind::Expr { expr } => match expr {
            ExprKind::FnCall { .. }
            | ExprKind::BinaryOp { .. }
            | ExprKind::UnaryOp { .. }
            | ExprKind::TupleLit(_)
            | ExprKind::ListLit(_)
            | ExprKind::MapLit(_) => Some(AllocationStrategy::Arena(ctx.current_arena_id)),

            //only stack allocate nocrypt
            ExprKind::IntLit(_) | ExprKind::StringLit(_) | ExprKind::RealLit { .. } => {
                if !is_encrypted {
                    Some(AllocationStrategy::Stack)
                } else {
                    Some(AllocationStrategy::Arena(ctx.current_arena_id))
                }
            }
            _ => None,
        },
        _ => None,
    }
}
