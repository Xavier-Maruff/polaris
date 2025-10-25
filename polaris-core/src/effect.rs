use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::{BOOL, FALSE, TRUE, VOID},
    parse::CodeSpan,
    symbol::SymbolId,
    types::{Ty, TyKind},
};
use rustc_hash::FxHashMap as HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Effect {
    Pure = 0,
    FheEffect = 1,
    HarnessPure = 2,
    HarnessEffect = 3,
}

impl Effect {
    fn is_harness(self) -> bool {
        matches!(self, Effect::HarnessPure | Effect::HarnessEffect)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BranchStrategy {
    Clear,
    Predicated,
    Harness,
}

#[derive(Clone)]
struct EffectCause {
    span: CodeSpan,
    message: String,
}

#[derive(Clone)]
struct EffectResult {
    effect: Effect,
    cause: Option<EffectCause>,
}

impl EffectResult {
    fn pure() -> Self {
        Self {
            effect: Effect::Pure,
            cause: None,
        }
    }

    fn new(effect: Effect, cause: Option<EffectCause>) -> Self {
        Self { effect, cause }
    }

    fn from_cause(effect: Effect, span: CodeSpan, message: impl Into<String>) -> Self {
        Self {
            effect,
            cause: Some(EffectCause {
                span,
                message: message.into(),
            }),
        }
    }

    fn absorb(&mut self, other: EffectResult) {
        if other.effect > self.effect {
            *self = other;
        } else if other.effect == self.effect && self.cause.is_none() {
            self.cause = other.cause;
        }
    }
}

fn effect_label(effect: Effect) -> &'static str {
    match effect {
        Effect::Pure => "pure",
        Effect::FheEffect => "fhe-effect",
        Effect::HarnessPure => "harness-pure",
        Effect::HarnessEffect => "harness-effect",
    }
}

#[derive(Clone, Default)]
pub struct EffectInfo {
    pub function_effects: HashMap<SymbolId, Effect>,
}

#[derive(Clone)]
struct FunctionMeta {
    host: bool,
    pure: bool,
    span: CodeSpan,
}

pub fn effect_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut effect_ctx = EffectContext::new(ctx);
    effect_ctx.collect_functions();
    effect_ctx.solve();
    Ok(())
}

struct EffectContext<'a> {
    compile_ctx: &'a mut CompileContext,
    effects: HashMap<SymbolId, Effect>,
    fn_meta: HashMap<SymbolId, FunctionMeta>,
    current_file: String,
    reported_purity_violations: HashMap<SymbolId, Effect>,
}

impl<'a> EffectContext<'a> {
    fn new(ctx: &'a mut CompileContext) -> Self {
        Self {
            compile_ctx: ctx,
            effects: HashMap::default(),
            fn_meta: HashMap::default(),
            current_file: String::new(),
            reported_purity_violations: HashMap::default(),
        }
    }

    fn collect_functions(&mut self) {
        let module_ids: Vec<String> = self
            .compile_ctx
            .dependencies
            .modules
            .keys()
            .cloned()
            .collect();
        for module_id in module_ids {
            if let Some(module) = self.compile_ctx.dependencies.modules.get(&module_id) {
                let ast_clone = module.ast.clone();
                self.collect_in_module(&ast_clone);
            }
        }
    }

    fn collect_in_module(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Module { children } => {
                for child in children {
                    self.collect_in_module(child);
                }
            }
            NodeKind::FnDecl { host, pure, .. } => {
                if let Some(id) = node.symbol_id {
                    let base = if *host {
                        if *pure {
                            Effect::HarnessPure
                        } else {
                            Effect::HarnessEffect
                        }
                    } else {
                        Effect::Pure
                    };
                    self.effects.entry(id).or_insert(base);
                    self.fn_meta.insert(
                        id,
                        FunctionMeta {
                            host: *host,
                            pure: *pure,
                            span: node.span.clone(),
                        },
                    );
                }
            }
            _ => {}
        }
    }

    fn solve(&mut self) {
        let mut changed = true;
        while changed {
            changed = false;
            let sccs = self.compile_ctx.dependencies.sccs.clone();
            for scc in sccs {
                for module_id in scc {
                    let mut cloned_ast = if let Some(module) =
                        self.compile_ctx.dependencies.modules.get(&module_id)
                    {
                        self.current_file = module.file.clone();
                        module.ast.clone()
                    } else {
                        continue;
                    };

                    if self.effect_module(&mut cloned_ast) {
                        changed = true;
                    }

                    if let Some(module) = self.compile_ctx.dependencies.modules.get_mut(&module_id)
                    {
                        module.ast = cloned_ast;
                    }
                }
            }
        }

        self.compile_ctx.effect_info.function_effects = self.effects.clone();
    }

    fn effect_module(&mut self, node: &mut Node) -> bool {
        match &mut node.kind {
            NodeKind::Module { children } => {
                let mut changed = false;
                for child in children {
                    changed |= self.effect_module(child);
                }
                node.effect = Some(Effect::Pure);
                changed
            }
            NodeKind::FnDecl {
                expr, host, pure, ..
            } => {
                let base_effect = if *host {
                    if *pure {
                        Effect::HarnessPure
                    } else {
                        Effect::HarnessEffect
                    }
                } else {
                    Effect::Pure
                };

                let mut result = EffectResult::new(base_effect, None);

                if let Some(body) = expr {
                    let body_result = self.effect_node(body);
                    result.absorb(body_result);
                }

                node.effect = Some(result.effect);

                if let Some(id) = node.symbol_id {
                    let previous = self.effects.get(&id).copied().unwrap_or(Effect::Pure);
                    let changed = if result.effect != previous {
                        self.effects.insert(id, result.effect);
                        true
                    } else {
                        false
                    };

                    if let Some(meta) = self.fn_meta.get(&id).cloned() {
                        if meta.pure {
                            let allowed = if meta.host {
                                Effect::HarnessPure
                            } else {
                                Effect::Pure
                            };
                            if result.effect > allowed {
                                if self
                                    .reported_purity_violations
                                    .get(&id)
                                    .copied()
                                    .map_or(true, |prev| prev != result.effect)
                                {
                                    self.emit_purity_error(
                                        id,
                                        meta,
                                        allowed,
                                        result.effect,
                                        result.cause.clone(),
                                    );
                                    self.reported_purity_violations.insert(id, result.effect);
                                }
                            } else {
                                self.reported_purity_violations.remove(&id);
                            }
                        }
                    }

                    return changed;
                }

                false
            }
            NodeKind::ConstDecl { expr, .. } => {
                let result = self.effect_node(expr);
                node.effect = Some(result.effect);
                false
            }
            NodeKind::Expr { .. } => {
                let result = self.effect_expr(node);
                node.effect = Some(result.effect);
                false
            }
            _ => {
                node.effect = Some(Effect::Pure);
                false
            }
        }
    }

    fn emit_purity_error(
        &mut self,
        id: SymbolId,
        meta: FunctionMeta,
        allowed: Effect,
        effect: Effect,
        cause: Option<EffectCause>,
    ) {
        let symbols = &self.compile_ctx.symbols;
        let name = symbols
            .symbol_names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| "<unknown>".into());

        let expected_label = effect_label(allowed);
        let observed_label = effect_label(effect);

        let message = if meta.host {
            format!(
                "Harness function '{}' is declared 'pure' but performs '{}' operations.",
                name, observed_label
            )
        } else {
            format!(
                "Function '{}' is declared 'pure' but performs '{}' operations.",
                name, observed_label
            )
        };

        let mut diag = Diagnostic::new(DiagnosticMsg {
            message,
            span: meta.span.clone(),
            file: self.current_file.clone(),
            err_type: DiagnosticMsgType::InvalidFunctionCall,
        });
        diag.add_hint(format!(
            "Expected at most '{}' behaviour here.",
            expected_label
        ));
        if let Some(cause) = cause {
            diag.add_note(DiagnosticMsg {
                message: cause.message,
                span: cause.span,
                file: self.current_file.clone(),
                err_type: DiagnosticMsgType::InvalidFunctionCall,
            });
        }
        self.compile_ctx.errors.push(diag);
    }

    fn effect_node(&mut self, node: &mut Node) -> EffectResult {
        let result = match &mut node.kind {
            NodeKind::Expr { .. } => self.effect_expr(node),
            NodeKind::ConstDecl { expr, .. } => self.effect_node(expr),
            _ => EffectResult::pure(),
        };

        node.effect = Some(result.effect);
        result
    }

    fn effect_expr(&mut self, node: &mut Node) -> EffectResult {
        let expr = match &mut node.kind {
            NodeKind::Expr { expr } => expr,
            _ => return EffectResult::pure(),
        };

        use ExprKind::*;

        node.branch_strategy = None;

        match expr {
            IntLit(_) | StringLit(_) | ListPattern(_) => EffectResult::pure(),
            RealLit { .. } => EffectResult::pure(),
            ListLit(elements) => {
                let mut res = EffectResult::pure();
                for elem in elements {
                    let elem_res = self.effect_node(elem);
                    res.absorb(elem_res);
                }
                res
            }
            TupleLit(elements) => {
                let mut res = EffectResult::pure();
                for elem in elements {
                    let elem_res = self.effect_node(elem);
                    res.absorb(elem_res);
                }
                res
            }
            MapLit(entries) => {
                let mut res = EffectResult::pure();
                for (k, v) in entries {
                    let key_res = self.effect_node(k);
                    let val_res = self.effect_node(v);
                    res.absorb(key_res);
                    res.absorb(val_res);
                }
                res
            }
            Symbol { .. } => {
                let effect = node
                    .symbol_id
                    .and_then(|id| self.effects.get(&id).copied())
                    .unwrap_or(Effect::Pure);
                EffectResult::new(effect, None)
            }
            FieldAccess { expr, .. } => self.effect_node(expr),
            UnaryOp { expr, .. } => self.effect_node(expr),
            BinaryOp { left, right, .. } => {
                let mut res = self.effect_node(left);
                let right_res = self.effect_node(right);
                res.absorb(right_res);
                res
            }
            FnCall { callee, args } => {
                let mut res = self.effect_node(callee);
                for (_, arg) in args {
                    let arg_res = self.effect_node(arg);
                    res.absorb(arg_res);
                }

                if let Some(sym) = callee.symbol_id {
                    if let Some(&callee_eff) = self.effects.get(&sym) {
                        if callee_eff != Effect::Pure {
                            let name = self
                                .compile_ctx
                                .symbols
                                .symbol_names
                                .get(&sym)
                                .cloned()
                                .unwrap_or_else(|| "<unknown>".into());
                            let message = match callee_eff {
                                Effect::HarnessEffect => {
                                    format!("calls harness function '{}'", name)
                                }
                                Effect::HarnessPure => {
                                    format!("calls harness-pure function '{}'", name)
                                }
                                Effect::FheEffect => {
                                    format!("calls effectful runtime function '{}'", name)
                                }
                                Effect::Pure => String::new(),
                            };
                            let cause =
                                EffectResult::from_cause(callee_eff, callee.span.clone(), message);
                            res.absorb(cause);
                        }
                    }
                }

                res
            }
            LetBinding { expr, .. } => self.effect_node(expr),
            Block(statements) => {
                let mut res = EffectResult::pure();
                for stmt in statements {
                    let stmt_res = self.effect_node(stmt);
                    res.absorb(stmt_res);
                }
                res
            }
            Closure { expr, .. } => self.effect_node(expr),
            For {
                start, end, body, ..
            } => {
                let mut res = self.effect_node(start);
                let end_res = self.effect_node(end);
                let body_res = self.effect_node(body);
                res.absorb(end_res);
                res.absorb(body_res);
                res
            }
            IndexAccess { expr, index } => {
                let mut res = self.effect_node(expr);
                let index_res = self.effect_node(index);
                res.absorb(index_res);
                res
            }
            //really don't want to clone here but that is a problem for future me
            IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                // if condition { then_branch } else { else_branch }
                // ->
                // match condition { True => then_branch, False => else_branch }

                let else_node = if let Some(else_branch) = else_branch {
                    else_branch.clone()
                } else {
                    //no else branch, must be unit
                    Box::new(
                        Node::new(
                            NodeKind::Expr {
                                expr: ExprKind::Symbol { name: VOID.into() },
                            },
                            condition.span.clone(),
                        )
                        .with_symbol_id(self.compile_ctx.symbols.intrinsic_symbols[VOID])
                        .with_type(Ty::new(TyKind::Concrete(
                            self.compile_ctx.symbols.intrinsic_types[VOID],
                        ))),
                    )
                };

                let true_id = self.compile_ctx.symbols.intrinsic_symbols[TRUE];
                let false_id = self.compile_ctx.symbols.intrinsic_symbols[FALSE];
                let bool_ty = Ty::new(TyKind::Concrete(
                    self.compile_ctx.symbols.intrinsic_types[BOOL],
                ));

                let true_pat = Node::new(
                    NodeKind::Expr {
                        expr: ExprKind::Symbol { name: TRUE.into() },
                    },
                    condition.span.clone(),
                )
                .with_symbol_id(true_id)
                .with_type(bool_ty.clone());

                let false_pat = Node::new(
                    NodeKind::Expr {
                        expr: ExprKind::Symbol { name: FALSE.into() },
                    },
                    condition.span.clone(),
                )
                .with_symbol_id(false_id)
                .with_type(bool_ty);

                let arms = vec![
                    (vec![true_pat], *then_branch.clone()),
                    (vec![false_pat], *else_node),
                ];

                *expr = Match {
                    expr: condition.clone(),
                    arms,
                };

                //just run on the new match expr
                return self.effect_expr(node);
            }
            Match { expr, arms } => {
                let cond_res = self.effect_node(expr);
                let mut res = cond_res.clone();
                let cond_secret = expr
                    .ty
                    .as_ref()
                    .map(|ty| ty.requires_secret())
                    .unwrap_or(false);
                let mut harness_branch = false;

                for (_, arm_expr) in arms {
                    let arm_res = self.effect_node(arm_expr);
                    harness_branch |= arm_res.effect.is_harness();
                    res.absorb(arm_res);
                }

                let strategy = if !cond_secret {
                    BranchStrategy::Clear
                } else if harness_branch {
                    BranchStrategy::Harness
                } else {
                    BranchStrategy::Predicated
                };
                node.branch_strategy = Some(strategy);

                if cond_secret && harness_branch {
                    let cause = EffectResult::from_cause(
                        Effect::HarnessEffect,
                        expr.span.clone(),
                        "secret match scrutinee selects harness branch",
                    );
                    res.absorb(cause);
                }

                res
            }
            Discard => EffectResult::pure(),
        }
    }
}
