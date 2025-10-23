//maranget pattern matching

use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    symbol::SymbolId,
};
use rustc_hash::FxHashMap as HashMap;

#[derive(Debug, Clone)]
pub enum DecisionTree {
    Switch {
        arms: HashMap<SwitchKey, Box<DecisionTree>>,
        default: Box<DecisionTree>,
    },
    Leaf {
        arm_index: usize,
    },
    Fail,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum SwitchKey {
    Literal(String),
    Ctor(SymbolId),
}

#[derive(Debug, Clone)]
enum Pattern {
    Wildcard,
    Symbol(SymbolId),
    //literals are all just converted to strings for now
    Literal(String),
    //ignoring list for now - will implement as a ctor pattern later
    Rest(Option<SymbolId>),
    Ctor { id: SymbolId, args: Vec<Pattern> },
}

#[derive(Debug, Clone)]
struct PatternMatrixRow {
    patterns: Vec<Pattern>,
    arm_index: usize,
    used: bool,
}

pub fn pattern_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut pattern_ctx = PatternContext::default();
    pattern_ctx.run_pass(ctx)
}

#[derive(Default)]
struct PatternContext {
    current_file: String,
}

impl PatternContext {
    fn run_pass(&mut self, ctx: &mut CompileContext) -> Result<(), ()> {
        //plan: build matrix, find column with most choices, build switch node, recurse on each arm
        Ok(())
    }

    fn visit_node(&mut self, node: &mut Node) -> Result<(), ()> {
        use NodeKind::*;
        match &mut node.kind {
            Module { children } => {
                for child in children.iter_mut() {
                    self.visit_node(child)?;
                }
                Ok(())
            }

            FnDecl { expr, .. } if expr.is_some() => self.visit_node(expr.as_mut().unwrap()),
            Expr { .. } => self.visit_expr(node),
            _ => Ok(()),
        }
    }

    fn visit_expr(&mut self, node: &mut Node) -> Result<(), ()> {
        let expr = match &mut node.kind {
            NodeKind::Expr { expr, .. } => expr,
            _ => unreachable!(),
        };

        use ExprKind::*;
        //not expecting any ifelse nodes here, were rewritten to match node earlier
        match expr {
            Block(nodes) | ListLit(nodes) | TupleLit(nodes) => {
                for child in nodes.iter_mut() {
                    self.visit_node(child)?;
                }
            }
            MapLit(entries) => {
                for (key, value) in entries.iter_mut() {
                    self.visit_node(key)?;
                    self.visit_node(value)?;
                }
            }
            LetBinding { expr, .. }
            | FieldAccess { expr, .. }
            | UnaryOp { expr, .. }
            | For { body: expr, .. }
            | Closure { expr, .. } => {
                self.visit_node(expr)?;
            }
            BinaryOp { left, right, .. } => {
                self.visit_node(left)?;
                self.visit_node(right)?;
            }
            Match { arms, .. } => {
                let mut matrix = Vec::new();
                let mut failed = false;
                for (i, (pattern_disjunctions, arm_expr)) in arms.iter_mut().enumerate() {
                    for pattern in pattern_disjunctions.iter_mut() {
                        matrix.push(PatternMatrixRow {
                            patterns: self.lower_pattern(pattern),
                            arm_index: i,
                            used: false,
                        });
                    }

                    failed = self.visit_node(arm_expr).is_err() || failed;
                }

                //actually build decision tree, do reachability + exhaust checking

                if failed {
                    return Err(());
                }
            }
            FnCall { callee, args } => {
                self.visit_node(callee)?;
                for (_name, arg) in args.iter_mut() {
                    self.visit_node(arg)?;
                }
            }

            IndexAccess { expr, index } => {
                self.visit_node(expr)?;
                self.visit_node(index)?;
            }

            IfElse { .. } => {
                unreachable!("IfElse nodes should have been desugared to Match before pattern pass")
            }
            _ => unreachable!("Unexpected expression kind in pattern pass"),
        }
        Ok(())
    }

    fn lower_pattern(&self, node: &mut Node) -> Vec<Pattern> {
        match &mut node.kind {
            NodeKind::Expr { expr, .. } => {
                use ExprKind::*;
                match expr {
                    IntLit(val) => vec![Pattern::Literal(val.to_string())],
                    RealLit { value, .. } => vec![Pattern::Literal(value.to_string())],
                    StringLit(val) => vec![Pattern::Literal(val.clone())],
                    TupleLit(nodes) => {
                        let mut patterns = Vec::new();
                        for node in nodes.iter_mut() {
                            patterns.extend(self.lower_pattern(node));
                        }

                        patterns
                    }
                    Symbol { .. } => {
                        if let Some(symbol_id) = node.symbol_id {
                            vec![Pattern::Symbol(symbol_id)]
                        } else {
                            unreachable!("Symbol node without symbol id in pattern lowering")
                        }
                    }
                    Discard => vec![Pattern::Wildcard],
                    FnCall { callee, args } => {
                        //assumed to be a constructor - earlier passes should have verified this
                        //lets hope lol
                        if let Some(symbol_id) = callee.symbol_id {
                            let mut patterns = Vec::new();
                            for (_name, arg) in args.iter_mut() {
                                patterns.extend(self.lower_pattern(arg));
                            }

                            vec![Pattern::Ctor {
                                id: symbol_id,
                                args: patterns,
                            }]
                        } else {
                            unreachable!("Constructor call without symbol id in pattern lowering")
                        }
                    }
                    //todo: ListPattern expr kind
                    _ => unreachable!("Unexpected expression kind in pattern lowering"),
                }
            }
            _ => unreachable!("Pattern lowering expected only Expr nodes"),
        }
    }
}
