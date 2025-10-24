//maranget pattern matching

use std::{collections::HashSet, fmt::Write};

use crate::{
    ast::{ExprKind, ListPatternElement, Node, NodeKind},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    parse::CodeSpan,
    symbol::SymbolId,
};
use rustc_hash::FxHashMap as HashMap;

#[derive(Debug, Clone)]
pub enum DecisionTree {
    Switch {
        occurrence: Occurrence,
        arms: HashMap<SwitchKey, Box<DecisionTree>>,
        default: Box<DecisionTree>,
        covers_all: bool,
        missing_keys: Vec<SwitchKey>,
        span: Option<CodeSpan>,
    },
    Leaf {
        arm_index: usize,
    },
    Fail,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SwitchKey {
    Literal(String),
    Ctor(SymbolId),
    ListNil,
    ListCons,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OccurrenceStep {
    TupleField(usize),
    CtorField(SymbolId, usize),
    ListHead,
    ListTail,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Occurrence {
    steps: Vec<OccurrenceStep>,
}

impl Occurrence {
    pub fn root() -> Self {
        Self { steps: Vec::new() }
    }

    pub fn steps(&self) -> &[OccurrenceStep] {
        &self.steps
    }

    fn tuple_field(&self, index: usize) -> Self {
        let mut steps = self.steps.clone();
        steps.push(OccurrenceStep::TupleField(index));
        Self { steps }
    }

    fn ctor_field(&self, ctor: SymbolId, index: usize) -> Self {
        let mut steps = self.steps.clone();
        steps.push(OccurrenceStep::CtorField(ctor, index));
        Self { steps }
    }

    fn list_head(&self) -> Self {
        let mut steps = self.steps.clone();
        steps.push(OccurrenceStep::ListHead);
        Self { steps }
    }

    fn list_tail(&self) -> Self {
        let mut steps = self.steps.clone();
        steps.push(OccurrenceStep::ListTail);
        Self { steps }
    }

    fn render(&self) -> String {
        self.render_with_names(&HashMap::default())
    }

    fn render_with_names(&self, symbol_names: &HashMap<SymbolId, String>) -> String {
        if self.steps.is_empty() {
            return "the value".to_string();
        }

        let mut out = String::new();
        let mut is_first = true;

        for step in &self.steps {
            match step {
                OccurrenceStep::TupleField(idx) => {
                    if is_first {
                        write!(&mut out, "tuple field {}", idx).unwrap();
                        is_first = false;
                    } else {
                        write!(&mut out, ".{}", idx).unwrap();
                    }
                }
                OccurrenceStep::CtorField(ctor, idx) => {
                    if is_first {
                        let name = symbol_names
                            .get(ctor)
                            .map(|s| s.as_str())
                            .unwrap_or("constructor");
                        write!(&mut out, "{} field {}", name, idx).unwrap();
                        is_first = false;
                    } else {
                        write!(&mut out, "[{}]", idx).unwrap();
                    }
                }
                OccurrenceStep::ListHead => {
                    if is_first {
                        out.push_str("list head");
                        is_first = false;
                    } else {
                        out.push_str(".head");
                    }
                }
                OccurrenceStep::ListTail => {
                    if is_first {
                        out.push_str("list tail");
                        is_first = false;
                    } else {
                        out.push_str(".tail");
                    }
                }
            }
        }
        out
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ListRest {
    Absent,
    Unbound,
    Bound(SymbolId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Pattern {
    Wildcard,
    Symbol(SymbolId),
    Literal(String),
    Tuple(Vec<Pattern>),
    List {
        prefix: Vec<Pattern>,
        rest: ListRest,
    },
    Ctor {
        id: SymbolId,
        args: Vec<Pattern>,
    },
}

#[derive(Debug, Clone)]
struct PatternMatrixRow {
    patterns: Vec<Pattern>,
    arm_index: usize,
    row_id: usize,
}

#[derive(Debug, Clone)]
struct PatternRowMeta {
    span: CodeSpan,
    patterns: Vec<Pattern>,
}

struct ColumnConstructors {
    keys: Vec<SwitchKey>,
    covers_all: bool,
    missing: Vec<SwitchKey>,
    span: Option<CodeSpan>,
}

pub fn pattern_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut pattern_ctx = PatternContext::default();
    pattern_ctx.run_pass(ctx)
}

#[derive(Default)]
struct PatternContext {
    current_file: String,
    warnings: Vec<Diagnostic>,
    errors: Vec<Diagnostic>,
    ctor_parents: HashMap<SymbolId, SymbolId>,
    type_constructors: HashMap<SymbolId, Vec<SymbolId>>,
    symbol_names: HashMap<SymbolId, String>,
}

impl PatternContext {
    fn run_pass(&mut self, ctx: &mut CompileContext) -> Result<(), ()> {
        self.ctor_parents = ctx.symbols.type_constructors.clone();
        self.type_constructors.clear();
        for (ctor, ty) in self.ctor_parents.iter() {
            self.type_constructors.entry(*ty).or_default().push(*ctor);
        }
        self.symbol_names = ctx.symbols.symbol_names.clone();

        for (_module_id, module) in ctx.dependencies.modules.iter_mut() {
            self.current_file = module.file.clone();
            self.visit_node(&mut module.ast)?;
        }

        ctx.warnings.extend(self.warnings.drain(..));
        ctx.errors.extend(self.errors.drain(..));
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
            Match {
                expr: scrutinee,
                arms,
                ..
            } => {
                self.visit_node(scrutinee)?;
                let mut matrix = Vec::new();
                let mut row_meta = Vec::new();
                let mut failed = false;
                for (i, (pattern_disjunctions, arm_expr)) in arms.iter_mut().enumerate() {
                    for pattern in pattern_disjunctions.iter_mut() {
                        let lowered = vec![self.lower_pattern(pattern)];
                        let row_id = row_meta.len();
                        row_meta.push(PatternRowMeta {
                            span: pattern.span.clone(),
                            patterns: lowered.clone(),
                        });
                        matrix.push(PatternMatrixRow {
                            patterns: lowered,
                            arm_index: i,
                            row_id,
                        });
                    }

                    failed = self.visit_node(arm_expr).is_err() || failed;
                }

                if failed || matrix.is_empty() {
                    return if failed { Err(()) } else { Ok(()) };
                }

                let mut usage = vec![false; row_meta.len()];
                let decision_tree = self.compile_pattern(
                    &mut matrix,
                    vec![Occurrence::root()],
                    &mut usage,
                    &row_meta,
                );
                let exhaustive = decision_tree.is_exhaustive();

                for (row_id, used) in usage.iter().enumerate() {
                    if !used {
                        let meta = &row_meta[row_id];
                        let mut diag = Diagnostic::new(DiagnosticMsg {
                            message: "Pattern is unreachable".to_string(),
                            span: meta.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::UnreachablePattern,
                        });

                        if let Some(dom_id) = self.find_dominator(row_id, &row_meta) {
                            let dom_meta = &row_meta[dom_id];
                            diag.add_note(DiagnosticMsg {
                                message: "Previously matched by this pattern".to_string(),
                                span: dom_meta.span.clone(),
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::UnreachablePattern,
                            });
                        }

                        self.warnings.push(diag);
                    }
                }

                if !exhaustive {
                    let mut diag = Diagnostic::new(DiagnosticMsg {
                        message: "Match expression is not exhaustive".to_string(),
                        span: node.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::NonExhaustivePatterns,
                    });

                    let missing_entries = decision_tree.missing_entries();
                    use std::collections::HashMap;
                    let mut grouped: HashMap<(Occurrence, Option<CodeSpan>), Vec<SwitchKey>> =
                        HashMap::new();
                    let mut literal_seen = false;
                    let mut has_uncovered_root = false;

                    for entry in missing_entries.into_iter() {
                        if entry.constraints.is_empty() {
                            has_uncovered_root = true;
                            continue;
                        }

                        let (occurrence, key, span) = &entry.constraints[0];
                        if matches!(key, SwitchKey::Literal(_)) {
                            literal_seen = true;
                        }

                        grouped
                            .entry((occurrence.clone(), span.clone()))
                            .or_insert_with(Vec::new)
                            .push(key.clone());
                    }

                    if has_uncovered_root {
                        diag.add_note(DiagnosticMsg {
                            message: "Not all possible values are covered".to_string(),
                            span: node.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::NonExhaustivePatterns,
                        });
                    }

                    for ((occurrence, span_opt), keys) in grouped.into_iter() {
                        if keys.is_empty() {
                            continue;
                        }

                        let span = span_opt.unwrap_or(node.span.clone());
                        let mut literal_values = Vec::new();
                        let mut ctor_names = Vec::new();
                        let mut list_cases = Vec::new();

                        for key in keys.iter() {
                            match key {
                                SwitchKey::Literal(lit) => literal_values.push(lit.clone()),
                                SwitchKey::Ctor(id) => ctor_names.push(
                                    self.symbol_names
                                        .get(id)
                                        .cloned()
                                        .unwrap_or_else(|| format!("#{}", id)),
                                ),
                                SwitchKey::ListNil => list_cases.push("[]".to_string()),
                                SwitchKey::ListCons => {
                                    list_cases.push("[head, ...tail]".to_string())
                                }
                            }
                        }

                        if !literal_values.is_empty() {
                            literal_seen = true;
                        }

                        let mut parts = Vec::new();
                        if !literal_values.is_empty() {
                            parts.push(format!("literals {}", literal_values.join(", ")));
                        }
                        if !ctor_names.is_empty() {
                            parts.push(format!("constructors {}", ctor_names.join(", ")));
                        }
                        if !list_cases.is_empty() {
                            parts.push(format!("list forms {}", list_cases.join(", ")));
                        }

                        let occurrence_desc = occurrence.render_with_names(&self.symbol_names);
                        let message = if parts.is_empty() {
                            format!("{} has uncovered cases", occurrence_desc)
                        } else {
                            format!("{} is missing {}", occurrence_desc, parts.join("; "))
                        };

                        diag.add_note(DiagnosticMsg {
                            message,
                            span,
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::NonExhaustivePatterns,
                        });
                    }

                    if literal_seen || has_uncovered_root {
                        diag.add_hint("Consider adding a wildcard pattern `_`".to_string());
                    }

                    self.errors.push(diag);
                }

                node.decision_tree = Some(decision_tree);
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
            _ => (),
        }
        Ok(())
    }

    fn compile_pattern(
        &self,
        matrix: &mut Vec<PatternMatrixRow>,
        occurrences: Vec<Occurrence>,
        usage: &mut Vec<bool>,
        row_meta: &[PatternRowMeta],
    ) -> DecisionTree {
        if matrix.is_empty() {
            return DecisionTree::Fail;
        }

        if matrix[0].patterns.is_empty() {
            usage[matrix[0].row_id] = true;
            return DecisionTree::Leaf {
                arm_index: matrix[0].arm_index,
            };
        }

        let col = match self.next_column(matrix) {
            Some(c) => c,
            None => return DecisionTree::Fail,
        };

        if self.column_contains_tuple(matrix, col) {
            let (mut expanded_matrix, expanded_occurrences) =
                self.expand_tuple_column(matrix, &occurrences, col);
            return self.compile_pattern(
                &mut expanded_matrix,
                expanded_occurrences,
                usage,
                row_meta,
            );
        }

        let ColumnConstructors {
            keys: constructors,
            covers_all,
            missing,
            span,
        } = self.get_constructors_in_column(matrix, col, row_meta);

        if constructors.is_empty() {
            let (mut default_matrix, default_occurrences) =
                self.default_specialise(matrix, &occurrences, col);
            return self.compile_pattern(&mut default_matrix, default_occurrences, usage, row_meta);
        }

        let occurrence = occurrences[col].clone();
        let (mut default_matrix, default_occurrences) =
            self.default_specialise(matrix, &occurrences, col);
        let default_subtree =
            self.compile_pattern(&mut default_matrix, default_occurrences, usage, row_meta);

        let mut branches = Vec::new();
        for ctor in constructors.iter() {
            let (mut specialised_matrix, specialised_occurrences) =
                self.specialise(matrix, &occurrences, col, ctor);
            let subtree = self.compile_pattern(
                &mut specialised_matrix,
                specialised_occurrences,
                usage,
                row_meta,
            );
            branches.push((ctor.clone(), subtree));
        }

        DecisionTree::Switch {
            occurrence,
            arms: branches
                .into_iter()
                .map(|(key, subtree)| (key, Box::new(subtree)))
                .collect(),
            default: Box::new(default_subtree),
            covers_all,
            missing_keys: missing,
            span,
        }
    }

    fn specialise(
        &self,
        matrix: &Vec<PatternMatrixRow>,
        occurrences: &Vec<Occurrence>,
        col: usize,
        key: &SwitchKey,
    ) -> (Vec<PatternMatrixRow>, Vec<Occurrence>) {
        let mut specialised = Vec::new();
        let parent_occurrence = occurrences[col].clone();
        let mut new_occurrences = occurrences.clone();
        new_occurrences.remove(col);

        match key {
            SwitchKey::Ctor(ctor_id) => {
                let arity = self.get_constructor_arity(matrix, col, ctor_id);
                for i in 0..arity {
                    new_occurrences.insert(col + i, parent_occurrence.ctor_field(*ctor_id, i));
                }

                for row in matrix.iter() {
                    match &row.patterns[col] {
                        Pattern::Ctor { id, args } if id == ctor_id => {
                            let mut new_patterns = row.patterns.clone();
                            new_patterns.remove(col);
                            for (i, arg_pattern) in args.iter().enumerate() {
                                new_patterns.insert(col + i, arg_pattern.clone());
                            }
                            specialised.push(PatternMatrixRow {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                                row_id: row.row_id,
                            });
                        }
                        Pattern::Wildcard | Pattern::Symbol(_) => {
                            let mut new_patterns = row.patterns.clone();
                            new_patterns.remove(col);
                            for i in 0..arity {
                                new_patterns.insert(col + i, Pattern::Wildcard);
                            }
                            specialised.push(PatternMatrixRow {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                                row_id: row.row_id,
                            });
                        }
                        _ => {}
                    }
                }
            }
            SwitchKey::Literal(key_lit) => {
                for row in matrix.iter() {
                    match &row.patterns[col] {
                        Pattern::Literal(lit) if lit == key_lit => {
                            let mut new_patterns = row.patterns.clone();
                            new_patterns.remove(col);
                            specialised.push(PatternMatrixRow {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                                row_id: row.row_id,
                            });
                        }
                        Pattern::Wildcard | Pattern::Symbol(_) => {
                            let mut new_patterns = row.patterns.clone();
                            new_patterns.remove(col);
                            specialised.push(PatternMatrixRow {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                                row_id: row.row_id,
                            });
                        }
                        _ => {}
                    }
                }
            }
            SwitchKey::ListCons => {
                new_occurrences.insert(col, parent_occurrence.list_head());
                new_occurrences.insert(col + 1, parent_occurrence.list_tail());

                for row in matrix.iter() {
                    match &row.patterns[col] {
                        Pattern::List { prefix, rest } => {
                            if prefix.is_empty() {
                                if matches!(rest, ListRest::Absent) {
                                    continue;
                                }

                                let mut new_patterns = row.patterns.clone();
                                new_patterns.remove(col);
                                new_patterns.insert(col, Pattern::Wildcard);
                                let tail_pattern = match rest {
                                    ListRest::Bound(symbol_id) => Pattern::Symbol(*symbol_id),
                                    _ => Pattern::Wildcard,
                                };
                                new_patterns.insert(col + 1, tail_pattern);
                                specialised.push(PatternMatrixRow {
                                    patterns: new_patterns,
                                    arm_index: row.arm_index,
                                    row_id: row.row_id,
                                });
                            } else {
                                let mut new_patterns = row.patterns.clone();
                                new_patterns.remove(col);

                                let mut remaining_prefix = prefix.clone();
                                let head_pattern = remaining_prefix.remove(0);
                                let tail_pattern = if !remaining_prefix.is_empty() {
                                    Pattern::List {
                                        prefix: remaining_prefix,
                                        rest: rest.clone(),
                                    }
                                } else {
                                    match rest {
                                        ListRest::Absent => Pattern::List {
                                            prefix: Vec::new(),
                                            rest: ListRest::Absent,
                                        },
                                        ListRest::Unbound => Pattern::Wildcard,
                                        ListRest::Bound(symbol_id) => Pattern::Symbol(*symbol_id),
                                    }
                                };

                                new_patterns.insert(col, head_pattern);
                                new_patterns.insert(col + 1, tail_pattern);
                                specialised.push(PatternMatrixRow {
                                    patterns: new_patterns,
                                    arm_index: row.arm_index,
                                    row_id: row.row_id,
                                });
                            }
                        }
                        Pattern::Wildcard | Pattern::Symbol(_) => {
                            let mut new_patterns = row.patterns.clone();
                            new_patterns.remove(col);
                            new_patterns.insert(col, Pattern::Wildcard);
                            new_patterns.insert(col + 1, Pattern::Wildcard);
                            specialised.push(PatternMatrixRow {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                                row_id: row.row_id,
                            });
                        }
                        _ => {}
                    }
                }
            }
            SwitchKey::ListNil => {
                for row in matrix.iter() {
                    match &row.patterns[col] {
                        Pattern::List { prefix, .. } if prefix.is_empty() => {
                            let mut new_patterns = row.patterns.clone();
                            new_patterns.remove(col);
                            specialised.push(PatternMatrixRow {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                                row_id: row.row_id,
                            });
                        }
                        Pattern::Wildcard | Pattern::Symbol(_) => {
                            let mut new_patterns = row.patterns.clone();
                            new_patterns.remove(col);
                            specialised.push(PatternMatrixRow {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                                row_id: row.row_id,
                            });
                        }
                        _ => {}
                    }
                }
            }
        }

        (specialised, new_occurrences)
    }

    fn get_constructor_arity(
        &self,
        matrix: &Vec<PatternMatrixRow>,
        col: usize,
        ctor: &SymbolId,
    ) -> usize {
        for row in matrix.iter() {
            if let Pattern::Ctor { id, args } = &row.patterns[col] {
                if id == ctor {
                    return args.len();
                }
            }
        }
        0
    }

    fn default_specialise(
        &self,
        matrix: &Vec<PatternMatrixRow>,
        occurrences: &Vec<Occurrence>,
        col: usize,
    ) -> (Vec<PatternMatrixRow>, Vec<Occurrence>) {
        let mut specialised = Vec::new();

        for row in matrix.iter() {
            match &row.patterns[col] {
                Pattern::Wildcard | Pattern::Symbol(_) => {
                    let mut new_patterns = row.patterns.clone();
                    new_patterns.remove(col);

                    specialised.push(PatternMatrixRow {
                        patterns: new_patterns,
                        arm_index: row.arm_index,
                        row_id: row.row_id,
                    });
                }
                _ => {}
            }
        }

        let mut new_occurrences = occurrences.clone();
        new_occurrences.remove(col);

        (specialised, new_occurrences)
    }

    fn get_constructors_in_column(
        &self,
        matrix: &Vec<PatternMatrixRow>,
        col: usize,
        row_meta: &[PatternRowMeta],
    ) -> ColumnConstructors {
        use std::collections::HashSet;

        let mut ctor_ids: HashSet<SymbolId> = HashSet::new();
        let mut literals: HashSet<String> = HashSet::new();
        let mut has_list_nil = false;
        let mut has_list_cons = false;
        let mut saw_list_pattern = false;
        let mut has_wildcard = false;
        let mut type_symbol: Option<SymbolId> = None;

        for row in matrix.iter() {
            match &row.patterns[col] {
                Pattern::Ctor { id, .. } => {
                    ctor_ids.insert(*id);
                    if let Some(parent) = self.ctor_parents.get(id) {
                        type_symbol = Some(*parent);
                    }
                }
                Pattern::Literal(lit) => {
                    literals.insert(lit.clone());
                }
                Pattern::List { prefix, rest } => {
                    saw_list_pattern = true;
                    if prefix.is_empty() {
                        if matches!(rest, ListRest::Absent) {
                            has_list_nil = true;
                        }
                    } else {
                        has_list_cons = true;
                    }
                }
                Pattern::Wildcard | Pattern::Symbol(_) => {
                    has_wildcard = true;
                }
                Pattern::Tuple(_) => {}
            }
        }

        let mut keys: Vec<SwitchKey> = ctor_ids.iter().map(|id| SwitchKey::Ctor(*id)).collect();
        keys.extend(literals.iter().cloned().map(SwitchKey::Literal));
        if has_list_cons {
            keys.push(SwitchKey::ListCons);
        }
        if has_list_nil {
            keys.push(SwitchKey::ListNil);
        }

        let mut missing = Vec::new();

        if !has_wildcard {
            if saw_list_pattern {
                if !has_list_nil {
                    missing.push(SwitchKey::ListNil);
                }
                if !has_list_cons {
                    missing.push(SwitchKey::ListCons);
                }
            }

            if let Some(type_symbol) = type_symbol {
                if let Some(all_ctors) = self.type_constructors.get(&type_symbol) {
                    for ctor in all_ctors {
                        if !ctor_ids.contains(ctor) {
                            missing.push(SwitchKey::Ctor(*ctor));
                        }
                    }
                }
            }
        }

        //literal can only be covered by wildcard
        let covers_all = if !literals.is_empty() {
            has_wildcard
        } else {
            has_wildcard || missing.is_empty()
        };

        let span = matrix.iter().find_map(|row| match &row.patterns[col] {
            Pattern::Wildcard | Pattern::Symbol(_) => None,
            _ => Some(row_meta[row.row_id].span.clone()),
        });

        ColumnConstructors {
            keys,
            covers_all,
            missing,
            span,
        }
    }

    fn next_column(&self, matrix: &Vec<PatternMatrixRow>) -> Option<usize> {
        if matrix.is_empty() {
            return None;
        }

        let num_columns = matrix[0].patterns.len();
        let mut best_column = None;
        let mut best_score = i32::MIN;
        for col in 0..num_columns {
            let mut distinct_ctors = HashSet::new();
            let mut distinct_literals = HashSet::new();
            let mut num_wildcards = 0;
            let mut has_list_nil = false;
            let mut has_list_cons = false;

            for row in matrix.iter() {
                match &row.patterns[col] {
                    Pattern::Ctor { id, .. } => {
                        distinct_ctors.insert(id);
                    }
                    Pattern::Literal(lit) => {
                        distinct_literals.insert(lit);
                    }
                    Pattern::Wildcard | Pattern::Symbol(_) => {
                        num_wildcards += 1;
                    }
                    Pattern::List { prefix, rest } => {
                        if prefix.is_empty() {
                            if matches!(rest, ListRest::Absent) {
                                has_list_nil = true;
                            } else {
                                num_wildcards += 1;
                            }
                        } else {
                            has_list_cons = true;
                        }
                    }
                    Pattern::Tuple(_) => {}
                }
            }
            let constructors = distinct_ctors.len()
                + distinct_literals.len()
                + usize::from(has_list_nil)
                + usize::from(has_list_cons);
            let score = constructors as i32 - num_wildcards as i32;
            if score > best_score {
                best_score = score;
                best_column = Some(col);
            }
        }

        best_column
    }

    fn column_contains_tuple(&self, matrix: &Vec<PatternMatrixRow>, col: usize) -> bool {
        matrix
            .iter()
            .any(|row| matches!(row.patterns[col], Pattern::Tuple(_)))
    }

    fn expand_tuple_column(
        &self,
        matrix: &Vec<PatternMatrixRow>,
        occurrences: &Vec<Occurrence>,
        col: usize,
    ) -> (Vec<PatternMatrixRow>, Vec<Occurrence>) {
        let arity = self
            .get_tuple_arity(matrix, col)
            .expect("expand_tuple_column called without tuple patterns");
        let parent_occurrence = occurrences[col].clone();

        let mut new_occurrences = occurrences.clone();
        new_occurrences.remove(col);
        for i in 0..arity {
            new_occurrences.insert(col + i, parent_occurrence.tuple_field(i));
        }

        let mut expanded = Vec::new();
        for row in matrix.iter() {
            let mut new_patterns = row.patterns.clone();
            match &row.patterns[col] {
                Pattern::Tuple(elements) => {
                    new_patterns.remove(col);
                    for (i, element) in elements.iter().enumerate() {
                        new_patterns.insert(col + i, element.clone());
                    }
                }
                Pattern::Wildcard | Pattern::Symbol(_) => {
                    new_patterns.remove(col);
                    for i in 0..arity {
                        new_patterns.insert(col + i, Pattern::Wildcard);
                    }
                }
                _ => {
                    new_patterns.remove(col);
                    for i in 0..arity {
                        new_patterns.insert(col + i, Pattern::Wildcard);
                    }
                }
            }

            expanded.push(PatternMatrixRow {
                patterns: new_patterns,
                arm_index: row.arm_index,
                row_id: row.row_id,
            });
        }

        (expanded, new_occurrences)
    }

    fn get_tuple_arity(&self, matrix: &Vec<PatternMatrixRow>, col: usize) -> Option<usize> {
        for row in matrix.iter() {
            if let Pattern::Tuple(elements) = &row.patterns[col] {
                return Some(elements.len());
            }
        }
        None
    }

    fn lower_pattern(&self, node: &mut Node) -> Pattern {
        match &mut node.kind {
            NodeKind::Expr { expr, .. } => {
                use ExprKind::*;
                match expr {
                    IntLit(val) => Pattern::Literal(val.to_string()),
                    RealLit { value, .. } => Pattern::Literal(value.to_string()),
                    StringLit(val) => Pattern::Literal(val.clone()),
                    TupleLit(nodes) => {
                        let elements = nodes
                            .iter_mut()
                            .map(|node| self.lower_pattern(node))
                            .collect();
                        Pattern::Tuple(elements)
                    }
                    Symbol { name, .. } => {
                        let symbol_id = node.symbol_id.expect("symbol without id");
                        match name.chars().next() {
                            Some(c) if c.is_uppercase() => Pattern::Ctor {
                                id: symbol_id,
                                args: Vec::new(),
                            },
                            _ => Pattern::Symbol(symbol_id),
                        }
                    }
                    Discard => Pattern::Wildcard,
                    FnCall { callee, args } => {
                        if let Some(symbol_id) = callee.symbol_id {
                            let patterns = args
                                .iter_mut()
                                .map(|(_name, arg)| self.lower_pattern(arg))
                                .collect();

                            Pattern::Ctor {
                                id: symbol_id,
                                args: patterns,
                            }
                        } else {
                            unreachable!("Constructor call without symbol id in pattern lowering")
                        }
                    }
                    ListPattern(elements) => {
                        let mut prefix = Vec::new();
                        let mut rest = ListRest::Absent;
                        let mut seen_rest = false;

                        for element in elements.iter_mut() {
                            match element {
                                ListPatternElement::Element(node) => {
                                    if seen_rest {
                                        unreachable!("Rest pattern must be last element");
                                    }
                                    prefix.push(self.lower_pattern(node));
                                }
                                ListPatternElement::Rest(rest_node) => {
                                    if seen_rest {
                                        unreachable!("Multiple rest patterns in list pattern");
                                    }
                                    seen_rest = true;
                                    match rest_node {
                                        Some(node) => {
                                            let rest_pattern = self.lower_pattern(node);
                                            rest = match rest_pattern {
                                                Pattern::Symbol(symbol_id) => {
                                                    ListRest::Bound(symbol_id)
                                                }
                                                Pattern::Wildcard => ListRest::Unbound,
                                                _ => unreachable!(
                                                    "Rest pattern must be a symbol or discard"
                                                ),
                                            };
                                        }
                                        None => rest = ListRest::Unbound,
                                    }
                                }
                            }
                        }

                        Pattern::List { prefix, rest }
                    }
                    ListLit(_) => Pattern::Wildcard,
                    _ => {
                        unreachable!("Unexpected expression kind in pattern lowering: {:?}", expr)
                    }
                }
            }
            _ => unreachable!("Pattern lowering expected only Expr nodes"),
        }
    }

    fn find_dominator(&self, row_id: usize, rows: &[PatternRowMeta]) -> Option<usize> {
        for idx in (0..row_id).rev() {
            if self.patterns_subsume(&rows[idx].patterns, &rows[row_id].patterns) {
                return Some(idx);
            }
        }
        None
    }

    fn patterns_subsume(&self, a: &[Pattern], b: &[Pattern]) -> bool {
        if a.len() != b.len() {
            return false;
        }

        a.iter()
            .zip(b.iter())
            .all(|(pa, pb)| self.pattern_subsumes(pa, pb))
    }

    fn pattern_subsumes(&self, a: &Pattern, b: &Pattern) -> bool {
        match (a, b) {
            (Pattern::Wildcard, _) => true,
            (Pattern::Symbol(_), _) => true,
            (Pattern::Literal(a_lit), Pattern::Literal(b_lit)) => a_lit == b_lit,
            (Pattern::Literal(_), _) => false,
            (Pattern::Tuple(a_elems), Pattern::Tuple(b_elems)) => {
                a_elems.len() == b_elems.len()
                    && a_elems
                        .iter()
                        .zip(b_elems.iter())
                        .all(|(pa, pb)| self.pattern_subsumes(pa, pb))
            }
            (Pattern::Tuple(_), _) => false,
            (
                Pattern::Ctor {
                    id: id_a,
                    args: args_a,
                },
                Pattern::Ctor {
                    id: id_b,
                    args: args_b,
                },
            ) => {
                id_a == id_b
                    && args_a.len() == args_b.len()
                    && args_a
                        .iter()
                        .zip(args_b.iter())
                        .all(|(pa, pb)| self.pattern_subsumes(pa, pb))
            }
            (Pattern::Ctor { .. }, _) => false,
            (
                Pattern::List {
                    prefix: a_prefix,
                    rest: a_rest,
                },
                Pattern::List {
                    prefix: b_prefix,
                    rest: b_rest,
                },
            ) => self.list_subsumes(a_prefix, a_rest, b_prefix, b_rest),
            (Pattern::List { .. }, Pattern::Wildcard | Pattern::Symbol(_)) => false,
            _ => false,
        }
    }

    fn list_subsumes(
        &self,
        a_prefix: &[Pattern],
        a_rest: &ListRest,
        b_prefix: &[Pattern],
        b_rest: &ListRest,
    ) -> bool {
        match a_rest {
            ListRest::Absent => {
                matches!(b_rest, ListRest::Absent)
                    && a_prefix.len() == b_prefix.len()
                    && a_prefix
                        .iter()
                        .zip(b_prefix.iter())
                        .all(|(pa, pb)| self.pattern_subsumes(pa, pb))
            }
            ListRest::Unbound | ListRest::Bound(_) => {
                if b_prefix.len() < a_prefix.len() {
                    return false;
                }

                a_prefix
                    .iter()
                    .zip(b_prefix.iter())
                    .all(|(pa, pb)| self.pattern_subsumes(pa, pb))
            }
        }
    }

    // fn format_constraint(&self, occurrence: &Occurrence, key: &SwitchKey) -> String {
    //     match key {
    //         SwitchKey::Literal(lit) => format!("{} == {}", occurrence.render(), lit),
    //         SwitchKey::Ctor(id) => {
    //             let name = self
    //                 .symbol_names
    //                 .get(id)
    //                 .cloned()
    //                 .unwrap_or_else(|| format!("#{}", id));
    //             format!("{} is {}", occurrence.render(), name)
    //         }
    //         SwitchKey::ListNil => format!("{} == []", occurrence.render()),
    //         SwitchKey::ListCons => format!("{} matches [head, ...tail]", occurrence.render()),
    //     }
    // }
}

impl DecisionTree {
    fn is_exhaustive(&self) -> bool {
        match self {
            DecisionTree::Fail => false,
            DecisionTree::Leaf { .. } => true,
            DecisionTree::Switch {
                arms,
                default,
                covers_all,
                missing_keys: _,
                ..
            } => {
                arms.values().all(|tree| tree.is_exhaustive())
                    && (*covers_all || default.is_exhaustive())
            }
        }
    }

    fn missing_entries(&self) -> Vec<MissingEntry> {
        let mut path: Vec<(Occurrence, SwitchKey, Option<CodeSpan>)> = Vec::new();
        let mut out = Vec::new();
        self.collect_missing_entries(&mut path, &mut out);
        out
    }

    fn collect_missing_entries(
        &self,
        path: &mut Vec<(Occurrence, SwitchKey, Option<CodeSpan>)>,
        out: &mut Vec<MissingEntry>,
    ) {
        match self {
            DecisionTree::Fail => {
                out.push(MissingEntry {
                    constraints: path.clone(),
                });
            }
            DecisionTree::Leaf { .. } => {}
            DecisionTree::Switch {
                occurrence,
                arms,
                default,
                covers_all,
                missing_keys,
                span,
            } => {
                for key in missing_keys.iter() {
                    out.push(MissingEntry {
                        constraints: vec![(occurrence.clone(), key.clone(), span.clone())],
                    });
                }

                for (key, subtree) in arms.iter() {
                    path.push((occurrence.clone(), key.clone(), span.clone()));
                    subtree.collect_missing_entries(path, out);
                    path.pop();
                }

                if !covers_all && missing_keys.is_empty() {
                    default.collect_missing_entries(path, out);
                }
            }
        }
    }

    #[allow(dead_code)]
    fn render(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        match self {
            DecisionTree::Switch {
                occurrence,
                arms,
                default,
                covers_all,
                missing_keys,
                span: _,
            } => {
                let mut result = format!(
                    "{}Switch @{} (covers_all: {}) {{\n",
                    indent_str,
                    occurrence.render(),
                    covers_all
                );
                if !missing_keys.is_empty() {
                    result.push_str(&format!("{}  missing: {:?}\n", indent_str, missing_keys));
                }
                for (key, subtree) in arms.iter() {
                    result.push_str(&format!(
                        "{}  {:?} => \n{}{},\n",
                        indent_str,
                        key,
                        subtree.render(indent + 2),
                        indent_str
                    ));
                }
                result.push_str(&format!(
                    "{}  Default => \n{}{}\n",
                    indent_str,
                    default.render(indent + 2),
                    indent_str
                ));
                result.push_str(&format!("{}}}", indent_str));
                result
            }
            DecisionTree::Leaf { arm_index } => {
                format!("{}Leaf {{ arm_index: {} }}", indent_str, arm_index)
            }
            DecisionTree::Fail => format!("{}Fail", indent_str),
        }
    }
}

#[derive(Debug, Clone)]
struct MissingEntry {
    constraints: Vec<(Occurrence, SwitchKey, Option<CodeSpan>)>,
}
