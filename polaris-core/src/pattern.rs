use crate::{
    ast::{ExprKind, ListPatternElement, Node, NodeKind},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::LIST,
    symbol::{SymbolContext, SymbolId},
    types::{Ty, TyKind},
};
use rustc_hash::FxHashMap as HashMap;

pub fn pattern_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut pass = PatternPass::new(ctx);
    pass.run()
}

type NodeId = usize;

struct PatternPass<'a> {
    ctx: &'a mut CompileContext,
    type_constructors: HashMap<SymbolId, Vec<ConstructorDescriptor>>,
    constructor_to_type: HashMap<SymbolId, SymbolId>,
    current_file: String,
    decision_trees: HashMap<NodeId, DecisionTree>,
}

#[derive(Clone, Debug)]
struct ConstructorDescriptor {
    id: ConstructorId,
    arg_types: Vec<TypeShape>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ConstructorId {
    Symbol(SymbolId),
    Tuple(usize),
    ListNil,
    ListCons,
}

#[derive(Clone, Debug)]
enum TypeShape {
    Unknown,
    Sum(SymbolId),
    Tuple(Vec<TypeShape>),
    List(Box<TypeShape>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pat {
    Wildcard,
    Constructor(ConstructorId, Vec<Pat>),
    Literal(LiteralValue),
    Binding(SymbolId),
    Rest(Option<SymbolId>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralValue {
    Int(i64),
    String(String),
    Real(String),
}

#[derive(Clone, Debug)]
struct Usefulness {
    useful: bool,
    witness: Option<Vec<Pat>>,
}

#[derive(Clone, Debug)]
pub struct DecisionTree {
    pub node: DecisionNode,
}

#[derive(Clone, Debug)]
pub enum DecisionNode {
    Action {
        arm_index: usize,
    },
    Switch {
        position: usize,
        branches: Vec<(SwitchKey, DecisionNode)>,
        default: Option<Box<DecisionNode>>,
    },
    Failure,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SwitchKey {
    Constructor(ConstructorId),
    Literal(LiteralValue),
    Tuple(usize), // arity
    ListNil,
    ListCons,
}

impl<'a> PatternPass<'a> {
    fn new(ctx: &'a mut CompileContext) -> Self {
        let (type_constructors, constructor_to_type) =
            collect_type_constructors(&ctx.symbols, &ctx.dependencies);

        Self {
            ctx,
            type_constructors,
            constructor_to_type,
            current_file: String::new(),
            decision_trees: HashMap::default(),
        }
    }

    fn run(&mut self) -> Result<(), ()> {
        let module_ids: Vec<String> = self.ctx.dependencies.modules.keys().cloned().collect();
        let initial_error_count = self.ctx.errors.len();

        for module_id in module_ids {
            if let Some(module) = self.ctx.dependencies.modules.get(&module_id) {
                self.current_file = module.file.clone();
                let module_ast = module.ast.clone();
                self.visit_node(&module_ast);
            }
        }

        if self.ctx.errors.len() > initial_error_count {
            Err(())
        } else {
            Ok(())
        }
    }

    //for debug
    #[allow(dead_code)]
    pub fn render_decision_tree(&self, tree: &DecisionTree) -> String {
        self.render_decision_node(&tree.node, 0)
    }

    fn render_decision_node(&self, node: &DecisionNode, depth: usize) -> String {
        let indent = "  ".repeat(depth);
        match node {
            DecisionNode::Action { arm_index } => {
                format!("{}Action(arm: {})\n", indent, arm_index)
            }
            DecisionNode::Switch {
                position,
                branches,
                default,
            } => {
                let mut result = format!("{}Switch(pos: {}) {{\n", indent, position);

                for (key, child) in branches {
                    let key_str = self.format_switch_key(key);
                    result.push_str(&format!("{}  {} =>\n", indent, key_str));
                    result.push_str(&self.render_decision_node(child, depth + 2));
                }

                if let Some(default_node) = default {
                    result.push_str(&format!("{}  _ =>\n", indent));
                    result.push_str(&self.render_decision_node(default_node, depth + 2));
                }

                result.push_str(&format!("{}}}\n", indent));
                result
            }
            DecisionNode::Failure => format!("{}Failure\n", indent),
        }
    }

    fn format_switch_key(&self, key: &SwitchKey) -> String {
        match key {
            SwitchKey::Constructor(ctor_id) => match ctor_id {
                ConstructorId::Symbol(id) => self
                    .ctx
                    .symbols
                    .symbol_names
                    .get(id)
                    .cloned()
                    .unwrap_or_else(|| format!("Symbol({})", id)),
                ConstructorId::Tuple(arity) => format!("Tuple({})", arity),
                ConstructorId::ListNil => "ListNil".to_string(),
                ConstructorId::ListCons => "ListCons".to_string(),
            },
            SwitchKey::Literal(lit) => match lit {
                LiteralValue::Int(n) => n.to_string(),
                LiteralValue::String(s) => format!("\"{}\"", s),
                LiteralValue::Real(r) => r.clone(),
            },
            SwitchKey::Tuple(arity) => format!("Tuple({})", arity),
            SwitchKey::ListNil => "[]".to_string(),
            SwitchKey::ListCons => "[_|_]".to_string(),
        }
    }

    fn visit_node(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Module { children } => {
                for child in children {
                    self.visit_node(child);
                }
            }
            NodeKind::FnDecl { expr, .. } => {
                if let Some(body) = expr {
                    self.visit_node(body);
                }
            }
            NodeKind::ConstDecl { expr, .. } => self.visit_node(expr),
            NodeKind::TypeAlias { actual, .. } => self.visit_node(actual),
            NodeKind::TypeDecl { variants, .. } => {
                for variant in variants {
                    self.visit_node(variant);
                }
            }
            NodeKind::TypeConstructor { fields, .. } => {
                for (_, ty, _) in fields {
                    self.visit_node(ty);
                }
            }
            NodeKind::Expr { expr } => self.visit_expr(node, expr),
            _ => {}
        }
    }

    fn visit_expr(&mut self, node: &Node, expr: &ExprKind) {
        match expr {
            ExprKind::Match {
                expr: scrutinee,
                arms,
            } => {
                self.visit_node(scrutinee);
                for (_, arm_expr) in arms {
                    self.visit_node(arm_expr);
                }
                self.analyze_match(node, scrutinee, arms);
            }
            ExprKind::LetBinding { expr, .. } => self.visit_node(expr),
            ExprKind::FnCall { callee, args } => {
                self.visit_node(callee);
                for (_, arg) in args {
                    self.visit_node(arg);
                }
            }
            ExprKind::Closure { expr, .. } => self.visit_node(expr),
            ExprKind::For {
                binding,
                start,
                end,
                body,
            } => {
                self.visit_node(binding);
                self.visit_node(start);
                self.visit_node(end);
                self.visit_node(body);
            }
            ExprKind::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.visit_node(condition);
                self.visit_node(then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_node(else_branch);
                }
            }
            ExprKind::BinaryOp { left, right, .. } => {
                self.visit_node(left);
                self.visit_node(right);
            }
            ExprKind::UnaryOp { expr, .. } => self.visit_node(expr),
            ExprKind::Block(statements) => {
                for stmt in statements {
                    self.visit_node(stmt);
                }
            }
            ExprKind::ListLit(elements) | ExprKind::TupleLit(elements) => {
                for element in elements {
                    self.visit_node(element);
                }
            }
            ExprKind::ListPattern(elements) => {
                for element in elements {
                    match element {
                        ListPatternElement::Element(node) => self.visit_node(node),
                        ListPatternElement::Wildcard => {}
                        ListPatternElement::Rest(Some(node)) => self.visit_node(node),
                        ListPatternElement::Rest(None) => {}
                    }
                }
            }
            ExprKind::MapLit(entries) => {
                for (key, value) in entries {
                    self.visit_node(key);
                    self.visit_node(value);
                }
            }
            ExprKind::FieldAccess { expr, .. } | ExprKind::IndexAccess { expr, .. } => {
                self.visit_node(expr)
            }
            ExprKind::IntLit(_)
            | ExprKind::RealLit { .. }
            | ExprKind::StringLit(_)
            | ExprKind::Symbol { .. }
            | ExprKind::Discard => {}
        }
    }

    fn analyze_match(&mut self, match_node: &Node, scrutinee: &Node, arms: &[(Vec<Node>, Node)]) {
        self.check_patterns(match_node, scrutinee, arms);

        let decision_tree = self.build_decision_tree(arms);
        let node_id = match_node as *const _ as usize;

        self.decision_trees.insert(node_id, decision_tree);
    }

    fn check_patterns(&mut self, match_node: &Node, scrutinee: &Node, arms: &[(Vec<Node>, Node)]) {
        let scrutinee_ty = match_node
            .ty
            .as_ref()
            .or_else(|| scrutinee.ty.as_ref())
            .cloned();

        let Some(scrutinee_ty) = scrutinee_ty else {
            return;
        };

        let scrutinee_shape = self.type_to_shape(&scrutinee_ty);
        let column_types = vec![scrutinee_shape.clone()];
        let mut matrix: Vec<Vec<Pat>> = Vec::new();

        for (patterns, _) in arms.iter() {
            for pattern in patterns.iter() {
                let lowered = self.lower_pattern(pattern);
                let row = vec![lowered.clone()];
                let usefulness = is_useful(&matrix, &row, &column_types, self, false);

                if usefulness.useful {
                    matrix.push(row);
                } else {
                    self.report_unreachable_pattern(pattern, &lowered);
                }
            }
        }
        let available_constructors = self.detect_constructors_from_patterns(&matrix);
        if !available_constructors.is_empty() {
            let type_id = if let Some(first_constructor_id) = available_constructors.keys().next() {
                self.constructor_to_type
                    .get(first_constructor_id)
                    .copied()
                    .unwrap_or(0)
            } else {
                0
            };

            let original_constructors = self.type_constructors.get(&type_id).cloned();

            if !available_constructors.is_empty() {
                let mut all_type_constructors = Vec::new();
                for (&constructor_id, &mapped_type_id) in &self.constructor_to_type {
                    if mapped_type_id == type_id {
                        let arg_types = if let Some(constructor_scheme) =
                            self.ctx.type_info.type_env.get(&constructor_id)
                        {
                            self.get_constructor_arg_shapes(&constructor_scheme.body)
                        } else {
                            vec![]
                        };

                        let descriptor = ConstructorDescriptor {
                            id: ConstructorId::Symbol(constructor_id),
                            arg_types,
                        };
                        all_type_constructors.push(descriptor);
                    }
                }

                self.type_constructors
                    .insert(type_id, all_type_constructors);

                let sum_column_types = vec![TypeShape::Sum(type_id)];
                self.check_exhaustiveness(&matrix, &sum_column_types, match_node);
            } else {
                self.check_exhaustiveness(&matrix, &column_types, match_node);
            }

            if let Some(original) = original_constructors {
                self.type_constructors.insert(type_id, original);
            } else {
                self.type_constructors.remove(&type_id);
            }
        }
    }

    fn check_exhaustiveness(
        &mut self,
        matrix: &[Vec<Pat>],
        column_types: &[TypeShape],
        match_node: &Node,
    ) {
        let wildcard_row = vec![Pat::Wildcard];
        let usefulness = is_useful(matrix, &wildcard_row, column_types, self, true);

        if usefulness.useful {
            if let Some(witness) = usefulness.witness
                && let Some(pattern) = witness.first()
            {
                let pattern_str = self.render_pattern(pattern);
                self.report_non_exhaustive(match_node, &pattern_str);
            }
        }
    }

    fn build_decision_tree(&mut self, arms: &[(Vec<Node>, Node)]) -> DecisionTree {
        let mut matrix = Vec::new();
        let mut actions = Vec::new();

        for (arm_index, (patterns, _)) in arms.iter().enumerate() {
            for pattern in patterns {
                let lowered = self.lower_pattern(pattern);
                matrix.push(vec![lowered]);
                actions.push(arm_index);
            }
        }

        let root_node = self.compile_decision_tree_recursive(matrix, actions);
        DecisionTree { node: root_node }
    }

    fn compile_decision_tree_recursive(
        &mut self,
        matrix: Vec<Vec<Pat>>,
        actions: Vec<usize>,
    ) -> DecisionNode {
        // Base cases
        if matrix.is_empty() {
            return DecisionNode::Failure;
        }

        if matrix.len() == 1 {
            return DecisionNode::Action {
                arm_index: actions[0],
            };
        }

        if let Some(&first_action) = actions.first() {
            if actions.iter().all(|&action| action == first_action) {
                let all_wildcards = matrix
                    .iter()
                    .all(|row| row.iter().all(|p| matches!(p, Pat::Wildcard)));
                if all_wildcards {
                    return DecisionNode::Action {
                        arm_index: first_action,
                    };
                }
            }
        }

        let switch_col = self.choose_switch_column(&matrix);
        let (branches, default) = self.group_patterns_by_structure(&matrix, &actions, switch_col);

        DecisionNode::Switch {
            position: switch_col,
            branches,
            default,
        }
    }

    fn choose_switch_column(&self, matrix: &[Vec<Pat>]) -> usize {
        matrix
            .iter()
            .find(|row| !row.is_empty() && !matches!(row[0], Pat::Wildcard))
            .map(|_| 0)
            .unwrap_or(0)
    }

    fn group_patterns_by_structure(
        &mut self,
        matrix: &[Vec<Pat>],
        actions: &[usize],
        switch_col: usize,
    ) -> (Vec<(SwitchKey, DecisionNode)>, Option<Box<DecisionNode>>) {
        let mut groups: HashMap<SwitchKey, (Vec<Vec<Pat>>, Vec<usize>)> = HashMap::default();
        let mut default_matrix = Vec::new();
        let mut default_actions = Vec::new();

        for (i, row) in matrix.iter().enumerate() {
            if switch_col >= row.len() {
                continue;
            }

            match &row[switch_col] {
                Pat::Wildcard => {
                    default_matrix.push(if row.len() > 1 {
                        row[1..].to_vec()
                    } else {
                        vec![]
                    });
                    default_actions.push(actions[i]);
                }
                Pat::Binding(_) => {
                    default_matrix.push(if row.len() > 1 {
                        row[1..].to_vec()
                    } else {
                        vec![]
                    });
                    default_actions.push(actions[i]);
                }
                Pat::Rest(_) => {
                    default_matrix.push(if row.len() > 1 {
                        row[1..].to_vec()
                    } else {
                        vec![]
                    });
                    default_actions.push(actions[i]);
                }
                Pat::Constructor(ctor_id, args) => {
                    let key = SwitchKey::Constructor(*ctor_id);
                    let entry = groups
                        .entry(key)
                        .or_insert_with(|| (Vec::new(), Vec::new()));

                    let mut new_row = args.clone();
                    if row.len() > 1 {
                        new_row.extend_from_slice(&row[1..]);
                    }
                    entry.0.push(new_row);
                    entry.1.push(actions[i]);
                }
                Pat::Literal(lit) => {
                    let key = SwitchKey::Literal(lit.clone());
                    let entry = groups
                        .entry(key)
                        .or_insert_with(|| (Vec::new(), Vec::new()));

                    entry.0.push(if row.len() > 1 {
                        row[1..].to_vec()
                    } else {
                        vec![]
                    });
                    entry.1.push(actions[i]);
                }
            }
        }

        let mut branches = Vec::new();
        for (key, (sub_matrix, sub_actions)) in groups {
            let child_node = self.compile_decision_tree_recursive(sub_matrix, sub_actions);
            branches.push((key, child_node));
        }

        let default = if !default_matrix.is_empty() {
            Some(Box::new(self.compile_decision_tree_recursive(
                default_matrix,
                default_actions,
            )))
        } else {
            None
        };

        (branches, default)
    }

    fn report_unreachable_pattern(&mut self, pattern_node: &Node, pattern: &Pat) {
        let pattern_str = self.render_pattern(pattern);

        let diag = Diagnostic::new(DiagnosticMsg {
            message: format!(
                "Pattern `{}` is unreachable because previous patterns cover all cases",
                pattern_str
            ),
            span: pattern_node.span.clone(),
            file: self.current_file.clone(),
            err_type: DiagnosticMsgType::UnreachablePattern,
        });
        self.ctx.errors.push(diag);
    }

    fn report_non_exhaustive(&mut self, match_node: &Node, pattern_str: &str) {
        let diag = Diagnostic::new(DiagnosticMsg {
            message: format!(
                "Match expression is not exhaustive; missing pattern `{}`",
                pattern_str
            ),
            span: match_node.span.clone(),
            file: self.current_file.clone(),
            err_type: DiagnosticMsgType::NonExhaustivePatterns,
        });
        self.ctx.errors.push(diag);
    }

    fn lower_pattern(&self, node: &Node) -> Pat {
        if let NodeKind::Expr { expr } = &node.kind {
            match expr {
                ExprKind::Discard => Pat::Wildcard,
                ExprKind::Symbol { .. } => self.lower_symbol_pattern(node),
                ExprKind::TupleLit(elements) => {
                    let args: Vec<Pat> = elements.iter().map(|e| self.lower_pattern(e)).collect();
                    Pat::Constructor(ConstructorId::Tuple(args.len()), args)
                }
                ExprKind::ListPattern(elements) => self.lower_list_pattern(elements),
                ExprKind::ListLit(elements) => {
                    let args: Vec<Pat> = elements.iter().map(|e| self.lower_pattern(e)).collect();
                    self.build_list_from_elements(
                        args,
                        Pat::Constructor(ConstructorId::ListNil, vec![]),
                    )
                }
                ExprKind::IntLit(value) => Pat::Literal(LiteralValue::Int(*value)),
                ExprKind::RealLit { value, .. } => {
                    Pat::Literal(LiteralValue::Real(value.to_string()))
                }
                ExprKind::StringLit(value) => Pat::Literal(LiteralValue::String(value.clone())),
                ExprKind::FnCall { callee, args } => self.lower_constructor_call(callee, args),
                _ => Pat::Wildcard,
            }
        } else {
            Pat::Wildcard
        }
    }

    fn lower_symbol_pattern(&self, node: &Node) -> Pat {
        if let Some(symbol_id) = node.symbol_id {
            if self.constructor_to_type.contains_key(&symbol_id) {
                return Pat::Constructor(ConstructorId::Symbol(symbol_id), vec![]);
            } else {
                return Pat::Binding(symbol_id);
            }
        }
        Pat::Wildcard
    }

    fn lower_constructor_call(&self, callee: &Node, args: &[(Option<String>, Node)]) -> Pat {
        if let NodeKind::Expr {
            expr: ExprKind::Symbol { .. },
        } = &callee.kind
        {
            if let Some(symbol_id) = callee.symbol_id {
                if self.constructor_to_type.contains_key(&symbol_id) {
                    let lowered_args: Vec<Pat> = args
                        .iter()
                        .map(|(_, node)| self.lower_pattern(node))
                        .collect();
                    return Pat::Constructor(ConstructorId::Symbol(symbol_id), lowered_args);
                }
            }
        }
        Pat::Wildcard
    }

    fn lower_list_pattern(&self, elements: &[ListPatternElement]) -> Pat {
        let mut prefix: Vec<Pat> = Vec::new();
        let mut rest_pattern: Option<Pat> = None;

        for element in elements {
            match element {
                ListPatternElement::Element(node) => prefix.push(self.lower_pattern(node)),
                ListPatternElement::Wildcard => prefix.push(Pat::Wildcard),
                ListPatternElement::Rest(Some(node)) => {
                    if let Some(symbol_id) = node.symbol_id {
                        rest_pattern = Some(Pat::Rest(Some(symbol_id)));
                    } else {
                        rest_pattern = Some(self.lower_pattern(node));
                    }
                    break;
                }
                ListPatternElement::Rest(None) => {
                    rest_pattern = Some(Pat::Rest(None));
                    break;
                }
            }
        }

        let tail = rest_pattern.unwrap_or_else(|| Pat::Constructor(ConstructorId::ListNil, vec![]));
        self.build_list_from_elements(prefix, tail)
    }

    fn build_list_from_elements(&self, mut elements: Vec<Pat>, tail: Pat) -> Pat {
        let mut acc = tail;
        while let Some(head) = elements.pop() {
            acc = Pat::Constructor(ConstructorId::ListCons, vec![head, acc]);
        }
        acc
    }

    fn get_constructor_arg_shapes(&self, constructor_type: &Ty) -> Vec<TypeShape> {
        let mut args = Vec::new();
        let mut current = constructor_type;

        while let TyKind::Fn(arg, ret) = &current.kind() {
            args.push(self.type_to_shape(arg));
            current = ret;
        }

        args
    }

    fn detect_constructors_from_patterns(
        &self,
        matrix: &[Vec<Pat>],
    ) -> HashMap<SymbolId, ConstructorDescriptor> {
        let mut constructors = HashMap::default();

        for row in matrix {
            if let Some(first_pattern) = row.first() {
                self.collect_constructors_from_pattern(first_pattern, &mut constructors);
            }
        }

        constructors
    }

    fn collect_constructors_from_pattern(
        &self,
        pattern: &Pat,
        constructors: &mut HashMap<SymbolId, ConstructorDescriptor>,
    ) {
        match pattern {
            Pat::Constructor(ConstructorId::Symbol(sym_id), args) => {
                if self.ctx.symbols.type_constructors.contains_key(sym_id) {
                    let arg_types = args.iter().map(|_| TypeShape::Unknown).collect();
                    constructors.insert(
                        *sym_id,
                        ConstructorDescriptor {
                            id: ConstructorId::Symbol(*sym_id),
                            arg_types,
                        },
                    );
                }
            }
            Pat::Constructor(_other_id, _args) => {
                //
            }
            _ => {}
        }
    }

    fn type_to_shape(&self, ty: &Ty) -> TypeShape {
        match ty.kind() {
            TyKind::Nocrypt(inner) => self.type_to_shape(inner.as_ref()),
            TyKind::Concrete(id) => {
                if *id == self.ctx.symbols.intrinsic_types[LIST] {
                    TypeShape::List(Box::new(TypeShape::Unknown))
                } else if self.type_constructors.contains_key(id) {
                    TypeShape::Sum(*id)
                } else {
                    TypeShape::Unknown
                }
            }
            TyKind::Ctor(id, args) => {
                if *id == self.ctx.symbols.intrinsic_types[LIST] {
                    let elem_shape = args
                        .get(0)
                        .map(|ty| self.type_to_shape(ty))
                        .unwrap_or(TypeShape::Unknown);
                    TypeShape::List(Box::new(elem_shape))
                } else if self.type_constructors.contains_key(id) {
                    TypeShape::Sum(*id)
                } else {
                    TypeShape::Unknown
                }
            }
            TyKind::Tuple(elements) => {
                let shapes = elements.iter().map(|ty| self.type_to_shape(ty)).collect();
                TypeShape::Tuple(shapes)
            }
            TyKind::Fn(_, _) | TyKind::Var(_) => TypeShape::Unknown,
        }
    }

    fn render_pattern(&self, pattern: &Pat) -> String {
        match pattern {
            Pat::Wildcard => "_".to_string(),
            Pat::Binding(symbol_id) => self
                .ctx
                .symbols
                .symbol_names
                .get(symbol_id)
                .cloned()
                .unwrap_or_else(|| format!("#{}", symbol_id)),
            Pat::Rest(Some(symbol_id)) => {
                let name = self
                    .ctx
                    .symbols
                    .symbol_names
                    .get(symbol_id)
                    .cloned()
                    .unwrap_or_else(|| format!("#{}", symbol_id));
                format!("...{}", name)
            }
            Pat::Rest(None) => "...".to_string(),
            Pat::Literal(lit_value) => match lit_value {
                LiteralValue::Int(value) => value.to_string(),
                LiteralValue::String(value) => format!("\"{}\"", value),
                LiteralValue::Real(value) => value.clone(),
            },
            Pat::Constructor(ConstructorId::Symbol(id), args) => {
                let name = self
                    .ctx
                    .symbols
                    .symbol_names
                    .get(id)
                    .cloned()
                    .unwrap_or_else(|| format!("#{}", id));
                if args.is_empty() {
                    name
                } else {
                    let rendered: Vec<String> =
                        args.iter().map(|p| self.render_pattern(p)).collect();
                    format!("{}({})", name, rendered.join(", "))
                }
            }
            Pat::Constructor(ConstructorId::Tuple(_), elements) => {
                let rendered: Vec<String> =
                    elements.iter().map(|p| self.render_pattern(p)).collect();
                format!("#({})", rendered.join(", "))
            }
            Pat::Constructor(ConstructorId::ListNil, _) => "[]".to_string(),
            Pat::Constructor(ConstructorId::ListCons, args) => self.render_list_pattern(args),
        }
    }

    fn render_list_pattern(&self, args: &[Pat]) -> String {
        if args.len() != 2 {
            return "[?]".to_string();
        }

        let mut elems: Vec<String> = Vec::new();
        let mut tail = &args[1];

        elems.push(self.render_pattern(&args[0]));

        while let Pat::Constructor(ConstructorId::ListCons, nested) = tail {
            if nested.len() != 2 {
                break;
            }
            elems.push(self.render_pattern(&nested[0]));
            tail = &nested[1];
        }

        let rest = match tail {
            Pat::Constructor(ConstructorId::ListNil, _) => None,
            Pat::Wildcard => Some("_".to_string()),
            other => Some(self.render_pattern(other)),
        };

        let prefix = elems.join(", ");
        if let Some(rest_pattern) = rest {
            if rest_pattern == "_" {
                if prefix.is_empty() {
                    "[...]".to_string()
                } else {
                    format!("[{}, ...]", prefix)
                }
            } else if prefix.is_empty() {
                format!("[{}]", rest_pattern)
            } else {
                format!("[{}, {}]", prefix, rest_pattern)
            }
        } else {
            format!("[{}]", prefix)
        }
    }
}

fn collect_type_constructors(
    symbols: &SymbolContext,
    deps: &crate::module::DepGraphContext,
) -> (
    HashMap<SymbolId, Vec<ConstructorDescriptor>>,
    HashMap<SymbolId, SymbolId>,
) {
    let mut type_constructors: HashMap<SymbolId, Vec<ConstructorDescriptor>> = HashMap::default();
    let mut constructor_to_type: HashMap<SymbolId, SymbolId> = HashMap::default();

    for module in deps.modules.values() {
        collect_from_node(
            &module.ast,
            &mut type_constructors,
            &mut constructor_to_type,
        );
    }

    for (&constructor_id, &type_id) in &symbols.type_constructors {
        constructor_to_type.insert(constructor_id, type_id);

        let descriptor = ConstructorDescriptor {
            id: ConstructorId::Symbol(constructor_id),
            arg_types: vec![],
        };

        type_constructors
            .entry(type_id)
            .or_insert_with(Vec::new)
            .push(descriptor);
    }

    (type_constructors, constructor_to_type)
}

fn collect_from_node(
    node: &Node,
    type_constructors: &mut HashMap<SymbolId, Vec<ConstructorDescriptor>>,
    constructor_to_type: &mut HashMap<SymbolId, SymbolId>,
) {
    match &node.kind {
        NodeKind::Module { children } => {
            for child in children {
                collect_from_node(child, type_constructors, constructor_to_type);
            }
        }
        NodeKind::TypeDecl { variants, .. } => {
            if let Some(type_id) = node.symbol_id {
                let mut ctors = Vec::new();
                for variant in variants {
                    if let NodeKind::TypeConstructor { fields, .. } = &variant.kind {
                        if let Some(constructor_id) = variant.symbol_id {
                            constructor_to_type.insert(constructor_id, type_id);
                            ctors.push(ConstructorDescriptor {
                                id: ConstructorId::Symbol(constructor_id),
                                arg_types: vec![TypeShape::Unknown; fields.len()],
                            });
                        }
                    }
                }
                if !ctors.is_empty() {
                    type_constructors.insert(type_id, ctors);
                }
            }
        }
        _ => {}
    }
}

fn constructors_for_shape(
    pass: &PatternPass<'_>,
    shape: &TypeShape,
) -> Option<Vec<ConstructorDescriptor>> {
    match shape {
        TypeShape::Unknown => None,
        TypeShape::Sum(type_id) => pass.type_constructors.get(type_id).cloned(),
        TypeShape::Tuple(elements) => Some(vec![ConstructorDescriptor {
            id: ConstructorId::Tuple(elements.len()),
            arg_types: elements.clone(),
        }]),
        TypeShape::List(elem) => Some(vec![
            ConstructorDescriptor {
                id: ConstructorId::ListNil,
                arg_types: vec![],
            },
            ConstructorDescriptor {
                id: ConstructorId::ListCons,
                arg_types: vec![elem.as_ref().clone(), shape.clone()],
            },
        ]),
    }
}

fn default_matrix(matrix: &[Vec<Pat>]) -> Vec<Vec<Pat>> {
    matrix
        .iter()
        .filter_map(|row| {
            if row.is_empty() {
                Some(vec![])
            } else if matches!(row[0], Pat::Wildcard | Pat::Binding(_) | Pat::Rest(_)) {
                Some(row[1..].to_vec())
            } else {
                None
            }
        })
        .collect()
}

fn default_vector(vector: &[Pat]) -> Vec<Pat> {
    if vector.len() <= 1 {
        vec![]
    } else {
        vector[1..].to_vec()
    }
}

fn specialize_matrix(matrix: &[Vec<Pat>], ctor_id: ConstructorId, arity: usize) -> Vec<Vec<Pat>> {
    let mut result = Vec::new();
    for row in matrix {
        if row.is_empty() {
            continue;
        }
        match &row[0] {
            Pat::Constructor(id, args) if *id == ctor_id => {
                let mut new_row = args.clone();
                new_row.extend_from_slice(&row[1..]);
                result.push(new_row);
            }
            Pat::Wildcard => {
                let mut new_row = vec![Pat::Wildcard; arity];
                new_row.extend_from_slice(&row[1..]);
                result.push(new_row);
            }
            Pat::Binding(_) => {
                let mut new_row = vec![Pat::Wildcard; arity];
                new_row.extend_from_slice(&row[1..]);
                result.push(new_row);
            }
            Pat::Rest(_) => {
                let mut new_row = vec![Pat::Wildcard; arity];
                new_row.extend_from_slice(&row[1..]);
                result.push(new_row);
            }
            Pat::Literal(_) => {
                //
            }
            Pat::Constructor(_, _) => {
                //
            }
        }
    }
    result
}

fn specialize_vector(vector: &[Pat], ctor_id: ConstructorId, arity: usize) -> Option<Vec<Pat>> {
    if vector.is_empty() {
        return Some(vec![]);
    }

    match &vector[0] {
        Pat::Constructor(id, args) if *id == ctor_id => {
            if args.len() != arity {
                return None;
            }
            let mut result = args.clone();
            result.extend_from_slice(&vector[1..]);
            Some(result)
        }
        Pat::Wildcard => {
            let mut result = vec![Pat::Wildcard; arity];
            result.extend_from_slice(&vector[1..]);
            Some(result)
        }
        Pat::Binding(_) => {
            let mut result = vec![Pat::Wildcard; arity];
            result.extend_from_slice(&vector[1..]);
            Some(result)
        }
        Pat::Rest(_) => {
            let mut result = vec![Pat::Wildcard; arity];
            result.extend_from_slice(&vector[1..]);
            Some(result)
        }
        Pat::Literal(_) => None,
        Pat::Constructor(_, _) => None,
    }
}

fn specialize_matrix_literal(matrix: &[Vec<Pat>], literal: &LiteralValue) -> Vec<Vec<Pat>> {
    let mut result = Vec::new();
    for row in matrix {
        if row.is_empty() {
            continue;
        }
        match &row[0] {
            Pat::Literal(lit) if lit == literal => {
                result.push(row[1..].to_vec());
            }
            Pat::Wildcard => {
                result.push(row[1..].to_vec());
            }
            Pat::Binding(_) => {
                result.push(row[1..].to_vec());
            }
            Pat::Rest(_) => {
                result.push(row[1..].to_vec());
            }
            _ => {}
        }
    }
    result
}

fn specialize_vector_literal(vector: &[Pat], literal: &LiteralValue) -> Option<Vec<Pat>> {
    if vector.is_empty() {
        return Some(vec![]);
    }

    match &vector[0] {
        Pat::Literal(lit) if lit == literal => Some(vector[1..].to_vec()),
        Pat::Wildcard => Some(vector[1..].to_vec()),
        Pat::Binding(_) => Some(vector[1..].to_vec()),
        Pat::Rest(_) => Some(vector[1..].to_vec()),
        _ => None,
    }
}

fn default_witness(pass: &PatternPass<'_>, types: &[TypeShape]) -> Vec<Pat> {
    types
        .iter()
        .map(|shape| {
            let result = match shape {
                TypeShape::Unknown => Pat::Wildcard,
                TypeShape::Sum(type_id) => pass
                    .type_constructors
                    .get(type_id)
                    .and_then(|ctors| ctors.first())
                    .map(|ctor| {
                        let args = default_witness(pass, &ctor.arg_types);
                        Pat::Constructor(ctor.id, args)
                    })
                    .unwrap_or(Pat::Wildcard),
                TypeShape::Tuple(elements) => {
                    let args = default_witness(pass, elements);
                    Pat::Constructor(ConstructorId::Tuple(elements.len()), args)
                }
                TypeShape::List(_) => Pat::Constructor(ConstructorId::ListNil, vec![]),
            };

            result
        })
        .collect()
}

fn is_useful(
    matrix: &[Vec<Pat>],
    vector: &[Pat],
    types: &[TypeShape],
    pass: &PatternPass<'_>,
    want_witness: bool,
) -> Usefulness {
    if vector.is_empty() {
        let useful = !matrix.iter().any(|row| row.is_empty());
        return Usefulness {
            useful,
            witness: if useful && want_witness {
                Some(vec![])
            } else {
                None
            },
        };
    }

    if matrix.is_empty() {
        return Usefulness {
            useful: true,
            witness: if want_witness {
                Some(default_witness(pass, types))
            } else {
                None
            },
        };
    }

    let head_type = &types[0];
    let tail_types = &types[1..];
    let head_pat = &vector[0];

    match head_pat {
        Pat::Literal(lit_value) => {
            handle_literal_usefulness(matrix, vector, tail_types, lit_value, pass, want_witness)
        }
        Pat::Constructor(ctor_id, _) => handle_constructor_usefulness(
            matrix,
            vector,
            head_type,
            tail_types,
            *ctor_id,
            pass,
            want_witness,
        ),
        Pat::Wildcard | Pat::Binding(_) | Pat::Rest(_) => {
            handle_wildcard_usefulness(matrix, vector, head_type, tail_types, pass, want_witness)
        }
    }
}

fn handle_literal_usefulness(
    matrix: &[Vec<Pat>],
    vector: &[Pat],
    tail_types: &[TypeShape],
    lit_value: &LiteralValue,
    pass: &PatternPass<'_>,
    want_witness: bool,
) -> Usefulness {
    let specialized_matrix = specialize_matrix_literal(matrix, lit_value);
    let Some(specialized_vector) = specialize_vector_literal(vector, lit_value) else {
        return Usefulness {
            useful: false,
            witness: None,
        };
    };

    let result = is_useful(
        &specialized_matrix,
        &specialized_vector,
        tail_types,
        pass,
        want_witness,
    );

    if result.useful && want_witness {
        if let Some(mut witness) = result.witness {
            let mut combined = vec![Pat::Literal(lit_value.clone())];
            combined.append(&mut witness);
            return Usefulness {
                useful: true,
                witness: Some(combined),
            };
        }
    }

    result
}

fn handle_constructor_usefulness(
    matrix: &[Vec<Pat>],
    vector: &[Pat],
    head_type: &TypeShape,
    tail_types: &[TypeShape],
    ctor_id: ConstructorId,
    pass: &PatternPass<'_>,
    want_witness: bool,
) -> Usefulness {
    let constructors = constructors_for_shape(pass, head_type);
    let Some(ctor_info) =
        constructors.and_then(|ctors| ctors.into_iter().find(|c| c.id == ctor_id))
    else {
        return Usefulness {
            useful: true,
            witness: if want_witness {
                Some(default_witness(pass, &[head_type.clone()]))
            } else {
                None
            },
        };
    };

    let Some(specialized_vector) =
        specialize_vector(vector, ctor_info.id, ctor_info.arg_types.len())
    else {
        return Usefulness {
            useful: false,
            witness: None,
        };
    };

    let specialized_matrix = specialize_matrix(matrix, ctor_info.id, ctor_info.arg_types.len());
    let mut new_types = ctor_info.arg_types.clone();
    new_types.extend_from_slice(tail_types);

    let result = is_useful(
        &specialized_matrix,
        &specialized_vector,
        &new_types,
        pass,
        want_witness,
    );

    if result.useful && want_witness {
        if let Some(mut witness) = result.witness {
            let mut rest = witness.split_off(ctor_info.arg_types.len());
            let args_witness = witness;
            let mut combined = vec![Pat::Constructor(ctor_info.id, args_witness)];
            combined.append(&mut rest);
            return Usefulness {
                useful: true,
                witness: Some(combined),
            };
        }
    }

    result
}

fn handle_wildcard_usefulness(
    matrix: &[Vec<Pat>],
    vector: &[Pat],
    head_type: &TypeShape,
    tail_types: &[TypeShape],
    pass: &PatternPass<'_>,
    want_witness: bool,
) -> Usefulness {
    if let Some(constructors) = constructors_for_shape(pass, head_type) {
        for ctor in constructors {
            let Some(specialized_vector) = specialize_vector(vector, ctor.id, ctor.arg_types.len())
            else {
                continue;
            };

            let specialized_matrix = specialize_matrix(matrix, ctor.id, ctor.arg_types.len());
            let mut new_types = ctor.arg_types.clone();
            new_types.extend_from_slice(tail_types);

            let result = is_useful(
                &specialized_matrix,
                &specialized_vector,
                &new_types,
                pass,
                want_witness,
            );

            if result.useful {
                if want_witness {
                    if let Some(mut witness) = result.witness {
                        let split_at = ctor.arg_types.len();
                        let mut rest = witness.split_off(split_at);
                        let args_witness = witness;
                        let mut combined = vec![Pat::Constructor(ctor.id, args_witness)];
                        combined.append(&mut rest);
                        return Usefulness {
                            useful: true,
                            witness: Some(combined),
                        };
                    }
                } else {
                    return result;
                }
            }
        }

        let mut all_types = vec![head_type.clone()];
        all_types.extend_from_slice(tail_types);
        check_uncovered_literals(matrix, vector, &all_types, pass, want_witness)
    } else {
        let default_matrix = default_matrix(matrix);
        let default_vector = default_vector(vector);
        let result = is_useful(
            &default_matrix,
            &default_vector,
            tail_types,
            pass,
            want_witness,
        );

        if result.useful && want_witness {
            if let Some(mut witness) = result.witness {
                let mut combined = vec![Pat::Wildcard];
                combined.append(&mut witness);
                return Usefulness {
                    useful: true,
                    witness: Some(combined),
                };
            } else {
                return Usefulness {
                    useful: true,
                    witness: Some(vec![Pat::Wildcard]),
                };
            }
        }

        result
    }
}

fn check_uncovered_literals(
    matrix: &[Vec<Pat>],
    vector: &[Pat],
    types: &[TypeShape],
    pass: &PatternPass<'_>,
    want_witness: bool,
) -> Usefulness {
    let literal_patterns_in_matrix: Vec<_> = matrix
        .iter()
        .filter_map(|row| {
            if let Some(Pat::Literal(lit)) = row.first() {
                Some(lit.clone())
            } else {
                None
            }
        })
        .collect();

    for literal in literal_patterns_in_matrix {
        let literal_vector = vec![Pat::Literal(literal.clone())];
        let mut full_vector = literal_vector;
        full_vector.extend_from_slice(&vector[1..]);

        let result = is_useful(matrix, &full_vector, types, pass, want_witness);
        if result.useful {
            if want_witness {
                if let Some(witness) = result.witness {
                    let mut combined = vec![Pat::Literal(literal)];
                    if witness.len() > 1 {
                        combined.extend_from_slice(&witness[1..]);
                    }
                    return Usefulness {
                        useful: true,
                        witness: Some(combined),
                    };
                }
            } else {
                return result;
            }
        }
    }

    Usefulness {
        useful: false,
        witness: None,
    }
}
