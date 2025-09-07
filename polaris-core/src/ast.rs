use crate::{diagnostic::Diagnostic, parse::CodeSpan};

pub struct Node {
    pub kind: NodeKind,
    pub span: CodeSpan,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
}

pub enum NodeKind {
    Program {
        children: Vec<Node>,
    },
    Import {
        path_parts: Vec<String>,
        symbol: String,
    },
    TypeAlias {
        public: bool,
        alias: Box<Node>,
        actual: Box<Node>,
    },
    TypeDecl {
        public: bool,
        symbol: String,
        type_vars: Vec<String>,
        variants: Vec<Node>,
    },
    TypeConstructor {
        symbol: String,
        //(name, type)
        fields: Vec<(Option<String>, Node, CodeSpan)>,
    },
    Type {
        nocrypt: bool,
        symbol: String,
        type_vars: Vec<Node>,
    },
    Fn {
        public: bool,
        host: bool,
        symbol: Option<String>,
        //(name, type)
        params: Vec<(String, Option<Node>, CodeSpan)>,
        return_type: Option<Box<Node>>,
        body: Option<Box<Node>>,
    },
    ConstDecl {
        public: bool,
        symbol: String,
        const_type: Box<Node>,
        value: Box<Node>,
    },
    Expr {
        expr: ExprKind,
    },
}

pub enum ExprKind {
    IntLit(i64),
    FloatLit(f64),
    BoolLit(bool),
    ListLit(Vec<Node>),
    MapLit(Vec<(Node, Node)>),
    StringLit(String),
    Binding(String),
    FnCall {
        callee: Box<Node>,
        //optionally named
        args: Vec<(Option<String>, Node)>,
    },
    ForLoop {
        binding: String,
        start: Box<Node>,
        end: Box<Node>,
        body: Box<Node>,
    },
    IfElse {
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Option<Box<Node>>,
    },
    Match {
        to_match: Box<Node>,
        arms: Vec<(Node, Node)>,
    },
    Block(Vec<Node>),
    //used to discard bindings and patterns
    Discard,
}

impl Node {
    pub fn new(kind: NodeKind, span: CodeSpan) -> Self {
        Self {
            kind,
            span,
            errors: vec![],
            warnings: vec![],
        }
    }

    pub fn all_errors(&self) -> Vec<Diagnostic> {
        let mut errs = self.errors.clone();
        self.collect_diagnostics(&mut errs, true);
        errs
    }

    pub fn all_warnings(&self) -> Vec<Diagnostic> {
        let mut warns = vec![];
        self.collect_diagnostics(&mut warns, false);
        warns
    }

    pub fn collect_diagnostics(&self, buf: &mut Vec<Diagnostic>, collect_errors: bool) {
        if collect_errors {
            buf.extend(self.errors.iter().cloned());
        } else {
            buf.extend(self.warnings.iter().cloned());
        }
        self.kind.collect_diagnostics(buf, collect_errors);
    }

    pub fn add_error(&mut self, err: Diagnostic) {
        self.errors.push(err);
    }

    pub fn add_warning(&mut self, warn: Diagnostic) {
        self.warnings.push(warn);
    }
}

impl NodeKind {
    pub fn collect_diagnostics(&self, errs: &mut Vec<Diagnostic>, collect_errors: bool) {
        match self {
            NodeKind::Program { children } => {
                for child in children {
                    child.collect_diagnostics(errs, collect_errors);
                }
            }
            NodeKind::Import { .. } => {}
            NodeKind::TypeAlias { .. } => {}
            NodeKind::TypeDecl { variants, .. } => {
                for variant in variants {
                    variant.collect_diagnostics(errs, collect_errors);
                }
            }
            NodeKind::TypeConstructor { fields, .. } => {
                for (_, field_type, _) in fields {
                    field_type.collect_diagnostics(errs, collect_errors);
                }
            }
            NodeKind::Type { type_vars, .. } => {
                for type_var in type_vars {
                    type_var.collect_diagnostics(errs, collect_errors);
                }
            }
            NodeKind::Fn {
                params,
                return_type,
                body,
                ..
            } => {
                for (_, param_type, _) in params {
                    if let Some(param_type) = param_type {
                        param_type.collect_diagnostics(errs, collect_errors);
                    }
                }

                if let Some(return_type) = return_type {
                    return_type.collect_diagnostics(errs, collect_errors);
                }

                if let Some(body) = body {
                    body.collect_diagnostics(errs, collect_errors);
                }
            }
            NodeKind::ConstDecl {
                const_type, value, ..
            } => {
                const_type.collect_diagnostics(errs, collect_errors);
                value.collect_diagnostics(errs, collect_errors);
            }
            NodeKind::Expr { expr } => {
                expr.collect_diagnostics(errs, collect_errors);
            }
        }
    }
}

impl ExprKind {
    pub fn collect_diagnostics(&self, errs: &mut Vec<Diagnostic>, collect_errors: bool) {
        match self {
            ExprKind::IntLit(_) => {}
            ExprKind::FloatLit(_) => {}
            ExprKind::BoolLit(_) => {}
            ExprKind::ListLit(elements) => {
                for elem in elements {
                    elem.collect_diagnostics(errs, collect_errors);
                }
            }
            ExprKind::MapLit(pairs) => {
                for (key, value) in pairs {
                    key.collect_diagnostics(errs, collect_errors);
                    value.collect_diagnostics(errs, collect_errors);
                }
            }
            ExprKind::StringLit(_) => {}
            ExprKind::Binding(_) => {}
            ExprKind::FnCall { callee, args } => {
                callee.collect_diagnostics(errs, collect_errors);
                for (_, arg) in args {
                    arg.collect_diagnostics(errs, collect_errors);
                }
            }
            ExprKind::ForLoop {
                start, end, body, ..
            } => {
                start.collect_diagnostics(errs, collect_errors);
                end.collect_diagnostics(errs, collect_errors);
                body.collect_diagnostics(errs, collect_errors);
            }
            ExprKind::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.collect_diagnostics(errs, collect_errors);
                then_branch.collect_diagnostics(errs, collect_errors);
                if let Some(else_branch) = else_branch {
                    else_branch.collect_diagnostics(errs, collect_errors);
                }
            }
            ExprKind::Match { to_match, arms } => {
                to_match.collect_diagnostics(errs, collect_errors);
                for (pattern, arm_body) in arms {
                    pattern.collect_diagnostics(errs, collect_errors);
                    arm_body.collect_diagnostics(errs, collect_errors);
                }
            }
            ExprKind::Block(statements) => {
                for stmt in statements {
                    stmt.collect_diagnostics(errs, collect_errors);
                }
            }
            ExprKind::Discard => {}
        }
    }
}
