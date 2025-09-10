use crate::{
    diagnostic::Diagnostic,
    parse::{CodeSpan, token::TokenVariant},
};

#[derive(Clone, Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub span: CodeSpan,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
    pub symbol_id: Option<usize>,
}

#[derive(Clone, Debug)]
pub enum NodeKind {
    Program {
        children: Vec<Node>,
    },
    Import {
        package: String,
        module: String,
        symbol: String,
        top_level_types: Vec<String>,
        top_level: Vec<String>,
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
    FnType {
        args: Vec<Node>,
        return_type: Option<Box<Node>>,
    },
    TupleType {
        elements: Vec<Node>,
    },

    FnDecl {
        public: bool,
        host: bool,
        symbol: String,
        return_type: Option<Box<Node>>,
        args: Vec<(Box<Node>, Option<Box<Node>>, CodeSpan)>,
        expr: Option<Box<Node>>,
    },
    ConstDecl {
        public: bool,
        symbol: Box<Node>,
        const_type: Option<Box<Node>>,
        expr: Box<Node>,
    },
    Expr {
        expr: ExprKind,
    },
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Negate,
    Not,
    BitNot,
    MonadBind,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Exponent,
    Equal,
    NotEqual,
    LessThan,
    LessThanEquiv,
    GreaterThan,
    GreaterThanEquiv,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    //Assign,
    Pipeline,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    IntLit(i64),
    RealLit {
        value: f64,
        places: usize,
    },
    ListLit(Vec<Node>),
    MapLit(Vec<(Node, Node)>),
    StringLit(String),
    TupleLit(Vec<Node>),
    Symbol {
        name: String,
    },
    LetBinding {
        symbols: Box<Node>,
        symbol_type: Option<Box<Node>>,
        expr: Box<Node>,
    },
    FieldAccess {
        expr: Box<Node>,
        field: String,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Node>,
    },
    BinaryOp {
        left: Box<Node>,
        op: BinaryOp,
        right: Box<Node>,
    },
    Match {
        expr: Box<Node>,
        arms: Vec<(Node, Node)>,
    },
    FnCall {
        callee: Box<Node>,
        //optionally named
        args: Vec<(Option<String>, Node)>,
    },
    Closure {
        args: Vec<(Box<Node>, Option<Box<Node>>, CodeSpan)>,
        return_type: Option<Box<Node>>,
        expr: Box<Node>,
    },
    For {
        binding: String,
        start: Box<Node>,
        end: Box<Node>,
        body: Box<Node>,
    },
    IndexAccess {
        expr: Box<Node>,
        index: Box<Node>,
    },
    IfElse {
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Option<Box<Node>>,
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
            symbol_id: None,
            errors: vec![],
            warnings: vec![],
        }
    }

    pub fn all_errors(&self) -> Vec<Diagnostic> {
        let mut errs = vec![];
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
        use NodeKind::*;
        match self {
            Program { children } => {
                for child in children {
                    child.collect_diagnostics(errs, collect_errors);
                }
            }
            Import { .. } => {}
            TypeAlias { .. } => {}
            TypeDecl { variants, .. } => {
                for variant in variants {
                    variant.collect_diagnostics(errs, collect_errors);
                }
            }
            TypeConstructor { fields, .. } => {
                for (_, field_type, _) in fields {
                    field_type.collect_diagnostics(errs, collect_errors);
                }
            }
            Type { type_vars, .. } => {
                for type_var in type_vars {
                    type_var.collect_diagnostics(errs, collect_errors);
                }
            }
            FnType {
                args: params,
                return_type,
            } => {
                for param in params {
                    param.collect_diagnostics(errs, collect_errors);
                }
                if let Some(return_type) = return_type {
                    return_type.collect_diagnostics(errs, collect_errors);
                }
            }

            TupleType { elements } => {
                for elem in elements {
                    elem.collect_diagnostics(errs, collect_errors);
                }
            }

            FnDecl { expr, .. } => {
                if let Some(expr) = expr {
                    expr.collect_diagnostics(errs, collect_errors);
                }
            }

            ConstDecl {
                const_type, expr, ..
            } => {
                if let Some(const_type) = const_type {
                    const_type.collect_diagnostics(errs, collect_errors);
                }
                expr.collect_diagnostics(errs, collect_errors);
            }

            Expr { expr } => {
                expr.collect_diagnostics(errs, collect_errors);
            }
        }
    }
}

impl ExprKind {
    pub fn collect_diagnostics(&self, errs: &mut Vec<Diagnostic>, collect_errors: bool) {
        use ExprKind::*;
        match self {
            IntLit(_) => {}
            RealLit { .. } => {}
            ListLit(elements) => {
                for elem in elements {
                    elem.collect_diagnostics(errs, collect_errors);
                }
            }
            MapLit(pairs) => {
                for (key, value) in pairs {
                    key.collect_diagnostics(errs, collect_errors);
                    value.collect_diagnostics(errs, collect_errors);
                }
            }
            StringLit(_) => {}
            Symbol { .. } => {}
            FieldAccess { expr, .. } => {
                expr.collect_diagnostics(errs, collect_errors);
            }
            IndexAccess { expr, index } => {
                expr.collect_diagnostics(errs, collect_errors);
                index.collect_diagnostics(errs, collect_errors);
            }
            UnaryOp { expr, .. } => {
                expr.collect_diagnostics(errs, collect_errors);
            }
            BinaryOp { left, right, .. } => {
                left.collect_diagnostics(errs, collect_errors);
                right.collect_diagnostics(errs, collect_errors);
            }
            FnCall { callee, args } => {
                callee.collect_diagnostics(errs, collect_errors);
                for (_, arg) in args {
                    arg.collect_diagnostics(errs, collect_errors);
                }
            }
            LetBinding { expr, .. } => {
                expr.collect_diagnostics(errs, collect_errors);
            }
            For {
                start, end, body, ..
            } => {
                start.collect_diagnostics(errs, collect_errors);
                end.collect_diagnostics(errs, collect_errors);
                body.collect_diagnostics(errs, collect_errors);
            }
            IfElse {
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
            Match { expr, arms } => {
                expr.collect_diagnostics(errs, collect_errors);
                for (pattern, arm_body) in arms {
                    pattern.collect_diagnostics(errs, collect_errors);
                    arm_body.collect_diagnostics(errs, collect_errors);
                }
            }
            Block(statements) => {
                for stmt in statements {
                    stmt.collect_diagnostics(errs, collect_errors);
                }
            }
            Discard => {}
            TupleLit(elements) => {
                for elem in elements {
                    elem.collect_diagnostics(errs, collect_errors);
                }
            }
            Closure {
                args: params,
                return_type,
                expr,
            } => {
                for (_, param_type, _) in params {
                    if let Some(param_type) = param_type {
                        param_type.collect_diagnostics(errs, collect_errors);
                    }
                }
                if let Some(return_type) = return_type {
                    return_type.collect_diagnostics(errs, collect_errors);
                }
                expr.collect_diagnostics(errs, collect_errors);
            }
        }
    }
}

pub fn get_unary_op(tok: TokenVariant) -> Option<UnaryOp> {
    use TokenVariant::*;
    match tok {
        Minus => Some(UnaryOp::Negate),
        Not => Some(UnaryOp::Not),
        BitNot => Some(UnaryOp::BitNot),
        _ => None,
    }
}

pub fn get_binary_op(tok: TokenVariant) -> Option<BinaryOp> {
    use TokenVariant::*;
    match tok {
        Plus => Some(BinaryOp::Add),
        Minus => Some(BinaryOp::Subtract),
        Star => Some(BinaryOp::Multiply),
        Slash => Some(BinaryOp::Divide),
        Percent => Some(BinaryOp::Modulus),
        Equiv => Some(BinaryOp::Equal),
        NotEquiv => Some(BinaryOp::NotEqual),
        LessThan => Some(BinaryOp::LessThan),
        LessThanEquiv => Some(BinaryOp::LessThanEquiv),
        GreaterThan => Some(BinaryOp::GreaterThan),
        GreaterThanEquiv => Some(BinaryOp::GreaterThanEquiv),
        And => Some(BinaryOp::And),
        Or => Some(BinaryOp::Or),
        BitAnd => Some(BinaryOp::BitAnd),
        BitOr => Some(BinaryOp::BitOr),
        BitXor => Some(BinaryOp::BitXor),
        //Assign => Some(BinaryOp::Assign),
        Pipeline => Some(BinaryOp::Pipeline),
        _ => None,
    }
}
