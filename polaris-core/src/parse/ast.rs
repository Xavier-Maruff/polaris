use std::fmt;

use crate::parse::diagnostic::Diagnostic;

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub variant: Variant,
    pub warnings: Option<Vec<Diagnostic>>,
    pub errors: Option<Vec<Diagnostic>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variant {
    Program {
        children: Vec<Node>,
    },
    Directive {
        name: String,
        args: Vec<Node>,
    },
    Expr(ExprNode),
    StructDecl {
        ident: Box<Node>,
        fields: Vec<(String, Node)>,
    },
    FuncDecl {
        ident: Option<Box<Node>>,
        capture_list: Option<Vec<Node>>,
        params: Vec<Node>,
        return_type: Option<Box<Node>>,
        body: Option<Box<Node>>,
    },
    VarDecl {
        name: String,
        var_type: Option<Box<Node>>,
        modifiable: bool,
        initialiser: Option<Box<Node>>,
    },
    EnumDecl {
        ident: Box<Node>,
        variants: Vec<(String, Option<Node>)>,
    },
    InterfaceDecl {
        name: String,
        interface: Vec<Node>,
    },
    ImplDecl {
        interface: Box<Node>,
        target: Box<Node>,
        methods: Vec<Node>,
    },
    Block {
        children: Vec<Node>,
    },
    Return {
        value: Option<Box<Node>>,
    },
    Yield {
        value: Option<Box<Node>>,
    },
    Break,
    Continue,
    Assert {
        condition: Box<Node>,
        messages: Vec<Node>,
    },
    If {
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Option<Box<Node>>,
    },
    For {
        variant: ForVariant,
        body: Box<Node>,
    },
    TypeDecl {
        ident: Box<Node>,
        alias_of: Box<Node>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForVariant {
    ForWhile { condition: Box<Node> },
    ForIter { ident: String, iterable: Box<Node> },
    ForInfinite,
}

impl fmt::Display for ForVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ForVariant::ForWhile { condition } => write!(f, "{}", condition),
            ForVariant::ForIter { ident, iterable } => write!(f, "{} in {}", ident, iterable),
            ForVariant::ForInfinite => Ok(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemoryMode {
    Auto,
    Ref,
    Weak,
}

impl fmt::Display for MemoryMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemoryMode::Ref => write!(f, "ref"),
            MemoryMode::Weak => write!(f, "weak"),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprNode {
    String(String),
    Ident {
        name: String,
        qualifier: bool,
        type_args: Vec<Node>,
    },
    QualifiedIdent {
        namespaces: Vec<Node>,
        name: String,
        type_args: Vec<Node>,
        memory_mode: MemoryMode,
    },
    IntLit(i64),
    FloatLit(f64),
    CharLit(String),
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    StructLit {
        struct_ident: Option<Box<Node>>,
        fields: Vec<(String, Node)>,
    },
    Call {
        callee: Box<Node>,
        args: Vec<Node>,
    },
    ListLit {
        elements: Vec<Node>,
    },
    Match {
        subject: Box<Node>,
        cases: Vec<(Node, Node)>,
    },
    FieldAccess {
        base: Box<Node>,
        field: Box<Node>,
    },
    Index {
        base: Box<Node>,
        index: Box<Node>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<Node>,
    },
}

impl ExprNode {
    pub fn warnings(&self) -> Vec<Diagnostic> {
        match self {
            ExprNode::Ident { type_args, .. } => {
                type_args.iter().flat_map(|n| n.warnings()).collect()
            }
            ExprNode::QualifiedIdent {
                namespaces,
                type_args,
                ..
            } => {
                let mut warnings = namespaces
                    .iter()
                    .flat_map(|n| n.warnings())
                    .collect::<Vec<_>>();
                warnings.extend(type_args.iter().flat_map(|n| n.warnings()));
                warnings
            }
            _ => Vec::new(),
        }
    }

    pub fn errors(&self) -> Vec<Diagnostic> {
        match self {
            ExprNode::Ident { type_args, .. } => {
                type_args.iter().flat_map(|n| n.errors()).collect()
            }
            ExprNode::QualifiedIdent {
                namespaces,
                type_args,
                ..
            } => {
                let mut errors = namespaces
                    .iter()
                    .flat_map(|n| n.errors())
                    .collect::<Vec<_>>();
                errors.extend(type_args.iter().flat_map(|n| n.errors()));
                errors
            }
            _ => Vec::new(),
        }
    }
}

impl fmt::Display for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprNode::String(s) => write!(f, "\"{}\"", s),
            ExprNode::Ident { name, .. } => write!(f, "{}", name),
            ExprNode::Index { base, index } => {
                write!(f, "{}[{}]", base, index)
            }
            ExprNode::UnaryOp { op, operand } => {
                write!(f, "{}{}", op, operand)
            }
            ExprNode::FieldAccess { base, field } => {
                write!(f, "{}.{}", base, field)
            }
            ExprNode::QualifiedIdent {
                namespaces,
                name,
                type_args,
                memory_mode,
                ..
            } => {
                write!(f, "{} ", memory_mode)?;
                for ns in namespaces {
                    write!(f, "{}::", ns)?;
                }
                write!(f, "{}", name)?;
                if !type_args.is_empty() {
                    let type_arg_str = type_args
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "::<{}>", type_arg_str)?;
                }
                Ok(())
            }
            ExprNode::Match { subject, cases } => {
                write!(f, "match {} {{\n", subject)?;
                for (pattern, body) in cases {
                    write!(f, "  {} => {},\n", pattern, body)?;
                }
                write!(f, "}}\n")
            }
            ExprNode::StructLit {
                struct_ident,
                fields,
            } => {
                write!(f, "struct")?;
                if let Some(ident) = struct_ident {
                    write!(f, "::{}", ident)?;
                }
                write!(f, " {{\n")?;
                for (i, (name, field)) in fields.iter().enumerate() {
                    write!(f, "{}: {},\n", name, field)?;
                }
                write!(f, " }}")
            }
            ExprNode::IntLit(i) => write!(f, "{}", i),
            ExprNode::FloatLit(v) => write!(f, "{}", v),
            ExprNode::CharLit(c) => write!(f, "'{}'", c),
            ExprNode::BinaryOp { op, lhs, rhs } => {
                write!(f, "({} {} {})", lhs, op, rhs)
            }
            ExprNode::Call { callee, args } => {
                write!(f, "{}(", callee)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            ExprNode::ListLit { elements } => {
                write!(
                    f,
                    "[{}]",
                    elements
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equiv,
    NotEquiv,
    LessThan,
    LessThanEquiv,
    GreaterThan,
    GreaterThanEquiv,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    And,
    Or,
    Not,
    Assign,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
    Not,
    BitNot,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::BitNot => write!(f, "~"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Subtract => write!(f, "-"),
            BinaryOp::Multiply => write!(f, "*"),
            BinaryOp::Divide => write!(f, "/"),
            BinaryOp::Modulo => write!(f, "%"),
            BinaryOp::Equiv => write!(f, "=="),
            BinaryOp::NotEquiv => write!(f, "!="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::LessThanEquiv => write!(f, "<="),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::GreaterThanEquiv => write!(f, ">="),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::BitXor => write!(f, "^"),
            BinaryOp::BitNot => write!(f, "~"),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
            BinaryOp::Not => write!(f, "!"),
            BinaryOp::Assign => write!(f, "="),
        }
    }
}

impl Node {
    pub fn new(variant: Variant) -> Self {
        Self {
            variant,
            warnings: None,
            errors: None,
        }
    }

    pub fn add_error(&mut self, error: Diagnostic) {
        if self.errors.is_none() {
            self.errors = Some(Vec::new());
        }
        self.errors.as_mut().unwrap().push(error);
    }

    pub fn add_warning(&mut self, warning: Diagnostic) {
        if self.warnings.is_none() {
            self.warnings = Some(Vec::new());
        }
        self.warnings.as_mut().unwrap().push(warning);
    }

    pub fn warnings(&self) -> Vec<Diagnostic> {
        let mut warnings = match &self.warnings {
            Some(w) => w.clone(),
            None => Vec::new(),
        };
        match &self.variant {
            Variant::Expr(expr) => warnings.extend(expr.warnings()),
            Variant::Directive { args, .. } => {
                for arg in args {
                    warnings.extend(arg.warnings());
                }
            }
            Variant::StructDecl { fields, .. } => {
                for (_, field) in fields {
                    warnings.extend(field.warnings());
                }
            }
            _ => {}
        }
        warnings
    }

    pub fn errors(&self) -> Vec<Diagnostic> {
        let mut errors = match &self.errors {
            Some(e) => e.clone(),
            None => Vec::new(),
        };

        match &self.variant {
            Variant::Expr(expr) => errors.extend(expr.errors()),
            Variant::Directive { args, .. } => {
                for arg in args {
                    errors.extend(arg.errors());
                }
            }
            Variant::StructDecl { fields, .. } => {
                for (_, field) in fields {
                    errors.extend(field.errors());
                }
            }
            _ => {}
        }
        errors
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.variant {
            Variant::Program { children } => {
                for child in children {
                    write!(f, "{}", child)?;
                }
                Ok(())
            }
            Variant::Block { children } => {
                write!(f, "{{\n")?;
                for (_i, child) in children.iter().enumerate() {
                    write!(f, "  {};\n", child)?;
                }
                write!(f, " }}\n")
            }
            Variant::TypeDecl { ident, alias_of } => {
                write!(f, "type {} = {};\n", ident, alias_of)
            }
            Variant::Break => write!(f, "break;\n"),
            Variant::Continue => write!(f, "continue;\n"),
            Variant::Assert {
                condition,
                messages,
            } => {
                write!(f, "assert({}", condition)?;
                for message in messages {
                    write!(f, ", {}", message)?;
                }
                write!(f, ");\n")
            }
            Variant::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {} ", condition)?;
                write!(f, "{}", then_branch)?;
                if let Some(else_branch) = else_branch {
                    write!(f, "else {}", else_branch)?;
                }
                write!(f, "\n")
            }
            Variant::EnumDecl { ident, variants } => {
                write!(f, "enum {} {{\n ", ident)?;
                for (_i, (name, value)) in variants.iter().enumerate() {
                    write!(f, "  {}", name)?;
                    if let Some(val) = value {
                        write!(f, "({})", val)?;
                    }
                    write!(f, ",\n")?;
                }
                write!(f, "\n}}\n")
            }
            Variant::Return { value } => {
                write!(f, "return")?;
                if let Some(val) = value {
                    write!(f, " {}", val)?;
                }
                Ok(())
            }
            Variant::Yield { value } => {
                write!(f, "yield")?;
                if let Some(val) = value {
                    write!(f, " {}", val)?;
                }
                Ok(())
            }
            Variant::For { variant, body } => {
                write!(f, "for {} ", variant)?;
                write!(f, "{}", body)?;
                Ok(())
            }
            Variant::Directive { name, args } => {
                write!(f, "@{}", name)?;
                if !args.is_empty() {
                    write!(
                        f,
                        "({})",
                        args.iter()
                            .map(|a| a.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }
                write!(f, ";\n")
            }
            Variant::Expr(expr) => write!(f, "{}", expr),
            Variant::StructDecl { ident, fields } => {
                write!(f, "struct {} {{\n ", ident)?;
                for (name, field) in fields {
                    write!(f, "  {}: {},\n", name, field)?;
                }
                write!(f, "}}\n")
            }

            Variant::FuncDecl {
                ident,
                params,
                return_type,
                body,
                capture_list,
            } => {
                write!(f, "func")?;
                match ident {
                    Some(ident) => write!(f, "{}", ident)?,
                    None => {}
                }

                match capture_list {
                    Some(capture_list) => {
                        write!(
                            f,
                            "[{}]",
                            capture_list
                                .iter()
                                .map(|c| c.to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )?;
                    }
                    None => {}
                }

                write!(f, "(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ")")?;
                if let Some(ret) = return_type {
                    write!(f, ": {}", ret)?;
                }
                if let Some(b) = body {
                    write!(f, " {}", b)?;
                } else {
                    write!(f, ";\n")?;
                }
                Ok(())
            }
            Variant::VarDecl {
                name,
                var_type,
                initialiser,
                modifiable,
            } => {
                write!(f, "let ")?;
                if *modifiable {
                    write!(f, "mod ")?;
                }
                write!(f, "{}", name)?;
                if var_type.is_some() {
                    write!(f, ": {}", var_type.as_ref().unwrap())?;
                }
                if let Some(init) = initialiser {
                    write!(f, " = {}", init)?;
                }
                Ok(())
            }
            Variant::InterfaceDecl {
                name,
                interface: functions,
            } => {
                write!(f, "interface {}", name)?;
                write!(f, " {{\n ")?;
                for (_, func) in functions.iter().enumerate() {
                    write!(f, "{}", func)?;
                }
                write!(f, " }}\n")
            }
            Variant::ImplDecl {
                interface,
                target,
                methods,
            } => {
                write!(f, "impl {}", interface)?;
                write!(f, " for {}", target)?;
                write!(f, " {{\n ")?;
                for (_i, method) in methods.iter().enumerate() {
                    writeln!(f, "{}", method)?;
                }
                write!(f, " }}\n")
            }
        }
    }
}
