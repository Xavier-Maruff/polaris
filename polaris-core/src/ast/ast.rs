use std::fmt;

use crate::diagnostic::Diagnostic;
use crate::module::ModuleId;
use crate::parse::CodeSpan;
use crate::symbol::SymbolId;
use crate::{collect_expr_diagnostics, collect_node_diagnostics};

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub variant: Variant,
    pub scope_id: Option<SymbolId>,
    pub warnings: Option<Vec<Diagnostic>>,
    pub errors: Option<Vec<Diagnostic>>,
    pub span: CodeSpan,
    pub export: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variant {
    Failed,
    Program {
        file: String,
        module_id: Option<ModuleId>,
        children: Vec<Node>,
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
        is_async: bool,
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
        ident: Box<Node>,
        interface: Vec<Node>,
    },
    ImplDecl {
        interface: Option<Box<Node>>,
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
    ActorDecl {
        ident: Box<Node>,
        methods: Vec<Node>,
        fields: Vec<(String, Node)>,
    },
    Import {
        module_id: ModuleId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForVariant {
    ForWhile {
        condition: Box<Node>,
    },
    ForIter {
        ident: Box<Node>,
        iterable: Box<Node>,
    },
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
        namespaces: Vec<String>,
        name: String,
        type_args: Vec<Node>,
        memory_mode: MemoryMode,
        id: Option<SymbolId>,
    },
    Directive {
        ident: Box<Node>,
        args: Vec<Node>,
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
    ActorLit {
        actor_ident: Box<Node>,
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
    //TODO: bit shift operators
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
    Deref,
    BindMonad,
    Await,
    Block,
    FusedAssign,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::BitNot => write!(f, "~"),
            UnaryOp::Deref => write!(f, "*"),
            UnaryOp::BindMonad => write!(f, "?"),
            UnaryOp::Await => write!(f, "await "),
            UnaryOp::Block => write!(f, "block "),
            UnaryOp::FusedAssign => write!(f, "="),
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

impl ExprNode {
    pub fn get_qualified_ident_str(&self) -> Option<String> {
        match self {
            ExprNode::Ident { name, .. } => Some(name.clone()),
            ExprNode::QualifiedIdent {
                name, namespaces, ..
            } => {
                let mut idents = vec![];
                for ns in namespaces {
                    idents.push(ns.clone());
                }
                idents.push(name.clone());
                Some(idents.join("::"))
            }
            ExprNode::Directive { ident, .. } => ident.get_qualified_ident_str(),
            _ => None,
        }
    }

    pub fn warnings(&self) -> Vec<Diagnostic> {
        let mut warnings = Vec::new();
        collect_expr_diagnostics!(self, warnings, warnings);
        warnings
    }

    pub fn errors(&self) -> Vec<Diagnostic> {
        let mut errors = Vec::new();
        collect_expr_diagnostics!(self, errors, errors);
        errors
    }
}

impl Node {
    pub fn new(variant: Variant) -> Self {
        Self {
            variant,
            span: CodeSpan::new(0, 0),
            warnings: None,
            errors: None,
            export: false,
            scope_id: None,
        }
    }

    pub fn new_with_span(variant: Variant, span: CodeSpan) -> Self {
        Self {
            variant,
            span: span,
            warnings: None,
            errors: None,
            export: false,
            scope_id: None,
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
        collect_node_diagnostics!(self, warnings, warnings);
        warnings
    }

    pub fn errors(&self) -> Vec<Diagnostic> {
        let mut errors = match &self.errors {
            Some(e) => e.clone(),
            None => Vec::new(),
        };

        collect_node_diagnostics!(self, errors, errors);
        errors
    }

    pub fn get_qualified_ident_str(&self) -> Option<String> {
        match &self.variant {
            Variant::FuncDecl {
                ident: Some(ident), ..
            } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_qualified_ident_str()
                } else {
                    None
                }
            }
            Variant::ActorDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_qualified_ident_str()
                } else {
                    None
                }
            }
            Variant::StructDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_qualified_ident_str()
                } else {
                    None
                }
            }
            Variant::TypeDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_qualified_ident_str()
                } else {
                    None
                }
            }
            Variant::EnumDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_qualified_ident_str()
                } else {
                    None
                }
            }
            Variant::InterfaceDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_qualified_ident_str()
                } else {
                    None
                }
            }
            Variant::VarDecl { name, .. } => Some(name.clone()),
            Variant::Expr(node) => node.get_qualified_ident_str(),
            _ => None,
        }
    }

    pub fn is_expr(&self) -> bool {
        matches!(self.variant, Variant::Expr(_))
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.variant {
            Variant::Failed => write!(f, "/* <failed> */\n"),
            Variant::Program { children, .. } => {
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
            Variant::Import { module_id } => {
                write!(f, "@import(Module[{}])", module_id)
            }
            Variant::ActorDecl {
                ident,
                methods,
                fields,
            } => {
                write!(f, "actor {} {{\n", ident)?;
                if !fields.is_empty() {
                    for (name, field) in fields {
                        write!(f, "  {}: {},\n", name, field)?;
                    }
                } else {
                    write!(f, ";")?;
                }
                if !methods.is_empty() {
                    write!(f, "\n")?;
                    for method in methods {
                        write!(f, "{}", method)?;
                    }
                }
                write!(f, "}}\n")?;
                Ok(())
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
                is_async,
            } => {
                if *is_async {
                    write!(f, "async ")?;
                }
                write!(f, "func")?;
                match ident {
                    Some(ident) => write!(f, " {}", ident)?,
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
                ident,
                interface: functions,
            } => {
                write!(f, "interface {}", ident)?;
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
                write!(f, "impl")?;
                if interface.is_some() {
                    write!(f, " {}", interface.as_ref().unwrap())?;
                }
                write!(f, " {}", target)?;
                write!(f, " {{\n ")?;
                for (_i, method) in methods.iter().enumerate() {
                    writeln!(f, "{}", method)?;
                }
                write!(f, " }}\n")
            }
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
            ExprNode::ActorLit {
                actor_ident,
                fields,
            } => {
                write!(f, "actor::{}", actor_ident)?;
                if !fields.is_empty() {
                    write!(f, " {{\n")?;
                    for (name, field) in fields {
                        write!(f, "  {}: {},\n", name, field)?;
                    }
                    write!(f, " }}")?;
                } else {
                    write!(f, ";")?;
                }
                Ok(())
            }
            ExprNode::Directive { ident, args } => {
                write!(f, "@{}", ident)?;
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
                Ok(())
            }
            ExprNode::QualifiedIdent {
                namespaces,
                name,
                type_args,
                memory_mode,
                ..
            } => {
                if memory_mode != &MemoryMode::Auto {
                    write!(f, "{} ", memory_mode)?;
                }
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
                for (name, field) in fields.iter() {
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
