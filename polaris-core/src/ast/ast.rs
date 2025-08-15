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
    pub type_id: Option<SymbolId>,
    pub warnings: Option<Vec<Diagnostic>>,
    pub errors: Option<Vec<Diagnostic>>,
    pub span: CodeSpan,
    pub export: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeIdent {
    pub body_symbol_id: SymbolId,
    pub params: Vec<TypeIdent>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variant {
    Failed,
    Dead,
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
        is_extern: bool,
    },
    VarDecl {
        name: String,
        id: Option<SymbolId>,
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
    Defer {
        body: Box<Node>,
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
    Empty,
    String(String),
    Ident {
        name: String,
        type_args: Vec<Node>,
        memory_mode: MemoryMode,
        id: Option<SymbolId>,
        is_directive: bool,
        is_type: bool,
    },
    BoolLit(bool),
    IntLit(i64),
    FloatLit(f64),
    CharLit(String),
    TupleLit {
        elements: Vec<Node>,
    },
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
        field: String,
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
    Ref,
    Deref,
    BindMonad,
    Await,
    Block,
    FusedAssign,
    Spread,
    Spawn,
}

impl Variant {
    pub fn user_friendly_name(&self) -> String {
        match self {
            Variant::Dead => "dead node".to_string(),
            Variant::Failed => "failed node".to_string(),
            Variant::Program { .. } => "program".to_string(),
            Variant::Expr(expr) => expr.user_friendly_name(),
            Variant::StructDecl { ident, .. } => {
                if let Some(ident) = ident.get_ident() {
                    format!("struct `{}`", ident)
                } else {
                    "struct".to_string()
                }
            }
            Variant::FuncDecl { ident, .. } => {
                if let Some(ident) = ident {
                    format!(
                        "function `{}`",
                        ident.get_ident().unwrap_or("unknown".to_string())
                    )
                } else {
                    "function".to_string()
                }
            }
            Variant::VarDecl { name, .. } => format!("variable `{}`", name),
            Variant::EnumDecl { ident, .. } => {
                if let Some(ident) = ident.get_ident() {
                    format!("enum `{}`", ident)
                } else {
                    "enum".to_string()
                }
            }
            Variant::InterfaceDecl { ident, .. } => {
                if let Some(ident) = ident.get_ident() {
                    format!("interface `{}`", ident)
                } else {
                    "interface".to_string()
                }
            }
            Variant::ImplDecl { target, .. } => {
                if let Some(ident) = target.get_ident() {
                    format!("implementation for `{}`", ident)
                } else {
                    "implementation".to_string()
                }
            }
            Variant::Block { .. } => "block".to_string(),
            Variant::Return { value } => {
                if let Some(val) = value {
                    format!("return with value `{}`", val)
                } else {
                    "return".to_string()
                }
            }
            Variant::Yield { value } => {
                if let Some(val) = value {
                    format!("yield with value `{}`", val)
                } else {
                    "yield".to_string()
                }
            }
            Variant::Break => "break statement".to_string(),
            Variant::Continue => "continue statement".to_string(),
            Variant::Assert {
                condition,
                messages,
            } => {
                let mut msg = format!("assertion on `{}`", condition);
                if !messages.is_empty() {
                    msg.push_str(" with messages: ");
                    msg.push_str(
                        &messages
                            .iter()
                            .map(|m| m.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                    );
                }
                msg
            }
            Variant::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut msg = format!("if statement on `{}`", condition);
                msg.push_str(&format!(" then branch: {}", then_branch));
                if let Some(else_branch) = else_branch {
                    msg.push_str(&format!(" else branch: {}", else_branch));
                }
                msg
            }
            Variant::For { variant, body } => {
                format!("for loop with variant `{}` and body `{}`", variant, body)
            }
            Variant::TypeDecl { ident, .. } => {
                if let Some(ident) = ident.get_ident() {
                    format!("type declaration `{}`", ident)
                } else {
                    "type declaration".to_string()
                }
            }
            Variant::ActorDecl {
                ident,
                methods,
                fields,
            } => {
                let mut msg = format!(
                    "actor `{}`",
                    ident.get_ident().unwrap_or("unknown".to_string())
                );
                if !fields.is_empty() {
                    msg.push_str(" with fields");
                }
                if !methods.is_empty() {
                    msg.push_str(" and methods");
                }
                msg
            }
            Variant::Defer { body } => {
                format!("defer statement with body `{}`", body)
            }
        }
    }
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
            UnaryOp::Spread => write!(f, "..."),
            UnaryOp::Spawn => write!(f, "spawn "),
            UnaryOp::Ref => write!(f, "ref "),
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
    pub fn get_type_args(&self) -> Option<&Vec<Node>> {
        match self {
            ExprNode::Ident { type_args, .. } => Some(type_args),
            _ => None,
        }
    }
    pub fn get_ident(&self) -> Option<String> {
        match self {
            ExprNode::Ident { name, .. } => Some(name.to_string()),
            ExprNode::Call { callee, .. } => callee.get_ident(),
            _ => None,
        }
    }

    pub fn is_type_ident(&self) -> bool {
        match self {
            ExprNode::Ident { is_type, .. } => *is_type,
            _ => false,
        }
    }

    pub fn user_friendly_name(&self) -> String {
        match self {
            ExprNode::Empty => "empty expression".to_string(),
            ExprNode::String(_) => "string literal".to_string(),
            ExprNode::Ident { name, .. } => format!("identifier `{}`", name),
            ExprNode::IntLit(_) => "integer literal".to_string(),
            ExprNode::FloatLit(_) => "float literal".to_string(),
            ExprNode::BoolLit(_) => "boolean literal".to_string(),
            ExprNode::CharLit(_) => "character literal".to_string(),
            ExprNode::TupleLit { .. } => "tuple literal".to_string(),
            ExprNode::BinaryOp { op, .. } => format!("binary operation `{}`", op),
            ExprNode::StructLit { struct_ident, .. } => {
                if let Some(ident) = struct_ident {
                    format!("struct literal `{}`", ident)
                } else {
                    "struct literal".to_string()
                }
            }
            ExprNode::ActorLit {
                actor_ident,
                fields,
                ..
            } => {
                if fields.is_empty() {
                    format!("actor literal `{}`", actor_ident)
                } else {
                    format!("actor literal `{}` with fields", actor_ident)
                }
            }
            ExprNode::Call { callee, .. } => {
                if let Some(ident) = callee.get_ident() {
                    format!("function call to `{}`", ident)
                } else {
                    "function call".to_string()
                }
            }
            ExprNode::ListLit { .. } => "list literal".to_string(),
            ExprNode::Match { subject, .. } => {
                format!("match expression on `{}`", subject)
            }
            ExprNode::FieldAccess { base, field } => {
                format!("field access `{}` on `{}`", field, base)
            }
            ExprNode::Index { base, index } => {
                format!("indexing `{}` with `{}`", base, index)
            }
            ExprNode::UnaryOp { op, operand } => {
                format!("unary operation `{}` on `{}`", op, operand)
            }
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

    pub fn get_directive(&self) -> Option<String> {
        match self {
            ExprNode::Ident {
                is_directive, name, ..
            } => {
                if *is_directive {
                    Some(name.clone())
                } else {
                    None
                }
            }
            ExprNode::Call { callee, .. } => callee.get_directive(),
            _ => None,
        }
    }

    pub fn set_symbol_id(&mut self, id: SymbolId) {
        match self {
            ExprNode::Ident { id: i, .. } => *i = Some(id),
            _ => {}
        }
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
            type_id: None,
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
            type_id: None,
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

    pub fn get_type_args(&self) -> Option<&Vec<Node>> {
        match &self.variant {
            Variant::Expr(node) => node.get_type_args(),
            _ => None,
        }
    }

    pub fn get_directive(&self) -> Option<String> {
        match &self.variant {
            Variant::Expr(node) => node.get_directive(),
            _ => None,
        }
    }

    pub fn get_string(&self) -> Option<String> {
        match &self.variant {
            Variant::Expr(ExprNode::String(s)) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn get_symbol_id(&mut self) -> &mut Option<SymbolId> {
        match &mut self.variant {
            //TODO: there might be others, jsut eyeballed this wihtout thinking too much
            Variant::Expr(ExprNode::Ident { id, .. }) => id,
            Variant::EnumDecl { ident, .. }
            | Variant::StructDecl { ident, .. }
            | Variant::TypeDecl { ident, .. }
            | Variant::ActorDecl { ident, .. } => ident.get_symbol_id(),
            _ => unreachable!("Tried to get symbol id for non-symbol node"),
        }
    }

    //symbol id + type param ids
    pub fn get_symbol_ids(&self) -> Option<TypeIdent> {
        match &self.variant {
            Variant::Expr(ExprNode::Ident { id, type_args, .. }) => {
                assert!(id.is_some(), "Identifier must have a symbol id");

                let type_params = type_args
                    .iter()
                    .map(|t| t.get_symbol_ids())
                    .filter(|t| t.is_some())
                    .map(|t| t.unwrap())
                    .collect::<Vec<_>>();

                assert!(
                    type_args.len() == type_params.len(),
                    "Not all type argument symbols associated ids"
                );

                Some(TypeIdent {
                    body_symbol_id: id.unwrap(),
                    params: type_params,
                })
            }
            _ => None,
        }
    }

    pub fn get_ident(&self) -> Option<String> {
        match &self.variant {
            Variant::FuncDecl {
                ident: Some(ident), ..
            } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_ident()
                } else {
                    None
                }
            }
            Variant::ActorDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_ident()
                } else {
                    None
                }
            }
            Variant::StructDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_ident()
                } else {
                    None
                }
            }
            Variant::TypeDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_ident()
                } else {
                    None
                }
            }
            Variant::EnumDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_ident()
                } else {
                    None
                }
            }
            Variant::InterfaceDecl { ident, .. } => {
                if let Variant::Expr(node) = &ident.variant {
                    node.get_ident()
                } else {
                    None
                }
            }
            Variant::VarDecl { name, .. } => Some(name.clone()),
            Variant::Expr(node) => node.get_ident(),
            _ => None,
        }
    }

    pub fn is_expr(&self) -> bool {
        matches!(self.variant, Variant::Expr(_))
    }

    pub fn is_type_ident(&self) -> bool {
        match &self.variant {
            Variant::Expr(node) => node.is_type_ident(),
            Variant::StructDecl { .. }
            | Variant::ActorDecl { .. }
            | Variant::TypeDecl { .. }
            | Variant::EnumDecl { .. }
            | Variant::InterfaceDecl { .. } => true,
            _ => false,
        }
    }

    pub fn set_symbol_id(&mut self, id: SymbolId) {
        match &mut self.variant {
            Variant::Expr(expr) => expr.set_symbol_id(id),
            Variant::ActorDecl { ident, .. }
            | Variant::EnumDecl { ident, .. }
            | Variant::StructDecl { ident, .. }
            | Variant::TypeDecl { ident, .. }
            | Variant::InterfaceDecl { ident, .. } => {
                ident.set_symbol_id(id);
            }
            Variant::FuncDecl { ident, .. } => {
                if let Some(ident) = ident {
                    ident.set_symbol_id(id);
                }
            }
            _ => {}
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.variant {
            Variant::Dead => Ok(()),
            Variant::Failed => write!(f, "/* <failed> */\n"),
            Variant::Program { children, .. } => {
                for child in children {
                    write!(
                        f,
                        "{}{}\n",
                        if child.export { "export " } else { "" },
                        child
                    )?;
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
            Variant::Defer { body } => {
                write!(f, "defer {};\n", body)
            }
            Variant::TypeDecl { ident, alias_of } => {
                write!(f, "type {} = {};\n", ident, alias_of)
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
                write!(f, "if {} \n", condition)?;
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
                is_extern,
            } => {
                if *is_extern {
                    write!(f, "extern ")?;
                }

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
                ..
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
            ExprNode::Empty => write!(f, "()"),
            ExprNode::String(s) => write!(f, "\"{}\"", s),
            ExprNode::Index { base, index } => {
                write!(f, "{}[{}]", base, index)
            }
            ExprNode::UnaryOp { op, operand } => {
                write!(f, "{}{}", op, operand)
            }
            ExprNode::FieldAccess { base, field } => {
                write!(f, "{}.{}", base, field)
            }
            ExprNode::TupleLit { elements } => {
                write!(
                    f,
                    "({})",
                    elements
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
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

            ExprNode::Ident {
                name,
                type_args,
                memory_mode,
                id,
                is_type,
                is_directive,
                ..
            } => {
                write!(f, "[")?;

                if *is_type {
                    write!(f, "type, ")?;
                }
                if let Some(id) = id {
                    write!(f, "id: {}, ", id)?;
                }
                if *is_directive {
                    write!(f, "directive")?;
                }
                write!(f, "] ")?;
                if memory_mode != &MemoryMode::Auto {
                    write!(f, "{} ", memory_mode)?;
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
            ExprNode::BoolLit(b) => write!(f, "{}", if *b { "true" } else { "false" }),
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
