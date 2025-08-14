use std::collections::HashMap;

use crate::{
    ast::ast::{ExprNode, ForVariant, Node, Variant},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    log::Logger,
    parse::CodeSpan,
    symbol::SymbolId,
    visit_ast_children,
};

pub fn typecheck_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut typecheck_ctx = TypecheckPassContext::new(ctx.logger.clone());
    let ret = typecheck_ctx.run_typecheck_pass(ctx);

    ctx.errors.extend(typecheck_ctx.errors);
    ctx.warnings.extend(typecheck_ctx.warnings);

    ret
}

type TypeId = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeScheme {
    pub body: Type,
    pub quantified_vars: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int32,
    Int64,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Bool,
    String,
    Char,
    Void,
    Vector,
    Lambda {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Struct {
        symbol: SymbolId,
        fields: HashMap<String, Type>,
    },
    Actor {
        symbol: SymbolId,
        methods: HashMap<String, Type>,
        fields: HashMap<String, Type>,
    },
    List(Box<Type>),
    Tuple(Vec<Type>),
    Enum {
        symbol: SymbolId,
        variants: HashMap<String, Type>,
    },
    TypeVar(TypeId),
}

impl Type {
    //todo: substitutions, etc.
}

enum Constraint {
    Equality(Type, Type),
    Instance(Type, TypeScheme),
}

struct TypecheckPassContext {
    _logger: Logger,
    current_file: String,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
    constraints: Vec<Constraint>,
    symbol_types: HashMap<SymbolId, Type>,
    parent_function: Option<SymbolId>,
    type_var_counter: usize,
}

impl TypecheckPassContext {
    fn new(logger: Logger) -> Self {
        Self {
            _logger: logger,
            current_file: String::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            constraints: Vec::new(),
            symbol_types: HashMap::new(),
            type_var_counter: 0,
            parent_function: None,
        }
    }

    fn fresh_type_var(&mut self) -> Type {
        let type_var = Type::TypeVar(self.type_var_counter);
        self.type_var_counter += 1;
        type_var
    }

    fn get_symbol_type(&mut self, symbol: SymbolId) -> Type {
        let fresh_var = self.fresh_type_var();
        self.symbol_types
            .entry(symbol)
            .or_insert_with(|| fresh_var)
            .clone()
    }

    fn run_typecheck_pass(&mut self, ctx: &mut CompileContext) -> Result<(), ()> {
        let mut ret = Ok(());

        for (file, ast) in ctx.asts.iter_mut() {
            self.current_file = file.clone();
            ret = match self.inference_visitor(ast) {
                Ok(_) => ret,
                Err(_) => Err(()),
            };
        }

        ret
    }

    fn visit_var_decl(
        &mut self,
        name: &String,
        var_type: &Option<Box<Node>>,
        id: &Option<SymbolId>,
        initialiser: &Option<Box<Node>>,
        span: CodeSpan,
    ) -> Result<(), ()> {
        if id.is_none() {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: format!("Undefined symbol '{}'", name),
                    span: span,
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::UndeclaredSymbol,
                },
                notes: vec![],
                hints: vec![],
            });
            return Err(());
        }
        if let Some(t) = var_type {
            let type_ids = t.get_symbol_ids();
            if type_ids.is_none() {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("No type scheme found for symbol '{}'", name),
                        span: span,
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::UndefinedType,
                    },
                    notes: vec![],
                    hints: vec![],
                });
                return Err(());
            }

            let symbol_type = self.get_symbol_type(id.unwrap());
            let type_ids = type_ids.unwrap();
            let type_scheme = TypeScheme {
                body: self.get_symbol_type(type_ids.body_symbol_id),
                quantified_vars: type_ids
                    .params
                    .iter()
                    .map(|t| self.get_symbol_type(t.body_symbol_id))
                    .collect(),
            };

            self.constraints
                .push(Constraint::Instance(symbol_type, type_scheme))
        }

        Ok(())
    }

    fn inference_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        match &mut ast.variant {
            Variant::VarDecl {
                name,
                var_type,
                modifiable,
                initialiser,
                id,
            } => self.visit_var_decl(name, var_type, id, initialiser, ast.span)?,
            _ => {}
        }

        visit_ast_children!(ast.variant, self, inference_visitor, {});
        Ok(())
    }
}
