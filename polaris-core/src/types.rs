//use std::collections::{HashMap, HashSet};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::{
    ast::{BinaryOp, ExprKind, Node, NodeKind, UnaryOp},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    intrinsics::{
        ARRAY, BOOL, INT, LIST, MAP, REAL, STRING, VOID, create_intrinsic_binops,
        create_intrinsic_type_env, create_intrinsic_unary_ops,
    },
    module::DepGraphContext,
    parse::CodeSpan,
    symbol::{SymbolContext, SymbolId},
};

pub fn typecheck_pass(compile_ctx: &mut CompileContext) -> Result<(), ()> {
    let mut ctx = TypecheckContext::new(compile_ctx);
    ctx.typecheck()
}

// it's hindley-milner time hell yeah

struct TypecheckContext<'a> {
    type_env: TypeEnv,
    type_var_counter: TypeVar,
    errors: &'a mut Vec<Diagnostic>,
    // warnings: &'a mut Vec<Diagnostic>,
    symbols: &'a mut SymbolContext,
    deps: &'a mut DepGraphContext,
    type_info: &'a mut TypeInfo,
    current_file: String,
    // op -> (lhs, rhs) -> result type
    accepting_binops: HashMap<BinaryOp, HashMap<(Ty, Ty), Ty>>,
    // op -> operand type -> result type
    accepting_unops: HashMap<UnaryOp, HashMap<Ty, Ty>>,
    //todo: move to compile context
    //should this map to node borrows instead of just symbold ids?
    ///map new func id -> (original func id, instantiated type)
    //field access types for single-constructor types
    field_access_types: HashMap<SymbolId, HashMap<String, Ty>>,
    type_aliases: HashMap<SymbolId, TypeAliasInfo>,
    alias_dependencies: HashMap<SymbolId, HashSet<SymbolId>>,
}

pub type TypeVar = usize;

#[derive(Debug)]
pub struct TypeError {
    kind: TypeErrorKind,
    types: Vec<Ty>,
    hints: Vec<String>,
}

#[derive(Debug)]
pub enum TypeErrorKind {
    UnificationFail,
    InfiniteType,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum TyKind {
    Var(TypeVar),
    Concrete(SymbolId),
    //ty1 -> ty2
    Fn(Box<Ty>, Box<Ty>),
    Tuple(Vec<Ty>),
    //f(ty1, ty2, ...)
    Ctor(SymbolId, Vec<Ty>),
    //nocrypt ty
    Nocrypt(Box<Ty>),
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct Ty {
    kind: TyKind,
    literal: bool,
    require_secret: bool,
    secret_origin: Option<CodeSpan>,
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub bound_vars: Vec<TypeVar>,
    pub body: Ty,
}

#[derive(Clone, Default)]
pub struct TypeInfo {
    pub type_env: TypeEnv,
    pub fn_arg_labels: HashMap<SymbolId, Vec<String>>,
    pub monomorphised_fns: HashMap<SymbolId, (SymbolId, Ty)>,
    pub fn_instantiation_ids: HashMap<(SymbolId, Ty), SymbolId>,
}

#[derive(Clone, Debug)]
struct TypeAliasInfo {
    params: Vec<String>,
    bound_vars: Vec<TypeVar>,
    body: Ty,
    span: CodeSpan,
    file: String,
}

#[derive(Clone, Debug)]
pub struct Substitution(pub HashMap<TypeVar, Ty>);

pub type TypeEnv = HashMap<SymbolId, Scheme>;

impl<'a> TypecheckContext<'a> {
    fn new(ctx: &'a mut CompileContext) -> Self {
        Self {
            symbols: &mut ctx.symbols,
            type_env: HashMap::default(),
            type_var_counter: 0,
            errors: &mut ctx.errors,
            deps: &mut ctx.dependencies,
            type_info: &mut ctx.type_info,
            // warnings: &mut ctx.warnings,
            accepting_binops: HashMap::default(),
            current_file: String::new(),
            accepting_unops: HashMap::default(),
            field_access_types: HashMap::default(),
            type_aliases: HashMap::default(),
            alias_dependencies: HashMap::default(),
        }
    }

    pub fn typecheck(&mut self) -> Result<(), ()> {
        self.type_info.type_env.clear();
        self.type_info.fn_arg_labels.clear();
        self.type_info.monomorphised_fns.clear();
        self.type_info.fn_instantiation_ids.clear();

        self.initialise_type_env();

        let sccs = self.deps.sccs.clone();
        for scc in &sccs {
            for module_id in scc {
                let (file_name, mut ast) = {
                    let module = self.deps.modules.get(module_id).ok_or(())?;
                    (module.file.clone(), module.ast.clone())
                };

                self.current_file = file_name;
                let mut type_env = self.type_env.clone();

                //todo: top level pass for type decls/consts
                //then second pass for functions

                let (subst, _) = match self.algo_w_top_level(&mut type_env, &mut ast) {
                    Ok(res) => res,
                    Err(_) => {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: format!("Type checking failed for module {}", module_id),
                            span: ast.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::TypeMismatch,
                        }));
                        (Substitution::new(), self.fresh_type_var())
                    }
                };

                let mut type_env = subst.apply_env(&type_env);
                self.pad_below_env(&mut type_env);

                match self.algo_w(&mut type_env, &mut ast) {
                    Ok((subst, _ty)) => {
                        self.type_env = subst.apply_env(&self.type_env);
                    }
                    Err(_) => {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: format!("Type checking failed for module {}", module_id),
                            span: ast.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::TypeMismatch,
                        }));
                        continue;
                    }
                }

                if let Some(module) = self.deps.modules.get_mut(module_id) {
                    module.ast = ast;
                }
            }
        }

        self.type_info.type_env = self.type_env.clone();
        //self.debug_type_env("Final type env".into(), &self.type_env);
        //self.print_monomorphised_fns();

        Ok(())
    }

    #[allow(dead_code)]
    fn print_monomorphised_fns(&self) {
        let type_info = &*self.type_info;
        println!("\nMonomorphised functions:");
        println!(
            "\n| {:<32} | {:<32} | {:<96} |",
            " Function ", " From ", " Type "
        );
        println!("|-{:-<32}-|-{:-<32}-|-{:-<96}-|", "", "", "");
        let unknown = "<unknown>".into();
        for (new_id, (orig_id, ty)) in &type_info.monomorphised_fns {
            let orig_name = self.symbols.symbol_names.get(orig_id).unwrap_or(&unknown);
            let new_name = self.symbols.symbol_names.get(new_id).unwrap_or(&unknown);
            println!(
                "| {:<32} | {:<32} | {:<96} |",
                new_name,
                orig_name,
                ty.render(self)
            );
        }
        println!("\n");
    }

    #[allow(dead_code)]
    fn debug_type_env(&self, title: String, env: &TypeEnv) {
        let type_info = &*self.type_info;
        println!("{}", title);
        println!("\n| {:<32} | {:<96} |", " Symbol ", " Type ");
        println!("|-{:-<32}-|-{:-<96}-|", "", "");

        for (symbol_id, scheme) in env {
            if !self
                .symbols
                .intrinsic_types
                .values()
                .any(|&id| id == *symbol_id)
                && !self
                    .symbols
                    .intrinsic_symbols
                    .values()
                    .any(|&id| id == *symbol_id)
            {
                let rendered = scheme.body.render(self);
                println!(
                    "| {:<32} | {:<96} |",
                    self.symbols.symbol_names[symbol_id], rendered
                );
            }
        }

        println!("\nFunction argument labels:");
        println!("\n| {:<32} | {:<64} |", " Function ", " Arg Labels ");
        println!("|-{:-<32}-|-{:-<64}-|", "", "");
        for (symbol_id, labels) in &type_info.fn_arg_labels {
            if !self
                .symbols
                .intrinsic_types
                .values()
                .any(|&id| id == *symbol_id)
                && !self
                    .symbols
                    .intrinsic_symbols
                    .values()
                    .any(|&id| id == *symbol_id)
            {
                let rendered = labels.join(", ");
                println!(
                    "| {:<32} | {:<64} |",
                    self.symbols.symbol_names[symbol_id], rendered
                );
            }
        }
        println!("\n");
    }

    fn pad_below_env(&self, env: &mut TypeEnv) {
        for (symbol_id, t) in &self.type_env {
            if !env.contains_key(symbol_id) {
                env.insert(
                    *symbol_id,
                    Scheme {
                        bound_vars: vec![],
                        body: t.body.clone(),
                    },
                );
            }
        }
    }

    fn algo_w_top_level(
        &mut self,
        env: &mut TypeEnv,
        node: &mut Node,
    ) -> Result<(Substitution, Ty), ()> {
        match &mut node.kind {
            NodeKind::Module { .. } => self.algo_w_module(env, node, true),
            NodeKind::ConstDecl { .. } => self.algo_w_const_decl(env, node),

            NodeKind::TypeAlias { alias, actual, .. } => {
                let alias_symbol_id = node.symbol_id.ok_or(())?;
                let (param_names, bound_vars, type_param_map) =
                    self.build_alias_param_map(alias)?;

                let alias_type =
                    self.type_from_node_with_params(actual.as_mut(), &type_param_map)?;

                let mut deps = HashSet::default();
                self.collect_alias_dependencies(alias_symbol_id, &alias_type, &mut deps);
                if let Some(conflict) = self.detect_alias_cycle(alias_symbol_id, &deps) {
                    let alias_name = self
                        .symbols
                        .symbol_names
                        .get(&alias_symbol_id)
                        .cloned()
                        .unwrap_or_else(|| "<unknown>".into());

                    let conflict_name = self
                        .symbols
                        .symbol_names
                        .get(&conflict)
                        .cloned()
                        .unwrap_or_else(|| "<unknown>".into());

                    let mut diagnostic = Diagnostic {
                        primary: DiagnosticMsg {
                            message: format!(
                                "Type alias '{}' forms a cycle through '{}'",
                                alias_name, conflict_name
                            ),
                            span: node.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::TypeAliasCycle,
                        },
                        notes: vec![],
                        hints: vec!["Break the cycle by replacing one alias with a full type definition or removing the circular reference.".into()],
                    };

                    if let Some(conflict_info) = self.type_aliases.get(&conflict) {
                        diagnostic.notes.push(DiagnosticMsg {
                            message: format!("'{}' was previously aliased here", conflict_name),
                            span: conflict_info.span.clone(),
                            file: conflict_info.file.clone(),
                            err_type: DiagnosticMsgType::TypeAliasCycle,
                        });
                    }

                    self.errors.push(diagnostic);
                    return Err(());
                }

                let info = TypeAliasInfo {
                    params: param_names.clone(),
                    bound_vars: bound_vars.clone(),
                    body: alias_type.clone(),
                    span: node.span.clone(),
                    file: self.current_file.clone(),
                };

                self.type_aliases.insert(alias_symbol_id, info);
                self.alias_dependencies.insert(alias_symbol_id, deps);

                let scheme = Scheme {
                    bound_vars: bound_vars.clone(),
                    body: alias_type.clone(),
                };

                env.insert(alias_symbol_id, scheme.clone());
                self.type_env.insert(alias_symbol_id, scheme);

                Ok((
                    Substitution::new(),
                    Ty::new(TyKind::Concrete(self.symbols.intrinsic_types[VOID])),
                ))
            }

            //todo: type decls / alias + constructors
            NodeKind::TypeDecl {
                symbol: _symbol,
                type_vars,
                variants,
                ..
            } => {
                let type_symbol_id = node.symbol_id.ok_or(())?;

                //new tv for each type param
                let mut type_param_map = HashMap::default();
                let mut bound_vars = Vec::new();

                for (param_name, _, _) in type_vars.iter() {
                    let param_var = fresh_type_var_id(&mut self.type_var_counter);
                    type_param_map.insert(param_name.clone(), Ty::new(TyKind::Var(param_var)));
                    bound_vars.push(param_var);
                }

                let variant_symbol_ids: Vec<SymbolId> =
                    variants.iter().filter_map(|v| v.symbol_id).collect();

                let type_params: Vec<Ty> = type_vars
                    .iter()
                    .map(|(param_name, _, _)| type_param_map[param_name].clone())
                    .collect();

                let base_type = if type_params.is_empty() {
                    Ty::new(TyKind::Concrete(type_symbol_id))
                } else {
                    Ty::new(TyKind::Ctor(type_symbol_id, type_params))
                };

                env.insert(
                    type_symbol_id,
                    Scheme {
                        bound_vars: bound_vars.clone(),
                        body: base_type.clone(),
                    },
                );

                let mut last_variant: HashMap<String, Ty> = HashMap::default();

                let total_variants = variants.len();
                for (i, variant) in variants.iter_mut().enumerate() {
                    if let NodeKind::TypeConstructor {
                        symbol: _ctor_name,
                        fields,
                    } = &mut variant.kind
                    {
                        let is_last_variant = i == total_variants - 1;

                        if let Some(ctor_symbol_id) = variant.symbol_id {
                            let result_type = base_type.clone();

                            if fields.is_empty() {
                                //just construct the type, no args
                                env.insert(
                                    ctor_symbol_id,
                                    Scheme {
                                        bound_vars: bound_vars.clone(),
                                        body: result_type,
                                    },
                                );
                            } else {
                                let mut curried_type = result_type;

                                //reverse for currying
                                for (label, field_type_node, _) in fields.iter_mut().rev() {
                                    let field_type = self.type_from_node_with_params(
                                        field_type_node,
                                        &type_param_map,
                                    )?;

                                    if is_last_variant && label.is_some() {
                                        last_variant
                                            .insert(label.clone().unwrap(), field_type.clone());
                                    }

                                    let prev_secret = curried_type.require_secret;
                                    let mut new_fn = Ty::new(TyKind::Fn(
                                        Box::new(field_type.clone()),
                                        Box::new(curried_type),
                                    ));
                                    new_fn.require_secret =
                                        prev_secret || field_type.require_secret;
                                    curried_type = new_fn;
                                }

                                env.insert(
                                    ctor_symbol_id,
                                    Scheme {
                                        bound_vars: bound_vars.clone(),
                                        body: curried_type.clone(),
                                    },
                                );

                                //add labels
                                let field_labels = fields
                                    .iter()
                                    .map(|(label, _, _)| label.clone())
                                    .collect::<Vec<Option<String>>>();

                                if !field_labels.iter().all(|l| l.is_none()) {
                                    let field_labels: Vec<String> = field_labels
                                        .iter()
                                        .map(|l| l.clone().unwrap_or_else(|| "_".into()))
                                        .collect();

                                    self.type_info
                                        .fn_arg_labels
                                        .insert(ctor_symbol_id, field_labels.clone());
                                }
                            }
                        }
                    }
                }

                if variants.len() == 1 {
                    //enable field access
                    self.field_access_types.insert(type_symbol_id, last_variant);
                }

                for (symbol_id, scheme) in env.iter() {
                    if *symbol_id == type_symbol_id || variant_symbol_ids.contains(symbol_id) {
                        self.type_env.insert(*symbol_id, scheme.clone());
                    }
                }

                //return void type
                Ok((
                    Substitution::new(),
                    Ty::new(TyKind::Concrete(self.symbols.intrinsic_types[VOID])),
                ))
            }

            //not doing full algo_w for fn decls at top level, instead just getting generic types
            NodeKind::FnDecl {
                args,
                return_type,
                pure: _,
                ..
            } => {
                let mut arg_types = Vec::new();

                for (_, arg_type, _) in args {
                    let arg_ty = if let Some(type_node) = arg_type {
                        self.type_from_node(type_node.as_mut())?
                    } else {
                        self.fresh_type_var()
                    };
                    arg_types.push(arg_ty.clone());
                }

                let ret_type = if let Some(ret) = return_type {
                    self.type_from_node(ret.as_mut())?
                } else {
                    self.fresh_type_var()
                };

                let mut fn_type = ret_type;
                let mut accumulated_secret = fn_type.require_secret;

                if arg_types.is_empty() {
                    let mut void_fn = Ty::new(TyKind::Fn(
                        Box::new(Ty::new(TyKind::Concrete(
                            self.symbols.intrinsic_types[VOID],
                        ))),
                        Box::new(fn_type),
                    ));
                    accumulated_secret = accumulated_secret || void_fn.require_secret;
                    void_fn.require_secret = accumulated_secret;
                    fn_type = void_fn;
                }

                for arg_type in arg_types.into_iter().rev() {
                    accumulated_secret = accumulated_secret || arg_type.require_secret;
                    let mut new_fn = Ty::new(TyKind::Fn(Box::new(arg_type), Box::new(fn_type)));
                    new_fn.require_secret = accumulated_secret;
                    fn_type = new_fn;
                }

                //inefficient, should refactor
                let mut env = env.clone();
                self.add_declaration_to_env(node, &fn_type, &mut env);

                Ok((Substitution::new(), fn_type))
            }

            _ => Ok((Substitution::new(), self.fresh_type_var())),
        }
    }

    fn algo_w_module(
        &mut self,
        env: &mut TypeEnv,
        node: &mut Node,
        top_level: bool,
    ) -> Result<(Substitution, Ty), ()> {
        match &mut node.kind {
            NodeKind::Module { children } => {
                let mut subst = Substitution::new();
                let mut current_env = env.clone();
                let mut last_type = self.fresh_type_var();

                for child in children {
                    if !top_level && !matches!(&child.kind, NodeKind::FnDecl { .. }) {
                        continue;
                    }

                    let (s, t) = match if top_level {
                        self.algo_w_top_level(&mut current_env, child)
                    } else {
                        self.algo_w(&mut current_env, child)
                    } {
                        Ok(res) => res,
                        Err(_) => (Substitution::new(), self.fresh_type_var()),
                    };
                    subst = subst.compose(&s);
                    current_env = subst.apply_env(&current_env);
                    last_type = subst.apply(&t);

                    self.add_declaration_to_env(child, &t, &mut current_env);
                }

                Ok((subst, last_type))
            }
            _ => self.algo_w(env, node),
        }
    }

    fn algo_w_const_decl(
        &mut self,
        env: &mut TypeEnv,
        node: &mut Node,
    ) -> Result<(Substitution, Ty), ()> {
        match &mut node.kind {
            NodeKind::ConstDecl {
                const_type, expr, ..
            } => {
                let (s1, t1) = self.algo_w(env, expr)?;

                if let Some(type_node) = const_type {
                    let declared_type = self.type_from_node(type_node.as_mut())?;
                    let s2 = self.unify(&s1.apply(&t1), &declared_type);
                    let s2 = self.bind_err_ctx(
                        s2,
                        node.span,
                        Some((
                            format!(
                                "Annotated type {} does not match inferred type {}",
                                declared_type.render(self),
                                t1.render(self),
                            )
                            .into(),
                            vec!["$1 does not match $2".into()],
                        )),
                    )?;
                    let final_subst = s1.compose(&s2);
                    Ok((final_subst, s2.apply(&t1)))
                } else {
                    Ok((s1, t1))
                }
            }
            _ => self.algo_w(env, node),
        }
    }

    fn algo_w(&mut self, env: &mut TypeEnv, node: &mut Node) -> Result<(Substitution, Ty), ()> {
        let result = match &mut node.kind {
            NodeKind::Module { .. } => self.algo_w_module(env, node, false),
            NodeKind::Expr { .. } => self.algo_w_expr(env, node),
            NodeKind::ConstDecl { .. } => self.algo_w_const_decl(env, node),
            NodeKind::FnDecl {
                args,
                return_type,
                expr,
                ..
            } => {
                if let Some(body) = expr {
                    let mut arg_types = Vec::new();
                    let mut new_env = env.clone();

                    for (arg_pattern, arg_type, _) in args {
                        let arg_ty = if let Some(type_node) = arg_type {
                            self.type_from_node(type_node.as_mut())?
                        } else {
                            self.fresh_type_var()
                        };
                        arg_types.push(arg_ty.clone());

                        if let NodeKind::Expr {
                            expr: ExprKind::Symbol { .. },
                        } = &arg_pattern.kind
                        {
                            if let Some(symbol_id) = arg_pattern.symbol_id {
                                new_env.insert(
                                    symbol_id,
                                    Scheme {
                                        bound_vars: vec![],
                                        body: arg_ty,
                                    },
                                );
                            }
                        }
                    }

                    let (s1, body_type) = match self.algo_w(&mut new_env, body) {
                        Ok(res) => res,
                        Err(_) => {
                            //fresh typevar to continue typecheck
                            (Substitution::new(), self.fresh_type_var())
                        }
                    };

                    let final_return_type = if let Some(ret_type_node) = return_type {
                        let declared_ret_type = self.type_from_node(ret_type_node.as_mut())?;
                        let s2 = self.unify(&s1.apply(&body_type), &declared_ret_type);
                        let s2 = self.bind_err_ctx(
                            s2,
                            node.span,
                            Some((
                                format!(
                                    "Annotated return type '{}' does not match inferred type '{}'",
                                    declared_ret_type.render(self),
                                    body_type.render(self),
                                )
                                .into(),
                                vec![],
                            )),
                        )?;

                        let final_subst = s1.compose(&s2);
                        (final_subst, s2.apply(&body_type))
                    } else {
                        (s1, body_type)
                    };

                    let mut fn_type = final_return_type.1;
                    let mut accumulated_secret = fn_type.require_secret;

                    if arg_types.is_empty() {
                        let mut void_fn = Ty::new(TyKind::Fn(
                            Box::new(Ty::new(TyKind::Concrete(
                                self.symbols.intrinsic_types[VOID],
                            ))),
                            Box::new(fn_type),
                        ));
                        accumulated_secret = accumulated_secret || void_fn.require_secret;
                        void_fn.require_secret = accumulated_secret;
                        fn_type = void_fn;
                    }

                    for arg_type in arg_types.into_iter().rev() {
                        accumulated_secret = accumulated_secret || arg_type.require_secret;
                        let mut new_fn = Ty::new(TyKind::Fn(
                            Box::new(final_return_type.0.apply(&arg_type)),
                            Box::new(fn_type),
                        ));
                        new_fn.require_secret = accumulated_secret;
                        fn_type = new_fn;
                    }

                    //actually just updating prior generic declaration
                    //with new algo-w-provided type
                    self.add_declaration_to_env(node, &fn_type, env);

                    Ok((final_return_type.0, fn_type))
                } else {
                    let mut arg_types = Vec::new();
                    for (_, arg_type, _) in args {
                        if let Some(type_node) = arg_type {
                            arg_types.push(self.type_from_node(type_node.as_mut())?);
                        } else {
                            self.errors.push(Diagnostic::new(DiagnosticMsg {
                                message: "Host function argument missing type annotation"
                                    .to_string(),
                                span: node.span.clone(),
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::UndefinedType,
                            }));
                            return Err(());
                        }
                    }

                    let return_ty = if let Some(ret_type_node) = return_type {
                        self.type_from_node(ret_type_node.as_mut())?
                    } else {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: "Host function missing return type annotation".to_string(),
                            span: node.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::UndefinedType,
                        }));

                        return Err(());
                    };

                    let mut fn_type = return_ty;
                    let mut accumulated_secret = fn_type.require_secret;
                    if arg_types.is_empty() {
                        let mut void_fn = Ty::new(TyKind::Fn(
                            Box::new(Ty::new(TyKind::Concrete(
                                self.symbols.intrinsic_types[VOID],
                            ))),
                            Box::new(fn_type),
                        ));
                        accumulated_secret = accumulated_secret || void_fn.require_secret;
                        void_fn.require_secret = accumulated_secret;
                        fn_type = void_fn;
                    }
                    for arg_type in arg_types.into_iter().rev() {
                        accumulated_secret = accumulated_secret || arg_type.require_secret;
                        let mut new_fn = Ty::new(TyKind::Fn(Box::new(arg_type), Box::new(fn_type)));
                        new_fn.require_secret = accumulated_secret;
                        fn_type = new_fn;
                    }

                    Ok((Substitution::new(), fn_type))
                }
            }
            _ => Ok((Substitution::new(), self.fresh_type_var())),
        };

        if let Ok((subst, ty)) = &result {
            let annotated = subst.apply(ty);
            node.ty = Some(annotated);
        }

        result
    }

    fn bind_err_ctx<T>(
        &mut self,
        result: Result<T, TypeError>,
        span: CodeSpan,
        msg: Option<(String, Vec<String>)>,
    ) -> Result<T, ()> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => {
                let rendered = e.types.iter().map(|t| t.render(self)).collect::<Vec<_>>();

                //this is wildly inefficient but only runs on errors so who really cares ig

                let (message, hints) = match msg {
                    Some((mut m, mut hints)) => {
                        for i in 0..rendered.len() {
                            let source_str = format!("${}", i + 1);
                            let target_str = format!("{}", rendered[i]);
                            m = m.replace(&source_str, &target_str);
                            hints = hints
                                .iter()
                                .map(|h| h.replace(&source_str, &target_str))
                                .collect();
                        }
                        hints.extend(e.hints);
                        (m, hints)
                    }
                    None => match e.kind {
                        TypeErrorKind::UnificationFail => (
                            format!(
                                "Type mismatch: could not unify types: {}",
                                rendered.join(", ")
                            ),
                            e.hints,
                        ),
                        TypeErrorKind::InfiniteType => (
                            format!(
                                "Infinite type detected involving types: {}",
                                rendered.join(", "),
                            ),
                            e.hints,
                        ),
                    },
                };

                let mut e = Diagnostic::new(DiagnosticMsg {
                    message,
                    span,
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::TypeMismatch,
                });

                for hint in hints {
                    e.add_hint(hint)
                }

                self.errors.push(e);

                Err(())
            }
        }
    }

    fn algo_w_expr(
        &mut self,
        env: &mut TypeEnv,
        node: &mut Node,
    ) -> Result<(Substitution, Ty), ()> {
        let expr = match &mut node.kind {
            NodeKind::Expr { expr } => expr,
            _ => unreachable!(),
        };

        match expr {
            ExprKind::IntLit(_) => Ok((
                Substitution::new(),
                Ty::new_literal(TyKind::Concrete(self.symbols.intrinsic_types[INT])),
            )),
            ExprKind::RealLit { .. } => Ok((
                Substitution::new(),
                Ty::new_literal(TyKind::Concrete(self.symbols.intrinsic_types[REAL])),
            )),
            ExprKind::StringLit(_) => Ok((
                Substitution::new(),
                Ty::new_literal(TyKind::Concrete(self.symbols.intrinsic_types[STRING])),
            )),
            ExprKind::MapLit(map) => {
                let mut key_type = self.fresh_type_var();
                let mut value_type = self.fresh_type_var();
                let mut env = env.clone();

                for (key, value) in map.iter_mut() {
                    let (s1, t1) = self.algo_w(&mut env, key)?;
                    let s2 = self.unify(&s1.apply(&t1), &key_type);
                    let s2 = self.bind_err_ctx(
                        s2,
                        key.span.clone(),
                        Some((
                            format!(
                                "Map key type mismatch - expected {}, got {}. Maps are not dynamically typed.",
                                key_type.render(self),
                                t1.render(self)
                            ),
                            vec![],
                        )),
                    )?;
                    let subst = s1.compose(&s2);
                    env = subst.apply_env(&env);

                    let (s3, t3) = self.algo_w(&mut env.clone(), value)?;
                    let s4 = self.unify(&subst.apply(&t3), &value_type);
                    let s4 = self.bind_err_ctx(
                        s4,
                        value.span.clone(),
                        Some((
                            format!(
                                "Map value type mismatch - expected {}, got {}. Maps are not dynamically typed.",
                                value_type.render(self),
                                t3.render(self)
                            ),
                            vec![],
                        )),
                    )?;

                    let final_subst = subst.compose(&s3).compose(&s4);
                    let final_env = final_subst.apply_env(&env);

                    key_type = final_subst.apply(&key_type);
                    value_type = final_subst.apply(&value_type);

                    env = final_env;
                }

                let mut ty = Ty::new(TyKind::Ctor(
                    self.symbols.intrinsic_types[MAP],
                    vec![key_type.clone(), value_type.clone()],
                ));
                ty.literal = key_type.literal && value_type.literal;
                ty.merge_secret_from(&key_type);
                ty.merge_secret_from(&value_type);

                Ok((Substitution::new(), ty))
            }

            ExprKind::Symbol { name } => {
                if let Some(symbol_id) = node.symbol_id {
                    if let Some(scheme) = env.get(&symbol_id) {
                        let ty = scheme.clone().instantiate(&mut self.type_var_counter);
                        Ok((Substitution::new(), ty))
                    } else {
                        Ok((Substitution::new(), self.fresh_type_var()))
                    }
                } else {
                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                        message: format!("Unresolved symbol: {}", name),
                        span: node.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::UndeclaredSymbol,
                    }));
                    Err(())
                }
            }

            ExprKind::FieldAccess { expr, field } => {
                //check if expr type has only a single constructor, and then check if field exists on that constructor
                let (s1, expr_type) = self.algo_w(env, expr)?;
                let expr_type = s1.apply(&expr_type);

                let expr_type_id = match &expr_type.kind {
                    TyKind::Concrete(id) => Some(*id),
                    TyKind::Ctor(id, _) => Some(*id),
                    _ => None,
                };

                if expr_type_id.is_none() {
                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                        message: format!("Type '{}' does not have fields", expr_type.render(self)),
                        span: node.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::TypeMismatch,
                    }));
                    return Err(());
                }

                let field_access_map = self.field_access_types.get(&expr_type_id.unwrap());
                match field_access_map {
                    None => {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                        message: format!(
                            "Type '{}' does not have fields - if you need to access wrapped data, use a 'match' expression to match on the type variant.",
                            expr_type.render(self)
                        ),
                        span: node.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::TypeMismatch,
                    }));
                        return Err(());
                    }
                    Some(field_access_map) => {
                        if let Some(expected_field_type) = field_access_map.get(field) {
                            //need to instantiate type params
                            let instantiated_field_type = match &expr_type.kind {
                                TyKind::Ctor(type_id, type_args) => {
                                    let mut param_subst = Substitution::new();

                                    //find param names
                                    if let Some(type_scheme) = self.type_env.get(type_id) {
                                        for (i, &bound_var) in
                                            type_scheme.bound_vars.iter().enumerate()
                                        {
                                            if i < type_args.len() {
                                                param_subst
                                                    .0
                                                    .insert(bound_var, type_args[i].clone());
                                            }
                                        }
                                    }

                                    param_subst.apply(expected_field_type)
                                }
                                _ => expected_field_type.clone(),
                            };

                            let final_field_type = s1.apply(&instantiated_field_type);
                            Ok((s1, final_field_type))
                        } else {
                            self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: format!(
                                "Type '{}' does not have a field named '{}'. Available fields are: {}",
                                expr_type.render(self),
                                field,
                                field_access_map
                                    .keys()
                                    .cloned()
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                            span: node.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::UndeclaredSymbol,
                        }));
                            Err(())
                        }
                    }
                }
            }

            ExprKind::TupleLit(elements) => {
                let mut subst = Substitution::new();
                let mut types = Vec::new();
                let mut current_env = env.clone();

                let mut is_literal = true;
                let mut require_secret = false;
                let mut secret_origin = None;
                for elem in elements {
                    let (s, t) = self.algo_w(&mut current_env, elem)?;
                    subst = subst.compose(&s);
                    current_env = subst.apply_env(&current_env);
                    let t = subst.apply(&t);

                    is_literal = is_literal && t.literal;
                    if t.require_secret && secret_origin.is_none() {
                        secret_origin = t.secret_origin.clone();
                    }
                    require_secret = require_secret || t.require_secret;

                    types.push(t);
                }

                let ty = Ty {
                    kind: TyKind::Tuple(types.clone()),
                    literal: is_literal,
                    require_secret,
                    secret_origin,
                };

                Ok((subst, ty))
            }

            ExprKind::ListLit(elements) => {
                if elements.is_empty() {
                    let elem_type = self.fresh_type_var();
                    Ok((
                        Substitution::new(),
                        Ty::new_literal(TyKind::Ctor(
                            self.symbols.intrinsic_types[LIST],
                            vec![elem_type],
                        )),
                    ))
                } else {
                    let (s1, t1) = self.algo_w(env, &mut elements[0])?;
                    let mut subst = s1;
                    let mut current_env = subst.apply_env(env);
                    let mut elem_type = subst.apply(&t1);

                    let mut is_literal = true;
                    let mut require_secret = elem_type.require_secret;
                    let mut secret_origin = elem_type.secret_origin.clone();

                    for elem in elements.iter_mut().skip(1) {
                        let (s, t) = self.algo_w(&mut current_env, elem)?;
                        let s2 = self.unify(&subst.apply(&elem_type), &s.apply(&t));
                        let s2 = self.bind_err_ctx(
                            s2,
                            elem.span,
                            Some(("List element type mismatch".into(), vec![])),
                        )?;
                        subst = subst.compose(&s).compose(&s2);
                        current_env = subst.apply_env(&current_env);
                        elem_type = subst.apply(&elem_type);

                        is_literal = is_literal && elem_type.literal;
                        if elem_type.require_secret && secret_origin.is_none() {
                            secret_origin = elem_type.secret_origin.clone();
                        }
                        require_secret = require_secret || elem_type.require_secret;
                    }

                    let mut ty = Ty::new(TyKind::Ctor(
                        self.symbols.intrinsic_types[LIST],
                        vec![elem_type],
                    ));
                    ty.literal = is_literal;
                    ty.require_secret = require_secret;
                    if ty.require_secret {
                        ty.secret_origin = secret_origin;
                    }

                    Ok((subst, ty))
                }
            }

            ExprKind::FnCall { callee, args } => {
                let (s1, callee_type) = self.algo_w(env, callee)?;
                let env1 = s1.apply_env(env);

                let fn_args = callee_type.fn_args();
                let is_void_function_call = fn_args.len() == 1
                    && args.len() == 0
                    && matches!(fn_args[0].kind, TyKind::Concrete(id) if id == self.symbols.intrinsic_types[VOID]);

                if !is_void_function_call && fn_args.len() != args.len() {
                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                        message: format!(
                            "Function '{}' expected {} arguments but got {}.",
                            self.symbols
                                .symbol_names
                                .get(&callee.symbol_id.unwrap_or(0))
                                .unwrap_or(&"<unknown>".into()),
                            fn_args.len(),
                            args.len()
                        ),
                        span: node.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::TypeMismatch,
                    }));
                    return Err(());
                }

                let mut subst = s1;
                let mut current_env = env1;
                let mut arg_types = Vec::new();

                let fn_arg_labels = if let Some(symbol_id) = callee.symbol_id {
                    self.type_info.fn_arg_labels.get(&symbol_id).cloned()
                } else {
                    None
                };

                if is_void_function_call {
                    arg_types.push((
                        None,
                        Ty::new(TyKind::Concrete(self.symbols.intrinsic_types[VOID])),
                    ));
                } else {
                    for (label, arg) in args {
                        let (s, t) = self.algo_w(&mut current_env, arg)?;
                        subst = subst.compose(&s);
                        current_env = subst.apply_env(&current_env);
                        arg_types.push((label.clone(), subst.apply(&t)));
                    }
                }

                //if fn_arg_labels some, reorder arg_types so labels are in order
                let arg_types = if let Some(fn_arg_labels) = fn_arg_labels {
                    let mut reordered_arg_types = Vec::new();
                    let mut used_indices = HashSet::default();
                    for label in fn_arg_labels.iter() {
                        if let Some((index, (_, ty))) = arg_types
                            .iter()
                            .enumerate()
                            .find(|(_, (arg_label, _))| arg_label.as_ref() == Some(&label))
                        {
                            reordered_arg_types.push(ty.clone());
                            used_indices.insert(index);
                        }
                    }

                    //find labels that where used in the call but not in the function definition
                    for (index, (arg_label, _)) in arg_types.iter().enumerate() {
                        if let Some(arg_label) = arg_label {
                            if !fn_arg_labels.contains(arg_label) && !used_indices.contains(&index)
                            {
                                let symbol = self
                                    .symbols
                                    .symbol_names
                                    .get(&callee.symbol_id.unwrap_or(0));

                                self.errors.push(Diagnostic::new(DiagnosticMsg {
                                    message: format!(
                                        "{} '{}' does not have an argument labelled '{}'",
                                        if symbol.is_some()
                                            && symbol
                                                .unwrap()
                                                .chars()
                                                .next()
                                                .map(|c| c.is_uppercase())
                                                .unwrap_or(false)
                                        {
                                            "Type constructor"
                                        } else {
                                            "Function"
                                        },
                                        symbol.unwrap_or(&"<unknown>".into()),
                                        arg_label
                                    ),
                                    span: node.span.clone(),
                                    file: self.current_file.clone(),
                                    err_type: DiagnosticMsgType::UndeclaredSymbol,
                                }));
                                return Err(());
                            }
                        }
                    }

                    //add any unlabeled args in original order
                    for (index, (_, ty)) in arg_types.iter().enumerate() {
                        if !used_indices.contains(&index) {
                            reordered_arg_types.push(ty.clone());
                        }
                    }
                    reordered_arg_types
                } else {
                    arg_types.into_iter().map(|(_, ty)| ty).collect()
                };

                let mut return_type = self.fresh_type_var();
                return_type.require_secret = return_type.require_secret
                    || arg_types.iter().any(|arg_type| arg_type.require_secret);

                let mut expected_fn_type = return_type.clone();
                let mut accumulated_secret = expected_fn_type.require_secret;

                if arg_types.is_empty() {
                    let mut void_fn = Ty::new(TyKind::Fn(
                        Box::new(Ty::new(TyKind::Concrete(
                            self.symbols.intrinsic_types[VOID],
                        ))),
                        Box::new(expected_fn_type),
                    ));
                    accumulated_secret = accumulated_secret || void_fn.require_secret;
                    void_fn.require_secret = accumulated_secret;
                    expected_fn_type = void_fn;
                }

                for arg_type in arg_types.into_iter().rev() {
                    accumulated_secret = accumulated_secret || arg_type.require_secret;
                    let mut new_fn =
                        Ty::new(TyKind::Fn(Box::new(arg_type), Box::new(expected_fn_type)));
                    new_fn.require_secret = accumulated_secret;
                    expected_fn_type = new_fn;
                }

                let s_unify = self.unify(&subst.apply(&callee_type), &expected_fn_type);
                let s_unify = self.bind_err_ctx(
                    s_unify,
                    node.span,
                    Some((
                        format!("Function argument types do not match function signature - tried to equate {} and {}", callee_type.render(self), expected_fn_type.render(self)),
                        vec![],
                    )),
                )?;
                let final_subst = subst.compose(&s_unify);
                let final_return_type = s_unify.apply(&return_type);
                let final_fn_type = final_subst.apply(&callee_type);

                if final_return_type.free_type_vars().len() == 0
                    && let Some(callee_id) = callee.symbol_id
                {
                    //monotype, check monormorphisation table
                    if let Some(existing_id) = self
                        .type_info
                        .fn_instantiation_ids
                        .get(&(callee_id, final_fn_type.clone()))
                    {
                        node.symbol_id = Some(*existing_id);
                    } else {
                        let new_symbol_id = self.register_monomorphised_fn(callee, &final_fn_type);
                        node.symbol_id = Some(new_symbol_id);
                    }
                }

                Ok((final_subst, final_return_type))
            }

            ExprKind::BinaryOp { left, op, right } => {
                let (s1, t1) = self.algo_w(env, left)?;
                let mut env1 = s1.apply_env(env);
                let (s2, t2) = self.algo_w(&mut env1, right)?;
                let s3 = s1.compose(&s2);

                let left_type = s3.apply(&t1);
                let right_type = s3.apply(&t2);

                let (left_is_nocrypt, left_unwrapped) = match &left_type.kind {
                    TyKind::Nocrypt(inner) => (true, (**inner).clone()),
                    _ => (false, left_type.clone()),
                };
                let (right_is_nocrypt, right_unwrapped) = match &right_type.kind {
                    TyKind::Nocrypt(inner) => (true, (**inner).clone()),
                    _ => (false, right_type.clone()),
                };

                if left_is_nocrypt != right_is_nocrypt {
                    self.bind_err_ctx(
                        Err(TypeError {
                            kind: TypeErrorKind::UnificationFail,
                            types: vec![left_type.clone(), right_type.clone()],
                            hints: vec!["Attempted to mix usage of encrypted and 'nocrypt' types - remove/add 'nocrypt' type modifiers to fix.".into()],
                        }),
                        node.span,
                        Some((
                            format!("Binary op '{}' is not valid for operands of types '{}' and '{}'", op, left_type.render(self), right_type.render(self)),
                            vec![],
                        )),
                    )?;
                    return Err(());
                }

                let op_map = self.accepting_binops.get(op);
                if op_map.is_none() {
                    self.bind_err_ctx(
                        Err(TypeError {
                            kind: TypeErrorKind::UnificationFail,
                            types: vec![left_type, right_type],
                            hints: vec![format!("Binary operator {:?} is not supported", op)],
                        }),
                        node.span,
                        Some((format!("Unsupported binary operator {:?}", op), vec![])),
                    )?;
                    return Err(());
                }
                let op_map = op_map.unwrap();

                if let Some(result_type) =
                    op_map.get(&(left_unwrapped.clone(), right_unwrapped.clone()))
                {
                    let mut ty = result_type.clone();
                    ty.merge_secret_from(&left_type);
                    ty.merge_secret_from(&right_type);

                    if left_type.literal && right_type.literal {
                        ty.literal = true;
                    }

                    //rewrap in nocrypt if was unwrapped
                    if left_is_nocrypt {
                        ty = Ty::wrap_nocrypt(ty);
                    }
                    Ok((s3, ty))
                } else {
                    // both literals, use typevar to allow backward inference
                    if left_type.literal && right_type.literal {
                        let mut result_ty = self.fresh_type_var();
                        result_ty.merge_secret_from(&left_type);
                        result_ty.merge_secret_from(&right_type);

                        if left_is_nocrypt {
                            result_ty = Ty::wrap_nocrypt(result_ty);
                        }

                        return Ok((s3, result_ty));
                    }

                    let mut allowed_combinations: Vec<_> = op_map
                        .iter()
                        .map(|((l, r), result)| (l.clone(), r.clone(), result.clone()))
                        .collect();

                    // prefer int over sized ints
                    if left_type.literal || right_type.literal {
                        use crate::intrinsics::INT;
                        let int_id = self.symbols.intrinsic_types[INT];
                        allowed_combinations.sort_by_key(|(l, r, _)| {
                            let l_is_int = matches!(&l.kind, TyKind::Concrete(id) if *id == int_id);
                            let r_is_int = matches!(&r.kind, TyKind::Concrete(id) if *id == int_id);
                            if l_is_int && r_is_int { 0 } else { 1 }
                        });
                    }

                    let mut found_result = None;

                    //unify unwrapped
                    for (allowed_left, allowed_right, result_type) in allowed_combinations {
                        if let Ok(s4) = self.unify(&left_unwrapped, &allowed_left) {
                            if let Ok(s5) =
                                self.unify(&s4.apply(&right_unwrapped), &s4.apply(&allowed_right))
                            {
                                let final_subst = s3.compose(&s4).compose(&s5);
                                found_result = Some((final_subst, result_type));
                                break;
                            }
                        }
                    }

                    if let Some((final_subst, mut result_type)) = found_result {
                        result_type.merge_secret_from(&left_type);
                        result_type.merge_secret_from(&right_type);

                        if left_type.literal && right_type.literal {
                            result_type.literal = true;
                        }

                        if left_is_nocrypt {
                            result_type = Ty::wrap_nocrypt(result_type);
                        }
                        Ok((final_subst, result_type))
                    } else {
                        self.bind_err_ctx(
                            Err(TypeError {
                                kind: TypeErrorKind::UnificationFail,
                                types: vec![left_type.clone(), right_type.clone()],
                                hints: vec![],
                            }),
                            node.span,
                            Some((
                                format!("Binary op '{}' is not valid for operands of types '{}' and '{}'", op, left_type.render(self), right_type.render(self)),
                                vec![],
                            )),
                        )?;
                        unreachable!()
                    }
                }
            }

            ExprKind::UnaryOp {
                expr: operand, op, ..
            } => {
                //atm just returning original type - todo
                let (s, t) = self.algo_w(env, operand)?;

                let op_map = self.accepting_unops.get(op);
                if op_map.is_none() {
                    self.bind_err_ctx(
                        Err(TypeError {
                            kind: TypeErrorKind::UnificationFail,
                            types: vec![t.clone()],
                            hints: vec![format!("Unary operator {:?} is not supported", op)],
                        }),
                        node.span,
                        Some((format!("Unsupported unary operator {:?}", op), vec![])),
                    )?;
                    return Err(());
                }
                let op_map = op_map.unwrap();

                if let Some(result_type) = op_map.get(&t) {
                    let mut result_type = result_type.clone();
                    result_type.merge_secret_from(&t);
                    Ok((s, result_type))
                } else {
                    let mut allowed_types: Vec<_> = op_map
                        .iter()
                        .map(|(ty, result)| (ty.clone(), result.clone()))
                        .collect();

                    if t.literal {
                        use crate::intrinsics::INT;
                        let int_id = self.symbols.intrinsic_types[INT];
                        allowed_types.sort_by_key(|(ty, _)| match &ty.kind {
                            TyKind::Concrete(id) if *id == int_id => 0,
                            _ => 1,
                        });
                    }

                    let mut found_result = None;
                    for (allowed_type, result_type) in allowed_types {
                        if let Ok(s2) = self.unify(&t, &allowed_type) {
                            let final_subst = s.compose(&s2);
                            found_result = Some((final_subst, result_type));
                            break;
                        }
                    }

                    if let Some((final_subst, mut result_type)) = found_result {
                        result_type.merge_secret_from(&t);
                        Ok((final_subst, result_type))
                    } else {
                        self.bind_err_ctx(
                            Err(TypeError {
                                kind: TypeErrorKind::UnificationFail,
                                types: vec![t.clone()],
                                hints: vec![],
                            }),
                            node.span,
                            Some((
                                format!(
                                    "Unary op '{}' is not valid for operand of type '{}'",
                                    op,
                                    t.render(self)
                                ),
                                vec![],
                            )),
                        )?;
                        unreachable!()
                    }
                }
            }

            ExprKind::LetBinding {
                symbol_type,
                expr: value_expr,
                symbols,
            } => {
                let (s1, t1) = self.algo_w(env, value_expr)?;
                let result = if let Some(declared_type) = symbol_type {
                    let expected_type = self.type_from_node(declared_type.as_mut())?;

                    //literal coercion before unification
                    let t1_for_unify = if t1.literal {
                        self.coerce_literal_to_type(&t1, &expected_type)
                            .unwrap_or(t1.clone())
                    } else {
                        t1.clone()
                    };

                    let s2 = self.unify(&s1.apply(&t1_for_unify), &expected_type);
                    let s2 = self.bind_err_ctx(
                        s2,
                        node.span,
                        Some((
                            format!(
                                "Let binding type annotation '{}' does not match inferred type '{}'",
                                expected_type.render(self),
                                t1.render(self)
                            )
                            .into(),
                            vec![],
                        )),
                    )?;
                    let final_subst = s1.compose(&s2);

                    let nocrypt = matches!(expected_type.kind, TyKind::Nocrypt(_));
                    let implicit_require_secret = t1.require_secret;
                    let implicit_origin = t1.secret_origin.clone();

                    let mut t1 = s2.apply(&expected_type);
                    t1.literal = false;
                    if implicit_require_secret {
                        t1.require_secret = true;
                        if t1.secret_origin.is_none() {
                            t1.secret_origin = implicit_origin.clone();
                        }
                    }

                    let t1 = if nocrypt && !matches!(t1.kind, TyKind::Nocrypt(_)) {
                        Ty::wrap_nocrypt_deep(t1)
                    } else {
                        t1
                    };

                    if nocrypt && implicit_require_secret {
                        let mut diag = Diagnostic::new(DiagnosticMsg {
                            message: "Cannot assign outcome of secret branch expression to nocrypt binding. Remove 'nocrypt' annotation or ensure all branches are non-secret.".into(),
                            span: node.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::TypeMismatch,
                        });

                        if let Some(origin_span) =
                            implicit_origin.or_else(|| t1.secret_origin.clone())
                        {
                            diag.add_note(DiagnosticMsg {
                                message: "This value is secret, meaning any bindings that depend on it must also be secret.".into(),
                                span: origin_span,
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::TypeMismatch,
                            });
                        }

                        self.errors.push(diag);
                        return Err(());
                    }

                    (final_subst, t1)
                } else {
                    (s1, t1)
                };

                let s_pat = self.typecheck_pattern(env, symbols, &result.1, false)?;
                let final_subst = result.0.compose(&s_pat);

                Ok((
                    final_subst,
                    Ty::new(TyKind::Concrete(self.symbols.intrinsic_types[VOID])),
                ))
            }

            ExprKind::Match { expr, arms } => {
                let (s1, t_expr) = self.algo_w(env, expr)?;
                let mut subst = s1;
                let mut t_expr = subst.apply(&t_expr);

                if !t_expr.nocryptable() {
                    t_expr.mark_secret(&expr.span);
                } else {
                    t_expr.require_secret = false;
                    t_expr.secret_origin = None;
                }

                let mut t_match = self.fresh_type_var();
                t_match.require_secret = t_expr.require_secret;

                for (patterns, arm_expr) in arms {
                    let mut arm_env = subst.apply_env(env);

                    for pattern in patterns {
                        let s_pat = self.typecheck_pattern(&mut arm_env, pattern, &t_expr, true)?;
                        subst = subst.compose(&s_pat);
                        t_expr = subst.apply(&t_expr);
                    }

                    let (s_arm, t_arm) = self.algo_w(&mut arm_env, arm_expr)?;
                    subst = subst.compose(&s_arm);

                    let s_unify = self.unify(&subst.apply(&t_match), &subst.apply(&t_arm));
                    let s_unify = self.bind_err_ctx(
                        s_unify,
                        arm_expr.span,
                        Some((
                            "Match arm expressions must have compatible types".into(),
                            vec![],
                        )),
                    )?;
                    subst = subst.compose(&s_unify);

                    t_match = subst.apply(&t_match);
                    let arm_type = subst.apply(&t_arm);
                    t_match.merge_secret_from(&arm_type);
                }

                t_match.merge_secret_from(&t_expr);

                Ok((subst, t_match))
            }

            ExprKind::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                let (s1, mut _cond_type) = self.algo_w(env, condition)?;
                let mut env1 = s1.apply_env(env);

                let cond_requires_secret = !_cond_type.nocryptable();
                if cond_requires_secret {
                    _cond_type.mark_secret(&condition.span);
                }

                //condition must be bool
                let s_cond = self.unify(
                    &s1.apply(&_cond_type),
                    &Ty::new_literal(TyKind::Concrete(self.symbols.intrinsic_types[BOOL])),
                );
                let s_cond = self.bind_err_ctx(
                    s_cond,
                    condition.span.clone(),
                    Some((
                        format!(
                            "If condition must be of type Bool, got {}",
                            _cond_type.render(self)
                        ),
                        vec![],
                    )),
                )?;
                let s1 = s1.compose(&s_cond);
                env1 = s1.apply_env(&env1);

                let (s2, then_type) = self.algo_w(&mut env1, then_branch)?;
                let s3 = s1.compose(&s2);
                let mut env2 = s3.apply_env(&env1);

                let ret = if let Some(else_expr) = else_branch {
                    let (s4, else_type) = self.algo_w(&mut env2, else_expr)?;
                    let s5 = self.unify(&s3.apply(&then_type), &s4.apply(&else_type));
                    let s5 = self.bind_err_ctx(
                        s5,
                        node.span,
                        Some(("Then and else branch types do not match".into(), vec![])),
                    )?;
                    let final_subst = s3.compose(&s4).compose(&s5);
                    let cond_ty = final_subst.apply(&_cond_type);
                    let then_ty = final_subst.apply(&then_type);
                    let else_ty = final_subst.apply(&else_type);
                    let mut final_type = else_ty.clone();
                    final_type.merge_secret_from(&cond_ty);
                    final_type.merge_secret_from(&then_ty);
                    final_type.merge_secret_from(&else_ty);
                    Ok((final_subst, final_type))
                } else {
                    let cond_ty = s3.apply(&_cond_type);
                    let mut then_ty = s3.apply(&then_type);
                    then_ty.merge_secret_from(&cond_ty);
                    Ok((s3, then_ty))
                };

                ret
            }

            ExprKind::Block(statements) => {
                if statements.is_empty() {
                    Ok((Substitution::new(), self.fresh_type_var()))
                } else {
                    let mut subst = Substitution::new();
                    let mut current_env = env.clone();
                    let mut last_type = self.fresh_type_var();
                    let mut let_bound_vars = Vec::new();
                    let mut require_secret = false;

                    for stmt in statements {
                        if let NodeKind::Expr {
                            expr: ExprKind::LetBinding { symbols, .. },
                        } = &stmt.kind
                        {
                            if let Some(symbol_id) = symbols.symbol_id {
                                let_bound_vars.push(symbol_id);
                            }
                        }

                        if let Ok((s, t)) = self.algo_w(&mut current_env, stmt) {
                            subst = subst.compose(&s);
                            current_env = subst.apply_env(&current_env);
                            self.refresh_polymorphic_constructors(&mut current_env);
                            last_type = subst.apply(&t);

                            require_secret = require_secret || last_type.require_secret;
                        }
                    }

                    //generalising bindings at end of block
                    for symbol_id in let_bound_vars {
                        if let Some(scheme) = current_env.get(&symbol_id) {
                            let generalized_scheme = Scheme::generalise(&current_env, &scheme.body);
                            current_env.insert(symbol_id, generalized_scheme);
                        }
                    }

                    Ok((subst, last_type))
                }
            }

            ExprKind::Closure {
                args,
                return_type,
                expr: body,
            } => {
                let mut arg_types = Vec::new();
                let mut new_env = env.clone();
                let mut require_secret = false;

                for (arg_pattern, arg_type, _) in args {
                    let arg_ty = if let Some(type_node) = arg_type {
                        self.type_from_node(type_node.as_mut())?
                    } else {
                        self.fresh_type_var()
                    };
                    arg_types.push(arg_ty.clone());
                    require_secret = require_secret || arg_ty.require_secret;

                    if let NodeKind::Expr {
                        expr: ExprKind::Symbol { .. },
                    } = &arg_pattern.kind
                    {
                        if let Some(symbol_id) = arg_pattern.symbol_id {
                            new_env.insert(
                                symbol_id,
                                Scheme {
                                    bound_vars: vec![],
                                    body: arg_ty,
                                },
                            );
                        }
                    }
                }

                let (s1, body_type) = self.algo_w(&mut new_env, body)?;

                let final_return_type = if let Some(ret_type_node) = return_type {
                    let declared_ret_type = self.type_from_node(ret_type_node.as_mut())?;
                    let s2 = self.unify(&s1.apply(&body_type), &declared_ret_type);
                    let s2 = self.bind_err_ctx(
                        s2,
                        node.span,
                        Some((
                            "Annotated return type does not match inferred type".into(),
                            vec![],
                        )),
                    )?;
                    let final_subst = s1.compose(&s2);
                    (final_subst, s2.apply(&body_type))
                } else {
                    (s1, body_type)
                };

                let mut fn_type = final_return_type.1;
                let mut accumulated_secret = fn_type.require_secret || require_secret;
                for arg_type in arg_types.into_iter().rev() {
                    accumulated_secret = accumulated_secret || arg_type.require_secret;
                    let mut new_fn = Ty::new(TyKind::Fn(
                        Box::new(final_return_type.0.apply(&arg_type)),
                        Box::new(fn_type),
                    ));
                    new_fn.require_secret = accumulated_secret;
                    fn_type = new_fn;
                }

                Ok((final_return_type.0, fn_type))
            }

            ExprKind::Discard => Ok((Substitution::new(), self.fresh_type_var())),

            _ => Ok((Substitution::new(), self.fresh_type_var())),
        }
    }

    fn register_monomorphised_fn(&mut self, callee: &Node, callee_type: &Ty) -> SymbolId {
        //check instantiation table for prior monomorphisation
        if let Some(symbol_id) = callee.symbol_id {
            if let Some(existing_id) = self
                .type_info
                .fn_instantiation_ids
                .get(&(symbol_id, callee_type.clone()))
            {
                return *existing_id;
            }
        }

        let new_symbol_id = self.symbols.symbol_idx;
        //should never fail - symbol ids are always assigned, even to closures
        let symbol_id = callee.symbol_id.unwrap();
        self.symbols.symbol_idx += 1;
        self.symbols.symbol_names.insert(
            new_symbol_id,
            format!(
                "{}${}",
                self.symbols
                    .symbol_names
                    .get(&symbol_id)
                    .unwrap_or(&"<unknown>".into()),
                new_symbol_id
            ),
        );

        self.type_info
            .monomorphised_fns
            .insert(new_symbol_id, (symbol_id, callee_type.clone()));
        self.type_info
            .fn_instantiation_ids
            .insert((symbol_id, callee_type.clone()), new_symbol_id);

        new_symbol_id
    }

    fn initialise_type_env(&mut self) {
        //add intrinsics
        self.type_env = create_intrinsic_type_env(self.symbols, &mut self.type_var_counter);
        self.accepting_binops = create_intrinsic_binops(self.symbols, &self.type_env);
        self.accepting_unops = create_intrinsic_unary_ops(self.symbols, &self.type_env);
    }

    fn build_alias_param_map(
        &mut self,
        alias_node: &Node,
    ) -> Result<(Vec<String>, Vec<TypeVar>, HashMap<String, Ty>), ()> {
        if let NodeKind::Type { type_vars, .. } = &alias_node.kind {
            let mut param_names = Vec::new();
            let mut bound_vars = Vec::new();
            let mut param_map: HashMap<String, Ty> = HashMap::default();

            for type_var in type_vars {
                if let NodeKind::Type { symbol, .. } = &type_var.kind {
                    let var_id = fresh_type_var_id(&mut self.type_var_counter);
                    param_names.push(symbol.clone());
                    bound_vars.push(var_id);
                    param_map.insert(symbol.clone(), Ty::new(TyKind::Var(var_id)));
                }
            }

            Ok((param_names, bound_vars, param_map))
        } else {
            self.errors.push(Diagnostic::new(DiagnosticMsg {
                message: "Invalid type alias declaration".into(),
                span: alias_node.span.clone(),
                file: self.current_file.clone(),
                err_type: DiagnosticMsgType::TypeAliasExpansionFailure,
            }));
            Err(())
        }
    }

    fn collect_alias_dependencies(
        &self,
        alias_symbol_id: SymbolId,
        ty: &Ty,
        deps: &mut HashSet<SymbolId>,
    ) {
        use TyKind::*;
        match &ty.kind {
            Var(_) => {}
            Concrete(id) => {
                if *id == alias_symbol_id || self.type_aliases.contains_key(id) {
                    deps.insert(*id);
                }
            }
            Ctor(id, args) => {
                if *id == alias_symbol_id || self.type_aliases.contains_key(id) {
                    deps.insert(*id);
                }
                for arg in args {
                    self.collect_alias_dependencies(alias_symbol_id, arg, deps);
                }
            }
            Fn(arg, ret) => {
                self.collect_alias_dependencies(alias_symbol_id, arg, deps);
                self.collect_alias_dependencies(alias_symbol_id, ret, deps);
            }
            Tuple(elements) => {
                for elem in elements {
                    self.collect_alias_dependencies(alias_symbol_id, elem, deps);
                }
            }
            Nocrypt(inner) => {
                self.collect_alias_dependencies(alias_symbol_id, inner, deps);
            }
        }
    }

    fn detect_alias_cycle(&self, alias_id: SymbolId, deps: &HashSet<SymbolId>) -> Option<SymbolId> {
        if deps.contains(&alias_id) {
            return Some(alias_id);
        }

        for dep in deps {
            let mut visited = HashSet::default();
            if self.alias_depends_on(*dep, alias_id, &mut visited) {
                return Some(*dep);
            }
        }

        None
    }

    fn alias_depends_on(
        &self,
        source: SymbolId,
        target: SymbolId,
        visited: &mut HashSet<SymbolId>,
    ) -> bool {
        if source == target {
            return true;
        }

        if !visited.insert(source) {
            return false;
        }

        if let Some(children) = self.alias_dependencies.get(&source) {
            if children.contains(&target) {
                return true;
            }
            for child in children {
                if self.alias_depends_on(*child, target, visited) {
                    return true;
                }
            }
        }

        false
    }

    fn instantiate_type_alias(
        &mut self,
        alias_id: SymbolId,
        type_arg_nodes: &mut Vec<Node>,
        type_param_map: &HashMap<String, Ty>,
        span: &CodeSpan,
    ) -> Result<Ty, ()> {
        let alias_info = if let Some(info) = self.type_aliases.get(&alias_id) {
            info.clone()
        } else {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Attempted to use an undefined type alias".into(),
                    span: span.clone(),
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::TypeAliasExpansionFailure,
                },
                notes: vec![],
                hints: vec![
                    "Ensure the type alias is defined before it is used, or import it from the correct module.".into(),
                ],
            });
            return Err(());
        };

        if alias_info.params.len() != type_arg_nodes.len() {
            let alias_name = self
                .symbols
                .symbol_names
                .get(&alias_id)
                .cloned()
                .unwrap_or_else(|| "<unknown>".into());

            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: format!(
                        "Type alias '{}' expects {} type argument(s), but {} were provided",
                        alias_name,
                        alias_info.params.len(),
                        type_arg_nodes.len()
                    ),
                    span: span.clone(),
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::TypeAliasArityMismatch,
                },
                notes: vec![],
                hints: vec![format!(
                    "Supply {} type argument(s) when using '{}'.",
                    alias_info.params.len(),
                    alias_name
                )],
            });
            return Err(());
        }

        let mut arg_types = Vec::new();
        for arg in type_arg_nodes.iter_mut() {
            arg_types.push(self.type_from_node_with_params(arg, type_param_map)?);
        }

        let mut subst_map: HashMap<TypeVar, Ty> = HashMap::default();
        for (var, ty) in alias_info.bound_vars.iter().zip(arg_types.into_iter()) {
            subst_map.insert(*var, ty);
        }

        let substitution = Substitution(subst_map);
        Ok(substitution.apply(&alias_info.body))
    }

    fn type_from_node_with_params(
        &mut self,
        node: &mut Node,
        type_param_map: &HashMap<String, Ty>,
    ) -> Result<Ty, ()> {
        let result = match &mut node.kind {
            NodeKind::Type {
                type_vars,
                symbol,
                nocrypt,
                ..
            } => {
                let ok = |a| {
                    if *nocrypt {
                        Ok(Ty::wrap_nocrypt_deep(a))
                    } else {
                        Ok(a)
                    }
                };

                //check for type param first
                if symbol.chars().next().map_or(false, |c| c.is_lowercase()) {
                    if let Some(param_ty) = type_param_map.get(symbol) {
                        return ok(param_ty.clone());
                    }
                    return ok(self.fresh_type_var());
                }

                let symbol_id = if let Some(id) = node.symbol_id {
                    Some(id)
                } else {
                    self.symbols.intrinsic_types.get(symbol.as_str()).cloned()
                };

                if let Some(symbol_id) = symbol_id {
                    if self.type_aliases.contains_key(&symbol_id) {
                        let alias_ty = self.instantiate_type_alias(
                            symbol_id,
                            type_vars,
                            type_param_map,
                            &node.span,
                        )?;
                        ok(alias_ty)
                    } else if type_vars.is_empty() {
                        ok(Ty::new(TyKind::Concrete(symbol_id)))
                    } else {
                        let mut args = Vec::new();
                        for type_var in type_vars.iter_mut() {
                            args.push(self.type_from_node_with_params(type_var, type_param_map)?);
                        }
                        ok(Ty::new(TyKind::Ctor(symbol_id, args)))
                    }
                } else {
                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                        message: format!(
                            "Unresolved type with id {:?}: {:?}",
                            node.symbol_id, node.kind
                        ),
                        span: node.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::UndefinedType,
                    }));
                    Err(())
                }
            }
            NodeKind::FnType { args, return_type } => {
                let mut arg_types = Vec::new();
                for arg in args.iter_mut() {
                    arg_types.push(self.type_from_node_with_params(arg, type_param_map)?);
                }

                let ret_type = if let Some(ret) = return_type {
                    self.type_from_node_with_params(ret.as_mut(), type_param_map)?
                } else {
                    self.fresh_type_var()
                };

                let mut fn_type = ret_type;
                let mut accumulated_secret = fn_type.require_secret;
                for arg_type in arg_types.into_iter().rev() {
                    accumulated_secret = accumulated_secret || arg_type.require_secret;
                    let mut new_fn = Ty::new(TyKind::Fn(Box::new(arg_type), Box::new(fn_type)));
                    new_fn.require_secret = accumulated_secret;
                    fn_type = new_fn;
                }

                Ok(fn_type)
            }
            NodeKind::TupleType { elements } => {
                let mut types = Vec::new();
                for elem in elements.iter_mut() {
                    types.push(self.type_from_node_with_params(elem, type_param_map)?);
                }
                Ok(Ty::new(TyKind::Tuple(types)))
            }
            _ => {
                self.errors.push(Diagnostic::new(DiagnosticMsg {
                    message: format!("Invalid type node: {:?}", node.kind),
                    span: node.span.clone(),
                    file: self.current_file.clone(),
                    err_type: DiagnosticMsgType::UndefinedType,
                }));
                Err(())
            }
        };

        if let Ok(ty) = &result {
            node.ty = Some(ty.clone());
        }

        result
    }

    fn type_from_node(&mut self, node: &mut Node) -> Result<Ty, ()> {
        let empty_map = HashMap::default();
        self.type_from_node_with_params(node, &empty_map)
    }

    fn fresh_type_var(&mut self) -> Ty {
        fresh_type_var(&mut self.type_var_counter)
    }

    fn refresh_polymorphic_constructors(&self, env: &mut TypeEnv) {
        for (symbol_id, global_scheme) in &self.type_env {
            if !global_scheme.bound_vars.is_empty() && env.contains_key(symbol_id) {
                env.insert(*symbol_id, global_scheme.clone());
            }
        }
    }

    fn add_declaration_to_env(&mut self, node: &Node, ty: &Ty, env: &mut TypeEnv) {
        match &node.kind {
            NodeKind::ConstDecl { symbol, .. } => {
                if let NodeKind::Expr {
                    expr: ExprKind::Symbol { .. },
                } = &symbol.kind
                {
                    if let Some(symbol_id) = symbol.symbol_id {
                        let scheme = if self.is_nonexpansive_node(node) {
                            Scheme::generalise(env, ty)
                        } else {
                            Scheme {
                                bound_vars: vec![],
                                body: ty.clone(),
                            }
                        };
                        env.insert(symbol_id, scheme);
                        self.type_env
                            .insert(symbol_id, env.get(&symbol_id).unwrap().clone());
                    }
                }
            }
            NodeKind::FnDecl { args, .. } => {
                if let Some(symbol_id) = node.symbol_id {
                    let scheme = Scheme::generalise(env, ty);
                    //bound vars empty -> monotype, can just register directly
                    if scheme.bound_vars.is_empty() {
                        self.register_monomorphised_fn(node, ty);
                        //monomorphisations of parametric functions will occur at call sites
                    }

                    //check for prior decl, if exists, unify
                    let scheme = if let Some(existing_scheme) = env.get(&symbol_id) {
                        let s = self
                            .unify(&existing_scheme.body, ty)
                            .expect("Failed to unify function declaration with prior declaration");
                        let new_type = s.apply(ty);
                        Scheme::generalise(env, &new_type)
                    } else {
                        scheme
                    };

                    env.insert(symbol_id, scheme.clone());
                    self.type_env.insert(symbol_id, scheme);

                    //record arg labels for reordering at call sites
                    let arg_labels = args
                        .iter()
                        .map(|(pattern, _, _)| {
                            if let NodeKind::Expr {
                                expr: ExprKind::Symbol { name },
                            } = &pattern.kind
                            {
                                Some(name.clone())
                            } else {
                                None
                            }
                        })
                        .filter_map(|label| label)
                        .collect::<Vec<_>>();
                    self.type_info.fn_arg_labels.insert(symbol_id, arg_labels);
                }
            }
            _ => {}
        }
    }

    fn is_nonexpansive_node(&self, node: &Node) -> bool {
        match &node.kind {
            NodeKind::Expr { expr } => self.is_nonexpansive(expr),
            NodeKind::ConstDecl { expr, .. } => {
                if let NodeKind::Expr { expr } = &expr.kind {
                    self.is_nonexpansive(expr)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn typecheck_pattern(
        &mut self,
        env: &mut TypeEnv,
        pattern: &Node,
        expected_type: &Ty,
        in_match: bool,
    ) -> Result<Substitution, ()> {
        if let NodeKind::Expr { expr } = &pattern.kind {
            use ExprKind::*;
            match expr {
                Symbol { .. } => {
                    if let Some(symbol_id) = pattern.symbol_id {
                        //zero-arg constructor pattern
                        if self.symbols.type_constructors.contains_key(&symbol_id) {
                            if let Some(constructor_scheme) = self.type_env.get(&symbol_id) {
                                let constructor_type = constructor_scheme
                                    .clone()
                                    .instantiate(&mut self.type_var_counter);

                                let arg_types = self.get_constructor_arg_types(&constructor_type);
                                if !arg_types.is_empty() {
                                    let ctor_name = self
                                        .symbols
                                        .symbol_names
                                        .get(&symbol_id)
                                        .cloned()
                                        .unwrap_or_else(|| "<unknown>".into());
                                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                                        message: format!(
                                            "Constructor {} expects {} arguments but pattern has 0",
                                            ctor_name,
                                            arg_types.len()
                                        ),
                                        span: pattern.span.clone(),
                                        file: self.current_file.clone(),
                                        err_type: DiagnosticMsgType::TypeMismatch,
                                    }));
                                    return Err(());
                                }

                                let return_type =
                                    self.get_constructor_return_type(&constructor_type);
                                let (expected_core, _expect_nocrypt) = match &expected_type.kind {
                                    TyKind::Nocrypt(inner) => (&**inner, true),
                                    _ => (expected_type, false),
                                };

                                let s_unify = self.unify(&return_type, expected_core);
                                let s_unify = self.bind_err_ctx(
                                    s_unify,
                                    pattern.span,
                                    Some((
                                        format!(
                                            "Constructor pattern {} does not match expected type {}",
                                            self.symbols.symbol_names
                                                .get(&symbol_id)
                                                .cloned()
                                                .unwrap_or_else(|| "<unknown>".into()),
                                            expected_type.render(self)
                                        ),
                                        vec![],
                                    )),
                                )?;

                                Ok(s_unify)
                            } else {
                                self.errors.push(Diagnostic::new(DiagnosticMsg {
                                    message: "Unknown constructor in pattern".into(),
                                    span: pattern.span.clone(),
                                    file: self.current_file.clone(),
                                    err_type: DiagnosticMsgType::UndeclaredSymbol,
                                }));
                                Err(())
                            }
                        } else {
                            if let Some(existing_scheme) = env.get(&symbol_id) {
                                let s = self.unify(&existing_scheme.body, expected_type);
                                return self.bind_err_ctx(
                                    s,
                                    pattern.span,
                                    Some(("Pattern variable types do not match".into(), vec![])),
                                );
                            } else {
                                env.insert(
                                    symbol_id,
                                    Scheme {
                                        bound_vars: vec![],
                                        body: expected_type.clone(),
                                    },
                                );
                            }

                            Ok(Substitution::new())
                        }
                    } else {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: "Pattern variable without symbol id".to_string(),
                            span: pattern.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::TypeMismatch,
                        }));
                        Err(())
                    }
                }
                Discard => Ok(Substitution::new()),
                ListPattern(elements) => {
                    use crate::ast::ListPatternElement;
                    match &expected_type.kind {
                        TyKind::Ctor(ctor_id, type_args)
                            if *ctor_id == self.symbols.intrinsic_types[LIST] =>
                        {
                            let elem_type = &type_args[0];
                            let mut subst = Substitution::new();

                            for elem in elements {
                                match elem {
                                    ListPatternElement::Element(node) => {
                                        let s = self.typecheck_pattern(
                                            env,
                                            node,
                                            &subst.apply(elem_type),
                                            in_match,
                                        )?;
                                        subst = subst.compose(&s);
                                    }
                                    ListPatternElement::Rest(Some(node)) => {
                                        let s = self.typecheck_pattern(
                                            env,
                                            node,
                                            &subst.apply(expected_type),
                                            in_match,
                                        )?;
                                        subst = subst.compose(&s);
                                    }
                                    ListPatternElement::Rest(None) => {
                                        //
                                    }
                                }
                            }
                            Ok(subst)
                        }
                        _ => {
                            self.errors.push(Diagnostic::new(DiagnosticMsg {
                                message: format!(
                                    "Expected list type for list pattern, found {}",
                                    expected_type.render(&self)
                                ),
                                span: pattern.span.clone(),
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::TypeMismatch,
                            }));
                            Err(())
                        }
                    }
                }
                TupleLit(elements) => match &expected_type.kind {
                    TyKind::Tuple(element_types) => {
                        if elements.len() != element_types.len() {
                            self.errors.push(Diagnostic::new(DiagnosticMsg {
                                message: format!(
                                    "Tuple pattern has {} elements but type has {}",
                                    elements.len(),
                                    element_types.len()
                                ),
                                span: pattern.span.clone(),
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::TypeMismatch,
                            }));
                            return Err(());
                        }

                        let mut subst = Substitution::new();
                        for (element_pattern, element_type) in
                            elements.iter().zip(element_types.iter())
                        {
                            let s = self.typecheck_pattern(
                                env,
                                element_pattern,
                                &subst.apply(element_type),
                                in_match,
                            )?;
                            subst = subst.compose(&s);
                        }
                        Ok(subst)
                    }
                    _ => {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: format!(
                                "Expected tuple type for tuple pattern, got {}",
                                expected_type.render(self)
                            ),
                            span: pattern.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::TypeMismatch,
                        }));
                        Err(())
                    }
                },
                FnCall { callee, args } => {
                    if let NodeKind::Expr {
                        expr: ExprKind::Symbol { name, .. },
                    } = &callee.kind
                    {
                        let constructor_symbol_id = callee.symbol_id;
                        if let Some(constructor_id) = constructor_symbol_id {
                            if let Some(constructor_scheme) = self.type_env.get(&constructor_id) {
                                let constructor_type = constructor_scheme
                                    .clone()
                                    .instantiate(&mut self.type_var_counter);

                                let return_type =
                                    self.get_constructor_return_type(&constructor_type);

                                let (expected_core, expect_nocrypt) = match &expected_type.kind {
                                    TyKind::Nocrypt(inner) => (&**inner, true),
                                    _ => (expected_type, false),
                                };

                                let s_unify = self.unify(&return_type, expected_core);
                                let s_unify = self.bind_err_ctx(s_unify, pattern.span, Some((format!(
                                    "Constructor pattern {} does not match expected type {}",
                                    name,
                                    expected_type.render(self)
                                ), vec![])))?;

                                let arg_types = self.get_constructor_arg_types(&constructor_type);
                                let instantiated_arg_types: Vec<Ty> = arg_types
                                    .iter()
                                    .map(|ty| {
                                        let inst = s_unify.apply(ty);
                                        if expect_nocrypt {
                                            Ty::wrap_nocrypt_deep(inst)
                                        } else {
                                            inst
                                        }
                                    })
                                    .collect();

                                if args.len() != instantiated_arg_types.len() {
                                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                                        message: format!(
                                            "Constructor {} expects {} arguments but pattern has {}",
                                            name,
                                            instantiated_arg_types.len(),
                                            args.len()
                                        ),
                                        span: pattern.span.clone(),
                                        file: self.current_file.clone(),
                                        err_type: DiagnosticMsgType::TypeMismatch,
                                    }));
                                    return Err(());
                                }

                                let mut s_args = Substitution::new();
                                for (arg_pattern, arg_type) in
                                    args.iter().zip(instantiated_arg_types.iter())
                                {
                                    let s_arg = self.typecheck_pattern(
                                        env,
                                        &arg_pattern.1,
                                        &s_args.apply(arg_type),
                                        in_match,
                                    )?;
                                    s_args = s_args.compose(&s_arg);
                                }

                                Ok(s_unify.compose(&s_args))
                            } else {
                                self.errors.push(Diagnostic::new(DiagnosticMsg {
                                    message: format!("Unknown constructor: {}", name),
                                    span: pattern.span.clone(),
                                    file: self.current_file.clone(),
                                    err_type: DiagnosticMsgType::UndeclaredSymbol,
                                }));
                                Err(())
                            }
                        } else {
                            self.errors.push(Diagnostic::new(DiagnosticMsg {
                                message: format!("Constructor {} not resolved", name),
                                span: pattern.span.clone(),
                                file: self.current_file.clone(),
                                err_type: DiagnosticMsgType::UndeclaredSymbol,
                            }));
                            Err(())
                        }
                    } else {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: "Function call patterns must use constructor names"
                                .to_string(),
                            span: pattern.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::InvalidBindingPattern,
                        }));
                        Err(())
                    }
                }
                IntLit(value) => {
                    //int lit pattern - todo - add type coercion rules
                    let int_type = Ty::new_literal(TyKind::Concrete(
                        self.symbols.intrinsic_types[crate::intrinsics::INT],
                    ));

                    let s_unify = self.unify(&int_type, expected_type);
                    self.bind_err_ctx(
                        s_unify,
                        pattern.span,
                        Some((
                            format!(
                                "Integer literal {} does not match expected type {}",
                                value,
                                expected_type.render(self)
                            ),
                            vec![],
                        )),
                    )
                }
                RealLit { value, .. } => {
                    let real_type = Ty::new_literal(TyKind::Concrete(
                        self.symbols.intrinsic_types[crate::intrinsics::REAL],
                    ));

                    let s_unify = self.unify(&real_type, expected_type);
                    self.bind_err_ctx(
                        s_unify,
                        pattern.span,
                        Some((
                            format!(
                                "Real literal {} does not match expected type {}",
                                value,
                                expected_type.render(self)
                            ),
                            vec![],
                        )),
                    )
                }
                StringLit(value) => {
                    let string_type = Ty::new_literal(TyKind::Concrete(
                        self.symbols.intrinsic_types[crate::intrinsics::STRING],
                    ));

                    let s_unify = self.unify(&string_type, expected_type);
                    self.bind_err_ctx(
                        s_unify,
                        pattern.span,
                        Some((
                            format!(
                                "String literal \"{}\" does not match expected type {}",
                                value,
                                expected_type.render(self)
                            ),
                            vec![],
                        )),
                    )
                }
                ListLit(elements) => match &expected_type.kind {
                    TyKind::Ctor(ctor_id, type_args)
                        if *ctor_id == self.symbols.intrinsic_types[crate::intrinsics::LIST] =>
                    {
                        let elem_type = &type_args[0];
                        let mut subst = Substitution::new();

                        for elem in elements {
                            let s = self.typecheck_pattern(
                                env,
                                elem,
                                &subst.apply(elem_type),
                                in_match,
                            )?;
                            subst = subst.compose(&s);
                        }
                        Ok(subst)
                    }
                    _ => {
                        self.errors.push(Diagnostic::new(DiagnosticMsg {
                            message: format!(
                                "Expected list type for list literal pattern, found {}",
                                expected_type.render(&self)
                            ),
                            span: pattern.span.clone(),
                            file: self.current_file.clone(),
                            err_type: DiagnosticMsgType::TypeMismatch,
                        }));
                        Err(())
                    }
                },
                //todo: add map literal patterns
                MapLit(_) => {
                    let message = if in_match {
                        "Map literal patterns are not supported in match expressions".to_string()
                    } else {
                        "Map literal patterns are not supported in let bindings".to_string()
                    };
                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                        message,
                        span: pattern.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::InvalidBindingPattern,
                    }));
                    Err(())
                }
                _ => {
                    let message = if in_match {
                        "Invalid pattern in match expression".to_string()
                    } else {
                        "Invalid pattern in let binding - only identifiers, tuples, and literals are allowed".to_string()
                    };
                    self.errors.push(Diagnostic::new(DiagnosticMsg {
                        message,
                        span: pattern.span.clone(),
                        file: self.current_file.clone(),
                        err_type: DiagnosticMsgType::InvalidBindingPattern,
                    }));
                    Err(())
                }
            }
        } else {
            let message = if in_match {
                "Invalid pattern structure in match expression".to_string()
            } else {
                "Invalid pattern structure in let binding".to_string()
            };
            self.errors.push(Diagnostic::new(DiagnosticMsg {
                message,
                span: pattern.span.clone(),
                file: self.current_file.clone(),
                err_type: DiagnosticMsgType::InvalidBindingPattern,
            }));
            Err(())
        }
    }

    fn get_constructor_return_type(&self, constructor_type: &Ty) -> Ty {
        let mut current: &Ty = constructor_type;
        while let TyKind::Fn(_, ret) = &current.kind {
            current = ret.as_ref();
        }
        current.clone()
    }

    fn get_constructor_arg_types(&self, constructor_type: &Ty) -> Vec<Ty> {
        let mut args = Vec::new();
        let mut current = constructor_type;

        while let TyKind::Fn(arg, ret) = &current.kind {
            args.push((**arg).clone());
            current = ret;
        }

        args
    }

    fn is_nonexpansive(&self, e: &ExprKind) -> bool {
        match e {
            ExprKind::IntLit(_)
            | ExprKind::RealLit { .. }
            | ExprKind::StringLit(_)
            | ExprKind::Discard => true,
            ExprKind::TupleLit(xs) | ExprKind::ListLit(xs) => xs
                .iter()
                .all(|n| matches!(&n.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))),
            ExprKind::ListPattern(elements) => {
                use crate::ast::ListPatternElement;
                elements.iter().all(|elem| match elem {
                    ListPatternElement::Element(node) => {
                        matches!(&node.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))
                    }
                    ListPatternElement::Rest(Some(node)) => {
                        matches!(&node.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))
                    }
                    ListPatternElement::Rest(None) => true,
                })
            }
            ExprKind::MapLit(kvs) => kvs.iter().all(|(k, v)| {
                matches!(&k.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))
                    && matches!(&v.kind, NodeKind::Expr{expr} if self.is_nonexpansive(expr))
            }),
            ExprKind::Closure { .. } => true,
            ExprKind::Symbol { .. } => true,
            _ => false,
        }
    }

    fn coerce_literal_to_type(&self, literal_ty: &Ty, target_ty: &Ty) -> Option<Ty> {
        use TyKind::*;

        if !literal_ty.literal {
            return None;
        }

        match (&literal_ty.kind, &target_ty.kind) {
            //IntLit -> any numeric type
            (Concrete(lit_id), Concrete(_))
                if *lit_id == self.symbols.intrinsic_types[INT]
                    && target_ty.is_integer_type(&self.symbols) =>
            {
                let mut result = target_ty.clone();
                result.literal = false;
                Some(result)
            }

            (Concrete(lit_id), Concrete(_))
                if *lit_id == self.symbols.intrinsic_types[REAL]
                    && target_ty.is_real_type(&self.symbols) =>
            {
                let mut result = target_ty.clone();
                result.literal = false;
                Some(result)
            }

            (Concrete(lit_id), Concrete(_))
                if *lit_id == self.symbols.intrinsic_types[INT]
                    && target_ty.is_real_type(&self.symbols) =>
            {
                let mut result = target_ty.clone();
                result.literal = false;
                Some(result)
            }

            //arbitrary nocrypt status
            (_, Nocrypt(inner)) => self
                .coerce_literal_to_type(literal_ty, inner)
                .map(|coerced| Ty::wrap_nocrypt(coerced)),

            _ => None,
        }
    }

    fn occurs(&mut self, a: TypeVar, t: &Ty) -> bool {
        use TyKind::*;
        match &t.kind {
            Var(b) => a == *b,
            Fn(arg, ret) => self.occurs(a, arg) || self.occurs(a, ret),
            Ctor(_, args) | Tuple(args) => args.iter().any(|arg| self.occurs(a, arg)),
            Concrete(_) => false,
            Nocrypt(t) => self.occurs(a, t),
        }
    }

    fn bind(&mut self, a: TypeVar, t: &Ty) -> Result<Substitution, TypeError> {
        if let TyKind::Var(b) = &t.kind
            && *b == a
        {
            return Ok(Substitution::new());
        }

        if self.occurs(a, t) {
            return Err(TypeError {
                kind: TypeErrorKind::InfiniteType,
                types: vec![Ty::new(TyKind::Var(a)), t.clone()],
                hints: vec![],
            });
        }

        let mut m = HashMap::default();
        m.insert(a, t.clone());

        Ok(Substitution(m))
    }

    fn unify(&mut self, t1: &Ty, t2: &Ty) -> Result<Substitution, TypeError> {
        use TyKind::*;
        match (&t1.kind, &t2.kind) {
            (a, b) if a == b => Ok(Substitution::new()),

            (Var(a), Var(b)) if a == b => Ok(Substitution::new()),
            (Var(a), _) => self.bind(*a, t2),
            (_, Var(b)) => self.bind(*b, t1),

            (Concrete(a), Concrete(b)) if a == b => Ok(Substitution::new()),

            (Fn(arg1, ret1), Fn(arg2, ret2)) => {
                let s1 = self.unify(arg1, arg2)?;
                let s2 = self.unify(&s1.apply(ret1), &s1.apply(ret2))?;
                Ok(s1.compose(&s2))
            }

            (Nocrypt(a), Nocrypt(b)) => self.unify(a, b),
            (Nocrypt(a), _) if t2.nocryptable() => self.unify(a, t2),
            (_, Nocrypt(b)) if t1.nocryptable() => self.unify(t1, b),
            (Nocrypt(_), _) | (_, Nocrypt(_)) => Err(TypeError {
                kind: TypeErrorKind::UnificationFail,
                types: vec![t1.clone(), t2.clone()],
                hints: vec!["Attempted to mix usage of encrypted and 'nocrypt' types - remove/add 'nocrypt' type modifiers to fix.".into()],
            }),


            (Concrete(id1), Concrete(_id2))
                if t1.literal
                    && !t2.literal
                    && *id1 == self.symbols.intrinsic_types[INT]
                    && t2.is_integer_type(&self.symbols) =>
            {
                Ok(Substitution::new())
            }

            //literal real -> any real/fixed
            (Concrete(id1), Concrete(_id2))
                if t1.literal
                    && !t2.literal
                    && *id1 == self.symbols.intrinsic_types[REAL]
                    && t2.is_real_type(&self.symbols) =>
            {
                Ok(Substitution::new())
            }

            (Concrete(id1), Concrete(id2))
                if id1 == id2
                    && (t1.literal || t2.literal)
                    && !(t1.literal && t2.literal)
                    && (t1.is_numeric_type(&self.symbols) || t2.is_numeric_type(&self.symbols)) =>
            {
                Ok(Substitution::new())
            }

            (Tuple(types1), Tuple(types2)) if types1.len() == types2.len() => {
                let mut s = Substitution::new();
                for (ty1, ty2) in types1.iter().zip(types2.iter()) {
                    let s2 = self.unify(&s.apply(ty1), &s.apply(ty2))?;
                    s = s.compose(&s2);
                }
                Ok(s)
            }

            (Tuple(types1), Tuple(types2)) => Err(TypeError {
                kind: TypeErrorKind::UnificationFail,
                types: vec![t1.clone(), t2.clone()],
                hints: vec![format!(
                    "These tuples are of different lengths - {} and {}.",
                    types1.len(),
                    types2.len()
                )],
            }),

            (Tuple(_), _) | (_, Tuple(_)) => Err(TypeError {
                kind: TypeErrorKind::UnificationFail,
                types: vec![t1.clone(), t2.clone()],
                hints: vec!["One of these is a tuple, and the other is not.".into()],
            }),

            //todo: this might not be right - len check?
            (Ctor(id1, args1), Ctor(id2, args2)) if id1 == id2 && args1.len() == args2.len() => {
                let mut s = Substitution::new();
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    let s2 = self.unify(&s.apply(arg1), &s.apply(arg2))?;
                    s = s.compose(&s2);
                }
                Ok(s)
            }
            (Ctor(id1, args1), Ctor(id2, args2))
                if args1.len() == args2.len()
                    && ((t1.literal
                        && *id1 == self.symbols.intrinsic_types[LIST]
                        && *id2 == self.symbols.intrinsic_types[ARRAY])
                        || (t2.literal
                            && *id2 == self.symbols.intrinsic_types[LIST]
                            && *id1 == self.symbols.intrinsic_types[ARRAY])) =>
            {
                let mut s = Substitution::new();
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    let s2 = self.unify(&s.apply(arg1), &s.apply(arg2))?;
                    s = s.compose(&s2);
                }
                Ok(s)
            }

            _ => Err(TypeError {
                kind: TypeErrorKind::UnificationFail,
                types: vec![t1.clone(), t2.clone()],
                hints: vec![],
            }),
        }
    }
}

impl Ty {
    pub fn new(kind: TyKind) -> Self {
        Ty {
            kind,
            literal: false,
            require_secret: false,
            secret_origin: None,
        }
    }

    pub fn new_literal(kind: TyKind) -> Self {
        Ty {
            kind,
            literal: true,
            require_secret: false,
            secret_origin: None,
        }
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }

    pub fn requires_secret(&self) -> bool {
        !self.nocryptable()
    }

    fn mark_secret(&mut self, span: &CodeSpan) {
        if !self.require_secret {
            self.require_secret = true;
            self.secret_origin = Some(span.clone());
        }
    }

    fn merge_secret_from(&mut self, other: &Ty) {
        if other.require_secret {
            self.require_secret = true;
            if self.secret_origin.is_none() {
                self.secret_origin = other.secret_origin.clone();
            }
        }
    }

    fn wrap_nocrypt(inner: Ty) -> Ty {
        let mut outer = Ty::new(TyKind::Nocrypt(Box::new(inner)));
        if let TyKind::Nocrypt(ref boxed) = outer.kind {
            outer.literal = boxed.literal;
        }
        outer.require_secret = false;
        outer.secret_origin = None;
        outer
    }

    fn wrap_nocrypt_deep(inner: Ty) -> Ty {
        if matches!(inner.kind, TyKind::Nocrypt(_)) {
            return inner;
        }

        let Ty {
            kind,
            literal,
            require_secret,
            secret_origin,
        } = inner;

        let kind = match kind {
            TyKind::Fn(arg, ret) => TyKind::Fn(
                Box::new(Ty::wrap_nocrypt_deep(*arg)),
                Box::new(Ty::wrap_nocrypt_deep(*ret)),
            ),
            TyKind::Tuple(types) => {
                TyKind::Tuple(types.into_iter().map(Ty::wrap_nocrypt_deep).collect())
            }
            TyKind::Ctor(id, args) => {
                TyKind::Ctor(id, args.into_iter().map(Ty::wrap_nocrypt_deep).collect())
            }
            other => other,
        };

        let rebuilt = Ty {
            kind,
            literal,
            require_secret,
            secret_origin,
        };

        Ty::wrap_nocrypt(rebuilt)
    }

    fn free_type_vars(&self) -> HashSet<TypeVar> {
        use TyKind::*;
        match &self.kind {
            Var(a) => {
                let mut set = HashSet::default();
                set.insert(*a);
                set
            }
            Fn(arg, ret) => {
                let mut set = arg.free_type_vars();
                set.extend(ret.free_type_vars());
                set
            }
            Tuple(types) => {
                let mut set = HashSet::default();
                for ty in types {
                    set.extend(ty.free_type_vars());
                }
                set
            }
            Ctor(_, args) => {
                let mut set = HashSet::default();
                for arg in args {
                    set.extend(arg.free_type_vars());
                }
                set
            }
            Concrete(_) => HashSet::default(),
            Nocrypt(t) => t.free_type_vars(),
        }
    }

    fn free_type_vars_in_scheme(scheme: &Scheme) -> HashSet<TypeVar> {
        let mut set = scheme.body.free_type_vars();
        for var in &scheme.bound_vars {
            set.remove(var);
        }
        set
    }

    fn free_type_vars_in_env(env: &TypeEnv) -> HashSet<TypeVar> {
        let mut set = HashSet::default();
        for scheme in env.values() {
            set.extend(Ty::free_type_vars_in_scheme(scheme));
        }
        set
    }

    fn nocryptable(&self) -> bool {
        use TyKind::*;
        match &self.kind {
            Nocrypt(_) => true,
            Var(_) | Concrete(_) => self.literal,
            Fn(arg, ret) => arg.nocryptable() && ret.nocryptable(),
            Tuple(types) => types.iter().all(|ty| ty.nocryptable()),
            Ctor(_, args) => args.iter().all(|arg| arg.nocryptable()),
        }
    }

    fn is_integer_type(&self, symbols: &SymbolContext) -> bool {
        use crate::intrinsics::{I8, I16, I32, I64, INT, U8, U16, U32, U64};
        if let TyKind::Concrete(id) = &self.kind {
            *id == symbols.intrinsic_types[INT]
                || *id == symbols.intrinsic_types[I8]
                || *id == symbols.intrinsic_types[U8]
                || *id == symbols.intrinsic_types[I16]
                || *id == symbols.intrinsic_types[U16]
                || *id == symbols.intrinsic_types[I32]
                || *id == symbols.intrinsic_types[U32]
                || *id == symbols.intrinsic_types[I64]
                || *id == symbols.intrinsic_types[U64]
        } else {
            false
        }
    }

    fn is_real_type(&self, symbols: &SymbolContext) -> bool {
        use crate::intrinsics::{FIXED1, FIXED2, FIXED4, REAL};
        if let TyKind::Concrete(id) = &self.kind {
            *id == symbols.intrinsic_types[REAL]
                || *id == symbols.intrinsic_types[FIXED1]
                || *id == symbols.intrinsic_types[FIXED2]
                || *id == symbols.intrinsic_types[FIXED4]
        } else {
            false
        }
    }

    fn is_numeric_type(&self, symbols: &SymbolContext) -> bool {
        self.is_integer_type(symbols) || self.is_real_type(symbols)
    }

    fn fn_args(&self) -> Vec<Ty> {
        use TyKind::*;
        let mut args = Vec::new();
        let mut current = self;
        while let Fn(arg, ret) = &current.kind {
            args.push((**arg).clone());
            current = ret;
        }
        args
    }

    fn render(&self, ctx: &TypecheckContext) -> String {
        use TyKind::*;
        match &self.kind {
            Var(a) => format!("T{}", a),
            Concrete(id) => ctx
                .symbols
                .symbol_names
                .get(id)
                .cloned()
                .unwrap_or("<unknown>".to_string()),
            Fn(_, _) => {
                let (args, ret) = self.render_curried_fn(ctx, &self, String::new());
                format!("({}) -> {}", args, ret)
            }
            Tuple(types) => {
                let elems: Vec<String> = types.iter().map(|ty| ty.render(ctx)).collect();
                format!("#({})", elems.join(", "))
            }
            Nocrypt(t) => format!("nocrypt {}", t.render(ctx)),
            Ctor(id, args) => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.render(ctx)).collect();

                format!(
                    "{}({})",
                    ctx.symbols
                        .symbol_names
                        .get(id)
                        .cloned()
                        .unwrap_or("<unknown>".to_string()),
                    args_str.join(", ")
                )
            }
        }
    }

    fn render_curried_fn(
        &self,
        ctx: &TypecheckContext,
        next: &Ty,
        arg_buf: String,
    ) -> (String, String) {
        use TyKind::*;
        match &next.kind {
            Fn(arg, ret) => {
                let arg_str = arg.render(ctx);
                let new_arg_buf = if arg_buf.is_empty() {
                    arg_str
                } else {
                    format!("{}, {}", arg_buf, arg_str)
                };
                self.render_curried_fn(ctx, ret, new_arg_buf)
            }
            _ => {
                let ret_str = next.render(ctx);
                (arg_buf, ret_str)
            }
        }
    }
}

impl Scheme {
    fn instantiate(self, counter: &mut TypeVar) -> Ty {
        let mut subst = Substitution::new();
        for &var in &self.bound_vars {
            subst.0.insert(var, fresh_type_var(counter));
        }
        subst.apply(&self.body)
    }

    fn generalise(env: &TypeEnv, t: &Ty) -> Scheme {
        let env_vars = Ty::free_type_vars_in_env(env);
        let type_vars = t.free_type_vars();
        let bound_vars: Vec<_> = type_vars.difference(&env_vars).cloned().collect();
        Scheme {
            bound_vars,
            body: t.clone(),
        }
    }
}

impl Substitution {
    pub fn new() -> Self {
        Substitution(HashMap::default())
    }

    fn compose(&self, other: &Substitution) -> Substitution {
        other.apply_to_substitution(&self);
        let mut new_map = self.0.clone();
        new_map.extend(other.0.clone());

        Substitution(new_map)
    }

    fn apply_to_substitution(&self, other: &Substitution) -> Substitution {
        let new_map = other.0.iter().map(|(k, v)| (*k, self.apply(v))).collect();
        Substitution(new_map)
    }

    pub fn apply(&self, t: &Ty) -> Ty {
        use TyKind::*;
        match &t.kind {
            Var(a) => self.0.get(a).cloned().unwrap_or_else(|| {
                let mut ty = Ty::new(Var(*a));
                ty.literal = t.literal;
                ty.require_secret = t.require_secret;
                ty.secret_origin = t.secret_origin.clone();
                ty
            }),
            Fn(arg, ret) => {
                let new_arg = self.apply(arg);
                let new_ret = self.apply(ret);
                let mut ty = Ty::new(Fn(Box::new(new_arg), Box::new(new_ret)));
                ty.literal = t.literal;
                ty.require_secret = t.require_secret;
                ty.secret_origin = t.secret_origin.clone();
                ty
            }
            Tuple(types) => {
                let new_types = types.iter().map(|ty| self.apply(ty)).collect();
                let mut ty = Ty::new(Tuple(new_types));
                ty.literal = t.literal;
                ty.require_secret = t.require_secret;
                ty.secret_origin = t.secret_origin.clone();
                ty
            }
            Ctor(name, args) => {
                let new_args = args.iter().map(|arg| self.apply(arg)).collect();
                let mut ty = Ty::new(Ctor(*name, new_args));
                ty.literal = t.literal;
                ty.require_secret = t.require_secret;
                ty.secret_origin = t.secret_origin.clone();
                ty
            }
            Concrete(_) => t.clone(),
            Nocrypt(t_inner) => {
                let mut ty = Ty::new(Nocrypt(Box::new(self.apply(t_inner))));
                ty.literal = t.literal;
                ty.require_secret = t.require_secret;
                ty.secret_origin = t.secret_origin.clone();
                ty
            }
        }
    }

    fn apply_scheme(&self, scheme: &Scheme) -> Scheme {
        //skip bound vars
        let mask: HashSet<_> = scheme.bound_vars.iter().cloned().collect();
        let filtered = Substitution(
            self.0
                .iter()
                .filter(|(k, _)| !mask.contains(k))
                .map(|(k, v)| (*k, v.clone()))
                .collect(),
        );
        Scheme {
            bound_vars: scheme.bound_vars.clone(),
            body: filtered.apply(&scheme.body),
        }
    }

    fn apply_env(&self, env: &TypeEnv) -> TypeEnv {
        env.iter()
            .map(|(k, v)| (*k, self.apply_scheme(v)))
            .collect()
    }

    #[allow(dead_code)]
    fn render(&self, ctx: &TypecheckContext) -> String {
        let mut parts: Vec<String> = Vec::new();
        for (var, ty) in &self.0 {
            parts.push(format!("T{} => {}", var, ty.render(ctx)));
        }
        format!("{{{}}}", parts.join(", "))
    }
}

pub fn fresh_type_var_id(counter: &mut TypeVar) -> TypeVar {
    let var = *counter;
    *counter += 1;
    var
}

pub fn fresh_type_var(counter: &mut TypeVar) -> Ty {
    Ty::new(TyKind::Var(fresh_type_var_id(counter)))
}
