use std::collections::HashMap;

use petgraph::{
    algo::{kosaraju_scc, toposort},
    prelude::DiGraphMap,
};

use crate::{
    ast::ast::{ExprNode, ForVariant, Node, Variant},
    compile::CompileContext,
    diagnostic::{Diagnostic, DiagnosticMsg, DiagnosticMsgType},
    log::Logger,
    parse::CodeSpan,
    symbol::{INVALID_SYMBOL_ID, PrimitiveType, Symbol, TypeVariant},
    visit_ast_children,
};

pub type ModuleId = usize;
pub const INVALID_MODULE_ID: ModuleId = usize::MAX;

#[derive(Debug, Clone)]
pub struct ModuleTable {
    pub modules: HashMap<ModuleId, Module>,
    pub module_ids: HashMap<String, ModuleId>,
    pub module_file_ids: HashMap<String, ModuleId>,
    pub import_graph: Vec<Vec<ModuleId>>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub id: ModuleId,
    pub name: String,
    pub file: String,
    pub dependencies: Vec<ModuleId>,
    pub exports: Vec<Symbol>,
}

pub fn module_graph_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut module_ctx = ModuleContext::new(ctx.logger.clone());
    let ret = module_ctx.run_module_graph_pass(ctx);

    ctx.modules = module_ctx.table.clone();
    ctx.errors.extend(module_ctx.errors);
    ctx.warnings.extend(module_ctx.warnings);

    ret
}

pub fn module_import_symbol_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut module_ctx = ModuleContext::new(ctx.logger.clone());
    let ret = module_ctx.run_module_import_symbol_pass(ctx);

    ctx.errors.extend(module_ctx.errors);
    ctx.warnings.extend(module_ctx.warnings);

    ret
}

impl Module {
    pub fn new(id: ModuleId, name: String, file: String) -> Self {
        Module {
            id,
            name,
            file,
            dependencies: Vec::new(),
            exports: Vec::new(),
        }
    }
}

impl ModuleTable {
    pub fn new() -> Self {
        ModuleTable {
            modules: HashMap::new(),
            module_ids: HashMap::new(),
            module_file_ids: HashMap::new(),
            import_graph: Vec::new(),
        }
    }

    pub fn add_module(&mut self, name: String, file: String) -> ModuleId {
        let id = self.modules.len();
        self.module_ids.insert(name.clone(), id);
        self.modules.insert(id, Module::new(id, name, file));
        id
    }

    pub fn get_module(&mut self, id: ModuleId) -> Option<&mut Module> {
        self.modules.get_mut(&id)
    }

    pub fn get_module_by_name(&mut self, name: &str) -> Option<&mut Module> {
        let id = self.module_ids.get(name)?;
        self.get_module(*id)
    }
}

struct ModuleContext {
    _logger: Logger,
    current_file: String,
    current_module_name: String,
    current_module_id: ModuleId,
    current_module_decl_loc: Option<CodeSpan>,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
    table: ModuleTable,
}

impl ModuleContext {
    pub fn new(logger: Logger) -> Self {
        ModuleContext {
            _logger: logger,
            current_file: String::new(),
            current_module_name: String::new(),
            current_module_id: 0,
            current_module_decl_loc: None,
            errors: Vec::new(),
            warnings: Vec::new(),
            table: ModuleTable::new(),
        }
    }

    fn run_module_import_symbol_pass(&mut self, ctx: &mut CompileContext) -> Result<(), ()> {
        Ok(())
    }

    fn run_module_graph_pass(&mut self, ctx: &mut CompileContext) -> Result<(), ()> {
        //a million clonings here, but i'm not too stressed
        // its easier than dealing with the goddamn bc
        // and only runs once
        let mut failed = false;
        for (file, ast) in ctx.asts.iter_mut() {
            self.current_module_decl_loc = None;
            self.current_file = file.clone();

            match self.register_module_visitor(ast) {
                Ok(_) => {
                    let id = self
                        .table
                        .add_module(self.current_module_name.clone(), self.current_file.clone());
                    self.table
                        .module_file_ids
                        .insert(self.current_file.clone(), id);
                }
                Err(_) => {
                    failed = true;
                }
            };
        }

        if failed {
            return Err(());
        }

        for (file, ast) in ctx.asts.iter_mut() {
            self.current_file = file.clone();
            self.current_module_id = self.table.module_file_ids[&self.current_file];

            match self.extract_module_meta_visitor(ast) {
                Ok(_) => {}
                Err(_) => {
                    failed = true;
                }
            };
        }

        if failed {
            return Err(());
        }

        self.build_module_graph();
        ctx.modules = self.table.clone();
        //self._logger
        //    .info(&format!("{:#?}", self.table.import_graph));

        Ok(())
    }

    fn register_module_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        visit_ast_children!(ast.variant, self, register_module_visitor, {
            Variant::Expr(ExprNode::Call{ ref mut callee, ref mut args, .. }) => {
                if let Some(directive) = callee.get_directive() {
                    match directive.as_str() {
                        "@module" => {
                            self.process_module_directive(ast.span, args)?;
                        }
                        _ => {}
                    }
                }
            }
        });

        Ok(())
    }

    fn extract_module_meta_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        if ast.export {
            if ast.is_expr() {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: "Attempted to export expression".to_string(),
                        file: self.current_file.clone(),
                        span: ast.span,
                        err_type: DiagnosticMsgType::InvalidAstOperation,
                    },
                    notes: vec![],
                    hints: vec!["Only declarations can be exported".to_string()],
                });
                return Err(());
            }
            self.register_exported_symbol(&ast)?;
        }

        visit_ast_children!(ast.variant, self, extract_module_meta_visitor, {
            Variant::Expr(ExprNode::Call { ref mut callee, ref mut args, .. }) => {
                if let Some(directive) = callee.get_directive() {
                match directive.as_str() {
                    "@import" => {
                        self.add_module_dep(ast.span, args)?;
                    }
                    _ => {}
                }
                }
            }
        });

        Ok(())
    }

    fn register_exported_symbol(&mut self, ast: &Node) -> Result<(), ()> {
        //check for valid export node variant
        let valid_export = match ast.variant {
            Variant::ActorDecl { .. }
            | Variant::StructDecl { .. }
            | Variant::FuncDecl { .. }
            | Variant::EnumDecl { .. }
            | Variant::InterfaceDecl { .. }
            | Variant::TypeDecl { .. }
            | Variant::VarDecl { .. }
            | Variant::Expr(ExprNode::Ident { .. }) => true,
            _ => false,
        };

        if !valid_export {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Invalid export".to_string(),
                    file: self.current_file.clone(),
                    span: ast.span,
                    err_type: DiagnosticMsgType::InvalidAstOperation,
                },
                notes: vec![],
                hints: vec![
                    "Only actors, structs, functions, enums, interfaces, types, variables, and imports can be exported".to_string(),
                ],
            });
            return Err(());
        }

        if let Some(ident) = ast.get_ident() {
            let exports = &mut self
                .table
                .get_module(self.current_module_id)
                .unwrap()
                .exports;
            if ast.is_type_ident() {
                exports.push(Symbol::new_type(
                    INVALID_SYMBOL_ID,
                    self.current_module_id,
                    INVALID_SYMBOL_ID,
                    Some(ident),
                    vec![],
                    TypeVariant::Primitive(PrimitiveType::Any),
                    Some(ast.span),
                ));
            } else {
                exports.push(Symbol::new_var(
                    INVALID_SYMBOL_ID,
                    self.current_module_id,
                    INVALID_SYMBOL_ID,
                    ident,
                    Some(ast.span),
                    None,
                    false,
                ));
            }
        } else {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Exported symbols must have a valid identifier".to_string(),
                    file: self.current_file.clone(),
                    span: ast.span,
                    err_type: DiagnosticMsgType::IllegalName,
                },
                notes: vec![],
                hints: vec![
                    "Ensure the exported symbol has a valid identifier".to_string(),
                    "A valid export could look like `export MyStruct`".to_string(),
                ],
            });
            return Err(());
        }

        Ok(())
    }

    fn add_module_dep(&mut self, span: CodeSpan, args: &Vec<Node>) -> Result<(), ()> {
        //another bodged type check
        if args.len() != 1 {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Import directive takes only one argument".to_string(),
                    file: self.current_file.clone(),
                    span,
                    err_type: DiagnosticMsgType::InvalidArgument,
                },
                notes: vec![],
                hints: vec![],
            });
            return Err(());
        }

        let import_name = match args[0].variant.clone() {
            Variant::Expr(ExprNode::String(name)) => name,
            _ => {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: "Invalid import name".to_string(),
                        file: self.current_file.clone(),
                        span,
                        err_type: DiagnosticMsgType::IllegalName,
                    },
                    notes: vec![],
                    hints: vec![
                        "Import names must be an identifier".to_string(),
                        "A valid import could look like `@import('module_name')`".to_string(),
                    ],
                });
                return Err(());
            }
        };

        let import_id = match self.table.module_ids.get(&import_name) {
            Some(id) => *id,
            None => {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: format!("Module '{}' not found", import_name),
                        file: self.current_file.clone(),
                        span,
                        err_type: DiagnosticMsgType::ModuleNotFound,
                    },
                    notes: vec![],
                    hints: vec![
                        "Ensure the module is declared in the project".to_string(),
                        "Check the module name for typos".to_string(),
                    ],
                });
                return Err(());
            }
        };

        self.table
            .get_module(self.current_module_id)
            .unwrap()
            .dependencies
            .push(import_id);

        Ok(())
    }

    fn process_module_directive(
        &mut self,
        parent_span: CodeSpan,
        args: &Vec<Node>,
    ) -> Result<(), ()> {
        //shoehorning in a manual type check bc this runs before the type checker
        //but is required for symbol resolution
        let mut fail = false;
        if let Some(span) = self.current_module_decl_loc {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Duplicate module declarations".to_string(),
                    file: self.current_file.clone(),
                    span: parent_span,
                    err_type: DiagnosticMsgType::DuplicateModuleDeclaration,
                },
                notes: vec![DiagnosticMsg {
                    message: format!(
                        "Current module already declared as '{}'",
                        self.current_module_name
                    ),
                    file: self.current_file.clone(),
                    span,
                    err_type: DiagnosticMsgType::DuplicateModuleDeclaration,
                }],
                hints: vec![
                    "Each file can only declare one module".to_string(),
                    "Remove the duplicate module declaration".to_string(),
                ],
            });
            fail = true;
        }

        if args.len() != 1 {
            self.errors.push(Diagnostic {
                primary: DiagnosticMsg {
                    message: "Module directive requires exactly one argument".to_string(),
                    file: self.current_file.clone(),
                    span: parent_span,
                    err_type: DiagnosticMsgType::InvalidArgument,
                },
                notes: vec![],
                hints: vec![],
            });
            return Err(());
        }

        if fail {
            return Err(());
        }

        self.current_module_name = match args[0].variant.clone() {
            Variant::Expr(ExprNode::String(name)) => name,
            _ => {
                self.errors.push(Diagnostic {
                    primary: DiagnosticMsg {
                        message: "Invalid module name".to_string(),
                        file: self.current_file.clone(),
                        span: parent_span,
                        err_type: DiagnosticMsgType::InvalidModuleName,
                    },
                    notes: vec![],
                    hints: vec![
                        "Module names must be an identifier".to_string(),
                        format!(
                            "A valid module declaration could look like `@module({})`",
                            self.current_file
                                .clone()
                                .split('.')
                                .next()
                                .unwrap_or("my_module")
                                .replace(|c: char| !c.is_alphanumeric() && c != '_', "_")
                                .to_lowercase()
                        ),
                    ],
                });
                return Err(());
            }
        };
        self.current_module_decl_loc = Some(parent_span);

        Ok(())
    }

    //compute strongly connected components of module graph
    //to form toposorted condensed module dag
    fn build_module_graph(&mut self) {
        let mut g: DiGraphMap<ModuleId, ()> = DiGraphMap::new();
        //module table -> petgraph graph
        for (&m_id, m) in &self.table.modules {
            g.add_node(m_id);
            for &dep in &m.dependencies {
                g.add_node(dep);
                g.add_edge(m_id, dep, ());
            }
        }

        //sccs
        let mut comps = kosaraju_scc(&g);

        for c in &mut comps {
            c.sort();
        }

        //condensation dag
        let comp_index: HashMap<ModuleId, usize> = {
            let mut map = HashMap::new();
            for (i, comp) in comps.iter().enumerate() {
                for &m in comp {
                    map.insert(m, i);
                }
            }
            map
        };

        let mut dag: DiGraphMap<usize, ()> = DiGraphMap::new();
        for i in 0..comps.len() {
            dag.add_node(i);
        }
        for (&m_id, m) in &self.table.modules {
            let c_u = comp_index[&m_id];
            for &dep in &m.dependencies {
                let c_v = comp_index[&dep];
                if c_u != c_v {
                    dag.add_edge(c_v, c_u, ());
                }
            }
        }

        //sort
        let order = toposort(&dag, None)
            .expect("Cycles detected in condensed module graph - something has gone wildly wrong");

        let mut ordered = Vec::with_capacity(comps.len());
        for idx in order {
            ordered.push(comps[idx].clone());
        }
        self.table.import_graph = ordered;
    }
}
