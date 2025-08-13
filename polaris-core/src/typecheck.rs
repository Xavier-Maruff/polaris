use crate::{
    ast::ast::{ExprNode, ForVariant, Node, Variant},
    compile::CompileContext,
    diagnostic::Diagnostic,
    log::Logger,
    visit_ast_children,
};

pub fn typecheck_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let mut typecheck_ctx = TypecheckPassContext::new(ctx);
    let ret = typecheck_ctx.run_typecheck_pass(ctx);

    ctx.errors.extend(typecheck_ctx.errors);
    ctx.warnings.extend(typecheck_ctx.warnings);

    ret
}

struct TypecheckPassContext {
    _logger: Logger,
    current_file: String,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
}

impl TypecheckPassContext {
    fn new(ctx: &mut CompileContext) -> Self {
        Self {
            _logger: ctx.logger.clone(),
            current_file: String::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    fn run_typecheck_pass(&mut self, ctx: &mut CompileContext) -> Result<(), ()> {
        //
        Ok(())
    }

    fn type_inference_visitor(&mut self, ast: &mut Node) -> Result<(), ()> {
        visit_ast_children!(ast.variant, self, type_inference_visitor, {});
        Ok(())
    }
}
