use crate::{
    ast::{ExprKind, Node, NodeKind},
    compile::CompileContext,
    module::ModuleId,
};

pub fn lowering_pmir_pass(ctx: &mut CompileContext) -> Result<(), ()> {
    let module_ids: Vec<String> = ctx.dependencies.modules.keys().cloned().collect();

    let mut failed = false;
    let mut ir_ctx = PMIRLoweringContext::default();

    for module_id in module_ids {
        failed = match ir_ctx.lower_module(ctx, &module_id) {
            Ok(_) => failed,
            Err(_) => true,
        }
    }

    if failed { Err(()) } else { Ok(()) }
}

#[derive(Default)]
struct PMIRLoweringContext {
    current_file: String,
}

impl PMIRLoweringContext {
    pub fn lower_module(
        &mut self,
        ctx: &mut CompileContext,
        module_id: &ModuleId,
    ) -> Result<(), ()> {
        let module_ctx = ctx.dependencies.modules.get_mut(module_id).ok_or(())?;
        self.current_file = module_ctx.file.clone();

        self.visit_node(&mut module_ctx.ast)?;

        Ok(())
    }

    pub fn visit_node(&mut self, node: &mut Node) -> Result<(), ()> {
        use NodeKind::*;
        match &mut node.kind {
            Module { children } => {
                for child in children.iter_mut() {
                    self.visit_node(child)?;
                }
            }
            ConstDecl { expr, .. } => {
                //
            }
            Expr { .. } => self.visit_expr(node)?,
            _ => {}
        }

        Ok(())
    }

    pub fn visit_expr(&mut self, node: &mut Node) -> Result<(), ()> {
        let expr = match &mut node.kind {
            NodeKind::Expr { expr } => expr,
            _ => unreachable!(),
        };

        use ExprKind::*;
        match expr {
            _ => {
                //
            }
        }

        Ok(())
    }
}
