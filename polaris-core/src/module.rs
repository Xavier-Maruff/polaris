use crate::ast::Node;

#[derive(Clone, Debug)]
pub struct ModuleContext {
    pub name: String,
    pub file: String,
    pub ast: Node,
}
