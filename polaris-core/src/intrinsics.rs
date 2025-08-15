use crate::ast::ast::{BinaryOp, UnaryOp};
use crate::{module::INVALID_MODULE_ID, symbol::NameResolverPassContext};

pub fn declare_intrinsics(nr: &mut NameResolverPassContext) {
    declare_intrinsic_primitives(nr);
    declare_intrinsic_directives(nr);
}

macro_rules! decl_symbol {
    ($nr:expr, $name:expr) => {{
        $nr.declare(INVALID_MODULE_ID, $name.to_string(), None, false, false);
    }};
}

macro_rules! decl_type_symbol {
    ($nr:expr, $name:expr) => {{
        $nr.declare_type(INVALID_MODULE_ID, Some($name.to_string()), None);
    }};
}

pub fn declare_intrinsic_primitives(nr: &mut NameResolverPassContext) {
    decl_type_symbol!(nr, "bool");
    decl_type_symbol!(nr, "int32");
    decl_type_symbol!(nr, "int64");
    decl_type_symbol!(nr, "uint32");
    decl_type_symbol!(nr, "uint64");
    decl_type_symbol!(nr, "float32");
    decl_type_symbol!(nr, "float64");
    decl_type_symbol!(nr, "string");
    decl_type_symbol!(nr, "void");
    decl_type_symbol!(nr, "vector");
}

/*

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
*/

pub fn declare_intrinsic_interfaces(nr: &mut NameResolverPassContext) {
    decl_symbol!(nr, "intrinsic");
    decl_symbol!(nr, BinaryOp::Add.name());
    decl_symbol!(nr, BinaryOp::Subtract.name());
    decl_symbol!(nr, BinaryOp::Multiply.name());
    decl_symbol!(nr, BinaryOp::Divide.name());
    decl_symbol!(nr, BinaryOp::Modulo.name());
    decl_symbol!(nr, BinaryOp::Equiv.name());
    decl_symbol!(nr, BinaryOp::NotEquiv.name());
    decl_symbol!(nr, BinaryOp::LessThan.name());
    decl_symbol!(nr, BinaryOp::GreaterThan.name());
    decl_symbol!(nr, BinaryOp::LessThanEquiv.name());
    decl_symbol!(nr, BinaryOp::GreaterThanEquiv.name());
    decl_symbol!(nr, BinaryOp::BitAnd.name());
    decl_symbol!(nr, BinaryOp::BitOr.name());
    decl_symbol!(nr, BinaryOp::BitXor.name());
    decl_symbol!(nr, BinaryOp::BitNot.name());
    decl_symbol!(nr, UnaryOp::Minus.name());
    decl_symbol!(nr, UnaryOp::Not.name());
    decl_symbol!(nr, UnaryOp::BitNot.name());
    decl_symbol!(nr, UnaryOp::Ref.name());
    decl_symbol!(nr, UnaryOp::Deref.name());
    decl_symbol!(nr, UnaryOp::BindMonad.name());
    decl_symbol!(nr, UnaryOp::Await.name());
    decl_symbol!(nr, UnaryOp::Block.name());
    decl_symbol!(nr, UnaryOp::FusedAssign.name());
    decl_symbol!(nr, UnaryOp::Spread.name());
    decl_symbol!(nr, UnaryOp::Spawn.name());
}

fn declare_intrinsic_directives(nr: &mut NameResolverPassContext) {
    decl_symbol!(nr, "@module");
    decl_symbol!(nr, "@import");
    decl_symbol!(nr, "@host");
    decl_symbol!(nr, "@blocking");
    decl_symbol!(nr, "@nonblocking");
    decl_symbol!(nr, "@gpu");
    decl_symbol!(nr, "@cpu");
}
