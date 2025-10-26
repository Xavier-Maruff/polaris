use crate::symbol::SymbolId;
use rustc_hash::FxHashMap as HashMap;

pub type ModuleId = usize;
pub type BlockId = usize;
pub type RegisterId = usize;
pub type DecisionId = usize;
pub type ArenaId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub id: usize,
    pub name: Option<String>,
    pub harness_contract: HarnessContract,
    pub adt_defs: HashMap<SymbolId, ADTDef>,
    pub register_allocators: HashMap<RegisterId, Allocator>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HarnessContract {
    pub const_manifest: Vec<ConstDecl>,
    pub fn_manifest: Vec<FnDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstDecl {
    pub register: RegisterId,
    pub name: Option<String>,
    pub _type: Type,
    pub value: RValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub id: SymbolId,
    pub name: Option<String>,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Vec<BasicBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ADTDef {
    pub id: SymbolId,
    pub name: Option<String>,
    pub variants: Vec<ADTVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ADTVariant {
    pub id: SymbolId,
    pub name: Option<String>,
    pub tag: usize,
    pub fields: Vec<FieldDef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldDef {
    pub name: Option<String>,
    pub _type: Type,
    pub offset: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub register: RegisterId,
    pub name: Option<String>,
    pub _type: Type,
    pub by_ref: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub id: SymbolId,
    pub nocrypt: bool,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Real,
    Array(Box<Type>, usize),
    Map(Box<Type>, Box<Type>),
    String,
    Char,
    ADT(SymbolId),
    //todo: how to handle map and tuple
}

#[derive(Debug, Clone)]
pub enum RValue {
    Int(i64),
    UInt(u64),
    Real(f64),
    Str(String),
    Char(String),
    Array(Vec<RValue>),
    Register(RegisterId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Allocator {
    Stack,
    Arena(RegisterId),
    RefCounted,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    pub id: BlockId,
    pub params: Vec<(RegisterId, Type)>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    pub dest: RegisterId,
    pub op: Op,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Break {
        target: BlockId,
        args: Vec<RegisterId>,
    },
    Return {
        values: Vec<RegisterId>,
    },
    If {
        cond: RValue,
        then_block: BlockId,
        else_block: BlockId,
    },
}

/// very first-draft stab at ops
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    ArenaAlloc {
        size: RValue,
    },
    ArenaFree {
        arena: RegisterId,
    },
    Call {
        fn_id: SymbolId,
        args: Vec<RegisterId>,
    },
    DecideBranch {
        decision_id: DecisionId,
        arg: RegisterId,
        blocks: Vec<(RValue, BlockId)>,
    },
    HarnessCall {
        fn_id: SymbolId,
        args: Vec<RegisterId>,
    },
    HarnessDecideBranch {
        decision_id: DecisionId,
        arg: RegisterId,
        blocks: Vec<(RValue, BlockId)>,
    },
    EncSelect {
        predicate: RValue,
        _1: RValue,
        _0: RValue,
    },
    EncExtractField {
        base: RegisterId,
        field_index: usize,
    },
    EncReplaceField {
        base: RegisterId,
        field_index: usize,
        new_value: RValue,
    },
    ExtractField {
        base: RegisterId,
        field_index: usize,
    },
    ReplaceField {
        base: RegisterId,
        field_index: usize,
        new_value: RValue,
    },
    ADTConstruct {
        adt_id: SymbolId,
        variant_tag: usize,
        field_values: Vec<RValue>,
    },

    //arithmetic shizzle
    EncAddI8(RValue, RValue),
    EncAddI16(RValue, RValue),
    EncAddI32(RValue, RValue),
    EncAddI64(RValue, RValue),
    EncAddU8(RValue, RValue),
    EncAddU16(RValue, RValue),
    EncAddU32(RValue, RValue),
    EncAddU64(RValue, RValue),
    EncAddReal(RValue, RValue),
    EncSubI8(RValue, RValue),
    EncSubI16(RValue, RValue),
    EncSubI32(RValue, RValue),
    EncSubI64(RValue, RValue),
    EncSubU8(RValue, RValue),
    EncSubU16(RValue, RValue),
    EncSubU32(RValue, RValue),
    EncSubU64(RValue, RValue),
    EncSubReal(RValue, RValue),
    EncMulI8(RValue, RValue),
    EncMulI16(RValue, RValue),
    EncMulI32(RValue, RValue),
    EncMulI64(RValue, RValue),
    EncMulU8(RValue, RValue),
    EncMulU16(RValue, RValue),
    EncMulU32(RValue, RValue),
    EncMulU64(RValue, RValue),
    EncMulReal(RValue, RValue),
    EncDivReal(RValue, RValue),
    //currently not bit-width specific, will probably need to be later
    //i'll see how the VM impl goes
    NCAddInt(RValue, RValue),
    NCAddUInt(RValue, RValue),
    NCAddReal(RValue, RValue),
    NCSubInt(RValue, RValue),
    NCSubUInt(RValue, RValue),
    NCSubReal(RValue, RValue),
    NCMulInt(RValue, RValue),
    NCMulUInt(RValue, RValue),
    NCMulReal(RValue, RValue),
    NCDivReal(RValue, RValue),

    //static array ops
    //just a first pass, will need to expand
    ArrayLen(RegisterId),
    ArrayGet {
        base: RegisterId,
        index: RValue,
    },
    ArrayUpdateElement {
        base: RegisterId,
        index: RValue,
        value: RValue,
    },
    ArrayConstruct {
        element_type: Type,
        elements: Vec<RValue>,
        len: usize,
    },
    //cons list ops
    //todo
}

impl PartialEq for RValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RValue::Int(a), RValue::Int(b)) => a == b,
            (RValue::UInt(a), RValue::UInt(b)) => a == b,
            (RValue::Real(a), RValue::Real(b)) => a.to_bits() == b.to_bits(),
            (RValue::Str(a), RValue::Str(b)) => a == b,
            (RValue::Char(a), RValue::Char(b)) => a == b,
            (RValue::Array(a), RValue::Array(b)) => a == b,
            (RValue::Register(a), RValue::Register(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for RValue {}
