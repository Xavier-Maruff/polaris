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
    String(usize),
    DynamicArray(Box<Type>),
    DynamicString,
    Char,
    ADT(SymbolId),
    Tuple(Vec<Type>),
}

#[derive(Debug, Clone)]
pub enum RValue {
    Int(i64),
    UInt(u64),
    Real(f64),
    Str(String),
    Char(String),
    Array(Vec<RValue>),
    RegisterContents(RegisterId),
    Tuple(Vec<RValue>),
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
    pub noise_budget: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Break {
        target: BlockId,
        args: Vec<RegisterId>,
    },
    Return(RegisterId),
    NCBranch {
        cond: RValue,
        then_block: BlockId,
        else_block: BlockId,
    },
}

/// very first-draft stab at ops
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    //allocation
    ArenaAlloc {
        size: RValue,
    },
    ArenaFree {
        arena: RegisterId,
    },

    //calls
    Call {
        fn_id: SymbolId,
        args: Vec<RegisterId>,
    },
    HarnessCall {
        fn_id: SymbolId,
        args: Vec<RegisterId>,
    },

    //branching
    DecideBranch {
        decision_id: DecisionId,
        arg: RegisterId,
        blocks: Vec<(RValue, BlockId)>,
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

    //adts
    EncADTExtractField {
        base: RegisterId,
        field_index: usize,
    },
    EncADTReplaceField {
        base: RegisterId,
        field_index: usize,
        new_value: RValue,
    },
    EncADTExtractTag {
        base: RegisterId,
    },
    NCADTExtractField {
        base: RegisterId,
        field_index: usize,
    },
    NCADTReplaceField {
        base: RegisterId,
        field_index: usize,
        new_value: RValue,
    },
    NCADTExtractTag {
        base: RegisterId,
    },

    EncADTConstruct {
        adt_id: SymbolId,
        variant_tag: usize,
        field_values: Vec<RValue>,
    },
    NCADTConstruct {
        adt_id: SymbolId,
        variant_tag: usize,
        field_values: Vec<RValue>,
    },

    //tuples
    TupleConstruct {
        elements: Vec<RValue>,
    },
    TupleExtract {
        base: RegisterId,
        index: usize,
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

    //these are disallowed for all actual ints
    //only for fixed-points as scaled ints
    EncDivI8(RValue, RValue),
    EncDivI16(RValue, RValue),
    EncDivI32(RValue, RValue),
    EncDivI64(RValue, RValue),
    EncDivU8(RValue, RValue),
    EncDivU16(RValue, RValue),
    EncDivU32(RValue, RValue),
    EncDivU64(RValue, RValue),
    EncDivReal(RValue, RValue),

    EncNegI8(RValue),
    EncNegI16(RValue),
    EncNegI32(RValue),
    EncNegI64(RValue),
    EncNegReal(RValue),

    EncModI8(RValue, RValue),
    EncModI16(RValue, RValue),
    EncModI32(RValue, RValue),
    EncModI64(RValue, RValue),
    EncModU8(RValue, RValue),
    EncModU16(RValue, RValue),
    EncModU32(RValue, RValue),
    EncModU64(RValue, RValue),

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
    NCDivInt(RValue, RValue),
    NCDivUInt(RValue, RValue),
    NCDivReal(RValue, RValue),
    NCModInt(RValue, RValue),
    NCModUInt(RValue),
    NCNegInt(RValue),
    NCNegReal(RValue),

    //comparison - will expand later for perf if needed
    EncEqI8(RValue, RValue),
    EncEqI16(RValue, RValue),
    EncEqI32(RValue, RValue),
    EncEqI64(RValue, RValue),
    EncEqU8(RValue, RValue),
    EncEqU16(RValue, RValue),
    EncEqU32(RValue, RValue),
    EncEqU64(RValue, RValue),
    EncEqReal(RValue, RValue),
    EncLtI8(RValue, RValue),
    EncLtI16(RValue, RValue),
    EncLtI32(RValue, RValue),
    EncLtI64(RValue, RValue),
    EncLtU8(RValue, RValue),
    EncLtU16(RValue, RValue),
    EncLtU32(RValue, RValue),
    EncLtU64(RValue, RValue),
    EncLtReal(RValue, RValue),

    //again just abtracting bit-width for now
    NCEqInt(RValue, RValue),
    NCEqUInt(RValue, RValue),
    NCEqReal(RValue, RValue),
    NCLtInt(RValue, RValue),
    NCLtUInt(RValue, RValue),
    NCLtReal(RValue, RValue),

    //todo: bitwise ops

    //static array ops
    //just a first pass, will need to expand
    //encrypted string ops map to these
    EncArrayConstructZeroed {
        element_type: Type,
        len: usize,
    },
    EncArrayLen(RegisterId),
    //requires nocrypt index
    EncArrayGet {
        base: RegisterId,
        index: RValue,
    },
    //allows encrypted index
    EncArrayObliviousGet {
        base: RegisterId,
        index: RValue,
    },
    EncArrayUpdateElement {
        base: RegisterId,
        index: RValue,
        value: RValue,
    },
    EncArrayConstruct {
        element_type: Type,
        elements: Vec<RValue>,
        len: usize,
    },
    EncArrayCmpEq {
        a: RegisterId,
        b: RegisterId,
    },
    EncArrayReduce {
        base: RegisterId,
        initial: RValue,
        func: SymbolId,
    },
    EncArrayMap {
        base: RegisterId,
        func: SymbolId,
    },
    //not certain of implementation yet, need to consider doability
    EncArrayFilter {
        base: RegisterId,
        func: SymbolId,
    },
    //not sorting concat yet, as unsure of implementation

    //dynamic data structures
    NCDynArrayConstruct {
        element_type: Type,
    },
    NCDynArrayLen(RegisterId),
    NCDynArrayGet {
        base: RegisterId,
        index: RValue,
    },
    NCDynArrayUpdateElement {
        base: RegisterId,
        index: RValue,
        value: RValue,
    },
    NCDynArrayPush {
        base: RegisterId,
        value: RValue,
    },
    NCDynArrayPop(RegisterId),
    NCDynArrayCmpEq {
        a: RegisterId,
        b: RegisterId,
    },
    NCDynArrayConcat {
        a: RegisterId,
        b: RegisterId,
    },

    NCDynStringConstruct {
        capacity: RValue,
    },
    NCDynStringLen(RegisterId),
    NCDynStringConcat {
        a: RegisterId,
        b: RegisterId,
    },
    NCDynStringEq {
        a: RegisterId,
        b: RegisterId,
    },
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
            (RValue::RegisterContents(a), RValue::RegisterContents(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for RValue {}
