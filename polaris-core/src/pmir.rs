use crate::symbol::SymbolId;
use bincode::{Decode, Encode, config};
use rustc_hash::FxHashMap as HashMap;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter, Result as FmtResult};

pub type ModuleId = usize;
pub type BlockId = usize;
pub type RegisterId = usize;
pub type DecisionId = usize;
pub type ArenaId = usize;

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub id: usize,
    pub name: Option<String>,
    pub harness_contract: HarnessContract,
    pub adt_defs: HashMap<SymbolId, ADTDef>,
    pub register_allocators: HashMap<RegisterId, Allocator>,
    pub fn_defs: HashMap<SymbolId, FnDef>,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct HarnessContract {
    pub const_manifest: Vec<ConstDecl>,
    pub fn_manifest: Vec<FnDecl>,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ConstDecl {
    pub register: RegisterId,
    pub name: Option<String>,
    pub _type: Type,
    pub value: RValue,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FnDef {
    pub id: SymbolId,
    pub name: Option<String>,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Vec<BasicBlock>,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub id: SymbolId,
    pub name: Option<String>,
    pub params: Vec<Param>,
    pub return_type: Type,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ADTDef {
    pub id: SymbolId,
    pub name: Option<String>,
    pub variants: Vec<ADTVariant>,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ADTVariant {
    pub id: SymbolId,
    pub name: Option<String>,
    pub tag: usize,
    pub fields: Vec<FieldDef>,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FieldDef {
    pub name: Option<String>,
    pub _type: Type,
    pub offset: usize,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub register: RegisterId,
    pub name: Option<String>,
    pub _type: Type,
    pub by_ref: bool,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub id: SymbolId,
    pub nocrypt: bool,
    pub kind: TypeKind,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
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

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone)]
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

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Allocator {
    Stack,
    Arena(RegisterId),
    RefCounted,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    pub id: BlockId,
    pub params: Vec<(RegisterId, Type)>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    pub dest: RegisterId,
    pub op: Op,
    pub noise_budget: Option<u32>,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
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
#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
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

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        writeln!(f, "module {}", self.name.as_deref().unwrap_or("unnamed"))?;
        writeln!(f)?;
        write!(f, "{}", self.harness_contract)?;

        for (_, adt_def) in &self.adt_defs {
            writeln!(f)?;
            write!(f, "{}", adt_def)?;
        }

        for (_, fn_def) in &self.fn_defs {
            writeln!(f)?;
            write!(f, "{}", fn_def)?;
        }

        Ok(())
    }
}

impl Display for HarnessContract {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        writeln!(f, "harness_contract {{")?;

        if !self.const_manifest.is_empty() {
            writeln!(f, "  ; constants that must be injected by the harness")?;
            writeln!(f, "  const_manifest {{")?;
            for const_decl in &self.const_manifest {
                writeln!(f, "    {}", const_decl)?;
            }
            writeln!(f, "  }}")?;
        }

        if !self.fn_manifest.is_empty() {
            writeln!(f)?;
            writeln!(f, "  ; functions that must be implemented by the harness")?;
            writeln!(f, "  fn_manifest {{")?;
            for fn_decl in &self.fn_manifest {
                writeln!(f, "    {}", fn_decl)?;
            }
            writeln!(f, "  }}")?;
        }

        writeln!(f, "}}")
    }
}

impl Display for ConstDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let default_name = format!("%{}", self.register);
        let name = self.name.as_deref().unwrap_or(&default_name);
        write!(f, "{}: {} = {}", name, self._type, self.value)
    }
}

impl Display for FnDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let default_name = format!("fn_{}", self.id);
        let name = self.name.as_deref().unwrap_or(&default_name);
        write!(f, "fn {}(", default_name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{}: {}",
                param.name.as_deref().unwrap_or("_"),
                param._type
            )?;
        }
        write!(f, "): {} ; {}", self.return_type, name)
    }
}

impl Display for FnDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let default_name = format!("fn_{}", self.id);
        let name = self.name.as_deref().unwrap_or(&default_name);
        write!(f, "fn {}(", default_name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            let default_param_name = format!("%{}", param.register);
            let param_name = param.name.as_deref().unwrap_or(&default_param_name);
            write!(f, "{}: {}", param_name, param._type)?;
        }
        writeln!(f, "): {} {{; {}", self.return_type, name)?;

        for block in &self.body {
            write!(f, "{}", block)?;
        }

        writeln!(f, "}}")
    }
}

impl Display for ADTDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let default_name = format!("adt_{}", self.id);
        let name = self.name.as_deref().unwrap_or(&default_name);
        writeln!(f, "type {} {{", name)?;
        for variant in &self.variants {
            writeln!(f, "  {}", variant)?;
        }
        writeln!(f, "}}")
    }
}

impl Display for ADTVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let default_name = format!("variant_{}", self.id);
        let name = self.name.as_deref().unwrap_or(&default_name);
        write!(f, "{}", name)?;
        if !self.fields.is_empty() {
            write!(f, " {{")?;
            for (i, field) in self.fields.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", field)?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

impl Display for FieldDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let name = self.name.as_deref().unwrap_or("_");
        write!(f, "{}: {}", name, self._type)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let prefix = if self.nocrypt { "nc." } else { "enc." };
        write!(f, "{}{}", prefix, self.kind)
    }
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            TypeKind::I8 => write!(f, "i8"),
            TypeKind::I16 => write!(f, "i16"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U16 => write!(f, "u16"),
            TypeKind::U32 => write!(f, "u32"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::Real => write!(f, "real"),
            TypeKind::Array(elem_type, size) => write!(f, "array({}, {})", elem_type, size),
            TypeKind::String(size) => write!(f, "str({})", size),
            TypeKind::DynamicArray(elem_type) => write!(f, "array({})", elem_type),
            TypeKind::DynamicString => write!(f, "str"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::ADT(id) => write!(f, "adt_{}", id),
            TypeKind::Tuple(types) => {
                write!(f, "tuple(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for RValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            RValue::Int(val) => write!(f, "{}", val),
            RValue::UInt(val) => write!(f, "{}", val),
            RValue::Real(val) => write!(f, "{}", val),
            RValue::Str(val) => write!(f, "\"{}\"", val),
            RValue::Char(val) => write!(f, "'{}'", val),
            RValue::Array(vals) => {
                write!(f, "[")?;
                for (i, val) in vals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
            RValue::RegisterContents(reg) => write!(f, "%{}", reg),
            RValue::Tuple(vals) => {
                write!(f, "(")?;
                for (i, val) in vals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "  ^bb{}(", self.id)?;
        for (i, (reg, ty)) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "%{}: {}", reg, ty)?;
        }
        writeln!(f, "):")?;

        for instruction in &self.instructions {
            writeln!(f, "    {}", instruction)?;
        }

        writeln!(f, "    {}", self.terminator)?;

        Ok(())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "%{} = {}", self.dest, self.op)
    }
}

impl Display for Terminator {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Terminator::Break { target, args } => {
                write!(f, "br ^bb{}", target)?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "%{}", arg)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Terminator::Return(reg) => write!(f, "ret %{}", reg),
            Terminator::NCBranch {
                cond,
                then_block,
                else_block,
            } => {
                write!(
                    f,
                    "nc.branch {} {{true: ^bb{}, false: ^bb{}}}",
                    cond, then_block, else_block
                )
            }
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Op::ArenaAlloc { size } => write!(f, "arena.alloc {}", size),
            Op::ArenaFree { arena } => write!(f, "arena.free %{}", arena),

            Op::Call { fn_id, args } => {
                write!(f, "call fn_{}(", fn_id)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", arg)?;
                }
                write!(f, ")")
            }
            Op::HarnessCall { fn_id, args } => {
                write!(f, "harness.call fn_{}(", fn_id)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", arg)?;
                }
                write!(f, ")")
            }

            Op::DecideBranch {
                decision_id,
                arg,
                blocks,
            } => {
                write!(f, "decide_branch ${} %{} {{", decision_id, arg)?;
                for (i, (val, block)) in blocks.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ^bb{}", val, block)?;
                }
                write!(f, "}}")
            }
            Op::HarnessDecideBranch {
                decision_id,
                arg,
                blocks,
            } => {
                write!(f, "harness.decide_branch ${} %{} {{", decision_id, arg)?;
                for (i, (val, block)) in blocks.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ^bb{}", val, block)?;
                }
                write!(f, "}}")
            }
            Op::EncSelect { predicate, _1, _0 } => {
                write!(f, "enc.select {} {} {}", predicate, _1, _0)
            }

            Op::EncADTExtractField { base, field_index } => {
                write!(f, "enc.adt.extract_field %{} {}", base, field_index)
            }
            Op::EncADTReplaceField {
                base,
                field_index,
                new_value,
            } => write!(
                f,
                "enc.adt.replace_field %{} {} {}",
                base, field_index, new_value
            ),
            Op::EncADTExtractTag { base } => write!(f, "enc.adt.extract_tag %{}", base),
            Op::NCADTExtractField { base, field_index } => {
                write!(f, "nc.adt.extract_field %{} {}", base, field_index)
            }
            Op::NCADTReplaceField {
                base,
                field_index,
                new_value,
            } => write!(
                f,
                "nc.adt.replace_field %{} {} {}",
                base, field_index, new_value
            ),
            Op::NCADTExtractTag { base } => write!(f, "nc.adt.extract_tag %{}", base),
            Op::EncADTConstruct {
                adt_id,
                variant_tag,
                field_values,
            } => {
                write!(f, "enc.adt.construct adt_{} {} [", adt_id, variant_tag)?;
                for (i, val) in field_values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
            Op::NCADTConstruct {
                adt_id,
                variant_tag,
                field_values,
            } => {
                write!(f, "nc.adt.construct adt_{} {} [", adt_id, variant_tag)?;
                for (i, val) in field_values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }

            Op::TupleConstruct { elements } => {
                write!(f, "tuple.construct [")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Op::TupleExtract { base, index } => write!(f, "tuple.extract %{} {}", base, index),

            //feel like this should be macroed but oh well
            Op::EncAddI8(a, b) => write!(f, "enc.add_i8 {} {}", a, b),
            Op::EncAddI16(a, b) => write!(f, "enc.add_i16 {} {}", a, b),
            Op::EncAddI32(a, b) => write!(f, "enc.add_i32 {} {}", a, b),
            Op::EncAddI64(a, b) => write!(f, "enc.add_i64 {} {}", a, b),
            Op::EncAddU8(a, b) => write!(f, "enc.add_u8 {} {}", a, b),
            Op::EncAddU16(a, b) => write!(f, "enc.add_u16 {} {}", a, b),
            Op::EncAddU32(a, b) => write!(f, "enc.add_u32 {} {}", a, b),
            Op::EncAddU64(a, b) => write!(f, "enc.add_u64 {} {}", a, b),
            Op::EncAddReal(a, b) => write!(f, "enc.add_real {} {}", a, b),

            Op::EncSubI8(a, b) => write!(f, "enc.sub_i8 {} {}", a, b),
            Op::EncSubI16(a, b) => write!(f, "enc.sub_i16 {} {}", a, b),
            Op::EncSubI32(a, b) => write!(f, "enc.sub_i32 {} {}", a, b),
            Op::EncSubI64(a, b) => write!(f, "enc.sub_i64 {} {}", a, b),
            Op::EncSubU8(a, b) => write!(f, "enc.sub_u8 {} {}", a, b),
            Op::EncSubU16(a, b) => write!(f, "enc.sub_u16 {} {}", a, b),
            Op::EncSubU32(a, b) => write!(f, "enc.sub_u32 {} {}", a, b),
            Op::EncSubU64(a, b) => write!(f, "enc.sub_u64 {} {}", a, b),
            Op::EncSubReal(a, b) => write!(f, "enc.sub_real {} {}", a, b),

            Op::EncMulI8(a, b) => write!(f, "enc.mul_i8 {} {}", a, b),
            Op::EncMulI16(a, b) => write!(f, "enc.mul_i16 {} {}", a, b),
            Op::EncMulI32(a, b) => write!(f, "enc.mul_i32 {} {}", a, b),
            Op::EncMulI64(a, b) => write!(f, "enc.mul_i64 {} {}", a, b),
            Op::EncMulU8(a, b) => write!(f, "enc.mul_u8 {} {}", a, b),
            Op::EncMulU16(a, b) => write!(f, "enc.mul_u16 {} {}", a, b),
            Op::EncMulU32(a, b) => write!(f, "enc.mul_u32 {} {}", a, b),
            Op::EncMulU64(a, b) => write!(f, "enc.mul_u64 {} {}", a, b),
            Op::EncMulReal(a, b) => write!(f, "enc.mul_real {} {}", a, b),

            Op::EncDivI8(a, b) => write!(f, "enc.div_i8 {} {}", a, b),
            Op::EncDivI16(a, b) => write!(f, "enc.div_i16 {} {}", a, b),
            Op::EncDivI32(a, b) => write!(f, "enc.div_i32 {} {}", a, b),
            Op::EncDivI64(a, b) => write!(f, "enc.div_i64 {} {}", a, b),
            Op::EncDivU8(a, b) => write!(f, "enc.div_u8 {} {}", a, b),
            Op::EncDivU16(a, b) => write!(f, "enc.div_u16 {} {}", a, b),
            Op::EncDivU32(a, b) => write!(f, "enc.div_u32 {} {}", a, b),
            Op::EncDivU64(a, b) => write!(f, "enc.div_u64 {} {}", a, b),
            Op::EncDivReal(a, b) => write!(f, "enc.div_real {} {}", a, b),

            Op::EncNegI8(a) => write!(f, "enc.neg_i8 {}", a),
            Op::EncNegI16(a) => write!(f, "enc.neg_i16 {}", a),
            Op::EncNegI32(a) => write!(f, "enc.neg_i32 {}", a),
            Op::EncNegI64(a) => write!(f, "enc.neg_i64 {}", a),
            Op::EncNegReal(a) => write!(f, "enc.neg_real {}", a),

            Op::EncModI8(a, b) => write!(f, "enc.mod_i8 {} {}", a, b),
            Op::EncModI16(a, b) => write!(f, "enc.mod_i16 {} {}", a, b),
            Op::EncModI32(a, b) => write!(f, "enc.mod_i32 {} {}", a, b),
            Op::EncModI64(a, b) => write!(f, "enc.mod_i64 {} {}", a, b),
            Op::EncModU8(a, b) => write!(f, "enc.mod_u8 {} {}", a, b),
            Op::EncModU16(a, b) => write!(f, "enc.mod_u16 {} {}", a, b),
            Op::EncModU32(a, b) => write!(f, "enc.mod_u32 {} {}", a, b),
            Op::EncModU64(a, b) => write!(f, "enc.mod_u64 {} {}", a, b),

            Op::NCAddInt(a, b) => write!(f, "nc.add_int {} {}", a, b),
            Op::NCAddUInt(a, b) => write!(f, "nc.add_uint {} {}", a, b),
            Op::NCAddReal(a, b) => write!(f, "nc.add_real {} {}", a, b),
            Op::NCSubInt(a, b) => write!(f, "nc.sub_int {} {}", a, b),
            Op::NCSubUInt(a, b) => write!(f, "nc.sub_uint {} {}", a, b),
            Op::NCSubReal(a, b) => write!(f, "nc.sub_real {} {}", a, b),
            Op::NCMulInt(a, b) => write!(f, "nc.mul_int {} {}", a, b),
            Op::NCMulUInt(a, b) => write!(f, "nc.mul_uint {} {}", a, b),
            Op::NCMulReal(a, b) => write!(f, "nc.mul_real {} {}", a, b),
            Op::NCDivInt(a, b) => write!(f, "nc.div_int {} {}", a, b),
            Op::NCDivUInt(a, b) => write!(f, "nc.div_uint {} {}", a, b),
            Op::NCDivReal(a, b) => write!(f, "nc.div_real {} {}", a, b),
            Op::NCModInt(a, b) => write!(f, "nc.mod_int {} {}", a, b),
            Op::NCModUInt(a) => write!(f, "nc.mod_uint {}", a),
            Op::NCNegInt(a) => write!(f, "nc.neg_int {}", a),
            Op::NCNegReal(a) => write!(f, "nc.neg_real {}", a),

            Op::EncEqI8(a, b) => write!(f, "enc.eq_i8 {} {}", a, b),
            Op::EncEqI16(a, b) => write!(f, "enc.eq_i16 {} {}", a, b),
            Op::EncEqI32(a, b) => write!(f, "enc.eq_i32 {} {}", a, b),
            Op::EncEqI64(a, b) => write!(f, "enc.eq_i64 {} {}", a, b),
            Op::EncEqU8(a, b) => write!(f, "enc.eq_u8 {} {}", a, b),
            Op::EncEqU16(a, b) => write!(f, "enc.eq_u16 {} {}", a, b),
            Op::EncEqU32(a, b) => write!(f, "enc.eq_u32 {} {}", a, b),
            Op::EncEqU64(a, b) => write!(f, "enc.eq_u64 {} {}", a, b),
            Op::EncEqReal(a, b) => write!(f, "enc.eq_real {} {}", a, b),
            Op::EncLtI8(a, b) => write!(f, "enc.lt_i8 {} {}", a, b),
            Op::EncLtI16(a, b) => write!(f, "enc.lt_i16 {} {}", a, b),
            Op::EncLtI32(a, b) => write!(f, "enc.lt_i32 {} {}", a, b),
            Op::EncLtI64(a, b) => write!(f, "enc.lt_i64 {} {}", a, b),
            Op::EncLtU8(a, b) => write!(f, "enc.lt_u8 {} {}", a, b),
            Op::EncLtU16(a, b) => write!(f, "enc.lt_u16 {} {}", a, b),
            Op::EncLtU32(a, b) => write!(f, "enc.lt_u32 {} {}", a, b),
            Op::EncLtU64(a, b) => write!(f, "enc.lt_u64 {} {}", a, b),
            Op::EncLtReal(a, b) => write!(f, "enc.lt_real {} {}", a, b),

            Op::NCEqInt(a, b) => write!(f, "nc.eq_int {} {}", a, b),
            Op::NCEqUInt(a, b) => write!(f, "nc.eq_uint {} {}", a, b),
            Op::NCEqReal(a, b) => write!(f, "nc.eq_real {} {}", a, b),
            Op::NCLtInt(a, b) => write!(f, "nc.lt_int {} {}", a, b),
            Op::NCLtUInt(a, b) => write!(f, "nc.lt_uint {} {}", a, b),
            Op::NCLtReal(a, b) => write!(f, "nc.lt_real {} {}", a, b),

            Op::EncArrayConstructZeroed { element_type, len } => {
                write!(f, "enc.array.construct_zeroed {} {}", element_type, len)
            }
            Op::EncArrayLen(reg) => write!(f, "enc.array.len %{}", reg),
            Op::EncArrayGet { base, index } => write!(f, "enc.array.get %{} {}", base, index),
            Op::EncArrayObliviousGet { base, index } => {
                write!(f, "enc.array.oblivious_get %{} {}", base, index)
            }
            Op::EncArrayUpdateElement { base, index, value } => {
                write!(f, "enc.array.update %{} {} {}", base, index, value)
            }
            Op::EncArrayConstruct {
                element_type,
                elements,
                len,
            } => {
                write!(f, "enc.array.construct {} {} [", element_type, len)?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Op::EncArrayCmpEq { a, b } => write!(f, "enc.array.cmp_eq %{} %{}", a, b),
            Op::EncArrayReduce {
                base,
                initial,
                func,
            } => write!(f, "enc.array.reduce %{} {} fn_{}", base, initial, func),
            Op::EncArrayMap { base, func } => write!(f, "enc.array.map %{} fn_{}", base, func),
            Op::EncArrayFilter { base, func } => {
                write!(f, "enc.array.filter %{} fn_{}", base, func)
            }

            Op::NCDynArrayConstruct { element_type } => {
                write!(f, "nc.dynarray.construct {}", element_type)
            }
            Op::NCDynArrayLen(reg) => write!(f, "nc.dynarray.len %{}", reg),
            Op::NCDynArrayGet { base, index } => write!(f, "nc.dynarray.get %{} {}", base, index),
            Op::NCDynArrayUpdateElement { base, index, value } => {
                write!(f, "nc.dynarray.update %{} {} {}", base, index, value)
            }
            Op::NCDynArrayPush { base, value } => write!(f, "nc.dynarray.push %{} {}", base, value),
            Op::NCDynArrayPop(reg) => write!(f, "nc.dynarray.pop %{}", reg),
            Op::NCDynArrayCmpEq { a, b } => write!(f, "nc.dynarray.cmp_eq %{} %{}", a, b),
            Op::NCDynArrayConcat { a, b } => write!(f, "nc.dynarray.concat %{} %{}", a, b),

            Op::NCDynStringConstruct { capacity } => {
                write!(f, "nc.dynstring.construct {}", capacity)
            }
            Op::NCDynStringLen(reg) => write!(f, "nc.dynstring.len %{}", reg),
            Op::NCDynStringConcat { a, b } => write!(f, "nc.dynstring.concat %{} %{}", a, b),
            Op::NCDynStringEq { a, b } => write!(f, "nc.dynstring.eq %{} %{}", a, b),
        }
    }
}

impl Module {
    pub fn serialise_to_bytes(&self) -> Result<Vec<u8>, bincode::error::EncodeError> {
        let config = config::standard();
        bincode::encode_to_vec(self, config)
    }

    pub fn deserialise_from_bytes(data: &[u8]) -> Result<Self, bincode::error::DecodeError> {
        let config = config::standard();
        let (module, _): (Module, _) = bincode::decode_from_slice(data, config)?;
        Ok(module)
    }

    pub fn serialise_to_text(&self) -> String {
        format!("{}", self)
    }
}
