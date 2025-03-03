use derive_more::{Display, From};
use either::Either;
use itertools::Itertools;
use llvm_ir::{
    BasicBlock, Constant, ConstantRef, FPPredicate, Function, Instruction, IntPredicate, Module,
    Name, Operand, Terminator,
    instruction::{InlineAssembly, RMWBinOp},
    module::{GlobalAlias, GlobalIFunc, GlobalVariable},
};
use tracing::debug;

use super::{CF, Fresh, Language, OpInfo};
use crate::{
    common::Matchable,
    hypergraph::traits::{WireType, WithType},
};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct LlvmIrSettings {
    pub sym_name_linking: bool,
}

impl Default for LlvmIrSettings {
    fn default() -> Self {
        Self {
            sym_name_linking: true,
        }
    }
}

pub struct LlvmIr;

pub type Expr = super::Expr<LlvmIr>;
pub type Bind = super::Bind<LlvmIr>;
pub type Value = super::Value<LlvmIr>;
pub type Thunk = super::Thunk<LlvmIr>;
pub type Block = super::Block<LlvmIr>;

impl Language for LlvmIr {
    type Op = Op;
    type Var = Var;
    type Addr = String;
    type VarDef = Var;
    type BlockAddr = Name;
    type Symbol = Box<String>;
}

#[derive(Clone, PartialEq, Debug, Hash, Display, From)]
pub enum Op {
    // constant-like
    Constant(ConstantRef),
    RMWBinOp(RMWBinOp),
    IntPredicate(IntPredicate),
    FPPredicate(FPPredicate),
    // globals
    #[display("inline assembly")]
    InlineAssembly(InlineAssembly),
    #[display("{}", _0.name)]
    GlobalVariable(GlobalVariable),
    #[display("{}", _0.name)]
    GlobalAlias(GlobalAlias),
    #[display("{}", _0.name)]
    GlobalIFunc(GlobalIFunc),
    // end of basic block
    Terminator(Terminator),
    // LLVM machine instructions
    Instruction(Instruction),
}

impl Eq for Op {}

impl Matchable for Op {
    fn is_match(&self, query: &str) -> bool {
        format!("{}", self).contains(query)
    }
}

impl OpInfo<LlvmIr> for Op {
    fn get_cf(&self) -> Option<CF<LlvmIr>> {
        match self {
            Op::Terminator(Terminator::Ret(_)) => Some(CF::Return),
            Op::Terminator(terminator) => Some(CF::Brs(match terminator {
                Terminator::Ret(_ret) => unreachable!(),
                Terminator::Br(br) => vec![br.dest.clone()],
                Terminator::CondBr(cond_br) => {
                    vec![cond_br.true_dest.clone(), cond_br.false_dest.clone()]
                }
                Terminator::Switch(switch) => [
                    switch
                        .dests
                        .iter()
                        .map(|(_val, dest)| dest)
                        .cloned()
                        .collect(),
                    vec![switch.default_dest.clone()],
                ]
                .concat(),
                Terminator::IndirectBr(indirect_br) => indirect_br.possible_dests.clone(),
                Terminator::Invoke(invoke) => vec![
                    invoke.result.clone(),
                    invoke.return_label.clone(),
                    invoke.exception_label.clone(),
                ],
                Terminator::Resume(_resume) => Vec::default(),
                Terminator::Unreachable(_unreachable) => Vec::default(),
                Terminator::CleanupRet(cleanup_ret) => {
                    cleanup_ret.unwind_dest.iter().cloned().collect()
                }
                Terminator::CatchRet(catch_ret) => vec![catch_ret.successor.clone()],
                Terminator::CatchSwitch(catch_switch) => [
                    catch_switch.catch_handlers.clone(),
                    catch_switch.default_unwind_dest.iter().cloned().collect(),
                    vec![catch_switch.result.clone()],
                ]
                .concat(),
                Terminator::CallBr(call_br) => {
                    vec![call_br.result.clone(), call_br.return_label.clone()]
                }
            })),
            _ => None,
        }
    }

    fn symbols_used(&self) -> impl Iterator<Item = <LlvmIr as Language>::Symbol> {
        match self {
            Op::Constant(c) => match c.as_ref() {
                Constant::GlobalReference {
                    name: Name::Name(sym_name),
                    ..
                } => Some(sym_name.to_owned()),
                _ => None,
            },
            _ => None,
        }
        .into_iter()
    }

    fn sym_name(&self) -> Option<<LlvmIr as Language>::Symbol> {
        match self {
            Op::GlobalVariable(global_variable) => match &global_variable.name {
                Name::Name(name) => Some(name.to_owned()),
                _ => None,
            },
            Op::GlobalAlias(global_alias) => match &global_alias.name {
                Name::Name(name) => Some(name.to_owned()),
                _ => None,
            },
            Op::GlobalIFunc(global_ifunc) => match &global_ifunc.name {
                Name::Name(name) => Some(name.to_owned()),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Hash, Debug, Display, From)]
pub enum Var {
    Var {
        name: Name,
    },
    Symbol {
        symbol: <LlvmIr as Language>::Symbol,
    },
    Metadata,
}

impl Eq for Var {}

impl Matchable for Var {
    fn is_match(&self, query: &str) -> bool {
        match self {
            Self::Var { name } => name.is_match(query),
            Self::Symbol { symbol } => symbol.is_match(query),
            Self::Metadata => false,
        }
    }
}

impl Fresh for Var {
    fn fresh(number: usize) -> Self {
        Name::from(number).into()
    }
}

impl WithType for Var {
    fn get_type(&self) -> WireType {
        match self {
            Var::Symbol { .. } => WireType::SymName,
            _ => WireType::Data,
        }
    }
}

impl Matchable for Name {
    fn is_match(&self, query: &str) -> bool {
        match self {
            Name::Name(name) => **name == query,
            Name::Number(_) => false,
        }
    }
}

// Conversion from `llvm_ir` crate types.

impl From<Module> for Expr {
    fn from(module: Module) -> Self {
        Self {
            binds: [
                module.functions.into_iter().map_into().collect::<Vec<_>>(),
                module.global_vars.into_iter().map_into().collect(),
                module.global_aliases.into_iter().map_into().collect(),
                module.global_ifuncs.into_iter().map_into().collect(),
            ]
            .concat(),
            values: Vec::default(),
        }
    }
}

impl From<Instruction> for Bind {
    fn from(instr: Instruction) -> Self {
        Bind {
            defs: instr
                .try_get_result()
                .into_iter()
                .cloned()
                .map_into()
                .collect(),
            value: instr.into(),
        }
    }
}

impl From<Function> for Bind {
    fn from(function: Function) -> Self {
        Self {
            defs: Vec::default(),
            value: Value::Thunk(function.into()),
        }
    }
}

macro_rules! from_global_bind {
    ($ty:ty) => {
        impl From<$ty> for Bind {
            fn from(global: $ty) -> Self {
                Self {
                    defs: Vec::default(),
                    value: global.into(),
                }
            }
        }
    };
}
from_global_bind!(GlobalVariable);
from_global_bind!(GlobalAlias);
from_global_bind!(GlobalIFunc);

impl From<Function> for Thunk {
    fn from(function: Function) -> Self {
        Thunk {
            addr: function.name,
            args: function
                .parameters
                .into_iter()
                .map(|parameter| parameter.name.into())
                .collect(),
            reqs: Vec::default(),
            body: Expr::default(),
            blocks: function.basic_blocks.into_iter().map_into().collect(),
        }
    }
}

impl From<BasicBlock> for Block {
    fn from(bb: BasicBlock) -> Self {
        Block {
            addr: bb.name,
            args: Vec::default(),
            expr: Expr {
                binds: [
                    bb.instrs.into_iter().map_into().collect(),
                    vec![Bind {
                        defs: Vec::default(),
                        value: bb.term.into(),
                    }],
                ]
                .concat(),
                values: Vec::default(),
            },
        }
    }
}

macro_rules! from_constant_value {
    ($ty:ty, $variant:ident) => {
        impl From<$ty> for Value {
            fn from(c: $ty) -> Self {
                Value::Op {
                    op: Op::$variant(c.into()),
                    args: Vec::default(),
                }
            }
        }
    };
}
from_constant_value!(ConstantRef, Constant);
from_constant_value!(RMWBinOp, RMWBinOp);
from_constant_value!(IntPredicate, IntPredicate);
from_constant_value!(FPPredicate, FPPredicate);
from_constant_value!(InlineAssembly, InlineAssembly);

impl From<GlobalVariable> for Value {
    fn from(global: GlobalVariable) -> Self {
        Value::Op {
            op: Op::GlobalVariable(global.clone()),
            args: global.initializer.into_iter().map_into().collect(),
        }
    }
}
impl From<GlobalAlias> for Value {
    fn from(global: GlobalAlias) -> Self {
        Value::Op {
            op: Op::GlobalAlias(global.clone()),
            args: vec![global.aliasee.into()],
        }
    }
}
impl From<GlobalIFunc> for Value {
    fn from(global: GlobalIFunc) -> Self {
        Value::Op {
            op: Op::GlobalIFunc(global.clone()),
            args: vec![global.resolver_fn.into()],
        }
    }
}

impl From<Terminator> for Value {
    fn from(term: Terminator) -> Self {
        Value::Op {
            op: Op::Terminator(term.clone()),
            args: match term {
                Terminator::Ret(ret) => ret.return_operand.into_iter().map_into().collect(),
                Terminator::Br(_br) => Vec::default(),
                Terminator::CondBr(cond_br) => vec![cond_br.condition.into()],
                Terminator::Switch(switch) => vec![switch.operand.into()],
                Terminator::IndirectBr(indirect_br) => vec![indirect_br.operand.into()],
                Terminator::Invoke(invoke) => [
                    vec![invoke.function.into()],
                    invoke
                        .arguments
                        .into_iter()
                        .map(|(arg, _attrs)| arg)
                        .map_into()
                        .collect(),
                ]
                .concat(),
                Terminator::Resume(resume) => vec![resume.operand.into()],
                Terminator::Unreachable(_unreachable) => Vec::default(),
                Terminator::CleanupRet(cleanup_ret) => vec![cleanup_ret.cleanup_pad.into()],
                Terminator::CatchRet(catch_ret) => vec![catch_ret.catch_pad.into()],
                Terminator::CatchSwitch(catch_switch) => vec![catch_switch.parent_pad.into()],
                Terminator::CallBr(call_br) => [
                    vec![call_br.function.into()],
                    call_br
                        .arguments
                        .into_iter()
                        .map(|(arg, _attrs)| arg)
                        .map_into()
                        .collect(),
                ]
                .concat(),
            },
        }
    }
}

impl From<Instruction> for Value {
    fn from(instr: Instruction) -> Self {
        Value::Op {
            op: Op::Instruction(instr.clone()),
            args: match instr {
                // NB: the TODOs listed below signify extra information presented by `llvm-ir`, but
                // currently ignored here
                Instruction::Add(add) => vec![add.operand0.into(), add.operand1.into()],
                Instruction::Sub(sub) => vec![sub.operand0.into(), sub.operand1.into()],
                Instruction::Mul(mul) => vec![mul.operand0.into(), mul.operand1.into()],
                Instruction::UDiv(udiv) => vec![udiv.operand0.into(), udiv.operand1.into()],
                Instruction::SDiv(sdiv) => vec![sdiv.operand0.into(), sdiv.operand1.into()],
                Instruction::URem(urem) => vec![urem.operand0.into(), urem.operand1.into()],
                Instruction::SRem(srem) => vec![srem.operand0.into(), srem.operand1.into()],
                Instruction::And(and) => vec![and.operand0.into(), and.operand1.into()],
                Instruction::Or(or) => vec![or.operand0.into(), or.operand1.into()],
                Instruction::Xor(xor) => vec![xor.operand0.into(), xor.operand1.into()],
                Instruction::Shl(shl) => vec![shl.operand0.into(), shl.operand1.into()],
                Instruction::LShr(lshr) => vec![lshr.operand0.into(), lshr.operand1.into()],
                Instruction::AShr(ashr) => vec![ashr.operand0.into(), ashr.operand1.into()],
                Instruction::FAdd(fadd) => vec![fadd.operand0.into(), fadd.operand1.into()],
                Instruction::FSub(fsub) => vec![fsub.operand0.into(), fsub.operand1.into()],
                Instruction::FMul(fmul) => vec![fmul.operand0.into(), fmul.operand1.into()],
                Instruction::FDiv(fdiv) => vec![fdiv.operand0.into(), fdiv.operand1.into()],
                Instruction::FRem(frem) => vec![frem.operand0.into(), frem.operand1.into()],
                Instruction::FNeg(fneg) => vec![fneg.operand.into()],
                Instruction::ExtractElement(extract_element) => {
                    vec![extract_element.vector.into(), extract_element.index.into()]
                }
                Instruction::InsertElement(insert_element) => vec![
                    insert_element.vector.into(),
                    insert_element.element.into(),
                    insert_element.index.into(),
                ],
                Instruction::ShuffleVector(shuffle_vector) => vec![
                    shuffle_vector.operand0.into(),
                    shuffle_vector.operand1.into(),
                ],
                Instruction::ExtractValue(extract_value) => [
                    vec![extract_value.aggregate.into()],
                    extract_value
                        .indices
                        .into_iter()
                        .map(|idx| {
                            Operand::ConstantOperand(ConstantRef::new(Constant::Int {
                                bits: 32,
                                value: idx.into(),
                            }))
                            .into()
                        })
                        .collect(),
                ]
                .concat(),
                Instruction::InsertValue(insert_value) => [
                    vec![insert_value.element.into()],
                    insert_value
                        .indices
                        .into_iter()
                        .map(|idx| {
                            Operand::ConstantOperand(ConstantRef::new(Constant::Int {
                                bits: 32,
                                value: idx.into(),
                            }))
                            .into()
                        })
                        .collect(),
                ]
                .concat(),
                Instruction::Alloca(alloca) => vec![
                    /* TODO: handle type */
                    alloca.num_elements.into(),
                    /* TODO: handle alignment */
                ],
                Instruction::Load(load) => vec![
                    load.address.into(),
                    /* TODO: handle type */
                    /* TODO: handle volatile */
                    /* TODO: handle atomicity */
                    /* TODO: handle alignment */
                ],
                Instruction::Store(store) => vec![
                    store.address.into(),
                    store.value.into(),
                    /* TODO: handle volatile */
                    /* TODO: handle atomicity */
                    /* TODO: handle alignment */
                ],
                Instruction::Fence(_fence) => vec![/* TODO: handle atomicity */],
                Instruction::CmpXchg(cmp_xchg) => vec![
                    cmp_xchg.address.into(),
                    cmp_xchg.expected.into(),
                    cmp_xchg.replacement.into(),
                    /* TODO: handle volatile */
                    /* TODO: handle atomicity */
                    /* TODO: handle memory ordering */
                    /* TODO: handle weak */
                ],
                Instruction::AtomicRMW(atomic_rmw) => vec![
                    atomic_rmw.operation.into(),
                    atomic_rmw.address.into(),
                    atomic_rmw.value.into(),
                    /* TODO: handle volatile */
                    /* TODO: handle atomicity */
                ],
                Instruction::GetElementPtr(get_element_ptr) => [
                    vec![get_element_ptr.address.into()],
                    get_element_ptr.indices.into_iter().map_into().collect(),
                    /* TODO: handle in bounds */
                    /* TODO: handle type */
                ]
                .concat(),
                Instruction::Trunc(trunc) => vec![
                    trunc.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::ZExt(zext) => vec![
                    zext.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::SExt(sext) => vec![
                    sext.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::FPTrunc(fptrunc) => vec![
                    fptrunc.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::FPExt(fpext) => vec![
                    fpext.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::FPToUI(fpto_ui) => vec![
                    fpto_ui.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::FPToSI(fpto_si) => vec![
                    fpto_si.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::UIToFP(uito_fp) => vec![
                    uito_fp.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::SIToFP(sito_fp) => vec![
                    sito_fp.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::PtrToInt(ptr_to_int) => vec![
                    ptr_to_int.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::IntToPtr(int_to_ptr) => vec![
                    int_to_ptr.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::BitCast(bit_cast) => vec![
                    bit_cast.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::AddrSpaceCast(addr_space_cast) => vec![
                    addr_space_cast.operand.into(),
                    /* TODO: handle type */
                ],
                Instruction::ICmp(icmp) => vec![
                    icmp.predicate.into(),
                    icmp.operand0.into(),
                    icmp.operand1.into(),
                ],
                Instruction::FCmp(fcmp) => vec![
                    fcmp.predicate.into(),
                    fcmp.operand0.into(),
                    fcmp.operand1.into(),
                ],
                Instruction::Phi(phi) => phi
                    .incoming_values
                    .into_iter()
                    .map(|(operand, _name)| operand.into())
                    .collect(),
                Instruction::Select(select) => vec![
                    select.condition.into(),
                    select.true_value.into(),
                    select.false_value.into(),
                ],
                Instruction::Freeze(freeze) => vec![freeze.operand.into()],
                Instruction::Call(call) => [
                    vec![call.function.unwrap_right().into()],
                    call.arguments
                        .into_iter()
                        .map(|(operand, _attr)| operand.into())
                        .collect(),
                    /* TODO: handle parameter attributes */
                    /* TODO: handle function attributes */
                    /* TODO: handle is_tail_call */
                    /* TODO: handle calling convention */
                ]
                .concat(),
                Instruction::VAArg(vaarg) => vec![
                    vaarg.arg_list.into(),
                    /* TODO: handle type */
                ],
                Instruction::LandingPad(_landing_pad) => {
                    vec![
                    /* TODO: handle type */
                    /* TODO: handle clauses */
                    /* TODO: handle cleanup */
                    ]
                }
                Instruction::CatchPad(catch_pad) => [
                    vec![catch_pad.catch_switch.into()],
                    catch_pad.args.into_iter().map_into().collect(),
                ]
                .concat(),
                Instruction::CleanupPad(cleanup_pad) => [
                    vec![cleanup_pad.parent_pad.into()],
                    cleanup_pad.args.into_iter().map_into().collect(),
                ]
                .concat(),
            },
        }
    }
}

impl From<Operand> for Value {
    fn from(value: Operand) -> Self {
        match value {
            Operand::LocalOperand { name, .. } => Value::Variable(name.into()),
            Operand::ConstantOperand(c) => c.into(),
            Operand::MetadataOperand => Value::Variable(Var::Metadata),
        }
    }
}

impl From<Either<InlineAssembly, Operand>> for Value {
    fn from(value: Either<InlineAssembly, Operand>) -> Self {
        match value {
            Either::Left(inline_assembly) => inline_assembly.into(),
            Either::Right(operand) => operand.into(),
        }
    }
}

pub fn parse(input: &str) -> Result<Expr, String> {
    let module = llvm_ir::Module::from_ir_str(input)?;
    debug!(
        "Parsed LLVM IR module\n\
            Name: {}\n\
            Source filename: {}\n\
            Target triple: {:?}\n\
            Functions: {:#?}\n\
            Function declarations: {:#?}\n\
            Global variables: {:#?}\n\
            Global aliases: {:#?}\n\
            Global ifuncs: {:#?}\n\
            Inline assembly: {}",
        module.name,
        module.source_file_name,
        module.target_triple,
        module.functions,
        module.func_declarations,
        module.global_vars,
        module.global_aliases,
        module.global_ifuncs,
        module.inline_assembly,
    );
    Ok(module.into())
}

#[cfg(test)]
pub(crate) mod tests {
    use std::path::Path;

    use dir_test::{Fixture, dir_test};

    use super::{Expr, parse};

    pub fn parse_llvm_ir(raw_path: &str) -> (&str, Expr) {
        let path = Path::new(raw_path);
        let program = std::fs::read_to_string(path).unwrap();
        let expr = parse(&program).unwrap_or_else(|err| {
            panic!(
                "could not parse program {:?}\n{err:?}",
                path.file_stem().unwrap()
            )
        });
        let name = path.file_stem().unwrap().to_str().unwrap();
        (name, expr)
    }

    #[dir_test(dir: "$CARGO_MANIFEST_DIR/../examples", glob: "**/*.ll", loader: crate::language::llvm_ir::tests::parse_llvm_ir, postfix: "check_parse")]
    fn check_parse(fixture: Fixture<(&str, Expr)>) {
        let (_name, _expr) = fixture.content();
    }
}
