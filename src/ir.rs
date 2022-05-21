//! Intermediate representations of instruction set.

#[allow(missing_docs)]
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum Instruction {
    StackPush(i64),
    StackDuplicate,
    StackCopy(i64),
    StackSwap,
    StackDiscard,
    StackSlide(i64),
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    HeapStore,
    HeapRetrieve,
    Mark(i64),
    Call(i64),
    Jump(i64),
    JumpIfZero(i64),
    JumpIfNegative(i64),
    Return,
    Exit,
    PutCharactor,
    PutNumber,
    GetCharactor,
    GetNumber,
}
