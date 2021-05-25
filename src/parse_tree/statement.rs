use crate::interner::IValue;
use crate::parse_tree::*;

pub enum Statement {
    Labeled(IValue<String>, Box<Statement>),
    Compound(Box<[DeclOrStmnt]>),
    Expr(TaggedExpr),
    If(TaggedExpr, Box<Statement>, Option<Box<Statement>>),
    Switch(TaggedExpr, Box<Statement>),
    DoWhile(Box<Statement>, TaggedExpr),
    While(TaggedExpr, Box<Statement>),
    For { 
        init: TaggedExpr,
        cond: TaggedExpr,
        inc: Option<TaggedExpr>,
        body: Box<Statement>
    },
    Goto(IValue<String>),
    Continue,
    Break,
    Return(Option<TaggedExpr>),
    Nop,
}
