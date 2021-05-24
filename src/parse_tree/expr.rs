use crate::interner::*;
use crate::parse_tree::*;
pub use crate::lexer::{FloatType, IntType};

#[derive(Clone)]
pub struct TaggedExpr {
    pub ty: Option<Ty>,
    pub loc: (usize, usize),
    pub expr: Box<Expr>,
}

impl TaggedExpr {
    pub fn new(expr: Expr, start: usize, end: usize) -> Self {
        TaggedExpr {
            expr: Box::new(expr),
            ty: None,
            loc: (start, end)
        }
    }

    pub fn pretty_print(&self) -> String {
        "<some expression>".to_string()
    }
}

#[derive(Copy, Clone)]
pub enum UnaryOp {
    Lea,
    Deref,
    Pos,
    Neg,
    BitNeg,
    Not,
    IncPre,
    IncPost,
    DecPre,
    DecPost,
}

#[derive(Copy, Clone)]
pub enum BinOp {
    Mul, Div, Mod,
    Add, Sub,
    LShift, RShift,
    Gt, Lt, Gte, Lte, Eq, Neq,
    Nop,
    BAnd, BOr, Xor,
    LAnd, LOr,
    Assign,
    MulAssign, DivAssign, ModAssign,
    AddAssign, SubAssign,
    LshAssign, RshAssign,
    AndAssign, XorAssign, OrAssign,
}

#[derive(Clone)]
pub enum Expr {
    Index { base: TaggedExpr, offset: TaggedExpr, },
    Call { fun: TaggedExpr, args: Box<[TaggedExpr]>, },
    Dot { expr: TaggedExpr, field: IValue<String>, },
    Arrow { expr: TaggedExpr, field: IValue<String>, },
    UnaryOp { expr: TaggedExpr, unop: UnaryOp, },
    Sizeof { expr: TaggedExpr, },
    SizeofType { ty: Ty, },
    Cast { ty: Ty, expr: TaggedExpr, },
    BinOp { lhs: TaggedExpr, op: BinOp, rhs: TaggedExpr, },
    Ternary { cond: TaggedExpr, tval: TaggedExpr, fval: TaggedExpr, },
    Comma { exprs: Box<[TaggedExpr]> },
    StringConst(IValue<Box<[u8]>>),
    IntConst(i128, IntType),
    FloatConst(f64, FloatType),
    Id(IValue<String>),
}