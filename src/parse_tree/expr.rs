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

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        self.expr.pretty_print(buf, si);
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

impl UnaryOp {
    pub fn is_pre(self) -> bool {
        match self {
            IncPost => false,
            DecPost => false,
            _ => true
        }
    }
    pub fn pretty_print(self, buf: &mut String) {
        buf.push_str(
            match self {
               Lea => "&",
               Deref => "*",
               Pos => "+",
               Neg => "-",
               BitNeg => "~",
               Not => "!",
               IncPre => "++",
               IncPost => "++",
               DecPre => "--",
               DecPost => "--",
            }
        )
    }
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

impl BinOp {
    pub fn pretty_print(self, buf: &mut String) {
        buf.push_str(
            match self {
                Mul => "*",
                Div => "/",
                Mod => "%",
                Add => "+",
                Sub => "-",
                LShift => "<<",
                RShift => ">>",
                Gt => ">",
                Lt => ">",
                Gte => ">=",
                Lte => "<=",
                Eq => "==",
                Neq => "!=",
                Nop => "NOP",
                BAnd => "&",
                BOr => "|",
                Xor => "^",
                LAnd => "&&",
                LOr => "||",
                Assign => "=",
                MulAssign => "*=",
                DivAssign => "/=",
                ModAssign => "%=",
                AddAssign => "+=",
                SubAssign => "-=",
                LshAssign => "<<",
                RshAssign => ">>",
                AndAssign => "&=",
                XorAssign => "^=",
                OrAssign => "|=",
            }
        )
    }
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

impl Expr {
    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        buf.push('(');
        match self {
            Index { base, offset, } => {
                base.pretty_print(buf, si);
                buf.push('[');
                offset.pretty_print(buf, si);
                buf.push(']');
            },
            Call { fun, args, } => {
                fun.pretty_print(buf, si);
                buf.push('(');
                for arg in args.iter() {
                    arg.pretty_print(buf, si);
                    buf.push_str(", ");
                }
                if args.len() > 0 {
                    buf.pop(); buf.pop();
                }
                buf.push(')');
            },
            Dot { expr, field, } => {
                expr.pretty_print(buf, si);
                buf.push('.');
                buf.push_str(si.get(*field).as_str());
            },
            Arrow { expr, field, } => {
                expr.pretty_print(buf, si);
                buf.push('->');
                buf.push_str(si.get(*field).as_str());
            },
            UnaryOp { expr, unop, } => {
                if unop.is_pre() {
                    unop.pretty_print(buf);
                    expr.pretty_print(buf, si);
                } else {
                    expr.pretty_print(buf, si);
                    unop.pretty_print(buf);
                }
            },
            Sizeof { expr, } => {
                buf.push_str("sizeof (");
                expr.pretty_print(buf, si);
                buf.push(')');
            },
            SizeofType { ty, } => {
                buf.push_str("sizeof (");
                ty.pretty_print(buf, si);
                buf.push(')');
            },
            Cast { ty, expr, } => {
                buf.push('(');
                ty.pretty_print(buf, si);
                buf.push_str(") ");
                expr.pretty_print(buf, si);
            },
            BinOp { lhs, op, rhs, } => {
                lhs.pretty_print(buf, si);
                buf.push(' ');
                op.pretty_print(buf);
                buf.push(' ');
                rhs.pretty_print(buf, si);
            },
            Ternary { cond, tval, fval, } => {
                cond.pretty_print(buf, si);
                buf.push_str(" ? ");
                tval.pretty_print(buf, si);
                buf.push_str(" : ");
                fval.pretty_print(buf, si);
            },
            Comma { exprs } => {
                for e in exprs.iter() {
                    e.pretty_print(buf, si);
                }
                if exprs.len() > 0 {
                    buf.pop(); buf.pop();
                }
            },
            StringConst(string_constant) => {
                buf.push_str("\"<STRING CONSTANT>\"");
            },
            IntConst(i, ity) => {
                buf.push_str(i.to_string().as_str());
                buf.push(
                    match ity {
                        IntType::Unsigned => 'U',
                        IntType::Long => 'L',
                    }
                )
            },
            FloatConst(f, fty) => {
                buf.push_str(f.to_string().as_str());
                buf.push(
                    match fty {
                        FloatType::Float => 'F',
                        FloatType::Double => 'L',
                    }
                );
            },
            Id(id) => {
                buf.push_str(f.to_string().as_str());
            },
        };
        buf.push(')');
    }
}
