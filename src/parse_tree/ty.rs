use std::cmp::Ordering;
use std::fmt;

use crate::interner::*;
use crate::parse_tree::*;

#[derive(Clone)]
#[repr(i32)]
pub enum TySpec {
    Unsigned,
    Signed,
    Long,
    Int,
    Void,
    Char,
    Short,
    Float,
    Double,
    Structure(Structure),
    Enumeration(Enumeration),
    Named(IValue<String>),
}

impl TySpec {
    fn as_i32(&self) -> i32 {
        match &self {
            TySpec::Unsigned  => -1,
            TySpec::Signed    => -2,
            TySpec::Long      => 0,
            TySpec::Int       => 1,
            TySpec::Void      => 2,
            TySpec::Char      => 3,
            TySpec::Short     => 4,
            TySpec::Float     => 5,
            TySpec::Double    => 6,
            TySpec::Structure(_) => 7,
            TySpec::Enumeration(_) => 8,
            TySpec::Named(_)  => 9,
        }
    }
}

impl fmt::Debug for TySpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            TySpec::Unsigned  => "unsigned",
            TySpec::Signed    => "signed",
            TySpec::Long      => "long",
            TySpec::Int       => "int",
            TySpec::Void      => "void",
            TySpec::Char      => "char",
            TySpec::Short     => "short",
            TySpec::Float     => "float",
            TySpec::Double    => "double",
            TySpec::Structure(_) => "struct { ... }",
            TySpec::Enumeration(_) => "enum { ... }",
            TySpec::Named(_)  => "<named_type>",
        })
    }
}

impl PartialEq for TySpec {
    fn eq(&self, other: &Self) -> bool {
        self.as_i32() == other.as_i32()
    }
}

impl Eq for TySpec {}

impl Ord for TySpec {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_i32().cmp(&other.as_i32())
    }
}

impl PartialOrd for TySpec {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Copy, Clone)]
#[repr(i32)]
pub enum TyQual {
    None        = 0x00,
    Const       = 0b01,
    Volatile    = 0b10,
    ConstVol    = 0b11,
}

impl TyQual {
    pub fn is_const(x: i32) -> bool {
        x & TyQual::Const as i32 != 0
    }

    pub fn is_volatile(x: i32) -> bool {
        x & TyQual::Volatile as i32 != 0
    }
}

#[derive(Clone)]
pub struct TySpecQualList {
    pub specs: Vec<TySpec>,
    pub quals: i32,
}

impl TySpecQualList {
    pub fn new() -> Self {
        TySpecQualList {
            specs: vec![],
            quals: 0,
        }
    }

    pub fn merge(mut left: Self, mut right: Self) -> Self {
        left.specs.append(&mut right.specs);
        let specs = left.specs;
        let quals = left.quals | right.quals;
        TySpecQualList { specs, quals, }
    }

    fn get_ty_kind(&mut self) -> TyKind {
        use TySpec::*;
        use TyKind::*;
        use FloatTy::*;
        use IntegralTy::*;

        self.specs.retain(|x| match x { Int | Signed => false, _ => true });
        self.specs.sort_unstable();

        // https://en.wikipedia.org/wiki/C_data_types
        match &self.specs[..] {
            [Char]              => Integral(I8),
            [Unsigned, Char]    => Integral(U8),
            [Short]             => Integral(I16),
            [Unsigned, Short]   => Integral(U16),
            []                  => Integral(I32),
            [Unsigned]          => Integral(U32),
            [Long]              => Integral(I64),
            [Unsigned, Long]    => Integral(U64),
            [Long, Long]        => Integral(I64),
            [Unsigned, Long, Long] 
                                => Integral(U64),
            [TySpec::Float]     => TyKind::Float(F32),
            [Double]            => TyKind::Float(F64),
            [Long, Double]      => TyKind::Float(F80),
            [TySpec::Void]      => TyKind::Void,
            [TySpec::Structure(s)]
                                => TyKind::Structure(s.clone()),
            [TySpec::Enumeration(e)]
                                => TyKind::Enumeration(e.clone()),
            [TySpec::Named(id)] => TyKind::Named(*id),
            _                   => panic!("Unrecognized type, {:?} TODO: Add actual error handling.", &self.specs[..])
        }
    }

    pub fn into_type(mut self) -> Ty {
        Ty::new(self.get_ty_kind(), TyQual::is_volatile(self.quals), TyQual::is_const(self.quals))
    }

    pub fn get_type(&mut self) -> Ty {
        Ty::new(self.get_ty_kind(), TyQual::is_volatile(self.quals), TyQual::is_const(self.quals))
    }

    pub fn add_qual(mut self, q: TyQual) -> Self {
        self.quals |= q as i32;
        self
    }

    pub fn add_spec(mut self, s: TySpec) -> Self {
        self.specs.push(s);
        self
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        let volatile = TyQual::is_volatile(self.quals);
        let constant = TyQual::is_const(self.quals);

        if volatile {
            buf.push_str("volatile ");
        }
        if constant {
            buf.push_str("constant ");
        }

        let ty = self.clone().into_type();
        ty.pretty_print(buf, si);
    }
}

/*
 * Each element in the vector represents a pointer, and the actual value corresponds
 * to a i32 with bit flag set of TyQuals.
 */
#[derive(Clone)]
pub struct PtrTy(pub Vec<i32>);

impl PtrTy {
    pub fn new() -> Self { PtrTy(vec![]) }

    pub fn wrap_type(&self, mut ty: Ty) -> Ty {
        if self.0.len() == 0 {
            ty
        } else {
            // C pointer types are in reverse order
            // e.g. char * * volatile is a volatile pointer to a pointer to a char
            //      char * volatile * is a pointer to a volatile pointer to a char.
            // But the order that the parser reads them in should be reversed...
            for &x in self.0.iter() {
                let volatile = TyQual::is_volatile(x);
                let constant = TyQual::is_const(x);
                ty = Ty::new(TyKind::Ptr(Box::new(ty)), volatile, constant);
            }
            ty
        }
    }

    pub fn push(mut self, i: i32) -> Self {
        self.0.push(i);
        self
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        if self.0.len() == 0 {
            ()
        } else {
            // C pointer types are in reverse order
            // e.g. char * * volatile is a volatile pointer to a pointer to a char
            //      char * volatile * is a pointer to a volatile pointer to a char.
            // But the order that the parser reads them in should be reversed...
            for &x in self.0.iter() {
                let volatile = TyQual::is_volatile(x);
                let constant = TyQual::is_const(x);
                if volatile {
                    buf.push_str("volatile ");
                }
                if constant {
                    buf.push_str("constant ");
                }
                buf.push('*');
            }
        }
    }
}

#[derive(Clone, Copy)]
pub enum IntegralTy {
    I64,
    U64,
    I32,
    U32,
    I16,
    U16,
    I8,
    U8,
}

impl IntegralTy {

    pub fn pretty_print(self, buf: &mut String) {
        use IntegralTy::*;
        buf.push_str(match self {
            I64 => "long",
            U64 => "unsigned long",
            I32 => "int",
            U32 => "unsigned int",
            I16 => "short",
            U16 => "unsigned short",
            I8  => "char",
            U8  => "unsigned char",
        })
    }

}

#[derive(Clone, Copy)]
pub enum FloatTy { F80, F64, F32 }

impl FloatTy {

    pub fn pretty_print(self, buf: &mut String) {
        use FloatTy::*;
        buf.push_str(match self {
            F80 => "long double",
            F64 => "double",
            F32 => "float",
        });
    }

}

#[derive(Clone)]
pub enum TyKind {
    TBD,
    Void,
    Ptr(Box<Ty>),
    Float(FloatTy),
    Integral(IntegralTy),
    Structure(Structure),
    Named(IValue<String>),
    Fn(Box<Ty>, Box<[Ty]>),
    Enumeration(Enumeration),
    Array(Box<Ty>, Option<Box<TaggedExpr>>),
}

impl TyKind {

    pub fn pretty_print(&self, buf: &mut String, str_interner: &Interner<String>) {
        use TyKind::*;
        match &self {
            Named(s) => 
                buf.push_str(str_interner.get(*s)),
            Structure(s) =>
                s.pretty_print(buf, str_interner),
            Enumeration(e) => 
                e.pretty_print(buf, str_interner),
            Ptr(p) => {
                buf.push_str("pointer to");
                p.pretty_print(buf, str_interner);
            },
            Fn(ret_type, arg_tys) => {
                buf.push_str("function (");
                for arg in arg_tys.iter() {
                    arg.pretty_print(buf, str_interner);
                    buf.push_str(", ");
                }
                if arg_tys.len() != 0 { buf.pop(); buf.pop(); }
                buf.push_str(") returning ");
                ret_type.pretty_print(buf, str_interner)
            },
            Integral(i) => i.pretty_print(buf),
            Float(f) => f.pretty_print(buf),
            Array(ty, Some(size_expr)) => {
                buf.push_str("array ");
                size_expr.pretty_print(buf, str_interner);
                buf.push_str(" of ");
                ty.pretty_print(buf, str_interner);
            },
            Array(ty, None) => {
                buf.push_str("array of ");
                ty.pretty_print(buf, str_interner);
            },
            Void => buf.push_str("void"),
            _ => buf.push_str("ICE"),
        }
    }

}

#[derive(Clone)]
pub struct Ty {
    pub kind: TyKind,
    pub volatile: bool,
    pub constant: bool,
}

impl Ty {
    pub fn new(kind: TyKind, volatile: bool, constant: bool) -> Self {
        Ty { kind, volatile, constant, }
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        if self.volatile {
            buf.push_str("volatile ");
        }
        if self.constant {
            buf.push_str("constant ");
        }
        self.kind.pretty_print(buf, si);
    }
}
