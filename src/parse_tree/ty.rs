use std::cmp::Ordering;
use data_type::StructDeclaration;

#[derive(Copy, Clone, Ord, PartalOrd, Eq, PartialEq, Debug)]
pub enum StructType {
    Struct,
    Union
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
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
    Structure(IValue<String>),
    Enum(IValue<String>),
}

impl TySpec {
    fn as_i32(&self) -> i32 {
        match &self {
            Unsigned  => -1,
            Signed    => -2,
            Long      => 0,
            Int       => 1,
            Void      => 2,
            Char      => 3,
            Short     => 4,
            Float     => 5,
            Double    => 6,
            Structure(_) => 7,
            Enum(_) => 8,
        }
    }
}

impl Ord for TySpec {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_i32().cmp(other.as_i32())
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

    fn get_ty_kind(&mut self) -> TyKind {
        use TySpec::*;
        use TyKind::*;
        use FloatTy::*;
        use IntegralTy::*;

        self.specs.retain(|&x| x != Signed);
        self.specs.retain(|&x| x != Int);
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
}

/*
 * Each element in the vector represents a pointer, and the actual value corresponds
 * to a i32 with bit flag set of TyQuals.
 */
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

    pub fn pretty_print(self) -> String {
        use IntegralTy::*;
        match self {
            I64 => "long",
            U64 => "unsigned long",
            I32 => "int",
            U32 => "unsigned int",
            I16 => "short",
            U16 => "unsigned short",
            I8  => "char",
            U8  => "unsigned char",
        }.to_string()
    }

}

#[derive(Clone, Copy)]
pub enum FloatTy { F80, F64, F32 }

impl FloatTy {

    pub fn pretty_print(self) -> String {
        use FloatTy::*;
        match self {
            F80 => "long double",
            F64 => "double",
            F32 => "float",
        }.to_string()
    }

}

pub enum TyKind {
    Named(IValue<String>),
    Struct(IValue<String>),
    Union(IValue<String>),
    Ptr(Box<Ty>),
    Fn(Box<Ty>, Box<[Ty]>),
    Integral(IntegralTy),
    Float(FloatTy),
    Array(Box<Ty>, Option<Box<TaggedExpr>>),
    Void,
}

impl TyKind {

    pub fn pretty_print(&self, str_interner: &Interner<String>) -> String {
        use TyKind::*;
        match &self {
            Named(s) => str_interner.get(*s).clone(),
            Struct(s) => format!("struct {}", str_interner.get(*s).clone()),
            Union(s) => format!("union {}", str_interner.get(*s).clone()),
            Ptr(p) => format!("pointer to{}", p.pretty_print(str_interner)),
            Fn(ret_type, arg_tys) => {
                let mut s = "function (".to_string();
                for arg in arg_tys.iter() {
                    s = format!("{}{}, ", s, arg.pretty_print(str_interner));
                }
                s = format!("{}) returning {}", s, ret_type.pretty_print(str_interner));
                s
            },
            Integral(i) => i.pretty_print(),
            Float(f) => f.pretty_print(),
            Array(ty, Some(size_expr)) => format!("array {} of {}", size_expr.pretty_print(), ty.pretty_print(str_interner)),
            Array(ty, None) => format!("array of {}", ty.pretty_print(str_interner)),
            Void => "void".to_string(),
            _ => String::new()
        }
    }

}

pub struct Ty {
    pub kind: TyKind,
    pub volatile: bool,
    pub constant: bool,
}

impl Ty {
    pub fn new(kind: TyKind, volatile: bool, constant: bool) -> Self {
        Ty { kind, volatile, constant, }
    }

    pub fn pretty_print(&self, str_interner: &Interner<String>) -> String {
        let mut s = String::new();
        if self.volatile {
            s += "volatile ";
        }
        if self.constant {
            s += "constant ";
        }
        format!("{} {}", s, self.kind.pretty_print(str_interner))
    }
}
