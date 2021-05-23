use crate::parse_tree::TaggedExpr;
use declarator::Declarator;
use crate::interner::IValue;

pub enum StructDeclarator {
    Declarator(Declarator),
    BitField(Option<Declarator>, usize),
}

pub struct StructDeclaration {
    pub id: IValue<String>,
    pub ty: Ty,
    pub fields: Box<[StructDeclarator]>,
}

pub struct Enumeration {
    pub id: IValue<String>,
    pub value: Option<TaggedExpr>,
}