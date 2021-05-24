use crate::parse_tree::*;

pub enum StructDeclarator {
    Declarator(Declarator),
    BitField(Option<Declarator>, TaggedExpr),
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