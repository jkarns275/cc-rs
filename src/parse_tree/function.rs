use crate::parse_tree::*;

pub enum DeclOrStmnt {
    Decl(Declaration),
    Stmnt(Statement),
}

pub struct Function {
    pub specs: DeclSpec,
    pub decl: Declarator,
    pub type_specs: Box<[Declaration]>,
    pub body: Box<[DeclOrStmnt]>,
}

impl Function {
    pub fn new(specs: DeclSpec, decl: Declarator, type_specs: Box<[Declaration]>, body: Box<[DeclOrStmnt]>) -> Self {
        Function { specs, decl, type_specs, body }
    }
}
