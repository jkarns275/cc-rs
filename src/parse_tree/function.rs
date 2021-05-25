use crate::parse_tree::*;

pub enum DeclOrStmnt {
    Decl(Declaration),
    Stmnt(Statement),
}

impl DeclOrStmnt {
    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        match self {
            DeclOrStmnt::Decl(d) => d.pretty_print(buf, si),
            DeclOrStmnt::Stmnt(s) => s.pretty_print(buf, si),
        }
    }
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

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        self.specs.pretty_print(buf, si);
        buf.push(' ');
        self.decl.pretty_print(buf, si);
        if self.type_specs.len() > 0 {
            for decl in self.type_specs.iter() {
                buf.push_str("    ");
                decl.pretty_print(buf, si);
                buf.push_str(";\n");
            }
            buf.pop();
            buf.pop();
        }
        buf.push_str(" {\n");
        for s in self.body.iter() {
            buf.push_str("    ");
            s.pretty_print(buf, si);
            buf.push_str(";\n")
        }
        buf.push_str("}\n");
    }
}
