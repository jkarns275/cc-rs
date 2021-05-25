use crate::parse_tree::*;

#[derive(Clone)]
pub enum StructDeclarator {
    Declarator(Declarator),
    BitField(Option<Declarator>, TaggedExpr),
}

impl StructDeclarator {
    pub fn pretty_print(&self, buf: &mut String, str_interner: &Interner<String>) {
        match self {
            StructDeclarator::Declarator(d) => d.pretty_print(buf, str_interner),
            StructDeclarator::BitField(decl, exp) => {
                if let Some(d) = decl {
                    d.pretty_print(buf, str_interner);
                }
                buf.push_str(":");
                exp.pretty_print(buf, str_interner);
            },
        }
    }
}

#[derive(Clone)]
pub struct StructDeclaration {
    pub ty: Ty,
    pub decls: Box<[StructDeclarator]>,
}

impl StructDeclaration {
    pub fn new(ty: Ty, decls: Box<[StructDeclarator]>) -> Self {
        StructDeclaration { ty, decls, }
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        self.ty.pretty_print(buf, si);
        for d in self.decls.iter() {
            buf.push_str("\n        ");
            d.pretty_print(buf, si);
            buf.push(',');
        }
        if self.decls.len() > 0 { buf.pop(); }

    }
}

#[derive(Copy, Clone)]
pub enum StructureType { Struct, Union }

#[derive(Clone)]
pub struct Structure {
    pub id: Option<IValue<String>>,
    pub structure_type: StructureType,
    pub fields: Box<[StructDeclaration]>,
}

impl Structure {
    pub fn new(structure_type: StructureType, id: Option<IValue<String>>, fields: Box<[StructDeclaration]>) -> Self {
        Structure { id, fields, structure_type, }
    }

    pub fn pretty_print(&self, buf: &mut String, str_interner: &Interner<String>) {
        match self.id {
            Some(id) => {
                let s = str_interner.get(id);
                buf.push_str("struct ");
                buf.push_str(s);
                if self.fields.len() == 0 {
                    buf.push_str("{}");
                } else {
                    buf.push_str(" {\n");
                    for field in self.fields.iter() {
                        buf.push_str("    ");
                        field.pretty_print(buf, str_interner);
                        buf.push_str(";\n");
                    }
                    buf.push_str("}\n");
                }
            },
            None => {
                if self.fields.len() == 0 {
                    buf.push_str("struct {}");
                } else {
                    buf.push_str("struct {\n");
                    for field in self.fields.iter() {
                        buf.push_str("    ");
                        field.pretty_print(buf, str_interner);
                        buf.push_str(";\n");
                    }
                    buf.push_str("}");
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct Enumerator {
    pub id: IValue<String>,
    pub value: Option<TaggedExpr>,
}

impl Enumerator {
    pub fn new(id: IValue<String>, value: Option<TaggedExpr>) -> Self {
        Enumerator { id, value, }
    }
    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        buf.push_str(si.get(self.id));
        if let Some(v) = &self.value {
            buf.push_str(" = ");
            v.pretty_print(buf, si);
        }
    }
}

#[derive(Clone)]
pub struct Enumeration {
    pub id: Option<IValue<String>>,
    pub es: Box<[Enumerator]>,
}

impl Enumeration {
    pub fn new(id: Option<IValue<String>>, es: Box<[Enumerator]>) -> Self {
        Enumeration { id, es, }
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        if let Some(id) = self.id {
            buf.push_str("enum ");
            buf.push_str(si.get(id));
            buf.push_str(" {");
        }
        if self.es.len() == 0 {
            buf.push_str("}");
        } else {
            for e in self.es.iter() {
                buf.push_str("    ");
                e.pretty_print(buf, si);
                buf.push_str(",\n");
            }
            buf.pop(); buf.pop();
            buf.push_str("\n}");
        }
    }
}
