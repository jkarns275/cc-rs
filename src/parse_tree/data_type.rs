use crate::parse_tree::*;

#[derive(Clone)]
pub enum StructDeclarator {
    Declarator(Declarator),
    BitField(Option<Declarator>, TaggedExpr),
}

#[derive(Copy, Clone)]
pub enum StructureType { Struct, Union }

#[derive(Clone)]
pub struct Structure {
    pub id: Option<IValue<String>>,
    pub structure_type: StructureType,
    pub fields: Box<[StructDeclarator]>,
}

impl Structure {
    pub fn new(structure_type: StructureType, id: Option<IValue<String>>, fields: Box<[StructDeclarator]>) {
        Structure { id, fields, structure_type, }
    }

    pub fn pretty_print(&self, buf: &mut String, str_interner: &Interner<String>) {
        match self.id {
            Some(id) => {
                let s = str_interner.get(*id);
                buf.push_str("struct ");
                buf.push_str(s);
                if self.fields.len() == 0 {
                    buf.push_str("{}"
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
                    buf.push_str("}\n");
                }
            }
        }
    }
}

pub struct Enumerator {
    pub id: IValue<String>,
    pub value: Option<TaggedExpr>,
}

pub struct Enumeration {
    pub id: Option<IValue<String>>,
    pub es: Box<[Enumerator]>,
}

impl Enumeration {
    pub fn new(id: Option<IValue<String>>, es: Box<[Enumerator]>) -> Self {
        Enumeration { id, es, }
    }
}
