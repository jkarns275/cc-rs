use crate::interner::*;
use crate::parse_tree::*;

#[derive(Copy, Clone)]
#[repr(i32)]
pub enum StorageClassSpec {
    Register  = 0b00001,
    Typedef   = 0b00010,
    Extern    = 0b00100,
    Static    = 0b01000,
    Auto      = 0b10000,
    None      = 0b00000,
}

impl StorageClassSpec {
    pub fn pretty_print(spec: i32, buf: &mut String) {
        use StorageClassSpec::*;
        const strs: &'static [&'static str] = &["register", "typedef", "extern", "static", "auto", "none"];
        const v: [StorageClassSpec; 6] = [Register, Typedef, Extern, Static, Auto, None];
        for i in 0..strs.len() {
            let e = v[i];
            if e as i32 & spec != 0 {
                buf.push_str(strs[i]);
                buf.push(' ');
            }
        }
    }
}

#[derive(Clone)]
pub struct DeclSpec {
    pub storage_class: i32,
    pub spec_qual_list: TySpecQualList,
}

impl DeclSpec {

    pub fn new() -> Self {
        DeclSpec {
            storage_class: 0,
            spec_qual_list: TySpecQualList::new(),
        }
    }

    pub fn get_type(&mut self) -> Ty {
       self.spec_qual_list.get_type()
    }

    pub fn storage_class_is(&self, s: StorageClassSpec) -> bool {
        s as i32 & self.storage_class != 0
    }

    pub fn add_storage_class_spec(mut self, s: StorageClassSpec) -> Self {
        self.storage_class |= s as i32;
        self
    }

    pub fn add_type_spec(mut self, spec: TySpec) -> Self {
        self.spec_qual_list = self.spec_qual_list.add_spec(spec);
        self
    }

    pub fn add_type_qual(mut self, qual: TyQual) -> Self {
        self.spec_qual_list = self.spec_qual_list.add_qual(qual);
        self
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        StorageClassSpec::pretty_print(self.storage_class, buf);
        if self.storage_class != 0 { buf.push(' '); }
        self.spec_qual_list.pretty_print(buf, si);
    }
}

pub struct Declaration {
    pub ty: Ty,
    pub storage_class: i32,
    pub declarators: Box<[InitDeclarator]>,
}

impl Declaration {
    pub fn from_spec(mut spec: DeclSpec, declarators: Box<[InitDeclarator]>) -> Self {
        Declaration {
            ty: spec.get_type(),
            storage_class: spec.storage_class,
            declarators,
        }
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        StorageClassSpec::pretty_print(self.storage_class, buf);
        self.ty.pretty_print(buf, si);
        if self.declarators.len() > 0 {
            buf.push(' ');
            for d in self.declarators.iter() {
                buf.push_str("\n        ");
                d.pretty_print(buf, si);
                buf.push(',');
            }
            buf.pop();
        }
    }
}

#[derive(Clone)]
pub struct NamedParam {
    pub id: IValue<String>
}

#[derive(Clone)]
pub struct TypedParam {
    pub storage_class_specs: i32,
    pub id: Option<IValue<String>>,
    pub ty: Ty,
}

#[derive(Clone)]
pub enum ParamListKind {
    Named(Box<[NamedParam]>),
    Typed(Box<[TypedParam]>),
}

impl ParamListKind {
    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        match self {
            ParamListKind::Named(x) => {
                for p in x.iter() {
                    p.pretty_print(buf, si);
                    buf.push_str(", ");
                }
                buf.pop(); buf.pop();
            },
            ParamListKind::Typed(x)  => {
                for p in x.iter() {
                    p.pretty_print(buf, si);
                    buf.push_str(", ");
                }
                buf.pop(); buf.pop();
            },
        }
    }
}


impl NamedParam {
    pub fn from_id(id: IValue<String>) -> Self {
        NamedParam { id }
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        buf.push_str(si.get(self.id));
    }
}

impl TypedParam {

    pub fn from_abs_decl(mut spec: DeclSpec, ad: AbsDecl) -> Self {
        let ty = ad.wrap_type(spec.get_type());
        let id = None;
        TypedParam { ty, id, storage_class_specs: spec.storage_class, }
    }

    pub fn from_decl(mut spec: DeclSpec, decl: Declarator) -> Self {
        let ty = decl.wrap_type(spec.get_type());
        let id = Some(decl.get_id());
        TypedParam { ty, id, storage_class_specs: spec.storage_class, }
    }

    pub fn from_spec(mut spec: DeclSpec) -> Self {
        let ty = spec.get_type();
        let id = None;
        TypedParam { ty, id, storage_class_specs: spec.storage_class, }
    }

    pub fn get_type(&self) -> Ty {
        self.ty.clone()
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        StorageClassSpec::pretty_print(self.storage_class_specs, buf);
        self.ty.pretty_print(buf, si);
        if let Some(id) = self.id {
            buf.push(' ');
            buf.push_str(si.get(id));
        }
    }
}

#[derive(Clone)]
pub struct ParamList {
    pub kind: ParamListKind,
    pub varargs: bool,
}

impl ParamList {
    pub fn new(kind: ParamListKind, varargs: bool) -> Self {
        ParamList { kind, varargs, }
    }

    pub fn empty() -> Self {
        ParamList { kind: ParamListKind::Typed(Box::new([])), varargs: false, }
    }

    pub fn get_types(&self) -> Box<[Ty]> {
        match &self.kind {
            ParamListKind::Named(x) =>
                x.iter()
                .map(|_| Ty::new(TyKind::TBD, false, false))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            ParamListKind::Typed(x) => 
                x.iter()
                .map(|x| x.ty.clone())
                .collect::<Vec<_>>()
                .into_boxed_slice()
        }
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        self.kind.pretty_print(buf, si);
        if self.varargs {
            buf.push_str("...");
        }
    }
}
