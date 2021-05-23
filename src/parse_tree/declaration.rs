use ty::*;
use declarator::*;
use abs_decl::*;
use interner::IValue;

#[derive(Copy, Clone)]
#[repr(i32)]
pub StorageClassSpec {
    Register  = 0b00001,
    Typedef   = 0b00010,
    Extern    = 0b00100,
    Static    = 0b01000,
    Auto      = 0b10000,
    None      = 0b00000,
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

}

pub struct Declaration {
    pub ty: Ty,
    pub declarators: Box<[InitDeclarator]>,
}

#[derive(Clone)]
pub struct ParamDeclaration {
    pub ty: Ty,
    pub id: Option<IValue<String>>
    pub storage_class_specs: i32,
}

impl ParamDeclaration {

    pub fn from_id(id: IValue<String>) -> Self {
        let ty = Ty::new(TyKind::Integral(IntegralTy::i32), false, false);
        ParamDeclaration { ty, id, storage_class_specs: 0, }
    }

    pub fn from_abs_decl(mut spec: DeclSpec, ad: AbsDecl) -> Self {
        let ty = ad.wrap_type(spec.get_type());
        let id = None;
        ParamDeclaration { ty, id, storage_class_specs: spec.storage_class, }
    }

    pub fn from_decl(mut spec: DeclSpec, decl: Declarator) -> Self {
        let ty = decl.wrap_type(spec.get_type());
        let id = Some(decl.get_id());
        ParamDeclaration { ty, id, storage_class_specs: spec.storage_class, }
    }

    pub fn from_spec(mut spec: DeclSpec) -> Self {
        let ty = spec.get_type();
        let id = None;
        ParamDeclaration { ty, id, storage_class_specs: spec.storage_class, }
    }

}

pub struct ParamList {
    pub params: Box<[ParamDeclaration]>,
    pub varargs: bool,
}

impl ParamList {
    pub fn new(params: Box<[ParamDeclaration]>, varargs: bool) -> Self {
        ParamList { params, varargs, }
    }
}
