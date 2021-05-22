use ty::*;

pub enum Declarator {
    PtrTo(PtrTy, DirDecl),
    Direct(DirDecl),
}

impl Declarator {

    pub fn wrap_type(self, ty: Ty) -> Ty {
        use Declarator::*;

        match self {
            PtrTo(ptr_ty, dir_decl) =>
                dir_abs_decl.wrap_type(ptr_ty.wrap_type(ty)),
            Direct(dir_decl) =>
                dir_abs_decl.wrap_type(ty),
        }
    }

}

pub enum DirDeclKind {
    Id(IValue<String>),
    /// Function with arguments, the return type is not specified here
    Fn(Box<DirDecl>, Box<[Ty]>),
    Array(Option<Box<DirDecl>>, Option<TaggedExpr>),
    Declarator(Box<Declarator>),
}

impl DirDeclKind {

    fn box_size(size: Option<TaggedExpr>) -> Option<Box<TaggedExpr>> {
        size.map(|x| Box::new(x))
    }

    pub fn wrap_type(self, ty: Ty) -> Ty {
        use DirDecl::*;
        match self {
            Fn(abs_decl, tys) => {
                abs_decl.wrap_type(Ty::new(TyKind::Fn(Box::new(ty), tys), false, false))
            },
            Array(None, size) => {
                Ty::new(TyKind::Array(Box::new(ty), Self::box_size(size)), false, false)
            },
            Array(Some(pre), size) => {
                let t = Ty::new(TyKind::Array(Box::new(ty), Self::box_size(size)), false, false);
                pre.wrap_type(t)
            },
            AbsDecl(decl) => {
                decl.wrap_type(ty)
            }
        }
    }

}
