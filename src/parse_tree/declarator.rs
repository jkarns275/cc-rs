use ty::*;

pub struct Declarator {
    pub id: IValue<String>,
    pub kind: DeclaratorKind,
}

impl Declarator {

    pub fn new(kind: DeclaratorKind) -> Self {
        let id = kind.get_id();
        Declarator { id, kind, }
    }

    pub fn wrap_type(&self, ty: Ty) -> Ty {
        match kind {
            DeclaratorKind::PtrTo(ptr_ty, dir_decl) =>
                dir_decl.wrap_type(ptr_ty.wrap_type(ty)),
            DeclaratorKind::Direct(dir_decl) =>
                dir_decl.wrap_type(ty),
        }
    }
}

pub struct InitDeclarator {
    pub decl: Declarator,
    pub initializer: Option<Initializer>,
}

pub enum Initializer {
    Expr(TaggedExpr),
    Structure(Box<[Initializer]>),
}

pub enum DeclaratorKind {
    PtrTo(PtrTy, DirDecl),
    Direct(DirDecl),
}

impl DeclaratorKind {

    pub fn wrap_type(&self, ty: Ty) -> Ty {
        use DeclaratorKind::*;

        match self {
            PtrTo(ptr_ty, dir_decl) =>
                dir_abs_decl.wrap_type(ptr_ty.wrap_type(ty)),
            Direct(dir_decl) =>
                dir_abs_decl.wrap_type(ty),
        }
    }

    pub fn get_id(&self) -> IValue<String> {
        match &self {
            PtrTo(ptr, dir_decl) => dir_decl.get_id(),
            Direct(dir_decl) => dir_decl.get_id(),
        }
    }

}

pub enum DirDecl {
    Id(IValue<String>),
    /// Function with arguments, the return type is not specified here
    Fn(Box<DirDecl>, ParamList),
    Array(Box<DirDecl>, Option<TaggedExpr>),
    Declarator(Box<Declarator>),
}

impl DirDecl {

    /// Create a function with the given parameter names that don't have a type specified.
    /// Since they don't have types just assign them int types.
    /// TODO: This should be a warning of some kind, since not specifying a type is sus.
    pub fn id_fn(decl: DirDecl, args: Box<[IValue<String>]>) -> Self {
        let args = args
            .into_iter()
            .map(ParamDeclaration::from_id)
            .collect::<Vec<_>>();
        let param_list = ParamList::new(args.into_boxed_slice(), false);
        DirDecl::Fn(Box::new(decl), param_list)
    }

    pub fn get_id(&self) -> Self {
        match &self {
            Id(x) => x,
            Fn(dir, _) => dir.get_id(),
            Array(dir, _) => dir.get_id(),
            Declarator(decl) => decl.get_id(),
        }
    }

    fn box_size(size: Option<TaggedExpr>) -> Option<Box<TaggedExpr>> {
        size.map(|x| Box::new(x))
    }

    pub fn wrap_type(&self, ty: Ty) -> Ty {
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
            Declarator(decl) => {
                decl.wrap_type(ty)
            }
        }
    }

}