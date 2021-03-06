use crate::parse_tree::*;

#[derive(Clone)]
pub enum AbsDecl {
    Ptr(PtrTy),
    PtrTo(PtrTy, DirAbsDecl),
    Direct(DirAbsDecl),
}

impl AbsDecl {

    pub fn wrap_type(self, ty: Ty) -> Ty {
        use AbsDecl::*;

        match self {
            Ptr(ptr_ty) =>
                ptr_ty.wrap_type(ty),
            PtrTo(ptr_ty, dir_abs_decl) =>
                dir_abs_decl.wrap_type(ptr_ty.wrap_type(ty)),
            Direct(dir_abs_decl) =>
                dir_abs_decl.wrap_type(ty),
        }
    }

}

#[derive(Clone)]
pub enum DirAbsDecl {
    /// Function with arguments, the return type is not specified here
    Fn(Box<DirAbsDecl>, ParamList),
    Array(Option<Box<DirAbsDecl>>, Option<TaggedExpr>),
    AbsDecl(Box<AbsDecl>),
}

impl DirAbsDecl {

    fn box_size(size: Option<TaggedExpr>) -> Option<Box<TaggedExpr>> {
        size.map(|x| Box::new(x))
    }

    pub fn wrap_type(self, ty: Ty) -> Ty {
        use DirAbsDecl::*;
        match self {
            Fn(abs_decl, params) => {
                abs_decl.wrap_type(Ty::new(TyKind::Fn(Box::new(ty), params.get_types()), false, false))
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