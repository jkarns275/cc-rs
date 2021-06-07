use crate::parse_tree::*;

#[derive(Clone)]
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
        match &self.kind {
            DeclaratorKind::PtrTo(ptr_ty, dir_decl) =>
                dir_decl.wrap_type(ptr_ty.wrap_type(ty)),
            DeclaratorKind::Direct(dir_decl) =>
                dir_decl.wrap_type(ty),
        }
    }

    pub fn get_id(&self) -> IValue<String> {
        self.id
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        self.kind.pretty_print(buf, si);
    }

}

#[derive(Clone)]
pub struct InitDeclarator {
    pub decl: Declarator,
    pub initializer: Option<Initializer>,
}

impl InitDeclarator {
    pub fn new(decl: Declarator, initializer: Option<Initializer>) -> Self {
        InitDeclarator { decl, initializer }
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        self.decl.pretty_print(buf, si);
        if let Some(i) = &self.initializer {
            buf.push_str(" = ");
            i.pretty_print(buf, si);
        }
    }
}

#[derive(Clone)]
pub enum Initializer {
    Expr(TaggedExpr),
    Structure(Box<[Initializer]>),
}

impl Initializer {
    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        match self {
            Initializer::Expr(e) => e.pretty_print(buf, si),
            Initializer::Structure(s) => {
                buf.push_str("{ ");
                for i in s.iter() {
                    i.pretty_print(buf, si);
                    buf.push_str(", ");
                }
                buf.pop(); buf.pop();
                buf.push_str(" }");
            },
        }
    }
}

#[derive(Clone)]
pub enum DeclaratorKind {
    PtrTo(PtrTy, DirDecl),
    Direct(DirDecl),
}

impl DeclaratorKind {

    pub fn wrap_type(&self, ty: Ty) -> Ty {
        use DeclaratorKind::*;

        match self {
            PtrTo(ptr_ty, dir_decl) =>
                dir_decl.wrap_type(ptr_ty.wrap_type(ty)),
            Direct(dir_decl) =>
                dir_decl.wrap_type(ty),
        }
    }

    pub fn get_id(&self) -> IValue<String> {
        use DeclaratorKind::*;
        match &self {
            PtrTo(ptr, dir_decl) => dir_decl.get_id(),
            Direct(dir_decl) => dir_decl.get_id(),
        }
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        match self {
            DeclaratorKind::PtrTo(ptr_ty, dir) => {
                ptr_ty.pretty_print(buf, si);
                dir.pretty_print(buf, si);
            },
            DeclaratorKind::Direct(dir) => {
                dir.pretty_print(buf, si);
            }
        }
    }

}

#[derive(Clone)]
pub enum DirDecl {
    Id(IValue<String>),
    /// Function with arguments, the return type is not specified here
    Fn(Box<DirDecl>, ParamList),
    Array(Box<DirDecl>, Option<TaggedExpr>),
    Declarator(Box<Declarator>),
}

impl DirDecl {

    pub fn get_id(&self) -> IValue<String> {
        use DirDecl::*;

        match &self {
            Id(x) => *x,
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
            Id(x) => ty,
            Fn(decl, tys) => {
                decl.wrap_type(Ty::new(TyKind::Fn(Box::new(ty), tys.get_types()), false, false))
            },
            Array(pre, size) => {
                let t = Ty::new(TyKind::Array(Box::new(ty), Self::box_size(size.clone())), false, false);
                pre.wrap_type(t)
            },
            Declarator(decl) => {
                decl.wrap_type(ty)
            }
        }
    }

    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        use DirDecl::*;
        match self {
            Id(id) => buf.push_str(si.get(*id)),
            Fn(decl, tys) => {
                decl.pretty_print(buf, si);
                buf.push('(');
                tys.pretty_print(buf, si);
                buf.push(')');
            },
            Array(pre, size) => {
                pre.pretty_print(buf, si);
                buf.push('[');
                if let Some(s) = size {
                    s.pretty_print(buf, si);
                }
                buf.push(']');
            },
            Declarator(decl) => {
                buf.push('(');
                decl.pretty_print(buf, si);
                buf.push(')');
            }
        }
    }
}