use crate::parse_tree::*;

pub enum TypeCheckError {
    BadFunctionDeclaration,
    FunctionArgRedeclaration,
}

pub trait TypeCheck {
    type Ast;
    fn type_check(&self, env: &mut Env) -> Result<Self::Ast, TypeCheckError>;
}

impl TypeCheck for Function {
    type Ast = ast::Function;

    fn type_check(&self, env: &mut Env) -> Result<Self::Ast, TypeCheckError> {
        // Check that decl is of type Fn
        match &self.decl.kind {
            DeclaratorKind::Direct(DirDecl::Fn(name, param_list)) => {
                panic!()
            },
            _ => Err(TypeCheckError::BadFunctionDeclaration),
        }

        // If that succeeds, check variable types

        // Check that the type_specs match the variable declarations in the 
        // declarator
    }
}