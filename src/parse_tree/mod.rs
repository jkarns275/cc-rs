mod ty;
mod expr;
mod abs_decl;
mod function;
mod data_type;
mod statement;
mod declarator;
mod declaration;

// Reimport for convenience
pub use crate::interner::*;

pub use declaration::*;
pub use declarator::*;
pub use statement::*;
pub use data_type::*;
pub use function::*;
pub use abs_decl::*;
pub use expr::*;
pub use ty::*;


pub enum TranslationUnit {
    Fn(Function),
    Decl(Declaration),
}

