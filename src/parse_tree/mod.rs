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

impl TranslationUnit {
    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        match self {
            TranslationUnit::Fn(f) => f.pretty_print(buf, si),
            TranslationUnit::Decl(d) => d.pretty_print(buf, si),
        }
    }
}