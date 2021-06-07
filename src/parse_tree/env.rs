use crate::parse_tree::*;
use crate::util::EnvStack;

use std::rc::Rc;
use std::hash::Hash;
use std::collections::HashMap;

pub struct Env {
    /// String interner, used for IDs
    pub si: Interner<String>,

    /// Byte-string interner. Since c strings are just byte strings, they are
    /// represented as boxed byte slices.
    pub bi: Interner<Box<[u8]>>,

    /// Value stack - maps values in scope to their type.
    pub vs: EnvStack<IValue<String>, Ty>,

    /// Declaration stack - maps declarations in scope to their type.
    pub decs: EnvStack<IValue<String>, Declaration>,
    
    /// Datatype stack - maps datatypes in scope to their type.
    pub dts: EnvStack<IValue<String>, Structure>,
}

impl Env {
    pub fn new(si: Interner<String>, bi: Interner<Box<[u8]>>) -> Self {
        Env { si, bi, vs: EnvStack::new(), decs: EnvStack::new(), dts: EnvStack::new(), }
    }
}