use crate::parse_tree::*;
use std::rc::Rc;

pub struct Env {
    pub si: Interner<String>,
    pub bi: Interner<Box<[u8]>>,
    pub vi: EnvStack<IValue<String>, Ty>,
}

impl Env {
    pub fn new(si: Interner<String>, bi: Interner<String>) -> Self {
        Env { si, bi, vi: EnvStack::new(), }
    }
}

pub struct EnvStack<K: Hash + Eq, V> {
    stack: Vec<HashMap<K, Rc<V>>>,
}

impl<K, V> EnvStack<K, V>
    where
        K: Hash + Eq, {
    pub fn new() -> Self {
        EnvStack { stack: Vec::with_capacity(32), }
    }

    pub fn push_frame(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop_frame(&mut self) {
        self.stack.pop();
    }

    pub fn get(&self, key: &K) -> Option<Rc<V>> {
        for i in (0..self.stack.len()).rev() {
            if let Some(v) = self.stack[i].get(key) {
                return Some(v.clone())
            }
        }
        None
    }

    /// Tries to insert the given key value pair into the current stack frame.
    /// If there is already a value for the supplied key it will be replaced
    /// with the supplied value. 
    pub fn insert(&self, key: K, value: V) -> Option<Rc<V>> {
        let v = Rc::new(value);
        self.stack[self.stack.len() - 1].insert(key, v)
    }
}