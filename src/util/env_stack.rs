use std::rc::Rc;
use std::hash::Hash;
use std::collections::HashMap;
use std::borrow::Borrow;

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

    pub fn get<Q>(&self, key: &Q) -> Option<Rc<V>> 
    where
        K: Borrow<Q>,
        Q: ToOwned<Owned=K> + Hash + Eq + ?Sized {
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
    pub fn insert(&mut self, key: K, value: V) -> Option<Rc<V>> {
        let v = Rc::new(value);
        let idx = self.stack.len() - 1;
        self.stack[idx].insert(key, v)
    }
}