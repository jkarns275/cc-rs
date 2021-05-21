use std::collections::BTreeMap;
use std::marker::PhantomData;
use std::borrow::Borrow;

pub type IValue<T> = (PhantomData<T>, usize);
pub struct Interner<T: Ord + Clone> {
    count: usize,
    i_to_t: BTreeMap<usize, T>,
    t_to_i: BTreeMap<T, usize>,
}

impl<T: Ord + Clone> Interner<T> {

    pub fn new() -> Self {
        Interner {
            count: 0,
            i_to_t: BTreeMap::new(),
            t_to_i: BTreeMap::new(),
        }
    }

    pub fn intern<Q>(&mut self, s: &Q) -> IValue<T>
    where
        T: Borrow<Q>,
        Q: ToOwned<Owned=T> + Ord + ?Sized
    {
        if let Some(i) = self.t_to_i.get(s) {
            (PhantomData, *i)
        } else {
            let i = self.count;
            self.count += 1;
            self.t_to_i.insert(s.to_owned(), i);
            self.i_to_t.insert(i, s.to_owned());
            (PhantomData, i)
        }
    }

    pub fn get(&self, i: IValue<T>) -> &T {
        self.i_to_t.get(&i.1).unwrap()
    }

    pub fn get_clone(&self, i: IValue<T>) -> T {
        self.i_to_t.get(&i.1).unwrap().clone()
    }
}
