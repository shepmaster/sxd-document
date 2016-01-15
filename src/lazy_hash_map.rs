use std::collections::HashMap;
use std::collections::hash_map;
use std::hash::Hash;
use std::borrow::Borrow;

pub struct LazyHashMap<K, V> {
    map: Option<HashMap<K, V>>,
}

impl<K, V> LazyHashMap<K, V>
    where K: ::std::hash::Hash + Eq
{
    pub fn new() -> LazyHashMap<K, V> {
        LazyHashMap { map: None }
    }

    pub fn contains_key<Q: ?Sized>(&self, key: &Q) -> bool
        where K: Borrow<Q>,
              Q: Hash + Eq
    {
        self.map.as_ref().map_or(false, |m| m.contains_key(key))
    }

    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
        where K: Borrow<Q>,
              Q: Hash + Eq
    {
        self.map.as_ref().and_then(|m| m.get(key))
    }

    pub fn insert(&mut self, key: K, val: V) -> Option<V> {
        self.map = match self.map.take() {
            Some(m) => Some(m),
            None => Some(HashMap::new()),
        };

        self.map.as_mut().and_then(|m| {
            m.insert(key, val)
        })
    }

    pub fn iter(&self) -> Iter<K, V> {
        Iter(self.map.as_ref().map(|m| m.iter()))
    }
}

pub struct Iter<'a, K: 'a, V: 'a>(Option<hash_map::Iter<'a, K, V>>);

impl<'a, K: 'a, V: 'a> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.as_mut().and_then(|i| i.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.as_ref().map_or((0, Some(0)), |i| i.size_hint())
    }
}
