/// Strongly borrowed from TypedArena
/// Differs in growth strategy (non-doubling)
/// And only for variable-sized strings

use std::borrow::Borrow;
use std::cell::{Cell,RefCell};
use std::cmp::max;
use std::collections::LinkedList;
use std::collections::hash_set::HashSet;
use std::default::Default;
use std::ops::Deref;
use std::slice;
use std::{fmt,hash,mem,ptr,str};

struct Chunk {
    start: *mut u8,
    capacity: usize,
}

impl Chunk {
    fn new(capacity: usize) -> Chunk {
        let mut slab: Vec<u8> = Vec::with_capacity(capacity);
        let start = slab.as_mut_ptr();

        // We will manually track the buffer and then drop it ourselves
        mem::forget(slab);

        Chunk {
            start: start,
            capacity: capacity,
        }
    }

    // Returns a pointer to the beginning of the allocated space.
    #[inline]
    fn start(&self) -> *const u8 {
        self.start as *const u8
    }

    // Returns a pointer to the end of the allocated space.
    #[inline]
    fn end(&self) -> *const u8 {
        unsafe { self.start().offset(self.capacity as isize) }
    }
}

impl Drop for Chunk {
    fn drop(&mut self) {
        // This is safe because we only ever store u8, which doesn't
        // have a destructor. This means the len doesn't matter, only
        // the capacity.
        unsafe {
            Vec::from_raw_parts(self.start, self.capacity, self.capacity);
        }
    }
}

#[derive(Copy,Clone)]
pub struct InternedString {
    data: *const u8,
    len: usize,
}

impl InternedString {
    fn from_parts(data: *const u8, len: usize) -> InternedString {
        InternedString {
            data: data,
            len: len,
        }
    }

    pub fn from_str(s: &str) -> InternedString {
        let bytes: &[u8] = s.as_bytes();
        InternedString {
            data: bytes.as_ptr(),
            len: bytes.len(),
        }
    }

    pub fn as_slice<'s>(&self) -> &'s str {
        unsafe {
            let bytes = slice::from_raw_parts(self.data, self.len);
            str::from_utf8_unchecked(bytes)
        }
    }
}

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_slice().fmt(f)
    }
}

impl PartialEq for InternedString {
    fn eq(&self, other: &InternedString) -> bool {
        self.as_slice().eq(other.as_slice())
    }
}

impl PartialEq<str> for InternedString {
    fn eq(&self, other: &str) -> bool {
        self.as_slice().eq(other)
    }
}

impl Eq for InternedString {}

impl hash::Hash for InternedString {
    fn hash<H>(&self, state: &mut H)
        where H: hash::Hasher
    {
        self.as_slice().hash(state)
    }
}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        self.as_slice()
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_slice()
    }
}

pub struct StringPool {
    start: Cell<*mut u8>,
    end: Cell<*const u8>,
    chunks: RefCell<LinkedList<Chunk>>,
    index: RefCell<HashSet<InternedString>>,
}

static CAPACITY: usize = 10240;

impl StringPool {
    pub fn new() -> StringPool {
        StringPool{
            start: Cell::new(ptr::null_mut()),
            end: Cell::new(ptr::null()),
            chunks: RefCell::new(LinkedList::new()),
            index: RefCell::new(Default::default()),
        }
    }

    pub fn intern<'s>(&'s self, s: &str) -> &'s str {
        if s == "" { return ""; }

        let mut index = self.index.borrow_mut();
        if let Some(interned) = index.get(s) {
            return unsafe { mem::transmute(interned as &str) };
        }

        let interned_str = self.do_intern(s);
        index.insert(interned_str);

        // The lifetime is really matched to us
        unsafe { mem::transmute(interned_str) }
    }

    fn do_intern(&self, s: &str) -> InternedString {
        self.ensure_capacity(s.len());
        self.store(s)
    }

    fn ensure_capacity(&self, str_len: usize) {
        let remaining = self.end.get() as usize - self.start.get() as usize;
        if remaining < str_len {
            self.allocate_another(max(CAPACITY, str_len))
        }
    }

    fn allocate_another(&self, capacity: usize) {
        let chunk = Chunk::new(capacity);
        self.start.set(chunk.start() as *mut u8);
        self.end.set(chunk.end());
        self.chunks.borrow_mut().push_front(chunk);
    }

    fn store(&self, s: &str) -> InternedString {
        let str_start = s.as_bytes().as_ptr();
        let str_len = s.len();

        unsafe {
            ptr::copy_nonoverlapping(str_start, self.start.get(), str_len);

            // Rebuild the string from our buffer
            let interned_str = InternedString::from_parts(self.start.get() as *const u8, str_len);

            // Increase current pointer
            self.start.set(self.start.get().offset(str_len as isize));

            interned_str
        }
    }
}

#[cfg(test)]
mod test {
    use std::borrow::ToOwned;

    use super::StringPool;

    #[test]
    fn keeps_the_same_string() {
        let s = StringPool::new();

        let interned = s.intern("hello");

        assert_eq!(interned, "hello");
    }

    #[test]
    fn does_not_reuse_the_pointer_of_the_input() {
        let s = StringPool::new();
        let input = "hello";

        let interned = s.intern(input);

        assert!(
            input.as_bytes().as_ptr() !=
            interned.as_bytes().as_ptr()
        );
    }

    #[test]
    fn reuses_the_pointer_for_repeated_input() {
        let s = StringPool::new();

        let interned1 = s.intern("world");
        let interned2 = s.intern("world");

        assert_eq!(
            interned1.as_bytes().as_ptr(),
            interned2.as_bytes().as_ptr()
        );
    }

    #[test]
    #[cfg(feature = "compile_failure")]
    fn string_cannot_outlive_the_pool() {
        let z = {
            let s = StringPool::new();
            s.intern("hello")
        };
    }

    #[test]
    fn ignores_the_lifetime_of_the_input_string() {
        let s = StringPool::new();

        let interned = {
            let allocated_string = "green".to_owned();
            s.intern(&allocated_string)
        };

        // allocated_string is gone now, but we should be able to
        // access the result value until the storage goes away.

        assert_eq!(interned, "green");
    }

    #[test]
    fn can_be_dropped_immediately() {
        StringPool::new();
    }

    fn return_populated_storage() -> (StringPool, *const u8) {
        let s = StringPool::new();
        let ptr = {
            let interned = s.intern("hello");
            interned.as_ptr()
        };
        (s, ptr)
    }

    #[test]
    fn can_return_storage_populated_with_values() {
        let (s, ptr_val) = return_populated_storage();
        let interned = s.intern("hello");
        assert_eq!(interned.as_ptr(), ptr_val);
    }
}

#[cfg(feature = "unstable")]
mod bench {
    extern crate test;

    use self::test::Bencher;

    use super::StringPool;

    #[bench]
    fn single_string(b: &mut Bencher) {
        let s = StringPool::new();
        b.iter(|| s.intern("hello"));
        b.bytes = "hello".len() as u64;
    }

    #[bench]
    fn many_unique_string(b: &mut Bencher) {
        let s = StringPool::new();

        let strings: Vec<String> =
            (0..1000)
            .map(|i| format!("str{}str", i))
            .collect();
        b.iter(|| {
            for ss in strings.iter() {
                s.intern(ss);
            }
        });
        b.bytes = strings.iter().fold(0, |a, s| a + s.len()) as u64;
    }

    #[bench]
    fn many_repeated_string(b: &mut Bencher) {
        let s = StringPool::new();

        let strings: Vec<String> =
            (0..1000)
            .map(|i| format!("str{}str", i % 100))
            .collect();
        b.iter(|| {
            for ss in strings.iter() {
                s.intern(ss);
            }
        });
        b.bytes = strings.iter().fold(0, |a, s| a + s.len()) as u64;
    }
}
