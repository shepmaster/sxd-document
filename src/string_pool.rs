/// Strongly borrowed from TypedArena
/// Differs in growth strategy (non-doubling)
/// And only for variable-sized strings

use std::cell::{Cell,RefCell};
use std::cmp::max;
use std::collections::DList;
use std::collections::hash_map::{HashMap,Occupied,Vacant};
use std::fmt;
use std::hash;
use std::mem;
use std::ptr;
use std::raw::Slice;
use std::rt::heap::{allocate, deallocate};
use std::str;
use xxhash::{XXHasher};

struct Chunk {
    start: *mut u8,
    capacity: uint,
}

impl Chunk {
    fn new(capacity: uint) -> Chunk {
        Chunk {
            start: unsafe { allocate(capacity, mem::min_align_of::<u8>()) },
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
        unsafe { self.start().offset(self.capacity as int) }
    }
}

impl Drop for Chunk {
    fn drop(&mut self) {
        unsafe {
            deallocate(self.start, self.capacity, mem::min_align_of::<u8>());
        }
    }
}

pub struct InternedString {
    slice: Slice<u8>,
}

impl InternedString {
    fn from_parts(data: *const u8, len: uint) -> InternedString {
        InternedString {
            slice: Slice {
                data: data,
                len: len,
            },
        }
    }

    pub fn from_str(s: &str) -> InternedString {
        let bytes: &[u8] = s.as_bytes();
        let slice: Slice<u8> = unsafe { mem::transmute(bytes) };
        InternedString {
            slice: slice,
        }
    }

    pub fn as_slice<'s>(&self) -> &'s str {
        unsafe {
            let bytes: &[u8] = mem::transmute(self.slice);
            str::raw::from_utf8(bytes)
        }
    }
}

impl fmt::Show for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_slice().fmt(f)
    }
}

impl PartialEq for InternedString {
    fn eq(&self, other: &InternedString) -> bool {
        self.as_slice().eq(other.as_slice())
    }
}

impl Eq for InternedString {}

impl<S: hash::Writer> hash::Hash<S> for InternedString {
    fn hash(&self, state: &mut S) {
        self.as_slice().hash(state)
    }
}

pub struct StringPool {
    start: Cell<*mut u8>,
    end: Cell<*const u8>,
    chunks: RefCell<DList<Chunk>>,
    index: RefCell<HashMap<InternedString, InternedString, XXHasher>>,
}

static CAPACITY: uint = 10240;

impl StringPool {
    pub fn new() -> StringPool {
        StringPool{
            start: Cell::new(ptr::null_mut()),
            end: Cell::new(ptr::null()),
            chunks: RefCell::new(DList::new()),
            index: RefCell::new(HashMap::with_hasher(XXHasher::new())),
        }
    }

    pub fn intern<'s>(&'s self, s: &str) -> &'s str {
        if s == "" { return ""; }

        let search_string = InternedString::from_str(s);

        let interned_str = match self.index.borrow_mut().entry(search_string) {
            Occupied(entry) => *entry.get(),
            Vacant(entry) => *entry.set(self.do_intern(s)),
        };

        // The lifetime is really matched to us
        unsafe { mem::transmute(interned_str.as_slice()) }
    }

    fn do_intern(&self, s: &str) -> InternedString {
        self.ensure_capacity(s.len());
        self.store(s)
    }

    fn ensure_capacity(&self, str_len: uint) {
        let remaining = self.end.get() as uint - self.start.get() as uint;
        if remaining < str_len {
            self.allocate_another(max(CAPACITY, str_len))
        }
    }

    fn allocate_another(&self, capacity: uint) {
        let chunk = Chunk::new(capacity);
        self.start.set(chunk.start() as *mut u8);
        self.end.set(chunk.end());
        self.chunks.borrow_mut().push_front(chunk);
    }

    fn store(&self, s: &str) -> InternedString {
        let str_start = s.as_bytes().as_ptr();
        let str_len = s.len();

        unsafe {
            ptr::copy_nonoverlapping_memory(self.start.get(), str_start, str_len);

            // Rebuild the string from our buffer
            let interned_str = InternedString::from_parts(self.start.get() as *const u8, str_len);

            // Increase current pointer
            self.start.set(self.start.get().offset(str_len as int));

            interned_str
        }
    }
}

#[cfg(test)]
mod test {
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

    // #[test]
    // #[compile_failure]
    // fn string_cannot_outlive_the_pool() {
    //     let z = {
    //         let s = StringPool::new();
    //         s.intern("hello")
    //     };
    // }

    #[test]
    fn ignores_the_lifetime_of_the_input_string() {
        let s = StringPool::new();

        let interned = {
            let allocated_string = String::from_str("green");
            s.intern(allocated_string.as_slice())
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

#[cfg(test)]
mod bench {
    use test::Bencher;

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
            range(0u, 1000)
            .map(|i| format!("str{}str", i))
            .collect();
        b.iter(|| {
            for ss in strings.iter() {
                s.intern(ss.as_slice());
            }
        });
        b.bytes = strings.iter().fold(0, |a, s| a + s.len()) as u64;
    }

    #[bench]
    fn many_repeated_string(b: &mut Bencher) {
        let s = StringPool::new();

        let strings: Vec<String> =
            range(0u, 1000)
            .map(|i| format!("str{}str", i % 100))
            .collect();
        b.iter(|| {
            for ss in strings.iter() {
                s.intern(ss.as_slice());
            }
        });
        b.bytes = strings.iter().fold(0, |a, s| a + s.len()) as u64;
    }
}
