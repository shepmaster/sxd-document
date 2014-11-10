use arena::TypedArena;
use std::cell::RefCell;
use std::raw::Slice;
use std::mem;
use std::str;

struct InternedString {
    slice: Slice<u8>,
}

impl InternedString {
    fn from_str(s: &str) -> InternedString {
        InternedString {
            slice: unsafe { mem::transmute(s.as_bytes()) },
        }
    }

    fn as_slice(&self) -> &str {
        unsafe {
            let bytes = mem::transmute(self.slice);
            str::raw::from_utf8(bytes)
        }
    }
}

struct StringPool {
    strings: RefCell<Vec<String>>,
}

impl StringPool {
    pub fn new() -> StringPool {
        StringPool {
            strings: RefCell::new(Vec::new()),
        }
    }

    pub fn intern(&self, s: &str) -> &str {
        let mut strings = self.strings.borrow_mut();
        strings.push(String::from_str(s));
        let interned = strings.last().unwrap();

        // We will never remove a string, so the lifetime is that of
        // this object.
        unsafe { mem::transmute(interned.as_slice()) }
    }
}

pub struct Element {
    name: InternedString,
    children: Vec<ChildOfElement>,
    parent: Option<ParentOfChild>,
}

impl Element {
    pub fn name(&self) -> &str { self.name.as_slice() }
}

pub enum ChildOfElement {
    ElementCOE(*mut Element),
}

pub enum ParentOfChild {
    ElementPOC(*mut Element),
}

pub struct Storage {
    strings: StringPool,
    elements: TypedArena<Element>,
}

impl Storage {
    pub fn new() -> Storage {
        Storage {
            strings: StringPool::new(),
            elements: TypedArena::new(),
        }
    }

    fn intern(&self, s: &str) -> InternedString {
        let interned = self.strings.intern(s);
        InternedString::from_str(interned)
    }

    pub fn create_element(&self, name: &str) -> *mut Element {
        let name = self.intern(name);

        self.elements.alloc(Element {
            name: name,
            children: Vec::new(),
            parent: None,
        })
    }
}

pub struct Connections;

impl Connections {
    pub fn new() -> Connections {
        Connections
    }

    pub fn element_parent(&self, child: *mut Element) -> Option<ParentOfChild> {
        let child_r = unsafe { &*child };
        child_r.parent
    }

    pub fn append_element_child(&self, parent: *mut Element, child: *mut Element) {
        let parent_r = unsafe { &mut *parent };
        let child_r = unsafe { &mut *child };

        // Cleanup previous parentage
        child_r.parent = Some(ElementPOC(parent));

        parent_r.children.push(ElementCOE(child));
    }

    pub unsafe fn element_children(&self, parent: *mut Element) -> &[ChildOfElement] {
        let parent_r = &*parent;
        parent_r.children.as_slice()
    }
}
