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

impl PartialEq for InternedString {
    fn eq(&self, other: &InternedString) -> bool {
        self.as_slice() == other.as_slice()
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
    attributes: Vec<*mut Attribute>,
}

impl Element {
    pub fn name(&self) -> &str { self.name.as_slice() }
}

pub struct Attribute {
    name: InternedString,
    value: InternedString,
    parent: Option<*mut Element>,
}

impl Attribute {
    pub fn name(&self)  -> &str { self.name.as_slice() }
    pub fn value(&self) -> &str { self.value.as_slice() }
}

pub struct Text {
    text: InternedString,
    parent: Option<*mut Element>,
}

impl Text {
    pub fn text(&self) -> &str { self.text.as_slice() }
}

#[allow(raw_pointer_deriving)]
#[deriving(PartialEq)]
pub enum ChildOfElement {
    ElementCOE(*mut Element),
    TextCOE(*mut Text),
}

impl ChildOfElement {
    fn replace_parent(&self, parent: *mut Element) {
        match self {
            &ElementCOE(n) => {
                let n = unsafe { &mut *n };

                if let Some(prev_parent) = n.parent {
                    match prev_parent {
                        ElementPOC(e) => {
                            let e_r = unsafe { &mut *e };
                            e_r.children.retain(|n| n != self);
                        },
                    }
                }

                n.parent = Some(ElementPOC(parent));
            },
            &TextCOE(n) => {
                let n = unsafe { &mut *n };

                if let Some(prev_parent) = n.parent {
                    let prev_parent_r = unsafe { &mut *prev_parent };
                    prev_parent_r.children.retain(|n| n != self);
                }

                n.parent = Some(parent);
            },
        };
    }
}

pub enum ParentOfChild {
    ElementPOC(*mut Element),
}

pub struct Storage {
    strings: StringPool,
    elements: TypedArena<Element>,
    attributes: TypedArena<Attribute>,
    texts: TypedArena<Text>,
}


macro_rules! conversion_trait(
    ($tr_name:ident, $method:ident, $res_type:ident,
        { $($leaf_type:ident => $variant:ident),* }
    ) => (
        pub trait $tr_name {
            fn $method(self) -> $res_type;
        }

        impl $tr_name for $res_type {
            fn $method(self) -> $res_type {
                self
            }
        }

        $(impl $tr_name for *mut $leaf_type {
            fn $method(self) -> $res_type {
                $variant(self)
            }
        })*
    )
)

conversion_trait!(ToChildOfElement, to_child_of_element, ChildOfElement, {
    Element => ElementCOE,
    Text => TextCOE
})

impl Storage {
    pub fn new() -> Storage {
        Storage {
            strings: StringPool::new(),
            elements: TypedArena::new(),
            attributes: TypedArena::new(),
            texts: TypedArena::new(),
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
            attributes: Vec::new(),
        })
    }

    pub fn create_attribute(&self, name: &str, value: &str) -> *mut Attribute {
        let name = self.intern(name);
        let value = self.intern(value);

        self.attributes.alloc(Attribute {
            name: name,
            value: value,
            parent: None,
        })
    }

    pub fn create_text(&self, text: &str) -> *mut Text {
        let text = self.intern(text);

        self.texts.alloc(Text {
            text: text,
            parent: None,
        })
    }

    pub fn element_set_name(&self, element: *mut Element, name: &str) {
        let name = self.intern(name);
        let element_r = unsafe { &mut * element };
        element_r.name = name;
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

    pub fn append_element_child<C : ToChildOfElement>(&self, parent: *mut Element, child: C) {
        let child = child.to_child_of_element();
        let parent_r = unsafe { &mut *parent };

        child.replace_parent(parent);
        parent_r.children.push(child);
    }

    pub unsafe fn element_children(&self, parent: *mut Element) -> &[ChildOfElement] {
        let parent_r = &*parent;
        parent_r.children.as_slice()
    }

    pub fn attribute_parent(&self, attribute: *mut Attribute) -> Option<*mut Element> {
        let attr_r = unsafe { &*attribute };
        attr_r.parent
    }

    pub unsafe fn attributes(&self, parent: *mut Element) -> &[*mut Attribute] {
        let parent_r = &*parent;
        parent_r.attributes.as_slice()
    }

    pub fn attribute(&self, element: *mut Element, name: &str) -> Option<*mut Attribute> {
        let element_r = unsafe { &*element };
        element_r.attributes.iter().find(|a| {
            let a_r: &Attribute = unsafe { &***a };
            a_r.name.as_slice() == name
        }).map(|a| *a)
    }

    pub fn set_attribute(&self, parent: *mut Element, attribute: *mut Attribute) {
        let parent_r = unsafe { &mut *parent };
        let attr_r = unsafe { &mut *attribute };

        parent_r.attributes.retain(|a| {
            let a_r: &Attribute = unsafe { &**a };
            a_r.name != attr_r.name
        });
        parent_r.attributes.push(attribute);
        attr_r.parent = Some(parent);
    }
}
