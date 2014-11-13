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

pub struct Root {
    children: Vec<ChildOfRoot>,
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

pub struct Comment {
    text: InternedString,
    parent: Option<ParentOfChild>,
}

impl Comment {
    pub fn text(&self) -> &str { self.text.as_slice() }
}

pub struct ProcessingInstruction {
    target: InternedString,
    value: Option<InternedString>,
    parent: Option<ParentOfChild>,
}

impl ProcessingInstruction {
    pub fn target(&self) -> &str { self.target.as_slice() }
    pub fn value(&self) -> Option<&str> { self.value.as_ref().map(|v| v.as_slice()) }
}

#[allow(raw_pointer_deriving)]
#[deriving(PartialEq)]
pub enum ChildOfRoot {
    ElementCOR(*mut Element),
    CommentCOR(*mut Comment),
    ProcessingInstructionCOR(*mut ProcessingInstruction),
}

impl ChildOfRoot {
    fn is_element(&self) -> bool {
        match self {
            &ElementCOR(_) => true,
            _ => false,
        }
    }

    fn to_child_of_element(self) -> ChildOfElement {
        match self {
            ElementCOR(n) => ElementCOE(n),
            CommentCOR(n) => CommentCOE(n),
            ProcessingInstructionCOR(n) => ProcessingInstructionCOE(n),
        }
    }

    fn replace_parent(&self, parent: *mut Root) {
        match self {
            &ElementCOR(n) => {
                let parent_r = unsafe { &mut *parent };
                let n = unsafe { &mut *n };
                parent_r.children.retain(|c| !c.is_element());
                replace_parent(*self, RootPOC(parent), &mut n.parent);
            },
            &CommentCOR(n) => {
                let n = unsafe { &mut *n };
                replace_parent(*self, RootPOC(parent), &mut n.parent);
            },
            &ProcessingInstructionCOR(n) => {
                let n = unsafe { &mut *n };
                replace_parent(*self, RootPOC(parent), &mut n.parent);
            },
        };
    }
}

#[allow(raw_pointer_deriving)]
#[deriving(PartialEq)]
pub enum ChildOfElement {
    ElementCOE(*mut Element),
    TextCOE(*mut Text),
    CommentCOE(*mut Comment),
    ProcessingInstructionCOE(*mut ProcessingInstruction),
}

fn replace_parent(child: ChildOfRoot, parent: ParentOfChild, parent_field: &mut Option<ParentOfChild>) {
    if let &Some(prev_parent) = parent_field {
        match prev_parent {
            RootPOC(r) => {
                let r_r = unsafe { &mut *r };
                r_r.children.retain(|n| *n != child);
            },
            ElementPOC(e) => {
                let e_r = unsafe { &mut *e };
                let as_element_child = child.to_child_of_element();
                e_r.children.retain(|n| *n != as_element_child);
            },
        }
    }

    *parent_field = Some(parent);
}


impl ChildOfElement {
    fn replace_parent(&self, parent: *mut Element) {
        match self {
            &ElementCOE(n) => {
                let n = unsafe { &mut *n };
                replace_parent(ElementCOR(n), ElementPOC(parent), &mut n.parent);
            },
            &CommentCOE(n) => {
                let n = unsafe { &mut *n };
                replace_parent(CommentCOR(n), ElementPOC(parent), &mut n.parent);
            }
            &ProcessingInstructionCOE(n) => {
                let n = unsafe { &mut *n };
                replace_parent(ProcessingInstructionCOR(n), ElementPOC(parent), &mut n.parent);
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
    RootPOC(*mut Root),
    ElementPOC(*mut Element),
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

conversion_trait!(ToChildOfRoot, to_child_of_root, ChildOfRoot, {
    Element => ElementCOR
})

pub struct Storage {
    strings: StringPool,
    roots: TypedArena<Root>,
    elements: TypedArena<Element>,
    attributes: TypedArena<Attribute>,
    texts: TypedArena<Text>,
    comments: TypedArena<Comment>,
    processing_instructions: TypedArena<ProcessingInstruction>,
}

impl Storage {
    pub fn new() -> Storage {
        Storage {
            strings: StringPool::new(),
            roots: TypedArena::new(),
            elements: TypedArena::new(),
            attributes: TypedArena::new(),
            texts: TypedArena::new(),
            comments: TypedArena::new(),
            processing_instructions: TypedArena::new(),
        }
    }

    fn intern(&self, s: &str) -> InternedString {
        let interned = self.strings.intern(s);
        InternedString::from_str(interned)
    }

    pub fn create_root(&self) -> *mut Root {
        self.roots.alloc(Root {
            children: Vec::new(),
        })
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

    pub fn create_comment(&self, text: &str) -> *mut Comment {
        let text = self.intern(text);

        self.comments.alloc(Comment {
            text: text,
            parent: None,
        })
    }

    pub fn create_processing_instruction(&self, target: &str, value: Option<&str>)
                                         -> *mut ProcessingInstruction {
        let target = self.intern(target);
        let value = value.map(|v| self.intern(v));

        self.processing_instructions.alloc(ProcessingInstruction {
            target: target,
            value: value,
            parent: None,
        })
    }

    pub fn element_set_name(&self, element: *mut Element, name: &str) {
        let name = self.intern(name);
        let element_r = unsafe { &mut * element };
        element_r.name = name;
    }

    pub fn text_set_text(&self, text: *mut Text, new_text: &str) {
        let new_text = self.intern(new_text);
        let text_r = unsafe { &mut * text };
        text_r.text = new_text;
    }

    pub fn comment_set_text(&self, comment: *mut Comment, new_text: &str) {
        let new_text = self.intern(new_text);
        let comment_r = unsafe { &mut * comment };
        comment_r.text = new_text;
    }

    pub fn processing_instruction_set_target(&self, pi: *mut ProcessingInstruction, new_target: &str) {
        let new_target = self.intern(new_target);
        let pi_r = unsafe { &mut * pi };
        pi_r.target = new_target;
    }

    pub fn processing_instruction_set_value(&self, pi: *mut ProcessingInstruction, new_value: Option<&str>) {
        let new_value = new_value.map(|v| self.intern(v));
        let pi_r = unsafe { &mut * pi };
        pi_r.value = new_value;
    }
}

pub struct Connections {
    root: *mut Root,
}

impl Connections {
    pub fn new(root: *mut Root) -> Connections {
        Connections {
            root: root,
        }
    }

    pub fn root(&self) -> *mut Root {
        self.root
    }

    pub fn element_parent(&self, child: *mut Element) -> Option<ParentOfChild> {
        let child_r = unsafe { &*child };
        child_r.parent
    }

    pub fn text_parent(&self, child: *mut Text) -> Option<*mut Element> {
        let child_r = unsafe { &*child };
        child_r.parent
    }

    pub fn comment_parent(&self, child: *mut Comment) -> Option<ParentOfChild> {
        let child_r = unsafe { &*child };
        child_r.parent
    }

    pub fn processing_instruction_parent(&self, child: *mut ProcessingInstruction) -> Option<ParentOfChild> {
        let child_r = unsafe { &*child };
        child_r.parent
    }

    pub fn append_root_child<C : ToChildOfRoot>(&self, child: C) {
        let child = child.to_child_of_root();
        let parent_r = unsafe { &mut *self.root };

        child.replace_parent(self.root);
        parent_r.children.push(child);
    }

    pub fn append_element_child<C : ToChildOfElement>(&self, parent: *mut Element, child: C) {
        let child = child.to_child_of_element();
        let parent_r = unsafe { &mut *parent };

        child.replace_parent(parent);
        parent_r.children.push(child);
    }

    pub unsafe fn root_children(&self) -> &[ChildOfRoot] {
        let parent_r = &*self.root;
        parent_r.children.as_slice()
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
