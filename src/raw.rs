use super::QName;
use super::lazy_hash_map::LazyHashMap;

use typed_arena::Arena;
use string_pool::{StringPool,InternedString};
use std::marker::PhantomData;
use std::slice;

struct InternedQName {
    namespace_uri: Option<InternedString>,
    local_part: InternedString,
}

impl InternedQName {
    fn as_qname(&self) -> QName {
        QName {
            namespace_uri: self.namespace_uri.map(|n| n.as_slice()),
            local_part: &self.local_part,
        }
    }
}

pub struct Root {
    children: Vec<ChildOfRoot>,
}

pub struct Element {
    name: InternedQName,
    default_namespace_uri: Option<InternedString>,
    preferred_prefix: Option<InternedString>,
    children: Vec<ChildOfElement>,
    parent: Option<ParentOfChild>,
    attributes: Vec<*mut Attribute>,
    prefix_to_namespace: LazyHashMap<InternedString, InternedString>,
}

impl Element {
    pub fn name(&self) -> QName {
        self.name.as_qname()
    }
    pub fn default_namespace_uri(&self) -> Option<&str> {
        self.default_namespace_uri.map(|p| p.as_slice())
    }
    pub fn preferred_prefix(&self) -> Option<&str> {
        self.preferred_prefix.map(|p| p.as_slice())
    }
}

pub struct Attribute {
    name: InternedQName,
    preferred_prefix: Option<InternedString>,
    value: InternedString,
    parent: Option<*mut Element>,
}

impl Attribute {
    pub fn name(&self)  -> QName { self.name.as_qname() }
    pub fn value(&self) -> &str { &self.value }
    pub fn preferred_prefix(&self) -> Option<&str> { self.preferred_prefix.map(|p| p.as_slice()) }
}

pub struct Text {
    text: InternedString,
    parent: Option<*mut Element>,
}

impl Text {
    pub fn text(&self) -> &str { &self.text }
}

pub struct Comment {
    text: InternedString,
    parent: Option<ParentOfChild>,
}

impl Comment {
    pub fn text(&self) -> &str { &self.text }
}

pub struct ProcessingInstruction {
    target: InternedString,
    value: Option<InternedString>,
    parent: Option<ParentOfChild>,
}

impl ProcessingInstruction {
    pub fn target(&self) -> &str { &self.target }
    pub fn value(&self) -> Option<&str> { self.value.map(|v| v.as_slice()) }
}

#[derive(Debug,Copy,Clone,PartialEq)]
pub enum ChildOfRoot {
    Element(*mut Element),
    Comment(*mut Comment),
    ProcessingInstruction(*mut ProcessingInstruction),
}

impl ChildOfRoot {
    fn is_element(&self) -> bool {
        match *self {
            ChildOfRoot::Element(_) => true,
            _ => false,
        }
    }

    fn replace_parent(&self, parent: *mut Root) {
        match *self {
            ChildOfRoot::Element(n) => {
                let parent_r = unsafe { &mut *parent };
                let n = unsafe { &mut *n };
                parent_r.children.retain(|c| !c.is_element());
                replace_parent(*self, ParentOfChild::Root(parent), &mut n.parent);
            },
            ChildOfRoot::Comment(n) => {
                let n = unsafe { &mut *n };
                replace_parent(*self, ParentOfChild::Root(parent), &mut n.parent);
            },
            ChildOfRoot::ProcessingInstruction(n) => {
                let n = unsafe { &mut *n };
                replace_parent(*self, ParentOfChild::Root(parent), &mut n.parent);
            },
        };
    }

    fn remove_parent(&self) {
        match *self {
            ChildOfRoot::Element(n) => {
                let n = unsafe { &mut *n };
                n.parent = None;
            },
            ChildOfRoot::Comment(n) => {
                let n = unsafe { &mut *n };
                n.parent = None;
            },
            ChildOfRoot::ProcessingInstruction(n) => {
                let n = unsafe { &mut *n };
                n.parent = None;
            },
        };
    }
}

#[derive(Debug,Copy,Clone,PartialEq)]
pub enum ChildOfElement {
    Element(*mut Element),
    Text(*mut Text),
    Comment(*mut Comment),
    ProcessingInstruction(*mut ProcessingInstruction),
}

fn replace_parent(child: ChildOfRoot, parent: ParentOfChild, parent_field: &mut Option<ParentOfChild>) {
    if let Some(prev_parent) = *parent_field {
        match prev_parent {
            ParentOfChild::Root(r) => {
                let r_r = unsafe { &mut *r };
                r_r.children.retain(|n| *n != child);
            },
            ParentOfChild::Element(e) => {
                let e_r = unsafe { &mut *e };
                let as_element_child = child.into();
                e_r.children.retain(|n| *n != as_element_child);
            },
        }
    }

    *parent_field = Some(parent);
}


impl ChildOfElement {
    fn replace_parent(&self, parent: *mut Element) {
        match *self {
            ChildOfElement::Element(n) => {
                let n = unsafe { &mut *n };
                replace_parent(ChildOfRoot::Element(n), ParentOfChild::Element(parent), &mut n.parent);
            },
            ChildOfElement::Comment(n) => {
                let n = unsafe { &mut *n };
                replace_parent(ChildOfRoot::Comment(n), ParentOfChild::Element(parent), &mut n.parent);
            }
            ChildOfElement::ProcessingInstruction(n) => {
                let n = unsafe { &mut *n };
                replace_parent(ChildOfRoot::ProcessingInstruction(n), ParentOfChild::Element(parent), &mut n.parent);
            },
            ChildOfElement::Text(n) => {
                let n = unsafe { &mut *n };

                if let Some(prev_parent) = n.parent {
                    let prev_parent_r = unsafe { &mut *prev_parent };
                    prev_parent_r.children.retain(|n| n != self);
                }

                n.parent = Some(parent);
            },
        };
    }

    fn remove_parent(&self) {
        match *self {
            ChildOfElement::Element(n) => {
                let n = unsafe { &mut *n };
                n.parent = None;
            },
            ChildOfElement::Comment(n) => {
                let n = unsafe { &mut *n };
                n.parent = None;
            },
            ChildOfElement::ProcessingInstruction(n) => {
                let n = unsafe { &mut *n };
                n.parent = None;
            },
            ChildOfElement::Text(n) => {
                let n = unsafe { &mut *n };
                n.parent = None;
            },
        };
    }
}

#[derive(Debug,Copy,Clone,PartialEq)]
pub enum ParentOfChild {
    Root(*mut Root),
    Element(*mut Element),
}

macro_rules! conversion_trait(
    ($res_type:ident, {
        $($leaf_type:ident => $variant:expr),*
    }) => (
        $(impl From<*mut $leaf_type> for $res_type {
            fn from(v: *mut $leaf_type) -> $res_type {
                $variant(v)
            }
        })*
    )
);

conversion_trait!(
    ChildOfElement, {
        Element               => ChildOfElement::Element,
        Text                  => ChildOfElement::Text,
        Comment               => ChildOfElement::Comment,
        ProcessingInstruction => ChildOfElement::ProcessingInstruction
    }
);

conversion_trait!(
    ChildOfRoot, {
        Element               => ChildOfRoot::Element,
        Comment               => ChildOfRoot::Comment,
        ProcessingInstruction => ChildOfRoot::ProcessingInstruction
    }
);

impl From<ChildOfRoot> for ChildOfElement {
    fn from(v: ChildOfRoot) -> ChildOfElement {
        match v {
            ChildOfRoot::Element(n)               => ChildOfElement::Element(n),
            ChildOfRoot::Comment(n)               => ChildOfElement::Comment(n),
            ChildOfRoot::ProcessingInstruction(n) => ChildOfElement::ProcessingInstruction(n),
        }
    }
}

pub struct Storage {
    strings: StringPool,
    roots: Arena<Root>,
    elements: Arena<Element>,
    attributes: Arena<Attribute>,
    texts: Arena<Text>,
    comments: Arena<Comment>,
    processing_instructions: Arena<ProcessingInstruction>,
}

impl Storage {
    pub fn new() -> Storage {
        Storage {
            strings: StringPool::new(),
            roots: Arena::new(),
            elements: Arena::new(),
            attributes: Arena::new(),
            texts: Arena::new(),
            comments: Arena::new(),
            processing_instructions: Arena::new(),
        }
    }

    fn intern(&self, s: &str) -> InternedString {
        let interned = self.strings.intern(s);
        InternedString::from_str(interned)
    }

    fn intern_qname(&self, q: QName) -> InternedQName {
        InternedQName {
            namespace_uri: q.namespace_uri.map(|p| self.intern(p)),
            local_part: self.intern(q.local_part),
        }
    }

    pub fn create_root(&self) -> *mut Root {
        self.roots.alloc(Root {
            children: Vec::new(),
        })
    }

    pub fn create_element<'n, N>(&self, name: N) -> *mut Element
        where N: Into<QName<'n>>
    {
        let name = name.into();
        let name = self.intern_qname(name);

        self.elements.alloc(Element {
            name: name,
            default_namespace_uri: None,
            preferred_prefix: None,
            children: Vec::new(),
            parent: None,
            attributes: Vec::new(),
            prefix_to_namespace: LazyHashMap::new(),
        })
    }

    pub fn create_attribute<'n, N>(&self, name: N, value: &str) -> *mut Attribute
        where N: Into<QName<'n>>
    {
        let name = name.into();
        let name = self.intern_qname(name);
        let value = self.intern(value);

        self.attributes.alloc(Attribute {
            name: name,
            preferred_prefix: None,
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

    pub fn element_set_name<'n, N>(&self, element: *mut Element, name: N)
        where N: Into<QName<'n>>
    {
        let name = name.into();
        let name = self.intern_qname(name);
        let element_r = unsafe { &mut * element };
        element_r.name = name;
    }

    pub fn element_register_prefix(&self, element: *mut Element, prefix: &str, namespace_uri: &str) {
        let prefix = self.intern(prefix);
        let namespace_uri = self.intern(namespace_uri);
        let element_r = unsafe { &mut * element };
        element_r.prefix_to_namespace.insert(prefix, namespace_uri);
    }

    pub fn element_set_default_namespace_uri(&self, element: *mut Element, namespace_uri: Option<&str>) {
        let namespace_uri = namespace_uri.map(|p| self.intern(p));
        let element_r = unsafe { &mut * element };
        element_r.default_namespace_uri = namespace_uri;
    }

    pub fn element_set_preferred_prefix(&self, element: *mut Element, prefix: Option<&str>) {
        let prefix = prefix.map(|p| self.intern(p));
        let element_r = unsafe { &mut * element };
        element_r.preferred_prefix = prefix;
    }

    pub fn attribute_set_preferred_prefix(&self, attribute: *mut Attribute, prefix: Option<&str>) {
        let prefix = prefix.map(|p| self.intern(p));
        let attribute_r = unsafe { &mut * attribute };
        attribute_r.preferred_prefix = prefix;
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

    pub fn append_root_child<C>(&self, child: C) where
        C: Into<ChildOfRoot>
    {
        let child = child.into();
        let parent_r = unsafe { &mut *self.root };

        child.replace_parent(self.root);
        parent_r.children.push(child);
    }

    pub fn append_element_child<C>(&self, parent: *mut Element, child: C)
        where C: Into<ChildOfElement>
    {
        let child = child.into();
        let parent_r = unsafe { &mut *parent };

        child.replace_parent(parent);
        parent_r.children.push(child);
    }

    pub fn remove_root_child<C>(&self, child: C)
        where C: Into<ChildOfRoot>
    {
        let parent_r = unsafe { &mut *self.root };
        let child = child.into();
        child.remove_parent();
        parent_r.children.retain(|&x| x != child);
    }

    pub fn remove_element_child<C>(&self, parent: *mut Element, child: C)
        where C: Into<ChildOfElement>
    {
        let parent_r = unsafe { &mut *parent };
        let child = child.into();
        child.remove_parent();
        parent_r.children.retain(|&x| x != child);
    }

    pub fn clear_root_children(&self) {
        let parent_r = unsafe { &mut *self.root };
        for c in &mut parent_r.children {
            c.remove_parent();
        }
        parent_r.children.clear();
    }

    pub fn clear_element_children(&self, parent: *mut Element) {
        let parent_r = unsafe { &mut *parent };
        for c in &mut parent_r.children {
            c.remove_parent();
        }
        parent_r.children.clear();
    }

    pub fn remove_element_from_parent(&self, child: *mut Element) {
        let child_r = unsafe { &mut *child };
        match child_r.parent {
            Some(ParentOfChild::Root(_)) => self.remove_root_child(child),
            Some(ParentOfChild::Element(parent)) => self.remove_element_child(parent, child),
            None => { /* no-op */ },
        }
    }

    pub fn remove_attribute_from_parent(&self, child: *mut Attribute) {
        let child_r = unsafe { &mut *child };
        if let Some(parent) = child_r.parent {
            self.remove_attribute_x(parent, |attr| attr as *mut Attribute == child);
        }
    }

    pub fn remove_text_from_parent(&self, child: *mut Text) {
        let child_r = unsafe { &mut *child };
        if let Some(parent) = child_r.parent {
            self.remove_element_child(parent, child);
        }
    }

    pub fn remove_comment_from_parent(&self, child: *mut Comment) {
        let child_r = unsafe { &mut *child };
        match child_r.parent {
            Some(ParentOfChild::Root(_)) => self.remove_root_child(child),
            Some(ParentOfChild::Element(parent)) => self.remove_element_child(parent, child),
            None => { /* no-op */ },
        }
    }

    pub fn remove_processing_instruction_from_parent(&self, child: *mut ProcessingInstruction) {
        let child_r = unsafe { &mut *child };
        match child_r.parent {
            Some(ParentOfChild::Root(_)) => self.remove_root_child(child),
            Some(ParentOfChild::Element(parent)) => self.remove_element_child(parent, child),
            None => { /* no-op */ },
        }
    }

    pub unsafe fn root_children(&self) -> &[ChildOfRoot] {
        let parent_r = &*self.root;
        &parent_r.children
    }

    pub unsafe fn element_children(&self, parent: *mut Element) -> &[ChildOfElement] {
        let parent_r = &*parent;
        &parent_r.children
    }

    /// Returns the sibling nodes that come before this node. The
    /// nodes are in document order.
    pub unsafe fn element_preceding_siblings(&self, element: *mut Element) -> SiblingIter {
        let element_r = &*element;
        match element_r.parent {
            Some(ParentOfChild::Root(root_parent)) =>
                SiblingIter::of_root(SiblingDirection::Preceding, root_parent, ChildOfRoot::Element(element)),
            Some(ParentOfChild::Element(element_parent)) =>
                SiblingIter::of_element(SiblingDirection::Preceding, element_parent, ChildOfElement::Element(element)),
            None =>
                SiblingIter::dead(),
        }
    }

    /// Returns the sibling nodes that come after this node. The
    /// nodes are in document order.
    pub unsafe fn element_following_siblings(&self, element: *mut Element) -> SiblingIter {
        let element_r = &*element;
        match element_r.parent {
            Some(ParentOfChild::Root(root_parent)) =>
                SiblingIter::of_root(SiblingDirection::Following, root_parent, ChildOfRoot::Element(element)),
            Some(ParentOfChild::Element(element_parent)) =>
                SiblingIter::of_element(SiblingDirection::Following, element_parent, ChildOfElement::Element(element)),
            None =>
                SiblingIter::dead(),
        }
    }

    /// Returns the sibling nodes that come before this node. The
    /// nodes are in document order.
    pub unsafe fn text_preceding_siblings(&self, text: *mut Text) -> SiblingIter {
        let text_r = &*text;
        match text_r.parent {
            Some(element_parent) =>
                SiblingIter::of_element(SiblingDirection::Preceding, element_parent, ChildOfElement::Text(text)),
            None =>
                SiblingIter::dead(),
        }
    }

    /// Returns the sibling nodes that come after this node. The
    /// nodes are in document order.
    pub unsafe fn text_following_siblings(&self, text: *mut Text) -> SiblingIter {
        let text_r = &*text;
        match text_r.parent {
            Some(element_parent) =>
                SiblingIter::of_element(SiblingDirection::Following, element_parent, ChildOfElement::Text(text)),
            None =>
                SiblingIter::dead(),
        }
    }

    /// Returns the sibling nodes that come before this node. The
    /// nodes are in document order.
    pub unsafe fn comment_preceding_siblings(&self, comment: *mut Comment) -> SiblingIter {
        let comment_r = &*comment;
        match comment_r.parent {
            Some(ParentOfChild::Root(root_parent)) =>
                SiblingIter::of_root(SiblingDirection::Preceding, root_parent, ChildOfRoot::Comment(comment)),
            Some(ParentOfChild::Element(element_parent)) =>
                SiblingIter::of_element(SiblingDirection::Preceding, element_parent, ChildOfElement::Comment(comment)),
            None =>
                SiblingIter::dead(),
        }
    }

    /// Returns the sibling nodes that come after this node. The
    /// nodes are in document order.
    pub unsafe fn comment_following_siblings(&self, comment: *mut Comment) -> SiblingIter {
        let comment_r = &*comment;
        match comment_r.parent {
            Some(ParentOfChild::Root(root_parent)) =>
                SiblingIter::of_root(SiblingDirection::Following, root_parent, ChildOfRoot::Comment(comment)),
            Some(ParentOfChild::Element(element_parent)) =>
                SiblingIter::of_element(SiblingDirection::Following, element_parent, ChildOfElement::Comment(comment)),
            None =>
                SiblingIter::dead(),
        }
    }

    /// Returns the sibling nodes that come before this node. The
    /// nodes are in document order.
    pub unsafe fn processing_instruction_preceding_siblings(&self, pi: *mut ProcessingInstruction) -> SiblingIter {
        let pi_r = &*pi;
        match pi_r.parent {
            Some(ParentOfChild::Root(root_parent)) =>
                SiblingIter::of_root(SiblingDirection::Preceding, root_parent, ChildOfRoot::ProcessingInstruction(pi)),
            Some(ParentOfChild::Element(element_parent)) =>
                SiblingIter::of_element(SiblingDirection::Preceding, element_parent, ChildOfElement::ProcessingInstruction(pi)),
            None =>
                SiblingIter::dead(),
        }
    }

    /// Returns the sibling nodes that come after this node. The
    /// nodes are in document order.
    pub unsafe fn processing_instruction_following_siblings(&self, pi: *mut ProcessingInstruction) -> SiblingIter {
        let pi_r = &*pi;
        match pi_r.parent {
            Some(ParentOfChild::Root(root_parent)) =>
                SiblingIter::of_root(SiblingDirection::Following, root_parent, ChildOfRoot::ProcessingInstruction(pi)),
            Some(ParentOfChild::Element(element_parent)) =>
                SiblingIter::of_element(SiblingDirection::Following, element_parent, ChildOfElement::ProcessingInstruction(pi)),
            None =>
                SiblingIter::dead(),
        }
    }

    pub fn attribute_parent(&self, attribute: *mut Attribute) -> Option<*mut Element> {
        let attr_r = unsafe { &*attribute };
        attr_r.parent
    }

    pub unsafe fn attributes(&self, parent: *mut Element) -> &[*mut Attribute] {
        let parent_r = &*parent;
        &parent_r.attributes
    }

    pub fn attribute<'n, N>(&self, element: *mut Element, name: N) -> Option<*mut Attribute>
        where N: Into<QName<'n>>
    {
        let name = name.into();
        let element_r = unsafe { &*element };
        element_r.attributes.iter().find(|a| {
            let a_r: &Attribute = unsafe { &***a };
            a_r.name.as_qname() == name
        }).map(|a| *a)
    }

    pub fn remove_attribute<'n, N>(&self, element: *mut Element, name: N)
        where N: Into<QName<'n>>
    {
        let name = name.into();
        self.remove_attribute_x(element, |a| a.name.as_qname() == name)
    }

    pub fn remove_attribute_x<'n, F>(&self, element: *mut Element, mut pred: F)
        where F: FnMut(&mut Attribute) -> bool,
    {
        let element_r = unsafe { &mut *element };

        element_r.attributes.retain(|&a| {
            let a_r = unsafe { &mut *a };
            let is_this_attr = pred(a_r);
            if is_this_attr {
                a_r.parent = None;
            }
            !is_this_attr
        })
    }

    pub fn set_attribute(&self, parent: *mut Element, attribute: *mut Attribute) {
        let parent_r = unsafe { &mut *parent };
        let attr_r = unsafe { &mut *attribute };

        if let Some(prev_parent) = attr_r.parent {
            let prev_parent_r = unsafe { &mut *prev_parent };
            prev_parent_r.attributes.retain(|&a| a != attribute);
        }

        parent_r.attributes.retain(|a| {
            let a_r: &Attribute = unsafe { &**a };
            a_r.name.as_qname() != attr_r.name.as_qname()
        });
        parent_r.attributes.push(attribute);
        attr_r.parent = Some(parent);
    }

    fn element_parents(&self, element: *mut Element) -> ElementParents {
        ElementParents { element: Some(element), marker: PhantomData }
    }

    pub fn element_namespace_uri_for_prefix(&self, element: *mut Element, prefix: &str)
                                            -> Option<&str>
    {
        self.element_parents(element)
            .filter_map(|e| e.prefix_to_namespace.get(prefix))
            .next()
            .map(|s| s.as_slice())
    }

    pub fn element_prefix_for_namespace_uri(&self,
                                            element: *mut Element,
                                            namespace_uri: &str,
                                            preferred_prefix: Option<&str>)
                                            -> Option<&str>
    {
        for element_r in self.element_parents(element) {
            let prefixes: Vec<_> = element_r.prefix_to_namespace.iter()
                .filter_map(|(&prefix, ns_uri)| {
                    if ns_uri == namespace_uri { Some(prefix) } else { None }
                })
                .collect();

            if let Some(preferred_prefix) = preferred_prefix {
                if let Some(prefix) = prefixes.iter().find(|&prefix| prefix == preferred_prefix) {
                    return Some(prefix.as_slice());
                }
            }

            if let Some(prefix) = prefixes.first() {
                return Some(prefix.as_slice());
            }
        }
        None
    }

    pub fn element_namespaces_in_scope(&self, element: *mut Element)
                                       -> NamespacesInScope
    {
        let mut namespaces = Vec::new();

        namespaces.push((::XML_NS_PREFIX, ::XML_NS_URI));

        let all_namespaces =
            self.element_parents(element)
            .flat_map(|e| e.prefix_to_namespace.iter());

        for (&prefix, &uri) in all_namespaces {
            let namespace = (prefix.as_slice(), uri.as_slice());
            if !namespaces.iter().any(|ns| ns.0 == namespace.0) {
                namespaces.push(namespace)
            }
        }

        NamespacesInScope { iter: namespaces.into_iter() }
    }

    pub fn element_default_namespace_uri(&self, element: *mut Element) -> Option<&str> {
        self.element_parents(element)
            .filter_map(|e| e.default_namespace_uri())
            .next()
    }
}

struct ElementParents<'a> {
    element: Option<*const Element>,
    marker: PhantomData<&'a Element>,
}

impl<'a> Iterator for ElementParents<'a> {
    type Item = &'a Element;

    fn next(&mut self) -> Option<&'a Element> {
        let element_ref = match self.element {
            None => return None,
            Some(e) => unsafe { &*e },
        };

        self.element = match element_ref.parent {
            Some(ParentOfChild::Element(parent)) => Some(parent),
            _ => None,
        };

        Some(element_ref)
    }
}

pub struct NamespacesInScope<'a> {
    // There's probably a more efficient way instead of building up
    // the entire vector, but this has the right API for now.
    iter: ::std::vec::IntoIter<(&'a str, &'a str)>,
}

impl<'a> Iterator for NamespacesInScope<'a> {
    type Item = (&'a str, &'a str);

    fn next(&mut self) -> Option<(&'a str, &'a str)> {
        self.iter.next()
    }
}

enum SiblingDirection {
    Preceding,
    Following,
}

enum SiblingData<'a> {
    FromRoot(slice::Iter<'a, ChildOfRoot>),
    FromElement(slice::Iter<'a, ChildOfElement>),
    Dead,
}

pub struct SiblingIter<'a> {
    data: SiblingData<'a>
}

impl<'a> SiblingIter<'a> {
    unsafe fn of_root(direction: SiblingDirection, root_parent: *mut Root, child: ChildOfRoot) -> SiblingIter<'a> {
        let root_parent_r = &*root_parent;
        let data = &root_parent_r.children;
        let pos = data.iter().position(|c| *c == child).unwrap();

        let data = match direction {
            SiblingDirection::Preceding => &data[..pos],
            SiblingDirection::Following => &data[pos+1..],
        };

        SiblingIter {
            data: SiblingData::FromRoot(data.iter()),
        }
    }

    unsafe fn of_element(direction: SiblingDirection, element_parent: *mut Element, child: ChildOfElement) -> SiblingIter<'a> {
        let element_parent_r = &*element_parent;
        let data = &element_parent_r.children;
        let pos = data.iter().position(|c| *c == child).unwrap();

        let data = match direction {
            SiblingDirection::Preceding => &data[..pos],
            SiblingDirection::Following => &data[pos+1..],
        };

        SiblingIter {
            data: SiblingData::FromElement(data.iter()),
        }
    }

    fn dead() -> SiblingIter<'a> {
        SiblingIter {
            data: SiblingData::Dead
        }
    }
}

impl<'d> Iterator for SiblingIter<'d> {
    type Item = ChildOfElement;

    fn next(&mut self) -> Option<ChildOfElement> {
        match self.data {
            SiblingData::FromRoot(ref mut children) => {
                children.next().map(|&sib| sib.into())
            },
            SiblingData::FromElement(ref mut children) => {
                children.next().cloned()
            },
            SiblingData::Dead => None
        }
    }
}
