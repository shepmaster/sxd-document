//! A traditional DOM tree interface for navigating and manipulating
//! XML documents.

use std::{fmt,hash};

use super::QName;
use super::raw;

type SiblingFn<T> = unsafe fn(&raw::Connections, T) -> raw::SiblingIter;

/// An XML document
#[derive(Copy,Clone)]
pub struct Document<'d> {
    storage: &'d raw::Storage,
    connections: &'d raw::Connections,
}

macro_rules! wrapper(
    ($name:ident, $wrapper:ident, $inner:ty) => (
        fn $name(self, node: *mut $inner) -> $wrapper<'d> {
            $wrapper {
                document: self,
                node: node,
            }
        }
    )
);

impl<'d> Document<'d> {
    wrapper!(wrap_root, Root, raw::Root);
    wrapper!(wrap_element, Element, raw::Element);
    wrapper!(wrap_attribute, Attribute, raw::Attribute);
    wrapper!(wrap_text, Text, raw::Text);
    wrapper!(wrap_comment, Comment, raw::Comment);
    wrapper!(wrap_pi, ProcessingInstruction, raw::ProcessingInstruction);

    #[doc(hidden)]
    pub fn new(storage: &'d raw::Storage, connections: &'d raw::Connections) -> Document<'d> {
        Document {
            storage: storage,
            connections: connections,
        }
    }

    fn wrap_parent_of_child(self, node: raw::ParentOfChild) -> ParentOfChild<'d> {
        match node {
            raw::ParentOfChild::Root(n) => ParentOfChild::Root(self.wrap_root(n)),
            raw::ParentOfChild::Element(n) => ParentOfChild::Element(self.wrap_element(n)),
        }
    }

    fn wrap_child_of_root(self, node: raw::ChildOfRoot) -> ChildOfRoot<'d> {
        match node {
            raw::ChildOfRoot::Element(n) => ChildOfRoot::Element(self.wrap_element(n)),
            raw::ChildOfRoot::Comment(n) => ChildOfRoot::Comment(self.wrap_comment(n)),
            raw::ChildOfRoot::ProcessingInstruction(n) => ChildOfRoot::ProcessingInstruction(self.wrap_pi(n)),
        }
    }

    fn wrap_child_of_element(self, node: raw::ChildOfElement) -> ChildOfElement<'d> {
        match node {
            raw::ChildOfElement::Element(n) => ChildOfElement::Element(self.wrap_element(n)),
            raw::ChildOfElement::Text(n) => ChildOfElement::Text(self.wrap_text(n)),
            raw::ChildOfElement::Comment(n) => ChildOfElement::Comment(self.wrap_comment(n)),
            raw::ChildOfElement::ProcessingInstruction(n) => ChildOfElement::ProcessingInstruction(self.wrap_pi(n)),
        }
    }

    pub fn root(self) -> Root<'d> {
        self.wrap_root(self.connections.root())
    }

    pub fn create_element<'n, N>(self, name: N) -> Element<'d>
        where N: Into<QName<'n>>
    {
        self.wrap_element(self.storage.create_element(name))
    }

    pub fn create_text(self, text: &str) -> Text<'d> {
        self.wrap_text(self.storage.create_text(text))
    }

    pub fn create_comment(self, text: &str) -> Comment<'d> {
        self.wrap_comment(self.storage.create_comment(text))
    }

    pub fn create_processing_instruction(self, target: &str, value: Option<&str>) -> ProcessingInstruction<'d> {
        self.wrap_pi(self.storage.create_processing_instruction(target, value))
    }

    fn siblings<T>(self, f: SiblingFn<T>, node: T) -> Vec<ChildOfElement<'d>> {
        // This is safe because we don't allow the connection
        // information to leak outside of this method.
        unsafe {
            f(self.connections, node).map(|n| self.wrap_child_of_element(n)).collect()
        }
    }
}

impl<'d> PartialEq for Document<'d> {
    fn eq(&self, other: &Document<'d>) -> bool {
        (self.storage as *const raw::Storage, self.connections as *const raw::Connections)
            == (other.storage as *const raw::Storage, other.connections as *const raw::Connections)
    }
}

impl<'d> fmt::Debug for Document<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Document {{ {:?} }}", self as *const Document)
    }
}

macro_rules! node(
    ($name:ident, $raw:ty, $doc:expr) => (
        #[doc = $doc]
        #[derive(Copy,Clone)]
        pub struct $name<'d> {
            document: Document<'d>,
            node: *mut $raw,
        }

        impl<'d> $name<'d> {
            #[allow(dead_code)]
            fn node(&self) -> &'d $raw { unsafe { &*self.node } }

            pub fn document(&self) -> Document<'d> { self.document }
        }

        impl<'d> PartialEq for $name<'d> {
            fn eq(&self, other: &$name<'d>) -> bool {
                self.node == other.node
            }
        }

        impl<'d> Eq for $name<'d> {}

        impl<'d> hash::Hash for $name<'d> {
            fn hash<H>(&self, state: &mut H)
                where H: hash::Hasher
            {
                self.node.hash(state)
            }
        }
    )
);

node!(
    Root, raw::Root,
    "The logical ancestor of every other node type"
);

impl<'d> Root<'d> {
    pub fn append_child<C>(&self, child: C)
        where C: Into<ChildOfRoot<'d>>
    {
        let child = child.into();
        self.document.connections.append_root_child(child.as_raw());
    }

    pub fn append_children<I>(&self, children: I)
        where I: IntoIterator,
              I::Item: Into<ChildOfRoot<'d>>,
    {
        for c in children {
            self.append_child(c.into());
        }
    }

    pub fn replace_children<I>(&self, children: I)
        where I: IntoIterator,
              I::Item: Into<ChildOfRoot<'d>>,
    {
        self.clear_children();
        self.append_children(children);
    }

    pub fn remove_child<C>(&self, child: C)
        where C: Into<ChildOfRoot<'d>>,
    {
        let child = child.into();
        self.document.connections.remove_root_child(child.as_raw())
    }

    pub fn clear_children(&self) {
        self.document.connections.clear_root_children();
    }

    pub fn children(&self) -> Vec<ChildOfRoot<'d>> {
        // This is safe because we copy of the children, and the
        // children are never deallocated.
        unsafe {
            self.document.connections.root_children().iter().map(|n| {
                self.document.wrap_child_of_root(*n)
            }).collect()
        }
    }
}

impl<'d> fmt::Debug for Root<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Root")
    }
}

/// A mapping from a prefix to a URI
pub struct Namespace<'d> {
    prefix: &'d str,
    uri: &'d str,
}

impl<'d> Namespace<'d> {
    pub fn prefix(&self) -> &'d str { self.prefix }
    pub fn uri(&self) -> &'d str { self.uri }
}

node!(
    Element, raw::Element,
    "Elements are the workhorse of a document and may contain any type of
    node, except for the Root node"
);

impl<'d> Element<'d> {
    pub fn name(&self) -> QName<'d> { self.node().name() }

    pub fn set_name<'n, N>(&self, name: N)
        where N: Into<QName<'n>>
    {
        self.document.storage.element_set_name(self.node, name)
    }

    pub fn set_default_namespace_uri(&self, namespace_uri: Option<&str>) {
        self.document.storage.element_set_default_namespace_uri(self.node, namespace_uri);
    }

    pub fn default_namespace_uri(&self) -> Option<&'d str> {
        self.node().default_namespace_uri()
    }

    pub fn recursive_default_namespace_uri(&self) -> Option<&'d str> {
        self.document.connections.element_default_namespace_uri(self.node)
    }

    /// Map a prefix to a namespace URI. Any existing prefix on this
    /// element will be replaced.
    pub fn register_prefix(&self, prefix: &str, namespace_uri: &str) {
        self.document.storage.element_register_prefix(self.node, prefix, namespace_uri);
    }

    /// Recursively resolve the prefix to a namespace URI.
    pub fn namespace_uri_for_prefix(&self, prefix: &str) -> Option<&'d str> {
        self.document.connections.element_namespace_uri_for_prefix(self.node, prefix)
    }

    /// Recursively find a prefix for the namespace URI. Since
    /// multiple prefixes may map to the same URI, `preferred` can be
    /// provided to select a specific prefix, if it is valid.
    pub fn prefix_for_namespace_uri(&self, namespace_uri: &str, preferred: Option<&str>)
                                    -> Option<&'d str>
    {
        self.document.connections.element_prefix_for_namespace_uri(
            self.node, namespace_uri, preferred
        )
    }

    /// Retrieve all namespaces that are in scope, recursively walking
    /// up the document tree.
    pub fn namespaces_in_scope(&self) -> Vec<Namespace<'d>> {
        self.document.connections.element_namespaces_in_scope(self.node).map(|(prefix, uri)| {
            Namespace { prefix: prefix, uri: uri }
        }).collect()
    }

    pub fn preferred_prefix(&self) -> Option<&'d str> {
        self.node().preferred_prefix()
    }

    pub fn set_preferred_prefix(&self, prefix: Option<&str>) {
        self.document.storage.element_set_preferred_prefix(self.node, prefix);
    }

    pub fn parent(&self) -> Option<ParentOfChild<'d>> {
        self.document.connections.element_parent(self.node).map(|n| {
            self.document.wrap_parent_of_child(n)
        })
    }

    pub fn remove_from_parent(&self) {
        self.document.connections.remove_element_from_parent(self.node);
    }

    pub fn append_child<C>(&self, child: C)
        where C: Into<ChildOfElement<'d>>
    {
        let child = child.into();
        self.document.connections.append_element_child(self.node, child.as_raw());
    }

    pub fn append_children<I>(&self, children: I)
        where I: IntoIterator,
              I::Item: Into<ChildOfElement<'d>>,
    {
        for c in children {
            self.append_child(c.into());
        }
    }

    pub fn replace_children<I>(&self, children: I)
        where I: IntoIterator,
              I::Item: Into<ChildOfElement<'d>>,
    {
        self.clear_children();
        self.append_children(children);
    }

    pub fn remove_child<C>(&self, child: C)
        where C: Into<ChildOfElement<'d>>,
    {
        let child = child.into();
        self.document.connections.remove_element_child(self.node, child.as_raw());
    }

    pub fn clear_children(&self) {
        self.document.connections.clear_element_children(self.node);
    }

    pub fn children(&self) -> Vec<ChildOfElement<'d>> {
        // This is safe because we make a copy of the children, and
        // the children are never deallocated.
        unsafe {
            self.document.connections.element_children(self.node).iter().map(|n| {
                self.document.wrap_child_of_element(*n)
            }).collect()
        }
    }

    pub fn preceding_siblings(&self) -> Vec<ChildOfElement<'d>> {
        self.document.siblings(raw::Connections::element_preceding_siblings, self.node)
    }

    pub fn following_siblings(&self) -> Vec<ChildOfElement<'d>> {
        self.document.siblings(raw::Connections::element_following_siblings, self.node)
    }

    pub fn attribute<'n, N>(&self, name: N) -> Option<Attribute<'d>>
        where N: Into<QName<'n>>
    {
        self.document.connections.attribute(self.node, name).map(|n| {
            self.document.wrap_attribute(n)
        })
    }

    pub fn attributes(&self) -> Vec<Attribute<'d>> {
        // This is safe because we make a copy of the children, and
        // the children are never deallocated.
        unsafe {
            self.document.connections.attributes(self.node).iter().map(|n| {
                self.document.wrap_attribute(*n)
            }).collect()
        }
    }

    pub fn set_attribute_value<'n, N>(&self, name: N, value: &str) -> Attribute<'d>
        where N: Into<QName<'n>>
    {
        let attr = self.document.storage.create_attribute(name, value);
        self.document.connections.set_attribute(self.node, attr);
        self.document.wrap_attribute(attr)
    }

    pub fn attribute_value<'n, N>(&self, name: N) -> Option<&'d str>
        where N: Into<QName<'n>>
    {
        self.document.connections.attribute(self.node, name).map(|a| {
            let a_r = unsafe { &*a };
            a_r.value()
        })
    }

    pub fn remove_attribute<'n, N>(&self, name: N)
        where N: Into<QName<'n>>
    {
        self.document.connections.remove_attribute(self.node, name);
    }

    pub fn set_text(&self, text: &str) -> Text {
        let text = self.document.create_text(text);
        self.clear_children();
        self.append_child(text);
        text
    }
}

impl<'d> fmt::Debug for Element<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Element {{ name: {:?} }}", self.name())
    }
}

node!(
    Attribute, raw::Attribute,
    "Metadata about the current element"
);

impl<'d> Attribute<'d> {
    pub fn name(&self)  -> QName<'d> { self.node().name() }
    pub fn value(&self) -> &'d str { self.node().value() }

    pub fn preferred_prefix(&self) -> Option<&'d str> {
        self.node().preferred_prefix()
    }

    pub fn set_preferred_prefix(&self, prefix: Option<&str>) {
        self.document.storage.attribute_set_preferred_prefix(self.node, prefix);
    }

    pub fn parent(&self) -> Option<Element<'d>> {
        self.document.connections.attribute_parent(self.node).map(|n| {
            self.document.wrap_element(n)
        })
    }

    pub fn remove_from_parent(&self) {
        self.document.connections.remove_attribute_from_parent(self.node);
    }
}

impl<'d> fmt::Debug for Attribute<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Attribute {{ name: {:?}, value: {:?} }}", self.name(), self.value())
    }
}

node!(
    Text, raw::Text,
    "Textual data"
);

impl<'d> Text<'d> {
    pub fn text(&self) -> &'d str { self.node().text() }

    pub fn set_text(&self, text: &str) {
        self.document.storage.text_set_text(self.node, text)
    }

    pub fn parent(&self) -> Option<Element<'d>> {
        self.document.connections.text_parent(self.node).map(|n| {
            self.document.wrap_element(n)
        })
    }

    pub fn remove_from_parent(&self) {
        self.document.connections.remove_text_from_parent(self.node);
    }

    pub fn preceding_siblings(&self) -> Vec<ChildOfElement<'d>> {
        self.document.siblings(raw::Connections::text_preceding_siblings, self.node)
    }

    pub fn following_siblings(&self) -> Vec<ChildOfElement<'d>> {
        self.document.siblings(raw::Connections::text_following_siblings, self.node)
    }
}

impl<'d> fmt::Debug for Text<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Text {{ text: {:?} }}", self.text())
    }
}

node!(
    Comment, raw::Comment,
    "Information only relevant to humans"
);

impl<'d> Comment<'d> {
    pub fn text(&self) -> &'d str { self.node().text() }

    pub fn set_text(&self, new_text: &str) {
        self.document.storage.comment_set_text(self.node, new_text)
    }

    pub fn parent(&self) -> Option<ParentOfChild<'d>> {
        self.document.connections.comment_parent(self.node).map(|n| {
            self.document.wrap_parent_of_child(n)
        })
    }

    pub fn remove_from_parent(&self) {
        self.document.connections.remove_comment_from_parent(self.node);
    }

    pub fn preceding_siblings(&self) -> Vec<ChildOfElement<'d>> {
        self.document.siblings(raw::Connections::comment_preceding_siblings, self.node)
    }

    pub fn following_siblings(&self) -> Vec<ChildOfElement<'d>> {
        self.document.siblings(raw::Connections::comment_following_siblings, self.node)
    }
}

impl<'d> fmt::Debug for Comment<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Comment {{ text: {:?} }}", self.text())
    }
}

node!(
    ProcessingInstruction, raw::ProcessingInstruction,
    "Metadata relevant to the application, but not the XML processor or humans"
);

impl<'d> ProcessingInstruction<'d> {
    pub fn target(&self) -> &'d str { self.node().target() }
    pub fn value(&self) -> Option<&'d str> { self.node().value() }

    pub fn set_target(&self, new_target: &str) {
        self.document.storage.processing_instruction_set_target(self.node, new_target);
    }

    pub fn set_value(&self, new_value: Option<&str>) {
        self.document.storage.processing_instruction_set_value(self.node, new_value);
    }

    pub fn parent(&self) -> Option<ParentOfChild<'d>> {
        self.document.connections.processing_instruction_parent(self.node).map(|n| {
            self.document.wrap_parent_of_child(n)
        })
    }

    pub fn remove_from_parent(&self) {
        self.document.connections.remove_processing_instruction_from_parent(self.node);
    }

    pub fn preceding_siblings(&self) -> Vec<ChildOfElement<'d>> {
        self.document.siblings(raw::Connections::processing_instruction_preceding_siblings, self.node)
    }

    pub fn following_siblings(&self) -> Vec<ChildOfElement<'d>> {
        self.document.siblings(raw::Connections::processing_instruction_following_siblings, self.node)
    }
}

impl<'d> fmt::Debug for ProcessingInstruction<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ProcessingInstruction {{ target: {:?}, value: {:?} }}", self.target(), self.value())
    }
}

macro_rules! unpack(
    ($enum_name:ident, $name:ident, $wrapper:ident, $inner:ident) => (
        pub fn $name(self) -> Option<$inner<'d>> {
            match self {
                $enum_name::$wrapper(n) => Some(n),
                _ => None,
            }
        }
    )
);

/// Nodes that may occur as a child of the root node
#[derive(Debug,Copy,Clone,PartialEq)]
pub enum ChildOfRoot<'d> {
    Element(Element<'d>),
    Comment(Comment<'d>),
    ProcessingInstruction(ProcessingInstruction<'d>),
}


impl<'d> ChildOfRoot<'d> {
    unpack!(ChildOfRoot, element, Element, Element);
    unpack!(ChildOfRoot, comment, Comment, Comment);
    unpack!(ChildOfRoot, processing_instruction, ProcessingInstruction, ProcessingInstruction);

    fn as_raw(&self) -> raw::ChildOfRoot {
        match *self {
            ChildOfRoot::Element(n) => raw::ChildOfRoot::Element(n.node),
            ChildOfRoot::Comment(n) => raw::ChildOfRoot::Comment(n.node),
            ChildOfRoot::ProcessingInstruction(n) => raw::ChildOfRoot::ProcessingInstruction(n.node),
        }
    }
}

/// Nodes that may occur as a child of an element node
#[derive(Debug,Copy,Clone,PartialEq)]
pub enum ChildOfElement<'d> {
    Element(Element<'d>),
    Text(Text<'d>),
    Comment(Comment<'d>),
    ProcessingInstruction(ProcessingInstruction<'d>),
}

impl<'d> ChildOfElement<'d> {
    unpack!(ChildOfElement, element, Element, Element);
    unpack!(ChildOfElement, text, Text, Text);
    unpack!(ChildOfElement, comment, Comment, Comment);
    unpack!(ChildOfElement, processing_instruction, ProcessingInstruction, ProcessingInstruction);

    fn as_raw(&self) -> raw::ChildOfElement {
        match *self {
            ChildOfElement::Element(n) => raw::ChildOfElement::Element(n.node),
            ChildOfElement::Text(n) => raw::ChildOfElement::Text(n.node),
            ChildOfElement::Comment(n) => raw::ChildOfElement::Comment(n.node),
            ChildOfElement::ProcessingInstruction(n) => raw::ChildOfElement::ProcessingInstruction(n.node),
        }
    }
}

/// Nodes that may occur as the parent of a child node
#[derive(Debug,Copy,Clone,PartialEq)]
pub enum ParentOfChild<'d> {
    Root(Root<'d>),
    Element(Element<'d>),
}

impl<'d> ParentOfChild<'d> {
    unpack!(ParentOfChild, root, Root, Root);
    unpack!(ParentOfChild, element, Element, Element);
}

macro_rules! conversion_trait(
    ($res_type:ident, {
        $($leaf_type:ident => $variant:expr),*
    }) => (
        $(impl<'d> From<$leaf_type<'d>> for $res_type<'d> {
            fn from(v: $leaf_type<'d>) -> $res_type<'d> {
                $variant(v)
            }
        })*

        $(impl<'a, 'd> From<&'a $leaf_type<'d>> for $res_type<'d> {
            fn from(v: &'a $leaf_type<'d>) -> $res_type<'d> {
                $variant(*v)
            }
        })*
    )
);

conversion_trait!(
    ChildOfRoot, {
        Element               => ChildOfRoot::Element,
        Comment               => ChildOfRoot::Comment,
        ProcessingInstruction => ChildOfRoot::ProcessingInstruction
    }
);

conversion_trait!(
    ChildOfElement, {
        Element               => ChildOfElement::Element,
        Text                  => ChildOfElement::Text,
        Comment               => ChildOfElement::Comment,
        ProcessingInstruction => ChildOfElement::ProcessingInstruction
    }
);

impl<'d> From<ChildOfRoot<'d>> for ChildOfElement<'d> {
    fn from(v: ChildOfRoot<'d>) -> ChildOfElement<'d> {
        match v {
            ChildOfRoot::Element(n) => ChildOfElement::Element(n),
            ChildOfRoot::Comment(n) => ChildOfElement::Comment(n),
            ChildOfRoot::ProcessingInstruction(n) => ChildOfElement::ProcessingInstruction(n),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::{Package,QName};
    use super::{ChildOfRoot,ChildOfElement,ParentOfChild};

    macro_rules! assert_qname_eq(
        ($l:expr, $r:expr) => (assert_eq!(Into::<QName>::into($l), $r.into()));
    );

    #[test]
    fn the_root_belongs_to_a_document() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();

        assert_eq!(doc, root.document());
    }

    #[test]
    fn root_can_have_element_children() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let element = doc.create_element("alpha");

        root.append_child(element);

        let children = root.children();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfRoot::Element(element));
    }

    #[test]
    fn root_has_maximum_of_one_element_child() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let alpha = doc.create_element("alpha");
        let beta = doc.create_element("beta");

        root.append_child(alpha);
        root.append_child(beta);

        let children = root.children();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfRoot::Element(beta));
    }

    #[test]
    fn root_can_have_comment_children() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let comment = doc.create_comment("Now is the winter of our discontent.");

        root.append_child(comment);

        let children = root.children();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfRoot::Comment(comment));
    }

    #[test]
    fn root_can_have_processing_instruction_children() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let pi = doc.create_processing_instruction("device", None);

        root.append_child(pi);

        let children = root.children();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfRoot::ProcessingInstruction(pi));
    }

    #[test]
    fn root_can_append_multiple_children() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let alpha = doc.create_comment("alpha");
        let beta = doc.create_comment("beta");

        root.append_children(&[alpha, beta]);

        let children = root.children();
        assert_eq!(2, children.len());
        assert_eq!(children[0], ChildOfRoot::Comment(alpha));
        assert_eq!(children[1], ChildOfRoot::Comment(beta));
    }

    #[test]
    fn root_can_replace_children() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let alpha = doc.create_comment("alpha");
        let beta = doc.create_comment("beta");
        let gamma = doc.create_comment("gamma");
        root.append_child(alpha);

        root.replace_children(&[beta, gamma]);

        let children = root.children();
        assert_eq!(2, children.len());
        assert_eq!(children[0], ChildOfRoot::Comment(beta));
        assert_eq!(children[1], ChildOfRoot::Comment(gamma));
    }

    #[test]
    fn root_can_remove_children() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let element = doc.create_element("alpha");
        root.append_child(element);

        root.remove_child(element);

        assert!(root.children().is_empty());
        assert!(element.parent().is_none());
    }

    #[test]
    fn root_can_clear_children() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let element = doc.create_element("alpha");
        root.append_child(element);

        root.clear_children();

        assert!(root.children().is_empty());
        assert!(element.parent().is_none());
    }

    #[test]
    fn root_child_knows_its_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let alpha = doc.create_element("alpha");

        root.append_child(alpha);

        assert_eq!(Some(ParentOfChild::Root(root)), alpha.parent());
    }

    #[test]
    fn elements_belong_to_a_document() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("alpha");

        assert_eq!(doc, element.document());
    }

    #[test]
    fn elements_can_have_element_children() {
        let package = Package::new();
        let doc = package.as_document();

        let alpha = doc.create_element("alpha");
        let beta  = doc.create_element("beta");

        alpha.append_child(beta);

        let children = alpha.children();

        assert_eq!(children[0], ChildOfElement::Element(beta));
    }

    #[test]
    fn elements_can_append_multiple_children() {
        let package = Package::new();
        let doc = package.as_document();

        let alpha = doc.create_element("alpha");
        let beta = doc.create_element("beta");
        let gamma = doc.create_element("gamma");

        alpha.append_children(&[beta, gamma]);

        let children = alpha.children();
        assert_eq!(2, children.len());
        assert_eq!(children[0], ChildOfElement::Element(beta));
        assert_eq!(children[1], ChildOfElement::Element(gamma));
    }

    #[test]
    fn elements_can_replace_children() {
        let package = Package::new();
        let doc = package.as_document();

        let alpha = doc.create_element("alpha");
        let beta = doc.create_element("beta");
        let gamma = doc.create_element("gamma");
        let zeta = doc.create_element("zeta");
        alpha.append_child(zeta);

        alpha.replace_children(&[beta, gamma]);

        let children = alpha.children();
        assert_eq!(2, children.len());
        assert_eq!(children[0], ChildOfElement::Element(beta));
        assert_eq!(children[1], ChildOfElement::Element(gamma));
    }

    #[test]
    fn elements_can_remove_children() {
        let package = Package::new();
        let doc = package.as_document();

        let alpha = doc.create_element("alpha");
        let beta  = doc.create_element("beta");
        alpha.append_child(beta);

        alpha.remove_child(beta);

        assert!(alpha.children().is_empty());
        assert!(beta.parent().is_none());
    }

    #[test]
    fn elements_can_be_removed_from_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let alpha = doc.create_element("alpha");
        let beta  = doc.create_element("beta");
        alpha.append_child(beta);

        beta.remove_from_parent();

        assert!(alpha.children().is_empty());
        assert!(beta.parent().is_none());
    }

    #[test]
    fn elements_can_clear_children() {
        let package = Package::new();
        let doc = package.as_document();

        let alpha = doc.create_element("alpha");
        let beta  = doc.create_element("beta");
        alpha.append_child(beta);

        alpha.clear_children();

        assert!(alpha.children().is_empty());
        assert!(beta.parent().is_none());
    }

    #[test]
    fn element_children_are_ordered() {
        let package = Package::new();
        let doc = package.as_document();

        let greek = doc.create_element("greek");
        let alpha = doc.create_element("alpha");
        let omega = doc.create_element("omega");

        greek.append_child(alpha);
        greek.append_child(omega);

        let children = greek.children();

        assert_eq!(children[0], ChildOfElement::Element(alpha));
        assert_eq!(children[1], ChildOfElement::Element(omega));
    }

    #[test]
    fn element_children_know_their_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let alpha = doc.create_element("alpha");
        let beta  = doc.create_element("beta");

        alpha.append_child(beta);

        assert_eq!(Some(ParentOfChild::Element(alpha)), beta.parent());
    }

    #[test]
    fn elements_know_preceding_siblings() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        let a = doc.create_element("a");
        let b = doc.create_element("b");
        let c = doc.create_element("c");
        let d = doc.create_element("d");

        parent.append_child(a);
        parent.append_child(b);
        parent.append_child(c);
        parent.append_child(d);

        assert_eq!(vec![ChildOfElement::Element(a), ChildOfElement::Element(b)], c.preceding_siblings());
    }

    #[test]
    fn elements_know_following_siblings() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        let a = doc.create_element("a");
        let b = doc.create_element("b");
        let c = doc.create_element("c");
        let d = doc.create_element("d");

        parent.append_child(a);
        parent.append_child(b);
        parent.append_child(c);
        parent.append_child(d);

        assert_eq!(vec![ChildOfElement::Element(c), ChildOfElement::Element(d)], b.following_siblings());
    }

    #[test]
    fn changing_parent_of_element_removes_element_from_original_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let parent1 = doc.create_element("parent1");
        let parent2 = doc.create_element("parent2");
        let child = doc.create_element("child");

        parent1.append_child(child);
        parent2.append_child(child);

        assert!(parent1.children().is_empty());
        assert_eq!(1, parent2.children().len());
    }

    #[test]
    fn elements_can_be_renamed() {
        let package = Package::new();
        let doc = package.as_document();

        let alpha = doc.create_element("alpha");
        alpha.set_name("beta");
        assert_qname_eq!(alpha.name(), "beta");
    }

    #[test]
    fn elements_know_in_scope_namespaces() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("alpha");
        element.register_prefix("a", "uri");

        let nses = element.namespaces_in_scope();
        assert_eq!(2, nses.len());

        let xml_ns = nses.iter().find(|ns| ns.prefix() == "xml").unwrap();
        assert_eq!("http://www.w3.org/XML/1998/namespace", xml_ns.uri());

        let a_ns = nses.iter().find(|ns| ns.prefix() == "a").unwrap();
        assert_eq!("uri", a_ns.uri());
    }

    #[test]
    fn elements_in_scope_namespaces_override_parents_with_the_same_prefix() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        parent.register_prefix("prefix", "uri1");

        let child = doc.create_element("child");
        child.register_prefix("prefix", "uri2");

        parent.append_child(child);

        let nses = child.namespaces_in_scope();
        assert_eq!(2, nses.len());

        let ns = nses.iter().find(|ns| ns.prefix() == "prefix").unwrap();
        assert_eq!("uri2", ns.uri());
    }

    #[test]
    fn attributes_belong_to_a_document() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("alpha");
        let attr = element.set_attribute_value("hello", "world");

        assert_eq!(doc, attr.document());
    }

    #[test]
    fn elements_have_attributes() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");

        element.set_attribute_value("hello", "world");

        assert_eq!(Some("world"), element.attribute_value("hello"));
    }

    #[test]
    fn attributes_know_their_element() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");
        let attr = element.set_attribute_value("hello", "world");

        assert_eq!(Some(element), attr.parent());
    }

    #[test]
    fn attributes_can_be_reset() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");

        element.set_attribute_value("hello", "world");
        element.set_attribute_value("hello", "galaxy");

        assert_eq!(Some("galaxy"), element.attribute_value("hello"));
    }

    #[test]
    fn attributes_can_be_removed() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");
        let attribute = element.set_attribute_value("hello", "world");

        element.remove_attribute("hello");

        assert!(element.attribute("hello").is_none());
        assert!(attribute.parent().is_none());
    }

    #[test]
    fn attributes_can_be_removed_from_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");
        let attribute = element.set_attribute_value("hello", "world");

        attribute.remove_from_parent();

        assert!(element.attribute("hello").is_none());
        assert!(attribute.parent().is_none());
    }

    #[test]
    fn attributes_can_be_iterated() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");

        element.set_attribute_value("name1", "value1");
        element.set_attribute_value("name2", "value2");

        let mut attrs = element.attributes();
        attrs.sort_by(|a, b| a.name().namespace_uri().cmp(&b.name().namespace_uri()));

        assert_eq!(2, attrs.len());
        assert_qname_eq!("name1",  attrs[0].name());
        assert_eq!("value1", attrs[0].value());
        assert_qname_eq!("name2",  attrs[1].name());
        assert_eq!("value2", attrs[1].value());
    }

    #[test]
    fn text_belongs_to_a_document() {
        let package = Package::new();
        let doc = package.as_document();

        let text = doc.create_text("Now is the winter of our discontent.");

        assert_eq!(doc, text.document());
    }

    #[test]
    fn elements_can_have_text_children() {
        let package = Package::new();
        let doc = package.as_document();

        let sentence = doc.create_element("sentence");
        let text = doc.create_text("Now is the winter of our discontent.");

        sentence.append_child(text);

        let children = sentence.children();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfElement::Text(text));
    }

    #[test]
    fn elements_can_set_text() {
        let package = Package::new();
        let doc = package.as_document();

        let sentence = doc.create_element("sentence");
        let quote = "Now is the winter of our discontent.";
        let text = sentence.set_text(quote);

        let children = sentence.children();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfElement::Text(text));
        assert_eq!(children[0].text().unwrap().text(), quote);
    }

    #[test]
    fn text_knows_its_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let sentence = doc.create_element("sentence");
        let text = doc.create_text("Now is the winter of our discontent.");

        sentence.append_child(text);

        assert_eq!(text.parent(), Some(sentence));
    }

    #[test]
    fn text_can_be_removed_from_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let sentence = doc.create_element("sentence");
        let text = doc.create_text("Now is the winter of our discontent.");
        sentence.append_child(text);

        text.remove_from_parent();

        assert!(sentence.children().is_empty());
        assert!(text.parent().is_none());
    }

    #[test]
    fn text_knows_preceding_siblings() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        let a = doc.create_element("a");
        let b = doc.create_text("b");

        parent.append_child(a);
        parent.append_child(b);

        assert_eq!(vec![ChildOfElement::Element(a)], b.preceding_siblings());
    }

    #[test]
    fn text_knows_following_siblings() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        let a = doc.create_text("a");
        let b = doc.create_element("b");

        parent.append_child(a);
        parent.append_child(b);

        assert_eq!(vec![ChildOfElement::Element(b)], a.following_siblings());
    }

    #[test]
    fn text_can_be_changed() {
        let package = Package::new();
        let doc = package.as_document();

        let text = doc.create_text("Now is the winter of our discontent.");

        text.set_text("Made glorious summer by this sun of York");

        assert_eq!(text.text(), "Made glorious summer by this sun of York");
    }

    #[test]
    fn comment_belongs_to_a_document() {
        let package = Package::new();
        let doc = package.as_document();

        let comment = doc.create_comment("Now is the winter of our discontent.");

        assert_eq!(doc, comment.document());
    }

    #[test]
    fn elements_can_have_comment_children() {
        let package = Package::new();
        let doc = package.as_document();

        let sentence = doc.create_element("sentence");
        let comment = doc.create_comment("Now is the winter of our discontent.");

        sentence.append_child(comment);

        let children = sentence.children();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfElement::Comment(comment));
    }

    #[test]
    fn comment_knows_its_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let sentence = doc.create_element("sentence");
        let comment = doc.create_comment("Now is the winter of our discontent.");

        sentence.append_child(comment);

        assert_eq!(comment.parent(), Some(ParentOfChild::Element(sentence)));
    }

    #[test]
    fn comments_can_be_removed_from_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let sentence = doc.create_element("sentence");
        let comment = doc.create_comment("Now is the winter of our discontent.");
        sentence.append_child(comment);

        comment.remove_from_parent();

        assert!(sentence.children().is_empty());
        assert!(comment.parent().is_none());
    }

    #[test]
    fn comment_knows_preceding_siblings() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        let a = doc.create_element("a");
        let b = doc.create_comment("b");

        parent.append_child(a);
        parent.append_child(b);

        assert_eq!(vec![ChildOfElement::Element(a)], b.preceding_siblings());
    }

    #[test]
    fn comment_knows_following_siblings() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        let a = doc.create_comment("a");
        let b = doc.create_element("b");

        parent.append_child(a);
        parent.append_child(b);

        assert_eq!(vec![ChildOfElement::Element(b)], a.following_siblings());
    }

    #[test]
    fn comment_can_be_changed() {
        let package = Package::new();
        let doc = package.as_document();

        let comment = doc.create_comment("Now is the winter of our discontent.");

        comment.set_text("Made glorious summer by this sun of York");

        assert_eq!(comment.text(), "Made glorious summer by this sun of York");
    }

    #[test]
    fn processing_instruction_belongs_to_a_document() {
        let package = Package::new();
        let doc = package.as_document();

        let pi = doc.create_processing_instruction("device", None);

        assert_eq!(doc, pi.document());
    }

    #[test]
    fn elements_can_have_processing_instruction_children() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");
        let pi = doc.create_processing_instruction("device", None);

        element.append_child(pi);

        let children = element.children();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfElement::ProcessingInstruction(pi));
    }

    #[test]
    fn processing_instruction_knows_its_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");
        let pi = doc.create_processing_instruction("device", None);

        element.append_child(pi);

        assert_eq!(pi.parent(), Some(ParentOfChild::Element(element)));
    }


    #[test]
    fn processing_instruction_can_be_removed_from_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");
        let pi = doc.create_processing_instruction("device", None);
        element.append_child(pi);

        pi.remove_from_parent();

        assert!(element.children().is_empty());
        assert!(pi.parent().is_none());
    }

    #[test]
    fn processing_instruction_knows_preceding_siblings() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        let a = doc.create_element("a");
        let b = doc.create_processing_instruction("b", None);

        parent.append_child(a);
        parent.append_child(b);

        assert_eq!(vec![ChildOfElement::Element(a)], b.preceding_siblings());
    }

    #[test]
    fn processing_instruction_knows_following_siblings() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        let a = doc.create_processing_instruction("a", None);
        let b = doc.create_element("b");

        parent.append_child(a);
        parent.append_child(b);

        assert_eq!(vec![ChildOfElement::Element(b)], a.following_siblings());
    }

    #[test]
    fn processing_instruction_can_be_changed() {
        let package = Package::new();
        let doc = package.as_document();

        let pi = doc.create_processing_instruction("device", None);

        pi.set_target("output");
        pi.set_value(Some("full-screen"));

        assert_eq!(pi.target(), "output");
        assert_eq!(pi.value(), Some("full-screen"));
    }

    #[test]
    fn can_return_a_populated_package() {
        fn populate() -> Package {
            let package = Package::new();
            {
                let doc = package.as_document();

                let element = doc.create_element("hello");
                doc.root().append_child(element);
            }

            package
        }

        let package = populate();
        let doc = package.as_document();
        let element = doc.root().children()[0].element().unwrap();
        assert_qname_eq!(element.name(), "hello");
    }

    #[test]
    #[cfg(feature = "compile_failure")]
    fn nodes_cannot_live_outside_of_the_document() {
        let package = Package::new();

        let _ = {
            let doc = package.as_document();

            doc.create_element("hello")
        };
    }
}
