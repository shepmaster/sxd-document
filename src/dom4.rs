use std::cell::RefCell;
use std::{fmt,hash};

use super::{QName,ToQName};
use super::raw;

type SiblingFn<T> = unsafe fn(&raw::Connections, T) -> raw::SiblingIter;

pub struct Document<'d> {
    storage: &'d raw::Storage,
    connections: RefCell<&'d raw::Connections>,
}

macro_rules! wrapper(
    ($name:ident, $wrapper:ident, $inner:ty) => (
        fn $name(&'d self, node: *mut $inner) -> $wrapper<'d> {
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

    pub fn new(storage: &'d raw::Storage, connections: &'d raw::Connections) -> Document<'d> {
        Document {
            storage: storage,
            connections: RefCell::new(connections),
        }
    }

    fn wrap_parent_of_child(&'d self, node: raw::ParentOfChild) -> ParentOfChild<'d> {
        match node {
            raw::ParentOfChild::Root(n) => ParentOfChild::Root(self.wrap_root(n)),
            raw::ParentOfChild::Element(n) => ParentOfChild::Element(self.wrap_element(n)),
        }
    }

    fn wrap_child_of_root(&'d self, node: raw::ChildOfRoot) -> ChildOfRoot<'d> {
        match node {
            raw::ChildOfRoot::Element(n) => ChildOfRoot::Element(self.wrap_element(n)),
            raw::ChildOfRoot::Comment(n) => ChildOfRoot::Comment(self.wrap_comment(n)),
            raw::ChildOfRoot::ProcessingInstruction(n) => ChildOfRoot::ProcessingInstruction(self.wrap_pi(n)),
        }
    }

    fn wrap_child_of_element(&'d self, node: raw::ChildOfElement) -> ChildOfElement<'d> {
        match node {
            raw::ChildOfElement::Element(n) => ChildOfElement::Element(self.wrap_element(n)),
            raw::ChildOfElement::Text(n) => ChildOfElement::Text(self.wrap_text(n)),
            raw::ChildOfElement::Comment(n) => ChildOfElement::Comment(self.wrap_comment(n)),
            raw::ChildOfElement::ProcessingInstruction(n) => ChildOfElement::ProcessingInstruction(self.wrap_pi(n)),
        }
    }

    pub fn root(&'d self) -> Root<'d> {
        self.wrap_root(self.connections.borrow().root())
    }

    pub fn create_element<'n, N>(&'d self, name: N) -> Element<'d>
        where N: ToQName<'n>
    {
        self.wrap_element(self.storage.create_element(name))
    }

    pub fn create_text(&'d self, text: &str) -> Text<'d> {
        self.wrap_text(self.storage.create_text(text))
    }

    pub fn create_comment(&'d self, text: &str) -> Comment<'d> {
        self.wrap_comment(self.storage.create_comment(text))
    }

    pub fn create_processing_instruction(&'d self, target: &str, value: Option<&str>) -> ProcessingInstruction<'d> {
        self.wrap_pi(self.storage.create_processing_instruction(target, value))
    }

    fn siblings<T>(&'d self, f: SiblingFn<T>, node: T) -> Vec<ChildOfElement<'d>> {
        let connections = self.connections.borrow();

        // This is safe because we don't allow the connection
        // information to leak outside of this method.
        unsafe {
            f(*connections, node).map(|n| self.wrap_child_of_element(n)).collect()
        }
    }
}

impl<'d> PartialEq for Document<'d> {
    fn eq(&self, other: &Document<'d>) -> bool {
        self as *const Document == other as *const Document
    }
}

impl<'d> fmt::Debug for Document<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Document {{ {:?} }}", self as *const Document)
    }
}

macro_rules! node(
    ($name:ident, $raw:ty) => (
        #[allow(raw_pointer_derive)]
        #[derive(Copy,Clone)]
        pub struct $name<'d> {
            document: &'d Document<'d>,
            node: *mut $raw,
        }

        impl<'d> $name<'d> {
            #[allow(dead_code)]
            fn node(&self) -> &'d $raw { unsafe { &*self.node } }

            pub fn document(&self) -> &'d Document<'d> { self.document }
        }

        impl<'d> PartialEq for $name<'d> {
            fn eq(&self, other: &$name<'d>) -> bool {
                self.node == other.node
            }
        }

        impl<'d> Eq for $name<'d> {}

        impl<'d, H> hash::Hash<H> for $name<'d>
            where H: hash::Hasher + hash::Writer
        {
            fn hash(&self, state: &mut H) {
                self.node.hash(state)
            }
        }
    )
);

node!(Root, raw::Root);

impl<'d> Root<'d> {
    pub fn append_child<C>(&self, child: C)
        where C: ToChildOfRoot<'d>
    {
        let child = child.to_child_of_root();
        let connections = self.document.connections.borrow_mut();
        connections.append_root_child(child.as_raw())
    }

    pub fn children(&self) -> Vec<ChildOfRoot<'d>> {
        let connections = self.document.connections.borrow();
        // This is safe because we copy of the children, and the
        // children are never deallocated.
        unsafe {
            let raw_children = connections.root_children();
            raw_children.iter().map(|n| {
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

node!(Element, raw::Element);

impl<'d> Element<'d> {
    pub fn name(&self) -> QName<'d> { self.node().name() }

    pub fn set_name<'n, N>(&self, name: N)
        where N: ToQName<'n>
    {
        self.document.storage.element_set_name(self.node, name)
    }

    pub fn register_prefix(&self, prefix: &str, namespace_uri: &str) {
        self.document.storage.element_register_prefix(self.node, prefix, namespace_uri);
    }

    pub fn namespace_uri_for_prefix(&self, prefix: &str) -> Option<&str> {
        let connections = self.document.connections.borrow();
        connections.element_namespace_uri_for_prefix(self.node, prefix)
    }

    pub fn preferred_prefix(&self) -> Option<&str> {
        self.node().preferred_prefix()
    }

    pub fn set_preferred_prefix(&self, prefix: Option<&str>) {
        self.document.storage.element_set_preferred_prefix(self.node, prefix);
    }

    pub fn parent(&self) -> Option<ParentOfChild<'d>> {
        let connections = self.document.connections.borrow();

        connections.element_parent(self.node).map(|n| {
            self.document.wrap_parent_of_child(n)
        })
    }

    pub fn append_child<C>(&self, child: C)
        where C: ToChildOfElement<'d>
    {
        let child = child.to_child_of_element();
        let connections = self.document.connections.borrow_mut();
        connections.append_element_child(self.node, child.as_raw())
    }

    pub fn children(&self) -> Vec<ChildOfElement<'d>> {
        let connections = self.document.connections.borrow();
        // This is safe because we copy of the children, and the
        // children are never deallocated.
        unsafe {
            let raw_children = connections.element_children(self.node);
            raw_children.iter().map(|n| {
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
        where N: ToQName<'n>
    {
        let connections = self.document.connections.borrow();
        connections.attribute(self.node, name).map(|n| {
            self.document.wrap_attribute(n)
        })
    }

    pub fn attributes(&self) -> Vec<Attribute<'d>> {
        let connections = self.document.connections.borrow();
        // This is safe because we copy of the children, and the
        // children are never deallocated.
        unsafe {
            connections.attributes(self.node).iter().map(|n| {
                self.document.wrap_attribute(*n)
            }).collect()
        }
    }

    pub fn set_attribute_value<'n, N>(&self, name: N, value: &str) -> Attribute<'d>
        where N: ToQName<'n>
    {
        let attr = self.document.storage.create_attribute(name, value);
        let connections = self.document.connections.borrow_mut();
        connections.set_attribute(self.node, attr);
        self.document.wrap_attribute(attr)
    }

    pub fn attribute_value<'n, N>(&self, name: N) -> Option<&'d str>
        where N: ToQName<'n>
    {
        let connections = self.document.connections.borrow();
        connections.attribute(self.node, name).map(|a| {
            let a_r = unsafe { &*a };
            a_r.value()
        })
    }
}

impl<'d> fmt::Debug for Element<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Element {{ name: {:?} }}", self.name())
    }
}

node!(Attribute, raw::Attribute);

impl<'d> Attribute<'d> {
    pub fn name(&self)  -> QName<'d> { self.node().name() }
    pub fn value(&self) -> &str { self.node().value() }

    pub fn preferred_prefix(&self) -> Option<&str> {
        self.node().preferred_prefix()
    }

    pub fn set_preferred_prefix(&self, prefix: Option<&str>) {
        self.document.storage.attribute_set_preferred_prefix(self.node, prefix);
    }

    pub fn parent(&self) -> Option<Element<'d>> {
        let connections = self.document.connections.borrow();
        connections.attribute_parent(self.node).map(|n| {
            self.document.wrap_element(n)
        })
    }
}

impl<'d> fmt::Debug for Attribute<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Attribute {{ name: {:?}, value: {:?} }}", self.name(), self.value())
    }
}

node!(Text, raw::Text);

impl<'d> Text<'d> {
    pub fn text(&self) -> &str { self.node().text() }

    pub fn set_text(&self, text: &str) {
        self.document.storage.text_set_text(self.node, text)
    }

    pub fn parent(&self) -> Option<Element<'d>> {
        let connections = self.document.connections.borrow();
        connections.text_parent(self.node).map(|n| {
            self.document.wrap_element(n)
        })
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

node!(Comment, raw::Comment);

impl<'d> Comment<'d> {
    pub fn text(&self) -> &str { self.node().text() }

    pub fn set_text(&self, new_text: &str) {
        self.document.storage.comment_set_text(self.node, new_text)
    }

    pub fn parent(&self) -> Option<ParentOfChild<'d>> {
        let connections = self.document.connections.borrow();
        connections.comment_parent(self.node).map(|n| {
            self.document.wrap_parent_of_child(n)
        })
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

node!(ProcessingInstruction, raw::ProcessingInstruction);

impl<'d> ProcessingInstruction<'d> {
    pub fn target(&self) -> &str { self.node().target() }
    pub fn value(&self) -> Option<&str> { self.node().value() }

    pub fn set_target(&self, new_target: &str) {
        self.document.storage.processing_instruction_set_target(self.node, new_target);
    }

    pub fn set_value(&self, new_value: Option<&str>) {
        self.document.storage.processing_instruction_set_value(self.node, new_value);
    }

    pub fn parent(&self) -> Option<ParentOfChild<'d>> {
        let connections = self.document.connections.borrow();
        connections.processing_instruction_parent(self.node).map(|n| {
            self.document.wrap_parent_of_child(n)
        })
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
        impl<'d> $enum_name<'d> {
            pub fn $name(self) -> Option<$inner<'d>> {
                match self {
                    $enum_name::$wrapper(n) => Some(n),
                    _ => None,
                }
            }
        }
    )
);

#[derive(PartialEq,Debug,Copy)]
pub enum ChildOfRoot<'d> {
    Element(Element<'d>),
    Comment(Comment<'d>),
    ProcessingInstruction(ProcessingInstruction<'d>),
}

unpack!(ChildOfRoot, element, Element, Element);
unpack!(ChildOfRoot, comment, Comment, Comment);
unpack!(ChildOfRoot, processing_instruction, ProcessingInstruction, ProcessingInstruction);

impl<'d> ChildOfRoot<'d> {
    pub fn as_raw(&self) -> raw::ChildOfRoot {
        match self {
            &ChildOfRoot::Element(n) => raw::ChildOfRoot::Element(n.node),
            &ChildOfRoot::Comment(n) => raw::ChildOfRoot::Comment(n.node),
            &ChildOfRoot::ProcessingInstruction(n) => raw::ChildOfRoot::ProcessingInstruction(n.node),
        }
    }
}

#[derive(PartialEq,Debug,Copy)]
pub enum ChildOfElement<'d> {
    Element(Element<'d>),
    Text(Text<'d>),
    Comment(Comment<'d>),
    ProcessingInstruction(ProcessingInstruction<'d>),
}

unpack!(ChildOfElement, element, Element, Element);
unpack!(ChildOfElement, text, Text, Text);
unpack!(ChildOfElement, comment, Comment, Comment);
unpack!(ChildOfElement, processing_instruction, ProcessingInstruction, ProcessingInstruction);

impl<'d> ChildOfElement<'d> {
    pub fn as_raw(&self) -> raw::ChildOfElement {
        match self {
            &ChildOfElement::Element(n) => raw::ChildOfElement::Element(n.node),
            &ChildOfElement::Text(n) => raw::ChildOfElement::Text(n.node),
            &ChildOfElement::Comment(n) => raw::ChildOfElement::Comment(n.node),
            &ChildOfElement::ProcessingInstruction(n) => raw::ChildOfElement::ProcessingInstruction(n.node),
        }
    }
}

#[derive(PartialEq,Debug,Copy)]
pub enum ParentOfChild<'d> {
    Root(Root<'d>),
    Element(Element<'d>),
}

macro_rules! conversion_trait(
    ($tr_name:ident, $method:ident, $res_type:ident,
        { $($leaf_type:ident => $variant:expr),* }
    ) => (
        pub trait $tr_name<'d> {
            fn $method(self) -> $res_type<'d>;
        }

        impl<'d> $tr_name<'d> for $res_type<'d> {
            fn $method(self) -> $res_type<'d> {
                self
            }
        }

        $(impl<'d> $tr_name<'d> for $leaf_type<'d> {
            fn $method(self) -> $res_type<'d> {
                $variant(self)
            }
        })*
    )
);

conversion_trait!(ToChildOfRoot, to_child_of_root, ChildOfRoot, {
    Element => ChildOfRoot::Element,
    Comment => ChildOfRoot::Comment,
    ProcessingInstruction => ChildOfRoot::ProcessingInstruction
});

conversion_trait!(ToChildOfElement, to_child_of_element, ChildOfElement, {
    Element => ChildOfElement::Element,
    Text => ChildOfElement::Text,
    Comment => ChildOfElement::Comment,
    ProcessingInstruction => ChildOfElement::ProcessingInstruction
});

impl<'d> ToChildOfElement<'d> for ChildOfRoot<'d> {
    fn to_child_of_element(self) -> ChildOfElement<'d> {
        match self {
            ChildOfRoot::Element(n) => ChildOfElement::Element(n),
            ChildOfRoot::Comment(n) => ChildOfElement::Comment(n),
            ChildOfRoot::ProcessingInstruction(n) => ChildOfElement::ProcessingInstruction(n),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::{Package,ToQName};
    use super::{ChildOfRoot,ChildOfElement,ParentOfChild};

    macro_rules! assert_qname_eq(
        ($l:expr, $r:expr) => (assert_eq!($l.to_qname(), $r.to_qname()));
    );

    #[test]
    fn the_root_belongs_to_a_document() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();

        assert_eq!(&doc, root.document());
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

        assert_eq!(&doc, element.document());
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
    fn attributes_belong_to_a_document() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("alpha");
        let attr = element.set_attribute_value("hello", "world");

        assert_eq!(&doc, attr.document());
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
    fn attributes_can_be_iterated() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");

        element.set_attribute_value("name1", "value1");
        element.set_attribute_value("name2", "value2");

        let mut attrs = element.attributes();
        attrs.sort_by(|a, b| a.name().cmp(&b.name()));

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

        assert_eq!(&doc, text.document());
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
    fn text_knows_its_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let sentence = doc.create_element("sentence");
        let text = doc.create_text("Now is the winter of our discontent.");

        sentence.append_child(text);

        assert_eq!(text.parent(), Some(sentence));
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

        assert_eq!(&doc, comment.document());
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

        assert_eq!(&doc, pi.document());
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

    // #[test]
    // #[compile_failure]
    // fn nodes_cannot_live_outside_of_the_document() {
    //     let package = Package::new();

    //     let _ = {
    //         let doc = package.as_document();

    //         doc.create_element("hello")
    //     };
    // }
}
