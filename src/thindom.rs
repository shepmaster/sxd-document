use std::marker::PhantomData;
use std::{fmt,hash,slice};

use super::QName;
use super::raw;

pub struct Storage<'d> {
    storage: &'d raw::Storage,
}

impl<'d> Storage<'d> {
    pub fn new(storage: &raw::Storage) -> Storage {
        Storage {
            storage: storage,
        }
    }

    pub fn create_element<'n, N>(&'d self, name: N) -> Element<'d>
        where N: Into<QName<'n>>
    {
        Element::wrap(self.storage.create_element(name))
    }

    pub fn create_attribute<'n, N>(&'d self, name: N, value: &str) -> Attribute<'d>
        where N: Into<QName<'n>>
    {
        Attribute::wrap(self.storage.create_attribute(name, value))
    }

    pub fn create_text(&'d self, text: &str) -> Text<'d> {
        Text::wrap(self.storage.create_text(text))
    }

    pub fn create_comment(&'d self, text: &str) -> Comment<'d> {
        Comment::wrap(self.storage.create_comment(text))
    }

    pub fn create_processing_instruction(&'d self, target: &str, value: Option<&str>) -> ProcessingInstruction<'d> {
        ProcessingInstruction::wrap(self.storage.create_processing_instruction(target, value))
    }

    pub fn element_set_name<'n, N>(&self, element: &Element, name: N)
        where N: Into<QName<'n>>
    {
        self.storage.element_set_name(element.node, name)
    }

    pub fn text_set_text(&self, text: &Text, new_text: &str) {
        self.storage.text_set_text(text.node, new_text)
    }

    pub fn comment_set_text(&self, comment: &Comment, new_text: &str) {
        self.storage.comment_set_text(comment.node, new_text)
    }

    pub fn processing_instruction_set_target(&self, pi: &ProcessingInstruction, new_target: &str) {
        self.storage.processing_instruction_set_target(pi.node, new_target)
    }

    pub fn processing_instruction_set_value(&self, pi: &ProcessingInstruction, new_value: Option<&str>) {
        self.storage.processing_instruction_set_value(pi.node, new_value)
    }
}

pub struct Connections<'d> {
    connections: &'d raw::Connections,
}

impl<'d> Connections<'d> {
    pub fn new(connections: &raw::Connections) -> Connections {
        Connections {
            connections: connections,
        }
    }

    pub fn root(&self) -> Root<'d> {
        Root::wrap(self.connections.root())
    }

    pub fn element_parent(&self, child: Element<'d>) -> Option<ParentOfChild<'d>> {
        self.connections.element_parent(child.node).map(|n| {
            ParentOfChild::wrap(n)
        })
    }

    pub fn text_parent(&self, child: Text<'d>) -> Option<Element<'d>> {
        self.connections.text_parent(child.node).map(|n| {
            Element::wrap(n)
        })
    }

    pub fn comment_parent(&self, child: Comment<'d>) -> Option<ParentOfChild<'d>> {
        self.connections.comment_parent(child.node).map(|n| {
            ParentOfChild::wrap(n)
        })
    }

    pub fn processing_instruction_parent(&self, child: ProcessingInstruction<'d>) -> Option<ParentOfChild<'d>> {
        self.connections.processing_instruction_parent(child.node).map(|n| {
            ParentOfChild::wrap(n)
        })
    }

    pub fn append_root_child<C>(&mut self, child: C)
        where C: Into<ChildOfRoot<'d>>
    {
        let child = child.into();
        self.connections.append_root_child(child.as_raw())
    }

    pub fn append_element_child<C>(&mut self, parent: Element<'d>, child: C)
        where C: Into<ChildOfElement<'d>>
    {
        let child = child.into();
        self.connections.append_element_child(parent.node, child.as_raw())
    }

    pub fn root_children(&self) -> RootChildren {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { RootChildren { iter: self.connections.root_children().iter() } }
    }

    pub fn element_children(&self, parent: Element) -> ElementChildren {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { ElementChildren { iter: self.connections.element_children(parent.node).iter() } }
    }

    pub fn element_preceding_siblings(&self, element: Element) -> Siblings {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { Siblings { iter: self.connections.element_preceding_siblings(element.node) } }
    }

    pub fn element_following_siblings(&self, element: Element) -> Siblings {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { Siblings { iter: self.connections.element_following_siblings(element.node) } }
    }

    pub fn text_preceding_siblings(&self, text: Text) -> Siblings {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { Siblings { iter: self.connections.text_preceding_siblings(text.node) } }
    }

    pub fn text_following_siblings(&self, text: Text) -> Siblings {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { Siblings { iter: self.connections.text_following_siblings(text.node) } }
    }

    pub fn comment_preceding_siblings(&self, comment: Comment) -> Siblings {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { Siblings { iter: self.connections.comment_preceding_siblings(comment.node) } }
    }

    pub fn comment_following_siblings(&self, comment: Comment) -> Siblings {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { Siblings { iter: self.connections.comment_following_siblings(comment.node) } }
    }

    pub fn processing_instruction_preceding_siblings(&self, pi: ProcessingInstruction) -> Siblings {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { Siblings { iter: self.connections.processing_instruction_preceding_siblings(pi.node) } }
    }

    pub fn processing_instruction_following_siblings(&self, pi: ProcessingInstruction) -> Siblings {
        // This is safe because we disallow mutation while this borrow is active.
        unsafe { Siblings { iter: self.connections.processing_instruction_following_siblings(pi.node) } }
    }

    pub fn attribute_parent(&self, attribute: Attribute<'d>) -> Option<Element<'d>> {
        self.connections.attribute_parent(attribute.node).map(Element::wrap)
    }

    pub fn attributes(&self, parent: Element<'d>) -> Attributes<'d> {
        // This is safe because we disallow mutation while this borrow is active
        // TODO: Test that
        unsafe { Attributes { iter: self.connections.attributes(parent.node).iter() } }
    }

    pub fn set_attribute(&mut self, parent: Element<'d>, attribute: Attribute<'d>) {
        self.connections.set_attribute(parent.node, attribute.node);
    }

    pub fn attribute_value(&self, parent: Element<'d>, name: &str) -> Option<&'d str> {
        self.connections.attribute(parent.node, name).map(|a| {
            let a_r = unsafe { &*a };
            a_r.value()
        })
    }
}

pub struct RootChildren<'d> {
    iter: slice::Iter<'d, raw::ChildOfRoot>,
}

impl<'d> Iterator for RootChildren<'d> {
    type Item = ChildOfRoot<'d>;

    fn next(&mut self) -> Option<ChildOfRoot<'d>> {
        self.iter.next().map(|&c| ChildOfRoot::wrap(c))
    }
}

pub struct ElementChildren<'d> {
    iter: slice::Iter<'d, raw::ChildOfElement>,
}

impl<'d> Iterator for ElementChildren<'d> {
    type Item = ChildOfElement<'d>;

    fn next(&mut self) -> Option<ChildOfElement<'d>> {
        self.iter.next().map(|&c| ChildOfElement::wrap(c))
    }
}

pub struct Attributes<'d> {
    iter: slice::Iter<'d, *mut raw::Attribute>,
}

impl<'d> Iterator for Attributes<'d> {
    type Item = Attribute<'d>;

    fn next(&mut self) -> Option<Attribute<'d>> {
        self.iter.next().map(|&a| Attribute::wrap(a))
    }
}

pub struct Siblings<'d> {
    iter: raw::SiblingIter<'d>,
}

impl<'d> Iterator for Siblings<'d> {
    type Item = ChildOfElement<'d>;

    fn next(&mut self) -> Option<ChildOfElement<'d>> {
        self.iter.next().map(ChildOfElement::wrap)
    }
}

macro_rules! node(
    ($name:ident, $raw:ty) => (
        #[derive(Copy,Clone)]
        pub struct $name<'d> {
            node: *mut $raw,
            lifetime: PhantomData<Storage<'d>>,
        }

        impl<'d> $name<'d> {
            fn wrap(node: *mut $raw) -> $name<'d> {
                $name {
                    node: node,
                    lifetime: PhantomData,
                }
            }

            #[allow(dead_code)]
            fn node(&self) -> &'d $raw { unsafe { &*self.node } }
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

node!(Root, raw::Root);

impl<'d> fmt::Debug for Root<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Root")
    }
}

node!(Element, raw::Element);

impl<'d> Element<'d> {
    pub fn name(&self) -> QName<'d> { self.node().name() }
}

impl<'d> fmt::Debug for Element<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Element {{ name: {:?} }}", self.name())
    }
}

node!(Attribute, raw::Attribute);

impl<'d> Attribute<'d> {
    pub fn name(&self)  -> QName { self.node().name() }
    pub fn value(&self) -> &str { self.node().value() }
}

impl<'d> fmt::Debug for Attribute<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Attribute {{ name: {:?}, value: {:?} }}", self.name(), self.value())
    }
}

node!(Text, raw::Text);

impl<'d> Text<'d> {
    pub fn text(&self) -> &str { self.node().text() }
}

impl<'d> fmt::Debug for Text<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Text {{ text: {:?} }}", self.text())
    }
}

node!(Comment, raw::Comment);

impl<'d> Comment<'d> {
    pub fn text(&self) -> &str { self.node().text() }
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

    pub fn wrap(node: raw::ChildOfRoot) -> ChildOfRoot<'d> {
        match node {
            raw::ChildOfRoot::Element(n)               => ChildOfRoot::Element(Element::wrap(n)),
            raw::ChildOfRoot::Comment(n)               => ChildOfRoot::Comment(Comment::wrap(n)),
            raw::ChildOfRoot::ProcessingInstruction(n) => ChildOfRoot::ProcessingInstruction(ProcessingInstruction::wrap(n)),
        }
    }

    pub fn as_raw(&self) -> raw::ChildOfRoot {
        match *self {
            ChildOfRoot::Element(n)               => raw::ChildOfRoot::Element(n.node),
            ChildOfRoot::Comment(n)               => raw::ChildOfRoot::Comment(n.node),
            ChildOfRoot::ProcessingInstruction(n) => raw::ChildOfRoot::ProcessingInstruction(n.node),
        }
    }
}

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

    pub fn wrap(node: raw::ChildOfElement) -> ChildOfElement<'d> {
        match node {
            raw::ChildOfElement::Element(n)               => ChildOfElement::Element(Element::wrap(n)),
            raw::ChildOfElement::Text(n)                  => ChildOfElement::Text(Text::wrap(n)),
            raw::ChildOfElement::Comment(n)               => ChildOfElement::Comment(Comment::wrap(n)),
            raw::ChildOfElement::ProcessingInstruction(n) => ChildOfElement::ProcessingInstruction(ProcessingInstruction::wrap(n)),
        }
    }

    pub fn as_raw(&self) -> raw::ChildOfElement {
        match *self {
            ChildOfElement::Element(n)               => raw::ChildOfElement::Element(n.node),
            ChildOfElement::Text(n)                  => raw::ChildOfElement::Text(n.node),
            ChildOfElement::Comment(n)               => raw::ChildOfElement::Comment(n.node),
            ChildOfElement::ProcessingInstruction(n) => raw::ChildOfElement::ProcessingInstruction(n.node),
        }
    }
}

#[derive(Debug,Copy,Clone,PartialEq)]
pub enum ParentOfChild<'d> {
    Root(Root<'d>),
    Element(Element<'d>),
}

impl<'d> ParentOfChild<'d> {
    unpack!(ParentOfChild, root, Root, Root);
    unpack!(ParentOfChild, element, Element, Element);

    pub fn wrap(node: raw::ParentOfChild) -> ParentOfChild<'d> {
        match node {
            raw::ParentOfChild::Root(n) => ParentOfChild::Root(Root::wrap(n)),
            raw::ParentOfChild::Element(n) => ParentOfChild::Element(Element::wrap(n)),
        }
    }
}

macro_rules! conversion_trait(
    ($res_type:ident, {
        $($leaf_type:ident => $variant:expr),*
    }) => (
        $(impl<'d> Into<$res_type<'d>> for $leaf_type<'d> {
            fn into(self) -> $res_type<'d> {
                $variant(self)
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

impl<'d> Into<ChildOfElement<'d>> for ChildOfRoot<'d> {
    fn into(self) -> ChildOfElement<'d> {
        match self {
            ChildOfRoot::Element(n)               => ChildOfElement::Element(n),
            ChildOfRoot::Comment(n)               => ChildOfElement::Comment(n),
            ChildOfRoot::ProcessingInstruction(n) => ChildOfElement::ProcessingInstruction(n),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::{Package,QName};
    use super::{ChildOfRoot,ChildOfElement,ParentOfChild};
    use super::Attribute;

    macro_rules! assert_qname_eq(
        ($l:expr, $r:expr) => (assert_eq!(Into::<QName>::into($l), $r.into()));
    );

    #[test]
    fn root_can_have_element_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element = s.create_element("alpha");

        c.append_root_child(element);

        let children: Vec<ChildOfRoot> = c.root_children().collect();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfRoot::Element(element));
    }

    #[test]
    fn root_has_maximum_of_one_element_child() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let alpha = s.create_element("alpha");
        let beta = s.create_element("beta");

        c.append_root_child(alpha);
        c.append_root_child(beta);

        let children: Vec<ChildOfRoot> = c.root_children().collect();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfRoot::Element(beta));
    }

    #[test]
    fn root_can_have_comment_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let comment = s.create_comment("Now is the winter of our discontent.");

        c.append_root_child(comment);

        let children: Vec<ChildOfRoot> = c.root_children().collect();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfRoot::Comment(comment));
    }

    #[test]
    fn root_can_have_processing_instruction_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let pi = s.create_processing_instruction("device", None);

        c.append_root_child(pi);

        let children: Vec<ChildOfRoot> = c.root_children().collect();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfRoot::ProcessingInstruction(pi));
    }

    #[test]
    fn root_child_knows_its_parent() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let alpha = s.create_element("alpha");

        c.append_root_child(alpha);

        assert_eq!(Some(ParentOfChild::Root(c.root())), c.element_parent(alpha));
    }

    #[test]
    fn elements_can_have_element_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let alpha = s.create_element("alpha");
        let beta  = s.create_element("beta");

        c.append_element_child(alpha, beta);

        let children: Vec<ChildOfElement> = c.element_children(alpha).collect();

        assert_eq!(children[0], ChildOfElement::Element(beta));
    }

    #[test]
    fn element_children_are_ordered() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let greek = s.create_element("greek");
        let alpha = s.create_element("alpha");
        let omega = s.create_element("omega");

        c.append_element_child(greek, alpha);
        c.append_element_child(greek, omega);

        let children: Vec<ChildOfElement> = c.element_children(greek).collect();

        assert_eq!(children[0], ChildOfElement::Element(alpha));
        assert_eq!(children[1], ChildOfElement::Element(omega));
    }

    #[test]
    fn element_children_know_their_parent() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let alpha = s.create_element("alpha");
        let beta  = s.create_element("beta");

        c.append_element_child(alpha, beta);

        assert_eq!(Some(ParentOfChild::Element(alpha)), c.element_parent(beta));
    }

    #[test]
    fn elements_know_preceding_siblings() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let parent = s.create_element("parent");
        let a = s.create_element("a");
        let b = s.create_element("b");

        c.append_element_child(parent, a);
        c.append_element_child(parent, b);

        let preceding: Vec<_> = c.element_preceding_siblings(b).collect();

        assert_eq!(vec![ChildOfElement::Element(a)], preceding);
    }

    #[test]
    fn changing_parent_of_element_removes_element_from_original_parent() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let parent1 = s.create_element("parent1");
        let parent2 = s.create_element("parent2");
        let child = s.create_element("child");

        c.append_element_child(parent1, child);
        c.append_element_child(parent2, child);

        assert_eq!(0, c.element_children(parent1).count());
        assert_eq!(1, c.element_children(parent2).count());
    }

    #[test]
    fn elements_can_be_renamed() {
        let package = Package::new();
        let (s, _) = package.as_thin_document();

        let alpha = s.create_element("alpha");
        s.element_set_name(&alpha, "beta");
        assert_qname_eq!(alpha.name(), "beta");
    }

    #[test]
    fn elements_have_attributes() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element = s.create_element("element");
        let attr = s.create_attribute("hello", "world");
        c.set_attribute(element, attr);

        assert_eq!(Some("world"), c.attribute_value(element, "hello"));
    }

    #[test]
    fn attributes_know_their_element() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element = s.create_element("element");
        let attr = s.create_attribute("hello", "world");
        c.set_attribute(element, attr);

        assert_eq!(Some(element), c.attribute_parent(attr));
    }

    #[test]
    fn attributes_belong_to_one_element() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element1 = s.create_element("element1");
        let element2 = s.create_element("element2");
        let attr = s.create_attribute("hello", "world");

        c.set_attribute(element1, attr);
        c.set_attribute(element2, attr);

        assert_eq!(0, c.attributes(element1).count());
        assert_eq!(1, c.attributes(element2).count());
    }

    #[test]
    fn attributes_can_be_reset() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element = s.create_element("element");

        let attr1 = s.create_attribute("hello", "world");
        let attr2 = s.create_attribute("hello", "galaxy");

        c.set_attribute(element, attr1);
        c.set_attribute(element, attr2);

        assert_eq!(Some("galaxy"), c.attribute_value(element, "hello"));
    }

    #[test]
    fn attributes_can_be_iterated() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element = s.create_element("element");

        let attr1 = s.create_attribute("name1", "value1");
        let attr2 = s.create_attribute("name2", "value2");

        c.set_attribute(element, attr1);
        c.set_attribute(element, attr2);

        let mut attrs: Vec<Attribute> = c.attributes(element).collect();
        attrs.sort_by(|a, b| a.name().namespace_uri().cmp(&b.name().namespace_uri()));

        assert_eq!(2, attrs.len());
        assert_qname_eq!("name1",  attrs[0].name());
        assert_eq!("value1", attrs[0].value());
        assert_qname_eq!("name2",  attrs[1].name());
        assert_eq!("value2", attrs[1].value());
    }

    #[test]
    fn elements_can_have_text_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let sentence = s.create_element("sentence");
        let text = s.create_text("Now is the winter of our discontent.");

        c.append_element_child(sentence, text);

        let children: Vec<ChildOfElement> = c.element_children(sentence).collect();

        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfElement::Text(text));
    }

    #[test]
    fn text_knows_its_parent() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let sentence = s.create_element("sentence");
        let text = s.create_text("Now is the winter of our discontent.");

        c.append_element_child(sentence, text);

        assert_eq!(c.text_parent(text), Some(sentence));
    }

    #[test]
    fn text_can_be_changed() {
        let package = Package::new();
        let (s, _) = package.as_thin_document();

        let text = s.create_text("Now is the winter of our discontent.");

        s.text_set_text(&text, "Made glorious summer by this sun of York");

        assert_eq!(text.text(), "Made glorious summer by this sun of York");
    }

    #[test]
    fn elements_can_have_comment_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let sentence = s.create_element("sentence");
        let comment = s.create_comment("Now is the winter of our discontent.");

        c.append_element_child(sentence, comment);

        let children: Vec<ChildOfElement> = c.element_children(sentence).collect();

        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfElement::Comment(comment));
    }

    #[test]
    fn comment_knows_its_parent() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let sentence = s.create_element("sentence");
        let comment = s.create_comment("Now is the winter of our discontent.");

        c.append_element_child(sentence, comment);

        assert_eq!(c.comment_parent(comment), Some(ParentOfChild::Element(sentence)));
    }

    #[test]
    fn comment_can_be_changed() {
        let package = Package::new();
        let (s, _) = package.as_thin_document();

        let comment = s.create_comment("Now is the winter of our discontent.");

        s.comment_set_text(&comment, "Made glorious summer by this sun of York");

        assert_eq!(comment.text(), "Made glorious summer by this sun of York");
    }

    #[test]
    fn elements_can_have_processing_instruction_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element = s.create_element("element");
        let pi = s.create_processing_instruction("device", None);

        c.append_element_child(element, pi);

        let children: Vec<ChildOfElement> = c.element_children(element).collect();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ChildOfElement::ProcessingInstruction(pi));
    }

    #[test]
    fn processing_instruction_knows_its_parent() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element = s.create_element("element");
        let pi = s.create_processing_instruction("device", None);

        c.append_element_child(element, pi);

        assert_eq!(c.processing_instruction_parent(pi), Some(ParentOfChild::Element(element)));
    }

    #[test]
    fn processing_instruction_can_be_changed() {
        let package = Package::new();
        let (s, _) = package.as_thin_document();

        let pi = s.create_processing_instruction("device", None);

        s.processing_instruction_set_target(&pi, "output");
        s.processing_instruction_set_value(&pi, Some("full-screen"));

        assert_eq!(pi.target(), "output");
        assert_eq!(pi.value(), Some("full-screen"));
    }

    #[test]
    fn can_return_a_populated_package() {
        fn populate() -> Package {
            let package = Package::new();
            {
                let (s, mut c) = package.as_thin_document();

                let element = s.create_element("hello");
                c.append_root_child(element);
            }

            package
        }

        let package = populate();
        let (_, c) = package.as_thin_document();
        let children: Vec<_> = c.root_children().collect();
        let element = children[0].element().unwrap();
        assert_qname_eq!(element.name(), "hello");
    }

    #[test]
    #[cfg(feature = "compile_failure")]
    //cannot borrow `c` as mutable because it is also borrowed as immutable
    fn cannot_mutate_connections_while_iterating_over_root_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let alpha = s.create_element("alpha");

        for _ in c.root_children() {
            c.append_root_child(alpha);
        }
    }

    #[test]
    #[cfg(feature = "compile_failure")]
    //cannot borrow `c` as mutable because it is also borrowed as immutable
    fn cannot_mutate_connections_while_iterating_over_element_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let alpha = s.create_element("alpha");
        let beta  = s.create_element("beta");

        for _ in c.element_children(alpha) {
            c.append_element_child(alpha, beta);
        }
    }

    #[test]
    #[cfg(feature = "compile_failure")]
    //cannot borrow `c` as mutable because it is also borrowed as immutable
    fn cannot_mutate_connections_while_iterating_over_siblings() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let alpha = s.create_element("alpha");
        let beta  = s.create_element("beta");

        for _ in c.element_preceding_siblings(alpha) {
            c.append_element_child(alpha, beta);
        }
    }

    #[test]
    #[cfg(feature = "compile_failure")]
    //`s` does not live long enough
    fn nodes_cannot_live_outside_of_the_document() {
        let package = Package::new();

        let _ = {
            let (s, _) = package.as_thin_document();

            s.create_element("hello")
        };
    }
}
