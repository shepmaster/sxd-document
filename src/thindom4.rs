use super::raw;
use std::fmt;
use std::kinds::marker::InvariantLifetime;

pub struct Storage<'d> {
    storage: &'d raw::Storage,
}

impl<'d> Storage<'d> {
    pub fn new(storage: &raw::Storage) -> Storage {
        Storage {
            storage: storage,
        }
    }

    pub fn create_element(&'d self, name: &str) -> Element<'d> {
        Element::wrap(self.storage.create_element(name))
    }

    pub fn create_attribute(&'d self, name: &str, value: &str) -> Attribute<'d> {
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

    pub fn element_set_name(&self, element: &Element, name: &str) {
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

    pub fn append_root_child<C : ToChildOfRoot<'d>>(&mut self, child: C) {
        let child = child.to_child_of_root();
        self.connections.append_root_child(child.as_raw())
    }

    pub fn append_element_child<C : ToChildOfElement<'d>>(&mut self, parent: Element<'d>, child: C) {
        let child = child.to_child_of_element();
        self.connections.append_element_child(parent.node, child.as_raw())
    }

    pub fn root_children(&self) -> RootChildren<'d> {
        // This is safe because we disallow mutation while this borrow is active.
        // TODO: Test that
        unsafe { RootChildren { x: self.connections.root_children(), idx: 0 } }
    }

    pub fn element_children(&self, parent: Element<'d>) -> ElementChildren<'d> {
        // This is safe because we disallow mutation while this borrow is active.
        // TODO: Test that
        unsafe { ElementChildren { x: self.connections.element_children(parent.node), idx: 0 } }
    }

    pub fn attribute_parent(&self, attribute: Attribute<'d>) -> Option<Element<'d>> {
        self.connections.attribute_parent(attribute.node).map(|a| Element::wrap(a))
    }

    pub fn attributes(&self, parent: Element<'d>) -> Attributes<'d> {
        // This is safe because we disallow mutation while this borrow is active
        // TODO: Test that
        unsafe { Attributes { x: self.connections.attributes(parent.node), idx: 0 } }
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
    x: &'d [raw::ChildOfRoot],
    idx: uint,
}

impl<'d> Iterator<ChildOfRoot<'d>> for RootChildren<'d> {
    fn next(&mut self) -> Option<ChildOfRoot<'d>> {
        if self.idx >= self.x.len() {
            None
        } else {
            let c = ChildOfRoot::wrap(self.x[self.idx]);
            self.idx += 1;
            Some(c)
        }
    }
}

pub struct ElementChildren<'d> {
    x: &'d [raw::ChildOfElement],
    idx: uint,
}

impl<'d> Iterator<ChildOfElement<'d>> for ElementChildren<'d> {
    fn next(&mut self) -> Option<ChildOfElement<'d>> {
        if self.idx >= self.x.len() {
            None
        } else {
            let c = ChildOfElement::wrap(self.x[self.idx]);
            self.idx += 1;
            Some(c)
        }
    }
}

pub struct Attributes<'d> {
    x: &'d [*mut raw::Attribute],
    idx: uint,
}

impl<'d> Iterator<Attribute<'d>> for Attributes<'d> {
    fn next(&mut self) -> Option<Attribute<'d>> {
        if self.idx >= self.x.len() {
            None
        } else {
            let a = Attribute::wrap(self.x[self.idx]);
            self.idx += 1;
            Some(a)
        }
    }
}

macro_rules! node(
    ($name:ident, $raw:ty) => (
        pub struct $name<'d> {
            lifetime: InvariantLifetime<'d>,
            node: *mut $raw,
        }

        impl<'d> $name<'d> {
            fn wrap(node: *mut $raw) -> $name<'d> {
                $name {
                    lifetime: InvariantLifetime,
                    node: node,
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
    )
)

node!(Root, raw::Root)

impl<'d> fmt::Show for Root<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Root")
    }
}

node!(Element, raw::Element)

impl<'d> Element<'d> {
    pub fn name(&self) -> &'d str { self.node().name() }
}

impl<'d> fmt::Show for Element<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Element {{ name: {} }}", self.name())
    }
}

node!(Attribute, raw::Attribute)

impl<'d> Attribute<'d> {
    pub fn name(&self)  -> &str { self.node().name() }
    pub fn value(&self) -> &str { self.node().value() }
}

node!(Text, raw::Text)

impl<'d> Text<'d> {
    pub fn text(&self) -> &str { self.node().text() }
}

impl<'d> fmt::Show for Text<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Text {{ text: {} }}", self.text())
    }
}

node!(Comment, raw::Comment)

impl<'d> Comment<'d> {
    pub fn text(&self) -> &str { self.node().text() }
}

impl<'d> fmt::Show for Comment<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Comment {{ text: {} }}", self.text())
    }
}

node!(ProcessingInstruction, raw::ProcessingInstruction)

impl<'d> ProcessingInstruction<'d> {
    pub fn target(&self) -> &str { self.node().target() }
    pub fn value(&self) -> Option<&str> { self.node().value() }
}

impl<'d> fmt::Show for ProcessingInstruction<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ProcessingInstruction {{ target: {}, value: {} }}", self.target(), self.value())
    }
}

macro_rules! unpack(
    ($enum_name:ident, $name:ident, $wrapper:ident, $inner:ident) => (
        impl<'d> $enum_name<'d> {
            pub fn $name(self) -> Option<$inner<'d>> {
                match self {
                    $wrapper(n) => Some(n),
                    _ => None,
                }
            }
        }
    )
)

#[deriving(PartialEq,Show)]
pub enum ChildOfRoot<'d> {
    ElementCOR(Element<'d>),
    CommentCOR(Comment<'d>),
    ProcessingInstructionCOR(ProcessingInstruction<'d>),
}

unpack!(ChildOfRoot, element, ElementCOR, Element)
unpack!(ChildOfRoot, comment, CommentCOR, Comment)
unpack!(ChildOfRoot, processing_instruction, ProcessingInstructionCOR, ProcessingInstruction)

impl<'d> ChildOfRoot<'d> {
    pub fn wrap(node: raw::ChildOfRoot) -> ChildOfRoot<'d> {
        match node {
            raw::ElementCOR(n) => ElementCOR(Element::wrap(n)),
            raw::CommentCOR(n) => CommentCOR(Comment::wrap(n)),
            raw::ProcessingInstructionCOR(n) => ProcessingInstructionCOR(ProcessingInstruction::wrap(n)),
        }
    }

    pub fn as_raw(&self) -> raw::ChildOfRoot {
        match self {
            &ElementCOR(n) => raw::ElementCOR(n.node),
            &CommentCOR(n) => raw::CommentCOR(n.node),
            &ProcessingInstructionCOR(n) => raw::ProcessingInstructionCOR(n.node),
        }
    }
}

#[deriving(PartialEq,Show)]
pub enum ChildOfElement<'d> {
    ElementCOE(Element<'d>),
    TextCOE(Text<'d>),
    CommentCOE(Comment<'d>),
    ProcessingInstructionCOE(ProcessingInstruction<'d>),
}

unpack!(ChildOfElement, element, ElementCOE, Element)
unpack!(ChildOfElement, text, TextCOE, Text)
unpack!(ChildOfElement, comment, CommentCOE, Comment)
unpack!(ChildOfElement, processing_instruction, ProcessingInstructionCOE, ProcessingInstruction)

impl<'d> ChildOfElement<'d> {
    pub fn wrap(node: raw::ChildOfElement) -> ChildOfElement<'d> {
        match node {
            raw::ElementCOE(n) => ElementCOE(Element::wrap(n)),
            raw::TextCOE(n) => TextCOE(Text::wrap(n)),
            raw::CommentCOE(n) => CommentCOE(Comment::wrap(n)),
            raw::ProcessingInstructionCOE(n) => ProcessingInstructionCOE(ProcessingInstruction::wrap(n)),
        }
    }

    pub fn as_raw(&self) -> raw::ChildOfElement {
        match self {
            &ElementCOE(n) => raw::ElementCOE(n.node),
            &TextCOE(n) => raw::TextCOE(n.node),
            &CommentCOE(n) => raw::CommentCOE(n.node),
            &ProcessingInstructionCOE(n) => raw::ProcessingInstructionCOE(n.node),
        }
    }
}

#[deriving(PartialEq,Show)]
pub enum ParentOfChild<'d> {
    RootPOC(Root<'d>),
    ElementPOC(Element<'d>),
}

unpack!(ParentOfChild, root, RootPOC, Root)
unpack!(ParentOfChild, element, ElementPOC, Element)

impl<'d> ParentOfChild<'d> {
    pub fn wrap(node: raw::ParentOfChild) -> ParentOfChild<'d> {
        match node {
            raw::RootPOC(n) => RootPOC(Root::wrap(n)),
            raw::ElementPOC(n) => ElementPOC(Element::wrap(n)),
        }
    }
}

macro_rules! conversion_trait(
    ($tr_name:ident, $method:ident, $res_type:ident,
        { $($leaf_type:ident => $variant:ident),* }
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
)

conversion_trait!(ToChildOfRoot, to_child_of_root, ChildOfRoot, {
    Element => ElementCOR,
    Comment => CommentCOR,
    ProcessingInstruction => ProcessingInstructionCOR
})

conversion_trait!(ToChildOfElement, to_child_of_element, ChildOfElement, {
    Element => ElementCOE,
    Text => TextCOE,
    Comment => CommentCOE,
    ProcessingInstruction => ProcessingInstructionCOE
})

#[cfg(test)]
mod test {
    use super::super::Package;
    use super::{ChildOfRoot,ElementCOR,CommentCOR,ProcessingInstructionCOR};
    use super::{ChildOfElement,ElementCOE,TextCOE,CommentCOE,ProcessingInstructionCOE};
    use super::{ElementPOC};
    use super::Attribute;

    #[test]
    fn root_can_have_element_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element = s.create_element("alpha");

        c.append_root_child(element);

        let children: Vec<ChildOfRoot> = c.root_children().collect();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ElementCOR(element));
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
        assert_eq!(children[0], ElementCOR(beta));
    }

    #[test]
    fn root_can_have_comment_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let comment = s.create_comment("Now is the winter of our discontent.");

        c.append_root_child(comment);

        let children: Vec<ChildOfRoot> = c.root_children().collect();
        assert_eq!(1, children.len());
        assert_eq!(children[0], CommentCOR(comment));
    }

    #[test]
    fn root_can_have_processing_instruction_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let pi = s.create_processing_instruction("device", None);

        c.append_root_child(pi);

        let children: Vec<ChildOfRoot> = c.root_children().collect();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ProcessingInstructionCOR(pi));
    }

    #[test]
    fn elements_can_have_element_children() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let alpha = s.create_element("alpha");
        let beta  = s.create_element("beta");

        c.append_element_child(alpha, beta);

        let children: Vec<ChildOfElement> = c.element_children(alpha).collect();

        assert_eq!(children[0], ElementCOE(beta));
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

        assert_eq!(children[0], ElementCOE(alpha));
        assert_eq!(children[1], ElementCOE(omega));
    }

    #[test]
    fn element_children_know_their_parent() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let alpha = s.create_element("alpha");
        let beta  = s.create_element("beta");

        c.append_element_child(alpha, beta);

        assert_eq!(Some(ElementPOC(alpha)), c.element_parent(beta));
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
        assert_eq!(alpha.name(), "beta");
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
        attrs.sort_by(|a, b| a.name().cmp(b.name()));

        assert_eq!(2, attrs.len());
        assert_eq!("name1",  attrs[0].name());
        assert_eq!("value1", attrs[0].value());
        assert_eq!("name2",  attrs[1].name());
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
        assert_eq!(children[0], TextCOE(text));
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
        assert_eq!(children[0], CommentCOE(comment));
    }

    #[test]
    fn comment_knows_its_parent() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let sentence = s.create_element("sentence");
        let comment = s.create_comment("Now is the winter of our discontent.");

        c.append_element_child(sentence, comment);

        assert_eq!(c.comment_parent(comment), Some(ElementPOC(sentence)));
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
        assert_eq!(children[0], ProcessingInstructionCOE(pi));
    }

    #[test]
    fn processing_instruction_knows_its_parent() {
        let package = Package::new();
        let (s, mut c) = package.as_thin_document();

        let element = s.create_element("element");
        let pi = s.create_processing_instruction("device", None);

        c.append_element_child(element, pi);

        assert_eq!(c.processing_instruction_parent(pi), Some(ElementPOC(element)));
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
}
