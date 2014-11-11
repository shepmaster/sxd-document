use super::raw;
use std::fmt;
use std::cell::RefCell;

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
)

impl<'d> Document<'d> {
    wrapper!(wrap_element, Element, raw::Element)
    wrapper!(wrap_attribute, Attribute, raw::Attribute)
    wrapper!(wrap_text, Text, raw::Text)
    wrapper!(wrap_comment, Comment, raw::Comment)
    wrapper!(wrap_pi, ProcessingInstruction, raw::ProcessingInstruction)

    pub fn new(storage: &'d raw::Storage, connections: &'d raw::Connections) -> Document<'d> {
        Document {
            storage: storage,
            connections: RefCell::new(connections),
        }
    }

    fn wrap_parent_of_child(&'d self, node: raw::ParentOfChild) -> ParentOfChild<'d> {
        match node {
            raw::ElementPOC(n) => ElementPOC(self.wrap_element(n)),
        }
    }

    fn wrap_child_of_element(&'d self, node: raw::ChildOfElement) -> ChildOfElement<'d> {
        match node {
            raw::ElementCOE(n) => ElementCOE(self.wrap_element(n)),
            raw::TextCOE(n) => TextCOE(self.wrap_text(n)),
            raw::CommentCOE(n) => CommentCOE(self.wrap_comment(n)),
            raw::ProcessingInstructionCOE(n) => ProcessingInstructionCOE(self.wrap_pi(n)),
        }
    }

    pub fn create_element(&'d self, name: &str) -> Element<'d> {
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
}

impl<'d> PartialEq for Document<'d> {
    fn eq(&self, other: &Document<'d>) -> bool {
        self as *const Document == other as *const Document
    }
}

impl<'d> fmt::Show for Document<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Document {{ {} }}", self as *const Document)
    }
}

macro_rules! node(
    ($name:ident, $raw:ty) => (
        pub struct $name<'d> {
            document: &'d Document<'d>,
            node: *mut $raw,
        }

        impl<'d> $name<'d> {
            fn node(&self) -> &$raw { unsafe { &*self.node } }

            pub fn document(&self) -> &'d Document<'d> { self.document }
        }

        impl<'d> PartialEq for $name<'d> {
            fn eq(&self, other: &$name<'d>) -> bool {
                self.node == other.node
            }
        }
    )
)

node!(Element, raw::Element)

impl<'d> Element<'d> {
    pub fn name(&self) -> &str { self.node().name() }

    pub fn set_name(&self, name: &str) {
        self.document.storage.element_set_name(self.node, name)
    }

    pub fn parent(&'d self) -> Option<ParentOfChild<'d>> {
        let connections = self.document.connections.borrow();

        connections.element_parent(self.node).map(|n| {
            self.document.wrap_parent_of_child(n)
        })
    }

    pub fn append_child<C : ToChildOfElement<'d>>(&self, child: C) {
        let child = child.to_child_of_element();
        let connections = self.document.connections.borrow_mut();
        connections.append_element_child(self.node, child.as_raw())
    }

    pub fn children(&'d self) -> Vec<ChildOfElement<'d>> {
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

    pub fn set_attribute_value(&self, name: &str, value: &str) -> Attribute<'d> {
        let attr = self.document.storage.create_attribute(name, value);
        let connections = self.document.connections.borrow_mut();
        connections.set_attribute(self.node, attr);
        self.document.wrap_attribute(attr)
    }

    pub fn attribute_value(&self, name: &str) -> Option<&'d str> {
        let connections = self.document.connections.borrow();
        connections.attribute(self.node, name).map(|a| {
            let a_r = unsafe { &*a };
            a_r.value()
        })
    }
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

    pub fn parent(&self) -> Option<Element<'d>> {
        let connections = self.document.connections.borrow();
        connections.attribute_parent(self.node).map(|n| {
            self.document.wrap_element(n)
        })
    }
}

node!(Text, raw::Text)

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
}

impl<'d> fmt::Show for Text<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Text {{ text: {} }}", self.text())
    }
}

node!(Comment, raw::Comment)

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
}

impl<'d> fmt::Show for Comment<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Comment {{ text: {} }}", self.text())
    }
}

node!(ProcessingInstruction, raw::ProcessingInstruction)

impl<'d> ProcessingInstruction<'d> {
    pub fn target(&self)  -> &str { self.node().target() }
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
    ElementPOC(Element<'d>),
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

conversion_trait!(ToChildOfElement, to_child_of_element, ChildOfElement, {
    Element => ElementCOE,
    Text => TextCOE,
    Comment => CommentCOE,
    ProcessingInstruction => ProcessingInstructionCOE
})

#[cfg(test)]
mod test {
    use super::super::Package;
    use super::{ElementCOE,TextCOE,CommentCOE,ProcessingInstructionCOE};
    use super::{ElementPOC};

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

        assert_eq!(children[0], ElementCOE(beta));
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

        assert_eq!(children[0], ElementCOE(alpha));
        assert_eq!(children[1], ElementCOE(omega));
    }

    #[test]
    fn element_children_know_their_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let alpha = doc.create_element("alpha");
        let beta  = doc.create_element("beta");

        alpha.append_child(beta);

        assert_eq!(Some(ElementPOC(alpha)), beta.parent());
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
        assert_eq!(alpha.name(), "beta");
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
        attrs.sort_by(|a, b| a.name().cmp(b.name()));

        assert_eq!(2, attrs.len());
        assert_eq!("name1",  attrs[0].name());
        assert_eq!("value1", attrs[0].value());
        assert_eq!("name2",  attrs[1].name());
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
        assert_eq!(children[0], TextCOE(text));
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
        assert_eq!(children[0], CommentCOE(comment));
    }

    #[test]
    fn comment_knows_its_parent() {
        let package = Package::new();
        let doc = package.as_document();

        let sentence = doc.create_element("sentence");
        let comment = doc.create_comment("Now is the winter of our discontent.");

        sentence.append_child(comment);

        assert_eq!(comment.parent(), Some(ElementPOC(sentence)));
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
    fn elements_can_have_processing_instruction_children() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("element");
        let pi = doc.create_processing_instruction("device", None);

        element.append_child(pi);

        let children = element.children();
        assert_eq!(1, children.len());
        assert_eq!(children[0], ProcessingInstructionCOE(pi));
    }
}
