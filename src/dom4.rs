use super::raw;
use std::fmt;
use std::cell::RefCell;

pub struct Document<'d> {
    storage: &'d raw::Storage,
    connections: RefCell<&'d raw::Connections>,
}

impl<'d> Document<'d> {
    pub fn new(storage: &'d raw::Storage, connections: &'d raw::Connections) -> Document<'d> {
        Document {
            storage: storage,
            connections: RefCell::new(connections),
        }
    }

    fn wrap_element(&'d self, node: *mut raw::Element) -> Element<'d> {
        Element {
            document: self,
            node: node,
        }
    }

    fn wrap_attribute(&'d self, node: *mut raw::Attribute) -> Attribute<'d> {
        Attribute {
            document: self,
            node: node,
        }
    }

    pub fn create_element(&'d self, name: &str) -> Element<'d> {
        self.wrap_element(self.storage.create_element(name))
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
            match n {
                raw::ElementPOC(n) => ElementPOC(self.document.wrap_element(n)),
            }
        })
    }

    pub fn append_child(&self, child: Element) {
        let connections = self.document.connections.borrow_mut();
        connections.append_element_child(self.node, child.node)
    }

    pub fn children(&'d self) -> Vec<ChildOfElement<'d>> {
        let connections = self.document.connections.borrow();
        // This is safe because we copy of the children, and the
        // children are never deallocated.
        unsafe {
            let raw_children = connections.element_children(self.node);
            raw_children.iter().map(|n| match n {
                &raw::ElementCOE(n) => ElementCOE(self.document.wrap_element(n)),
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
        let node = unsafe { &* self.node };
        write!(f, "Element {{ name: {}  }}", node.name())
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

#[deriving(PartialEq,Show)]
pub enum ChildOfElement<'d> {
    ElementCOE(Element<'d>)
}

impl<'d> ChildOfElement<'d> {
    pub fn element(self) -> Option<Element<'d>> {
        match self {
            ElementCOE(n) => Some(n),
        }
    }
}

#[deriving(PartialEq,Show)]
pub enum ParentOfChild<'d> {
    ElementPOC(Element<'d>),
}

#[cfg(test)]
mod test {
    use super::super::Package;
    use super::{ElementCOE};
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
}
