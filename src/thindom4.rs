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

    pub fn element_set_name(&self, element: &Element, name: &str) {
        self.storage.element_set_name(element.node, name)
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
            match n {
                raw::ElementPOC(n) => ElementPOC(Element::wrap(n)),
            }
        })
    }

    pub fn append_element_child(&mut self, parent: Element<'d>, child: Element<'d>) {
        self.connections.append_element_child(parent.node, child.node)
    }

    pub fn element_children(&self, parent: Element<'d>) -> ElementChildren<'d> {
        // This is safe because we disallow mutation while this borrow is active.
        // TODO: Test that
        unsafe { ElementChildren { x: self.connections.element_children(parent.node), idx: 0 } }
    }

    pub fn attribute_parent(&self, attribute: Attribute<'d>) -> Option<Element<'d>> {
        self.connections.attribute_parent(attribute.node).map(|a| Element::wrap(a))
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

pub struct ElementChildren<'d> {
    x: &'d [raw::ChildOfElement],
    idx: uint,
}

impl<'d> Iterator<ChildOfElement<'d>> for ElementChildren<'d> {
    fn next(&mut self) -> Option<ChildOfElement<'d>> {
        if self.idx >= self.x.len() {
            None
        } else {
            let e = Element {
                lifetime: InvariantLifetime,
                node: match self.x[self.idx] {
                    raw::ElementCOE(n) => n
                },
            };
            self.idx += 1;
            Some(ElementCOE(e))
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

            fn node(&self) -> &'d $raw { unsafe { &*self.node } }
        }
    )
)

node!(Element, raw::Element)

impl<'d> Element<'d> {
    pub fn name(&self) -> &'d str { self.node().name() }
}

impl<'d> PartialEq for Element<'d> {
    fn eq(&self, other: &Element<'d>) -> bool {
        self.node == other.node
    }
}

impl<'d> fmt::Show for Element<'d> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let node = unsafe { &* self.node };
        write!(f, "Element {{ name: {}  }}", node.name())
    }
}

node!(Attribute, raw::Attribute)

#[deriving(PartialEq,Show)]
pub enum ChildOfElement<'d> {
    ElementCOE(Element<'d>),
}

impl<'d> ChildOfElement<'d> {
    pub fn element(self) -> Option<Element<'d>> {
        match self {
            ElementCOE(n) => Some(n)
        }
    }
}

#[deriving(PartialEq,Show)]
pub enum ParentOfChild<'d> {
    ElementPOC(Element<'d>),
}

impl<'d> ParentOfChild<'d> {
    pub fn element(self) -> Option<Element<'d>> {
        match self {
            ElementPOC(n) => Some(n)
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::Package;
    use super::{ChildOfElement,ElementCOE};
    use super::{ElementPOC};

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
}
