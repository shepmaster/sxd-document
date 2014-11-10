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

pub struct Element<'d> {
    lifetime: InvariantLifetime<'d>,
    node: *mut raw::Element,
}

impl<'d> Element<'d> {
    fn wrap(node: *mut raw::Element) -> Element<'d> {
        Element {
            lifetime: InvariantLifetime,
            node: node,
        }
    }
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
}
