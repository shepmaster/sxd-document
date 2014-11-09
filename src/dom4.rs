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

    pub fn create_element(&'d self, name: &str) -> Element<'d> {
        self.wrap_element(self.storage.create_element(name))
    }
}

pub struct Element<'d> {
    document: &'d Document<'d>,
    node: *mut raw::Element,
}

impl<'d> Element<'d> {
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
    ElementCOE(Element<'d>)
}

impl<'d> ChildOfElement<'d> {
    pub fn element(self) -> Option<Element<'d>> {
        match self {
            ElementCOE(n) => Some(n),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::Package;
    use super::{ElementCOE};

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
}
