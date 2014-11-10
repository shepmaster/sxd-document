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
    fn node(&self) -> &raw::Element { unsafe { &*self.node } }

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
}
