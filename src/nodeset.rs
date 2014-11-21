use super::dom4::{Node,ToNode};
use std::slice::Items;

/// Convenience constructor for a nodeset
#[macro_export]
macro_rules! nodeset(
    ($($e:expr),*) => ({
        // leading _ to allow empty construction without a warning.
        let mut _temp = ::document::nodeset::Nodeset::new();
        $(_temp.add($e);)*
        _temp
    });
    ($($e:expr),+,) => (nodeset!($($e),+))
)

/// A collection of nodes
#[deriving(PartialEq,Show,Clone)]
pub struct Nodeset<'d> {
    nodes: Vec<Node<'d>>,
}

impl<'d> Nodeset<'d> {
    pub fn new() -> Nodeset<'d> {
        Nodeset { nodes: Vec::new() }
    }

    pub fn add<N : ToNode<'d>>(&mut self, node: N) {
        self.nodes.push(node.to_node());
    }

    pub fn iter(&self) -> Items<Node<'d>> {
        self.nodes.iter()
    }

    pub fn add_nodeset(& mut self, other: &Nodeset<'d>) {
        self.nodes.push_all(other.nodes.as_slice());
    }

    pub fn size(&self) -> uint {
        self.nodes.len()
    }
}

#[cfg(test)]
mod test {
    use super::super::Package;
    use super::super::dom4::Node::{
        AttributeNode,
        CommentNode,
        ElementNode,
        ProcessingInstructionNode,
        RootNode,
        TextNode,
    };
    use super::Nodeset;

    #[test]
    fn nodeset_can_include_all_node_types() {
        let package = Package::new();
        let doc = package.as_document();
        let mut nodes = Nodeset::new();

        let r = doc.root();
        let e = doc.create_element("element");
        let a = e.set_attribute_value("name", "value");
        let t = doc.create_text("text");
        let c = doc.create_comment("comment");
        let p = doc.create_processing_instruction("pi", None);

        nodes.add(r);
        nodes.add(e);
        nodes.add(a);
        nodes.add(t);
        nodes.add(c);
        nodes.add(p);

        let node_vec: Vec<_> = nodes.iter().collect();

        assert_eq!(6, node_vec.len());
        assert_eq!(node_vec[0], &RootNode(r));
        assert_eq!(node_vec[1], &ElementNode(e));
        assert_eq!(node_vec[2], &AttributeNode(a));
        assert_eq!(node_vec[3], &TextNode(t));
        assert_eq!(node_vec[4], &CommentNode(c));
        assert_eq!(node_vec[5], &ProcessingInstructionNode(p));
    }

    #[test]
    fn nodesets_can_be_combined() {
        let package = Package::new();
        let doc = package.as_document();

        let mut all_nodes = Nodeset::new();
        let mut nodes1 = Nodeset::new();
        let mut nodes2 = Nodeset::new();

        let e1 = doc.create_element("element1");
        let e2 = doc.create_element("element2");

        all_nodes.add(e1);
        all_nodes.add(e2);

        nodes1.add(e1);
        nodes2.add(e2);

        nodes1.add_nodeset(&nodes2);

        assert_eq!(all_nodes, nodes1);
    }
}
