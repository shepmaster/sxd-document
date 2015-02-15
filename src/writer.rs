//! Formats a DOM structure to a Writer
//!
//! ### Example
//! ```
//! use document::Package;
//! use document::writer::format_document;
//!
//! let package = Package::new();
//! let doc = package.as_document();
//!
//! let hello = doc.create_element("hello");
//! hello.set_attribute_value("planet", "Earth");
//! doc.root().append_child(hello);
//!
//! let mut output = std::old_io::stdio::stdout();
//! format_document(&doc, &mut output).ok().expect("unable to output XML");
//! ```
//!
//! ### Known issues
//!
//! Output is not escaped in any way,
//! it's very easy to create malformed XML!
//!
//! ### Potential options to support
//!
//! - Space before `/>`
//! - Single vs double quotes
//! - Fixed ordering of attributes

use std::num::Int;
use std::collections::HashMap;
use std::slice;
use std::old_io::IoResult;

use self::Content::*;

use super::QName;

use super::dom4;
use super::dom4::{ChildOfElement,ChildOfRoot};

// TODO: Duplicating the String seems inefficient...
struct PrefixScope<'d> {
    ns_to_prefix: HashMap<&'d str, String>,
    prefix_to_ns: HashMap<String, &'d str>,
    defined_prefixes: Vec<(String, &'d str)>,
}

impl<'d> PrefixScope<'d> {
    fn new() -> PrefixScope<'d> {
        PrefixScope {
            ns_to_prefix: HashMap::new(),
            prefix_to_ns: HashMap::new(),
            defined_prefixes: Vec::new(),
        }
    }

    fn has_prefix(&self, prefix: &str) -> bool {
        self.prefix_to_ns.contains_key(prefix)
    }

    fn has_namespace_uri(&self, namespace_uri: &str) -> bool {
        self.ns_to_prefix.contains_key(namespace_uri)
    }

    fn prefix_is(&self, prefix: &str, namespace_uri: &str) -> bool {
        match self.prefix_to_ns.get(prefix) {
            Some(ns) => *ns == namespace_uri,
            _ => false,
        }
    }

    fn prefix_for(&self, namespace_uri: &str) -> Option<&str> {
        self.ns_to_prefix.get(namespace_uri).map(|p| p.as_slice())
    }

    fn add_mapping(&mut self, prefix: &str, namespace_uri: &'d str) {
        let prefix = String::from_str(prefix);

        self.prefix_to_ns.insert(prefix.clone(), namespace_uri);
        self.ns_to_prefix.insert(namespace_uri, prefix);
    }

    fn define_prefix(&mut self, prefix: String, namespace_uri: &'d str) {
        self.defined_prefixes.push((prefix, namespace_uri));
    }
}

struct PrefixMapping<'d> {
    scopes: Vec<PrefixScope<'d>>,
}

impl<'d> PrefixMapping<'d> {
    fn new() -> PrefixMapping<'d> {
        PrefixMapping {
            scopes: vec![PrefixScope::new()],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(PrefixScope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn prefixes_in_current_scope(&self) -> slice::Iter<(String, &'d str)> {
        self.scopes.last().unwrap().defined_prefixes.iter()
    }

    fn populate_scope(&mut self, element: &dom4::Element<'d>, attributes: &[dom4::Attribute<'d>]) {
        if let Some(prefix) = element.preferred_prefix() {
            let name = element.name();
            if let Some(uri) = name.namespace_uri {
                self.set_prefix(prefix, uri);
            }
        }

        for attribute in attributes.iter() {
            if let Some(prefix) = attribute.preferred_prefix() {
                let name = attribute.name();
                if let Some(uri) = name.namespace_uri {
                    self.set_prefix(prefix, uri);
                }
            }
        }

        let name = element.name();
        if let Some(uri) = name.namespace_uri {
            self.generate_prefix(element, uri);
        }

        for attribute in attributes.iter() {
            let name = attribute.name();
            if let Some(uri) = name.namespace_uri {
                self.generate_prefix(element, uri);
            }
        }
    }

    fn set_prefix(&mut self, prefix: &str, namespace_uri: &'d str) {
        let idx_of_last = self.scopes.len().saturating_sub(1);
        let (parents, current_scope) = self.scopes.split_at_mut(idx_of_last);
        let current_scope = &mut current_scope[0];

        // If we're already using this prefix, we can't redefine it.
        if current_scope.has_prefix(prefix) {
            return;
        }

        // We are definitely going to use this prefix, claim it
        current_scope.add_mapping(prefix, namespace_uri);

        for parent_scope in parents.iter().rev() {
            if parent_scope.prefix_is(prefix, namespace_uri) {
                // A parent defines it as the URI we want.
                // Prevent redefining it
                return;
            }
        }

        // Defined by us, must be added to the element
        current_scope.define_prefix(String::from_str(prefix), namespace_uri);
    }

    fn generate_prefix(&mut self, element: &dom4::Element<'d>, namespace_uri: &'d str) {
        let idx_of_last = self.scopes.len().saturating_sub(1);
        let (parents, current_scope) = self.scopes.split_at_mut(idx_of_last);
        let current_scope = &mut current_scope[0];

        if current_scope.has_namespace_uri(namespace_uri) {
            // We already map this namespace to *some* prefix
            return;
        }

        // Check if the parent already defined a prefix for this ns
        for parent_scope in parents.iter().rev() {
            if let Some(prefix) = parent_scope.prefix_for(namespace_uri) {
                // A parent happens to have a prefix for this URI.
                // Prevent redefining it
                current_scope.add_mapping(prefix, namespace_uri);
                return;
            }
        }

        let prefix = element.generate_prefix(namespace_uri, None);
        current_scope.add_mapping(&prefix, namespace_uri);
        current_scope.define_prefix(prefix.to_string(), namespace_uri);
    }

    fn prefix<'a : 'c, 'b : 'c, 'c>(&'a self, preferred_prefix: Option<&'b str>, namespace_uri: &str) -> &'c str {
        if let Some(prefix) = preferred_prefix {
            let scope = self.scopes.last().unwrap();
            if scope.prefix_is(prefix, namespace_uri) {
                return prefix;
            }
        }

        for scope in self.scopes.iter().rev() {
            if let Some(prefix) = scope.prefix_for(namespace_uri) {
                return prefix;
            }
        }

        panic!("No namespace prefix available for {}", namespace_uri);
    }
}

enum Content<'d> {
    Element(dom4::Element<'d>),
    ElementEnd(dom4::Element<'d>),
    Text(dom4::Text<'d>),
    Comment(dom4::Comment<'d>),
    ProcessingInstruction(dom4::ProcessingInstruction<'d>),
}

fn format_qname<'d, W>(q: QName<'d>,
                       mapping: &mut PrefixMapping<'d>,
                       preferred_prefix: Option<&str>,
                       writer: &mut W)
                       -> IoResult<()>
    where W: Writer
{
    if let Some(namespace_uri) = q.namespace_uri {
        let prefix = mapping.prefix(preferred_prefix, namespace_uri);
        try!(writer.write_str(prefix));
        try!(writer.write_str(":"));
    }
    writer.write_str(q.local_part)
}

fn format_element<'d, W>(element: dom4::Element<'d>,
                         todo: &mut Vec<Content<'d>>,
                         mapping: &mut PrefixMapping<'d>,
                         writer: &mut W)
                         -> IoResult<()>
    where W: Writer
{
    let attrs = element.attributes();

    mapping.populate_scope(&element, &attrs);

    try!(writer.write_str("<"));
    try!(format_qname(element.name(), mapping, element.preferred_prefix(), writer));

    for attr in attrs.iter() {
        try!(writer.write_str(" "));
        try!(format_qname(attr.name(), mapping, attr.preferred_prefix(), writer));
        try!(write!(writer, "='{}'", attr.value()));
    }

    for &(ref prefix, ref ns_uri) in mapping.prefixes_in_current_scope() {
        try!(writer.write_str(" xmlns:"));
        try!(writer.write_str(prefix));
        try!(write!(writer, "='{}'", ns_uri));
    }

    let mut children = element.children();
    if children.is_empty() {
        writer.write_str("/>")
    } else {
        try!(writer.write_str(">"));

        todo.push(ElementEnd(element));
        children.reverse();
        let x = children.into_iter().map(|c| match c {
            ChildOfElement::Element(element)         => Element(element),
            ChildOfElement::Text(t)                  => Text(t),
            ChildOfElement::Comment(c)               => Comment(c),
            ChildOfElement::ProcessingInstruction(p) => ProcessingInstruction(p),
        });
        todo.extend(x);

        Ok(())
    }
}

fn format_element_end<'d, W>(element: dom4::Element<'d>,
                             mapping: &mut PrefixMapping<'d>,
                             writer: &mut W)
                             -> IoResult<()>
    where W: Writer
{
    try!(writer.write_str("</"));
    try!(format_qname(element.name(), mapping, element.preferred_prefix(), writer));
    writer.write_str(">")
}

fn format_comment<W>(comment: dom4::Comment, writer: &mut W) -> IoResult<()>
    where W: Writer
{
    write!(writer, "<!--{}-->", comment.text())
}

fn format_processing_instruction<W>(pi: dom4::ProcessingInstruction, writer: &mut W) -> IoResult<()>
    where W: Writer
{
    match pi.value() {
        None    => write!(writer, "<?{}?>", pi.target()),
        Some(v) => write!(writer, "<?{} {}?>", pi.target(), v),
    }
}

fn format_one<'d, W>(content: Content<'d>,
                     todo: &mut Vec<Content<'d>>,
                     mapping: &mut PrefixMapping<'d>,
                     writer: &mut W)
                     -> IoResult<()>
    where W: Writer
{
    match content {
        Element(e)               => {
            mapping.push_scope();
            format_element(e, todo, mapping, writer)
        },
        ElementEnd(e)            => {
            let r = format_element_end(e, mapping, writer);
            mapping.pop_scope();
            r
        },
        Text(t)                  => writer.write_str(t.text()),
        Comment(c)               => format_comment(c, writer),
        ProcessingInstruction(p) => format_processing_instruction(p, writer),
    }
}

fn format_body<W>(element: dom4::Element, writer: &mut W) -> IoResult<()>
    where W: Writer
{
    let mut todo = vec![Element(element)];
    let mut mapping = PrefixMapping::new();

    while ! todo.is_empty() {
        try!(format_one(todo.pop().unwrap(), &mut todo, &mut mapping, writer));
    }

    Ok(())
}

/// Formats a document into a Writer
pub fn format_document<'d, W>(doc: &'d dom4::Document<'d>, writer: &mut W) -> IoResult<()>
    where W: Writer
{
    try!(writer.write_str("<?xml version='1.0'?>"));

    for child in doc.root().children().into_iter() {
        try!(match child {
            ChildOfRoot::Element(e) => format_body(e, writer),
            ChildOfRoot::Comment(c) => format_comment(c, writer),
            ChildOfRoot::ProcessingInstruction(p)      => format_processing_instruction(p, writer),
        })
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::super::Package;
    use super::super::dom4;
    use super::format_document;

    fn format_xml<'d>(doc: &'d dom4::Document<'d>) -> String {
        let mut w = Vec::new();
        format_document(doc, &mut w).ok().expect("Not formatted");
        String::from_utf8(w).ok().expect("Not a string")
    }

    #[test]
    fn top_element() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello/>");
    }

    #[test]
    fn element_with_namespace() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element(("namespace", "local-part"));
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><autons0:local-part xmlns:autons0='namespace'/>");
    }

    #[test]
    fn element_with_preferred_namespace_prefix() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element(("namespace", "local-part"));
        e.set_preferred_prefix(Some("prefix"));
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><prefix:local-part xmlns:prefix='namespace'/>");
    }

    #[test]
    fn element_with_attributes() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value("a", "b");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello a='b'/>");
    }

    #[test]
    fn attribute_with_namespace() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value(("namespace", "a"), "b");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello autons0:a='b' xmlns:autons0='namespace'/>");
    }

    #[test]
    fn attribute_with_preferred_namespace_prefix() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        let a = e.set_attribute_value(("namespace", "a"), "b");
        a.set_preferred_prefix(Some("p"));
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello p:a='b' xmlns:p='namespace'/>");
    }

    #[test]
    fn attributes_with_conflicting_preferred_namespace_prefixes() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");

        let a = e.set_attribute_value(("namespace1", "a1"), "b1");
        a.set_preferred_prefix(Some("p"));

        let a = e.set_attribute_value(("namespace2", "a2"), "b2");
        a.set_preferred_prefix(Some("p"));

        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello p:a1='b1' autons0:a2='b2' xmlns:p='namespace1' xmlns:autons0='namespace2'/>");
    }

    #[test]
    fn attributes_with_different_preferred_namespace_prefixes_for_same_namespace() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");

        let a = e.set_attribute_value(("namespace", "a1"), "b1");
        a.set_preferred_prefix(Some("p1"));

        let a = e.set_attribute_value(("namespace", "a2"), "b2");
        a.set_preferred_prefix(Some("p2"));

        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello p1:a1='b1' p2:a2='b2' xmlns:p1='namespace' xmlns:p2='namespace'/>");
    }

    #[test]
    fn nested_element() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let world = d.create_element("world");
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello><world/></hello>");
    }

    #[test]
    fn nested_element_with_namespaces() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element(("outer", "hello"));
        let world = d.create_element(("inner", "world"));
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><autons0:hello xmlns:autons0='outer'><autons1:world xmlns:autons1='inner'/></autons0:hello>");
    }

    #[test]
    fn nested_element_with_namespaces_with_reused_namespaces() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element(("ns", "hello"));
        let world = d.create_element(("ns", "world"));
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><autons0:hello xmlns:autons0='ns'><autons0:world/></autons0:hello>");
    }

    #[test]
    fn nested_element_with_with_conflicting_preferred_namespace_prefixes() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element(("outer", "hello"));
        let world = d.create_element(("inner", "world"));
        hello.set_preferred_prefix(Some("p"));
        world.set_preferred_prefix(Some("p"));
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><p:hello xmlns:p='outer'><p:world xmlns:p='inner'/></p:hello>");
    }

    #[test]
    fn nested_text() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let text = d.create_text("A fine day to you!");
        hello.append_child(text);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello>A fine day to you!</hello>");
    }

    #[test]
    fn nested_comment() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let comment = d.create_comment(" Fill this in ");
        hello.append_child(comment);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello><!-- Fill this in --></hello>");
    }

    #[test]
    fn nested_processing_instruction_without_value() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let pi = d.create_processing_instruction("display", None);
        hello.append_child(pi);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello><?display?></hello>");
    }

    #[test]
    fn nested_processing_instruction_with_value() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let pi = d.create_processing_instruction("display", Some("screen"));
        hello.append_child(pi);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello><?display screen?></hello>");
    }

    #[test]
    fn top_level_comment() {
        let p = Package::new();
        let d = p.as_document();
        let comment = d.create_comment(" Fill this in ");
        d.root().append_child(comment);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><!-- Fill this in -->");
    }

    #[test]
    fn top_level_processing_instruction() {
        let p = Package::new();
        let d = p.as_document();
        let pi = d.create_processing_instruction("display", None);
        d.root().append_child(pi);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><?display?>");
    }
}
