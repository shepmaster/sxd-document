//! Formats a DOM structure to a Write
//!
//! ### Example
//! ```
//! use sxd_document::Package;
//! use sxd_document::writer::format_document;
//!
//! let package = Package::new();
//! let doc = package.as_document();
//!
//! let hello = doc.create_element("hello");
//! hello.set_attribute_value("planet", "Earth");
//! doc.root().append_child(hello);
//!
//! let mut output = Vec::new();
//! format_document(&doc, &mut output).ok().expect("unable to output XML");
//! ```
//!
//! ### Potential options to support
//!
//! - Space before `/>`
//! - Single vs double quotes
//! - Fixed ordering of attributes

use std::borrow::ToOwned;
use std::io::{self,Write};
use std::slice;

use self::Content::*;

use super::QName;

use super::dom;
use super::dom::{ChildOfElement,ChildOfRoot};
use super::lazy_hash_map::LazyHashMap;

trait WriteStr: Write {
    fn write_str(&mut self, s: &str) -> io::Result<()> {
        self.write_all(s.as_bytes())
    }
}

impl<W: ?Sized> WriteStr for W where W: Write {}

// TODO: Duplicating the String seems inefficient...
struct PrefixScope<'d> {
    ns_to_prefix: LazyHashMap<&'d str, String>,
    prefix_to_ns: LazyHashMap<String, &'d str>,
    defined_prefixes: Vec<(String, &'d str)>,
    default_namespace_uri: Option<&'d str>,
}

impl<'d> PrefixScope<'d> {
    fn new() -> PrefixScope<'d> {
        PrefixScope {
            ns_to_prefix: LazyHashMap::new(),
            prefix_to_ns: LazyHashMap::new(),
            defined_prefixes: Vec::new(),
            default_namespace_uri: None,
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

    fn namespace_uri_for(&self, prefix: &str) -> Option<&'d str> {
        self.prefix_to_ns.get(prefix).map(|&ns| ns)
    }

    fn prefix_for(&self, namespace_uri: &str) -> Option<&str> {
        self.ns_to_prefix.get(namespace_uri).map(|p| &p[..])
    }

    fn add_mapping(&mut self, prefix: &str, namespace_uri: &'d str) {
        let prefix = prefix.to_owned();

        self.prefix_to_ns.insert(prefix.clone(), namespace_uri);
        self.ns_to_prefix.insert(namespace_uri, prefix);
    }

    fn define_prefix(&mut self, prefix: String, namespace_uri: &'d str) {
        self.defined_prefixes.push((prefix, namespace_uri));
    }
}

enum NamespaceType<'a> {
    Default,
    Prefix(&'a str),
    Unknown,
}

struct PrefixMapping<'d> {
    scopes: Vec<PrefixScope<'d>>,
    generated_prefix_count: usize,
}

impl<'d> PrefixMapping<'d> {
    fn new() -> PrefixMapping<'d> {
        PrefixMapping {
            scopes: vec![PrefixScope::new()],
            generated_prefix_count: 0,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(PrefixScope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn active_default_namespace_uri(&self) -> Option<&'d str> {
        self.scopes.iter().rev().filter_map(|s| s.default_namespace_uri).next()
    }

    fn active_namespace_uri_for_prefix(&self, prefix: &str) -> Option<&'d str> {
        self.scopes.iter().rev().filter_map(|s| s.namespace_uri_for(prefix)).next()
    }

    fn default_namespace_uri_in_current_scope(&self) -> Option<&'d str> {
        self.scopes.last().unwrap().default_namespace_uri
    }

    fn prefixes_in_current_scope(&self) -> slice::Iter<(String, &'d str)> {
        self.scopes.last().unwrap().defined_prefixes.iter()
    }

    fn populate_scope(&mut self, element: &dom::Element<'d>, attributes: &[dom::Attribute<'d>]) {
        self.scopes.last_mut().unwrap().default_namespace_uri = element.default_namespace_uri();

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
            self.generate_prefix(uri);
        }

        for attribute in attributes.iter() {
            let name = attribute.name();
            if let Some(uri) = name.namespace_uri {
                self.generate_prefix(uri);
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
        current_scope.define_prefix(prefix.to_owned(), namespace_uri);
    }

    fn generate_prefix(&mut self, namespace_uri: &'d str) {
        if Some(namespace_uri) == self.active_default_namespace_uri() {
            // We already map this namespace to the default
            return;
        }

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

        loop {
            let prefix = format!("autons{}", self.generated_prefix_count);
            self.generated_prefix_count += 1;

            if ! current_scope.has_prefix(&prefix) {
                current_scope.add_mapping(&prefix, namespace_uri);
                current_scope.define_prefix(prefix, namespace_uri);
                break;
            }
        }
    }

    fn namespace_type<'a>(&'a self, preferred_prefix: Option<&'a str>, namespace_uri: &str) -> NamespaceType<'a> {
        if Some(namespace_uri) == self.active_default_namespace_uri() {
            return NamespaceType::Default;
        }

        if let Some(prefix) = preferred_prefix {
            if Some(namespace_uri) == self.active_namespace_uri_for_prefix(prefix) {
                return NamespaceType::Prefix(prefix);
            }
        }

        for scope in self.scopes.iter().rev() {
            if let Some(prefix) = scope.prefix_for(namespace_uri) {
                return NamespaceType::Prefix(prefix);
            }
        }

        NamespaceType::Unknown
    }
}

enum Content<'d> {
    Element(dom::Element<'d>),
    ElementEnd(dom::Element<'d>),
    Text(dom::Text<'d>),
    Comment(dom::Comment<'d>),
    ProcessingInstruction(dom::ProcessingInstruction<'d>),
}

fn format_qname<'d, W: ?Sized>(q: QName<'d>,
                               mapping: &mut PrefixMapping<'d>,
                               preferred_prefix: Option<&str>,
                               writer: &mut W)
                               -> io::Result<()>
    where W: Write
{
    // Can something without a namespace be prefixed? No, because
    // defining a prefix requires a non-empty URI
    if let Some(namespace_uri) = q.namespace_uri {
        match mapping.namespace_type(preferred_prefix, namespace_uri) {
            NamespaceType::Default => {
                // No need to do anything
            },
            NamespaceType::Prefix(prefix) => {
                try!(writer.write_str(prefix));
                try!(writer.write_str(":"));
            },
            NamespaceType::Unknown => {
                panic!("No namespace prefix available for {}", namespace_uri);
            },
        }
    }
    writer.write_str(q.local_part)
}

fn format_attribute_value<W: ?Sized>(value: &str, writer: &mut W) -> io::Result<()>
    where W: Write
{
    for item in value.split_keeping_delimiter(|c| c == '<' || c == '>' || c == '&' || c == '\'' || c == '"') {
        match item {
            SplitType::Match(t)        => try!(writer.write_str(t)),
            SplitType::Delimiter("<")  => try!(writer.write_str("&lt;")),
            SplitType::Delimiter(">")  => try!(writer.write_str("&gt;")),
            SplitType::Delimiter("&")  => try!(writer.write_str("&amp;")),
            SplitType::Delimiter("'")  => try!(writer.write_str("&apos;")),
            SplitType::Delimiter("\"") => try!(writer.write_str("&quot;")),
            SplitType::Delimiter(..)   => unreachable!(),
        }
    }
    Ok(())
}

fn format_element<'d, W: ?Sized>(element: dom::Element<'d>,
                                 todo: &mut Vec<Content<'d>>,
                                 mapping: &mut PrefixMapping<'d>,
                                 writer: &mut W)
                                 -> io::Result<()>
    where W: Write
{
    let attrs = element.attributes();

    mapping.populate_scope(&element, &attrs);

    try!(writer.write_str("<"));
    try!(format_qname(element.name(), mapping, element.preferred_prefix(), writer));

    for attr in &attrs {
        try!(writer.write_str(" "));
        try!(format_qname(attr.name(), mapping, attr.preferred_prefix(), writer));
        try!(write!(writer, "='"));
        try!(format_attribute_value(attr.value(), writer));
        try!(write!(writer, "'"));
    }

    if let Some(ns_uri) = mapping.default_namespace_uri_in_current_scope() {
        try!(writer.write_str(" xmlns='"));
        try!(writer.write_str(ns_uri));
        try!(writer.write_str("'"));
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

fn format_element_end<'d, W: ?Sized>(element: dom::Element<'d>,
                                     mapping: &mut PrefixMapping<'d>,
                                     writer: &mut W)
                                     -> io::Result<()>
    where W: Write
{
    try!(writer.write_str("</"));
    try!(format_qname(element.name(), mapping, element.preferred_prefix(), writer));
    writer.write_str(">")
}

use super::str_ext::{SplitKeepingDelimiterExt,SplitType};

fn format_text<W: ?Sized>(text: dom::Text, writer: &mut W) -> io::Result<()>
    where W: Write
{
    for item in text.text().split_keeping_delimiter(|c| c == '<' || c == '>' || c == '&') {
        match item {
            SplitType::Match(t)       => try!(writer.write_str(t)),
            SplitType::Delimiter("<") => try!(writer.write_str("&lt;")),
            SplitType::Delimiter(">") => try!(writer.write_str("&gt;")),
            SplitType::Delimiter("&") => try!(writer.write_str("&amp;")),
            SplitType::Delimiter(..)  => unreachable!(),
        }
    }
    Ok(())
}

fn format_comment<W: ?Sized>(comment: dom::Comment, writer: &mut W) -> io::Result<()>
    where W: Write
{
    write!(writer, "<!--{}-->", comment.text())
}

fn format_processing_instruction<W: ?Sized>(pi: dom::ProcessingInstruction, writer: &mut W)
                                            -> io::Result<()>
    where W: Write
{
    match pi.value() {
        None    => write!(writer, "<?{}?>", pi.target()),
        Some(v) => write!(writer, "<?{} {}?>", pi.target(), v),
    }
}

fn format_one<'d, W: ?Sized>(content: Content<'d>,
                             todo: &mut Vec<Content<'d>>,
                             mapping: &mut PrefixMapping<'d>,
                             writer: &mut W)
                             -> io::Result<()>
    where W: Write
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
        Text(t)                  => format_text(t, writer),
        Comment(c)               => format_comment(c, writer),
        ProcessingInstruction(p) => format_processing_instruction(p, writer),
    }
}

fn format_body<W: ?Sized>(element: dom::Element, writer: &mut W) -> io::Result<()>
    where W: Write
{
    let mut todo = vec![Element(element)];
    let mut mapping = PrefixMapping::new();

    while ! todo.is_empty() {
        try!(format_one(todo.pop().unwrap(), &mut todo, &mut mapping, writer));
    }

    Ok(())
}

/// Formats a document into a Write
pub fn format_document<'d, W: ?Sized>(doc: &'d dom::Document<'d>, writer: &mut W) -> io::Result<()>
    where W: Write
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

fn pretty_print_one<'d, W: ?Sized>(content: Content<'d>,
                             todo: &mut Vec<Content<'d>>,
                             mapping: &mut PrefixMapping<'d>,
                             depth: &mut usize,
                             writer: &mut W,
                             indent: &str)
                             -> io::Result<()>
    where W: Write
{
    match content {
        Element(e)               => {
            mapping.push_scope();
            if e.children().iter().any(|c| match c { &ChildOfElement::Element(_) => true, _ => false }) { *depth += 1; };
            let r = format_element(e, todo, mapping, writer);
            if e.children().is_empty() && e.following_siblings().is_empty() && *depth > 0 { *depth -= 1; }
            match e.children().first() {
                Some(&ChildOfElement::Element(_)) | None => {
                    try!(writeln!(writer, ""));
                    for _ in 0..*depth { try!(writer.write_str(indent)) };
                },
                _ => {}
            }
            r
        },
        ElementEnd(e)            => {
            if e.following_siblings().is_empty() && *depth > 0 { *depth -= 1; };
            let r = format_element_end(e, mapping, writer);
            try!(writeln!(writer, ""));
            for _ in 0..*depth { try!(writer.write_str(indent)) };
            mapping.pop_scope();
            r
        },
        Text(t)                  => format_text(t, writer),
        Comment(c)               => format_comment(c, writer),
        ProcessingInstruction(p) => format_processing_instruction(p, writer),
    }
}

fn pretty_print_body<W: ?Sized>(element: dom::Element, writer: &mut W, indent: &str) -> io::Result<()>
    where W: Write
{
    let mut todo = vec![Element(element)];
    let mut mapping = PrefixMapping::new();
    let mut depth = 0;

    while ! todo.is_empty() {
        try!(pretty_print_one(todo.pop().unwrap(), &mut todo, &mut mapping, &mut depth, writer, &indent));
    }

    Ok(())
}

/// Pretty-prints a document into a Write
pub fn pretty_print_document<'d, W: ?Sized>(doc: &'d dom::Document<'d>, writer: &mut W, indent: &str) -> io::Result<()>
    where W: Write
{
    try!(writer.write_str("<?xml version='1.0'?>"));
    try!(writeln!(writer, ""));

    for child in doc.root().children().into_iter() {
        try!(match child {
            ChildOfRoot::Element(e) => pretty_print_body(e, writer, indent),
            ChildOfRoot::Comment(c) => format_comment(c, writer),
            ChildOfRoot::ProcessingInstruction(p) => format_processing_instruction(p, writer),
        })
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::super::Package;
    use super::super::dom;
    use super::super::parser::parse;
    use super::{format_document, pretty_print_document};

    fn format_xml<'d>(doc: &'d dom::Document<'d>) -> String {
        let mut w = Vec::new();
        format_document(doc, &mut w).expect("Not formatted");
        String::from_utf8(w).expect("Not a string")
    }

    fn pretty_print_xml<'d>(doc: &'d dom::Document<'d>) -> String {
        let mut w = Vec::new();
        pretty_print_document(doc, &mut w, "\t").expect("Not formatted");
        String::from_utf8(w).expect("Not a string")
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
    fn element_with_default_namespace() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element(("namespace", "local-part"));
        e.set_default_namespace_uri(Some("namespace"));
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><local-part xmlns='namespace'/>");
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
    fn attribute_values_with_less_than_greater_than_ampersand_apostrophe_or_quote_are_escaped() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value("name", r#"'1 < 2' & "4 > 3""#);
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello name='&apos;1 &lt; 2&apos; &amp; &quot;4 &gt; 3&quot;'/>");
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
    fn text_escapes_less_than_greater_than_and_ampersand() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("escaped");
        let text = d.create_text("1 < 3 & 4 > 2");
        hello.append_child(text);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><escaped>1 &lt; 3 &amp; 4 &gt; 2</escaped>");
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

    #[test]
    fn pretty_printing_uncollapsed_empty_xml() {
        let unformatted_xml = "<r></r>";
        let formatted_xml = "<?xml version='1.0'?>\n<r/>\n";
        let package = parse(&unformatted_xml).unwrap();
        let document = package.as_document();
        let pretty_printed_xml = pretty_print_xml(&document);

        assert_eq!(&pretty_printed_xml, &formatted_xml);
    }

    #[test]
    fn pretty_printing_empty_xml() {
        let unformatted_xml = "<r/>";
        let formatted_xml = "<?xml version='1.0'?>\n<r/>\n";
        let package = parse(&unformatted_xml).unwrap();
        let document = package.as_document();
        let pretty_printed_xml = pretty_print_xml(&document);

        assert_eq!(&pretty_printed_xml, &formatted_xml);
    }

    #[test]
    fn pretty_printing_xml_with_prolog() {
        let unformatted_xml = "<?xml version='1.0'?><r/>";
        let formatted_xml = "<?xml version='1.0'?>\n<r/>\n";
        let package = parse(&unformatted_xml).unwrap();
        let document = package.as_document();
        let pretty_printed_xml = pretty_print_xml(&document);

        assert_eq!(&pretty_printed_xml, &formatted_xml);
    }
/*
    #[test]
    fn pretty_printing_xml_with_prolog_and_dtd() {
        let unformatted_xml = "<?xml version='1.0'?><!DOCTYPE DOC SYSTEM \"DOC.DTD\"><r/>";
        let formatted_xml = "<?xml version='1.0'?>\n<!DOCTYPE DOC SYSTEM 'DOC.DTD'>\n<r/>\n";
        let package = parse(&unformatted_xml).unwrap();
        let document = package.as_document();
        let pretty_printed_xml = pretty_print_xml(&document);

        assert_eq!(&pretty_printed_xml, &formatted_xml);
    }
*/
    #[test]
    fn pretty_printing_simple_xml() {
        let unformatted_xml = "<r><a><b>c</b></a></r>";
        let formatted_xml = "<?xml version='1.0'?>\n<r>\n\t<a>\n\t\t<b>c</b>\n\t</a>\n</r>\n";
        let package = parse(&unformatted_xml).unwrap();
        let document = package.as_document();
        let pretty_printed_xml = pretty_print_xml(&document);

        assert_eq!(&pretty_printed_xml, &formatted_xml);
    }

    #[test]
    fn pretty_printing_xml_with_siblings() {
        let unformatted_xml = "<r><a><b>c1</b><b>c2</b></a></r>";
        let formatted_xml = "<?xml version='1.0'?>\n<r>\n\t<a>\n\t\t<b>c1</b>\n\t\t<b>c2</b>\n\t</a>\n</r>\n";
        let package = parse(&unformatted_xml).unwrap();
        let document = package.as_document();
        let pretty_printed_xml = pretty_print_xml(&document);

        assert_eq!(&pretty_printed_xml, &formatted_xml);
    }

    #[test]
    fn pretty_printing_xml_with_empty_siblings() {
        let unformatted_xml = "<r><a></a><b/><c></c><d></d><e/><f/><g></g><h></h><i></i></r>";
        let formatted_xml = "<?xml version='1.0'?>\n<r>\n\t<a/>\n\t<b/>\n\t<c/>\n\t<d/>\n\t<e/>\n\t<f/>\n\t<g/>\n\t<h/>\n\t<i/>\n</r>\n";
        let package = parse(&unformatted_xml).unwrap();
        let document = package.as_document();
        let pretty_printed_xml = pretty_print_xml(&document);

        assert_eq!(&pretty_printed_xml, &formatted_xml);
    }
/*
    #[test]
    fn pretty_printing_pretty_xml() {
        let pretty_xml = "<?xml version='1.0'?>\n<r>\n\t<a>\n\t\t<b/>\n\t</a>\n</r>\n";
        let package = parse(&pretty_xml).unwrap();
        let document = package.as_document();
        let pretty_printed_xml = pretty_print_xml(&document);

        assert_eq!(&pretty_printed_xml, &pretty_xml);
    }
*/
}
