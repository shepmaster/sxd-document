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
//! format_document(&doc, &mut output).expect("unable to output XML");
//! ```
//!
//! ### Potential options to support
//!
//! - Space before `/>`
//! - Single vs double quotes
//! - Fixed ordering of attributes

use std::{
    borrow::ToOwned,
    io::{self, Write},
    slice,
};

use self::Content::*;

use super::{
    str_ext::{SplitKeepingDelimiterExt, SplitType},
    QName,
};

use super::{
    dom,
    dom::{ChildOfElement, ChildOfRoot},
    lazy_hash_map::LazyHashMap,
};

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
        self.prefix_to_ns.get(prefix).cloned()
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
        self.scopes
            .iter()
            .rev()
            .filter_map(|s| s.default_namespace_uri)
            .next()
    }

    fn active_namespace_uri_for_prefix(&self, prefix: &str) -> Option<&'d str> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|s| s.namespace_uri_for(prefix))
            .next()
    }

    fn default_namespace_uri_in_current_scope(&self) -> Option<&'d str> {
        self.scopes.last().unwrap().default_namespace_uri
    }

    fn prefixes_in_current_scope(&self) -> slice::Iter<'_, (String, &'d str)> {
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

            if !current_scope.has_prefix(&prefix) {
                current_scope.add_mapping(&prefix, namespace_uri);
                current_scope.define_prefix(prefix, namespace_uri);
                break;
            }
        }
    }

    fn namespace_type<'a>(
        &'a self,
        preferred_prefix: Option<&'a str>,
        namespace_uri: &str,
        ignore_default: bool,
    ) -> NamespaceType<'a> {
        if !ignore_default && Some(namespace_uri) == self.active_default_namespace_uri() {
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

/// Write a document, specifying some formatting options
///
/// For example, the default is to use single-quotes for attributes. To use
/// double quotes for attributes, you need to use `set_single_quotes(false)`.
///
/// ```
/// use sxd_document::{Package, writer::Writer};
///
/// // Create a new document
/// let p = Package::new();
/// let doc = p.as_document();
/// let el = doc.create_element("hello");
/// el.set_attribute_value("a", "b");
/// doc.root().append_child(el);
///
/// // Format the document as bytes
/// let mut output = Vec::new();
/// Writer::new().set_single_quotes(false).format_document(&doc, &mut output);
///
/// // Check that the output is correct
/// let output_string = String::from_utf8(output).unwrap();
/// assert_eq!(output_string, r#"<?xml version="1.0"?><hello a="b"/>"#);
/// ```
pub struct Writer {
    single_quotes: bool,
    write_encoding: bool,
}

impl Default for Writer {
    fn default() -> Self {
        Self {
            single_quotes: true,
            write_encoding: false,
        }
    }
}

impl Writer {
    /// Create a new `Writer` with default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set whether single quotes should be used for writing a document.
    pub fn set_single_quotes(mut self, single_quotes: bool) -> Self {
        self.single_quotes = single_quotes;
        self
    }

    /// Set whether the encoding should be specified in the output document header.
    pub fn set_write_encoding(mut self, write_encoding: bool) -> Self {
        self.write_encoding = write_encoding;
        self
    }

    fn quote_char(&self) -> &'static str {
        if self.single_quotes {
            "'"
        } else {
            "\""
        }
    }
}

impl Writer {
    fn format_qname<'d, W: ?Sized>(
        &self,
        q: QName<'d>,
        mapping: &mut PrefixMapping<'d>,
        preferred_prefix: Option<&str>,
        ignore_default: bool,
        writer: &mut W,
    ) -> io::Result<()>
    where
        W: Write,
    {
        // Can something without a namespace be prefixed? No, because
        // defining a prefix requires a non-empty URI
        if let Some(namespace_uri) = q.namespace_uri {
            match mapping.namespace_type(preferred_prefix, namespace_uri, ignore_default) {
                NamespaceType::Default => {
                    // No need to do anything
                }
                NamespaceType::Prefix(prefix) => {
                    writer.write_str(prefix)?;
                    writer.write_str(":")?;
                }
                NamespaceType::Unknown => {
                    panic!("No namespace prefix available for {}", namespace_uri);
                }
            }
        }
        writer.write_str(q.local_part)
    }

    fn format_attribute_value<W: ?Sized>(&self, value: &str, writer: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        for item in value
            .split_keeping_delimiter(|c| c == '<' || c == '>' || c == '&' || c == '\'' || c == '"')
        {
            match item {
                SplitType::Match(t) => writer.write_str(t)?,
                SplitType::Delimiter("<") => writer.write_str("&lt;")?,
                SplitType::Delimiter(">") => writer.write_str("&gt;")?,
                SplitType::Delimiter("&") => writer.write_str("&amp;")?,
                SplitType::Delimiter("'") => writer.write_str("&apos;")?,
                SplitType::Delimiter("\"") => writer.write_str("&quot;")?,
                SplitType::Delimiter(..) => unreachable!(),
            }
        }
        Ok(())
    }

    fn format_element<'d, W: ?Sized>(
        &self,
        element: dom::Element<'d>,
        todo: &mut Vec<Content<'d>>,
        mapping: &mut PrefixMapping<'d>,
        writer: &mut W,
    ) -> io::Result<()>
    where
        W: Write,
    {
        let attrs = element.attributes();

        mapping.populate_scope(&element, &attrs);

        writer.write_str("<")?;
        self.format_qname(
            element.name(),
            mapping,
            element.preferred_prefix(),
            false,
            writer,
        )?;

        for attr in &attrs {
            writer.write_str(" ")?;
            self.format_qname(attr.name(), mapping, attr.preferred_prefix(), true, writer)?;
            write!(writer, "=")?;
            write!(writer, "{}", self.quote_char())?;
            self.format_attribute_value(attr.value(), writer)?;
            write!(writer, "{}", self.quote_char())?;
        }

        if let Some(ns_uri) = mapping.default_namespace_uri_in_current_scope() {
            writer.write_str(" xmlns='")?;
            writer.write_str(ns_uri)?;
            writer.write_str("'")?;
        }

        for &(ref prefix, ref ns_uri) in mapping.prefixes_in_current_scope() {
            writer.write_str(" xmlns:")?;
            writer.write_str(prefix)?;
            write!(writer, "='{}'", ns_uri)?;
        }

        let mut children = element.children();
        if children.is_empty() {
            writer.write_str("/>")?;
            mapping.pop_scope();
            Ok(())
        } else {
            writer.write_str(">")?;

            todo.push(ElementEnd(element));
            children.reverse();
            let x = children.into_iter().map(|c| match c {
                ChildOfElement::Element(element) => Element(element),
                ChildOfElement::Text(t) => Text(t),
                ChildOfElement::Comment(c) => Comment(c),
                ChildOfElement::ProcessingInstruction(p) => ProcessingInstruction(p),
            });
            todo.extend(x);

            Ok(())
        }
    }

    fn format_element_end<'d, W: ?Sized>(
        &self,
        element: dom::Element<'d>,
        mapping: &mut PrefixMapping<'d>,
        writer: &mut W,
    ) -> io::Result<()>
    where
        W: Write,
    {
        writer.write_str("</")?;
        self.format_qname(
            element.name(),
            mapping,
            element.preferred_prefix(),
            false,
            writer,
        )?;
        writer.write_str(">")
    }

    fn format_text<W: ?Sized>(&self, text: dom::Text<'_>, writer: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        for item in text
            .text()
            .split_keeping_delimiter(|c| c == '<' || c == '>' || c == '&')
        {
            match item {
                SplitType::Match(t) => writer.write_str(t)?,
                SplitType::Delimiter("<") => writer.write_str("&lt;")?,
                SplitType::Delimiter(">") => writer.write_str("&gt;")?,
                SplitType::Delimiter("&") => writer.write_str("&amp;")?,
                SplitType::Delimiter(..) => unreachable!(),
            }
        }
        Ok(())
    }

    fn format_comment<W: ?Sized>(&self, comment: dom::Comment<'_>, writer: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        write!(writer, "<!--{}-->", comment.text())
    }

    fn format_processing_instruction<W: ?Sized>(
        &self,
        pi: dom::ProcessingInstruction<'_>,
        writer: &mut W,
    ) -> io::Result<()>
    where
        W: Write,
    {
        match pi.value() {
            None => write!(writer, "<?{}?>", pi.target()),
            Some(v) => write!(writer, "<?{} {}?>", pi.target(), v),
        }
    }

    fn format_one<'d, W: ?Sized>(
        &self,
        content: Content<'d>,
        todo: &mut Vec<Content<'d>>,
        mapping: &mut PrefixMapping<'d>,
        writer: &mut W,
    ) -> io::Result<()>
    where
        W: Write,
    {
        match content {
            Element(e) => {
                mapping.push_scope();
                self.format_element(e, todo, mapping, writer)
            }
            ElementEnd(e) => {
                let r = self.format_element_end(e, mapping, writer);
                mapping.pop_scope();
                r
            }
            Text(t) => self.format_text(t, writer),
            Comment(c) => self.format_comment(c, writer),
            ProcessingInstruction(p) => self.format_processing_instruction(p, writer),
        }
    }

    fn format_body<W: ?Sized>(&self, element: dom::Element<'_>, writer: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        let mut todo = vec![Element(element)];
        let mut mapping = PrefixMapping::new();

        while !todo.is_empty() {
            self.format_one(todo.pop().unwrap(), &mut todo, &mut mapping, writer)?;
        }

        Ok(())
    }

    fn format_declaration<W: ?Sized>(&self, writer: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        write!(
            writer,
            "<?xml version={}1.0{}",
            self.quote_char(),
            self.quote_char()
        )?;

        if self.write_encoding {
            write!(
                writer,
                " encoding={}UTF-8{}",
                self.quote_char(),
                self.quote_char()
            )?;
        }

        write!(writer, "?>")?;

        Ok(())
    }

    /// Formats a document into a Write
    pub fn format_document<'d, W: ?Sized>(
        &self,
        doc: &'d dom::Document<'d>,
        writer: &mut W,
    ) -> io::Result<()>
    where
        W: Write,
    {
        self.format_declaration(writer)?;

        for child in doc.root().children().into_iter() {
            match child {
                ChildOfRoot::Element(e) => self.format_body(e, writer),
                ChildOfRoot::Comment(c) => self.format_comment(c, writer),
                ChildOfRoot::ProcessingInstruction(p) => {
                    self.format_processing_instruction(p, writer)
                }
            }?
        }

        Ok(())
    }
}

/// Formats a document into a `Write` using the default `Writer`
pub fn format_document<'d, W: ?Sized>(doc: &'d dom::Document<'d>, writer: &mut W) -> io::Result<()>
where
    W: Write,
{
    Writer::default().format_document(doc, writer)
}

#[cfg(test)]
mod test {
    use super::{
        super::{dom, Package},
        Writer,
    };

    fn format_xml<'d>(doc: &'d dom::Document<'d>) -> String {
        format_xml_writer(Writer::default(), doc)
    }

    fn format_xml_writer(writer: Writer, doc: &dom::Document<'_>) -> String {
        let mut w = Vec::new();
        writer.format_document(doc, &mut w).expect("Not formatted");
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
        assert_eq!(
            xml,
            "<?xml version='1.0'?><autons0:local-part xmlns:autons0='namespace'/>"
        );
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
        assert_eq!(
            xml,
            "<?xml version='1.0'?><prefix:local-part xmlns:prefix='namespace'/>"
        );
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
    fn element_with_attributes_double_quotes() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value("a", "b");
        d.root().append_child(e);

        let xml = format_xml_writer(Writer::new().set_single_quotes(false), &d);
        assert_eq!(xml, r#"<?xml version="1.0"?><hello a="b"/>"#);
    }

    #[test]
    fn attribute_with_namespace() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value(("namespace", "a"), "b");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(
            xml,
            "<?xml version='1.0'?><hello autons0:a='b' xmlns:autons0='namespace'/>"
        );
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
        assert_eq!(
            xml,
            "<?xml version='1.0'?><hello p:a='b' xmlns:p='namespace'/>"
        );
    }

    #[test]
    fn attribute_with_default_namespace_prefix() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element(("namespace", "hello"));
        e.set_preferred_prefix(Some("p"));
        e.set_default_namespace_uri(Some("namespace"));
        e.set_attribute_value(("namespace", "a"), "b");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(
            xml,
            "<?xml version='1.0'?><hello p:a='b' xmlns='namespace' xmlns:p='namespace'/>"
        );
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
        assert_eq!(
            xml,
            "<?xml version='1.0'?><hello name='&apos;1 &lt; 2&apos; &amp; &quot;4 &gt; 3&quot;'/>"
        );
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
    fn nested_empty_element_with_namespaces() {
        let p = Package::new();
        let d = p.as_document();

        let hello = d.create_element(("outer", "hello"));
        hello.set_default_namespace_uri(Some("outer"));
        hello.set_preferred_prefix(Some("o"));

        let world = d.create_element("world");
        world.set_default_namespace_uri(Some("inner"));

        let empty = d.create_element("empty");
        world.append_child(empty);
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello xmlns='outer' xmlns:o='outer'><world xmlns='inner'><empty/></world></hello>");
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
        assert_eq!(
            xml,
            "<?xml version='1.0'?><p:hello xmlns:p='outer'><p:world xmlns:p='inner'/></p:hello>"
        );
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
        assert_eq!(
            xml,
            "<?xml version='1.0'?><hello>A fine day to you!</hello>"
        );
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
        assert_eq!(
            xml,
            "<?xml version='1.0'?><escaped>1 &lt; 3 &amp; 4 &gt; 2</escaped>"
        );
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
        assert_eq!(
            xml,
            "<?xml version='1.0'?><hello><!-- Fill this in --></hello>"
        );
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
        assert_eq!(
            xml,
            "<?xml version='1.0'?><hello><?display screen?></hello>"
        );
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
    fn declaration_with_encoding() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        d.root().append_child(e);

        let xml = format_xml_writer(Writer::new().set_write_encoding(true), &d);
        assert_eq!(xml, "<?xml version='1.0' encoding='UTF-8'?><hello/>");
    }

    #[test]
    fn declaration_with_encoding_and_double_quotes() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        d.root().append_child(e);

        let xml = format_xml_writer(
            Writer::new()
                .set_write_encoding(true)
                .set_single_quotes(false),
            &d,
        );
        assert_eq!(xml, r#"<?xml version="1.0" encoding="UTF-8"?><hello/>"#);
    }
}
