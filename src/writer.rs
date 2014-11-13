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
//! let mut output = std::io::stdio::stdout();
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

use std::io::IoResult;

use super::dom4;
use super::dom4::{ElementCOE,TextCOE,CommentCOE,ProcessingInstructionCOE};
use super::dom4::{ElementCOR,CommentCOR,ProcessingInstructionCOR};

enum Content<'d> {
    Element(dom4::Element<'d>),
    ElementEnd(dom4::Element<'d>),
    Text(dom4::Text<'d>),
    Comment(dom4::Comment<'d>),
    ProcessingInstruction(dom4::ProcessingInstruction<'d>),
}

fn format_element<'d, W : Writer>(element: dom4::Element<'d>, todo: &mut Vec<Content<'d>>, writer: &mut W) -> IoResult<()> {
    try!(write!(writer, "<{}", element.name()));

    for attr in element.attributes().iter() {
        try!(write!(writer, " {}='{}'", attr.name(), attr.value()));
    }

    let mut children = element.children();
    if children.is_empty() {
        writer.write_str("/>")
    } else {
        try!(writer.write_str(">"));

        todo.push(ElementEnd(element));
        children.reverse();
        let x = children.into_iter().map(|c| match c {
            ElementCOE(element) => Element(element),
            TextCOE(t)          => Text(t),
            CommentCOE(c)       => Comment(c),
            ProcessingInstructionCOE(p)            => ProcessingInstruction(p),
        });
        todo.extend(x);

        Ok(())
    }
}

fn format_comment<W : Writer>(comment: dom4::Comment, writer: &mut W) -> IoResult<()> {
    write!(writer, "<!--{}-->", comment.text())
}

fn format_processing_instruction<W : Writer>(pi: dom4::ProcessingInstruction, writer: &mut W) -> IoResult<()> {
    match pi.value() {
        None    => write!(writer, "<?{}?>", pi.target()),
        Some(v) => write!(writer, "<?{} {}?>", pi.target(), v),
    }
}

fn format_one<'d, W : Writer>(content: Content<'d>, todo: &mut Vec<Content<'d>>, writer: &mut W) -> IoResult<()> {
    match content {
        Element(e)               => format_element(e, todo, writer),
        ElementEnd(e)            => write!(writer, "</{}>", e.name()),
        Text(t)                  => writer.write_str(t.text().as_slice()),
        Comment(c)               => format_comment(c, writer),
        ProcessingInstruction(p) => format_processing_instruction(p, writer),
    }
}

fn format_body<W : Writer>(element: dom4::Element, writer: &mut W) -> IoResult<()> {
    let mut todo = vec![Element(element)];

    while ! todo.is_empty() {
        try!(format_one(todo.pop().unwrap(), &mut todo, writer));
    }

    Ok(())
}

/// Formats a document into a Writer
pub fn format_document<'d, W : Writer>(doc: &'d dom4::Document<'d>, writer: &mut W) -> IoResult<()> {
    try!(writer.write_str("<?xml version='1.0'?>"));

    for child in doc.root().children().into_iter() {
        try!(match child {
            ElementCOR(e) => format_body(e, writer),
            CommentCOR(c) => format_comment(c, writer),
            ProcessingInstructionCOR(p)      => format_processing_instruction(p, writer),
        })
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use std::io::MemWriter;

    use super::super::Package;
    use super::super::dom4;
    use super::format_document;

    macro_rules! assert_str_eq(
        ($l:expr, $r:expr) => (assert_eq!($l.as_slice(), $r.as_slice()));
    )

    fn format_xml<'d>(doc: &'d dom4::Document<'d>) -> String {
        let mut w = MemWriter::new();
        format_document(doc, &mut w).ok().expect("Not formatted");
        String::from_utf8(w.unwrap()).ok().expect("Not a string")
    }

    #[test]
    fn top_element() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello/>");
    }

    #[test]
    fn element_with_attributes() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value("a", "b");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello a='b'/>");
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
        assert_str_eq!(xml, "<?xml version='1.0'?><hello><world/></hello>");
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
        assert_str_eq!(xml, "<?xml version='1.0'?><hello>A fine day to you!</hello>");
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
        assert_str_eq!(xml, "<?xml version='1.0'?><hello><!-- Fill this in --></hello>");
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
        assert_str_eq!(xml, "<?xml version='1.0'?><hello><?display?></hello>");
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
        assert_str_eq!(xml, "<?xml version='1.0'?><hello><?display screen?></hello>");
    }

    #[test]
    fn top_level_comment() {
        let p = Package::new();
        let d = p.as_document();
        let comment = d.create_comment(" Fill this in ");
        d.root().append_child(comment);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><!-- Fill this in -->");
    }

    #[test]
    fn top_level_processing_instruction() {
        let p = Package::new();
        let d = p.as_document();
        let pi = d.create_processing_instruction("display", None);
        d.root().append_child(pi);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><?display?>");
    }
}
