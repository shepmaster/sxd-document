//! Converts XML strings into a DOM structure
//!
//! ### Example
//!
//! ```
//! use document::parser::Parser;
//! let parser = Parser::new();
//! let xml = r#"<?xml version="1.0"?>
//! <!-- Awesome data incoming -->
//! <data awesome="true">
//!   <datum>Science</datum>
//!   <datum><![CDATA[Literature]]></datum>
//!   <datum>Math &gt; others</datum>
//! </data>"#;
//! let doc = parser.parse(xml).ok().expect("Failed to parse");
//! ```
//!
//! ### Error handling
//!
//! When an error occurs in an alternation,
//! we return the most interesting failure.
//!
//! When an error occurs while parsing zero-or-more items,
//! we return the items parsed in addition to the failure point.
//! If the next required item fails,
//! we return the more interesting error of the two.
//!
//! When we have multiple errors,
//! the *most interesting error* is the one that occurred last in the input.
//! We assume that this will be closest to what the user intended.
//!
//! ### Unresolved questions:
//!
//! - Should zero-or-one mimic zero-or-more?
//! - Should we restart from both the failure point and the original start point?
//! - Should we preserve a tree of all the failures?
//!
//! ### Known issues
//!
//! - `panic!` is used in recoverable situations.
//!
//! ### Influences
//!
//! - http://www.scheidecker.net/2012/12/03/parser-combinators/

use std::ascii::AsciiExt;
use std::char::from_u32;
use std::num::from_str_radix;
use std::mem::replace;
use std::collections::HashMap;

use self::AttributeValue::*;
use self::Reference::*;
use self::xmlstr::XmlStr;

use super::QName;
use super::dom4;
use super::peresil::{ParseResult,Point};

mod xmlstr;

#[allow(missing_copy_implementations)]
pub struct Parser;

// TODO: It is proper to compare simply on the prefix?
// Should this work:
// <a xmlns:x1="x" xmlns:x2="x"> <x1:b></x2:b> </a>
// xmllint reports it as an error...
#[deriving(PartialEq,Copy,Show)]
struct PrefixedName<'a> {
    prefix: Option<&'a str>,
    local_part: &'a str,
}

#[deriving(Show,Copy)]
enum AttributeValue<'a> {
    ReferenceAttributeValue(Reference<'a>),
    LiteralAttributeValue(&'a str),
}

#[deriving(Show,Copy)]
enum Reference<'a> {
    EntityReference(&'a str),
    DecimalCharReference(&'a str),
    HexCharReference(&'a str),
}

trait XmlParseExt<'a> {
    fn consume_space(&self) -> ParseResult<'a, &'a str>;
    fn consume_attribute_value(&self, quote: &str) -> ParseResult<'a, &'a str>;
    fn consume_literal(&self, literal: &str) -> ParseResult<'a, &'a str>;
    fn consume_name(&self) -> ParseResult<'a, &'a str>;
    fn consume_ncname(&self) -> ParseResult<'a, &'a str>;
    fn consume_version_num(&self) -> ParseResult<'a, &'a str>;
    fn consume_decimal_chars(&self) -> ParseResult<'a, &'a str>;
    fn consume_hex_chars(&self) -> ParseResult<'a, &'a str>;
    fn consume_char_data(&self) -> ParseResult<'a, &'a str>;
    fn consume_cdata(&self) -> ParseResult<'a, &'a str>;
    fn consume_comment(&self) -> ParseResult<'a, &'a str>;
    fn consume_pi_value(&self) -> ParseResult<'a, &'a str>;
    fn consume_start_tag(&self) -> ParseResult<'a, &'a str>;
}

impl<'a> XmlParseExt<'a> for Point<'a> {
    fn consume_space(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_space())
    }

    fn consume_attribute_value(&self, quote: &str) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_attribute(quote))
    }

    fn consume_literal(&self, literal: &str) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_literal(literal))
    }

    fn consume_name(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_name())
    }

    fn consume_ncname(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_ncname())
    }

    fn consume_version_num(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_version_num())
    }

    fn consume_decimal_chars(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_decimal_chars())
    }

    fn consume_hex_chars(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_hex_chars())
    }

    fn consume_char_data(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_char_data())
    }

    fn consume_cdata(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_cdata())
    }

    fn consume_comment(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_comment())
    }

    fn consume_pi_value(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_pi_value())
    }

    fn consume_start_tag(&self) -> ParseResult<'a, &'a str> {
        self.consume_to(self.s.end_of_start_tag())
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser
    }

    fn parse_prefixed_name<'a>(&self, xml: Point<'a>) -> ParseResult<'a, PrefixedName<'a>> {
        fn parse_local<'a>(xml: Point<'a>) -> ParseResult<'a, &'a str> {
            let (_, xml) = try_parse!(xml.consume_literal(":"));
            xml.consume_ncname()
        }

        let (prefix, xml) = try_parse!(xml.consume_ncname());
        let (local, xml)  = parse_optional!(parse_local(xml), xml);

        let name = match local {
            Some(local) => PrefixedName { prefix: Some(prefix), local_part: local },
            None        => PrefixedName { prefix: None, local_part: prefix },
        };

        ParseResult::Success(name, xml)
    }

    fn parse_eq<'a>(&self, xml: Point<'a>) -> ParseResult<'a, ()> {
        let (_, xml) = parse_optional!(xml.consume_space(), xml);
        let (_, xml) = try_parse!(xml.consume_literal("="));
        let (_, xml) = parse_optional!(xml.consume_space(), xml);

        ParseResult::Success((), xml)
    }

    fn parse_version_info<'a>(&self, xml: Point<'a>) -> ParseResult<'a, &'a str> {
        let (_, xml) = try_parse!(xml.consume_space());
        let (_, xml) = try_parse!(xml.consume_literal("version"));
        let (_, xml) = try_parse!(self.parse_eq(xml));
        let (version, xml) = try_parse!(
            self.parse_quoted_value(xml, |xml, _| xml.consume_version_num())
        );

        ParseResult::Success(version, xml)
    }

    fn parse_xml_declaration<'a>(&self, xml: Point<'a>) -> ParseResult<'a, ()> {
        let (_, xml) = try_parse!(xml.consume_literal("<?xml"));
        let (_version, xml) = try_parse!(self.parse_version_info(xml));
        // let (encoding, xml) = parse_optional!(self.parse_encoding_declaration(xml));
        // let (standalone, xml) = parse_optional!(self.parse_standalone_declaration(xml));
        let (_, xml) = parse_optional!(xml.consume_space(), xml);
        let (_, xml) = try_parse!(xml.consume_literal("?>"));

        ParseResult::Success((), xml)
    }

    fn parse_misc<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S) -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        parse_alternate!(xml, {
            [|xml: Point<'a>| self.parse_comment(xml, sink) -> |_| ()],
            [|xml: Point<'a>| self.parse_pi(xml, sink)      -> |_| ()],
            [|xml: Point<'a>| xml.consume_space()           -> |_| ()],
        })
    }

    fn parse_miscs<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S) -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        parse_zero_or_more!(xml, |xml| self.parse_misc(xml, sink))
    }

    fn parse_prolog<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S) -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (_, xml) = parse_optional!(self.parse_xml_declaration(xml), xml);
        self.parse_miscs(xml, sink)
    }

    fn parse_one_quoted_value<'a, T>(&self,
                                     xml: Point<'a>,
                                     quote: &str,
                                     f: |Point<'a>| -> ParseResult<'a, T>)
                                     -> ParseResult<'a, T>
    {
        let (_, xml) = try_parse!(xml.consume_literal(quote));
        let (value, f, xml) = try_partial_parse!(f(xml));
        let (_, xml) = try_resume_after_partial_failure!(f, xml.consume_literal(quote));

        ParseResult::Success(value, xml)
    }

    fn parse_quoted_value<'a, T>(&self,
                                 xml: Point<'a>,
                                 f: |Point<'a>, &str| -> ParseResult<'a, T>)
                                 -> ParseResult<'a, T>
    {
        parse_alternate!(xml, {
            [|xml| self.parse_one_quoted_value(xml, "'",  |xml| f(xml, "'"))  -> |v| v],
            [|xml| self.parse_one_quoted_value(xml, "\"", |xml| f(xml, "\"")) -> |v| v],
        })
    }

    fn parse_attribute_values<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S, quote: &str)
                                         -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        parse_zero_or_more!(xml, |xml|
            parse_alternate!(xml, {
                [|xml: Point<'a>| xml.consume_attribute_value(quote) -> |v| sink.attribute_value(LiteralAttributeValue(v))],
                [|xml: Point<'a>| self.parse_reference(xml)          -> |e| sink.attribute_value(ReferenceAttributeValue(e))],
            }))
    }

    fn parse_attribute<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S)
                                  -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (_, xml) = try_parse!(xml.consume_space());

        let (name, xml) = try_parse!(self.parse_prefixed_name(xml));

        sink.attribute_start(name);

        let (_, xml) = try_parse!(self.parse_eq(xml));

        let (_, xml) = try_parse!(
            self.parse_quoted_value(xml, |xml, quote| self.parse_attribute_values(xml, sink, quote))
        );

        sink.attribute_end(name);

        ParseResult::Success((), xml)
    }

    fn parse_attributes<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S)
                                   -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        parse_zero_or_more!(xml, |xml| self.parse_attribute(xml, sink))
    }

    fn parse_element_end<'a>(&self, xml: Point<'a>) -> ParseResult<'a, PrefixedName<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("</"));
        let (name, xml) = try_parse!(self.parse_prefixed_name(xml));
        let (_, xml) = parse_optional!(xml.consume_space(), xml);
        let (_, xml) = try_parse!(xml.consume_literal(">"));
        ParseResult::Success(name, xml)
    }

    fn parse_char_data<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S)
                                  -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (text, xml) = try_parse!(xml.consume_char_data());

        sink.text(text);

        ParseResult::Success((), xml)
    }

    fn parse_cdata<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S)
                              -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (_, xml) = try_parse!(xml.consume_literal("<![CDATA["));
        let (text, xml) = try_parse!(xml.consume_cdata());
        let (_, xml) = try_parse!(xml.consume_literal("]]>"));

        sink.text(text);

        ParseResult::Success((), xml)
    }

    fn parse_entity_ref<'a>(&self, xml: Point<'a>) -> ParseResult<'a, Reference<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("&"));
        let (name, xml) = try_parse!(xml.consume_name());
        let (_, xml) = try_parse!(xml.consume_literal(";"));

        ParseResult::Success(EntityReference(name), xml)
    }

    fn parse_decimal_char_ref<'a>(&self, xml: Point<'a>) -> ParseResult<'a, Reference<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("&#"));
        let (dec, xml) = try_parse!(xml.consume_decimal_chars());
        let (_, xml) = try_parse!(xml.consume_literal(";"));

        ParseResult::Success(DecimalCharReference(dec), xml)
    }

    fn parse_hex_char_ref<'a>(&self, xml: Point<'a>) -> ParseResult<'a, Reference<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("&#x"));
        let (hex, xml) = try_parse!(xml.consume_hex_chars());
        let (_, xml) = try_parse!(xml.consume_literal(";"));

        ParseResult::Success(HexCharReference(hex), xml)
    }

    fn parse_reference<'a>(&self, xml: Point<'a>) -> ParseResult<'a, Reference<'a>> {
        parse_alternate!(xml, {
            [|xml| self.parse_entity_ref(xml)       -> |e| e],
            [|xml| self.parse_decimal_char_ref(xml) -> |d| d],
            [|xml| self.parse_hex_char_ref(xml)     -> |h| h],
        })
    }

    fn parse_comment<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S) -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (_, xml) = try_parse!(xml.consume_literal("<!--"));
        let (text, xml) = try_parse!(xml.consume_comment());
        let (_, xml) = try_parse!(xml.consume_literal("-->"));

        sink.comment(text);

        ParseResult::Success((), xml)
    }

    fn parse_pi_value<'a>(&self, xml: Point<'a>) -> ParseResult<'a, &'a str> {
        let (_, xml) = try_parse!(xml.consume_space());
        xml.consume_pi_value()
    }

    fn parse_pi<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S) -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (_, xml) = try_parse!(xml.consume_literal("<?"));
        let (target, xml) = try_parse!(xml.consume_name());
        let (value, xml) = parse_optional!(self.parse_pi_value(xml), xml);
        let (_, xml) = try_parse!(xml.consume_literal("?>"));

        if target.eq_ignore_ascii_case("xml") {
            panic!("Can't use xml as a PI target");
        }

        sink.processing_instruction(target, value);

        ParseResult::Success((), xml)
    }

    fn parse_content<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S) -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (_, xml) = parse_optional!(self.parse_char_data(xml, sink), xml);

        // Pattern: zero-or-more
        let mut start = xml;
        loop {
            let xxx = parse_alternate!(start, {
                [|xml| self.parse_element(xml, sink) -> |_| ()],
                [|xml| self.parse_cdata(xml, sink)   -> |_| ()],
                [|xml| self.parse_reference(xml)     -> |r| sink.reference(r)],
                [|xml| self.parse_comment(xml, sink) -> |_| ()],
                [|xml| self.parse_pi(xml, sink)      -> |_| ()],
            });

            let (_, after) = match xxx {
                ParseResult::Success(v, xml) => (v, xml),
                ParseResult::Partial(_, pf, _) |
                ParseResult::Failure(pf) => return ParseResult::Partial((), pf, start),
            };

            let (_, xml) = parse_optional!(self.parse_char_data(after, sink), after);

            start = xml;
        }
    }

    fn parse_empty_element_tail<'a>(&self, xml: Point<'a>) -> ParseResult<'a, ()> {
        let (_, xml) = try_parse!(xml.consume_literal("/>"));

        ParseResult::Success((), xml)
    }

    fn parse_non_empty_element_tail<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S, start_name: PrefixedName<'a>)
                                               -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (_, xml) = try_parse!(xml.consume_literal(">"));

        let (_, f, xml) = try_partial_parse!(self.parse_content(xml, sink));

        let (end_name, xml) = try_resume_after_partial_failure!(f, self.parse_element_end(xml));

        if start_name != end_name {
            panic!("tags do not match!");
        }

        ParseResult::Success((), xml)
    }

    fn parse_element<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S) -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (_, xml) = try_parse!(xml.consume_start_tag());
        let (name, xml) = try_parse!(self.parse_prefixed_name(xml));

        sink.element_start(name);

        sink.attributes_start();
        let (_, f, xml) = try_partial_parse!(self.parse_attributes(xml, sink));
        sink.attributes_end();

        let (_, xml) = parse_optional!(xml.consume_space(), xml);

        let (_, xml) = try_resume_after_partial_failure!(f,
            parse_alternate!(xml, {
                [|xml| self.parse_empty_element_tail(xml)                 -> |_| ()],
                [|xml| self.parse_non_empty_element_tail(xml, sink, name) -> |_| ()],
            })
        );

        sink.element_end(name);

        ParseResult::Success((), xml)
    }

    fn parse_document<'a, 's, S>(&self, xml: Point<'a>, sink: &'s mut S)
                                 -> ParseResult<'a, ()>
        where S: ParserSink<'a>
    {
        let (_, f, xml) = try_partial_parse!(self.parse_prolog(xml, sink));
        let (_, xml) = try_resume_after_partial_failure!(f, self.parse_element(xml, sink));
        let (_, xml) = parse_optional!(self.parse_miscs(xml, sink), xml);

        ParseResult::Success((), xml)
    }

    pub fn parse<'a>(&self, xml: &'a str) -> Result<super::Package, uint> {
        let xml = Point{offset: 0, s: xml};
        let package = super::Package::new();

        {
            let doc = package.as_document();
            let mut hydrator = SaxHydrator::new(&doc);

            match self.parse_document(xml, &mut hydrator) {
                ParseResult::Success(v, xml) => (v, xml),
                ParseResult::Partial(_, pf, _) |
                ParseResult::Failure(pf) => return Err(pf.point.offset),
            };
        }

        // TODO: Check fully parsed

        Ok(package)
    }
}

trait ParserSink<'x> {
    fn element_start(&mut self, name: PrefixedName<'x>);
    fn element_end(&mut self, name: PrefixedName<'x>);
    fn comment(&mut self, text: &'x str);
    fn processing_instruction(&mut self, target: &'x str, value: Option<&'x str>);
    fn text(&mut self, text: &'x str);
    fn reference(&mut self, reference: Reference<'x>);
    fn attributes_start(&mut self);
    fn attributes_end(&mut self);
    fn attribute_start(&mut self, name: PrefixedName<'x>);
    fn attribute_value(&mut self, value: AttributeValue<'x>);
    fn attribute_end(&mut self, name: PrefixedName<'x>);
}


fn decode_reference<T>(ref_data: Reference, cb: |&str| -> T) -> T {
    match ref_data {
        DecimalCharReference(d) => {
            let code: u32 = from_str_radix(d, 10).expect("Not valid decimal");
            let c: char = from_u32(code).expect("Not a valid codepoint");
            let s = String::from_char(1, c);
            cb(s.as_slice())
        },
        HexCharReference(h) => {
            let code: u32 = from_str_radix(h, 16).expect("Not valid hex");
            let c: char = from_u32(code).expect("Not a valid codepoint");
            let s = String::from_char(1, c);
            cb(s.as_slice())
        },
        EntityReference(e) => {
            let s = match e {
                "amp"  => "&",
                "lt"   => "<",
                "gt"   => ">",
                "apos" => "'",
                "quot" => "\"",
                _      => panic!("unknown entity"),
            };
            cb(s)
        }
    }
}

struct AttributeValueBuilder {
    value: String,
}

impl AttributeValueBuilder {
    fn convert(values: &Vec<AttributeValue>) -> String {
        let mut builder = AttributeValueBuilder::new();
        builder.ingest(values);
        builder.implode()
    }

    fn new() -> AttributeValueBuilder {
        AttributeValueBuilder {
            value: String::new(),
        }
    }

    fn ingest(&mut self, values: &Vec<AttributeValue>) {
        for value in values.iter() {
            match value {
                &LiteralAttributeValue(v) => self.value.push_str(v),
                &ReferenceAttributeValue(r) => decode_reference(r, |s| self.value.push_str(s)),
            }
        }
    }

    fn clear(&mut self) {
        self.value.clear();
    }

    fn as_slice(&self) -> &str {
        self.value.as_slice()
    }

    fn implode(self) -> String {
        self.value
    }
}

struct DeferredAttribute<'d> {
    name: PrefixedName<'d>,
    values: Vec<AttributeValue<'d>>,
}

struct SaxHydrator<'d, 'x> {
    doc: &'d dom4::Document<'d>,
    stack: Vec<dom4::Element<'d>>,
    element: Option<PrefixedName<'x>>,
    attributes: Vec<DeferredAttribute<'x>>,
}

impl<'d, 'x> SaxHydrator<'d, 'x> {
    fn new(doc: &'d dom4::Document<'d>) -> SaxHydrator<'d, 'x> {
        SaxHydrator {
            doc: doc,
            stack: Vec::new(),
            element: None,
            attributes: Vec::new(),
        }
    }

    fn current_element(&self) -> &dom4::Element<'d> {
        self.stack.last().expect("No element to append to")
    }

    fn namespace_uri_for_prefix(&self, prefix: &str) -> Option<&str> {
        self.stack.last().and_then(|e| e.namespace_uri_for_prefix(prefix))
    }

    fn append_text(&self, text: dom4::Text<'d>) {
        self.current_element().append_child(text);
    }

    fn append_to_either<T>(&self, child: T)
        where T: dom4::ToChildOfRoot<'d>
    {
        match self.stack.last() {
            None => self.doc.root().append_child(child),
            Some(parent) => parent.append_child(child.to_child_of_root()),
        }
    }
}

impl<'d, 'x> ParserSink<'x> for SaxHydrator<'d, 'x> {
    fn element_start(&mut self, name: PrefixedName<'x>) {
        self.element = Some(name);
    }

    fn element_end(&mut self, _name: PrefixedName) {
        self.stack.pop();
    }

    fn comment(&mut self, text: &str) {
        let comment = self.doc.create_comment(text);
        self.append_to_either(comment);
    }

    fn processing_instruction(&mut self, target: &str, value: Option<&str>) {
        let pi = self.doc.create_processing_instruction(target, value);
        self.append_to_either(pi);
    }

    fn text(&mut self, text: &str) {
        let text = self.doc.create_text(text);
        self.append_text(text);
    }

    fn reference(&mut self, reference: Reference) {
        let text = decode_reference(reference, |s| self.doc.create_text(s));
        self.append_text(text);
    }

    fn attributes_start(&mut self) {
    }

    fn attributes_end(&mut self) {
        let deferred_element = self.element.take().unwrap();

        let deferred_attributes = replace(&mut self.attributes, Vec::new());
        let (namespaces, attributes) = deferred_attributes.partition(|attr| {
            // TODO: Default namespace
            attr.name.prefix == Some("xmlns")
        });

        let mut new_prefix_mappings = HashMap::new();
        for ns in namespaces.iter() {
            let value = AttributeValueBuilder::convert(&ns.values);
            new_prefix_mappings.insert(ns.name.local_part, value);
        }
        let new_prefix_mappings = new_prefix_mappings;

        let element = if let Some(prefix) = deferred_element.prefix {
            if let Some(ns_uri) = new_prefix_mappings.get(prefix).map(|p| p.as_slice()).or_else(|| self.namespace_uri_for_prefix(prefix)) {
                let name = QName::with_namespace_uri(Some(ns_uri),
                                                     deferred_element.local_part);
                let element = self.doc.create_element(name);
                element.set_preferred_prefix(Some(prefix));
                element
            } else {
                panic!("Unknown namespace prefix '{}'", prefix);
            }
        } else {
            self.doc.create_element(deferred_element.local_part)
        };

        for (prefix, ns_uri) in new_prefix_mappings.iter() {
            element.register_prefix(*prefix, ns_uri.as_slice());
        }

        self.append_to_either(element);
        self.stack.push(element);

        let mut builder = AttributeValueBuilder::new();

        for attribute in attributes.iter() {
            builder.clear();
            builder.ingest(&attribute.values);
            let value = builder.as_slice();

            if let Some(prefix) = attribute.name.prefix {
                if let Some(ns_uri) = new_prefix_mappings.get(prefix).map(|p| p.as_slice()).or_else(|| self.namespace_uri_for_prefix(prefix)) {
                    let name = QName::with_namespace_uri(Some(ns_uri.as_slice()),
                                                         attribute.name.local_part);
                    let attr = element.set_attribute_value(name, value);
                    attr.set_preferred_prefix(Some(prefix));
                } else {
                    panic!("Unknown namespace prefix '{}'", prefix);
                }
            } else {
                element.set_attribute_value(attribute.name.local_part, value);
            }
        }
    }

    fn attribute_start(&mut self, name: PrefixedName<'x>) {
        let attr = DeferredAttribute { name: name, values: Vec::new() };
        self.attributes.push(attr);
    }

    fn attribute_value(&mut self, value: AttributeValue<'x>) {
        self.attributes.last_mut().unwrap().values.push(value);
    }

    fn attribute_end(&mut self, _name: PrefixedName) {
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use super::super::{Package,QName,ToQName};
    use super::super::dom4;

    macro_rules! assert_qname_eq(
        ($l:expr, $r:expr) => (assert_eq!($l.to_qname(), $r.to_qname()));
    );

    macro_rules! assert_str_eq(
        ($l:expr, $r:expr) => (assert_eq!($l.as_slice(), $r.as_slice()));
    );

    fn full_parse(xml: &str) -> Result<Package, uint> {
        Parser::new()
            .parse(xml)
    }

    fn quick_parse(xml: &str) -> Package {
        full_parse(xml)
            .ok()
            .expect("Failed to parse the XML string")
    }

    fn top<'d>(doc: &'d dom4::Document<'d>) -> dom4::Element<'d> {
        doc.root().children()[0].element().unwrap()
    }

    #[test]
    fn a_document_with_a_prolog() {
        let package = quick_parse("<?xml version='1.0' ?><hello />");
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn a_document_with_a_prolog_with_double_quotes() {
        let package = quick_parse("<?xml version=\"1.0\" ?><hello />");
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn a_document_with_a_single_element() {
        let package = quick_parse("<hello />");
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn an_element_with_a_namespace() {
        let package = quick_parse("<ns:hello xmlns:ns='namespace'/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_eq!(top.preferred_prefix(), Some("ns"));
        assert_eq!(QName::with_namespace_uri(Some("namespace"), "hello"), top.name());
    }

    #[test]
    fn an_element_with_an_attribute() {
        let package = quick_parse("<hello scope='world'/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_str_eq!(top.attribute_value("scope").unwrap(), "world");
    }

    #[test]
    fn an_element_with_an_attribute_using_double_quotes() {
        let package = quick_parse("<hello scope=\"world\"/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_str_eq!(top.attribute_value("scope").unwrap(), "world");
    }

    #[test]
    fn an_element_with_multiple_attributes() {
        let package = quick_parse("<hello scope='world' happy='true'/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_str_eq!(top.attribute_value("scope").unwrap(), "world");
        assert_str_eq!(top.attribute_value("happy").unwrap(), "true");
    }

    #[test]
    fn an_attribute_with_a_namespace() {
        let package = quick_parse("<hello ns:a='b' xmlns:ns='namespace'/>");
        let doc = package.as_document();
        let top = top(&doc);

        let attr = top.attribute(QName::with_namespace_uri(Some("namespace"), "a")).unwrap();

        assert_eq!(attr.preferred_prefix(), Some("ns"));
        assert_eq!(attr.value(), "b");
    }

    #[test]
    fn an_attribute_with_references() {
        let package = quick_parse("<log msg='I &lt;3 math' />");
        let doc = package.as_document();
        let top = top(&doc);

        assert_str_eq!(top.attribute_value("msg").unwrap(), "I <3 math");
    }

    #[test]
    fn an_element_that_is_not_self_closing() {
        let package = quick_parse("<hello></hello>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn nested_elements() {
        let package = quick_parse("<hello><world/></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        assert_qname_eq!(world.name(), "world");
    }

    #[test]
    fn multiply_nested_elements() {
        let package = quick_parse("<hello><awesome><world/></awesome></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let awesome = hello.children()[0].element().unwrap();
        let world = awesome.children()[0].element().unwrap();

        assert_qname_eq!(world.name(), "world");
    }

    #[test]
    fn nested_elements_with_namespaces() {
        let package = quick_parse("<ns1:hello xmlns:ns1='outer'><ns2:world xmlns:ns2='inner'/></ns1:hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        assert_qname_eq!(world.name(), QName::with_namespace_uri(Some("inner"), "world"));
    }

    #[test]
    fn nested_elements_with_inherited_namespaces() {
        let package = quick_parse("<ns1:hello xmlns:ns1='outer'><ns1:world/></ns1:hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        assert_eq!(world.preferred_prefix(), Some("ns1"));
        assert_eq!(world.name(), QName::with_namespace_uri(Some("outer"), "world"));
    }

    #[test]
    fn nested_elements_with_attributes() {
        let package = quick_parse("<hello><world name='Earth'/></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        assert_str_eq!(world.attribute_value("name").unwrap(), "Earth");
    }

    #[test]
    fn nested_elements_with_attributes_with_inherited_namespaces() {
        let package = quick_parse("<hello xmlns:ns1='outer'><world ns1:name='Earth'/></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        let attr = world.attribute(QName::with_namespace_uri(Some("outer"), "name")).unwrap();

        assert_eq!(attr.preferred_prefix(), Some("ns1"));
        assert_eq!(attr.value(), "Earth");
    }

    #[test]
    fn element_with_text() {
        let package = quick_parse("<hello>world</hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let text = hello.children()[0].text().unwrap();

        assert_str_eq!(text.text(), "world");
    }

    #[test]
    fn element_with_cdata() {
        let package = quick_parse("<words><![CDATA[I have & and < !]]></words>");
        let doc = package.as_document();
        let words = top(&doc);
        let text = words.children()[0].text().unwrap();

        assert_str_eq!(text.text(), "I have & and < !");
    }

    #[test]
    fn element_with_comment() {
        let package = quick_parse("<hello><!-- A comment --></hello>");
        let doc = package.as_document();
        let words = top(&doc);
        let comment = words.children()[0].comment().unwrap();

        assert_str_eq!(comment.text(), " A comment ");
    }

    #[test]
    fn comment_before_top_element() {
        let package = quick_parse("<!-- A comment --><hello />");
        let doc = package.as_document();
        let comment = doc.root().children()[0].comment().unwrap();

        assert_str_eq!(comment.text(), " A comment ");
    }

    #[test]
    fn multiple_comments_before_top_element() {
        let xml = r"
<!--Comment 1-->
<!--Comment 2-->
<hello />";
        let package = quick_parse(xml);
        let doc = package.as_document();
        let comment1 = doc.root().children()[0].comment().unwrap();
        let comment2 = doc.root().children()[1].comment().unwrap();

        assert_str_eq!(comment1.text(), "Comment 1");
        assert_str_eq!(comment2.text(), "Comment 2");
    }

    #[test]
    fn multiple_comments_after_top_element() {
        let xml = r"
<hello />
<!--Comment 1-->
<!--Comment 2-->";
        let package = quick_parse(xml);
        let doc = package.as_document();
        let comment1 = doc.root().children()[1].comment().unwrap();
        let comment2 = doc.root().children()[2].comment().unwrap();

        assert_str_eq!(comment1.text(), "Comment 1");
        assert_str_eq!(comment2.text(), "Comment 2");
    }

    #[test]
    fn element_with_processing_instruction() {
        let package = quick_parse("<hello><?device?></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let pi = hello.children()[0].processing_instruction().unwrap();

        assert_str_eq!(pi.target(), "device");
        assert_eq!(pi.value(), None);
    }

    #[test]
    fn top_level_processing_instructions() {
        let xml = r"
<?output printer?>
<hello />
<?validated?>";

        let package = quick_parse(xml);
        let doc = package.as_document();
        let pi1 = doc.root().children()[0].processing_instruction().unwrap();
        let pi2 = doc.root().children()[2].processing_instruction().unwrap();

        assert_str_eq!(pi1.target(), "output");
        assert_str_eq!(pi1.value().unwrap(), "printer");

        assert_str_eq!(pi2.target(), "validated");
        assert_eq!(pi2.value(), None);
    }

    #[test]
    fn element_with_decimal_char_reference() {
        let package = quick_parse("<math>2 &#62; 1</math>");
        let doc = package.as_document();
        let math = top(&doc);
        let text1 = math.children()[0].text().unwrap();
        let text2 = math.children()[1].text().unwrap();
        let text3 = math.children()[2].text().unwrap();

        assert_str_eq!(text1.text(), "2 ");
        assert_str_eq!(text2.text(), ">");
        assert_str_eq!(text3.text(), " 1");
    }

    #[test]
    fn element_with_hexidecimal_char_reference() {
        let package = quick_parse("<math>1 &#x3c; 2</math>");
        let doc = package.as_document();
        let math = top(&doc);
        let text1 = math.children()[0].text().unwrap();
        let text2 = math.children()[1].text().unwrap();
        let text3 = math.children()[2].text().unwrap();

        assert_str_eq!(text1.text(), "1 ");
        assert_str_eq!(text2.text(), "<");
        assert_str_eq!(text3.text(), " 2");
    }

    #[test]
    fn element_with_entity_reference() {
        let package = quick_parse("<math>I &lt;3 math</math>");
        let doc = package.as_document();
        let math = top(&doc);
        let text1 = math.children()[0].text().unwrap();
        let text2 = math.children()[1].text().unwrap();
        let text3 = math.children()[2].text().unwrap();

        assert_str_eq!(text1.text(), "I ");
        assert_str_eq!(text2.text(), "<");
        assert_str_eq!(text3.text(), "3 math");
    }

    #[test]
    fn element_with_mixed_children() {
        let package = quick_parse("<hello>to <!--fixme--><a><![CDATA[the]]></a><?world?></hello>");
        let doc = package.as_document();
        let hello = top(&doc);

        let text    = hello.children()[0].text().unwrap();
        let comment = hello.children()[1].comment().unwrap();
        let element = hello.children()[2].element().unwrap();
        let pi      = hello.children()[3].processing_instruction().unwrap();

        assert_str_eq!(text.text(),    "to ");
        assert_str_eq!(comment.text(), "fixme");
        assert_qname_eq!(element.name(), "a");
        assert_str_eq!(pi.target(),    "world");
    }

    #[test]
    fn failure_no_open_brace() {
        let r = full_parse("hi />");

        assert_eq!(r, Err(0));
    }

    #[test]
    fn failure_unclosed_tag() {
        let r = full_parse("<hi");

        assert_eq!(r, Err(3));
    }

    #[test]
    fn failure_unexpected_space() {
        let r = full_parse("<hi / >");

        assert_eq!(r, Err(4));
    }

    #[test]
    fn failure_attribute_without_open_quote() {
        let r = full_parse("<hi oops=value' />");
        assert_eq!(r, Err(9));
    }

    #[test]
    fn failure_attribute_without_close_quote() {
        let r = full_parse("<hi oops='value />");

        assert_eq!(r, Err(18));
    }

    #[test]
    fn failure_unclosed_attribute_and_tag() {
        let r = full_parse("<hi oops='value");

        assert_eq!(r, Err(15));
    }

    #[test]
    fn failure_nested_unclosed_tag() {
        let r = full_parse("<hi><oops</hi>");

        assert_eq!(r, Err(9));
    }

    #[test]
    fn failure_nested_unexpected_space() {
        let r = full_parse("<hi><oops / ></hi>");

        assert_eq!(r, Err(10));
    }

    #[test]
    fn failure_malformed_entity_reference() {
        let r = full_parse("<hi>Entity: &;</hi>");

        assert_eq!(r, Err(13));
    }

    #[test]
    fn failure_nested_malformed_entity_reference() {
        let r = full_parse("<hi><bye>Entity: &;</bye></hi>");

        assert_eq!(r, Err(18));
    }

    #[test]
    fn failure_nested_attribute_without_open_quote() {
        let r = full_parse("<hi><bye oops=value' /></hi>");
        assert_eq!(r, Err(14));
    }

    #[test]
    fn failure_nested_attribute_without_close_quote() {
        let r = full_parse("<hi><bye oops='value /></hi>");

        assert_eq!(r, Err(23));
    }

    #[test]
    fn failure_nested_unclosed_attribute_and_tag() {
        let r = full_parse("<hi><bye oops='value</hi>");

        assert_eq!(r, Err(20));
    }
}
