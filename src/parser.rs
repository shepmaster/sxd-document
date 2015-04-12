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
use std::collections::HashMap;
use std::mem::replace;
use std::ops::Deref;
use std::{char,iter};

use peresil::{self,StringPoint,ParseMaster,Recoverable};

use self::AttributeValue::*;
use self::Reference::*;

use super::PrefixedName;
use super::dom4;
use super::str::XmlStr;

#[derive(Debug,Copy,Clone,PartialEq)]
pub enum Error {
    Expected(&'static str),

    ExpectedAttribute,
    ExpectedAttributeValue,

    ExpectedCData,

    ExpectedCharacterData,

    ExpectedComment,
    ExpectedCommentBody,

    ExpectedElement,
    ExpectedElementName,
    ExpectedElementEnd,
    ExpectedElementSelfClosed,

    ExpectedProcessingInstruction,
    ExpectedProcessingInstructionTarget,
    ExpectedProcessingInstructionValue,

    ExpectedVersionNumber,
    ExpectedWhitespace,

    ExpectedClosingQuote(&'static str),
    ExpectedOpeningQuote(&'static str),

    ExpectedDecimalReferenceValue,
    ExpectedHexReferenceValue,
    ExpectedNamedReferenceValue,

    ExpectedDecimalReference,
    ExpectedHexReference,
    ExpectedNamedReference,

    InvalidProcessingInstructionTarget,
    MismatchedElementEndName,

    InvalidDecimalReference,
    InvalidHexReference,
    UnknownNamedReference,
}

impl Recoverable for Error {
    fn recoverable(&self) -> bool { true }
}

macro_rules! adapt_sink_result(
    ($r:expr) => ({
        match $r {
            Ok(v) => v,
            Err((span, e)) => {
                return peresil::Progress { point: span.into_point(), status: peresil::Status::Failure(e) }
            },
        }
    });
);

type XmlMaster<'a> = peresil::ParseMaster<StringPoint<'a>, Error>;
type XmlProgress<'a, T> = peresil::Progress<StringPoint<'a>, T, Error>;

fn success<'a, T>(data: T, point: StringPoint<'a>) -> XmlProgress<'a, T> {
    peresil::Progress { point: point, status: peresil::Status::Success(data) }
}

/// Parses XML strings
#[allow(missing_copy_implementations)]
pub struct Parser;

/// A truncated point; the string ends before the end of input
#[derive(Debug,Copy,Clone)]
struct Span<'a> {
    s: &'a str,
    offset: usize,
}

impl<'a> Span<'a> {
    fn new(a: StringPoint<'a>, b: StringPoint<'a>) -> Span<'a> {
        let len = b.offset - a.offset;
        Span { s: &a.s[0..len], offset: a.offset }
    }

    // Should this just be the return type of the parser methods?
    fn parse<'z, F, T>(pt: StringPoint<'z>, f: F) -> XmlProgress<'z, Span<'z>>
        where F: Fn(StringPoint<'z>) -> XmlProgress<'z, T>
    {
        let (after, _) = try_parse!(f(pt));
        let span = Span::new(pt, after);
        peresil::Progress { status: peresil::Status::Success(span), point: after }
    }

    fn into_point(self) -> StringPoint<'a> {
        peresil::StringPoint { offset: self.offset, s: self.s }
    }
}

#[derive(Debug,Copy,Clone)]
enum AttributeValue<'a> {
    ReferenceAttributeValue(Reference<'a>),
    LiteralAttributeValue(&'a str),
}

#[derive(Debug,Copy,Clone)]
enum Reference<'a> {
    EntityReference(Span<'a>),
    DecimalCharReference(Span<'a>),
    HexCharReference(Span<'a>),
}

/// Common reusable XML parsing methods
pub trait XmlParseExt<'a> {
    /// Parse XML whitespace
    fn consume_space(&self) -> peresil::Progress<StringPoint<'a>, &'a str, ()>;
    /// Parse XML decimal characters
    fn consume_decimal_chars(&self) -> peresil::Progress<StringPoint<'a>, &'a str, ()>;
    /// Parse an XML [NCName](http://www.w3.org/TR/REC-xml-names/#NT-NCName)
    fn consume_ncname(&self) -> peresil::Progress<StringPoint<'a>, &'a str, ()>;
    /// Parse an XML [prefixed name](http://www.w3.org/TR/REC-xml-names/#NT-QName)
    fn consume_prefixed_name(&self) -> peresil::Progress<StringPoint<'a>, PrefixedName<'a>, ()>;
}

impl<'a> XmlParseExt<'a> for StringPoint<'a> {
    fn consume_space(&self) -> peresil::Progress<StringPoint<'a>, &'a str, ()> {
        self.consume_to(self.s.end_of_space())
    }

    fn consume_decimal_chars(&self) -> peresil::Progress<StringPoint<'a>, &'a str, ()> {
        self.consume_to(self.s.end_of_decimal_chars())
    }

    fn consume_ncname(&self) -> peresil::Progress<StringPoint<'a>, &'a str, ()> {
        self.consume_to(self.s.end_of_ncname())
    }

    fn consume_prefixed_name(&self) -> peresil::Progress<StringPoint<'a>, PrefixedName<'a>, ()> {
        fn parse_local<'a>(xml: StringPoint<'a>) -> peresil::Progress<StringPoint<'a>, &'a str, ()> {
            let (xml, _) = try_parse!(xml.consume_literal(":"));
            xml.consume_ncname()
        }

        let (xml, prefix) = try_parse!(self.consume_ncname());
        let (xml, local)  = parse_local(xml).optional(xml);

        let name = match local {
            Some(local) => PrefixedName::with_prefix(Some(prefix),local),
            None        => PrefixedName::new(prefix),
        };

        peresil::Progress { point: xml, status: peresil::Status::Success(name) }
    }
}

trait PrivateXmlParseExt<'a> {
    fn consume_attribute_value(&self, quote: &str) -> XmlProgress<'a, &'a str>;
    fn consume_name(&self) -> peresil::Progress<StringPoint<'a>, &'a str, ()>;
    fn consume_hex_chars(&self) -> XmlProgress<'a, &'a str>;
    fn consume_char_data(&self) -> XmlProgress<'a, &'a str>;
    fn consume_cdata(&self) -> XmlProgress<'a, &'a str>;
    fn consume_comment(&self) -> XmlProgress<'a, &'a str>;
    fn consume_pi_value(&self) -> XmlProgress<'a, &'a str>;
    fn consume_start_tag(&self) -> XmlProgress<'a, &'a str>;
}

impl<'a> PrivateXmlParseExt<'a> for StringPoint<'a> {
    fn consume_attribute_value(&self, quote: &str) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_attribute(quote)).map_err(|_| Error::ExpectedAttributeValue)
    }

    fn consume_name(&self) -> peresil::Progress<StringPoint<'a>, &'a str, ()> {
        self.consume_to(self.s.end_of_name())
    }

    fn consume_hex_chars(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_hex_chars()).map_err(|_| Error::ExpectedHexReferenceValue)
    }

    fn consume_char_data(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_char_data()).map_err(|_| Error::ExpectedCharacterData)
    }

    fn consume_cdata(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_cdata()).map_err(|_| Error::ExpectedCData)
    }

    fn consume_comment(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_comment()).map_err(|_| Error::ExpectedCommentBody)
    }

    fn consume_pi_value(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_pi_value()).map_err(|_| Error::ExpectedProcessingInstructionValue)
    }

    fn consume_start_tag(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_start_tag()).map_err(|_| Error::ExpectedElement)
    }
}

trait X<'a> {
    fn expect_space(&self) -> XmlProgress<'a, &'a str>;
    fn expect_literal(&self, s: &'static str) -> XmlProgress<'a, &'a str>;
}

impl<'a> X<'a> for StringPoint<'a> {
    fn expect_space(&self) -> XmlProgress<'a, &'a str> {
        self.consume_space().map_err(|_| Error::ExpectedWhitespace)
    }

    fn expect_literal(&self, s: &'static str) -> XmlProgress<'a, &'a str> {
        self.consume_literal(s).map_err(|_| Error::Expected(s))
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser
    }

    fn parse_eq<'a>(&self, xml: StringPoint<'a>) -> XmlProgress<'a, ()> {
        let (xml, _) = xml.consume_space().optional(xml);
        let (xml, _) = try_parse!(xml.expect_literal("="));
        let (xml, _) = xml.consume_space().optional(xml);

        success((), xml)
    }

    fn parse_version_info<'a>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, &'a str> {
        fn version_num<'a>(xml: StringPoint<'a>) -> peresil::Progress<StringPoint<'a>, &'a str, ()> {
            let start_point = xml;

            let (xml, _) = try_parse!(xml.consume_literal("1."));
            let (xml, _) = try_parse!(xml.consume_decimal_chars());

            peresil::Progress::success(xml, start_point.to(xml))
        }

        let (xml, _) = try_parse!(xml.expect_space().map_err(|_| Error::ExpectedWhitespace));
        let (xml, _) = try_parse!(xml.expect_literal("version"));
        let (xml, _) = try_parse!(self.parse_eq(xml));
        let (xml, version) = try_parse!(
            self.parse_quoted_value(pm, xml, |_, xml, _| version_num(xml).map_err(|_| Error::ExpectedVersionNumber))
        );

        success(version, xml)
    }

    fn parse_xml_declaration<'a>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, ()> {
        let (xml, _) = try_parse!(xml.expect_literal("<?xml"));
        let (xml, _version) = try_parse!(self.parse_version_info(pm, xml));
        // let (xml, encoding) = self.parse_encoding_declaration(xml).optional(xml));
        // let (xml, standalone) = self.parse_standalone_declaration(xml).optional(xml);
        let (xml, _) = xml.consume_space().optional(xml);
        let (xml, _) = try_parse!(xml.expect_literal("?>"));

        success((), xml)
    }

    fn parse_misc<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S) -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        pm.alternate()
            .one(|_| self.parse_comment(xml, sink))
            .one(|_| self.parse_pi(xml, sink))
            .one(|_| xml.expect_space().map(|_| ()))
            .finish()
    }

    fn parse_miscs<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S) -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        pm.zero_or_more(xml, |pm, xml| self.parse_misc(pm, xml, sink)).map(|_| ())
    }

    fn parse_prolog<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S) -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = self.parse_xml_declaration(pm, xml).optional(xml);
        self.parse_miscs(pm, xml, sink)
    }

    fn parse_one_quoted_value<'a, T, F>(&self, xml: StringPoint<'a>, quote: &'static str, f: F)
                                        -> XmlProgress<'a, T>
        where F: FnMut(StringPoint<'a>) -> XmlProgress<'a, T>
    {
        let mut f = f;
        let (xml, _) = try_parse!(xml.consume_literal(quote).map_err(|_| Error::ExpectedOpeningQuote(quote)));
        let (xml, value) = try_parse!(f(xml));
        let (xml, _) = try_parse!(xml.consume_literal(quote).map_err(|_| Error::ExpectedClosingQuote(quote)));

        success(value, xml)
    }

    fn parse_quoted_value<'a, T, F>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, f: F) -> XmlProgress<'a, T>
        where F: FnMut(&mut XmlMaster<'a>, StringPoint<'a>, &str) -> XmlProgress<'a, T>
    {
        let mut f = f;
        pm.alternate()
            .one(|pm| self.parse_one_quoted_value(xml, "'",  |xml| f(pm, xml, "'")))
            .one(|pm| self.parse_one_quoted_value(xml, "\"", |xml| f(pm, xml, "\"")))
            .finish()
    }

    fn parse_attribute_literal<'a, 's, S>(&self, xml: StringPoint<'a>, sink: &'s mut S, quote: &str)
                                          -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, val) = try_parse!(xml.consume_attribute_value(quote));
        sink.attribute_value(LiteralAttributeValue(val));

        success((), xml)
    }

    fn parse_attribute_reference<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S)
                                          -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, val) = try_parse!(self.parse_reference(pm, xml));
        sink.attribute_value(ReferenceAttributeValue(val));

        success((), xml)
    }

    fn parse_attribute_values<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S, quote: &str)
                                         -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {

        pm.zero_or_more(xml, |pm, xml| {
            pm.alternate()
                .one(|_|  self.parse_attribute_literal(xml, sink, quote))
                .one(|pm| self.parse_attribute_reference(pm, xml, sink))
                .finish()
        }).map(|_| ())
    }

    fn parse_attribute<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S)
                                  -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = try_parse!(xml.expect_space());

        let (xml, name) = try_parse!(xml.consume_prefixed_name().map_err(|_| Error::ExpectedAttribute));

        sink.attribute_start(name);

        let (xml, _) = try_parse!(self.parse_eq(xml));

        let (xml, _) = try_parse!(
            self.parse_quoted_value(pm, xml, |pm, xml, quote| self.parse_attribute_values(pm, xml, sink, quote))
        );

        sink.attribute_end(name);

        success((), xml)
    }

    fn parse_attributes<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S)
                                   -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        pm.zero_or_more(xml, |pm, xml| self.parse_attribute(pm, xml, sink)).map(|_| ())
    }

    fn parse_element_end<'a>(&self, xml: StringPoint<'a>, start_name: PrefixedName<'a>)
                             -> XmlProgress<'a, ()>
    {
        let (xml, _) = try_parse!(xml.expect_literal("</"));

        let name_xml = xml;
        let (xml, name) = try_parse!(xml.consume_prefixed_name().map_err(|_| Error::ExpectedElementName));

        let (xml, _) = xml.consume_space().optional(xml);
        let (xml, _) = try_parse!(xml.expect_literal(">"));

        if start_name != name {
            return peresil::Progress::failure(name_xml, Error::MismatchedElementEndName);
        }

        success((), xml)
    }

    fn parse_char_data<'a, 's, S>(&self, xml: StringPoint<'a>, sink: &'s mut S)
                                  -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, text) = try_parse!(xml.consume_char_data());

        sink.text(text);

        success((), xml)
    }

    fn parse_cdata<'a, 's, S>(&self, xml: StringPoint<'a>, sink: &'s mut S)
                              -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = try_parse!(xml.expect_literal("<![CDATA["));
        let (xml, text) = try_parse!(xml.consume_cdata());
        let (xml, _) = try_parse!(xml.expect_literal("]]>"));

        sink.text(text);

        success((), xml)
    }

    fn parse_entity_ref<'a>(&self, xml: StringPoint<'a>) -> XmlProgress<'a, Reference<'a>> {
        let (xml, _) = try_parse!(xml.consume_literal("&").map_err(|_| Error::ExpectedNamedReference));
        let (xml, name) = try_parse!(Span::parse(xml, |xml| xml.consume_name().map_err(|_| Error::ExpectedNamedReferenceValue)));
        let (xml, _) = try_parse!(xml.expect_literal(";"));

        success(EntityReference(name), xml)
    }

    fn parse_decimal_char_ref<'a>(&self, xml: StringPoint<'a>) -> XmlProgress<'a, Reference<'a>> {
        let (xml, _) = try_parse!(xml.consume_literal("&#").map_err(|_| Error::ExpectedDecimalReference));
        let (xml, dec) = try_parse!(Span::parse(xml, |xml| xml.consume_decimal_chars().map_err(|_| Error::ExpectedDecimalReferenceValue)));
        let (xml, _) = try_parse!(xml.expect_literal(";"));

        success(DecimalCharReference(dec), xml)
    }

    fn parse_hex_char_ref<'a>(&self, xml: StringPoint<'a>) -> XmlProgress<'a, Reference<'a>> {
        let (xml, _) = try_parse!(xml.consume_literal("&#x").map_err(|_| Error::ExpectedHexReference));
        let (xml, hex) = try_parse!(Span::parse(xml, |xml| xml.consume_hex_chars()));
        let (xml, _) = try_parse!(xml.expect_literal(";"));

        success(HexCharReference(hex), xml)
    }

    fn parse_reference<'a>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, Reference<'a>> {
        pm.alternate()
            .one(|_| self.parse_entity_ref(xml))
            .one(|_| self.parse_decimal_char_ref(xml))
            .one(|_| self.parse_hex_char_ref(xml))
            .finish()
    }

    fn parse_comment<'a, 's, S>(&self, xml: StringPoint<'a>, sink: &'s mut S) -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = try_parse!(xml.consume_literal("<!--").map_err(|_| Error::ExpectedComment));
        let (xml, text) = try_parse!(xml.consume_comment());
        let (xml, _) = try_parse!(xml.expect_literal("-->"));

        sink.comment(text);

        success((), xml)
    }

    fn parse_pi_value<'a>(&self, xml: StringPoint<'a>) -> XmlProgress<'a, &'a str> {
        let (xml, _) = try_parse!(xml.expect_space());
        xml.consume_pi_value()
    }

    fn parse_pi<'a, 's, S>(&self, xml: StringPoint<'a>, sink: &'s mut S) -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = try_parse!(xml.consume_literal("<?").map_err(|_| Error::ExpectedProcessingInstruction));
        let target_xml = xml;
        let (xml, target) = try_parse!(xml.consume_name().map_err(|_| Error::ExpectedProcessingInstructionTarget));
        let (xml, value) = self.parse_pi_value(xml).optional(xml);
        let (xml, _) = try_parse!(xml.expect_literal("?>"));

        if target.eq_ignore_ascii_case("xml") {
            return peresil::Progress::failure(target_xml, Error::InvalidProcessingInstructionTarget);
        }

        sink.processing_instruction(target, value);

        success((), xml)
    }

    fn parse_content_reference<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S)
                                        -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, r) = try_parse!(self.parse_reference(pm, xml));
        adapt_sink_result!(sink.reference(r));

        success((), xml)
    }

    fn parse_rest_of_content<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S)
                                        -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = try_parse!({
            pm.alternate()
                .one(|pm| self.parse_element(pm, xml, sink))
                .one(|_|  self.parse_cdata(xml, sink))
                .one(|pm| self.parse_content_reference(pm, xml, sink))
                .one(|_|  self.parse_comment(xml, sink))
                .one(|_|  self.parse_pi(xml, sink))
                .finish()
        });

        let (xml, _) = self.parse_char_data(xml, sink).optional(xml);

        success((), xml)
    }

    fn parse_content<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S) -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = self.parse_char_data(xml, sink).optional(xml);
        pm.zero_or_more(xml, |pm, xml| self.parse_rest_of_content(pm, xml, sink)).map(|_| ())
    }

    fn parse_empty_element_tail<'a>(&self, xml: StringPoint<'a>) -> XmlProgress<'a, ()> {
        let (xml, _) = try_parse!(xml.consume_literal("/>").map_err(|_| Error::ExpectedElementSelfClosed));

        success((), xml)
    }

    fn parse_non_empty_element_tail<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S, start_name: PrefixedName<'a>)
                                               -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = try_parse!(xml.consume_literal(">").map_err(|_| Error::ExpectedElementEnd));

        let (xml, _) = try_parse!(self.parse_content(pm, xml, sink));

        let (xml, _) = try_parse!(self.parse_element_end(xml, start_name));

        success((), xml)
    }

    fn parse_element<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S) -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = try_parse!(xml.consume_start_tag());
        let (xml, name) = try_parse!(xml.consume_prefixed_name().map_err(|_| Error::ExpectedElementName));

        sink.element_start(name);

        sink.attributes_start();
        let (xml, _) = try_parse!(self.parse_attributes(pm, xml, sink));
        adapt_sink_result!(sink.attributes_end());

        let (xml, _) = xml.consume_space().optional(xml);

        let (xml, _) = try_parse!(
            pm.alternate()
                .one(|_|  self.parse_empty_element_tail(xml))
                .one(|pm| self.parse_non_empty_element_tail(pm, xml, sink, name))
                .finish()
        );

        sink.element_end(name);

        success((), xml)
    }

    fn parse_document<'a, 's, S>(&self, pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, sink: &'s mut S)
                                 -> XmlProgress<'a, ()>
        where S: ParserSink<'a>
    {
        let (xml, _) = try_parse!(self.parse_prolog(pm, xml, sink));
        let (xml, _) = try_parse!(self.parse_element(pm, xml, sink));
        let (xml, _) = self.parse_miscs(pm, xml, sink).optional(xml);

        success((), xml)
    }

    /// Parses a string into a DOM. On failure, the location of the
    /// parsing failure will be returned.
    pub fn parse(&self, xml: &str) -> Result<super::Package, (usize, Vec<Error>)> {
        // TODO: Evaluate how useful the error result is.

        let xml = StringPoint::new(xml);
        let package = super::Package::new();

        {
            let doc = package.as_document();
            let mut hydrator = SaxHydrator::new(&doc);

            let mut pm = ParseMaster::new();
            let r = self.parse_document(&mut pm, xml, &mut hydrator);

            match pm.finish(r) {
                peresil::Progress { status: peresil::Status::Success(..), .. } => (),
                peresil::Progress { status: peresil::Status::Failure(e), point } => {
                    return Err((point.offset, e));
                }
            };
        }

        // TODO: Check fully parsed

        Ok(package)
    }
}

type SinkResult<'x, T> = Result<T, (Span<'x>, Error)>;

trait ParserSink<'x> {
    fn element_start(&mut self, name: PrefixedName<'x>);
    fn element_end(&mut self, name: PrefixedName<'x>);
    fn comment(&mut self, text: &'x str);
    fn processing_instruction(&mut self, target: &'x str, value: Option<&'x str>);
    fn text(&mut self, text: &'x str);
    fn reference(&mut self, reference: Reference<'x>) -> SinkResult<'x, ()>;
    fn attributes_start(&mut self);
    fn attributes_end(&mut self) -> SinkResult<'x, ()>;
    fn attribute_start(&mut self, name: PrefixedName<'x>);
    fn attribute_value(&mut self, value: AttributeValue<'x>);
    fn attribute_end(&mut self, name: PrefixedName<'x>);
}


fn decode_reference<'x, T, F>(ref_data: Reference<'x>, cb: F) -> SinkResult<'x, T>
    where F: FnMut(&str) -> SinkResult<'x, T>
{
    let mut cb = cb;
    match ref_data {
        DecimalCharReference(span) => {
            u32::from_str_radix(span.s, 10).ok()
                .and_then(|code| char::from_u32(code))
                .ok_or((span, Error::InvalidDecimalReference))
                .and_then(|c| {
                    let s: String = iter::repeat(c).take(1).collect();
                    cb(&s)
                })
        },
        HexCharReference(span) => {
            u32::from_str_radix(span.s, 16).ok()
                .and_then(|code| char::from_u32(code))
                .ok_or((span, Error::InvalidHexReference))
                .and_then(|c| {
                    let s: String = iter::repeat(c).take(1).collect();
                    cb(&s)
                })
        },
        EntityReference(span) => {
            let s = match span.s {
                "amp"  => "&",
                "lt"   => "<",
                "gt"   => ">",
                "apos" => "'",
                "quot" => "\"",
                _      => return Err((span, Error::UnknownNamedReference)),
            };
            cb(s)
        }
    }
}

struct AttributeValueBuilder {
    value: String,
}

impl AttributeValueBuilder {
    fn convert<'x>(values: &Vec<AttributeValue<'x>>) -> SinkResult<'x, String> {
        let mut builder = AttributeValueBuilder::new();
        try!(builder.ingest(values));
        Ok(builder.implode())
    }

    fn new() -> AttributeValueBuilder {
        AttributeValueBuilder {
            value: String::new(),
        }
    }

    fn ingest<'x>(&mut self, values: &Vec<AttributeValue<'x>>) -> SinkResult<'x, ()> {
        for value in values.iter() {
            match value {
                &LiteralAttributeValue(v) => self.value.push_str(v),
                &ReferenceAttributeValue(r) => try!(decode_reference(r, |s| Ok(self.value.push_str(s)))),
            }
        }

        Ok(())
    }

    fn clear(&mut self) {
        self.value.clear();
    }

    fn implode(self) -> String {
        self.value
    }
}

impl Deref for AttributeValueBuilder {
    type Target = str;

    fn deref(&self) -> &str {
        &self.value
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
        where T: dom4::IntoChildOfRoot<'d>
    {
        match self.stack.last() {
            None => self.doc.root().append_child(child),
            Some(parent) => parent.append_child(child.into_child_of_root()),
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

    fn reference(&mut self, reference: Reference<'x>) -> SinkResult<'x, ()> {
        let text = try!(decode_reference(reference, |s| Ok(self.doc.create_text(s))));
        self.append_text(text);
        Ok(())
    }

    fn attributes_start(&mut self) {
    }

    fn attributes_end(&mut self) -> SinkResult<'x, ()> {
        let deferred_element = self.element.take().unwrap();
        let attributes = replace(&mut self.attributes, Vec::new());

        let (namespaces, attributes): (Vec<_>, Vec<_>) =
            attributes.into_iter().partition(|attr| attr.name.prefix == Some("xmlns"));

        let (default_namespaces, attributes): (Vec<_>, Vec<_>) =
            attributes.into_iter().partition(|attr| attr.name.local_part == "xmlns");

        let default_namespace = match &default_namespaces[..] {
            [] => None,
            [ref ns] => {
                let value = try!(AttributeValueBuilder::convert(&ns.values));
                Some(value)
            },
            _ => panic!("Cannot declare multiple default namespaces"),
        };

        let mut new_prefix_mappings = HashMap::new();
        for ns in namespaces.iter() {
            let value = try!(AttributeValueBuilder::convert(&ns.values));
            new_prefix_mappings.insert(ns.name.local_part, value);
        }
        let new_prefix_mappings = new_prefix_mappings;

        let element = if let Some(prefix) = deferred_element.prefix {
            let ns_uri = new_prefix_mappings.get(prefix).map(|p| &p[..]);
            let ns_uri = ns_uri.or_else(|| self.namespace_uri_for_prefix(prefix));

            if let Some(ns_uri) = ns_uri {
                let element = self.doc.create_element((ns_uri, deferred_element.local_part));
                element.set_preferred_prefix(Some(prefix));
                element
            } else {
                panic!("Unknown namespace prefix '{}'", prefix);
            }
        } else if let Some(ns_uri) = default_namespace {
            let ns_uri = &ns_uri[..];
            let element = self.doc.create_element((ns_uri, deferred_element.local_part));
            element.set_default_namespace_uri(Some(ns_uri));
            element
        } else {
            self.doc.create_element(deferred_element.local_part)
        };

        for (prefix, ns_uri) in new_prefix_mappings.iter() {
            element.register_prefix(*prefix, ns_uri);
        }

        self.append_to_either(element);
        self.stack.push(element);

        let mut builder = AttributeValueBuilder::new();

        for attribute in attributes.iter() {
            builder.clear();
            try!(builder.ingest(&attribute.values));
            let value = &builder;

            if let Some(prefix) = attribute.name.prefix {
                let ns_uri = new_prefix_mappings.get(prefix).map(|p| &p[..]);
                let ns_uri = ns_uri.or_else(|| self.namespace_uri_for_prefix(prefix));

                if let Some(ns_uri) = ns_uri {
                    let attr = element.set_attribute_value((ns_uri, attribute.name.local_part),
                                                           &value);
                    attr.set_preferred_prefix(Some(prefix));
                } else {
                    panic!("Unknown namespace prefix '{}'", prefix);
                }
            } else {
                element.set_attribute_value(attribute.name.local_part, &value);
            }
        }

        Ok(())
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
    use super::{Parser,Error};
    use super::super::{Package,IntoQName};
    use super::super::dom4;

    macro_rules! assert_qname_eq(
        ($l:expr, $r:expr) => (assert_eq!($l.into_qname(), $r.into_qname()));
    );

    fn full_parse(xml: &str) -> Result<Package, (usize, Vec<Error>)> {
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
        assert_qname_eq!(("namespace", "hello"), top.name());
    }

    #[test]
    fn an_element_with_a_default_namespace() {
        let package = quick_parse("<hello xmlns='namespace'/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_eq!(top.default_namespace_uri(), Some("namespace"));
        assert_qname_eq!(("namespace", "hello"), top.name());
    }

    #[test]
    fn an_element_with_an_attribute() {
        let package = quick_parse("<hello scope='world'/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_eq!(top.attribute_value("scope"), Some("world"));
    }

    #[test]
    fn an_element_with_an_attribute_using_double_quotes() {
        let package = quick_parse("<hello scope=\"world\"/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_eq!(top.attribute_value("scope"), Some("world"));
    }

    #[test]
    fn an_element_with_multiple_attributes() {
        let package = quick_parse("<hello scope='world' happy='true'/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_eq!(top.attribute_value("scope"), Some("world"));
        assert_eq!(top.attribute_value("happy"), Some("true"));
    }

    #[test]
    fn an_attribute_with_a_namespace() {
        let package = quick_parse("<hello ns:a='b' xmlns:ns='namespace'/>");
        let doc = package.as_document();
        let top = top(&doc);

        let attr = top.attribute(("namespace", "a")).unwrap();

        assert_eq!(attr.preferred_prefix(), Some("ns"));
        assert_eq!(attr.value(), "b");
    }

    #[test]
    fn an_attribute_with_references() {
        let package = quick_parse("<log msg='I &lt;3 math' />");
        let doc = package.as_document();
        let top = top(&doc);

        assert_eq!(top.attribute_value("msg"), Some("I <3 math"));
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

        assert_qname_eq!(world.name(), ("inner", "world"));
    }

    #[test]
    fn nested_elements_with_inherited_namespaces() {
        let package = quick_parse("<ns1:hello xmlns:ns1='outer'><ns1:world/></ns1:hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        assert_eq!(world.preferred_prefix(), Some("ns1"));
        assert_qname_eq!(world.name(), ("outer", "world"));
    }

    #[test]
    fn nested_elements_with_attributes() {
        let package = quick_parse("<hello><world name='Earth'/></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        assert_eq!(world.attribute_value("name"), Some("Earth"));
    }

    #[test]
    fn nested_elements_with_attributes_with_inherited_namespaces() {
        let package = quick_parse("<hello xmlns:ns1='outer'><world ns1:name='Earth'/></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        let attr = world.attribute(("outer", "name")).unwrap();

        assert_eq!(attr.preferred_prefix(), Some("ns1"));
        assert_eq!(attr.value(), "Earth");
    }

    #[test]
    fn element_with_text() {
        let package = quick_parse("<hello>world</hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let text = hello.children()[0].text().unwrap();

        assert_eq!(text.text(), "world");
    }

    #[test]
    fn element_with_cdata() {
        let package = quick_parse("<words><![CDATA[I have & and < !]]></words>");
        let doc = package.as_document();
        let words = top(&doc);
        let text = words.children()[0].text().unwrap();

        assert_eq!(text.text(), "I have & and < !");
    }

    #[test]
    fn element_with_comment() {
        let package = quick_parse("<hello><!-- A comment --></hello>");
        let doc = package.as_document();
        let words = top(&doc);
        let comment = words.children()[0].comment().unwrap();

        assert_eq!(comment.text(), " A comment ");
    }

    #[test]
    fn comment_before_top_element() {
        let package = quick_parse("<!-- A comment --><hello />");
        let doc = package.as_document();
        let comment = doc.root().children()[0].comment().unwrap();

        assert_eq!(comment.text(), " A comment ");
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

        assert_eq!(comment1.text(), "Comment 1");
        assert_eq!(comment2.text(), "Comment 2");
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

        assert_eq!(comment1.text(), "Comment 1");
        assert_eq!(comment2.text(), "Comment 2");
    }

    #[test]
    fn element_with_processing_instruction() {
        let package = quick_parse("<hello><?device?></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let pi = hello.children()[0].processing_instruction().unwrap();

        assert_eq!(pi.target(), "device");
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

        assert_eq!(pi1.target(), "output");
        assert_eq!(pi1.value(), Some("printer"));

        assert_eq!(pi2.target(), "validated");
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

        assert_eq!(text1.text(), "2 ");
        assert_eq!(text2.text(), ">");
        assert_eq!(text3.text(), " 1");
    }

    #[test]
    fn element_with_hexidecimal_char_reference() {
        let package = quick_parse("<math>1 &#x3c; 2</math>");
        let doc = package.as_document();
        let math = top(&doc);
        let text1 = math.children()[0].text().unwrap();
        let text2 = math.children()[1].text().unwrap();
        let text3 = math.children()[2].text().unwrap();

        assert_eq!(text1.text(), "1 ");
        assert_eq!(text2.text(), "<");
        assert_eq!(text3.text(), " 2");
    }

    #[test]
    fn element_with_entity_reference() {
        let package = quick_parse("<math>I &lt;3 math</math>");
        let doc = package.as_document();
        let math = top(&doc);
        let text1 = math.children()[0].text().unwrap();
        let text2 = math.children()[1].text().unwrap();
        let text3 = math.children()[2].text().unwrap();

        assert_eq!(text1.text(), "I ");
        assert_eq!(text2.text(), "<");
        assert_eq!(text3.text(), "3 math");
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

        assert_eq!(text.text(),    "to ");
        assert_eq!(comment.text(), "fixme");
        assert_qname_eq!(element.name(), "a");
        assert_eq!(pi.target(),    "world");
    }

    // TODO: untested errors
    //
    // versionnumber
    // prefixedname
    // decimal chars
    // hex chars
    // chardata
    // cdata
    // commentbody
    // pinstructionvalue

    #[test]
    fn failure_no_open_brace() {
        use super::Error::*;

        let r = full_parse("hi />");

        assert_eq!(r, Err((0, vec![ExpectedComment, ExpectedProcessingInstruction, ExpectedWhitespace, ExpectedElement])));
    }

    #[test]
    fn failure_unclosed_tag() {
        use super::Error::*;

        let r = full_parse("<hi");

        assert_eq!(r, Err((3, vec![ExpectedWhitespace, ExpectedElementSelfClosed, ExpectedElementEnd])));
    }

    #[test]
    fn failure_unexpected_space() {
        use super::Error::*;

        let r = full_parse("<hi / >");

        assert_eq!(r, Err((4, vec![ExpectedAttribute, ExpectedElementSelfClosed, ExpectedElementEnd])));
    }

    #[test]
    fn failure_attribute_without_open_quote() {
        use super::Error::*;

        let r = full_parse("<hi oops=value' />");

        assert_eq!(r, Err((9, vec![ExpectedOpeningQuote("\'"), ExpectedOpeningQuote("\"")])));
    }

    #[test]
    fn failure_attribute_without_close_quote() {
        use super::Error::*;

        let r = full_parse("<hi oops='value />");

        assert_eq!(r, Err((18, vec![ExpectedNamedReference, ExpectedDecimalReference, ExpectedAttributeValue, ExpectedHexReference, ExpectedClosingQuote("\'")])));
    }

    #[test]
    fn failure_unclosed_attribute_and_tag() {
        use super::Error::*;

        let r = full_parse("<hi oops='value");

        assert_eq!(r, Err((15, vec![ExpectedNamedReference, ExpectedDecimalReference, ExpectedAttributeValue, ExpectedHexReference, ExpectedClosingQuote("\'")])));
    }

    #[test]
    fn failure_nested_unclosed_tag() {
        use super::Error::*;

        let r = full_parse("<hi><oops</hi>");

        assert_eq!(r, Err((9, vec![ExpectedWhitespace, ExpectedElementSelfClosed, ExpectedElementEnd])));
    }

    #[test]
    fn failure_nested_unexpected_space() {
        use super::Error::*;

        let r = full_parse("<hi><oops / ></hi>");

        assert_eq!(r, Err((10, vec![ExpectedAttribute, ExpectedElementSelfClosed, ExpectedElementEnd])));
    }

    #[test]
    fn failure_malformed_entity_reference() {
        use super::Error::*;

        let r = full_parse("<hi>Entity: &;</hi>");

        assert_eq!(r, Err((13, vec![ExpectedNamedReferenceValue])));
    }

    #[test]
    fn failure_nested_malformed_entity_reference() {
        use super::Error::*;

        let r = full_parse("<hi><bye>Entity: &;</bye></hi>");

        assert_eq!(r, Err((18, vec![ExpectedNamedReferenceValue])));
    }

    #[test]
    fn failure_nested_attribute_without_open_quote() {
        use super::Error::*;

        let r = full_parse("<hi><bye oops=value' /></hi>");

        assert_eq!(r, Err((14, vec![ExpectedOpeningQuote("\'"), ExpectedOpeningQuote("\"")])));
    }

    #[test]
    fn failure_nested_attribute_without_close_quote() {
        use super::Error::*;

        let r = full_parse("<hi><bye oops='value /></hi>");

        assert_eq!(r, Err((23, vec![ExpectedNamedReference, ExpectedDecimalReference, ExpectedAttributeValue, ExpectedHexReference, ExpectedClosingQuote("\'")])));
    }

    #[test]
    fn failure_nested_unclosed_attribute_and_tag() {
        use super::Error::*;

        let r = full_parse("<hi><bye oops='value</hi>");

        assert_eq!(r, Err((20, vec![ExpectedNamedReference, ExpectedDecimalReference, ExpectedAttributeValue, ExpectedHexReference, ExpectedClosingQuote("\'")])));
    }

    #[test]
    fn failure_pi_target_as_xml() {
        use super::Error::*;

        let r = full_parse("<a><?xml?></a>");

        assert_eq!(r, Err((5, vec![InvalidProcessingInstructionTarget])));
    }

    #[test]
    fn failure_end_tag_does_not_match() {
        use super::Error::*;

        let r = full_parse("<a></b>");

        assert_eq!(r, Err((5, vec![MismatchedElementEndName])));
    }

    #[test]
    fn failure_invalid_decimal_reference() {
        use super::Error::*;

        let r = full_parse("<a>&#99999999;</a>");

        assert_eq!(r, Err((5, vec![InvalidDecimalReference])));
    }

    #[test]
    fn failure_invalid_hex_reference() {
        use super::Error::*;

        let r = full_parse("<a>&#x99999999;</a>");

        assert_eq!(r, Err((6, vec![InvalidHexReference])));
    }

    #[test]
    fn failure_unknown_named_reference() {
        use super::Error::*;

        let r = full_parse("<a>&fake;</a>");

        assert_eq!(r, Err((4, vec![UnknownNamedReference])));
    }
}
