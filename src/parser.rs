//! Converts XML strings into a DOM structure
//!
//! ### Example
//!
//! ```
//! use sxd_document::parser;
//! let xml = r#"<?xml version="1.0"?>
//! <!-- Awesome data incoming -->
//! <data awesome="true">
//!   <datum>Science</datum>
//!   <datum><![CDATA[Literature]]></datum>
//!   <datum>Math &gt; others</datum>
//! </data>"#;
//! let doc = parser::parse(xml).expect("Failed to parse");
//! ```

#[allow(unused, deprecated)] // rust-lang/rust#46510
use std::ascii::AsciiExt;
use std::collections::{BTreeSet, HashMap};
use std::mem::replace;
use std::ops::Deref;
use std::{char, error, fmt, iter};

use peresil::{self,StringPoint,ParseMaster,Recoverable};

use self::Reference::*;

use super::{PrefixedName,QName};
use super::dom;
use super::str::XmlStr;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum SpecificError {
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
    ExpectedEncoding,
    ExpectedYesNo,
    ExpectedWhitespace,

    ExpectedDocumentTypeName,
    ExpectedIntSubset,
    ExpectedSystemLiteral,

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

    DuplicateAttribute,
    RedefinedNamespace,
    RedefinedDefaultNamespace,
    EmptyNamespace,
    UnknownNamespacePrefix,
    UnclosedElement,
}

impl Recoverable for SpecificError {
    fn recoverable(&self) -> bool {
        use self::SpecificError::*;

        match *self {
            ExpectedEncoding                   |
            ExpectedYesNo                      |
            InvalidProcessingInstructionTarget |
            MismatchedElementEndName           |
            InvalidDecimalReference            |
            InvalidHexReference                |
            UnknownNamedReference              |
            DuplicateAttribute                 |
            RedefinedNamespace                 |
            RedefinedDefaultNamespace          |
            EmptyNamespace                     |
            UnknownNamespacePrefix             |
            UnclosedElement                    => {
                false
            },
            _ => true
        }
    }
}

impl fmt::Display for SpecificError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::error::Error;
        use self::SpecificError::*;

        match *self {
            Expected(s)             |
            ExpectedClosingQuote(s) |
            ExpectedOpeningQuote(s) => {
                write!(f, "Parser error: {} {}", self.description(), s)
            },
            _ => write!(f, "Parser error: {}", self.description())
        }
    }
}

impl error::Error for SpecificError {
    fn description(&self) -> &str {
        use self::SpecificError::*;

        match *self {
            Expected(_) => "expected",
            ExpectedAttribute => "expected attribute",
            ExpectedAttributeValue => "expected attribute value",
            ExpectedCData => "expected CDATA",
            ExpectedCharacterData => "expected character data",
            ExpectedComment => "expected comment",
            ExpectedCommentBody => "expected comment body",
            ExpectedElement => "expected element",
            ExpectedElementName => "expected element name",
            ExpectedElementEnd => "expected element end",
            ExpectedElementSelfClosed => "expected element self closed",
            ExpectedProcessingInstruction => "expected processing instruction",
            ExpectedProcessingInstructionTarget => "expected processing instruction target",
            ExpectedProcessingInstructionValue => "expected processing instruction value",
            ExpectedVersionNumber => "expected version number",
            ExpectedEncoding => "expected encoding",
            ExpectedYesNo => "expected yes or no",
            ExpectedWhitespace => "expected whitespace",
            ExpectedDocumentTypeName => "expected document type name",
            ExpectedIntSubset => "expected int subset",
            ExpectedSystemLiteral => "expected system literal",
            ExpectedClosingQuote(_) => "expected closing quote",
            ExpectedOpeningQuote(_) => "expected opening quote",
            ExpectedDecimalReferenceValue => "expected decimal reference value",
            ExpectedHexReferenceValue => "expected hex reference value",
            ExpectedNamedReferenceValue => "expected named reference value",
            ExpectedDecimalReference => "expected decimal reference",
            ExpectedHexReference => "expected hex reference",
            ExpectedNamedReference => "expected named reference",
            InvalidProcessingInstructionTarget => "invalid processing instruction target",
            MismatchedElementEndName => "mismatched element end name",
            InvalidDecimalReference => "invalid decimal reference",
            InvalidHexReference => "invalid hex reference",
            UnknownNamedReference => "unknown named reference",
            DuplicateAttribute => "duplicate attribute",
            RedefinedNamespace => "redefined namespace",
            RedefinedDefaultNamespace => "redefined default namespace",
            EmptyNamespace => "empty namespace",
            UnknownNamespacePrefix => "unknown namespace prefix",
            UnclosedElement => "unclosed element",
        }
    }
}

type XmlMaster<'a> = peresil::ParseMaster<StringPoint<'a>, SpecificError>;
type XmlProgress<'a, T> = peresil::Progress<StringPoint<'a>, T, SpecificError>;

fn success<T>(data: T, point: StringPoint) -> XmlProgress<T> {
    peresil::Progress { point: point, status: peresil::Status::Success(data) }
}

/// A truncated point; the string ends before the end of input
#[derive(Debug,Copy,Clone)]
struct Span<T> {
    offset: usize,
    value: T,
}

impl<T> Span<T> {
    // Should this just be the return type of the parser methods?
    fn parse<'a, F>(pt: StringPoint<'a>, f: F) -> XmlProgress<'a, Span<T>>
        where F: Fn(StringPoint<'a>) -> XmlProgress<'a, T>
    {
        let (after, v) = try_parse!(f(pt));
        let span = Span { value: v, offset: pt.offset };
        peresil::Progress { status: peresil::Status::Success(span), point: after }
    }

    fn map<F, U>(self, f: F) -> Span<U>
        where F: FnOnce(T) -> U
    {
        Span { value: f(self.value), offset: self.offset }
    }
}

#[derive(Debug,Copy,Clone)]
enum Reference<'a> {
    EntityReference(Span<&'a str>),
    DecimalCharReference(Span<&'a str>),
    HexCharReference(Span<&'a str>),
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
        fn parse_local(xml: StringPoint) -> peresil::Progress<StringPoint, &str, ()> {
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
    fn consume_int_subset(&self) -> XmlProgress<'a, &'a str>;
    fn consume_comment(&self) -> XmlProgress<'a, &'a str>;
    fn consume_pi_value(&self) -> XmlProgress<'a, &'a str>;
    fn consume_start_tag(&self) -> XmlProgress<'a, &'a str>;
    fn consume_encoding(&self) -> XmlProgress<'a, &'a str>;
}

impl<'a> PrivateXmlParseExt<'a> for StringPoint<'a> {
    fn consume_attribute_value(&self, quote: &str) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_attribute(quote)).map_err(|_| SpecificError::ExpectedAttributeValue)
    }

    fn consume_name(&self) -> peresil::Progress<StringPoint<'a>, &'a str, ()> {
        self.consume_to(self.s.end_of_name())
    }

    fn consume_hex_chars(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_hex_chars()).map_err(|_| SpecificError::ExpectedHexReferenceValue)
    }

    fn consume_char_data(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_char_data()).map_err(|_| SpecificError::ExpectedCharacterData)
    }

    fn consume_cdata(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_cdata()).map_err(|_| SpecificError::ExpectedCData)
    }

    fn consume_int_subset(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_int_subset()).map_err(|_| SpecificError::ExpectedIntSubset)
    }

    fn consume_comment(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_comment()).map_err(|_| SpecificError::ExpectedCommentBody)
    }

    fn consume_pi_value(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_pi_value()).map_err(|_| SpecificError::ExpectedProcessingInstructionValue)
    }

    fn consume_start_tag(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_start_tag()).map_err(|_| SpecificError::ExpectedElement)
    }

    fn consume_encoding(&self) -> XmlProgress<'a, &'a str> {
        self.consume_to(self.s.end_of_encoding()).map_err(|_| SpecificError::ExpectedEncoding)
    }
}

trait X<'a> {
    fn expect_space(&self) -> XmlProgress<'a, &'a str>;
    fn expect_literal(&self, s: &'static str) -> XmlProgress<'a, &'a str>;
}

impl<'a> X<'a> for StringPoint<'a> {
    fn expect_space(&self) -> XmlProgress<'a, &'a str> {
        self.consume_space().map_err(|_| SpecificError::ExpectedWhitespace)
    }

    fn expect_literal(&self, s: &'static str) -> XmlProgress<'a, &'a str> {
        self.consume_literal(s).map_err(|_| SpecificError::Expected(s))
    }
}

#[derive(Debug, Copy, Clone)]
enum Token<'a> {
    XmlDeclaration,
    DocumentTypeDeclaration,
    Comment(&'a str),
    ProcessingInstruction(&'a str, Option<&'a str>),
    Whitespace(&'a str),
    ElementStart(Span<PrefixedName<'a>>),
    ElementStartClose,
    ElementSelfClose,
    ElementClose(Span<PrefixedName<'a>>),
    AttributeStart(Span<PrefixedName<'a>>, &'static str),
    AttributeEnd,
    LiteralAttributeValue(&'a str),
    ReferenceAttributeValue(Reference<'a>),
    CharData(&'a str),
    CData(&'a str),
    ContentReference(Reference<'a>),
}

#[derive(Debug, Copy, Clone)]
enum State {
    AtBeginning,
    AfterDeclaration,
    AfterElementStart(usize),
    AfterAttributeStart(usize, &'static str),
    Content(usize),
    AfterMainElement,
}

#[derive(Debug)]
struct PullParser<'a> {
    pm: XmlMaster<'a>,
    xml: StringPoint<'a>,
    state: State,
}

impl<'a> PullParser<'a> {
    fn new(xml: &str) -> PullParser {
        PullParser {
            pm: ParseMaster::new(),
            xml: StringPoint::new(xml),
            state: State::AtBeginning,
        }
    }
}

fn parse_comment<'a>(xml: StringPoint<'a>) -> XmlProgress<'a, Token> {
    let (xml, _) = try_parse!(xml.consume_literal("<!--").map_err(|_| SpecificError::ExpectedComment));
    let (xml, text) = try_parse!(xml.consume_comment());
    let (xml, _) = try_parse!(xml.expect_literal("-->"));

    success(Token::Comment(text), xml)
}

fn parse_one_quoted_value<'a, T, F>(xml: StringPoint<'a>, quote: &'static str, f: F)
                                    -> XmlProgress<'a, T>
    where F: FnMut(StringPoint<'a>) -> XmlProgress<'a, T>
{
    let mut f = f;
    let (xml, _) = try_parse!(xml.consume_literal(quote).map_err(|_| SpecificError::ExpectedOpeningQuote(quote)));
    let (xml, value) = try_parse!(f(xml));
    let (xml, _) = try_parse!(xml.consume_literal(quote).map_err(|_| SpecificError::ExpectedClosingQuote(quote)));

    success(value, xml)
}

fn parse_quoted_value<'a, T, F>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>, f: F) -> XmlProgress<'a, T>
    where F: FnMut(&mut XmlMaster<'a>, StringPoint<'a>, &str) -> XmlProgress<'a, T>
{
    let mut f = f;
    pm.alternate()
        .one(|pm| parse_one_quoted_value(xml, "'",  |xml| f(pm, xml, "'")))
        .one(|pm| parse_one_quoted_value(xml, "\"", |xml| f(pm, xml, "\"")))
        .finish()
}

fn parse_eq(xml: StringPoint) -> XmlProgress<()> {
    let (xml, _) = xml.consume_space().optional(xml);
    let (xml, _) = try_parse!(xml.expect_literal("="));
    let (xml, _) = xml.consume_space().optional(xml);

    success((), xml)
}

fn parse_version_info<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, &'a str> {
    fn version_num(xml: StringPoint) -> peresil::Progress<StringPoint, &str, ()> {
        let start_point = xml;

        let (xml, _) = try_parse!(xml.consume_literal("1."));
        let (xml, _) = try_parse!(xml.consume_decimal_chars());

        peresil::Progress::success(xml, start_point.to(xml))
    }

    let (xml, _) = try_parse!(xml.expect_space());
    let (xml, _) = try_parse!(xml.expect_literal("version"));
    let (xml, _) = try_parse!(parse_eq(xml));
    let (xml, version) = try_parse!(
        parse_quoted_value(pm, xml, |_, xml, _| version_num(xml).map_err(|_| SpecificError::ExpectedVersionNumber))
            );

    success(version, xml)
}

fn parse_encoding_declaration<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>)
                                  -> XmlProgress<'a, &'a str>
{
    let (xml, _) = try_parse!(xml.expect_space());
    let (xml, _) = try_parse!(xml.expect_literal("encoding"));
    let (xml, _) = try_parse!(parse_eq(xml));
    let (xml, encoding) = try_parse!(
        parse_quoted_value(pm, xml, |_, xml, _| xml.consume_encoding())
            );

    success(encoding, xml)
}

fn parse_standalone_declaration<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>)
                                    -> XmlProgress<'a, &'a str>
{
    let (xml, _) = try_parse!(xml.expect_space());
    let (xml, _) = try_parse!(xml.expect_literal("standalone"));
    let (xml, _) = try_parse!(parse_eq(xml));
    let (xml, standalone) = try_parse!(
        parse_quoted_value(pm, xml, |pm, xml, _| {
            pm.alternate()
                .one(|_| xml.expect_literal("yes"))
                .one(|_| xml.expect_literal("no"))
                .finish()
                .map_err(|_| SpecificError::ExpectedYesNo)
        })
            );

    success(standalone, xml)
}

fn parse_xml_declaration<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, Token<'a>> {
    let (xml, _) = try_parse!(xml.expect_literal("<?xml"));
    let (xml, _version) = try_parse!(parse_version_info(pm, xml));
    let (xml, _encoding) = try_parse!(pm.optional(xml, |pm, xml| {
        parse_encoding_declaration(pm, xml)
    }));
    let (xml, _standalone) = try_parse!(pm.optional(xml, |pm, xml| {
        parse_standalone_declaration(pm, xml)
    }));
    let (xml, _) = xml.consume_space().optional(xml);
    let (xml, _) = try_parse!(xml.expect_literal("?>"));

    success(Token::XmlDeclaration, xml)
}

/* only the SYSTEM variant */
fn parse_external_id<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>)
                         -> XmlProgress<'a, &'a str>
{
    let (xml, _) = try_parse!(xml.expect_space());
    let (xml, _) = try_parse!(xml.expect_literal("SYSTEM"));
    let (xml, _) = try_parse!(xml.expect_space());
    let (xml, external_id) = try_parse!(
        parse_quoted_value(pm, xml, |_, xml, quote|
            xml.consume_attribute_value(quote).map_err(|_| SpecificError::ExpectedSystemLiteral)
        )
    );

    success(external_id, xml)
}

fn parse_int_subset<'a>(_pm: &mut XmlMaster<'a>, xml: StringPoint<'a>)
                          -> XmlProgress<'a, &'a str>
{
    let (xml, _) = try_parse!(xml.expect_literal("["));
    let (xml, _) = xml.consume_space().optional(xml);
    let (xml, elements) = try_parse!(
        xml.consume_int_subset().map_err(|_| SpecificError::ExpectedIntSubset)
    );
    let (xml, _) = xml.consume_space().optional(xml);
    let (xml, _) = try_parse!(xml.expect_literal("]"));
    let (xml, _) = xml.consume_space().optional(xml);

    success(elements, xml)
}

fn parse_document_type_declaration<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, Token<'a>> {
    let (xml, _) = try_parse!(xml.expect_literal("<!DOCTYPE"));
    let (xml, _) = try_parse!(xml.expect_space());
    let (xml, _type_name) = try_parse!(
        xml.consume_name().map_err(|_| SpecificError::ExpectedDocumentTypeName)
    );
    let (xml, _id) = try_parse!(pm.optional(xml, |p, x| parse_external_id(p, x)));
    let (xml, _) = xml.consume_space().optional(xml);
    let (xml, _int_subset) = try_parse!(pm.optional(xml, |p, x| parse_int_subset(p, x)));
    let (xml, _) = try_parse!(xml.expect_literal(">"));

    success(Token::DocumentTypeDeclaration, xml)
}

fn parse_pi_value(xml: StringPoint) -> XmlProgress<&str> {
    let (xml, _) = try_parse!(xml.expect_space());
    xml.consume_pi_value()
}

fn parse_pi<'a>(xml: StringPoint<'a>) -> XmlProgress<'a, Token> {
    let (xml, _) = try_parse!(xml.consume_literal("<?").map_err(|_| SpecificError::ExpectedProcessingInstruction));
    let target_xml = xml;
    let (xml, target) = try_parse!(xml.consume_name().map_err(|_| SpecificError::ExpectedProcessingInstructionTarget));
    let (xml, value) = parse_pi_value(xml).optional(xml);
    let (xml, _) = try_parse!(xml.expect_literal("?>"));

    if target.eq_ignore_ascii_case("xml") {
        return peresil::Progress::failure(target_xml, SpecificError::InvalidProcessingInstructionTarget);
    }

    success(Token::ProcessingInstruction(target, value), xml)
}

fn parse_element_start(xml: StringPoint) -> XmlProgress<Token> {
    let (xml, _) = try_parse!(xml.consume_start_tag());
    let (xml, name) = try_parse!(Span::parse(xml, |xml| xml.consume_prefixed_name().map_err(|_| SpecificError::ExpectedElementName)));

    success(Token::ElementStart(name), xml)
}

fn parse_element_start_close(xml: StringPoint) -> XmlProgress<Token> {
    let (xml, _) = xml.expect_space().optional(xml);

    xml.consume_literal(">")
        .map(|_| Token::ElementStartClose)
        .map_err(|_| SpecificError::ExpectedElementEnd)
}

fn parse_element_self_close(xml: StringPoint) -> XmlProgress<Token> {
    let (xml, _) = xml.expect_space().optional(xml);

    xml.consume_literal("/>")
        .map(|_| Token::ElementSelfClose)
        .map_err(|_| SpecificError::ExpectedElementSelfClosed)
}

fn parse_element_close(xml: StringPoint) -> XmlProgress<Token> {
    let (xml, _) = try_parse!(xml.expect_literal("</"));

    let (xml, name) = try_parse!(Span::parse(xml, |xml| xml.consume_prefixed_name().map_err(|_| SpecificError::ExpectedElementName)));

    let (xml, _) = xml.consume_space().optional(xml);
    let (xml, _) = try_parse!(xml.expect_literal(">"));

    success(Token::ElementClose(name), xml)
}

const QUOT: &'static str = r#"""#;
const APOS: &'static str = r#"'"#;

fn parse_attribute_start<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, Token<'a>> {
    let (xml, _) = try_parse!(xml.expect_space());

    let (xml, name) = try_parse!(Span::parse(xml, |xml| xml.consume_prefixed_name().map_err(|_| SpecificError::ExpectedAttribute)));

    let (xml, _) = try_parse!(parse_eq(xml));

    let (xml, q) = try_parse!(
        pm.alternate()
            .one(|_| xml.expect_literal(QUOT).map_err(|_| SpecificError::ExpectedOpeningQuote(QUOT)))
            .one(|_| xml.expect_literal(APOS).map_err(|_| SpecificError::ExpectedOpeningQuote(APOS)))
            .finish());

    let q = if q == QUOT { QUOT } else { APOS };

    success(Token::AttributeStart(name, q), xml)
}

fn parse_attribute_end<'a>(xml: StringPoint<'a>, quote: &'static str) -> XmlProgress<'a, Token<'a>> {
    xml.consume_literal(quote)
        .map(|_| Token::AttributeEnd)
        .map_err(|_| SpecificError::ExpectedClosingQuote(quote))
}

fn parse_attribute_literal<'a>(xml: StringPoint<'a>, quote: &str) -> XmlProgress<'a, Token<'a>> {
    let (xml, val) = try_parse!(xml.consume_attribute_value(quote));

    success(Token::LiteralAttributeValue(val), xml)
}

fn parse_entity_ref(xml: StringPoint) -> XmlProgress<Reference> {
    let (xml, _) = try_parse!(xml.consume_literal("&").map_err(|_| SpecificError::ExpectedNamedReference));
    let (xml, name) = try_parse!(Span::parse(xml, |xml| xml.consume_name().map_err(|_| SpecificError::ExpectedNamedReferenceValue)));
    let (xml, _) = try_parse!(xml.expect_literal(";"));

    success(EntityReference(name), xml)
}

fn parse_decimal_char_ref(xml: StringPoint) -> XmlProgress<Reference> {
    let (xml, _) = try_parse!(xml.consume_literal("&#").map_err(|_| SpecificError::ExpectedDecimalReference));
    let (xml, dec) = try_parse!(Span::parse(xml, |xml| xml.consume_decimal_chars().map_err(|_| SpecificError::ExpectedDecimalReferenceValue)));
    let (xml, _) = try_parse!(xml.expect_literal(";"));

    success(DecimalCharReference(dec), xml)
}

fn parse_hex_char_ref(xml: StringPoint) -> XmlProgress<Reference> {
    let (xml, _) = try_parse!(xml.consume_literal("&#x").map_err(|_| SpecificError::ExpectedHexReference));
    let (xml, hex) = try_parse!(Span::parse(xml, |xml| xml.consume_hex_chars()));
    let (xml, _) = try_parse!(xml.expect_literal(";"));

    success(HexCharReference(hex), xml)
}

fn parse_reference<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, Reference<'a>> {
    pm.alternate()
        .one(|_| parse_entity_ref(xml))
        .one(|_| parse_decimal_char_ref(xml))
        .one(|_| parse_hex_char_ref(xml))
        .finish()
}

fn parse_attribute_reference<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, Token<'a>> {
    let (xml, val) = try_parse!(parse_reference(pm, xml));

    success(Token::ReferenceAttributeValue(val), xml)
}

fn parse_char_data<'a>(xml: StringPoint<'a>) -> XmlProgress<'a, Token> {
    xml.consume_char_data()
        .map(Token::CharData)
}

fn parse_cdata<'a>(xml: StringPoint<'a>) -> XmlProgress<'a, Token> {
    let (xml, _) = try_parse!(xml.expect_literal("<![CDATA["));
    let (xml, text) = try_parse!(xml.consume_cdata());
    let (xml, _) = try_parse!(xml.expect_literal("]]>"));

    success(Token::CData(text), xml)
}

fn parse_content_reference<'a>(pm: &mut XmlMaster<'a>, xml: StringPoint<'a>) -> XmlProgress<'a, Token<'a>> {
    let (xml, r) = try_parse!(parse_reference(pm, xml));
    success(Token::ContentReference(r), xml)
}

impl<'a> Iterator for PullParser<'a> {
    type Item = Result<Token<'a>, (usize, Vec<SpecificError>)>;

    fn next(&mut self) -> Option<Self::Item> {
        let pm = &mut self.pm;
        let xml = self.xml;

        let r = match self.state {
            State::AtBeginning => {
                pm.alternate()
                    .one(|pm| parse_xml_declaration(pm, xml))
                    .one(|_| parse_element_start(xml))
                    .one(|_| xml.expect_space().map(Token::Whitespace))
                    .one(|_| parse_comment(xml))
                    .one(|_| parse_pi(xml))
                    .finish()
            },

            State::AfterDeclaration => {
                pm.alternate()
                    .one(|pm| parse_document_type_declaration(pm, xml))
                    .one(|_| parse_element_start(xml))
                    .one(|_| xml.expect_space().map(Token::Whitespace))
                    .one(|_| parse_comment(xml))
                    .one(|_| parse_pi(xml))
                    .finish()
            },

            State::AfterElementStart(..) => {
                pm.alternate()
                    .one(|pm| parse_attribute_start(pm, xml))
                    .one(|_| parse_element_start_close(xml))
                    .one(|_| parse_element_self_close(xml))
                    .finish()
            },

            State::AfterAttributeStart(_, quote) => {
                pm.alternate()
                    .one(|_| parse_attribute_literal(xml, quote))
                    .one(|pm| parse_attribute_reference(pm, xml))
                    .one(|_| parse_attribute_end(xml, quote))
                    .finish()
            },

            State::Content(..) => {
                pm.alternate()
                    .one(|_| parse_element_start(xml))
                    .one(|_| parse_element_close(xml))
                    .one(|_| parse_char_data(xml))
                    .one(|_| parse_cdata(xml))
                    .one(|pm| parse_content_reference(pm, xml))
                    .one(|_| parse_comment(xml))
                    .one(|_| parse_pi(xml))
                    .finish()
            },

            State::AfterMainElement => {
                if xml.is_empty() {
                    return None;
                }

                pm.alternate()
                    .one(|_| parse_comment(xml))
                    .one(|_| parse_pi(xml))
                    .one(|_| xml.expect_space().map(Token::Whitespace))
                    .finish()
            },
        };

        let (r, pt) = match pm.finish(r) {
            peresil::Progress { status: peresil::Status::Success(t), point } => (t, point),
            peresil::Progress { status: peresil::Status::Failure(e), point } => {
                return Some(Err((point.offset, e)));
            },
        };

        if pt == xml {
            return None;
        }

        let next_state = match (self.state, r) {
            (State::AtBeginning, Token::XmlDeclaration) |
            (State::AtBeginning, Token::ProcessingInstruction(..)) |
            (State::AtBeginning, Token::Comment(..)) |
            (State::AtBeginning, Token::Whitespace(..)) => {
                State::AfterDeclaration
            },
            (State::AtBeginning, Token::ElementStart(..)) => {
                State::AfterElementStart(0)
            },

            (State::AfterDeclaration, Token::ProcessingInstruction(..)) |
            (State::AfterDeclaration, Token::Comment(..)) |
            (State::AfterDeclaration, Token::Whitespace(..)) => {
                State::AfterDeclaration
            },
            (State::AfterDeclaration, Token::DocumentTypeDeclaration) => {
                State::AfterDeclaration
            },
            (State::AfterDeclaration, Token::ElementStart(..)) => {
                State::AfterElementStart(0)
            },

            (State::AfterElementStart(d), Token::AttributeStart(_, q)) => {
                State::AfterAttributeStart(d, q)
            },
            (State::AfterElementStart(d), Token::ElementStartClose) => {
                State::Content(d)
            },
            (State::AfterElementStart(0), Token::ElementSelfClose) => {
                State::AfterMainElement
            },
            (State::AfterElementStart(d), Token::ElementSelfClose) => {
                State::Content(d - 1)
            },

            (State::AfterAttributeStart(d, q), Token::LiteralAttributeValue(..)) |
            (State::AfterAttributeStart(d, q), Token::ReferenceAttributeValue(..)) => {
                State::AfterAttributeStart(d, q)
            },
            (State::AfterAttributeStart(d, _), Token::AttributeEnd) => {
                State::AfterElementStart(d)
            },

            (State::Content(d), Token::CharData(..)) |
            (State::Content(d), Token::CData(..)) |
            (State::Content(d), Token::ContentReference(..)) |
            (State::Content(d), Token::Comment(..)) |
            (State::Content(d), Token::ProcessingInstruction(..)) => {
                State::Content(d)
            },
            (State::Content(d), Token::ElementStart(..)) => {
                State::AfterElementStart(d + 1)
            }
            (State::Content(0), Token::ElementClose(..)) => {
                State::AfterMainElement
            }
            (State::Content(d), Token::ElementClose(..)) => {
                State::Content(d - 1)
            }

            (State::AfterMainElement, Token::Comment(..)) |
            (State::AfterMainElement, Token::ProcessingInstruction(..)) |
            (State::AfterMainElement, Token::Whitespace(..)) => {
                State::AfterMainElement
            }

            (s, t) => {
                unreachable!("Transitioning from {:?} to {:?} is impossible", s, t);
            },
        };

        self.state = next_state;
        self.xml = pt;

        Some(Ok(r))
    }
}

struct DomBuilder<'d> {
    doc: dom::Document<'d>,
    elements: Vec<dom::Element<'d>>,
    element_names: Vec<Span<PrefixedName<'d>>>,
    attributes: Vec<DeferredAttribute<'d>>,
    seen_top_element: bool,
}

impl<'d> DomBuilder<'d> {
    fn new(doc: dom::Document<'d>) -> DomBuilder<'d> {
        DomBuilder {
            doc: doc,
            elements: vec![],
            element_names: Vec::new(),
            attributes: Vec::new(),
            seen_top_element: false,
        }
    }

    fn append_to_either<T>(&self, child: T)
        where T: Into<dom::ChildOfRoot<'d>>
    {
        match self.elements.last() {
            None => self.doc.root().append_child(child),
            Some(parent) => parent.append_child(child.into()),
        }
    }

    fn default_namespace_uri(&self) -> Option<&str> {
        self.elements.last().and_then(|e| e.recursive_default_namespace_uri())
    }

    fn namespace_uri_for_prefix(&self, prefix: &str) -> Option<&str> {
        self.elements.last().and_then(|e| e.namespace_uri_for_prefix(prefix))
    }

    fn finish_opening_tag(&mut self) -> DomBuilderResult<()> {
        let deferred_element = self.element_names.last().expect("Unknown element name");
        let attributes = DeferredAttributes::new(replace(&mut self.attributes, Vec::new()));

        try!(attributes.check_duplicates());
        let default_namespace = try!(attributes.default_namespace());

        let mut new_prefix_mappings = HashMap::new();
        for ns in attributes.namespaces() {
            let value = try!(AttributeValueBuilder::convert(&ns.values));

            if value.is_empty() {
                return Err(ns.name.map(|_| SpecificError::EmptyNamespace));
            }

            new_prefix_mappings.insert(ns.name.value.local_part, value);
        }
        let new_prefix_mappings = new_prefix_mappings;

        let element_name = &deferred_element.value;

        let element = if let Some(prefix) = element_name.prefix {
            let ns_uri = new_prefix_mappings.get(prefix).map(|p| &p[..]);
            let ns_uri = ns_uri.or_else(|| self.namespace_uri_for_prefix(prefix));

            if let Some(ns_uri) = ns_uri {
                let element = self.doc.create_element((ns_uri, element_name.local_part));
                element.set_preferred_prefix(Some(prefix));
                element
            } else {
                return Err(deferred_element.map(|_| SpecificError::UnknownNamespacePrefix));
            }
        } else if let Some(ns_uri) = default_namespace {
            if ns_uri.is_empty() {
                let element = self.doc.create_element(element_name.local_part);
                element.set_default_namespace_uri(None);
                element
            } else {
                let element = self.doc.create_element((&ns_uri[..], element_name.local_part));
                element.set_default_namespace_uri(Some(&ns_uri));
                element
            }
        } else {
            let ns_uri = self.default_namespace_uri();
            let name = QName::with_namespace_uri(ns_uri, element_name.local_part);
            self.doc.create_element(name)
        };

        for (prefix, ns_uri) in &new_prefix_mappings {
            element.register_prefix(*prefix, ns_uri);
        }

        if !self.seen_top_element {
            self.seen_top_element = true;
            element.register_prefix(::XML_NS_PREFIX, ::XML_NS_URI);
        }

        self.append_to_either(element);
        self.elements.push(element);

        let mut builder = AttributeValueBuilder::new();

        for attribute in attributes.attributes() {
            let name = &attribute.name.value;

            builder.clear();
            try!(builder.ingest(&attribute.values));

            if let Some(prefix) = name.prefix {
                let ns_uri = new_prefix_mappings.get(prefix).map(|p| &p[..]);
                let ns_uri = ns_uri.or_else(|| self.namespace_uri_for_prefix(prefix));

                if let Some(ns_uri) = ns_uri {
                    let attr = element.set_attribute_value((ns_uri, name.local_part), &builder);
                    attr.set_preferred_prefix(Some(prefix));
                } else {
                    return Err(attribute.name.map(|_| SpecificError::UnknownNamespacePrefix))
                }
            } else {
                element.set_attribute_value(name.local_part, &builder);
            }
        }

        Ok(())
    }

    fn add_attribute_value(&mut self, v: AttributeValue<'d>) {
        let a = self.attributes.last_mut().expect("Cannot add attribute value without an attribute");
        a.values.push(v);
    }

    fn add_text_data(&self, text: &str) {
        let e = self.elements.last().expect("Cannot add text node without a parent");
        let t = self.doc.create_text(text);
        e.append_child(t);
    }

    fn has_unclosed_elements(&self) -> bool {
        !self.elements.is_empty()
    }

    fn consume(&mut self, token: Token<'d>) -> DomBuilderResult<()> {
        use self::Token::*;

        match token {
            XmlDeclaration => {},

            DocumentTypeDeclaration => {},

            ElementStart(n) => {
                self.element_names.push(n);
            },

            ElementStartClose => {
                try!(self.finish_opening_tag());
            },

            ElementSelfClose => {
                try!(self.finish_opening_tag());

                self.element_names.pop();
                self.elements.pop();
            },

            ElementClose(n) => {
                let open_name = self.element_names.pop().expect("No open element");
                self.elements.pop();

                if n.value != open_name.value {
                    return Err(n.map(|_| SpecificError::MismatchedElementEndName));
                }
            },

            AttributeStart(n, _) => {
                let attr = DeferredAttribute { name: n, values: Vec::new() };
                self.attributes.push(attr);
            },

            LiteralAttributeValue(v) => {
                self.add_attribute_value(AttributeValue::LiteralAttributeValue(v));
            },

            ReferenceAttributeValue(v) => {
                self.add_attribute_value(AttributeValue::ReferenceAttributeValue(v));
            },

            AttributeEnd => {},

            Whitespace(..) => {},

            CharData(t) | CData(t) => {
                self.add_text_data(t)
            },

            ContentReference(t) => {
                try!(decode_reference(t, |s| self.add_text_data(s)));
            },

            Comment(c) => {
                let c = self.doc.create_comment(c);
                self.append_to_either(c);
            },

            ProcessingInstruction(t, v) => {
                let pi = self.doc.create_processing_instruction(t, v);
                self.append_to_either(pi);
            },
        };

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    location: usize,
    errors: BTreeSet<SpecificError>,
}

impl Error {
    fn new(location: usize, error: SpecificError) -> Self {
        let mut errors = BTreeSet::new();
        errors.insert(error);
        Error { location, errors }
    }

    pub fn location(&self) -> usize { self.location }
}

impl From<(usize, Vec<SpecificError>)> for Error {
    fn from(other: (usize, Vec<SpecificError>)) -> Self {
        let (location, errors) = other;
        let errors = errors.into_iter().collect();
        Error { location, errors }
    }
}

impl From<Span<SpecificError>> for Error {
    fn from(other: Span<SpecificError>) -> Self {
        Self::new(other.offset, other.value)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "XML parsing error at {}: {:?}", self.location, self.errors)
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        "Unable to parse XML"
    }
}

/// Parses a string into a DOM. On failure, the location of the
/// parsing failure and all possible failures will be returned.
pub fn parse(xml: &str) -> Result<super::Package, Error> {
    let parser = PullParser::new(xml);
    let package = super::Package::new();

    {
        let doc = package.as_document();
        let mut builder = DomBuilder::new(doc);

        for token in parser {
            let token = try!(token);
            try!(builder.consume(token));
        }

        if builder.has_unclosed_elements() {
            return Err(Error::new(xml.len(), SpecificError::UnclosedElement));
        }
    }

    Ok(package)
}

type DomBuilderResult<T> = Result<T, Span<SpecificError>>;

fn decode_reference<F>(ref_data: Reference, cb: F) -> DomBuilderResult<()>
    where F: FnOnce(&str)
{
    match ref_data {
        DecimalCharReference(span) => {
            u32::from_str_radix(span.value, 10).ok()
                .and_then(char::from_u32)
                .ok_or(span.map(|_| SpecificError::InvalidDecimalReference))
                .and_then(|c| {
                    let s: String = iter::repeat(c).take(1).collect();
                    cb(&s);
                    Ok(())
                })
        },
        HexCharReference(span) => {
            u32::from_str_radix(span.value, 16).ok()
                .and_then(char::from_u32)
                .ok_or(span.map(|_| SpecificError::InvalidHexReference))
                .and_then(|c| {
                    let s: String = iter::repeat(c).take(1).collect();
                    cb(&s);
                    Ok(())
                })
        },
        EntityReference(span) => {
            let s = match span.value {
                "amp"  => "&",
                "lt"   => "<",
                "gt"   => ">",
                "apos" => "'",
                "quot" => "\"",
                _      => return Err(span.map(|_| SpecificError::UnknownNamedReference)),
            };
            cb(s);
            Ok(())
        }
    }
}

#[derive(Debug,Copy,Clone)]
enum AttributeValue<'a> {
    ReferenceAttributeValue(Reference<'a>),
    LiteralAttributeValue(&'a str),
}

struct AttributeValueBuilder {
    value: String,
}

impl AttributeValueBuilder {
    fn convert(values: &[AttributeValue]) -> DomBuilderResult<String> {
        let mut builder = AttributeValueBuilder::new();
        try!(builder.ingest(values));
        Ok(builder.implode())
    }

    fn new() -> AttributeValueBuilder {
        AttributeValueBuilder {
            value: String::new(),
        }
    }

    fn ingest(&mut self, values: &[AttributeValue]) -> DomBuilderResult<()> {
        use self::AttributeValue::*;

        for value in values.iter() {
            match *value {
                LiteralAttributeValue(v) => self.value.push_str(v),
                ReferenceAttributeValue(r) => try!(decode_reference(r, |s| self.value.push_str(s))),
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

#[derive(Debug)]
struct DeferredAttribute<'d> {
    name: Span<PrefixedName<'d>>,
    values: Vec<AttributeValue<'d>>,
}

struct DeferredAttributes<'a> {
    attributes: Vec<DeferredAttribute<'a>>,
    namespaces: Vec<DeferredAttribute<'a>>,
    default_namespaces: Vec<DeferredAttribute<'a>>,
}

impl<'a> DeferredAttributes<'a> {
    fn new(attributes: Vec<DeferredAttribute<'a>>) -> DeferredAttributes<'a> {
        let (mut namespaces, attributes): (Vec<_>, Vec<_>) =
            attributes.into_iter().partition(|attr| attr.name.value.prefix == Some("xmlns"));

        let (mut default_namespaces, mut attributes): (Vec<_>, Vec<_>) =
            attributes.into_iter().partition(|attr| attr.name.value.local_part == "xmlns");

        fn sort_by_name(a: &DeferredAttribute, b: &DeferredAttribute) -> ::std::cmp::Ordering {
            a.name.value.cmp(&b.name.value)
        }

        attributes.sort_by(sort_by_name);
        namespaces.sort_by(sort_by_name);
        default_namespaces.sort_by(sort_by_name);

        DeferredAttributes {
            attributes: attributes,
            namespaces: namespaces,
            default_namespaces: default_namespaces,
        }
    }

    fn check_duplicates(&self) -> DomBuilderResult<()> {
        for w in self.attributes.windows(2) {
            if w[0].name.value == w[1].name.value {
                return Err(w[1].name.map(|_| SpecificError::DuplicateAttribute));
            }
        }

        for w in self.namespaces.windows(2) {
            if w[0].name.value == w[1].name.value {
                return Err(w[1].name.map(|_| SpecificError::RedefinedNamespace));
            }
        }

        Ok(())
    }

    fn attributes(&self) -> &[DeferredAttribute<'a>] {
        &self.attributes
    }

    fn namespaces(&self) -> &[DeferredAttribute<'a>] {
        &self.namespaces
    }

    fn default_namespace(&self) -> DomBuilderResult<Option<String>> {
        match self.default_namespaces.len() {
            0 => Ok(None),
            1 => {
                let ns = &self.default_namespaces[0];
                let value = try!(AttributeValueBuilder::convert(&ns.values));
                Ok(Some(value))
            },
            _ => {
                let last_namespace = self.default_namespaces.last().unwrap();
                Err(last_namespace.name.map(|_| SpecificError::RedefinedDefaultNamespace))
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ::{dom, Package, QName};

    macro_rules! assert_qname_eq(
        ($l:expr, $r:expr) => (assert_eq!(Into::<QName>::into($l), $r.into()));
    );

    fn full_parse(xml: &str) -> Result<Package, Error> {
        super::parse(xml)
    }

    fn quick_parse(xml: &str) -> Package {
        full_parse(xml).expect("Failed to parse the XML string")
    }

    fn top<'d>(doc: &'d dom::Document<'d>) -> dom::Element<'d> {
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
    fn a_prolog_with_an_encoding() {
        let package = quick_parse("<?xml version='1.0' encoding='UTF-8' ?><hello />");
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn a_non_standalone_prolog() {
        let package = quick_parse("<?xml version='1.0' standalone='no'?><hello/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn a_complete_prolog() {
        let package = quick_parse("<?xml version='1.0' encoding='UTF-8' standalone='yes'?><hello/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn a_prolog_with_a_doc_type_declaration_external_id() {
        let package = quick_parse(r#"<?xml version='1.0'?>
        <!DOCTYPE doc SYSTEM "http://example.com/doc.dtd">
        <hello/>"#);
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn a_prolog_with_a_doc_type_declaration_int_subset() {
        let package = quick_parse(r#"<?xml version="1.0"?>
            <!DOCTYPE note [
            <!ELEMENT note (to,from,heading,body)>
            <!ELEMENT to (#PCDATA)>
            <!ELEMENT from (#PCDATA)>
            <!ELEMENT heading (#PCDATA)>
            <!ELEMENT body (#PCDATA)>
            ]>
            <note>
            <to>Tove</to>
            <from>Jani</from>
            <heading>Reminder</heading>
            <body>Don't forget me this weekend</body>
            </note>
        "#);
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "note");
    }

    #[test]
    fn a_prolog_with_a_doc_type_declaration_int_subset_trailing_ws() {
        let package = quick_parse(r#"<?xml version="1.0"?>
            <!DOCTYPE note
                [
                    <!ELEMENT note (to,from,heading,body)>
                    <!ELEMENT to (#PCDATA)>
                    <!ELEMENT from (#PCDATA)>
                    <!ELEMENT heading (#PCDATA)>
                    <!ELEMENT body (#PCDATA)>
                ]

            >
            <note>
            <to>Tove</to>
            <from>Jani</from>
            <heading>Reminder</heading>
            <body>Don't forget me this weekend</body>
            </note>
        "#);
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "note");
    }

    #[test]
    fn a_prolog_with_a_doc_type_declaration_zero_def() {
        let package = quick_parse("<?xml version='1.0'?>
        <!DOCTYPE doc>
        <hello/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn a_prolog_with_a_doc_type_declaration_zero_def_trailing_ws() {
        let package = quick_parse("<?xml version='1.0'?>
        <!DOCTYPE doc  >
        <hello/>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn a_prolog_with_a_doc_type_declaration_both_int_subset_and_external_id() {
        let package = quick_parse(r#"<?xml version='1.0'?>
        <!DOCTYPE doc SYSTEM "http://example.com/doc.dtd" [
        <!ELEMENT hello (#PCDATA)>
        ]>
        <hello/>"#);
        let doc = package.as_document();
        let top = top(&doc);

        assert_qname_eq!(top.name(), "hello");
    }

    #[test]
    fn a_prolog_with_a_doc_type_declaration_both_int_subset_and_external_id_trailing_ws() {
        let package = quick_parse(r#"<?xml version='1.0'?>
        <!DOCTYPE doc SYSTEM "http://example.com/doc.dtd" [
        <!ELEMENT hello (#PCDATA)>
         ]  >
        <hello/>"#);
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
    fn an_attribute_with_xml_space_preserve() {
        let package = quick_parse("<hello xml:space='preserve'> <a/> </hello>");
        let doc = package.as_document();
        let top = top(&doc);

        assert_eq!(top.attribute((::XML_NS_URI, "space")).unwrap().value(), "preserve");

        let children = top.children();
        assert_eq!(children.len(), 3);
        assert_eq!(children[0].text().unwrap().text(), " ");
        assert_qname_eq!(children[1].element().unwrap().name(), "a");
        assert_eq!(children[2].text().unwrap().text(), " ");
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
    fn nested_elements_with_inherited_default_namespace() {
        let package = quick_parse("<hello xmlns='outer'><world/></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        assert_qname_eq!(world.name(), ("outer", "world"));
    }

    #[test]
    fn nested_elements_with_reset_default_namespace() {
        let package = quick_parse("<hello xmlns='outer'><world xmlns=''/></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        assert_qname_eq!(world.name(), "world");
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

    macro_rules! assert_parse_failure {
        ($actual:expr, $pos:expr, $($err:expr),+) => {
            {
                let errors = vec![$($err),+];
                let expected = Err(Error::from(($pos, errors)));
                assert_eq!($actual, expected);
            }
        }
    }

    #[test]
    fn failure_invalid_encoding() {
        use super::SpecificError::*;

        let r = full_parse("<?xml version='1.0' encoding='8BIT' ?><hi/>");

        assert_parse_failure!(r, 30, ExpectedEncoding);
    }

    #[test]
    fn failure_invalid_standalone() {
        use super::SpecificError::*;

        let r = full_parse("<?xml version='1.0' standalone='invalid'?><hello/>");

        assert_parse_failure!(r, 32, ExpectedYesNo);
    }

    #[test]
    fn failure_no_open_brace() {
        use super::SpecificError::*;

        let r = full_parse("hi />");

        assert_parse_failure!(r, 0, Expected("<?xml"), ExpectedComment, ExpectedProcessingInstruction, ExpectedWhitespace, ExpectedElement);
    }

    #[test]
    fn failure_unclosed_tag() {
        use super::SpecificError::*;

        let r = full_parse("<hi");

        assert_parse_failure!(r, 3, ExpectedWhitespace, ExpectedElementSelfClosed, ExpectedElementEnd);
    }

    #[test]
    fn failure_unexpected_space() {
        use super::SpecificError::*;

        let r = full_parse("<hi / >");

        assert_parse_failure!(r, 4, ExpectedAttribute, ExpectedElementSelfClosed, ExpectedElementEnd);
    }

    #[test]
    fn failure_attribute_without_open_quote() {
        use super::SpecificError::*;

        let r = full_parse("<hi oops=value' />");

        assert_parse_failure!(r, 9, ExpectedOpeningQuote("\'"), ExpectedOpeningQuote("\""));
    }

    #[test]
    fn failure_attribute_without_close_quote() {
        use super::SpecificError::*;

        let r = full_parse("<hi oops='value />");

        assert_parse_failure!(r, 18, ExpectedNamedReference, ExpectedDecimalReference, ExpectedAttributeValue, ExpectedHexReference, ExpectedClosingQuote("\'"));
    }

    #[test]
    fn failure_unclosed_attribute_and_tag() {
        use super::SpecificError::*;

        let r = full_parse("<hi oops='value");

        assert_parse_failure!(r, 15, ExpectedNamedReference, ExpectedDecimalReference, ExpectedAttributeValue, ExpectedHexReference, ExpectedClosingQuote("\'"));
    }

    #[test]
    fn failure_nested_unclosed_tag() {
        use super::SpecificError::*;

        let r = full_parse("<hi><oops</hi>");

        assert_parse_failure!(r, 9, ExpectedWhitespace, ExpectedElementSelfClosed, ExpectedElementEnd);
    }

    #[test]
    fn failure_missing_close_tag() {
        use super::SpecificError::*;

        let r = full_parse("<hi>wow");

        assert_parse_failure!(r, 7, UnclosedElement);
    }

    #[test]
    fn failure_nested_unexpected_space() {
        use super::SpecificError::*;

        let r = full_parse("<hi><oops / ></hi>");

        assert_parse_failure!(r, 10, ExpectedAttribute, ExpectedElementSelfClosed, ExpectedElementEnd);
    }

    #[test]
    fn failure_malformed_entity_reference() {
        use super::SpecificError::*;

        let r = full_parse("<hi>Entity: &;</hi>");

        assert_parse_failure!(r, 13, ExpectedNamedReferenceValue);
    }

    #[test]
    fn failure_nested_malformed_entity_reference() {
        use super::SpecificError::*;

        let r = full_parse("<hi><bye>Entity: &;</bye></hi>");

        assert_parse_failure!(r, 18, ExpectedNamedReferenceValue);
    }

    #[test]
    fn failure_nested_attribute_without_open_quote() {
        use super::SpecificError::*;

        let r = full_parse("<hi><bye oops=value' /></hi>");

        assert_parse_failure!(r, 14, ExpectedOpeningQuote("\'"), ExpectedOpeningQuote("\""));
    }

    #[test]
    fn failure_nested_attribute_without_close_quote() {
        use super::SpecificError::*;

        let r = full_parse("<hi><bye oops='value /></hi>");

        assert_parse_failure!(r, 23, ExpectedNamedReference, ExpectedDecimalReference, ExpectedAttributeValue, ExpectedHexReference, ExpectedClosingQuote("\'"));
    }

    #[test]
    fn failure_nested_unclosed_attribute_and_tag() {
        use super::SpecificError::*;

        let r = full_parse("<hi><bye oops='value</hi>");

        assert_parse_failure!(r, 20, ExpectedNamedReference, ExpectedDecimalReference, ExpectedAttributeValue, ExpectedHexReference, ExpectedClosingQuote("\'"));
    }

    #[test]
    fn failure_pi_target_as_xml() {
        use super::SpecificError::*;

        let r = full_parse("<a><?xml?></a>");

        assert_parse_failure!(r, 5, InvalidProcessingInstructionTarget);
    }

    #[test]
    fn failure_end_tag_does_not_match() {
        use super::SpecificError::*;

        let r = full_parse("<a></b>");

        assert_parse_failure!(r, 5, MismatchedElementEndName);
    }

    #[test]
    fn failure_invalid_decimal_reference() {
        use super::SpecificError::*;

        let r = full_parse("<a>&#99999999;</a>");

        assert_parse_failure!(r, 5, InvalidDecimalReference);
    }

    #[test]
    fn failure_invalid_hex_reference() {
        use super::SpecificError::*;

        let r = full_parse("<a>&#x99999999;</a>");

        assert_parse_failure!(r, 6, InvalidHexReference);
    }

    #[test]
    fn failure_unknown_named_reference() {
        use super::SpecificError::*;

        let r = full_parse("<a>&fake;</a>");

        assert_parse_failure!(r, 4, UnknownNamedReference);
    }

    #[test]
    fn failure_duplicate_attribute() {
        use super::SpecificError::*;

        let r = full_parse("<a b='c' b='d'/>");

        assert_parse_failure!(r, 9, DuplicateAttribute);
    }

    #[test]
    fn failure_redefined_namespace() {
        use super::SpecificError::*;

        let r = full_parse("<a xmlns:b='c' xmlns:b='d'/>");

        assert_parse_failure!(r, 15, RedefinedNamespace);
    }

    #[test]
    fn failure_redefined_default_namespace() {
        use super::SpecificError::*;

        let r = full_parse("<a xmlns='a' xmlns='b'/>");

        assert_parse_failure!(r, 13, RedefinedDefaultNamespace);
    }

    #[test]
    fn failure_empty_namespace() {
        use super::SpecificError::*;

        let r = full_parse("<a xmlns:b=''/>");

        assert_parse_failure!(r, 3, EmptyNamespace);
    }

    #[test]
    fn failure_unknown_attribute_namespace_prefix() {
        use super::SpecificError::*;

        let r = full_parse("<a b:foo='a'/>");

        assert_parse_failure!(r, 3, UnknownNamespacePrefix);
    }

    #[test]
    fn failure_unknown_element_namespace_prefix() {
        use super::SpecificError::*;

        let r = full_parse("<b:a/>");

        assert_parse_failure!(r, 1, UnknownNamespacePrefix);
    }

    #[test]
    fn failure_is_an_error() {
        fn __assert_well_behaved_error()
        where
            Error: ::std::error::Error + Send + Sync + 'static,
        {}
    }
}
