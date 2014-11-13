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
//! ### Influences
//!
//! - http://www.scheidecker.net/2012/12/03/parser-combinators/

use std::ascii::AsciiExt;
use std::char::from_u32;
use std::num::from_str_radix;

use self::xmlstr::XmlStr;
use super::Package;
use super::dom4;

mod xmlstr;

pub struct Parser;

#[deriving(Show)]
struct Document<'a> {
    before_children: Vec<RootChild<'a>>,
    element: Element<'a>,
    after_children: Vec<RootChild<'a>>,
}

#[deriving(Show)]
struct Element<'a> {
    name: &'a str,
    attributes: Vec<Attribute<'a>>,
    children: Vec<Child<'a>>,
}

#[deriving(Show)]
enum AttributeValue<'a> {
    ReferenceAttributeValue(Reference<'a>),
    LiteralAttributeValue(&'a str),
}

#[deriving(Show)]
struct Attribute<'a> {
    name: &'a str,
    values: Vec<AttributeValue<'a>>,
}

#[deriving(Show)]
struct Text<'a> {
    text: &'a str,
}

#[deriving(Show)]
enum Reference<'a> {
    EntityReference(&'a str),
    DecimalCharReference(&'a str),
    HexCharReference(&'a str),
}

#[deriving(Show)]
struct Comment<'a> {
    text: &'a str,
}

#[deriving(Show)]
struct ProcessingInstruction<'a> {
    target: &'a str,
    value: Option<&'a str>,
}

#[deriving(Show)]
enum RootChild<'a> {
    CommentRootChild(Comment<'a>),
    PIRootChild(ProcessingInstruction<'a>),
    IgnoredRootChild,
}

#[deriving(Show)]
enum Child<'a> {
    ElementChild(Element<'a>),
    TextChild(Text<'a>),
    ReferenceChild(Reference<'a>),
    CommentChild(Comment<'a>),
    PIChild(ProcessingInstruction<'a>),
}

fn best_failure<'a>(mut errors: Vec<ParseFailure<'a>>) -> ParseFailure<'a> {
    errors.sort_by(|l, r| l.point.offset.cmp(&r.point.offset));
    errors.pop().expect("Called without any errors")
}

macro_rules! try_parse(
    ($e:expr) => ({
        match $e {
            Success(x) => x,
            Partial((_, pf, _)) |
            Failure(pf) => return Failure(pf),
        }
    })
)

macro_rules! try_partial_parse(
    ($e:expr) => ({
        match $e {
            Success((v, xml)) => (v, vec![], xml),
            Partial((v, pf, xml)) => (v, vec![pf], xml),
            Failure(pf) => return Failure(pf),
        }
    })
)

macro_rules! try_resume_after_partial_failure(
    ($partial:expr, $e:expr) => ({
        match $e {
            Success(x) => x,
            Partial((_, pf, _)) |
            Failure(pf) => {
                let mut partial = $partial;
                partial.push(pf);
                return Failure(best_failure(partial))
            },
        }
    });
)

// Pattern: zero-or-one
macro_rules! parse_optional(
    ($parser:expr, $start:expr) => ({
        match $parser {
            Success((value, next)) => (Some(value), next),
            Partial((value, _, next)) => (Some(value), next),
            Failure(_) => (None, $start),
        }
    })
)

// Pattern: alternate
macro_rules! parse_alternate_rec(
    ($start:expr, $errors:expr, {}) => ({
        Failure(best_failure($errors))
    });
    ($start:expr, $errors:expr, {
        [$parser:expr -> $transformer:expr],
        $([$parser_rest:expr -> $transformer_rest:expr],)*
    }) => (
        match $parser($start) {
            Success((val, next)) => Success(($transformer(val), next)),
            Partial((_, pf, _)) |
            Failure(pf) => {
                $errors.push(pf);
                parse_alternate_rec!($start, $errors, {
                    $([$parser_rest -> $transformer_rest],)*
                })
            },
        }
    );
)

macro_rules! parse_alternate(
    ($start:expr, {
        $([$parser_rest:expr -> $transformer_rest:expr],)*
    }) => ({
        let mut errors = Vec::new();
        parse_alternate_rec!($start, errors, {
            $([$parser_rest -> $transformer_rest],)*
        })
    });
)

// Pattern: zero-or-more
macro_rules! parse_zero_or_more(
    ($start:expr, $parser:expr) => {{
        let mut items = Vec::new();
        let mut err;

        let mut start = $start;
        loop {
            let (item, next_start) = match $parser(start) {
                Success(x) => x,
                Partial((_, pf, _)) |
                Failure(pf) => {
                    err = Some(pf);
                    break
                }
            };

            items.push(item);
            start = next_start;
        }

        Partial((items, err.unwrap(), start))
    }};
)

#[deriving(Show,Clone,PartialEq)]
struct StartPoint<'a> {
    offset: uint,
    s: &'a str,
}

impl<'a> StartPoint<'a> {
    fn slice_at(&self, position: uint) -> (&'a str, StartPoint<'a>) {
        (self.s.slice_to(position), StartPoint{offset: self.offset + position,
                                               s: self.s.slice_from(position)})
    }

    fn consume_to(&self, l: Option<uint>) -> ParseResult<'a, &'a str> {
        match l {
            None => Failure(ParseFailure{point: self.clone()}),
            Some(position) => Success(self.slice_at(position)),
        }
    }

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

struct ParseFailure<'a> {
    point: StartPoint<'a>,
}

enum ParseResult<'a, T> {
    Success((T, StartPoint<'a>)),
    Partial((T, ParseFailure<'a>, StartPoint<'a>)),
    Failure((ParseFailure<'a>)),
}

impl Parser {
    pub fn new() -> Parser {
        Parser
    }

    fn parse_eq<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, ()> {
        let (_, xml) = parse_optional!(xml.consume_space(), xml);
        let (_, xml) = try_parse!(xml.consume_literal("="));
        let (_, xml) = parse_optional!(xml.consume_space(), xml);

        Success(((), xml))
    }

    fn parse_version_info<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, &'a str> {
        let (_, xml) = try_parse!(xml.consume_space());
        let (_, xml) = try_parse!(xml.consume_literal("version"));
        let (_, xml) = try_parse!(self.parse_eq(xml));
        let (version, xml) = try_parse!(
            self.parse_quoted_value(xml, |xml, _| xml.consume_version_num())
        );

        Success((version, xml))
    }

    fn parse_xml_declaration<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, ()> {
        let (_, xml) = try_parse!(xml.consume_literal("<?xml"));
        let (_version, xml) = try_parse!(self.parse_version_info(xml));
        // let (encoding, xml) = parse_optional!(self.parse_encoding_declaration(xml));
        // let (standalone, xml) = parse_optional!(self.parse_standalone_declaration(xml));
        let (_, xml) = parse_optional!(xml.consume_space(), xml);
        let (_, xml) = try_parse!(xml.consume_literal("?>"));

        Success(((), xml))
    }

    fn parse_misc<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, RootChild<'a>> {
        parse_alternate!(xml, {
            [|xml: StartPoint<'a>| self.parse_comment(xml) -> |c| CommentRootChild(c)],
            [|xml: StartPoint<'a>| self.parse_pi(xml)      -> |p| PIRootChild(p)],
            [|xml: StartPoint<'a>| xml.consume_space()     -> |_| IgnoredRootChild],
        })
    }

    fn parse_miscs<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Vec<RootChild<'a>>> {
        parse_zero_or_more!(xml, |xml| self.parse_misc(xml))
    }

    fn parse_prolog<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Vec<RootChild<'a>>> {
        let (_, xml) = parse_optional!(self.parse_xml_declaration(xml), xml);
        self.parse_miscs(xml)
    }

    fn parse_one_quoted_value<'a, T>(&self,
                                     xml: StartPoint<'a>,
                                     quote: &str,
                                     f: |StartPoint<'a>| -> ParseResult<'a, T>)
                                     -> ParseResult<'a, T>
    {
        let (_, xml) = try_parse!(xml.consume_literal(quote));
        let (value, f, xml) = try_partial_parse!(f(xml));
        let (_, xml) = try_resume_after_partial_failure!(f, xml.consume_literal(quote));

        Success((value, xml))
    }

    fn parse_quoted_value<'a, T>(&self,
                                 xml: StartPoint<'a>,
                                 f: |StartPoint<'a>, &str| -> ParseResult<'a, T>)
                                 -> ParseResult<'a, T>
    {
        parse_alternate!(xml, {
            [|xml| self.parse_one_quoted_value(xml, "'",  |xml| f(xml, "'"))  -> |v| v],
            [|xml| self.parse_one_quoted_value(xml, "\"", |xml| f(xml, "\"")) -> |v| v],
        })
    }

    fn parse_attribute_values<'a>(&self, xml: StartPoint<'a>, quote: &str)
                                  -> ParseResult<'a, Vec<AttributeValue<'a>>>
    {
        parse_zero_or_more!(xml, |xml|
            parse_alternate!(xml, {
                [|xml: StartPoint<'a>| xml.consume_attribute_value(quote) -> |v| LiteralAttributeValue(v)],
                [|xml: StartPoint<'a>| self.parse_reference(xml)          -> |e| ReferenceAttributeValue(e)],
            }))
    }

    fn parse_attribute<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Attribute<'a>> {
        let (_, xml) = try_parse!(xml.consume_space());

        let (name, xml) = try_parse!(xml.consume_name());

        let (_, xml) = try_parse!(self.parse_eq(xml));

        let (values, xml) = try_parse!(
            self.parse_quoted_value(xml, |xml, quote| self.parse_attribute_values(xml, quote))
        );

        Success((Attribute{name: name, values: values}, xml))
    }

    fn parse_attributes<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Vec<Attribute<'a>>> {
        parse_zero_or_more!(xml, |xml| self.parse_attribute(xml))
    }

    fn parse_element_end<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, &'a str> {
        let (_, xml) = try_parse!(xml.consume_literal("</"));
        let (name, xml) = try_parse!(xml.consume_name());
        let (_, xml) = parse_optional!(xml.consume_space(), xml);
        let (_, xml) = try_parse!(xml.consume_literal(">"));
        Success((name, xml))
    }

    fn parse_char_data<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Text<'a>> {
        let (text, xml) = try_parse!(xml.consume_char_data());

        Success((Text{text: text}, xml))
    }

    fn parse_cdata<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Text<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("<![CDATA["));
        let (text, xml) = try_parse!(xml.consume_cdata());
        let (_, xml) = try_parse!(xml.consume_literal("]]>"));

        Success((Text{text: text}, xml))
    }

    fn parse_entity_ref<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Reference<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("&"));
        let (name, xml) = try_parse!(xml.consume_name());
        let (_, xml) = try_parse!(xml.consume_literal(";"));

        Success((EntityReference(name), xml))
    }

    fn parse_decimal_char_ref<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Reference<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("&#"));
        let (dec, xml) = try_parse!(xml.consume_decimal_chars());
        let (_, xml) = try_parse!(xml.consume_literal(";"));

        Success((DecimalCharReference(dec), xml))
    }

    fn parse_hex_char_ref<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Reference<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("&#x"));
        let (hex, xml) = try_parse!(xml.consume_hex_chars());
        let (_, xml) = try_parse!(xml.consume_literal(";"));

        Success((HexCharReference(hex), xml))
    }

    fn parse_reference<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Reference<'a>> {
        parse_alternate!(xml, {
            [|xml| self.parse_entity_ref(xml)       -> |e| e],
            [|xml| self.parse_decimal_char_ref(xml) -> |d| d],
            [|xml| self.parse_hex_char_ref(xml)     -> |h| h],
        })
    }

    fn parse_comment<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Comment<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("<!--"));
        let (text, xml) = try_parse!(xml.consume_comment());
        let (_, xml) = try_parse!(xml.consume_literal("-->"));

        Success((Comment{text: text}, xml))
    }

    fn parse_pi_value<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, &'a str> {
        let (_, xml) = try_parse!(xml.consume_space());
        xml.consume_pi_value()
    }

    fn parse_pi<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, ProcessingInstruction<'a>> {
        let (_, xml) = try_parse!(xml.consume_literal("<?"));
        let (target, xml) = try_parse!(xml.consume_name());
        let (value, xml) = parse_optional!(self.parse_pi_value(xml), xml);
        let (_, xml) = try_parse!(xml.consume_literal("?>"));

        if target.eq_ignore_ascii_case("xml") {
            panic!("Can't use xml as a PI target");
        }

        Success((ProcessingInstruction{target: target, value: value}, xml))
    }

    fn parse_content<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Vec<Child<'a>>> {
        let mut children = Vec::new();

        let (char_data, xml) = parse_optional!(self.parse_char_data(xml), xml);
        char_data.map(|c| children.push(TextChild(c)));

        // Pattern: zero-or-more
        let mut start = xml;
        loop {
            let xxx = parse_alternate!(start, {
                [|xml| self.parse_element(xml)   -> |e| ElementChild(e)],
                [|xml| self.parse_cdata(xml)     -> |t| TextChild(t)],
                [|xml| self.parse_reference(xml) -> |r| ReferenceChild(r)],
                [|xml| self.parse_comment(xml)   -> |c| CommentChild(c)],
                [|xml| self.parse_pi(xml)        -> |p| PIChild(p)],
            });

            let (child, after) = match xxx {
                Success(x) => x,
                Partial((_, pf, _)) |
                Failure(pf) => return Partial((children, pf, start)),
            };

            let (char_data, xml) = parse_optional!(self.parse_char_data(after), after);

            children.push(child);
            char_data.map(|c| children.push(TextChild(c)));

            start = xml;
        }
    }

    fn parse_empty_element_tail<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Vec<Child<'a>>> {
        let (_, xml) = try_parse!(xml.consume_literal("/>"));

        Success((Vec::new(), xml))
    }

    fn parse_non_empty_element_tail<'a>(&self, xml: StartPoint<'a>, start_name: &str) -> ParseResult<'a, Vec<Child<'a>>> {
        let (_, xml) = try_parse!(xml.consume_literal(">"));

        let (children, f, xml) = try_partial_parse!(self.parse_content(xml));

        let (end_name, xml) = try_resume_after_partial_failure!(f, self.parse_element_end(xml));

        if start_name != end_name {
            panic!("tags do not match!");
        }

        Success((children, xml))
    }

    fn parse_element<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Element<'a>> {
        let (_, xml) = try_parse!(xml.consume_start_tag());
        let (name, xml) = try_parse!(xml.consume_name());

        let (attrs, f, xml) = try_partial_parse!(self.parse_attributes(xml));

        let (_, xml) = parse_optional!(xml.consume_space(), xml);

        let (children, xml) = try_resume_after_partial_failure!(f,
            parse_alternate!(xml, {
                [|xml| self.parse_empty_element_tail(xml)           -> |e| e],
                [|xml| self.parse_non_empty_element_tail(xml, name) -> |e| e],
            })
        );

        Success((Element{name: name, attributes: attrs, children: children}, xml))
    }

    fn parse_document<'a>(&self, xml: StartPoint<'a>) -> ParseResult<'a, Document<'a>> {
        let (before_children, f, xml) = try_partial_parse!(self.parse_prolog(xml));
        let (element, xml) = try_resume_after_partial_failure!(f, self.parse_element(xml));
        let (after_children, xml) = parse_optional!(self.parse_miscs(xml), xml);

        Success((Document{before_children: before_children,
                          element: element,
                          after_children: after_children.unwrap_or(Vec::new())}, xml))
    }

    pub fn parse(&self, xml: &str) -> Result<Package, uint> {
        let xml = StartPoint{offset: 0, s: xml};

        let (document, _) = match self.parse_document(xml) {
            Success(x) => x,
            Partial((_, pf, _)) |
            Failure(pf) => return Err(pf.point.offset),
        };

        // TODO: Check fully parsed

        let h = Hydrator;

        Ok(h.hydrate_document(document))
    }
}

struct Hydrator;

impl Hydrator {
    fn hydrate_text<'d>(&self, doc: &'d dom4::Document<'d>, text_data: Text) -> dom4::Text<'d> {
        doc.create_text(text_data.text)
    }

    fn hydrate_reference_raw<'d>(&self, ref_data: Reference) -> String {
        match ref_data {
            DecimalCharReference(d) => {
                let code: u32 = from_str_radix(d, 10).expect("Not valid decimal");
                let c: char = from_u32(code).expect("Not a valid codepoint");
                String::from_char(1, c)
            },
            HexCharReference(h) => {
                let code: u32 = from_str_radix(h, 16).expect("Not valid hex");
                let c: char = from_u32(code).expect("Not a valid codepoint");
                String::from_char(1, c)
            },
            EntityReference(e) => {
                String::from_str(match e {
                    "amp"  => "&",
                    "lt"   => "<",
                    "gt"   => ">",
                    "apos" => "'",
                    "quot" => "\"",
                    _      => panic!("unknown entity"),
                })
            }
        }
    }

    fn hydrate_reference<'d>(&self, doc: &'d dom4::Document<'d>, ref_data: Reference) -> dom4::Text<'d> {
        doc.create_text(self.hydrate_reference_raw(ref_data).as_slice())
    }

    fn hydrate_comment<'d>(&self, doc: &'d dom4::Document<'d>, comment_data: Comment) -> dom4::Comment<'d> {
        doc.create_comment(comment_data.text)
    }

    fn hydrate_pi<'d>(&self, doc: &'d dom4::Document<'d>, pi_data: ProcessingInstruction) -> dom4::ProcessingInstruction<'d> {
        doc.create_processing_instruction(pi_data.target, pi_data.value)
    }

    fn hydrate_element<'d>(&self, doc: &'d dom4::Document<'d>, element_data: Element) -> dom4::Element<'d> {
        let element = doc.create_element(element_data.name);

        for attr in element_data.attributes.into_iter() {
            let to_v_str = |v: AttributeValue| match v {
                LiteralAttributeValue(v) => String::from_str(v),
                ReferenceAttributeValue(r) => self.hydrate_reference_raw(r),
            };

            let mut v = String::new();
            for val in attr.values.into_iter() {
                v.push_str(to_v_str(val).as_slice());
            }

            element.set_attribute_value(attr.name, v.as_slice());
        }

        for child in element_data.children.into_iter() {
            match child {
                ElementChild(e)   => element.append_child(self.hydrate_element(doc, e)),
                TextChild(t)      => element.append_child(self.hydrate_text(doc, t)),
                ReferenceChild(r) => element.append_child(self.hydrate_reference(doc, r)),
                CommentChild(c)   => element.append_child(self.hydrate_comment(doc, c)),
                PIChild(pi)       => element.append_child(self.hydrate_pi(doc, pi)),
            }
        }

        element
    }

    fn hydrate_misc<'d>(&self, doc: &'d dom4::Document<'d>, children: Vec<RootChild>) {
        for child in children.into_iter() {
            match child {
                CommentRootChild(c) => doc.root().append_child(self.hydrate_comment(doc, c)),
                PIRootChild(p)      => doc.root().append_child(self.hydrate_pi(doc, p)),
                IgnoredRootChild    => {},
            }
        }
    }

    pub fn hydrate_document(&self, document: Document) -> Package {
        let package = Package::new();

        {
            let doc = package.as_document();
            let root = doc.root();

            self.hydrate_misc(&doc, document.before_children);

            root.append_child(self.hydrate_element(&doc, document.element));

            self.hydrate_misc(&doc, document.after_children);
        }

        package
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use super::super::{Package,Document,Element};
    use super::super::dom4;

    macro_rules! assert_str_eq(
        ($l:expr, $r:expr) => (assert_eq!($l.as_slice(), $r.as_slice()));
    )

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

        assert_str_eq!(top.name(), "hello");
    }

    #[test]
    fn a_document_with_a_prolog_with_double_quotes() {
        let package = quick_parse("<?xml version=\"1.0\" ?><hello />");
        let doc = package.as_document();
        let top = top(&doc);

        assert_str_eq!(top.name(), "hello");
    }

    #[test]
    fn a_document_with_a_single_element() {
        let package = quick_parse("<hello />");
        let doc = package.as_document();
        let top = top(&doc);

        assert_str_eq!(top.name(), "hello");
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

        assert_str_eq!(top.name(), "hello");
    }

    #[test]
    fn nested_elements() {
        let package = quick_parse("<hello><world/></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let world = hello.children()[0].element().unwrap();

        assert_str_eq!(world.name(), "world");
    }

    #[test]
    fn multiply_nested_elements() {
        let package = quick_parse("<hello><awesome><world/></awesome></hello>");
        let doc = package.as_document();
        let hello = top(&doc);
        let awesome = hello.children()[0].element().unwrap();
        let world = awesome.children()[0].element().unwrap();

        assert_str_eq!(world.name(), "world");
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
        assert_str_eq!(element.name(), "a");
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
