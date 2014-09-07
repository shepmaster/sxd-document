use std::ascii::AsciiExt;
use std::char::from_u32;
use std::num::from_str_radix;

use self::xmlstr::XmlStr;

mod xmlstr;

pub struct Parser;

struct Element<'a> {
    name: &'a str,
    attributes: Vec<Attribute<'a>>,
    children: Vec<Child<'a>>,
}

enum AttributeValue<'a> {
    ReferenceAttributeValue(Reference<'a>),
    LiteralAttributeValue(&'a str),
}

struct Attribute<'a> {
    name: &'a str,
    values: Vec<AttributeValue<'a>>,
}

struct Text<'a> {
    text: &'a str,
}

enum Reference<'a> {
    EntityReference(&'a str),
    DecimalCharReference(&'a str),
    HexCharReference(&'a str),
}

struct Comment<'a> {
    text: &'a str,
}

struct ProcessingInstruction<'a> {
    target: &'a str,
    value: Option<&'a str>,
}

enum RootChild<'a> {
    CommentRootChild(Comment<'a>),
    PIRootChild(ProcessingInstruction<'a>),
    IgnoredRootChild,
}

enum Child<'a> {
    ElementChild(Element<'a>),
    TextChild(Text<'a>),
    ReferenceChild(Reference<'a>),
    CommentChild(Comment<'a>),
    PIChild(ProcessingInstruction<'a>),
}

macro_rules! try_parse(
    ($e:expr) => ({
        match $e {
            None => return None,
            Some(x) => x,
        }
    })
)

// Pattern: 0-or-1
macro_rules! optional_parse(
    ($parser:expr, $start:expr) => ({
        match $parser {
            None => (None, $start),
            Some((value, next)) => (Some(value), next),
        }
    })
)

// Pattern: alternate
macro_rules! alternate_parse(
    ($start:expr, {}) => ( None );
    ($start:expr, {
        [$parser:expr -> $transformer:expr],
        $([$parser_rest:expr -> $transformer_rest:expr],)*
    }) => (
        match $parser($start) {
            Some((val, next)) => Some(($transformer(val), next)),
            None => alternate_parse!($start, {$([$parser_rest -> $transformer_rest],)*}),
        }
    );
)

// Pattern: zero-or-more
macro_rules! parse_zero_or_more(
    ($start:expr, $parser:expr) => {{
        let mut items = Vec::new();

        let mut start = $start;
        loop {
            let (item, next_start) = match $parser(start) {
                Some(x) => x,
                None => break,
            };

            items.push(item);
            start = next_start;
        }

        Some((items, start))
    }};
)

type ParseResult<'a, T> = Option<(T, &'a str)>;

impl Parser {
    pub fn new() -> Parser {
        Parser
    }

    fn parse_eq<'a>(&self, xml: &'a str) -> ParseResult<'a, ()> {
        let (_, xml) = optional_parse!(xml.slice_space(), xml);
        let (_, xml) = try_parse!(xml.slice_literal("="));
        let (_, xml) = optional_parse!(xml.slice_space(), xml);

        Some(((), xml))
    }

    fn parse_version_info<'a>(&self, xml: &'a str) -> ParseResult<'a, &'a str> {
        let (_, xml) = try_parse!(xml.slice_space());
        let (_, xml) = try_parse!(xml.slice_literal("version"));
        let (_, xml) = try_parse!(self.parse_eq(xml));
        let (version, xml) = try_parse!(
            self.parse_quoted_value(xml, |xml, _| xml.slice_version_num())
        );

        Some((version, xml))
    }

    fn parse_xml_declaration<'a>(&self, xml: &'a str) -> ParseResult<'a, ()> {
        let (_, xml) = try_parse!(xml.slice_literal("<?xml"));
        let (_version, xml) = try_parse!(self.parse_version_info(xml));
        // let (encoding, xml) = optional_parse!(self.parse_encoding_declaration(xml));
        // let (standalone, xml) = optional_parse!(self.parse_standalone_declaration(xml));
        let (_, xml) = optional_parse!(xml.slice_space(), xml);
        let (_, xml) = try_parse!(xml.slice_literal("?>"));

        Some(((), xml))
    }

    fn parse_space<'a>(&self, xml: &'a str) -> ParseResult<'a, &'a str> {
        xml.slice_space()
    }

    fn parse_misc<'a>(&self, xml: &'a str) -> ParseResult<'a, RootChild<'a>> {
        alternate_parse!(xml, {
            [|xml| self.parse_comment(xml) -> |c| CommentRootChild(c)],
            [|xml| self.parse_pi(xml)      -> |p| PIRootChild(p)],
            [|xml| self.parse_space(xml)   -> |_| IgnoredRootChild],
        })
    }

    fn parse_miscs<'a>(&self, xml: &'a str) -> ParseResult<'a, Vec<RootChild<'a>>> {
        parse_zero_or_more!(xml, |xml| self.parse_misc(xml))
    }

    fn parse_prolog<'a>(&self, xml: &'a str) -> ParseResult<'a, Vec<RootChild<'a>>> {
        let (_, xml) = optional_parse!(self.parse_xml_declaration(xml), xml);
        self.parse_miscs(xml)
    }

    fn parse_one_quoted_value<'a, T>(&self,
                                     xml: &'a str,
                                     quote: &str,
                                     f: |&'a str| -> ParseResult<'a, T>)
                                     -> ParseResult<'a, T>
    {
        let (_, xml) = try_parse!(xml.slice_literal(quote));
        let (value, xml) = try_parse!(f(xml));
        let (_, xml) = try_parse!(xml.slice_literal(quote));

        Some((value, xml))
    }

    fn parse_quoted_value<'a, T>(&self,
                                 xml: &'a str,
                                 f: |&'a str, &str| -> ParseResult<'a, T>)
                                 -> ParseResult<'a, T>
    {
        alternate_parse!(xml, {
            [|xml| self.parse_one_quoted_value(xml, "'",  |xml| f(xml, "'"))  -> |v| v],
            [|xml| self.parse_one_quoted_value(xml, "\"", |xml| f(xml, "\"")) -> |v| v],
        })
    }

    fn parse_attribute_values<'a>(&self, xml: &'a str, quote: &str)
                                  -> ParseResult<'a, Vec<AttributeValue<'a>>>
    {
        parse_zero_or_more!(xml, |xml|
            alternate_parse!(xml, {
                [|xml: &'a str| xml.slice_attribute(quote) -> |v| LiteralAttributeValue(v)],
                [|xml: &'a str| self.parse_reference(xml)  -> |e| ReferenceAttributeValue(e)],
            }))
    }

    fn parse_attribute<'a>(&self, xml: &'a str) -> ParseResult<'a, Attribute<'a>> {
        let (_, xml) = try_parse!(xml.slice_space());

        let (name, xml) = try_parse!(xml.slice_name());

        let (_, xml) = try_parse!(self.parse_eq(xml));

        let (values, xml) = try_parse!(
            self.parse_quoted_value(xml, |xml, quote| self.parse_attribute_values(xml, quote))
        );

        Some((Attribute{name: name, values: values}, xml))
    }

    fn parse_attributes<'a>(&self, xml: &'a str) -> ParseResult<'a, Vec<Attribute<'a>>> {
        parse_zero_or_more!(xml, |xml| self.parse_attribute(xml))
    }

    fn parse_empty_element<'a>(&self, xml: &'a str) -> ParseResult<'a, Element<'a>> {
        let (_, xml) = try_parse!(xml.slice_literal("<"));
        let (name, xml) = try_parse!(xml.slice_name());
        let (attrs, xml) = try_parse!(self.parse_attributes(xml));
        let (_, xml) = optional_parse!(xml.slice_space(), xml);
        let (_, xml) = try_parse!(xml.slice_literal("/>"));

        Some((Element{name: name, attributes: attrs, children: Vec::new()}, xml))
    }

    fn parse_element_start<'a>(&self, xml: &'a str) -> ParseResult<'a, Element<'a>> {
        let (_, xml) = try_parse!(xml.slice_literal("<"));
        let (name, xml) = try_parse!(xml.slice_name());
        let (attrs, xml) = try_parse!(self.parse_attributes(xml));
        let (_, xml) = optional_parse!(xml.slice_space(), xml);
        let (_, xml) = try_parse!(xml.slice_literal(">"));

        Some((Element{name: name, attributes: attrs, children: Vec::new()}, xml))
    }

    fn parse_element_end<'a>(&self, xml: &'a str) -> ParseResult<'a, &'a str> {
        let (_, xml) = try_parse!(xml.slice_literal("</"));
        let (name, xml) = try_parse!(xml.slice_name());
        let (_, xml) = optional_parse!(xml.slice_space(), xml);
        let (_, xml) = try_parse!(xml.slice_literal(">"));
        Some((name, xml))
    }

    fn parse_char_data<'a>(&self, xml: &'a str) -> ParseResult<'a, Text<'a>> {
        let (text, xml) = try_parse!(xml.slice_char_data());

        Some((Text{text: text}, xml))
    }

    fn parse_cdata<'a>(&self, xml: &'a str) -> ParseResult<'a, Text<'a>> {
        let (_, xml) = try_parse!(xml.slice_literal("<![CDATA["));
        let (text, xml) = try_parse!(xml.slice_cdata());
        let (_, xml) = try_parse!(xml.slice_literal("]]>"));

        Some((Text{text: text}, xml))
    }

    fn parse_entity_ref<'a>(&self, xml: &'a str) -> ParseResult<'a, Reference<'a>> {
        let (_, xml) = try_parse!(xml.slice_literal("&"));
        let (name, xml) = try_parse!(xml.slice_name());
        let (_, xml) = try_parse!(xml.slice_literal(";"));

        Some((EntityReference(name), xml))
    }

    fn parse_decimal_char_ref<'a>(&self, xml: &'a str) -> ParseResult<'a, Reference<'a>> {
        let (_, xml) = try_parse!(xml.slice_literal("&#"));
        let (dec, xml) = try_parse!(xml.slice_decimal_chars());
        let (_, xml) = try_parse!(xml.slice_literal(";"));

        Some((DecimalCharReference(dec), xml))
    }

    fn parse_hex_char_ref<'a>(&self, xml: &'a str) -> ParseResult<'a, Reference<'a>> {
        let (_, xml) = try_parse!(xml.slice_literal("&#x"));
        let (hex, xml) = try_parse!(xml.slice_hex_chars());
        let (_, xml) = try_parse!(xml.slice_literal(";"));

        Some((HexCharReference(hex), xml))
    }

    fn parse_reference<'a>(&self, xml: &'a str) -> ParseResult<'a, Reference<'a>> {
        alternate_parse!(xml, {
            [|xml| self.parse_entity_ref(xml)       -> |e| e],
            [|xml| self.parse_decimal_char_ref(xml) -> |d| d],
            [|xml| self.parse_hex_char_ref(xml)     -> |h| h],
        })
    }

    fn parse_comment<'a>(&self, xml: &'a str) -> ParseResult<'a, Comment<'a>> {
        let (_, xml) = try_parse!(xml.slice_literal("<!--"));
        let (text, xml) = try_parse!(xml.slice_comment());
        let (_, xml) = try_parse!(xml.slice_literal("-->"));

        Some((Comment{text: text}, xml))
    }

    fn parse_pi_value<'a>(&self, xml: &'a str) -> ParseResult<'a, &'a str> {
        let (_, xml) = try_parse!(xml.slice_space());
        xml.slice_pi_value()
    }

    fn parse_pi<'a>(&self, xml: &'a str) -> ParseResult<'a, ProcessingInstruction<'a>> {
        let (_, xml) = try_parse!(xml.slice_literal("<?"));
        let (target, xml) = try_parse!(xml.slice_name());
        let (value, xml) = optional_parse!(self.parse_pi_value(xml), xml);
        let (_, xml) = try_parse!(xml.slice_literal("?>"));

        if target.eq_ignore_ascii_case("xml") {
            fail!("Can't use xml as a PI target");
        }

        Some((ProcessingInstruction{target: target, value: value}, xml))
    }

    fn parse_content<'a>(&self, xml: &'a str) -> (Vec<Child<'a>>, &'a str) {
        let mut children = Vec::new();

        let (char_data, xml) = optional_parse!(self.parse_char_data(xml), xml);
        char_data.map(|c| children.push(TextChild(c)));

        // Pattern: zero-or-more
        let mut start = xml;
        loop {
            let xxx = alternate_parse!(start, {
                [|xml| self.parse_element(xml)   -> |e| ElementChild(e)],
                [|xml| self.parse_cdata(xml)     -> |t| TextChild(t)],
                [|xml| self.parse_reference(xml) -> |r| ReferenceChild(r)],
                [|xml| self.parse_comment(xml)   -> |c| CommentChild(c)],
                [|xml| self.parse_pi(xml)        -> |p| PIChild(p)],
            });

            let (child, after) = match xxx {
                Some(x) => x,
                None => return (children, start),
            };

            let (char_data, xml) = optional_parse!(self.parse_char_data(after), after);

            children.push(child);
            char_data.map(|c| children.push(TextChild(c)));

            start = xml;
        }
    }

    fn parse_non_empty_element<'a>(&self, xml: &'a str) -> ParseResult<'a, Element<'a>> {
        let (mut element, xml) = try_parse!(self.parse_element_start(xml));
        let (children, xml) = self.parse_content(xml);
        let (name, xml) = try_parse!(self.parse_element_end(xml));

        if element.name != name {
            fail!("tags do not match!");
        }

        element.children = children;

        Some((element, xml))
    }

    fn parse_element<'a>(&self, xml: &'a str) -> ParseResult<'a, Element<'a>> {
        alternate_parse!(xml, {
            [|xml| self.parse_empty_element(xml)     -> |e| e],
            [|xml| self.parse_non_empty_element(xml) -> |e| e],
        })
    }

    fn hydrate_text(&self, doc: &super::Document, text_data: Text) -> super::Text {
        doc.new_text(text_data.text.to_string())
    }

    fn hydrate_reference_raw(&self, ref_data: Reference) -> String {
        match ref_data {
            DecimalCharReference(d) => {
                let code: u32 = from_str_radix(d, 10).expect("Not valid decimal");
                let c: char = from_u32(code).expect("Not a valid codepoint");
                c.to_string()
            },
            HexCharReference(h) => {
                let code: u32 = from_str_radix(h, 16).expect("Not valid hex");
                let c: char = from_u32(code).expect("Not a valid codepoint");
                c.to_string()
            },
            EntityReference(e) => {
                match e {
                    "amp"  => "&",
                    "lt"   => "<",
                    "gt"   => ">",
                    "apos" => "'",
                    "quot" => "\"",
                    _      => fail!("unknown entity"),
                }.to_string()
            }
        }
    }

    fn hydrate_reference(&self, doc: &super::Document, ref_data: Reference) -> super::Text {
        doc.new_text(self.hydrate_reference_raw(ref_data))
    }

    fn hydrate_comment(&self, doc: &super::Document, comment_data: Comment) -> super::Comment {
        doc.new_comment(comment_data.text.to_string())
    }

    fn hydrate_pi(&self, doc: &super::Document, pi_data: ProcessingInstruction) -> super::ProcessingInstruction {
        doc.new_processing_instruction(pi_data.target.to_string(), pi_data.value.map(|v| v.to_string()))
    }

    fn hydrate_element(&self, doc: &super::Document, element_data: Element) -> super::Element {
        let element = doc.new_element(element_data.name.to_string());

        for attr in element_data.attributes.move_iter() {
            let to_v_str = |v: AttributeValue| match v {
                LiteralAttributeValue(v) => v.to_string(),
                ReferenceAttributeValue(r) => self.hydrate_reference_raw(r),
            };

            let v = attr.values.move_iter().fold(String::new(), |s, v| s.append(to_v_str(v).as_slice()));
            element.set_attribute(attr.name.to_string(), v);
        }

        for child in element_data.children.move_iter() {
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

    fn hydrate_misc(&self, doc: &super::Document, children: Vec<RootChild>) {
        for child in children.move_iter() {
            match child {
                CommentRootChild(c) => doc.root().append_child(self.hydrate_comment(doc, c)),
                PIRootChild(p)      => doc.root().append_child(self.hydrate_pi(doc, p)),
                IgnoredRootChild    => {},
            }
        }
    }

    fn hydrate_document(&self,
                        before_children: Vec<RootChild>,
                        element_data: Element,
                        after_children: Vec<RootChild>)
                        -> super::Document
    {
        let doc = super::Document::new();
        let root = doc.root();

        self.hydrate_misc(&doc, before_children);

        root.append_child(self.hydrate_element(&doc, element_data));

        self.hydrate_misc(&doc, after_children);

        doc
    }

    pub fn parse(&self, xml: &str) -> super::Document {
        let (before_children, xml) = optional_parse!(self.parse_prolog(xml), xml);
        let (element, xml) = self.parse_element(xml).expect("no element");
        let (after_children, _xml) = optional_parse!(self.parse_miscs(xml), xml);

        self.hydrate_document(before_children.unwrap_or(Vec::new()),
                              element,
                              after_children.unwrap_or(Vec::new()))
    }
}

#[cfg(test)]
mod test {

use super::Parser;

#[test]
fn a_document_with_a_prolog() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello />");
    let top = doc.root().children()[0].element().unwrap();

    assert_eq!(top.name().as_slice(), "hello");
}

#[test]
fn a_document_with_a_prolog_with_double_quotes() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version=\"1.0\" ?><hello />");
    let top = doc.root().children()[0].element().unwrap();

    assert_eq!(top.name().as_slice(), "hello");
}

#[test]
fn a_document_with_a_single_element() {
    let parser = Parser::new();
    let doc = parser.parse("<hello />");
    let top = doc.root().children()[0].element().unwrap();

    assert_eq!(top.name().as_slice(), "hello");
}

#[test]
fn an_element_with_an_attribute() {
    let parser = Parser::new();
    let doc = parser.parse("<hello scope='world'/>");
    let top = doc.root().children()[0].element().unwrap();

    assert_eq!(top.get_attribute("scope").unwrap().as_slice(), "world");
}

#[test]
fn an_element_with_an_attribute_using_double_quotes() {
    let parser = Parser::new();
    let doc = parser.parse("<hello scope=\"world\"/>");
    let top = doc.root().children()[0].element().unwrap();

    assert_eq!(top.get_attribute("scope").unwrap().as_slice(), "world");
}

#[test]
fn an_element_with_multiple_attributes() {
    let parser = Parser::new();
    let doc = parser.parse("<hello scope=\"world\" happy='true'/>");
    let top = doc.root().children()[0].element().unwrap();

    assert_eq!(top.get_attribute("scope").unwrap().as_slice(), "world");
    assert_eq!(top.get_attribute("happy").unwrap().as_slice(), "true");
}

#[test]
fn an_attribute_with_references() {
    let parser = Parser::new();
    let doc = parser.parse("<log msg='I &lt;3 math' />");
    let top = doc.root().children()[0].element().unwrap();

    assert_eq!(top.get_attribute("msg").unwrap().as_slice(), "I <3 math");
}

#[test]
fn an_element_that_is_not_self_closing() {
    let parser = Parser::new();
    let doc = parser.parse("<hello></hello>");
    let top = doc.root().children()[0].element().unwrap();

    assert_eq!(top.name().as_slice(), "hello");
}

#[test]
fn nested_elements() {
    let parser = Parser::new();
    let doc = parser.parse("<hello><world/></hello>");
    let nested = doc.root().children()[0].element().unwrap().children()[0].element().unwrap();

    assert_eq!(nested.name().as_slice(), "world");
}

#[test]
fn multiply_nested_elements() {
    let parser = Parser::new();
    let doc = parser.parse("<hello><awesome><world/></awesome></hello>");
    let hello = doc.root().children()[0].element().unwrap();
    let awesome = hello.children()[0].element().unwrap();
    let world = awesome.children()[0].element().unwrap();

    assert_eq!(world.name().as_slice(), "world");
}

#[test]
fn nested_elements_with_attributes() {
    let parser = Parser::new();
    let doc = parser.parse("<hello><world name='Earth'/></hello>");
    let hello = doc.root().children()[0].element().unwrap();
    let world = hello.children()[0].element().unwrap();

    assert_eq!(world.get_attribute("name").unwrap().as_slice(), "Earth");
}

#[test]
fn element_with_text() {
    let parser = Parser::new();
    let doc = parser.parse("<hello>world</hello>");
    let hello = doc.root().children()[0].element().unwrap();
    let text = hello.children()[0].text().unwrap();

    assert_eq!(text.text().as_slice(), "world");
}

#[test]
fn element_with_cdata() {
    let parser = Parser::new();
    let doc = parser.parse("<words><![CDATA[I have & and < !]]></words>");
    let words = doc.root().children()[0].element().unwrap();
    let text = words.children()[0].text().unwrap();

    assert_eq!(text.text().as_slice(), "I have & and < !");
}

#[test]
fn element_with_comment() {
    let parser = Parser::new();
    let doc = parser.parse("<hello><!-- A comment --></hello>");
    let words = doc.root().children()[0].element().unwrap();
    let comment = words.children()[0].comment().unwrap();

    assert_eq!(comment.text().as_slice(), " A comment ");
}

#[test]
fn comment_before_top_element() {
    let parser = Parser::new();
    let doc = parser.parse("<!-- A comment --><hello />");
    let comment = doc.root().children()[0].comment().unwrap();

    assert_eq!(comment.text().as_slice(), " A comment ");
}

#[test]
fn multiple_comments_before_top_element() {
    let parser = Parser::new();
    let xml = r"
<!--Comment 1-->
<!--Comment 2-->
<hello />";
    let doc = parser.parse(xml);
    let comment1 = doc.root().children()[0].comment().unwrap();
    let comment2 = doc.root().children()[1].comment().unwrap();

    assert_eq!(comment1.text().as_slice(), "Comment 1");
    assert_eq!(comment2.text().as_slice(), "Comment 2");
}

#[test]
fn multiple_comments_after_top_element() {
    let parser = Parser::new();
    let xml = r"
<hello />
<!--Comment 1-->
<!--Comment 2-->";
    let doc = parser.parse(xml);
    let comment1 = doc.root().children()[1].comment().unwrap();
    let comment2 = doc.root().children()[2].comment().unwrap();

    assert_eq!(comment1.text().as_slice(), "Comment 1");
    assert_eq!(comment2.text().as_slice(), "Comment 2");
}

#[test]
fn element_with_processing_instruction() {
    let parser = Parser::new();
    let doc = parser.parse("<hello><?device?></hello>");
    let hello = doc.root().children()[0].element().unwrap();
    let pi = hello.children()[0].processing_instruction().unwrap();

    assert_eq!(pi.target().as_slice(), "device");
    assert_eq!(pi.value(), None);
}

#[test]
fn top_level_processing_instructions() {
    let parser = Parser::new();
    let xml = r"
<?output printer?>
<hello />
<?validated?>";

    let doc = parser.parse(xml);
    let pi1 = doc.root().children()[0].processing_instruction().unwrap();
    let pi2 = doc.root().children()[2].processing_instruction().unwrap();

    assert_eq!(pi1.target().as_slice(), "output");
    assert_eq!(pi1.value().unwrap().as_slice(), "printer");

    assert_eq!(pi2.target().as_slice(), "validated");
    assert_eq!(pi2.value(), None);
}

#[test]
fn element_with_decimal_char_reference() {
    let parser = Parser::new();
    let doc = parser.parse("<math>2 &#62; 1</math>");
    let math = doc.root().children()[0].element().unwrap();
    let text1 = math.children()[0].text().unwrap();
    let text2 = math.children()[1].text().unwrap();
    let text3 = math.children()[2].text().unwrap();

    assert_eq!(text1.text().as_slice(), "2 ");
    assert_eq!(text2.text().as_slice(), ">");
    assert_eq!(text3.text().as_slice(), " 1");
}

#[test]
fn element_with_hexidecimal_char_reference() {
    let parser = Parser::new();
    let doc = parser.parse("<math>1 &#x3c; 2</math>");
    let math = doc.root().children()[0].element().unwrap();
    let text1 = math.children()[0].text().unwrap();
    let text2 = math.children()[1].text().unwrap();
    let text3 = math.children()[2].text().unwrap();

    assert_eq!(text1.text().as_slice(), "1 ");
    assert_eq!(text2.text().as_slice(), "<");
    assert_eq!(text3.text().as_slice(), " 2");
}

#[test]
fn element_with_entity_reference() {
    let parser = Parser::new();
    let doc = parser.parse("<math>I &lt;3 math</math>");
    let math = doc.root().children()[0].element().unwrap();
    let text1 = math.children()[0].text().unwrap();
    let text2 = math.children()[1].text().unwrap();
    let text3 = math.children()[2].text().unwrap();

    assert_eq!(text1.text().as_slice(), "I ");
    assert_eq!(text2.text().as_slice(), "<");
    assert_eq!(text3.text().as_slice(), "3 math");
}

#[test]
fn element_with_mixed_children() {
    let parser = Parser::new();
    let doc = parser.parse("<hello>to <a>the</a> world</hello>");
    let hello = doc.root().children()[0].element().unwrap();
    let text1 = hello.children()[0].text().unwrap();
    let middle = hello.children()[1].element().unwrap();
    let text2 = hello.children()[2].text().unwrap();

    assert_eq!(text1.text().as_slice(), "to ");
    assert_eq!(middle.name().as_slice(), "a");
    assert_eq!(text2.text().as_slice(), " world");
}

}
