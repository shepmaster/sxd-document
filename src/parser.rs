use super::{Document,Root,RootChild,Element,ElementChild,Text};

#[allow(dead_code)]
struct Parser;

#[allow(dead_code)]
struct ParsedElement<'a> {
    name: &'a str,
    attributes: Vec<ParsedAttribute<'a>>,
    children: Vec<ParsedChild<'a>>,
}

#[allow(dead_code)]
struct ParsedAttribute<'a> {
    name: &'a str,
    value: &'a str,
}

#[allow(dead_code)]
struct ParsedText<'a> {
    text: &'a str,
}

#[allow(dead_code)]
enum ParsedChild<'a> {
    ElementParsedChild(ParsedElement<'a>),
    TextParsedChild(ParsedText<'a>),
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
    ($f:expr, $start:expr) => ({
        match $f {
            None => (None, $start),
            Some((value, next)) => (Some(value), next),
        }
    })
)

#[allow(dead_code)]
impl Parser {
    fn new() -> Parser {
        Parser
    }

    fn parse_preamble<'a>(&self, xml: &'a str) -> &'a str {
        // Parse the preamble
        let idx = xml.find_str("?>").expect("No preamble end");
        let end_of_preamble = idx + "?>".len();
        xml.slice_from(end_of_preamble)
    }

    fn optional_space<'a>(&self, xml: &'a str) -> &'a str {
        match xml.slice_space() {
            Some((_, next_xml)) => next_xml,
            None => xml,
        }
    }

    fn parse_attribute_value_quote<'a>(&self, xml: &'a str, quote: &str) -> Option<(&'a str, &'a str)> {
        let (_, xml) = match xml.slice_literal(quote) {
            None => return None,
            Some(x) => x,
        };
        // TODO: don't consume & or <
        // TODO: support references
        let (value, xml) = xml.slice_until(quote).expect("No value");
        let (_, xml) = xml.slice_literal(quote).expect("No quote");

        Some((value, xml))
    }

    fn parse_attribute<'a>(&self, xml: &'a str) -> Option<(ParsedAttribute<'a>, &'a str)> {
        let (name, xml) = match xml.slice_name() {
            Some(x) => x,
            None => return None,
        };

        let xml = self.optional_space(xml);
        let (_, xml) = xml.slice_literal("=").expect("No equal sign");
        let xml = self.optional_space(xml);

        // Pattern: alternate
        let (value, xml) = match self.parse_attribute_value_quote(xml, "'") {
            Some(x) => x,
            None => match self.parse_attribute_value_quote(xml, "\"") {
                Some(x) => x,
                None => fail!("No attribute value"),
            },
        };

        Some((ParsedAttribute{name: name, value: value}, xml))
    }

    fn parse_attributes<'a>(&self, xml: &'a str) -> (Vec<ParsedAttribute<'a>>, &'a str) {
        let mut xml = xml;
        let mut attrs = Vec::new();

        // Pattern: zero-or-more
        // On failure, return the end of the last successful parse
        loop {
            let (_, after_space) = match xml.slice_space() {
                None => return (attrs, xml),
                Some(x) => x,
            };

            xml = match self.parse_attribute(after_space) {
                None => return (attrs, xml),
                Some((attr, after_attr)) => {
                    attrs.push(attr);
                    after_attr
                },
            };
        }
    }

    fn parse_empty_element<'a>(&self, xml: &'a str) -> Option<(ParsedElement<'a>, &'a str)> {
        let (_, xml) = try_parse!(xml.slice_literal("<"));
        let (name, xml) = try_parse!(xml.slice_name());
        let (attrs, xml) = self.parse_attributes(xml);
        let xml = self.optional_space(xml);
        let (_, xml) = try_parse!(xml.slice_literal("/>"));

        Some((ParsedElement{name: name, attributes: attrs, children: Vec::new()}, xml))
    }

    fn parse_element_start<'a>(&self, xml: &'a str) -> Option<(ParsedElement<'a>, &'a str)> {
        let (_, xml) = try_parse!(xml.slice_literal("<"));
        let (name, xml) = try_parse!(xml.slice_name());
        let (attrs, xml) = self.parse_attributes(xml);
        let xml = self.optional_space(xml);
        let (_, xml) = try_parse!(xml.slice_literal(">"));

        Some((ParsedElement{name: name, attributes: attrs, children: Vec::new()}, xml))
    }

    fn parse_element_end<'a>(&self, xml: &'a str) -> Option<(&'a str, &'a str)> {
        let (_, xml) = try_parse!(xml.slice_literal("</"));
        let (name, xml) = try_parse!(xml.slice_name());
        let xml = self.optional_space(xml);
        let (_, xml) = try_parse!(xml.slice_literal(">"));
        Some((name, xml))
    }

    fn parse_char_data<'a>(&self, xml: &'a str) -> Option<(ParsedText<'a>, &'a str)> {
        let (text, xml) = try_parse!(xml.slice_char_data());

        Some((ParsedText{text: text}, xml))
    }

    fn parse_content<'a>(&self, xml: &'a str) -> (Vec<ParsedChild<'a>>, &'a str) {
        let mut children = Vec::new();

        let (char_data, xml) = optional_parse!(self.parse_char_data(xml), xml);
        char_data.map(|c| children.push(TextParsedChild(c)));

        // Pattern: zero-or-more
        let mut start = xml;
        loop {
            let (e, after) = match self.parse_element(start) {
                None => return (children, start),
                Some(x) => x,
            };

            let (char_data, xml) = optional_parse!(self.parse_char_data(after), after);

            children.push(ElementParsedChild(e));
            char_data.map(|c| children.push(TextParsedChild(c)));

            start = xml;
        }
    }

    fn parse_non_empty_element<'a>(&self, xml: &'a str) -> Option<(ParsedElement<'a>, &'a str)> {
        let (mut element, xml) = try_parse!(self.parse_element_start(xml));
        let (children, xml) = self.parse_content(xml);
        let (name, xml) = try_parse!(self.parse_element_end(xml));

        if element.name != name {
            fail!("tags do not match!");
        }

        element.children = children;

        Some((element, xml))
    }

    fn parse_element<'a>(&self, xml: &'a str) -> Option<(ParsedElement<'a>, &'a str)> {
        // Pattern: alternate
        match self.parse_empty_element(xml) {
            Some(x) => Some(x),
            None => match self.parse_non_empty_element(xml) {
                Some(x) => Some(x),
                None => None,
            },
        }
    }

    fn hydrate_text(&self, doc: Document, text_data: ParsedText) -> Text {
        doc.new_text(text_data.text.to_string())
    }

    fn hydrate_element(&self, doc: Document, element_data: ParsedElement) -> Element {
        let element = doc.new_element(element_data.name.to_string());
        for attr in element_data.attributes.iter() {
            element.set_attribute(attr.name.to_string(), attr.value.to_string());
        }
        for child in element_data.children.move_iter() {
            match child {
                ElementParsedChild(e) => element.append_child(self.hydrate_element(doc.clone(), e)),
                TextParsedChild(t) => element.append_child(self.hydrate_text(doc.clone(), t)),
            }
        }
        element
    }

    fn hydrate_parsed_data(&self, element_data: ParsedElement) -> Document {
        let doc = Document::new();

        doc.root().append_child(self.hydrate_element(doc.clone(), element_data));

        doc
    }

    fn parse(&self, xml: &str) -> Document {
        let after_preamble = self.parse_preamble(xml);

        let (element, _tail) = self.parse_element(after_preamble).expect("no element");

        self.hydrate_parsed_data(element)
    }
}

trait XmlStr<'a> {
    fn slice_at(&self, position: uint) -> (&'a str, &'a str);
    fn slice_until(&self, s: &str) -> Option<(&'a str, &'a str)>;
    fn slice_literal(&self, expected: &str) -> Option<(&'a str, &'a str)>;
    fn slice_char_data(&self) -> Option<(&'a str, &'a str)>;
    fn slice_start_rest(&self, is_first: |char| -> bool, is_rest: |char| -> bool) -> Option<(&'a str, &'a str)>;
    fn slice_name(&self) -> Option<(&'a str, &'a str)>;
    fn slice_space(&self) -> Option<(&'a str, &'a str)>;
}

impl<'a> XmlStr<'a> for &'a str {
    fn slice_at(&self, position: uint) -> (&'a str, &'a str) {
        (self.slice_to(position), self.slice_from(position))
    }

    fn slice_until(&self, s: &str) -> Option<(&'a str, &'a str)> {
        match self.find_str(s) {
            Some(position) => Some(self.slice_at(position)),
            None => None
        }
    }

    fn slice_literal(&self, expected: &str) -> Option<(&'a str, &'a str)> {
        if self.starts_with(expected) {
            Some(self.slice_at(expected.len()))
        } else {
            None
        }
    }

    fn slice_char_data(&self) -> Option<(&'a str, &'a str)> {
        if self.starts_with("<") ||
           self.starts_with("&") ||
           self.starts_with("]]>")
        {
            return None
        }

        // Using a hex literal because emacs' rust-mode doesn't
        // understand ] in a char literal. :-(
        let mut positions = self.char_indices().skip_while(|&(_, c)| c != '<' && c != '&' && c != '\x5d');

        loop {
            match positions.next() {
                None => return Some((self.clone(), "")),
                Some((offset, c)) if c == '<' || c == '&' => return Some(self.slice_at(offset)),
                Some((offset, _)) => {
                    let (head, tail) = self.slice_at(offset);
                    if tail.starts_with("]]>") {
                        return Some((head, tail))
                    } else {
                        // False alarm, resume scanning
                        continue;
                    }
                },
            }
        }
    }

    fn slice_start_rest(&self,
                        is_first: |char| -> bool,
                        is_rest: |char| -> bool)
                        -> Option<(&'a str, &'a str)>
    {
        let mut positions = self.char_indices();

        match positions.next() {
            Some((_, c)) if is_first(c) => (),
            Some((_, _)) => return None,
            None => return None,
        };

        let mut positions = positions.skip_while(|&(_, c)| is_rest(c));
        match positions.next() {
            Some((offset, _)) => Some(self.slice_at(offset)),
            None => Some((self.clone(), "")),
        }
    }

    fn slice_name(&self) -> Option<(&'a str, &'a str)> {
        self.slice_start_rest(|c| c.is_name_start_char(), |c| c.is_name_char())
    }

    fn slice_space(&self) -> Option<(&'a str, &'a str)> {
        self.slice_start_rest(|c| c.is_space_char(), |c| c.is_space_char())
    }
}

trait XmlChar {
    fn is_name_start_char(&self) -> bool;
    fn is_name_char(&self) -> bool;
    fn is_space_char(&self) -> bool;
}

impl XmlChar for char {
    fn is_name_start_char(&self) -> bool {
        match *self {
            ':'                        |
            'A'..'Z'                   |
            '_'                        |
            'a'..'z'                   |
            '\U000000C0'..'\U000000D6' |
            '\U000000D8'..'\U000000F6' |
            '\U000000F8'..'\U000002FF' |
            '\U00000370'..'\U0000037D' |
            '\U0000037F'..'\U00001FFF' |
            '\U0000200C'..'\U0000200D' |
            '\U00002070'..'\U0000218F' |
            '\U00002C00'..'\U00002FEF' |
            '\U00003001'..'\U0000D7FF' |
            '\U0000F900'..'\U0000FDCF' |
            '\U0000FDF0'..'\U0000FFFD' |
            '\U00010000'..'\U000EFFFF' => true,
            _ => false,
        }
    }

    fn is_name_char(&self) -> bool {
        if self.is_name_start_char() { return true; }
        match *self {
            '-'                |
            '.'                |
            '0'..'9'           |
            '\u00B7'           |
            '\u0300'..'\u036F' |
            '\u203F'..'\u2040' => true,
            _ => false
        }
    }

    fn is_space_char(&self) -> bool {
        match *self {
            '\x20' |
            '\x09' |
            '\x0D' |
            '\x0A' => true,
            _ => false,
        }
    }
}

trait Hax {
    fn first_child(&self) -> Option<RootChild>;
}

impl Hax for Root {
    fn first_child(&self) -> Option<RootChild> {
        self.children().remove(0)
    }
}

trait Hax2 {
    fn first_child(&self) -> Option<ElementChild>;
}

impl Hax2 for Element {
    fn first_child(&self) -> Option<ElementChild> {
        self.children().remove(0)
    }
}

#[test]
fn parses_a_document_with_a_single_element() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello />");
    let top = doc.root().first_child().unwrap().element().unwrap();

    assert_eq!(top.name().as_slice(), "hello");
}

#[test]
fn parses_an_element_with_an_attribute() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello scope='world'/>");
    let top = doc.root().first_child().unwrap().element().unwrap();

    assert_eq!(top.get_attribute("scope").unwrap().as_slice(), "world");
}

#[test]
fn parses_an_element_with_an_attribute_using_double_quotes() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello scope=\"world\"/>");
    let top = doc.root().first_child().unwrap().element().unwrap();

    assert_eq!(top.get_attribute("scope").unwrap().as_slice(), "world");
}

#[test]
fn parses_an_element_with_multiple_attributes() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello scope=\"world\" happy='true'/>");
    let top = doc.root().first_child().unwrap().element().unwrap();

    assert_eq!(top.get_attribute("scope").unwrap().as_slice(), "world");
    assert_eq!(top.get_attribute("happy").unwrap().as_slice(), "true");
}

#[test]
fn parses_an_element_that_is_not_self_closing() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello></hello>");
    let top = doc.root().first_child().unwrap().element().unwrap();

    assert_eq!(top.name().as_slice(), "hello");
}

#[test]
fn parses_nested_elements() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello><world/></hello>");
    let nested = doc.root().first_child().unwrap().element().unwrap().first_child().unwrap().element().unwrap();

    assert_eq!(nested.name().as_slice(), "world");
}

#[test]
fn parses_multiply_nested_elements() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello><awesome><world/></awesome></hello>");
    let hello = doc.root().first_child().unwrap().element().unwrap();
    let awesome = hello.first_child().unwrap().element().unwrap();
    let world = awesome.first_child().unwrap().element().unwrap();

    assert_eq!(world.name().as_slice(), "world");
}

#[test]
fn parses_nested_elements_with_attributes() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello><world name='Earth'/></hello>");
    let hello = doc.root().first_child().unwrap().element().unwrap();
    let world = hello.first_child().unwrap().element().unwrap();

    assert_eq!(world.get_attribute("name").unwrap().as_slice(), "Earth");
}

#[test]
fn parses_element_with_text() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello>world</hello>");
    let hello = doc.root().first_child().unwrap().element().unwrap();
    let text = hello.first_child().unwrap().text().unwrap();

    assert_eq!(text.text().as_slice(), "world");
}

#[test]
fn parses_element_with_mixed_children() {
    let parser = Parser::new();
    let doc = parser.parse("<?xml version='1.0' ?><hello>to <a>the</a> world</hello>");
    let hello = doc.root().first_child().unwrap().element().unwrap();
    let text1 = hello.children()[0].text().unwrap();
    let middle = hello.children()[1].element().unwrap();
    let text2 = hello.children()[2].text().unwrap();

    assert_eq!(text1.text().as_slice(), "to ");
    assert_eq!(middle.name().as_slice(), "a");
    assert_eq!(text2.text().as_slice(), " world");
}

#[test]
fn slice_char_data_leading_ampersand() {
    assert_eq!("&".slice_char_data(), None);
}

#[test]
fn slice_char_data_leading_less_than() {
    assert_eq!("<".slice_char_data(), None);
}

#[test]
fn slice_char_data_leading_cdata_end() {
    assert_eq!("]]>".slice_char_data(), None);
}

#[test]
fn slice_char_data_until_ampersand() {
    assert_eq!("hello&world".slice_char_data(), Some(("hello", "&world")));
}

#[test]
fn slice_char_data_until_less_than() {
    assert_eq!("hello<world".slice_char_data(), Some(("hello", "<world")));
}

#[test]
fn slice_char_data_until_cdata_end() {
    assert_eq!("hello]]>world".slice_char_data(), Some(("hello", "]]>world")));
}

#[test]
fn slice_char_data_includes_right_square() {
    assert_eq!("hello]world".slice_char_data(), Some(("hello]world", "")));
}
