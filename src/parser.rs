use super::{Document,Root,RootChild};

struct Parser;

impl Parser {
    fn new() -> Parser {
        Parser
    }

    fn parse_preamble(&self, xml: &str) -> uint {
        // Parse the preamble
        let idx = xml.find_str("?>").expect("No preamble end");
        let end_of_preamble = idx + "?>".len();
        end_of_preamble
    }

    fn parse_element<'a>(&self, xml: &'a str) -> (uint, &'a str) {
        // Skip the opening
        // TODO: verify is a <
        let after_brace = "<".len();
        let ele = xml.slice_from(after_brace);
        let name = ele.slice_name().expect("failed to parse a name!");
        (after_brace + name.len(), name)
    }

    fn parse(&self, xml: &str) -> Document {
        let doc = Document::new();

        let end_of_preamble = self.parse_preamble(xml);
        let element = xml.slice_from(end_of_preamble);
        let (_end_of_element, name) = self.parse_element(element);

        let e = doc.new_element(name.to_string());
        doc.root().append_child(e);
        doc
    }
}

trait XmlStr<'a> {
    fn slice_name(&self) -> Option<&'a str>;
}

impl<'a> XmlStr<'a> for &'a str {
    fn slice_name(&self) -> Option<&'a str> {
        let mut positions = self.char_indices();

        let first_char = match positions.next() {
            Some((_, c)) if c.is_name_start_char() => c,
            Some((_, c)) => return None,
            None => return None,
        };

        // Skip past all the name chars
        let mut positions = positions.skip_while(|&(_, c)| c.is_name_char());

        match positions.next() {
            Some((offset, _)) => Some(self.slice_to(offset)),
            None => Some(self.clone()),
        }
    }
}

trait XmlChar {
    fn is_name_start_char(&self) -> bool;
    fn is_name_char(&self) -> bool;
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
}

trait Hax {
    fn first_child(&self) -> Option<RootChild>;
}

impl Hax for Root {
    fn first_child(&self) -> Option<RootChild> {
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
