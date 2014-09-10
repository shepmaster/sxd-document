pub trait XmlStr {
    fn end_of_attribute(&self, quote: &str) -> Option<uint>;
    fn end_of_literal(&self, expected: &str) -> Option<uint>;
    fn end_of_version_num(&self) -> Option<uint>;
    fn end_of_char_data(&self) -> Option<uint>;
    fn end_of_cdata(&self) -> Option<uint>;
    fn end_of_decimal_chars(&self) -> Option<uint>;
    fn end_of_hex_chars(&self) -> Option<uint>;
    fn end_of_comment(&self) -> Option<uint>;
    fn end_of_pi_value(&self) -> Option<uint>;
    fn end_of_start_rest(&self, is_first: |char| -> bool, is_rest: |char| -> bool) -> Option<uint>;
    fn end_of_name(&self) -> Option<uint>;
    fn end_of_space(&self) -> Option<uint>;
    fn end_of_start_tag(&self) -> Option<uint>;
}

impl<'a> XmlStr for &'a str {
    fn end_of_attribute(&self, quote: &str) -> Option<uint> {
        if self.starts_with("&") ||
           self.starts_with("<") ||
           self.starts_with(quote)
        {
            return None;
        }

        let (quote_char, _) = quote.slice_shift_char();
        let quote_char = quote_char.expect("Cant have null quote");

        let mut positions = self.char_indices().skip_while(|&(_, c)| c != '&' && c != '<' && c != quote_char);

        match positions.next() {
            Some((offset, _)) => Some(offset),
            None => Some(self.len())
        }
    }

    fn end_of_literal(&self, expected: &str) -> Option<uint> {
        if self.starts_with(expected) {
            Some(expected.len())
        } else {
            None
        }
    }

    fn end_of_version_num(&self) -> Option<uint> {
        if self.starts_with("1.") {
            let mut positions = self.char_indices().peekable();
            positions.next();
            positions.next();

            // Need at least one character
            match positions.peek() {
                Some(&(_, c)) if c.is_decimal_char() => {},
                _ => return None,
            };

            let mut positions = positions.skip_while(|&(_, c)| c.is_decimal_char());
            match positions.next() {
                Some((offset, _)) => Some(offset),
                None => Some(self.len()),
            }
        } else {
            None
        }
    }


    fn end_of_char_data(&self) -> Option<uint> {
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
                None => return Some(self.len()),
                Some((offset, c)) if c == '<' || c == '&' => return Some(offset),
                Some((offset, _)) => {
                    let tail = self.slice_from(offset);
                    if tail.starts_with("]]>") {
                        return Some(offset)
                    } else {
                        // False alarm, resume scanning
                        continue;
                    }
                },
            }
        }
    }

    fn end_of_cdata(&self) -> Option<uint> {
        match self.find_str("]]>") {
            None => None,
            Some(offset) => Some(offset),
        }
    }

    fn end_of_decimal_chars(&self) -> Option<uint> {
        self.end_of_start_rest(|c| c.is_decimal_char(),
                               |c| c.is_decimal_char())
    }

    fn end_of_hex_chars(&self) -> Option<uint> {
        self.end_of_start_rest(|c| c.is_hex_char(),
                               |c| c.is_hex_char())
    }

    fn end_of_comment(&self) -> Option<uint> {
        // This deliberately does not include the >. -- is not allowed
        // in a comment, so we can just test the end if it matches the
        // complete close delimiter.
        match self.find_str("--") {
            None => None,
            Some(offset) => Some(offset),
        }
    }

    fn end_of_pi_value(&self) -> Option<uint> {
        match self.find_str("?>") {
            None => None,
            Some(offset) => Some(offset),
        }
    }

    fn end_of_start_rest(&self,
                         is_first: |char| -> bool,
                         is_rest: |char| -> bool)
                         -> Option<uint>
    {
        let mut positions = self.char_indices();

        match positions.next() {
            Some((_, c)) if is_first(c) => (),
            Some((_, _)) => return None,
            None => return None,
        };

        let mut positions = positions.skip_while(|&(_, c)| is_rest(c));
        match positions.next() {
            Some((offset, _)) => Some(offset),
            None => Some(self.len()),
        }
    }

    fn end_of_name(&self) -> Option<uint> {
        self.end_of_start_rest(|c| c.is_name_start_char(), |c| c.is_name_char())
    }

    fn end_of_space(&self) -> Option<uint> {
        self.end_of_start_rest(|c| c.is_space_char(), |c| c.is_space_char())
    }

    fn end_of_start_tag(&self) -> Option<uint> {
        let mut positions = self.char_indices();

        match positions.next() {
            Some((_, c)) if '<' == c => (),
            _ => return None,
        };

        match positions.next() {
            Some((offset, c)) =>
                match c {
                    '?' | '!' | '/' => None,
                    _ => Some(offset),
                },
            None => Some(self.len()),
        }
    }
}

trait XmlChar {
    fn is_name_start_char(&self) -> bool;
    fn is_name_char(&self) -> bool;
    fn is_space_char(&self) -> bool;
    fn is_decimal_char(&self) -> bool;
    fn is_hex_char(&self) -> bool;
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

    fn is_decimal_char(&self) -> bool {
        match *self {
            '0'..'9' => true,
            _ => false,
        }
    }

    fn is_hex_char(&self) -> bool {
        match *self {
            '0'..'9' |
            'a'..'f' |
            'A'..'F' => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {

use super::XmlStr;

#[test]
fn end_of_char_data_leading_ampersand() {
    assert_eq!("&".end_of_char_data(), None);
}

#[test]
fn end_of_char_data_leading_less_than() {
    assert_eq!("<".end_of_char_data(), None);
}

#[test]
fn end_of_char_data_leading_cdata_end() {
    assert_eq!("]]>".end_of_char_data(), None);
}

#[test]
fn end_of_char_data_until_ampersand() {
    assert_eq!("hello&world".end_of_char_data(), Some("hello".len()));
}

#[test]
fn end_of_char_data_until_less_than() {
    assert_eq!("hello<world".end_of_char_data(), Some("hello".len()));
}

#[test]
fn end_of_char_data_until_cdata_end() {
    assert_eq!("hello]]>world".end_of_char_data(), Some("hello".len()));
}

#[test]
fn end_of_char_data_includes_right_square() {
    assert_eq!("hello]world".end_of_char_data(), Some("hello]world".len()));
}

}
