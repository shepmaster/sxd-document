use super::peresil::StrParseExt;

pub trait XmlStr {
    fn end_of_attribute(&self, quote: &str) -> Option<usize>;
    fn end_of_char_data(&self) -> Option<usize>;
    fn end_of_cdata(&self) -> Option<usize>;
    fn end_of_decimal_chars(&self) -> Option<usize>;
    fn end_of_hex_chars(&self) -> Option<usize>;
    fn end_of_comment(&self) -> Option<usize>;
    fn end_of_pi_value(&self) -> Option<usize>;
    fn end_of_name(&self) -> Option<usize>;
    fn end_of_ncname(&self) -> Option<usize>;
    fn end_of_space(&self) -> Option<usize>;
    fn end_of_start_tag(&self) -> Option<usize>;
}

impl<'a> XmlStr for &'a str {
    fn end_of_attribute(&self, quote: &str) -> Option<usize> {
        if self.len() == 0 ||
           self.starts_with("&") ||
           self.starts_with("<") ||
           self.starts_with(quote)
        {
            return None;
        }

        let (quote_char, _) = quote.slice_shift_char().expect("Cant have null quote");

        let mut positions = self.char_indices().skip_while(|&(_, c)| c != '&' && c != '<' && c != quote_char);

        match positions.next() {
            Some((offset, _)) => Some(offset),
            None => Some(self.len())
        }
    }

    fn end_of_char_data(&self) -> Option<usize> {
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
                    let tail = &self[offset..];
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

    fn end_of_cdata(&self) -> Option<usize> {
        match self.find("]]>") {
            None => None,
            Some(offset) => Some(offset),
        }
    }

    fn end_of_decimal_chars(&self) -> Option<usize> {
        self.end_of_start_rest(|c| c.is_decimal_char(),
                               |c| c.is_decimal_char())
    }

    fn end_of_hex_chars(&self) -> Option<usize> {
        self.end_of_start_rest(|c| c.is_hex_char(),
                               |c| c.is_hex_char())
    }

    fn end_of_comment(&self) -> Option<usize> {
        // This deliberately does not include the >. -- is not allowed
        // in a comment, so we can just test the end if it matches the
        // complete close delimiter.
        match self.find("--") {
            None => None,
            Some(offset) => Some(offset),
        }
    }

    fn end_of_pi_value(&self) -> Option<usize> {
        match self.find("?>") {
            None => None,
            Some(offset) => Some(offset),
        }
    }

    fn end_of_name(&self) -> Option<usize> {
        self.end_of_start_rest(|c| c.is_name_start_char(), |c| c.is_name_char())
    }

    fn end_of_ncname(&self) -> Option<usize> {
        self.end_of_start_rest(|c| c.is_ncname_start_char(), |c| c.is_ncname_char())
    }

    fn end_of_space(&self) -> Option<usize> {
        self.end_of_start_rest(|c| c.is_space_char(), |c| c.is_space_char())
    }

    fn end_of_start_tag(&self) -> Option<usize> {
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

pub trait XmlChar {
    fn is_name_start_char(self) -> bool;
    fn is_name_char(self) -> bool;
    fn is_ncname_start_char(self) -> bool;
    fn is_ncname_char(self) -> bool;
    fn is_space_char(self) -> bool;
    fn is_decimal_char(self) -> bool;
    fn is_hex_char(self) -> bool;
}

impl XmlChar for char {
    fn is_name_start_char(self) -> bool {
        self == ':' || self.is_ncname_start_char()
    }

    fn is_name_char(self) -> bool {
        self.is_name_start_char() || self.is_ncname_char()
    }

    fn is_ncname_start_char(self) -> bool {
        match self {
            'A'...'Z'                   |
            '_'                         |
            'a'...'z'                   |
            '\u{0000C0}'...'\u{0000D6}' |
            '\u{0000D8}'...'\u{0000F6}' |
            '\u{0000F8}'...'\u{0002FF}' |
            '\u{000370}'...'\u{00037D}' |
            '\u{00037F}'...'\u{001FFF}' |
            '\u{00200C}'...'\u{00200D}' |
            '\u{002070}'...'\u{00218F}' |
            '\u{002C00}'...'\u{002FEF}' |
            '\u{003001}'...'\u{00D7FF}' |
            '\u{00F900}'...'\u{00FDCF}' |
            '\u{00FDF0}'...'\u{00FFFD}' |
            '\u{010000}'...'\u{0EFFFF}' => true,
            _ => false,
        }
    }

    fn is_ncname_char(self) -> bool {
        if self.is_ncname_start_char() { return true; }
        match self {
            '-'                     |
            '.'                     |
            '0'...'9'               |
            '\u{00B7}'              |
            '\u{0300}'...'\u{036F}' |
            '\u{203F}'...'\u{2040}' => true,
            _ => false
        }
    }

    fn is_space_char(self) -> bool {
        match self {
            '\x20' |
            '\x09' |
            '\x0D' |
            '\x0A' => true,
            _ => false,
        }
    }

    fn is_decimal_char(self) -> bool {
        match self {
            '0'...'9' => true,
            _ => false,
        }
    }

    fn is_hex_char(self) -> bool {
        match self {
            '0'...'9' |
            'a'...'f' |
            'A'...'F' => true,
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
