trait StrParseExt {
    fn end_of_start_rest<F1, F2>(&self, is_first: F1, is_rest: F2) -> Option<usize>
        where F1: Fn(char) -> bool,
              F2: Fn(char) -> bool;
}

impl<'a> StrParseExt for &'a str {
    fn end_of_start_rest<F1, F2>(&self, is_first: F1, is_rest: F2) -> Option<usize>
        where F1: Fn(char) -> bool,
              F2: Fn(char) -> bool,
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
}

pub trait XmlStr {
    /// Find the end of the quoted attribute value, not including the quote
    fn end_of_attribute(&self, quote: &str) -> Option<usize>;
    /// Find the end of the direct character data
    fn end_of_char_data(&self) -> Option<usize>;
    /// Find the end of the CData section, not including the ]]>
    fn end_of_cdata(&self) -> Option<usize>;
    /// Find the end of a run of decimal characters
    fn end_of_decimal_chars(&self) -> Option<usize>;
    /// Find the end of a run of hexidecimal characters
    fn end_of_hex_chars(&self) -> Option<usize>;
    /// Find the end of the comment, not including the -->
    fn end_of_comment(&self) -> Option<usize>;
    /// Find the end of the processing instruction, not including the ?>
    fn end_of_pi_value(&self) -> Option<usize>;
    /// Find the end of the [Name](http://www.w3.org/TR/xml/#NT-Name)
    fn end_of_name(&self) -> Option<usize>;
    /// Find the end of the [NCName](http://www.w3.org/TR/REC-xml-names/#NT-NCName)
    fn end_of_ncname(&self) -> Option<usize>;
    /// Find the end of a run of space characters
    fn end_of_space(&self) -> Option<usize>;
    /// Find the end of the starting tag
    fn end_of_start_tag(&self) -> Option<usize>;
    fn end_of_encoding(&self) -> Option<usize>;
    /// Find the end of the internal doc type declaration, not including the ]
    fn end_of_int_subset(&self) -> Option<usize>;
}

impl<'a> XmlStr for &'a str {
    fn end_of_attribute(&self, quote: &str) -> Option<usize> {
        if self.len() == 0 ||
           self.starts_with('&') ||
           self.starts_with('<') ||
           self.starts_with(quote)
        {
            return None;
        }

        let quote_char = quote.chars().next().expect("Cant have null quote");

        self.find(&['&', '<', quote_char][..]).or(Some(self.len()))
    }

    fn end_of_char_data(&self) -> Option<usize> {
        fn find_end_of_char_data(bytes: &[u8]) -> Option<usize> {
            for (i, &b) in bytes.iter().enumerate() {
                if b == b'<' || b == b'&' { return Some(i) }

                if b == b']' && bytes[i..].starts_with(b"]]>") {
                    return Some(i)
                }
            }
            None
        }

        match find_end_of_char_data(self.as_bytes()) {
            Some(0) => None,
            Some(v) => Some(v),
            None => Some(self.len()),
        }
    }

    fn end_of_cdata(&self) -> Option<usize> {
        self.find("]]>")
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
        self.find("--")
    }

    fn end_of_pi_value(&self) -> Option<usize> {
        self.find("?>")
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

    fn end_of_encoding(&self) -> Option<usize> {
        self.end_of_start_rest(|c| c.is_encoding_start_char(), |c| c.is_encoding_rest_char())
    }

    fn end_of_int_subset(&self) -> Option<usize> { self.find("]") }
}

/// Predicates used when parsing an characters in an XML document.
pub trait XmlChar {
    /// Is this a [NameStartChar](http://www.w3.org/TR/xml/#NT-NameStartChar)?
    fn is_name_start_char(self) -> bool;
    /// Is this a [NameChar](http://www.w3.org/TR/xml/#NT-NameChar)?
    fn is_name_char(self) -> bool;
    /// Does this start a [NCName](http://www.w3.org/TR/REC-xml-names/#NT-NCName)?
    fn is_ncname_start_char(self) -> bool;
    /// Is this a component of a [NCName](http://www.w3.org/TR/REC-xml-names/#NT-NCName)?
    fn is_ncname_char(self) -> bool;
    /// Is this an [XML space](http://www.w3.org/TR/xml/#NT-S)?
    fn is_space_char(self) -> bool;
    fn is_decimal_char(self) -> bool;
    fn is_hex_char(self) -> bool;
    fn is_encoding_start_char(self) -> bool;
    fn is_encoding_rest_char(self) -> bool;
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

    fn is_encoding_start_char(self) -> bool {
        match self {
            'A'...'Z' |
            'a'...'z' => true,
            _ => false,
        }
    }

    fn is_encoding_rest_char(self) -> bool {
        match self {
            'A'...'Z' |
            'a'...'z' |
            '0'...'9' |
            '.' |
            '_' |
            '-' => true,
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

    #[test]
    fn end_of_char_data_includes_multiple_right_squares() {
        assert_eq!("hello]]world".end_of_char_data(), Some("hello]]world".len()));
    }

    #[test]
    fn end_of_int_subset_excludes_right_square() {
        assert_eq!("hello]>world".end_of_int_subset(), Some("hello".len()))
    }
}
