use super::{Document,Root,RootChild};

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
