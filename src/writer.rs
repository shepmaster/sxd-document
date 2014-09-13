// Options
// Space before /?
// Quote style

#[cfg(test)]
mod test {
    use std::io::{IoResult,MemWriter};

    use super::super::{Document,Element};

    macro_rules! assert_str_eq(
        ($l:expr, $r:expr) => (assert_eq!($l.as_slice(), $r.as_slice()));
    )

    fn format_document<W : Writer>(doc: &Document, writer: &mut W) -> IoResult<()> {
        try!(writer.write_str("<?xml version='1.0'?>"));
        let e = doc.root().children()[0].element().expect("not an element?");
        write!(writer, "<{}/>", e.name())
    }

    fn format_xml(doc: &Document) -> String {
        let mut w = MemWriter::new();
        format_document(doc, &mut w).ok().expect("Not formatted");
        String::from_utf8(w.unwrap()).ok().expect("Not a string")
    }

    #[test]
    fn format_an_element() {
        let d = Document::new();
        let e = d.new_element("hello".to_string());
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello/>");
    }
}
