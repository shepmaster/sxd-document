// Options
// Space before /
// Quote style
// Ordering of attributes

#[cfg(test)]
mod test {
    use std::io::{IoResult,MemWriter};

    use super::super::{Document,Element};
    use super::super::{ElementElementChild,TextElementChild,CommentElementChild,PIElementChild};

    macro_rules! assert_str_eq(
        ($l:expr, $r:expr) => (assert_eq!($l.as_slice(), $r.as_slice()));
    )

    enum Content {
        WElement(Element),
        WElementEnd(String),
    }

    fn format_element<W : Writer>(element: Element, todo: &mut Vec<Content>, writer: &mut W) -> IoResult<()> {
        try!(write!(writer, "<{}", element.name()));

        for attr in element.attributes().iter() {
            try!(write!(writer, " {}='{}'", attr.name(), attr.value()));
        }

        let mut children = element.children();
        if children.is_empty() {
            writer.write_str("/>")
        } else {
            try!(writer.write_str(">"));

            todo.push(WElementEnd(element.name()));
            children.reverse();
            let x = children.move_iter().map(|c| match c {
                ElementElementChild(element) => WElement(element),
                TextElementChild(_) => fail!("text?"),
                CommentElementChild(_) => fail!("comment?"),
                PIElementChild(_) => fail!("PI?"),
            }).collect();
            todo.push_all_move(x);

            Ok(())
        }
    }

    fn format_document<W : Writer>(doc: &Document, writer: &mut W) -> IoResult<()> {
        let mut todo = Vec::new();

        try!(writer.write_str("<?xml version='1.0'?>"));
        let e = doc.root().children()[0].element().expect("not an element?");

        todo.push(WElement(e));

        while ! todo.is_empty() {
            match todo.pop().unwrap() {
                WElement(e) => try!(format_element(e, &mut todo, writer)),
                WElementEnd(name) => try!(write!(writer, "</{}>", name)),
            }
        }

        Ok(())
    }

    fn format_xml(doc: &Document) -> String {
        let mut w = MemWriter::new();
        format_document(doc, &mut w).ok().expect("Not formatted");
        String::from_utf8(w.unwrap()).ok().expect("Not a string")
    }

    #[test]
    fn top_element() {
        let d = Document::new();
        let e = d.new_element("hello".to_string());
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello/>");
    }

    #[test]
    fn element_with_attributes() {
        let d = Document::new();
        let e = d.new_element("hello".to_string());
        e.set_attribute("a".to_string(), "b".to_string());
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello a='b'/>");
    }

    #[test]
    fn nested_element() {
        let d = Document::new();
        let hello = d.new_element("hello".to_string());
        let world = d.new_element("world".to_string());
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello><world/></hello>");
    }
}
