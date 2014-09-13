// Options
// Space before /
// Quote style
// Ordering of attributes

#[cfg(test)]
mod test {
    use std::io::{IoResult,MemWriter};

    use super::super::{Document,Element,Text,Comment,ProcessingInstruction};
    use super::super::{ElementElementChild,TextElementChild,CommentElementChild,PIElementChild};
    use super::super::{ElementRootChild,CommentRootChild,PIRootChild};

    macro_rules! assert_str_eq(
        ($l:expr, $r:expr) => (assert_eq!($l.as_slice(), $r.as_slice()));
    )

    enum Content {
        WElement(Element),
        WElementEnd(String),
        WText(Text),
        WComment(Comment),
        WProcessingInstruction(ProcessingInstruction),
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
                TextElementChild(t) => WText(t),
                CommentElementChild(c) => WComment(c),
                PIElementChild(p) => WProcessingInstruction(p),
            }).collect();
            todo.push_all_move(x);

            Ok(())
        }
    }

    fn format_comment<W : Writer>(comment: Comment, writer: &mut W) -> IoResult<()> {
        write!(writer, "<!--{}-->", comment.text())
    }

    fn format_processing_instruction<W : Writer>(pi: ProcessingInstruction, writer: &mut W) -> IoResult<()> {
        match pi.value() {
            None    => write!(writer, "<?{}?>", pi.target()),
            Some(v) => write!(writer, "<?{} {}?>", pi.target(), v),
        }
    }

    fn format_one<W : Writer>(content: Content, todo: &mut Vec<Content>, writer: &mut W) -> IoResult<()> {
        match content {
            WElement(e)               => format_element(e, todo, writer),
            WElementEnd(name)         => write!(writer, "</{}>", name),
            WText(t)                  => writer.write_str(t.text().as_slice()),
            WComment(c)               => format_comment(c, writer),
            WProcessingInstruction(p) => format_processing_instruction(p, writer),
        }
    }

    fn format_document<W : Writer>(doc: &Document, writer: &mut W) -> IoResult<()> {
        try!(writer.write_str("<?xml version='1.0'?>"));

        for child in doc.root().children().move_iter() {
            match child {
                ElementRootChild(e) => {
                    let mut todo = vec![WElement(e)];

                    while ! todo.is_empty() {
                        try!(format_one(todo.pop().unwrap(), &mut todo, writer));
                    }
                },
                CommentRootChild(c) => try!(format_comment(c, writer)),
                PIRootChild(_) => fail!("pi?"),
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

    #[test]
    fn nested_text() {
        let d = Document::new();
        let hello = d.new_element("hello".to_string());
        let text = d.new_text("A fine day to you!".to_string());
        hello.append_child(text);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello>A fine day to you!</hello>");
    }

    #[test]
    fn nested_comment() {
        let d = Document::new();
        let hello = d.new_element("hello".to_string());
        let comment = d.new_comment(" Fill this in ".to_string());
        hello.append_child(comment);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello><!-- Fill this in --></hello>");
    }

    #[test]
    fn nested_processing_instruction_without_value() {
        let d = Document::new();
        let hello = d.new_element("hello".to_string());
        let pi = d.new_processing_instruction("display".to_string(), None);
        hello.append_child(pi);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello><?display?></hello>");
    }

    #[test]
    fn nested_processing_instruction_with_value() {
        let d = Document::new();
        let hello = d.new_element("hello".to_string());
        let pi = d.new_processing_instruction("display".to_string(), Some("screen".to_string()));
        hello.append_child(pi);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><hello><?display screen?></hello>");
    }

    #[test]
    fn top_level_comment() {
        let d = Document::new();
        let pi = d.new_comment(" Fill this in ".to_string());
        d.root().append_child(pi);

        let xml = format_xml(&d);
        assert_str_eq!(xml, "<?xml version='1.0'?><!-- Fill this in -->");
    }
}
