extern crate document;

use std::cmp::min;
use std::io::File;
use std::io::BufferedWriter;
use std::io::stdio::stdout_raw;

use document::parser::Parser;

fn pretty_error(xml: &str, position: uint) -> &str {
    let s = xml.slice_from(position);
    let l = s.char_len();
    s.slice_chars(0, min(l, 15))
}

fn main() {
    let mut args = std::os::args();

    let filename = args.remove(1).expect("File required");
    let path = Path::new(filename);
    let mut file = File::open(&path);

    let data = match file.read_to_end() {
        Ok(x) => x,
        Err(x) => fail!("Can't read: {}", x),
    };

    let data = match String::from_utf8(data) {
        Ok(x) => x,
        Err(x) => fail!("Unable to convert to UTF-8: {}", x),
    };

    let p = Parser::new();

    let d = match p.parse(data.as_slice()) {
        Ok(d) => d,
        Err(point) => fail!("Unable to parse: {}", pretty_error(data.as_slice(), point)),
    };

    let mut out = BufferedWriter::new(stdout_raw());

    document::writer::format_document(&d, &mut out).ok().expect("I can't output");
}
