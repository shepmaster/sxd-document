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

    let data = match file.read_to_string() {
        Ok(x) => x,
        Err(x) => panic!("Can't read: {}", x),
    };

    let p = Parser::new();

    let package = match p.parse(data.as_slice()) {
        Ok(d) => d,
        Err(point) => panic!("Unable to parse: {}", pretty_error(data.as_slice(), point)),
    };

    let mut out = BufferedWriter::new(stdout_raw());

    {
        let d = package.as_document();
        document::writer::format_document(&d, &mut out).ok().expect("I can't output");
    }
}
