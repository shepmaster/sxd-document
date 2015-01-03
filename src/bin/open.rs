extern crate document;

use std::cmp::min;
use std::io::File;
use std::io::BufferedWriter;
use std::io::stdio;

use document::parser::Parser;

fn pretty_error(xml: &str, position: uint) -> &str {
    let s = xml.slice_from(position);
    let l = s.chars().count();
    s.slice_chars(0, min(l, 15))
}

fn process_input<R>(input: R)
    where R: Reader
{
    let mut input = input;
    let data = match input.read_to_string() {
        Ok(x) => x,
        Err(x) => panic!("Can't read: {}", x),
    };

    let p = Parser::new();

    let package = match p.parse(data.as_slice()) {
        Ok(d) => d,
        Err(point) => panic!("Unable to parse: {}", pretty_error(data.as_slice(), point)),
    };

    let mut out = BufferedWriter::new(stdio::stdout_raw());

    {
        let d = package.as_document();
        document::writer::format_document(&d, &mut out).ok().expect("I can't output");
    }
}

#[allow(dead_code)]
fn main() {
    let mut args = std::os::args();

    let filename = args.remove(1);

    if filename == "-" {
        process_input(stdio::stdin_raw());
    } else {
        process_input(File::open(&Path::new(filename)))
    }
}
