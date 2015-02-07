#![feature(collections)]
#![feature(io)]
#![feature(os)]
#![feature(env)]
#![feature(path)]

extern crate document;

use std::env;
use std::cmp::min;
use std::old_io::{File,BufferedWriter,stdio};

use document::parser::Parser;

fn pretty_error(xml: &str, position: usize) -> &str {
    let s = &xml[position..];
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

    let package = match p.parse(&data) {
        Ok(d) => d,
        Err(point) => panic!("Unable to parse: {}", pretty_error(&data, point)),
    };

    let mut out = BufferedWriter::new(stdio::stdout_raw());

    {
        let d = package.as_document();
        document::writer::format_document(&d, &mut out).ok().expect("I can't output");
    }
}

#[allow(dead_code)]
fn main() {
    let args: Result<Vec<_>, _> = env::args().map(|a| a.into_string()).collect();
    let mut args = args.unwrap();

    let filename = args.remove(1);

    if filename == "-" {
        process_input(stdio::stdin_raw());
    } else {
        process_input(File::open(&Path::new(filename)))
    }
}
