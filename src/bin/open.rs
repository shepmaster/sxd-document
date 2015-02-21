#![cfg_attr(test, allow(dead_code))]

#![feature(collections)]
#![feature(env)]
#![feature(fs)]
#![feature(io)]
#![feature(old_io)]

extern crate document;

use std::env;
use std::cmp::min;
use std::fs::File;
use std::io::{Read,Cursor};
use std::old_io::stdio;

use document::parser::Parser;

fn pretty_error(xml: &str, position: usize) -> &str {
    let s = &xml[position..];
    let l = s.chars().count();
    s.slice_chars(0, min(l, 15))
}

fn process_input<R>(input: R)
    where R: Read
{
    let mut input = input;
    let mut data = String::new();
    match input.read_to_string(&mut data) {
        Ok(x) => x,
        Err(x) => panic!("Can't read: {}", x),
    };

    let p = Parser::new();

    let package = match p.parse(&data) {
        Ok(d) => d,
        Err(point) => panic!("Unable to parse: {}", pretty_error(&data, point)),
    };

    // TODO: Remove buffering when std::io gets stdout
    let mut out = Vec::new();
    let d = package.as_document();
    document::writer::format_document(&d, &mut out).ok().expect("I can't output");

    stdio::stdout_raw().write_all(&out).unwrap();
}

fn main() {
    let mut args: Vec<_> = env::args().collect();

    let filename = args.remove(1);

    if filename == "-" {
        let data = stdio::stdin_raw().read_to_end().unwrap();
        process_input(Cursor::new(data));
    } else {
        let file = File::open(&filename).unwrap();
        process_input(file);
    }
}
