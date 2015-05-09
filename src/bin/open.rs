#![cfg_attr(test, allow(dead_code))]

extern crate document;

use std::env;
use std::cmp::min;
use std::fs::File;
use std::io::{self,Read,Write};

use document::parser::Parser;

fn pretty_error(xml: &str, position: usize) -> String {
    let s = &xml[position..];
    let l = s.chars().count();
    s.chars().take(min(l, 15)).collect()
}

fn process_input<R>(input: R)
    where R: Read
{
    let mut input = input;
    let mut data = String::new();

    if let Err(x) = input.read_to_string(&mut data) {
        panic!("Can't read: {}", x);
    }

    let p = Parser::new();

    let package = match p.parse(&data) {
        Ok(d) => d,
        Err((point, _)) => panic!("Unable to parse: {}", pretty_error(&data, point)),
    };

    let mut out = io::stdout();
    let d = package.as_document();

    document::writer::format_document(&d, &mut out).ok().expect("I can't output");
    // Remove when we move back to stdout_raw + buffer or when stdout flushed at program exit
    out.flush().unwrap();
}

fn main() {
    let mut args: Vec<_> = env::args().collect();

    let filename = args.remove(1);

    if filename == "-" {
        process_input(io::stdin());
    } else {
        let file = File::open(&filename).unwrap();
        process_input(file);
    }
}
