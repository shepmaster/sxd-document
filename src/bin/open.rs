#![cfg_attr(test, allow(dead_code))]

extern crate sxd_document;

use std::env;
use std::cmp::min;
use std::fs::File;
use std::io::{self,Read,Write};

use sxd_document::parser;

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

    let package = parser::parse(&data).unwrap_or_else(|e| {
        panic!("Unable to parse: {}", pretty_error(&data, e.location()));
    });

    // let mut out = io::stdout();
    let mut out = io::sink();
    let d = package.as_document();

    sxd_document::writer::format_document(&d, &mut out).expect("I can't output");
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
