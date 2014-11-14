//!
//! ```
//! use document::Package;
//! let package = Package::new();
//! let doc = package.as_document();
//!
//! let hello = doc.create_element("hello");
//! hello.set_attribute_value("planet", "Earth");
//! let comment = doc.create_comment("What about other planets?");
//! let text = doc.create_text("Greetings, Earthlings!");
//!
//! hello.append_child(comment);
//! hello.append_child(text);
//! doc.root().append_child(hello);
//! ```
//!
//! ### Design decisions
//!
//! Try to leverage the type system as much as possible.

#![crate_name = "document"]
#![experimental]
#![feature(macro_rules)]
#![feature(if_let)]
#![feature(default_type_params)]

extern crate arena;
extern crate test;
extern crate xxhash;

use std::fmt;

mod string_pool;
mod raw;
pub mod thindom4;
pub mod dom4;
pub mod parser;
pub mod writer;

pub struct Package {
    storage: raw::Storage,
    connections: raw::Connections,
}

impl Package {
    pub fn new() -> Package {
        let s = raw::Storage::new();
        let root = s.create_root();
        Package {
            storage: s,
            connections: raw::Connections::new(root),
        }
    }

    pub fn as_document(&self) -> dom4::Document {
        dom4::Document::new(&self.storage, &self.connections)
    }

    pub fn as_thin_document(&self) -> (thindom4::Storage, thindom4::Connections) {
        let s = thindom4::Storage::new(&self.storage);
        let c = thindom4::Connections::new(&self.connections);
        (s, c)
    }
}

impl PartialEq for Package {
    fn eq(&self, other: &Package) -> bool {
        self as *const Package == other as *const Package
    }
}

impl fmt::Show for Package {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Package")
    }
}
