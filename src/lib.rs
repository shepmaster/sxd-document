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
#![feature(slicing_syntax)]

extern crate arena;
extern crate test;
extern crate xxhash;

use std::fmt;

mod string_pool;
mod raw;

#[macro_use]
pub mod peresil;
pub mod thindom4;
pub mod dom4;
pub mod parser;
pub mod writer;

#[derive(Show,PartialEq,Eq,PartialOrd)]
pub struct QName<'s> {
    namespace_uri: Option<&'s str>,
    local_part: &'s str,
}

impl<'s> QName<'s> {
    pub fn new(local_part: &'s str) -> QName<'s> {
        QName::with_namespace_uri(None, local_part)
    }

    pub fn with_namespace_uri(namespace_uri: Option<&'s str>, local_part: &'s str) -> QName<'s> {
        QName {
            namespace_uri: namespace_uri,
            local_part: local_part,
        }
    }

    pub fn namespace_uri(&self) -> Option<&'s str> { self.namespace_uri }
    pub fn local_part(&self) -> &'s str { self.local_part }
}

impl<'s> Ord for QName<'s> {
    fn cmp(&self, other: &QName<'s>) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub trait ToQName<'s> {
    fn to_qname(self) -> QName<'s>;
}

impl<'s> ToQName<'s> for QName<'s> {
    fn to_qname(self) -> QName<'s> { self }
}

impl<'s> ToQName<'s> for &'s str {
    fn to_qname(self) -> QName<'s> { QName { namespace_uri: None, local_part: self } }
}

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

#[doc(hidden)]
mod document {
    pub use peresil;
}
