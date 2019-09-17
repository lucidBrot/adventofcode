// macro for parsing strings
#[macro_use] extern crate scan_rules;

use std::cell::{RefCell,RefMut,Ref};

// define type Reader to learn how to use types
#[derive(Debug)]
struct Reader<'a> {
    filename: &'a str,
    content_rc: RefCell<String>,
}

// define functionality of reader
impl Reader<'_> {

    fn new(filename: &str) -> Reader {
        Reader {filename: filename, content_rc: RefCell::new(String::new())}
    }

    fn read(&self) -> std::io::Result<Ref<String>> {
        use std::io::prelude::*;
        use std::fs::File;

        let mut f = File::open(&self.filename)?;

        {
        let mut rc_refmut : RefMut<_> = self.content_rc.borrow_mut();
        (*rc_refmut).clear();
        f.read_to_string(&mut rc_refmut)?;
        }

        let myref : Ref<String> = self.content_rc.borrow();

        return Ok(myref);
    }
}

fn main() {
    println!("Hello, world!");
    let mut reader = Reader::new("input.txt");
    println!("Reader: {:?}", reader);
    let len = match reader.read() {
        Ok(s) => s.len(),
        Err(e) => { println!("Error parsing file: {:?}", e); 0 },
    };
    println!("File length: {}", len);
}
