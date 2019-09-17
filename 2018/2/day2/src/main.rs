// macro for parsing strings
#[macro_use] extern crate scan_rules;

// define type Reader to learn how to use types
#[derive(Debug)]
struct Reader<'a> {
    filename: &'a str,
    content: String,
}

// define functionality of reader
impl<'a> Reader<'a> {

    fn new(filename: &'a str) -> Reader<'a> {
        Reader {filename: filename, content: String::new()}
    }

    fn read(&mut self) -> std::io::Result<&String> {
        use std::io::prelude::*;
        use std::fs::File;

        let mut f = File::open(&self.filename)?;
        self.content = String::new();
        f.read_to_string(&mut self.content)?;
        return Ok(&self.content);
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
