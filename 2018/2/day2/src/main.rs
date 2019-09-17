// macro for parsing strings
#[macro_use] extern crate scan_rules;

// define type Reader to learn how to use types
#[derive(Debug)]
struct Reader<'a> {
    filename: &'a str
}

// define functionality of reader
impl Reader<'_> {
    fn read(&self) -> std::io::Result<String> {
        use std::io::prelude::*;
        use std::fs::File;

        let mut f = File::open(&self.filename)?;
        let mut buffer = String::new();
        f.read_to_string(&mut buffer)?;
        return Ok(buffer);
    }
}

fn main() {
    println!("Hello, world!");
    let reader = Reader {filename: "input.txt"};
    println!("Reader: {:?}", reader);
    let len = match reader.read() {
        Ok(s) => s.len(),
        Err(e) => { println!("Error parsing file: {:?}", e); 0 },
    };
    println!("File length: {}", len);
}
