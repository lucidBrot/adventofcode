// macro for parsing strings
#[macro_use] extern crate scan_rules;

// define type Reader to learn how to use types
#[derive(Debug)]
struct Reader<'a> {
    filename: &'a str,
}

// define functionality of reader
impl Reader<'_> {

    fn new(filename: &str) -> Reader {
        Reader {filename: filename}
    }

    fn read(&self, mut output: &mut String) -> std::io::Result<()> {
        use std::io::prelude::*;
        use std::fs::File;

        let mut f = File::open(self.filename)?;

        output.clear();
        f.read_to_string(&mut output)?;
    
        return Ok(());
    }

    fn better_read(&self) -> std::io::Result<String>{
        Ok(std::fs::read_to_string(self.filename)?)
    }
}

fn main() {
    println!("Hello, world!");
    let reader = Reader::new("input.txt");
    println!("Reader: {:?}", reader);
    let mut output : String = String::new();
    let len = match reader.read(&mut output) {
        Ok(_) => output.len(),
        Err(e) => { println!("Error parsing file: {:?}", e); 0 },
    };
    println!("File length: {}", len);
}
