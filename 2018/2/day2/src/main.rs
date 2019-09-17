// macro for parsing strings
#[macro_use] extern crate scan_rules;

fn main() {
    // read from file to string
    let filename = "input.txt";
    let text = std::fs::read_to_string(filename).expect(
        format!("Problem reading file {}", &filename).as_str()
        );
    println!("File read! {}", text.len());
}
