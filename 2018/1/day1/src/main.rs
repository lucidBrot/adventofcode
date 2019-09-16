/*
 * My first attempt with Rust.
 * Author: Eric Mink
 */

// macro for parsing strings
#[macro_use] extern crate scan_rules;

/// My module *supports* **markdown** in the documentation
mod main_mod {
    /// entry point for my hello world module
    pub fn run(){
        println!("Module Ran!");
        let string : String = SampleStruct{val: 5}.sample(3);
        println!("{}", string);
    }

    /// a struct that contains a single `i32` value
    struct SampleStruct {
        val : i32,
    }

    impl SampleStruct {
        fn sample(&self, x: i32) -> String {
            return format!("Hello {}",(x+&self.val));
        }
    }


    // TESTING
    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_sample(){
            let my_struct = SampleStruct{val: 10};
            assert_eq!(my_struct.sample(15), "Hello 25");
        }
    }
}

mod day1_mod {
    /// reads from `input.txt` and prints the sum
    pub fn run() -> std::io::Result<()>{
        use std::io::prelude::*;
        use std::fs::File;

        let mut f = File::open("input.txt")?;
        let mut buffer = String::new();
        f.read_to_string(&mut buffer)?;

        let result: i32 = parse_buffer(&buffer);

        println!("Sum: {}", result);

        let list = parse_day2_buffer(&buffer);
        find_first_duplicate_frequency(&list);

        return Ok(());
    }

    /// Expects a buffer with content of multiple lines
    /// where each line consists of exactly one number preceded by either a plus or a minus.
    fn parse_buffer(buffer: &String) -> i32{
        let mut my_sum: Option<i32> = None;
        scan!(buffer; ([ let ns2: i32 ]*, "\n") => {
            my_sum = Some(ns2.iter().sum());
        }).unwrap(); 
        if let None = my_sum {
            panic!("Parsing failed!");
        }
        return my_sum.unwrap();
    }

    /// Computes the first number that appears twice in the list
    fn find_first_duplicate_frequency(list: &Vec<i32>) -> i32 {
        use std::collections::HashSet;
        let mut encountered_frequencies : HashSet::<i32> = HashSet::new();
        let mut prev : i32 = 0;
        println!("Starting with list of size {}", list.len());
        'outer: loop {
        for elem in list {
            let i : i32 = prev + elem;
            if encountered_frequencies.contains(&i) {
                println!("First Duplicate Frequency: {}", i);
                return i;
            }
            encountered_frequencies.insert(i);
            prev = i;
        }
        }
    }

    fn parse_day2_buffer(buffer: &String) -> Vec<i32> {
        let mut list : Vec<i32> = Vec::<i32>::new();
        scan!(buffer; ([ let ns2: i32 ]*, "\n") => {
            list = ns2;
        }).unwrap();
        return list;
    }

    // TESTING
    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_1(){
            let mylist : Vec<i32> = vec![10,2,-10];
            let dupfr : i32 = find_first_duplicate_frequency(&mylist);
            assert_eq!(dupfr, 12);
        }
    }
}

/// # main function documentation  
/// only public functions are documented
/// unless generated with
/// ```bash
/// cargo rustdoc -- \
/// --no-defaults \
/// --passes strip-hidden \
/// --passes collapse-docs \
/// --passes unindent-comments \
/// --passes strip-priv-imports
/// ```
/// or more simply:  
/// `cargo doc --document-private-items`
pub fn main() {
    println!("Hello, world!");
    main_mod::run();
    let result = day1_mod::run();
    match result {
        Ok(_v) => (),
        Err(e) => println!("error parsing file: {:?}", e),
    }
}
