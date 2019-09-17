// macro for parsing strings
#[macro_use] extern crate scan_rules;

use std::iter::FromIterator;
use std::collections::HashSet;

fn str_to_sorted_vec(line : &str) -> Vec<char> {
    let mut chars: Vec<char> = line.chars().collect();
    chars.sort();
    return chars;
}

fn count_appearing_chars(sorted_chars : &[char]){
    let mut goal_2: i32 = 0;
    let mut goal_3: i32 = 0;
    let charset : HashSet<char> = HashSet::from_iter(sorted_chars.iter().cloned());
    'initial: for initial_c in charset {
        let mut count : i32 = 0;
        'current: for c in sorted_chars{
            if *c != initial_c {break 'current;}
            count += 1;
        }
        match count {
            2 => goal_2 += 1,
            3 => goal_3 += 1,
            _ => (),
        }
    }
}

fn main() {
    // read from file to string
    let filename = "input.txt";
    let text = std::fs::read_to_string(filename).expect(
        format!("Problem reading file {}", &filename).as_str()
        );
    println!("File length: {}", text.len());

    // parse text into number of exactly two-times-occurring characters

}
