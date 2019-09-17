use std::iter::FromIterator;
use std::collections::HashSet;

fn str_to_sorted_vec(line : &str) -> Vec<char> {
    let mut chars: Vec<char> = line.chars().collect();
    chars.sort();
    return chars;
}

fn count_appearing_chars(sorted_chars : &[char]) -> (i32, i32){
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
    return (goal_2, goal_3);
}

fn parse_buffer(buffer: &String) -> (i32, i32) {
    let mut score_2 : i32 = 0;
    let mut score_3 : i32 = 0;
    let res = buffer.lines().map(|s| count_appearing_chars(&str_to_sorted_vec(s)));
    let (score_2, score_3) = res.fold((0,0), |(a,b), (c,d)| (a+b, c+d));
    println!("scores:: {:?}", (score_2, score_3));
    return (score_2, score_3);
}

fn main() {
    // read from file to string
    let filename = "input.txt";
    let text = std::fs::read_to_string(filename).expect(
        format!("Problem reading file {}", &filename).as_str()
        );
    println!("File length: {}", text.len());

    // parse text into number of exactly two-times-occurring characters
    let results = parse_buffer(&text);
    println!("Checksum: {}", results.0*results.1);

}
