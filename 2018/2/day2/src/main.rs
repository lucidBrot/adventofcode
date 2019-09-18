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
    let mut initial_c : char;
    let mut count : i32 = 0;
    let mut index : usize = 0;
    while index < sorted_chars.len() {
        initial_c = sorted_chars[index];
        for c in sorted_chars{
            index+=1;
            if *c != initial_c {break;}
            count += 1;
        }
        match count {
            2 => goal_2 += 1,
            3 => goal_3 += 1,
            _ => (),
        }
    // repeat for initial_c = sorted_chars[index]
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


//TESTS
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_sorted_line(){
        let input = vec!['a','a','b','b','b','c','d', 'd'];
        let (goal_2, goal_3) = count_appearing_chars(&input);
        assert_eq!(goal_2, 2);
        assert_eq!(goal_3, 1);
    }

    #[test]
    fn test_count_sorted_line_2(){
        let input = vec!['a','b','c','d','d'];
        let (goal_2, goal_3) = count_appearing_chars(&input);
        assert_eq!(goal_2, 1);
        assert_eq!(goal_3, 0);
    }

    #[test]
    fn test_count_sorted_line_3(){
        let input = vec!['d'];
        let (goal_2, goal_3) = count_appearing_chars(&input);
        assert_eq!(goal_2, 0);
        assert_eq!(goal_3, 0);
    }

    #[test]
    fn test_count_sorted_line_4(){
        let input = vec!['a','a'];
        let (goal_2, goal_3) = count_appearing_chars(&input);
        assert_eq!(goal_2, 1);
        assert_eq!(goal_3, 0);
    }

    #[test]
    fn test_count_sorted_line_5(){
        let input = vec!['a','a', 'a', 'b'];
        let (goal_2, goal_3) = count_appearing_chars(&input);
        assert_eq!(goal_2, 0);
        assert_eq!(goal_3, 1);
    }


    #[test]
    fn test_count_sorted_line_5_inv(){
        let input = vec!['b', 'a','a', 'a'];
        let (goal_2, goal_3) = count_appearing_chars(&input);
        assert_eq!(goal_2, 0);
        assert_eq!(goal_3, 1);
    }
}

