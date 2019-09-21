fn str_to_sorted_vec(line : &str) -> Vec<char> {
    let mut chars: Vec<char> = line.chars().collect();
    chars.sort();
    return chars;
}

fn count_appearing_chars(sorted_chars : &[char]) -> (i32, i32){
    let mut goal_2: i32 = 0;
    let mut goal_3: i32 = 0;

    let mut i : usize = 0;
    let mut stop : bool = false;
    while (i < sorted_chars.len()-1) && !stop {
        let initial_c : char = sorted_chars[i];
        let mut count : i32 = 0;

        'inner: for cc in i..sorted_chars.len() {
            let c:char = sorted_chars[cc];
            if c != initial_c {
                // skip ahead so that the next i points to the new character;
                // and decrement by one because the outer loop will increment it again
                i = cc -1 ;
                break 'inner;
            }

            // count current character
            count += 1;

            // stop if at last char
            if cc+1 == sorted_chars.len() {stop = true;}
        }


        match count {
            2 => goal_2 += 1,
            3 => goal_3 += 1,
            _ => (),
        }

        i+=1;
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
macro_rules! test_parse_buffer {
    ( $name:ident, $input:expr ) => {
        #[test]
        fn $name () {
            println!("Testing {:?}", $input);
        }
    };
}

#[cfg(test)]
mod parse_tests {
    test_parse_buffer!(hai, "hello");
}

#[cfg(test)]
mod count_tests {
    use super::*;

    #[test]
    fn test_count_sorted_line_1(){
        let input = vec!['a','a','b','b','b','c','d', 'd'];
        let (goal_2, goal_3) = count_appearing_chars(&input);
        assert_eq!(goal_2, 2, "goal_2 was {} but should have been {}", goal_2, 2);
        assert_eq!(goal_3, 1, "goal_3 was {} but should have been {}", goal_3, 1);
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
        assert_eq!(goal_2, 0, "goal_2 was {} but should have been {}", goal_2, 0);
        assert_eq!(goal_3, 1, "goal_3 was {} but should have been {}", goal_3, 1);
    }


    #[test]
    fn test_count_sorted_line_5_inv(){
        let input = vec!['b', 'a','a', 'a'];
        let (goal_2, goal_3) = count_appearing_chars(&input);
        assert_eq!(goal_2, 0, "goal_2 was {}", goal_2);
        assert_eq!(goal_3, 1, "goal_3 was {}", goal_3);
    }
}

