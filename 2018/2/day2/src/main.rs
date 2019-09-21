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
    return (
        if goal_2 > 0 { 1 } else { 0 },
        if goal_3 > 0 { 1 } else { 0 }
        );
}

fn parse_buffer(buffer: &String) -> (i32, i32) {
    let res = buffer.lines().map(|s| count_appearing_chars(&str_to_sorted_vec(s)));
    let (score_2, score_3) = res.fold((0,0), |(a,b), (c,d)| (a+c, b+d));
    return (score_2, score_3);
}

fn checksum(filename: &str) -> i32 {
    // read from file to string
    let text = std::fs::read_to_string(filename).expect(
        format!("Problem reading file {}", &filename).as_str()
        );

    // parse text into number of exactly two-times-occurring characters
    let results = parse_buffer(&text);
    println!("Duo: {}, Trio:{}", results.0, results.1);
    return results.0*results.1;
}

fn main() {
    println!("Checksum: {}", checksum(&"input.txt"));
}


//TESTS
/// macro that generates test functions for calling parse_buffer
#[cfg(test)]
macro_rules! test_parse_buffer {
    ( $name:ident, $input:expr, $expected:expr ) => {
        #[test]
        fn $name () {
            println!("Testing {:?}", $input);
            let strinput = String::from($input);
            let res = parse_buffer(&strinput);
            assert_eq!(res, $expected, "\nResult was {:?} but should have been {:?}", res, $expected);
        }
    };
}

/// macro that generates test functions for calling any function that takes a string as input and
/// returns something that returns a vector of chars
#[cfg(test)]
macro_rules! test_str_fun {
    ( $fun:ident, $name:ident, $input:expr, $expected:expr ) => {
        #[test]
        fn $name () {
            println!("Testing {:?}", $input);
            let strinput = String::from($input);
            let res : String = $fun(&strinput).iter().collect();
            assert_eq!(res, $expected, "\nResult was {:?} but should have been {:?}", res, $expected);
        }
    };
}

/// macro that generates test functions for checksum
#[cfg(test)]
macro_rules! test_checksum {
    ( $testname:ident, $filename:expr, $expected:expr ) => {
        #[test]
        fn $testname () {
            let res = checksum($filename);
            assert_eq!(res, $expected, "\nResult was {:?} but should have been {:?}", res, $expected);
        }
    };
}

#[cfg(test)]
mod checksum_tests {
    use super::*;
 
    test_checksum!(file1, "test/test1.txt", (1+0+1+1+1+0)*(1+0+0+0+1));
}


#[cfg(test)]
mod parse_tests {
    use super::*;

    test_parse_buffer!(empty, "", (0,0));

    test_parse_buffer!(short, "aba", (1,0));
    
    test_parse_buffer!(two_lines, "aba\nbbadbae", (1+1,1));

    test_parse_buffer!(two_lines_one_empty, "aba\n", (1, 0));

    test_parse_buffer!(three_lines, "aba\ndddafa\ngay", (1+1+0, 0+1+0));
 
    test_parse_buffer!(file3_line4, "ijklmnopqrst", (0,0));
}

#[cfg(test)]
mod str_tests {
    use super::*;

    test_str_fun!(str_to_sorted_vec, strtest1, "aba", "aab");

    test_str_fun!(str_to_sorted_vec, strtest2, "abzaefe", "aabeefz");
}

#[cfg(test)]
mod count_tests {
    use super::*;

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

