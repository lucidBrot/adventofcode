/*
 * My first attempt with Rust.
 * Author: Eric Mink
 */

mod main_mod {
    pub fn run(){
        println!("Module Ran!");
        let string : String = SampleStruct{val: 5}.sample(3);
        println!("{}", string);
    }

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

fn main() {
    println!("Hello, world!");
    main_mod::run();
}
