/*
 * My first attempt with Rust.
 * Author: Eric Mink
 */

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
}
