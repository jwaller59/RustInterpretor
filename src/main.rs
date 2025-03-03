use lexer::lexer::new_lexer;
use std::io::{stdin, stdout};
pub mod lexer;
pub mod token;

fn main() {
    println!("Hello, world!");
    let mut buf = String::new();
    println!("Input some code and we can try to parse!");
    stdin().read_line(&mut buf);
    let buf_len = buf.len();
    println!("{}", buf);
    let mut lex = new_lexer(buf);
    let mut i = 1;
    while lex.read_position < buf_len {
        let token = lex.next_token();
        println!("{:?}", token);
        i += 1
    }
}
