use crate::{
    lexer::{self, lexer::Lexer},
    token::token::{Operator, TokenType},
};
use std::{collections::HashMap, io::stdin};

pub struct Repl {
    pub assignments: HashMap<String, String>,
    pub tokens: Vec<TokenType>,
}

//impl Repl {
//    fn
//
//}

pub fn read() {
    println!("Input some code and we can try to parse!");
    let mut rep = Repl {
        assignments: HashMap::new(),
        tokens: Vec::new(),
    };
    loop {
        let mut buf = String::new();
        stdin().read_line(&mut buf);
        let mut lex = Lexer::new();
        lex.process_input(&buf);
        let mut token: TokenType = lex.next_token();
        rep.tokens.push(token.clone());
        while token != TokenType::EOF {
            println!("{:?}", token);
            token = lex.next_token();
            match rep.tokens[rep.tokens.len() - 1] {
                TokenType::Operator(Operator::ASSIGN(_)) => {
                    let target_token = rep.tokens[rep.tokens.len() - 2].clone();
                    rep.assignments.insert(
                        target_token.retrieve_value().unwrap().to_string(),
                        token.retrieve_value().unwrap().to_string(),
                    );
                }
                _ => (),
            }
            rep.tokens.push(token.clone());
        }
        println!("{:?}", rep.tokens);
        println!("{:?}", rep.assignments);
    }
}
