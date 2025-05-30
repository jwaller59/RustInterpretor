use crate::ast::*;
use crate::{
    lexer::lexer::Lexer,
    token::token::{Operator, TokenType},
};
use std::collections::HashMap;

pub struct Repl {
    pub assignments: HashMap<String, String>,
    pub tokens: Vec<TokenType>,
    pub ast: HashMap<String, String>,
}

pub fn read(input: String) -> Repl {
    println!("Input some code and we can try to parse!");
    let mut rep = Repl {
        assignments: HashMap::new(),
        tokens: Vec::new(),
        ast: HashMap::new(),
    };
    let mut lex = Lexer::new();
    lex.process_input(&input);
    let mut token: TokenType = lex.next_token();
    rep.tokens.push(token.clone());
    while token != TokenType::EOF {
        token = lex.next_token();
        if let TokenType::Operator(Operator::ASSIGN(_)) = rep.tokens[rep.tokens.len() - 1] {
            let target_token = rep.tokens[rep.tokens.len() - 2].clone();
            rep.assignments.insert(
                target_token.retrieve_string().to_string(),
                token.retrieve_string().to_string(),
            );
        }
        rep.tokens.push(token.clone());
    }
    rep
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::token::token::*;

    #[test]
    fn read_test_tokens() {
        let input = "let orange = 5".to_string();
        let repls = read(input);
        let expected_tokens = vec![
            TokenType::Keyword(Keywords::LET("let")),
            TokenType::Ident(Identifier::IDENT("orange".to_string())),
            TokenType::Operator(Operator::ASSIGN("=")),
            TokenType::Ident(Identifier::INT("5".to_string())),
            TokenType::EOF,
        ];
        assert_eq!(repls.tokens, expected_tokens)
    }
}
