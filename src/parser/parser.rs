use ast::{AstNode, LetStatement};

use crate::ast::*;
use crate::lexer::{self, lexer::*};
use crate::token::token::{Identifier, *};

struct Parser<'a> {
    lex: &'a mut Lexer<'a>,
    curtoken: TokenType,
    peektoken: TokenType,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'a mut Lexer<'a>) -> Self {
        //cur token needs to become peek token
        // and peek token is always token returned from lexer.
        Self {
            lex: lexer,
            curtoken: TokenType::EOF,
            peektoken: TokenType::EOF,
        }
    }

    fn next_token(&mut self) {
        self.curtoken = self.peektoken.clone();
        self.peektoken = self.lex.next_token();
    }

    fn parse_programme(&mut self) -> Result<ast::Program, ()> {
        let mut prog = ast::Program::new();

        while self.curtoken == TokenType::EOF {
            self.next_token();
        }

        while self.curtoken != TokenType::EOF {
            let stat = self
                .parse_statement()
                .expect("Statement parsed was not understood and returned None");
            prog.statements.push(stat);
            self.next_token();
        }
        return Ok(prog);
    }

    fn parse_statement(&mut self) -> Result<Box<dyn ast::Statement>, ()> {
        match &self.curtoken {
            TokenType::Keyword(Keywords::LET("let")) => {
                return self
                    .parse_let_statement()
                    .map(|let_stmt| -> Box<dyn ast::Statement> { Box::new(let_stmt) })
            }
            _ => Err(()),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::LetStatement, ()> {
        // advance tokens,
        // identify the identifier
        // advance tokens again
        // check if current token is equal sign - if its not then we raise error
        // advance tokens
        // retrieve the value from the token
        // create variable statement item
        // assign values and return that statement value
        // this triggers once the current token is identied as a let token
        let lettoken = self.curtoken.clone();
        self.next_token();
        let identifier = self
            .parse_identifier()
            .expect("Parse identifier should return an Identifier Object not None");
        self.next_token();
        if self.curtoken != TokenType::Operator(Operator::ASSIGN("=")) {
            panic!();
        } else {
            // we know current value is assign operator - so we iterate forwards again
            self.next_token();
            let astnode = Box::new(ast::AstNode::new(
                self.curtoken.clone(),
                self.curtoken.retrieve_value().unwrap().to_string(),
            ));
            let tok = Ok(LetStatement::new(lettoken, identifier, astnode));
            while self.curtoken != TokenType::Del(Delimiters::SEMICOLON(";")) {
                self.next_token();
            }
            return tok;
        }
    }

    fn parse_identifier(&self) -> Option<ast::Identifier> {
        Some(ast::Identifier::new(
            self.curtoken.clone(),
            self.curtoken.retrieve_value()?.to_string(),
        ))
    }
}

fn check_parse_response_valid(resp: Option<impl ast::Statement>) -> Result<char, ()> {
    match resp {
        None => Err(()),
        _ => Ok('_'),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::ast::*;

    #[test]
    fn test_let_statements_values() {
        let input = "let x = 5;
        let y = 10;
        let footbar = 6546;
        ";
        let mut lex = Lexer::new();

        lex.process_input(input);

        let mut parser = Parser::new(&mut lex);

        let prog = parser.parse_programme();

        let test_output = vec!["5", "10", "6546"];

        for (i, v) in test_output.iter().enumerate() {
            match &prog {
                Ok(a) => assert_eq!(get_let_statement_values(&*a.statements[i]), v.to_string()),
                Err(_a) => (),
            }
        }
    }

    #[test]
    fn test_let_statements_identifiers() {
        let input = "let x = 5;
        let y = 10;
        let footbar = 6546;
        ";
        let mut lex = Lexer::new();

        lex.process_input(input);

        let mut parser = Parser::new(&mut lex);

        let prog = parser.parse_programme();

        let test_output = vec!["x", "y", "footbar"];

        for (i, v) in test_output.iter().enumerate() {
            match &prog {
                Ok(a) => assert_eq!(
                    get_let_statement_identifiers(&*a.statements[i]),
                    v.to_string()
                ),
                Err(_a) => (),
            }
        }
    }

    fn get_let_statement_values(s: &dyn Statement) -> String {
        s.get_value().token_literal()
    }

    fn get_let_statement_identifiers(s: &dyn Statement) -> String {
        s.get_identifier().token_literal()
    }
}
