use ast::{AstNode, LetStatement, ReturnStatement};

use crate::ast::*;
use crate::lexer::{self, lexer::*};
use crate::token::token::{Identifier, *};

struct Parser<'a> {
    lex: &'a mut Lexer<'a>,
    curtoken: TokenType,
    peektoken: TokenType,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'a mut Lexer<'a>) -> Self {
        //cur token needs to become peek token
        // and peek token is always token returned from lexer.
        Self {
            lex: lexer,
            curtoken: TokenType::EOF,
            peektoken: TokenType::EOF,
            errors: Vec::new(),
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
            TokenType::Keyword(Keywords::RETURN("return")) => {
                return self
                    .parse_return_statement()
                    .map(|return_stmt| -> Box<dyn ast::Statement> { Box::new(return_stmt) })
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
        println!("{:?}", self.peektoken);
        self.expect_peek(|t| matches!(t, TokenType::Ident(Identifier::IDENT(_))));
        let identifier = self
            .parse_identifier()
            .expect("Parse identifier should return an Identifier Object not None");
        //self.next_token();
        self.expect_peek(|t| matches!(t, TokenType::Operator(Operator::ASSIGN("="))));
        // we know current value is assign operator - so we iterate forwards again
        self.next_token();
        let astnode = Box::new(ast::Expression::new(
            self.curtoken.clone(),
            self.curtoken.retrieve_value().unwrap().to_string(),
        ));
        let tok = Ok(LetStatement::new(lettoken, identifier, astnode));
        while !self.cur_token_is(TokenType::Del(Delimiters::SEMICOLON(";"))) {
            self.next_token();
        }
        return tok;
    }

    fn parse_return_statement(&mut self) -> Result<ast::ReturnStatement, ()> {
        // assume its a return statement
        println!("{:?}", self.peektoken);
        let expression = ast::Expression::new(
            self.peektoken.clone(),
            self.peektoken.retrieve_value().unwrap().to_string(),
        );
        let return_stmnt = ast::ReturnStatement::new(self.curtoken.clone(), expression);
        self.next_token();
        if !self.cur_token_is(TokenType::Del(Delimiters::SEMICOLON(";"))) {
            self.next_token();
        }
        return Ok(return_stmnt);
    }

    fn parse_identifier(&self) -> Option<ast::Identifier> {
        Some(ast::Identifier::new(
            self.curtoken.clone(),
            self.curtoken.retrieve_value()?.to_string(),
        ))
    }

    fn cur_token_is(&self, tok: TokenType) -> bool {
        self.curtoken == tok
    }

    fn peek_token_is(&self, tok: &TokenType) -> bool {
        self.peektoken == *tok
    }

    fn peek_error(&mut self, tok: TokenType) -> () {
        let msg = format!(
            "Expected token to be {}, got {}",
            tok.retrieve_value().unwrap(),
            self.peektoken.retrieve_value().unwrap()
        );
        //panic!("{}", msg);
        self.errors.push(msg);
    }

    fn expect_peek<F>(&mut self, matcher: F) -> bool
    where
        F: FnOnce(&TokenType) -> bool,
    {
        if matcher(&self.peektoken) {
            self.next_token();
            return true;
        } else {
            self.peek_error(self.curtoken.clone());
            return false;
        }
    }

    fn get_errors(&self) -> &Vec<String> {
        &self.errors
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
                Ok(a) => assert_eq!(get_statement_value(&*a.statements[i]), v.to_string()),
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

        check_parser_errors(&parser);

        let test_output = vec!["x", "y", "footbar"];

        for (i, v) in test_output.iter().enumerate() {
            match &prog {
                Ok(a) => assert_eq!(get_statement_identifiers(&*a.statements[i]), v.to_string()),
                Err(_a) => (),
            }
        }
    }

    #[test]
    #[should_panic]
    fn test_let_statements_errors() {
        let input = "let x 5;
        let y = 10;
        let footbar = 6546;
        ";
        let mut lex = Lexer::new();

        lex.process_input(input);

        let mut parser = Parser::new(&mut lex);

        let prog = parser.parse_programme();

        check_parser_errors(&parser);

        let test_output = vec!["x", "y", "footbar"];

        for (i, v) in test_output.iter().enumerate() {
            match &prog {
                Ok(a) => assert_eq!(get_statement_identifiers(&*a.statements[i]), v.to_string()),
                Err(_a) => (),
            }
        }
    }

    #[test]
    fn test_return_statement_values() {
        let input = "return 5;
            return 10;";
        let mut lex = Lexer::new();

        lex.process_input(input);

        let mut parser = Parser::new(&mut lex);

        let prog = parser.parse_programme();

        check_parser_errors(&parser);

        let test_output = vec!["5", "10"];

        for (i, v) in test_output.iter().enumerate() {
            match &prog {
                Ok(a) => assert_eq!(get_statement_value(&*a.statements[i]), v.to_string()),
                Err(_a) => (),
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "return 5;
            return 10;";
        let mut lex = Lexer::new();

        lex.process_input(input);

        let mut parser = Parser::new(&mut lex);

        let prog = parser.parse_programme();

        check_parser_errors(&parser);

        let test_output = vec!["5", "10"];

        for (i, _v) in test_output.iter().enumerate() {
            match &prog {
                Ok(a) => assert_eq!(
                    check_statement_type(&*a.statements[i]),
                    TokenType::Keyword(Keywords::RETURN("return"))
                ),
                Err(_a) => (),
            }
        }
    }

    fn get_statement_identifiers(s: &dyn Statement) -> String {
        s.get_identifier().unwrap().token_literal()
    }

    fn get_statement_value(s: &dyn Statement) -> String {
        s.get_value().token_literal()
    }

    fn check_statement_type(s: &dyn Statement) -> TokenType {
        s.get_token().clone()
    }

    fn check_parser_errors(p: &Parser) {
        let errors = p.get_errors();
        if errors.len() == 0 {
            return;
        } else {
            for i in errors {
                println!("{}", i)
            }
            panic!();
        }
    }
}
