use ast::LetStatement;

use crate::ast::ast::{Expression, ExpressionStatement};
use crate::ast::*;
use crate::lexer::lexer::*;
use crate::token::token::{Identifier, *};

const LOWEST: i8 = 1;
const EQUALS: i8 = 2;
const LESSGREATER: i8 = 3;
const SUM: i8 = 4;
const PRODUCT: i8 = 5;
const PREFIX: i8 = 6;
const CALL: i8 = 7;

pub struct Parser<'a> {
    lex: &'a mut Lexer<'a>,
    curtoken: TokenType,
    peektoken: TokenType,
    errors: &'a mut Vec<String>,
}

impl<'a> Parser<'a> {
    /// Creates a new [`Parser`].
    pub fn new(lex: &'a mut Lexer<'a>, errors: &'a mut Vec<String>) -> Self {
        Self {
            lex,
            curtoken: TokenType::EOF,
            peektoken: TokenType::EOF,
            errors,
        }
    }
}

impl<'a> Parser<'a> {
    fn next_token(&mut self) {
        self.curtoken = self.peektoken.clone();
        self.peektoken = self.lex.next_token();
    }

    /// Returns a reference to the get curtoken of this [`Parser`].
    fn get_curtoken(&self) -> &TokenType {
        &self.curtoken
    }

    pub fn parse_programme(&mut self) -> Result<ast::Program, ()> {
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
        Ok(prog)
    }

    fn parse_statement(&mut self) -> Result<Box<dyn ast::Statement>, ()> {
        match &self.curtoken {
            TokenType::Keyword(Keywords::LET("let")) => self
                .parse_let_statement()
                .map(|let_stmt| -> Box<dyn ast::Statement> { Box::new(let_stmt) }),
            TokenType::Keyword(Keywords::RETURN("return")) => self
                .parse_return_statement()
                .map(|return_stmt| -> Box<dyn ast::Statement> { Box::new(return_stmt) }),
            _ => self
                .parse_expression_statement()
                .map(|express_stmt| -> Box<dyn ast::Statement> { Box::new(express_stmt) }),
        }
    }
    // TODO: Reformat this as I don't like the multiple clones required - may bee too abstracted
    // Need to check which parsing function this expression has associated with it
    fn parse_expression_statement(&mut self) -> Result<ast::ExpressionStatement, ()> {
        let cur_tok = self.curtoken.clone();
        let o = self
            .parse_expresssion(LOWEST)
            .expect("Expression returned should never be Null");
        if self.peektoken == TokenType::Del(Delimiters::SEMICOLON(";")) {
            self.next_token();
        }
        Ok(ast::ExpressionStatement::new(cur_tok, o))
    }

    fn parse_expresssion(&mut self, preced: i8) -> Option<Box<dyn Expression>> {
        println!("{:?}", self.curtoken);
        Some(self.registerprefix().unwrap_or_else(|| {
            panic!(
                "This token cannot have a Prefix associated with it, {}",
                self.curtoken.retrieve_string().unwrap()
            )
        }))
        // Some(self.registerprefix().expect("t"))
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
        self.expect_peek(|t| matches!(t, TokenType::Ident(Identifier::IDENT(_))));
        let identifier = self
            .parse_identifier()
            .expect("Parse identifier should return an Identifier Object not None");
        //self.next_token();
        self.expect_peek(|t| matches!(t, TokenType::Operator(Operator::ASSIGN("="))));
        // we know current value is assign operator - so we iterate forwards again
        self.next_token();
        let value = ast::Identifier::new(
            self.curtoken.clone(),
            ast::ReturnValue::String(self.curtoken.retrieve_string().unwrap().to_string()),
        );
        let tok = Ok(LetStatement::new(lettoken, identifier, Box::new(value)));
        while !self.cur_token_is(TokenType::Del(Delimiters::SEMICOLON(";"))) {
            self.next_token();
        }
        tok
    }

    fn parse_return_statement(&mut self) -> Result<ast::ReturnStatement, ()> {
        // assume its a return statement
        let expression = ast::Identifier::new(
            self.peektoken.clone(),
            ast::ReturnValue::String(self.peektoken.retrieve_string().unwrap().to_string()),
        );
        let return_stmnt = ast::ReturnStatement::new(self.curtoken.clone(), Box::new(expression));
        self.next_token();
        if !self.cur_token_is(TokenType::Del(Delimiters::SEMICOLON(";"))) {
            self.next_token();
        }
        Ok(return_stmnt)
    }

    fn parse_identifier(&self) -> Option<ast::Identifier> {
        Some(ast::Identifier::new(
            self.curtoken.clone(),
            ast::ReturnValue::String(self.curtoken.retrieve_string().unwrap().to_string()),
        ))
    }

    fn parse_integer_literal(&mut self) -> Option<ast::IntegerLiteral> {
        match self.curtoken.retrieve_string()?.parse() {
            Ok(numb) => Some(ast::IntegerLiteral::new(
                self.curtoken.clone(),
                ast::ReturnValue::Int8(numb),
            )),
            Err(_) => {
                self.push_error("Unable to parse String".to_string());
                None
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::PrefixOperator> {
        let curr_tok = self.curtoken.clone();
        let operator = curr_tok.retrieve_string().unwrap();
        self.next_token();
        let right = self.parse_expresssion(PREFIX);
        Some(ast::PrefixOperator::new(
            curr_tok.clone(),
            operator.to_string(),
            right?,
        ))
    }

    fn cur_token_is(&self, tok: TokenType) -> bool {
        self.curtoken == tok
    }

    fn peek_error(&mut self, tok: TokenType) {
        let msg = format!(
            "Expected token to be {}, got {}",
            tok.retrieve_string().unwrap(),
            self.peektoken.retrieve_string().unwrap()
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
            true
        } else {
            self.peek_error(self.curtoken.clone());
            false
        }
    }

    fn get_errors(&self) -> &Vec<String> {
        self.errors
    }

    fn push_error(&mut self, value: String) {
        self.errors.push(value)
    }

    // helper function for parser to check whether or not the current token has the required
    // prefix/postfix method within it.
    fn registerprefix(&mut self) -> Option<Box<dyn Expression>> {
        match self.get_curtoken() {
            TokenType::Ident(Identifier::IDENT(_)) => {
                Some(Box::new(self.parse_identifier().unwrap()))
            }
            TokenType::Ident(Identifier::INT(_)) => {
                Some(Box::new(self.parse_integer_literal().unwrap()))
            }
            TokenType::Operator(Operator::BANG(_)) => {
                Some(Box::new(self.parse_prefix_expression().unwrap()))
            }
            TokenType::Operator(Operator::SUBTRACT(_)) => {
                Some(Box::new(self.parse_prefix_expression().unwrap()))
            }
            _ => None,
        }
        // if let &self.TokenType =  {
        //
        // }
    }

    fn register_postfix(&mut self) -> Option<Box<dyn Expression>> {
        match self.get_curtoken() {
            TokenType::Ident(Identifier::IDENT(_)) => {
                Some(Box::new(self.parse_identifier().unwrap()))
            }
            TokenType::Ident(Identifier::INT(_)) => {
                Some(Box::new(self.parse_integer_literal().unwrap()))
            }
            _ => None,
        }
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
    use crate::{
        ast::ast::{Identifier, *},
        token::{self},
    };

    #[test]
    fn test_let_statements_values() {
        let input = "let x = 5;
        let y = 10;
        let footbar = 6546;
        ";
        let mut lex = Lexer::new();

        lex.process_input(input);

        let mut errors = vec![];
        let mut parser = Parser::new(&mut lex, &mut errors);

        let prog = parser.parse_programme();

        let test_output = ["5", "10", "6546"];

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

        let mut errors = vec![];
        let mut parser = Parser::new(&mut lex, &mut errors);

        let prog = parser.parse_programme();

        check_parser_errors(&parser);

        let test_output = ["x", "y", "footbar"];

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

        let mut errors = vec![];
        let mut parser = Parser::new(&mut lex, &mut errors);

        let prog = parser.parse_programme();

        check_parser_errors(&parser);

        let test_output = ["x", "y", "footbar"];

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
        let mut errors = vec![];

        let mut parser = Parser::new(&mut lex, &mut errors);

        let prog = parser.parse_programme();

        check_parser_errors(&parser);

        let test_output = ["5", "10"];

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

        let mut errors = vec![];
        let mut parser = Parser::new(&mut lex, &mut errors);

        let prog = parser.parse_programme();

        check_parser_errors(&parser);

        let test_output = ["5", "10"];

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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let mut lexer: Lexer = Lexer::new();
        lexer.process_input(input);
        let mut errors = vec![];
        let mut parser: Parser = Parser::new(&mut lexer, &mut errors);
        let program = parser.parse_programme();
        check_parser_errors(&parser);
        let parsed_values = program.unwrap();
        // assert length of statements is 1
        assert_eq!(parsed_values.statements.len(), 1);
        // assert that returned value in statements is of type Expression Statement
        let expected = Box::new(ExpressionStatement::new(
            TokenType::Ident(token::token::Identifier::IDENT("foobar".to_string())),
            Box::new(ast::Identifier::new(
                TokenType::Ident(token::token::Identifier::IDENT("foobar".to_string())),
                ast::ReturnValue::String("foobar".to_string()),
            )),
        ));
        let response_val = &*parsed_values.statements[0];
        assert_eq!(expected.get_token(), response_val.get_token());
        assert_eq!(expected.get_value(), response_val.get_value());
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";
        let mut lexer: Lexer = Lexer::new();
        lexer.process_input(input);
        let mut errors = vec![];
        let mut parser: Parser = Parser::new(&mut lexer, &mut errors);
        let program = parser.parse_programme();
        check_parser_errors(&parser);
        let parsed_values = program.expect("Parser program should not panic and return None");
        let processed = &*parsed_values.statements[0];
        let expected = ExpressionStatement::new(
            TokenType::Ident(token::token::Identifier::INT("5".to_string())),
            Box::new(ast::Identifier::new(
                TokenType::Ident(token::token::Identifier::INT("5".to_string())),
                ReturnValue::Int8(5),
            )),
        );
        assert_eq!(processed.get_token(), expected.get_token());
        assert_eq!(processed.get_value(), expected.get_value());
    }

    #[test]
    fn test_prefix_operator() {
        struct Foo {
            input: String,
            operator: String,
            integer_value: i64,
        }
        let prefix: Vec<Foo> = vec![
            Foo {
                input: "!5;".to_string(),
                operator: "!".to_string(),
                integer_value: 5,
            },
            Foo {
                input: "-15;".to_string(),
                operator: "-".to_string(),
                integer_value: 15,
            },
        ];
        for i in prefix.iter() {
            println!("{:?}", &i.input);
            let mut lexer = Lexer::new();
            lexer.process_input(&i.input);
            let mut errors = vec![];
            let mut parser = Parser::new(&mut lexer, &mut errors);
            let result = parser.parse_programme().unwrap();
            check_parser_errors(&parser);
            let statements = result.statements;
            for m in statements {
                assert_eq!(i.operator.clone(), m.get_token().retrieve_string().unwrap());
                assert_eq!(ast::ReturnValue::Int8(i.integer_value), *m.get_value());
            }
        }
    }

    fn test_integer_literal(input: Box<dyn Statement>) -> Option<i64> {
        if let Some(concrete) = input.as_any().downcast_ref::<IntegerLiteral>() {
            match &concrete.value {
                ReturnValue::Int8(e) => Some(*e),
                ReturnValue::String(_) => None,
            }
        } else {
            None
        }
    }

    fn get_statement_identifiers(s: &dyn ast::Statement) -> String {
        s.get_identifier().unwrap().token_literal()
    }

    fn get_expression_value(s: &dyn ast::Expression) -> String {
        match s.get_value() {
            ReturnValue::String(e) => e.to_string(),
            ReturnValue::Int8(e) => e.to_string(),
        }
    }

    fn get_statement_value(s: &dyn ast::Statement) -> String {
        match s.get_value() {
            ReturnValue::String(e) => e.to_string(),
            ReturnValue::Int8(e) => e.to_string(),
        }
    }

    fn check_statement_type(s: &dyn ast::Statement) -> TokenType {
        s.get_token().clone()
    }

    fn check_parser_errors(p: &Parser) {
        let errors = p.get_errors();
        if errors.is_empty() {
        } else {
            for i in errors {
                println!("{}", i)
            }
            panic!();
        }
    }
}
