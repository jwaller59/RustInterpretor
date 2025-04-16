use std::any::Any;
use std::fmt::Debug;

use token::TokenType;

use crate::token::*;
// AST takes token inputs and identifies and parses input information
// into a datastructure relating to the input.
// this data structure contains items on left and right side of operator
// as well as the consequence of the operation.
// this is created for the entire input information
//
// example
// right: { type: "integer-literal", value: 10},
// consequence : {
//      type: "return-literal",
//      returnValue: { type: "string-literal", value: "hello"}
//      },
//      alternative: {
//          type: "return statement",
//          returnValue: {
//              type: "string-literal",
//              value: "goodbye"}
//              }
//      }
//
// left and right values come from the decision operation either side of an operator
// e.g left items are identifiers and right items are assignments
//
//#[derive(Debug)]
#[derive(Debug)]
pub struct LetStatement {
    pub token: token::TokenType,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}
impl Node for LetStatement {
    fn token_literal(&self) -> String {
        let val = self.token.retrieve_string();
        match val {
            Some(a) => a.to_string(),
            _ => "".to_string(),
        }
    }
}

impl LetStatement {
    pub fn new(token: token::TokenType, name: Identifier, value: Box<dyn Expression>) -> Self {
        Self { token, name, value }
    }
}

impl Statement for LetStatement {
    fn generate_string(&self) -> String {
        let mut stringbuff = String::new();
        stringbuff.push_str(self.token.retrieve_string().unwrap());
        stringbuff.push(' ');
        stringbuff.push_str(self.name.get_value());
        stringbuff.push(' ');
        stringbuff.push('=');
        stringbuff.push(' ');
        stringbuff.push_str(self.value.get_value());
        stringbuff.push(';');
        stringbuff
    }

    fn get_value(&self) -> &str {
        self.value.get_value()
    }

    fn get_identifier(&self) -> Option<&Identifier> {
        Some(&self.name)
    }

    fn get_token(&self) -> &token::TokenType {
        &self.token
    }
    fn is_equal(&self, other: &dyn Expression) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<LetStatement>() {
            true
        } else {
            false
        }
    }

    fn partialEq(&self, other: &dyn Statement) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<LetStatement>() {
            let o = &*other.value;
            self.name == other.name && self.token == other.token && self.is_equal(o)
        } else {
            false
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    token: token::TokenType,
    value: Box<dyn Expression>,
}

impl ReturnStatement {
    pub fn new(token: TokenType, value: Box<dyn Expression>) -> Self {
        Self { token, value }
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        let val = self.token.retrieve_string();
        match val {
            Some(a) => a.to_string(),
            _ => "".to_string(),
        }
    }
}

impl Statement for ReturnStatement {
    fn generate_string(&self) -> String {
        let mut stringbuff = String::new();
        stringbuff.push_str(&self.token.retrieve_string().unwrap());
        stringbuff.push(' ');
        stringbuff.push_str(&self.value.get_value());
        stringbuff.push(';');
        stringbuff
    }

    fn get_token(&self) -> &TokenType {
        &self.token
    }

    fn get_value(&self) -> &str {
        self.value.get_value()
    }

    fn get_identifier(&self) -> Option<&Identifier> {
        todo!()
    }
    fn is_equal(&self, other: &dyn Expression) -> bool {
        todo!()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn partialEq(&self, other: &dyn Statement) -> bool {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    token: token::TokenType,
    value: String,
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        let val = self.token.retrieve_string();
        match val {
            Some(a) => a.to_string(),
            _ => "".to_string(),
        }
    }
}

impl Expression for Identifier {
    fn get_identifier(&self) -> Option<&Identifier> {
        None
    }
    fn get_value(&self) -> &str {
        &self.value
    }
    fn get_token(&self) -> &token::TokenType {
        &self.token
    }
    fn generate_string(&self) -> String {
        let mut stringbuff = String::new();
        stringbuff.push_str(self.get_value());
        stringbuff.push(';');
        stringbuff
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    // fn is_equal(&self, other: &dyn Expression) -> bool {
    //     if let Some(other) = other.as_any().downcast_ref::<Identifier>() {
    //         self == other
    //     } else {
    //         false
    //     }
    // }
}

impl Identifier {
    pub fn new(token: token::TokenType, value: String) -> Self {
        Self { token, value }
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    token: TokenType,
    express: Box<dyn Expression>,
}

impl ExpressionStatement {
    pub fn new(token: TokenType, express: Box<dyn Expression>) -> Self {
        Self { token, express }
    }
}
impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        let val = self.token.retrieve_string();
        match val {
            Some(a) => a.to_string(),
            _ => "".to_string(),
        }
    }
}

impl Statement for ExpressionStatement {
    fn generate_string(&self) -> String {
        let mut string_buff = String::new();
        string_buff.push_str(self.token.retrieve_string().unwrap());
        string_buff.push(' ');
        string_buff.push_str(self.express.get_value());
        string_buff
    }

    fn get_value(&self) -> &str {
        self.express.get_value()
    }

    fn get_identifier(&self) -> Option<&Identifier> {
        None
    }

    fn get_token(&self) -> &token::TokenType {
        &self.token
    }

    fn is_equal(&self, other: &dyn Expression) -> bool {
        // if *self.value matches!(other.as_any().downcast_ref::<Identifier()>)
        // matches!(other.as_any().downcast_ref::<Identifier>(), Some(other))
        true
    }

    fn partialEq(&self, other: &dyn Statement) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<LetStatement>() {
            let o = &*other.value;
            self.token == other.token && self.is_equal(o)
        } else {
            false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct AstNode {
    pub r#type: token::TokenType,
    pub value: String,
}

impl AstNode {
    pub fn new(t: token::TokenType, v: String) -> Self {
        Self {
            r#type: t,
            value: v,
        }
    }
}
impl Node for AstNode {
    fn token_literal(&self) -> String {
        let val = self.r#type.retrieve_string();
        match val {
            Some(a) => a.to_string(),
            _ => "".to_string(),
        }
    }
}

pub trait Node: Debug {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn partialEq(&self, other: &dyn Statement) -> bool;
    fn generate_string(&self) -> String;

    fn get_value(&self) -> &str;

    fn get_identifier(&self) -> Option<&Identifier>;

    fn get_token(&self) -> &token::TokenType;

    fn is_equal(&self, other: &dyn Expression) -> bool;
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    fn get_value(&self) -> &str;

    fn get_identifier(&self) -> Option<&Identifier>;

    fn get_token(&self) -> &token::TokenType;

    fn generate_string(&self) -> String {
        let mut string_buff = String::new();
        string_buff.push_str(self.get_token().retrieve_string().unwrap());
        string_buff.push(' ');
        string_buff.push_str(self.get_value());
        string_buff
    }
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn string(&self) -> String {
        // iterate through statements and add each string value to buffer to be read
        // out
        let mut strbuff = String::new();
        for (i, n) in self.statements.iter().enumerate() {
            strbuff.push_str(&n.generate_string());
            if i != self.statements.len() - 1 {
                strbuff.push(' ');
            }
        }
        strbuff
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::Lexer;
    use crate::parser::parser::Parser;

    #[test]
    fn test_let_programme_print() {
        let input = "let x = 5; let y = 10; let footbar = 6546;";
        let mut lex = Lexer::new();
        lex.process_input(input);
        let mut parser: Parser = Parser::new(&mut lex);

        let prog = parser.parse_programme();
        let s: String = prog.unwrap().string();
        assert_eq!(s, input)
    }
    #[test]
    fn test_return_programme_print() {
        let input = "return x; return 5; return t;";
        let mut lex = Lexer::new();
        lex.process_input(input);
        let mut parser: Parser = Parser::new(&mut lex);
        let prog = parser.parse_programme();
        let s: String = prog.unwrap().string();
        assert_eq!(s, input)
    }

    #[test]
    fn test_let_programme_rebuild_print() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: TokenType::Keyword(token::Keywords::LET("let")),
                name: Identifier {
                    token: TokenType::Ident(token::Identifier::IDENT("a".to_string())),
                    value: "a".to_string(),
                },
                value: Box::new(Identifier {
                    token: TokenType::Ident(token::Identifier::INT('1'.to_string())),
                    value: "1".to_string(),
                }),
            })],
        };

        let output = "let a = 1;";
        assert_eq!(program.string(), output);
    }

    #[test]
    fn test_return_programme_build_print() {
        let program = Program {
            statements: vec![Box::new(ReturnStatement {
                token: TokenType::Keyword(token::Keywords::RETURN("return")),
                value: Box::new(Identifier {
                    token: TokenType::Ident(token::Identifier::INT("5".to_string())),
                    value: "5".to_string(),
                }),
            })],
        };
        let output = "return 5;";
        assert_eq!(output, program.string());
    }
}
