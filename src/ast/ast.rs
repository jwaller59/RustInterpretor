use std::{fmt::Debug, vec};

use crate::token::token::*;
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
    pub token: TokenType,
    pub name: Identifier,
    pub value: Box<dyn Node>,
}
impl Node for LetStatement {
    fn token_literal(&self) -> String {
        let val = self.token.retrieve_value();
        match val {
            Some(a) => return a.to_string(),
            _ => return "".to_string(),
        }
    }
}

impl LetStatement {
    pub fn new(token: TokenType, name: Identifier, value: Box<dyn Node>) -> Self {
        Self { token, name, value }
    }
}

impl Statement for LetStatement {
    fn get_value(&self) -> &Box<dyn Node> {
        &self.value
    }

    fn get_identifier(&self) -> &Identifier {
        &self.name
    }

    fn get_token(&self) -> &TokenType {
        &self.token
    }
}

#[derive(Debug)]
pub struct Identifier {
    token: TokenType,
    value: String,
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        let val = self.token.retrieve_value();
        match val {
            Some(a) => return a.to_string(),
            _ => return "".to_string(),
        }
    }
}

impl Identifier {
    pub fn new(token: TokenType, value: String) -> Self {
        Self { token, value }
    }
}

impl Expression for Identifier {}
impl Expression for AstNode {}

#[derive(Debug)]
pub struct AstNode {
    pub r#type: TokenType,
    pub value: String,
}

impl AstNode {
    pub fn new(t: TokenType, v: String) -> Self {
        Self {
            r#type: t,
            value: v,
        }
    }
}

pub trait Node: Debug {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn get_value(&self) -> &Box<dyn Node>;
    fn get_identifier(&self) -> &Identifier;
    fn get_token(&self) -> &TokenType;
}

pub trait Expression: Node {}

impl Node for AstNode {
    fn token_literal(&self) -> String {
        let val = self.r#type.retrieve_value();
        match val {
            Some(a) => return a.to_string(),
            _ => return "".to_string(),
        }
    }
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
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        } else {
            return "".to_string();
        }
    }
}
