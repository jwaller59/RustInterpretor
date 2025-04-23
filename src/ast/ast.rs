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
#[derive(Debug, PartialEq, Clone)]
pub enum ReturnValue {
    Int8(i64),
    String(String),
}
#[derive(Debug)]
pub struct LetStatement {
    pub token: token::TokenType,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}
impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.retrieve_string().to_string()
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
        stringbuff.push_str(self.token.retrieve_string());
        stringbuff.push(' ');
        let mut value = self.name.get_value();
        match value {
            ReturnValue::String(e) => stringbuff.push_str(e),
            ReturnValue::Int8(e) => stringbuff.push_str(&e.to_string()),
        }
        stringbuff.push(' ');
        stringbuff.push('=');
        stringbuff.push(' ');
        value = self.value.get_value();
        match value {
            ReturnValue::String(e) => stringbuff.push_str(e),
            ReturnValue::Int8(e) => stringbuff.push_str(&e.to_string()),
        }
        stringbuff.push(';');
        stringbuff
    }

    fn get_value(&self) -> &ReturnValue {
        self.value.get_value()
    }

    fn get_identifier(&self) -> Option<&Identifier> {
        Some(&self.name)
    }

    fn get_token(&self) -> &token::TokenType {
        &self.token
    }
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_expess(&self) -> &dyn Any {
        todo!()
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
        self.token.retrieve_string().to_string()
    }
}

impl Statement for ReturnStatement {
    fn generate_string(&self) -> String {
        let mut stringbuff = String::new();
        stringbuff.push_str(self.token.retrieve_string());
        stringbuff.push(' ');
        let value = self.get_value();
        match value {
            ReturnValue::String(e) => stringbuff.push_str(e),
            ReturnValue::Int8(e) => stringbuff.push_str(&e.to_string()),
        }
        stringbuff.push(';');
        stringbuff
    }

    fn get_token(&self) -> &TokenType {
        &self.token
    }

    fn get_value(&self) -> &ReturnValue {
        self.value.get_value()
    }

    fn get_identifier(&self) -> Option<&Identifier> {
        todo!()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_expess(&self) -> &dyn Any {
        todo!()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    token: token::TokenType,
    value: ReturnValue,
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.retrieve_string().to_string()
    }
}

impl Expression for Identifier {
    fn get_identifier(&self) -> Option<&Identifier> {
        None
    }
    fn get_value(&self) -> &ReturnValue {
        &self.value
    }
    fn get_token(&self) -> &token::TokenType {
        &self.token
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Identifier {
    pub fn new(token: token::TokenType, value: ReturnValue) -> Self {
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

    pub fn get_express(&self) -> &Box<dyn Expression> {
        &self.express
    }
}
impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.retrieve_string().to_string()
    }
}

impl Statement for ExpressionStatement {
    fn generate_string(&self) -> String {
        let mut string_buff = String::new();
        string_buff.push_str(self.token.retrieve_string());
        string_buff.push(' ');
        let current_value = self.express.get_value();
        match current_value {
            ReturnValue::String(e) => string_buff.push_str(e),
            ReturnValue::Int8(e) => string_buff.push_str(&e.to_string()),
        }
        // string_buff.push_str(self.express.get_value());
        string_buff
    }

    fn get_value(&self) -> &ReturnValue {
        self.express.get_value()
    }

    fn get_identifier(&self) -> Option<&Identifier> {
        None
    }

    fn get_token(&self) -> &token::TokenType {
        &self.token
    }
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_expess(&self) -> &dyn Any {
        self.get_express()
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
        self.r#type.retrieve_string().to_string()
    }
}

pub trait Node: Debug {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn generate_string(&self) -> String;

    fn get_value(&self) -> &ReturnValue;

    fn get_identifier(&self) -> Option<&Identifier>;

    fn get_token(&self) -> &token::TokenType;

    fn as_any(&self) -> &dyn Any;

    fn get_expess(&self) -> &dyn Any;
}

pub trait Expression: Node + ExpressionClone {
    fn get_value(&self) -> &ReturnValue;

    fn get_identifier(&self) -> Option<&Identifier>;

    fn get_token(&self) -> &token::TokenType;

    fn as_any(&self) -> &dyn Any;
}

pub trait ExpressionClone {
    fn clone_box(&self) -> Box<dyn Expression>;
}

impl<T> ExpressionClone for T
where
    T: Expression + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Expression> {
    fn clone(&self) -> Box<dyn Expression> {
        self.clone_box()
    }
}

#[derive(Debug)]
pub struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn get_statements(&self) -> &Vec<Box<dyn Statement>> {
        &self.statements
    }

    pub fn push_statement(&mut self, statement: Box<dyn Statement>) {
        self.statements.push(statement)
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

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    token: TokenType,
    pub value: ReturnValue,
}

impl IntegerLiteral {
    pub fn new(token: TokenType, value: ReturnValue) -> Self {
        Self { token, value }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        todo!()
    }
}

impl Expression for IntegerLiteral {
    fn get_value(&self) -> &ReturnValue {
        &self.value
    }

    fn get_identifier(&self) -> Option<&Identifier> {
        None
    }

    fn get_token(&self) -> &token::TokenType {
        &self.token
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    token: TokenType,
    operator: String,
    right: Box<dyn Expression>,
    left: Box<dyn Expression>,
}

impl InfixExpression {
    pub fn new(
        token: TokenType,
        operator: String,
        right: Box<dyn Expression>,
        left: Box<dyn Expression>,
    ) -> Self {
        Self {
            token,
            operator,
            right,
            left,
        }
    }

    pub fn get_left(&self) -> &ReturnValue {
        self.left.get_value()
    }

    pub fn get_right(&self) -> &ReturnValue {
        self.right.get_value()
    }

    pub fn get_operator(&self) -> &String {
        &self.operator
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        todo!()
    }
}

impl Expression for InfixExpression {
    fn get_value(&self) -> &ReturnValue {
        self.get_left()
    }

    fn get_identifier(&self) -> Option<&Identifier> {
        todo!()
    }

    fn get_token(&self) -> &token::TokenType {
        &self.token
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    token: TokenType,
    operator: String,
    right: Box<dyn Expression>,
}

impl PrefixExpression {
    pub fn new(token: TokenType, operator: String, right: Box<dyn Expression>) -> Self {
        Self {
            token,
            operator,
            right,
        }
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        todo!()
    }
}
impl Expression for PrefixExpression {
    fn get_value(&self) -> &ReturnValue {
        self.right.get_value()
    }

    fn get_identifier(&self) -> Option<&Identifier> {
        None
    }

    fn get_token(&self) -> &token::TokenType {
        &self.token
    }

    fn as_any(&self) -> &dyn Any {
        self
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
        let mut errors = vec![];
        lex.process_input(input);
        let mut parser: Parser = Parser::new(&mut lex, &mut errors);

        let prog = parser.parse_programme();
        let s: String = prog.unwrap().string();
        assert_eq!(s, input)
    }
    #[test]
    fn test_return_programme_print() {
        let input = "return x; return 5; return t;";
        let mut lex = Lexer::new();
        lex.process_input(input);
        let mut errors = vec![];
        let mut parser: Parser = Parser::new(&mut lex, &mut errors);
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
                    value: ReturnValue::String("a".to_string()),
                },
                value: Box::new(Identifier {
                    token: TokenType::Ident(token::Identifier::INT('1'.to_string())),
                    value: ReturnValue::String("1".to_string()),
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
                    value: ReturnValue::Int8(5),
                }),
            })],
        };
        let output = "return 5;";
        assert_eq!(output, program.string());
    }
}
