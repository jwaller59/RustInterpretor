use core::fmt;

use phf::phf_map;

const LOWEST: i8 = 1;
const EQUALS: i8 = 2;
const LESSGREATER: i8 = 3;
const SUM: i8 = 4;
const PRODUCT: i8 = 5;
const PREFIX: i8 = 6;
const CALL: i8 = 7;

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    ASSIGN(&'static str),
    PLUS(&'static str),
    SUBTRACT(&'static str),
    BANG(&'static str),
    NOEQUAL(&'static str),
    SLASH(&'static str),
    ASTER(&'static str),
    LTHAN(&'static str),
    GTHAN(&'static str),
    EQ(&'static str),
}

impl Operator {
    pub fn precedence(&self) -> i8 {
        match self {
            Self::EQ(_) => EQUALS,
            Self::NOEQUAL(_) => EQUALS,
            Self::LTHAN(_) => LESSGREATER,
            Self::GTHAN(_) => LESSGREATER,
            Self::PLUS(_) => SUM,
            Self::SUBTRACT(_) => SUM,
            Self::SLASH(_) => PRODUCT,
            Self::ASTER(_) => PRODUCT,
            _ => LOWEST,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Identifier {
    IDENT(String),
    INT(String),
}

impl Identifier {
    pub fn prefix_parser(&self) -> bool {
        true
    }
    pub fn postfix_parser(&self) -> bool {
        false
    }
}

//#[derive(Debug, PartialEq)]
//pub enum Illegal {
//    ILLEGAL(String),
//}
//
//#[derive(Debug, PartialEq)]
//pub enum EOF {
//    EOF,
//}

#[derive(Debug, PartialEq, Clone)]
pub enum Delimiters {
    COMMA(&'static str),
    SEMICOLON(&'static str),
    LPAREN(&'static str),
    RPAREN(&'static str),
    LBRACE(&'static str),
    RBRACE(&'static str),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keywords {
    FUNCTION(&'static str),
    LET(&'static str),
    TRUE(&'static str),
    FALSE(&'static str),
    RETURN(&'static str),
    IF(&'static str),
    ELSE(&'static str),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Operator(Operator),
    Ident(Identifier),
    Illegal,
    EOF,
    Del(Delimiters),
    Keyword(Keywords),
}

static KEYWORDS: phf::Map<&'static str, Keywords> = phf_map! {
    "let" => Keywords::LET("let"),
    "fn" => Keywords::FUNCTION("fn"),
    "true" => Keywords::TRUE("true"),
    "false" => Keywords::FALSE("false"),
    "return" =>Keywords::RETURN("return"),
    "if" => Keywords::IF("if"),
    "else" => Keywords::ELSE("else"),
};

pub fn get_keywords(keyword: &str) -> Option<Keywords> {
    KEYWORDS.get(keyword).cloned()
}

impl TokenType {
    pub fn retrieve_string(&self) -> &str {
        match self {
            Self::Ident(Identifier::IDENT(ref s)) => s,
            Self::Ident(Identifier::INT(ref s)) => s,
            Self::Keyword(Keywords::LET(s)) => s,
            Self::Keyword(Keywords::TRUE(s)) => s,
            Self::Keyword(Keywords::IF(s)) => s,
            Self::Keyword(Keywords::ELSE(s)) => s,
            Self::Keyword(Keywords::RETURN(s)) => s,
            Self::Keyword(Keywords::FALSE(s)) => s,
            Self::Keyword(Keywords::FUNCTION(s)) => s,
            Self::Operator(Operator::BANG(s)) => s,
            Self::Operator(Operator::SUBTRACT(s)) => s,
            TokenType::Operator(_) => todo!(),
            TokenType::Illegal => todo!(),
            TokenType::EOF => todo!(),
            TokenType::Del(_) => todo!(),
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self)
    }
}

// impl From<Box<dyn Expression>> for TokenType {
//     fn from(stmt: Box<dyn Expression>) -> Self {
//         if let Some(operator) = stmt.as_any().downcast_ref::<Operator>() {
//             TokenType::Operator(operator.clone())
//         } else if let Some(ident) = stmt.as_any().downcast_ref::<Identifier>() {
//             TokenType::Ident(ident.clone())
//         } else if let Some(del) = stmt.as_any().downcast_ref::<Delimiters>() {
//             TokenType::Del(del.clone())
//         } else if let Some(key) = stmt.as_any().downcast_ref::<Keywords>() {
//             TokenType::Keyword(key.clone())
//         } else {
//             panic!()
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_retrieve_value() {
        let tokenident = TokenType::Ident(Identifier::IDENT("orange".to_string()));
        assert_eq!(tokenident.retrieve_string(), "orange")
    }
}
