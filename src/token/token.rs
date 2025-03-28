use core::fmt;

use phf::phf_map;

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

#[derive(Debug, PartialEq, Clone)]
pub enum Identifier {
    IDENT(String),
    INT(String),
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
    pub fn retrieve_string(&self) -> Option<&str> {
        match self {
            Self::Ident(Identifier::IDENT(s)) => Some(s),
            Self::Ident(Identifier::INT(s)) => Some(s),
            Self::Keyword(Keywords::LET(s)) => Some(s),
            Self::Keyword(Keywords::TRUE(s)) => Some(s),
            Self::Keyword(Keywords::IF(s)) => Some(s),
            Self::Keyword(Keywords::ELSE(s)) => Some(s),
            Self::Keyword(Keywords::RETURN(s)) => Some(s),
            Self::Keyword(Keywords::FALSE(s)) => Some(s),
            Self::Keyword(Keywords::FUNCTION(s)) => Some(s),
            _ => None,
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_retrieve_value() {
        let tokenident = TokenType::Ident(Identifier::IDENT("orange".to_string()));
        assert_eq!(tokenident.retrieve_string().unwrap(), "orange")
    }
}
