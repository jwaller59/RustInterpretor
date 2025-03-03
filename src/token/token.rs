use phf::phf_map;

#[derive(Debug, PartialEq)]
pub enum Operator {
    ASSIGN(String),
    PLUS(String),
    SUBTRACT(String),
}

#[derive(Debug, PartialEq)]
pub enum Identifier {
    IDENT(String),
    INT(String),
}

#[derive(Debug, PartialEq)]
pub enum Illegal {
    ILLEGAL(String),
}

#[derive(Debug, PartialEq)]
pub enum EOF {
    EOF(String),
}

#[derive(Debug, PartialEq)]
pub enum Delimiters {
    COMMA(String),
    SEMICOLON(String),
    LPAREN(String),
    RPAREN(String),
    LBRACE(String),
    RBRACE(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keywords {
    FUNCTION(&'static str),
    LET(&'static str),
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Operator(Operator),
    Ident(Identifier),
    Illegal(Illegal),
    EOF(EOF),
    Del(Delimiters),
    Keyword(Keywords),
}

static KEYWORDS: phf::Map<&'static str, Keywords> = phf_map! {
    "let" => Keywords::LET("let"),
    "fn" => Keywords::FUNCTION("fn")

};

pub fn get_keywords(keyword: &str) -> Option<Keywords> {
    KEYWORDS.get(keyword).cloned()
}
