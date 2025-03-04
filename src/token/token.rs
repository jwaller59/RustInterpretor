use phf::phf_map;

#[derive(Debug, PartialEq)]
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
