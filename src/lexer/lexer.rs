use crate::token::token::{get_keywords, Delimiters, Identifier, Illegal, Operator, TokenType};

pub struct Lexer {
    pub input: String,
    pub position: usize,
    pub read_position: usize,
    pub ch: u8,
}

impl Lexer {
    fn read_char(&mut self) {
        if usize::from(self.read_position) >= self.input.len() {
            self.ch = 0
        } else {
            // this will only work if its ascii characters.
            // we format to bytes
            self.ch = self.input.bytes().nth(self.read_position.into()).unwrap()
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> TokenType {
        let tok: TokenType;

        self.skip_whitespace();
        println!("{:}", self.ch as char);
        match self.ch {
            b'=' => tok = TokenType::Operator(Operator::ASSIGN("=")),
            b'+' => tok = TokenType::Operator(Operator::PLUS("+")),
            b'(' => tok = TokenType::Del(Delimiters::LPAREN("(".to_string())),
            b')' => tok = TokenType::Del(Delimiters::RPAREN(")".to_string())),
            b';' => tok = TokenType::Del(Delimiters::SEMICOLON(";")),
            b'{' => tok = TokenType::Del(Delimiters::LBRACE("{".to_string())),
            b'}' => tok = TokenType::Del(Delimiters::RBRACE("}".to_string())),
            b',' => tok = TokenType::Del(Delimiters::COMMA(",".to_string())),
            b'-' => tok = TokenType::Operator(Operator::SUBTRACT("-")),
            b'!' => tok = TokenType::Operator(Operator::BANG("!")),
            b'/' => tok = TokenType::Operator(Operator::SLASH("/")),
            b'*' => tok = TokenType::Operator(Operator::ASTER("*")),
            b'<' => tok = TokenType::Operator(Operator::LTHAN("<")),
            b'>' => tok = TokenType::Operator(Operator::GTHAN(">")),

            // if value is a word and not an operator - then we need to move position up to next
            // non string value
            //
            _ => {
                // need to take the readidentifier word and if its alphabetic check if exists in
                // mapping hashmap
                if self.ch.is_ascii_alphabetic() {
                    let r = self.read_identifier();
                    let token_t = get_keywords(&r);
                    match token_t {
                        Some(x) => tok = TokenType::Keyword(x),
                        None => tok = TokenType::Ident(Identifier::IDENT(r.to_string())),
                    }
                    return tok;
                } else if self.ch.is_ascii_digit() {
                    tok = TokenType::Ident(Identifier::INT(self.read_identifier().to_string()));
                    return tok;
                } else {
                    println!("{:?}", self.ch as char);
                    tok = TokenType::Illegal(Illegal::ILLEGAL(self.ch.to_string()));
                }
            }
        };
        self.read_char();
        return tok;
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch.is_ascii_digit() {
            self.read_char();
        }

        &self.input[position..self.position]
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char()
        }
    }
}

pub fn new_lexer(input: String) -> Lexer {
    let mut lex = Lexer {
        input,
        position: 0,
        read_position: 0,
        ch: 0,
    };
    lex.read_char();
    return lex;
}

#[cfg(test)]
mod tests {
    use crate::token::token::{Identifier, Keywords};

    use super::*;

    #[test]
    fn build_user_test() {
        let user = new_lexer("abcd".to_string());
        assert_eq!(user.input, "abcd")
    }
    #[test]
    fn read_char_test() {
        let mut lex = new_lexer("abcd".to_string());
        lex.read_char();
        assert_eq!(lex.read_position, 2);
        assert_eq!(lex.ch, b'b');
    }
    #[test]
    fn next_token_test() {
        let input = "let five = 5; 
    let ten = 10;
    let add = fn(x,y) {
        x + y;
    };
    let result = add(five, ten);
    !-/*5;
    5<10>5;
    5!=10;
"
        .to_string();
        let mut lex = new_lexer(input);
        let v = vec![
            TokenType::Keyword(Keywords::LET("let")),
            TokenType::Ident(Identifier::IDENT("five".to_string())),
            TokenType::Operator(Operator::ASSIGN("=")),
            TokenType::Ident(Identifier::INT("5".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Keyword(Keywords::LET("let")),
            TokenType::Ident(Identifier::IDENT("ten".to_string())),
            TokenType::Operator(Operator::ASSIGN("=")),
            TokenType::Ident(Identifier::INT("10".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Keyword(Keywords::LET("let")),
            TokenType::Ident(Identifier::IDENT("add".to_string())),
            TokenType::Operator(Operator::ASSIGN("=")),
            TokenType::Keyword(Keywords::FUNCTION("fn")),
            TokenType::Del(Delimiters::LPAREN("(".to_string())),
            TokenType::Ident(Identifier::IDENT("x".to_string())),
            TokenType::Del(Delimiters::COMMA(",".to_string())),
            TokenType::Ident(Identifier::IDENT("y".to_string())),
            TokenType::Del(Delimiters::RPAREN(")".to_string())),
            TokenType::Del(Delimiters::LBRACE("{".to_string())),
            TokenType::Ident(Identifier::IDENT("x".to_string())),
            TokenType::Operator(Operator::PLUS("+")),
            TokenType::Ident(Identifier::IDENT("y".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Del(Delimiters::RBRACE("}".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Keyword(Keywords::LET("let")),
            TokenType::Ident(Identifier::IDENT("result".to_string())),
            TokenType::Operator(Operator::ASSIGN("=")),
            TokenType::Ident(Identifier::IDENT("add".to_string())),
            TokenType::Del(Delimiters::LPAREN("(".to_string())),
            TokenType::Ident(Identifier::IDENT("five".to_string())),
            TokenType::Del(Delimiters::COMMA(",".to_string())),
            TokenType::Ident(Identifier::IDENT("ten".to_string())),
            TokenType::Del(Delimiters::RPAREN(")".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Operator(Operator::BANG("!")),
            TokenType::Operator(Operator::SUBTRACT("-")),
            TokenType::Operator(Operator::SLASH("/")),
            TokenType::Operator(Operator::ASTER("*")),
            TokenType::Ident(Identifier::INT("5".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Ident(Identifier::INT("5".to_string())),
            TokenType::Operator(Operator::LTHAN("<")),
            TokenType::Ident(Identifier::INT("10".to_string())),
            TokenType::Operator(Operator::GTHAN(">")),
            TokenType::Ident(Identifier::INT("5".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Ident(Identifier::INT("5".to_string())),
            TokenType::Operator(Operator::BANG("!")),
            TokenType::Operator(Operator::ASSIGN("=")),
            TokenType::Ident(Identifier::INT("10".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
        ];
        for i in &v {
            let token = lex.next_token();
            assert_eq!(*i, token);
        }
    }
}
