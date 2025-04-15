use crate::token::token::{get_keywords, Delimiters, Identifier, Operator, TokenType};

pub struct Lexer<'a> {
    pub input: &'a str,
    pub position: usize,
    pub read_position: usize,
    pub ch: u8,
}

impl<'a> Lexer<'a> {
    fn read_char(&mut self) {
        self.ch = self.peekahead();
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> TokenType {
        let tok: TokenType;
        self.skip_whitespace();
        match self.ch {
            b'=' => tok = self.handle_eq_and_noeq(),
            b'+' => tok = TokenType::Operator(Operator::PLUS("+")),
            b'(' => tok = TokenType::Del(Delimiters::LPAREN("(")),
            b')' => tok = TokenType::Del(Delimiters::RPAREN(")")),
            b';' => tok = TokenType::Del(Delimiters::SEMICOLON(";")),
            b'{' => tok = TokenType::Del(Delimiters::LBRACE("{")),
            b'}' => tok = TokenType::Del(Delimiters::RBRACE("}")),
            b',' => tok = TokenType::Del(Delimiters::COMMA(",")),
            b'-' => tok = TokenType::Operator(Operator::SUBTRACT("-")),
            b'!' => tok = self.handle_eq_and_noeq(),
            b'/' => tok = TokenType::Operator(Operator::SLASH("/")),
            b'*' => tok = TokenType::Operator(Operator::ASTER("*")),
            b'<' => tok = TokenType::Operator(Operator::LTHAN("<")),
            b'>' => tok = TokenType::Operator(Operator::GTHAN(">")),
            b'\0' => tok = TokenType::EOF,

            // if value is a word and not an operator - then we need to move position up to next
            // non string value
            //
            _ => {
                // need to take the readidentifier word and if its alphabetic check if exists in
                // mapping hashmap
                if self.ch.is_ascii_alphabetic() {
                    let r = self.read_identifier();
                    let token_t = get_keywords(r);
                    match token_t {
                        Some(x) => tok = TokenType::Keyword(x),
                        None => tok = TokenType::Ident(Identifier::IDENT(r.to_string())),
                    }
                    return tok;
                } else if self.ch.is_ascii_digit() {
                    tok = TokenType::Ident(Identifier::INT(self.read_identifier().to_string()));

                    // if value at top of tokens stack is an assignment value - then we take the
                    // value at top of stack -2 as the identifier and assign our digit to this
                    // value in the hashmap

                    return tok;
                } else {
                    //println!("{:?}", self.ch as char);
                    tok = TokenType::Illegal
                }
            }
        };
        self.read_char();
        tok
    }

    fn peekahead(&mut self) -> u8 {
        // peek ahead at next character
        if self.read_position >= self.input.len() {
            0
        } else {
            // this will only work if its ascii characters.
            // we format to bytes
            self.input.as_bytes()[self.read_position]
        }
    }

    fn handle_eq_and_noeq(&mut self) -> TokenType {
        let peek = self.peekahead();
        let current_char = self.ch;
        if current_char == b'!' {
            if peek == b'=' {
                self.read_char();
                TokenType::Operator(Operator::NOEQUAL("!="))
            } else {
                TokenType::Operator(Operator::BANG("!"))
            }
        } else if peek == b'=' {
            self.read_char();
            TokenType::Operator(Operator::EQ("=="))
        } else {
            TokenType::Operator(Operator::ASSIGN("="))
        }
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
    pub fn new() -> Self {
        Self {
            input: "",
            position: 0,
            read_position: 0,
            ch: 0,
        }
    }
    pub fn process_input(&mut self, input: &'a str) {
        self.input = input;
        self.read_char();
    }
}

#[cfg(test)]
mod tests {
    use crate::token::token::{Identifier, Keywords};

    use super::*;

    #[test]
    fn build_user_test() {
        let mut lexer = Lexer::new();
        lexer.process_input("abcd");
        assert_eq!(lexer.input, "abcd")
    }
    #[test]
    fn read_char_test() {
        let mut lex = Lexer::new();
        lex.process_input("abcd");
        lex.read_char();
        assert_eq!(lex.read_position, 2);
        assert_eq!(lex.ch, b'b');
    }
    #[test]
    fn peak_ahead_test() {
        let mut lex = Lexer::new();
        lex.process_input("abcd");
        lex.read_char();
        assert_eq!(lex.peekahead() as char, b'c' as char)
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
    10==10;
    if (5 < 10) {
        return true; } 
        else {
        return false;
}
";
        let mut lex = Lexer::new();
        lex.process_input(input);
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
            TokenType::Del(Delimiters::LPAREN("(")),
            TokenType::Ident(Identifier::IDENT("x".to_string())),
            TokenType::Del(Delimiters::COMMA(",")),
            TokenType::Ident(Identifier::IDENT("y".to_string())),
            TokenType::Del(Delimiters::RPAREN(")")),
            TokenType::Del(Delimiters::LBRACE("{")),
            TokenType::Ident(Identifier::IDENT("x".to_string())),
            TokenType::Operator(Operator::PLUS("+")),
            TokenType::Ident(Identifier::IDENT("y".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Del(Delimiters::RBRACE("}")),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Keyword(Keywords::LET("let")),
            TokenType::Ident(Identifier::IDENT("result".to_string())),
            TokenType::Operator(Operator::ASSIGN("=")),
            TokenType::Ident(Identifier::IDENT("add".to_string())),
            TokenType::Del(Delimiters::LPAREN("(")),
            TokenType::Ident(Identifier::IDENT("five".to_string())),
            TokenType::Del(Delimiters::COMMA(",")),
            TokenType::Ident(Identifier::IDENT("ten".to_string())),
            TokenType::Del(Delimiters::RPAREN(")")),
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
            TokenType::Operator(Operator::NOEQUAL("!=")),
            TokenType::Ident(Identifier::INT("10".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Ident(Identifier::INT("10".to_string())),
            TokenType::Operator(Operator::EQ("==")),
            TokenType::Ident(Identifier::INT("10".to_string())),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Keyword(Keywords::IF("if")),
            TokenType::Del(Delimiters::LPAREN("(")),
            TokenType::Ident(Identifier::INT("5".to_string())),
            TokenType::Operator(Operator::LTHAN("<")),
            TokenType::Ident(Identifier::INT("10".to_string())),
            TokenType::Del(Delimiters::RPAREN(")")),
            TokenType::Del(Delimiters::LBRACE("{")),
            TokenType::Keyword(Keywords::RETURN("return")),
            TokenType::Keyword(Keywords::TRUE("true")),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Del(Delimiters::RBRACE("}")),
            TokenType::Keyword(Keywords::ELSE("else")),
            TokenType::Del(Delimiters::LBRACE("{")),
            TokenType::Keyword(Keywords::RETURN("return")),
            TokenType::Keyword(Keywords::FALSE("false")),
            TokenType::Del(Delimiters::SEMICOLON(";")),
            TokenType::Del(Delimiters::RBRACE("}")),
        ];
        for i in &v {
            let token = lex.next_token();
            assert_eq!(*i, token);
        }
    }
}
