#[derive(Debug,PartialEq)]
pub enum TokenClass {
    Number,
    Symbol,
    Assign,
    Plus,
    Times,
    LParen,
    RParen,
    Semicolon,
    WS,
    EOF,
}

#[derive(Debug,PartialEq)]
pub struct Token<'a>(pub TokenClass, pub &'a str);

pub fn lex(input: &str) -> Result<Vec<Token>, String> {
    let mut prev_tclass = TokenClass::WS;
    let mut i_start = 0;
    let mut tokenized_input = Vec::new();
    for (i, chr) in input.char_indices() {
        let tclass = match chr {
            '(' => TokenClass::LParen,
            ')' => TokenClass::RParen,
            '+' => TokenClass::Plus,
            '*' => TokenClass::Times,
            '0'..='9' => TokenClass::Number,
            'a'..='z' => TokenClass::Symbol,
            'A'..='Z' => TokenClass::Symbol,
            '_' => TokenClass::Symbol,
            '=' => TokenClass::Assign,
            ';' => TokenClass::Semicolon,
            ' ' => TokenClass::WS,
            '\n' => TokenClass::WS,
            _ => {
                return Err(format!("bad char {} at {}", chr, i));
            }
        };

        if (tclass != prev_tclass) && (i > 0) {
            tokenized_input.push(
                Token(prev_tclass, &input[i_start..i]));
            i_start = i;
        }

        prev_tclass = tclass;
    }
    tokenized_input.push(Token(prev_tclass, &input[i_start..]));
    tokenized_input.push(Token(TokenClass::EOF, ""));

    Ok(tokenized_input)
}

pub fn trim_ws(ts: &mut Vec<Token>) {
    ts.retain(|tok| tok.0 != TokenClass::WS);
}


#[cfg(test)]
mod test {
    use super::{Token, TokenClass, lex};

    #[test]
    fn sequence() {
        assert_eq!(lex("seq=(foo bar 1234+4 + 5*90);"), Ok(vec![
            Token(TokenClass::Symbol, "seq"),
            Token(TokenClass::Assign, "="),
            Token(TokenClass::LParen, "("),
            Token(TokenClass::Symbol, "foo"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Symbol, "bar"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Number, "1234"),
            Token(TokenClass::Plus, "+"),
            Token(TokenClass::Number, "4"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Plus, "+"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Number, "5"),
            Token(TokenClass::Times, "*"),
            Token(TokenClass::Number, "90"),
            Token(TokenClass::RParen, ")"),
            Token(TokenClass::Semicolon, ";"),
            Token(TokenClass::EOF, ""),
        ]));
    }

    #[test]
    fn weird() {
        assert_eq!(lex("***+++;;;"), Ok(vec![
            Token(TokenClass::Times, "***"),
            Token(TokenClass::Plus, "+++"),
            Token(TokenClass::Semicolon, ";;;"),
            Token(TokenClass::EOF, ""),
        ]));
    }
}
