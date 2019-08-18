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

#[derive(Debug)]
pub struct Token<'a>(pub TokenClass, pub &'a str);

pub fn lex(input: &str) -> Vec<Token> {
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
                println!("bad char {} at {}", chr, i);
                TokenClass::EOF
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

    tokenized_input
}
