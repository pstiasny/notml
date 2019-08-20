#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub enum TokenClass {
    Number,
    Symbol,
    Assign,
    Plus,
    Minus,
    Times,
    LParen,
    RParen,
    If,
    Then,
    Else,
    Semicolon,
    WS,
    EOF,
}

#[derive(Debug, PartialEq)]
pub struct Token<'a>(pub TokenClass, pub &'a str);

fn transitions(state: u8, chr: Option<char>) -> &'static [u8] {
    match chr {
        None => match state {
            0 => &[3],
            _ => &[],
        }

        Some(c) => match state {
            0 => match c {
                'i' => &[100, 11],
                't' => &[100, 13],
                'e' => &[100, 17],
                'a'..='z'| 'A'..='Z' | '_' => &[100],
                ' ' | '\n' => &[2],
                '=' => &[4],
                '(' => &[5],
                ')' => &[6],
                '+' => &[7],
                '-' => &[21],
                '*' => &[8],
                '0'..='9' => &[9],
                ';' => &[10],
                _ => &[],
            },

            2 => match c {
                ' ' | '\n' => &[2],
                _ => &[],
            }

            9 => match c { '0'..='9' => &[9], _ => &[], }

            11 => match c { 'f' => &[12], _ => &[], }

            13 => match c { 'h' => &[14], _ => &[], }
            14 => match c { 'e' => &[15], _ => &[], }
            15 => match c { 'n' => &[16], _ => &[], }

            17 => match c { 'l' => &[18], _ => &[], }
            18 => match c { 's' => &[19], _ => &[], }
            19 => match c { 'e' => &[20], _ => &[], }

            100 => match c {
                'a'..='z'| 'A'..='Z' | '_' => &[100],
                _ => &[],
            }

            _ => &[]
        }
    }
}

fn accepting(state: u8) -> &'static Option<TokenClass> {
    match state {
        0 => &None,
        2 => &Some(TokenClass::WS),
        3 => &Some(TokenClass::EOF),
        4 => &Some(TokenClass::Assign),
        5 => &Some(TokenClass::LParen),
        6 => &Some(TokenClass::RParen),
        7 => &Some(TokenClass::Plus),
        8 => &Some(TokenClass::Times),
        9 => &Some(TokenClass::Number),
        10 => &Some(TokenClass::Semicolon),
        12 => &Some(TokenClass::If),
        16 => &Some(TokenClass::Then),
        20 => &Some(TokenClass::Else),
        21 => &Some(TokenClass::Minus),
        100 => &Some(TokenClass::Symbol),
        _ => &None,
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, String> {
    let mut tokenized_input = Vec::new();
    let mut ac: Vec<TokenClass> = Vec::new();
    let mut states: Vec<u8> = vec![0];
    let mut i = input.chars();

    let mut chr = i.next();
    let mut pos_start = 0;
    let mut pos = 0;

    loop {
        states = states.iter()
            .flat_map(|&state| transitions(state, chr).iter())
            .cloned()
            .collect();
        states.sort();
        states.dedup();

        if states.is_empty() {
            if let Some(tc) = ac.iter().cloned().next() {
                if tc == TokenClass::EOF {
                    tokenized_input.push(Token(tc, ""));
                    return Ok(tokenized_input)
                }

                tokenized_input.push(Token(tc, &input[pos_start..pos]));

                ac.clear();
                states.clear();
                states.push(0);
                pos_start = pos;
            } else {
                return Err(format!("unexpected character {:?} at pos {}", chr, pos));
            }
        } else {
            ac = states.iter()
                .flat_map(|&state| accepting(state).iter())
                .cloned()
                .collect();
            chr = i.next();
            pos += 1;
        }
    }
}

pub fn trim_ws(ts: &mut Vec<Token>) {
    ts.retain(|tok| tok.0 != TokenClass::WS);
}


#[cfg(test)]
mod test {
    use super::{Token, TokenClass, lex};

    #[test]
    fn sequence() {
        assert_eq!(lex("seq=(foo bar 1234+4 +- 5*90);"), Ok(vec![
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
            Token(TokenClass::Minus, "-"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Number, "5"),
            Token(TokenClass::Times, "*"),
            Token(TokenClass::Number, "90"),
            Token(TokenClass::RParen, ")"),
            Token(TokenClass::Semicolon, ";"),
            Token(TokenClass::EOF, ""),
        ]));

        assert_eq!(lex("if iff then thenn else elsee"), Ok(vec![
            Token(TokenClass::If, "if"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Symbol, "iff"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Then, "then"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Symbol, "thenn"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Else, "else"),
            Token(TokenClass::WS, " "),
            Token(TokenClass::Symbol, "elsee"),
            Token(TokenClass::EOF, ""),
        ]));
    }

    #[test]
    fn weird() {
        assert_eq!(lex("***+++;;;"), Ok(vec![
            Token(TokenClass::Times, "*"),
            Token(TokenClass::Times, "*"),
            Token(TokenClass::Times, "*"),
            Token(TokenClass::Plus, "+"),
            Token(TokenClass::Plus, "+"),
            Token(TokenClass::Plus, "+"),
            Token(TokenClass::Semicolon, ";"),
            Token(TokenClass::Semicolon, ";"),
            Token(TokenClass::Semicolon, ";"),
            Token(TokenClass::EOF, ""),
        ]));
    }
}
