#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub enum TokenClass {
    Number,
    Symbol,
    TypeName,
    Assign,
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    Eq,
    Gt,
    Gte,
    Lt,
    Lte,
    And,
    Or,
    LParen,
    RParen,
    If,
    Then,
    Else,
    Do,
    End,
    Colon,
    Semicolon,
    Arrow,
    WS,
    EOF,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
pub struct Position(pub u64, pub u64);

#[derive(Debug, PartialEq)]
pub struct Token<'a>(pub TokenClass, pub &'a str, pub Position);

fn transitions(state: u8, chr: Option<char>) -> &'static [u8] {
    match chr {
        None => match state {
            0 => &[3],
            _ => &[],
        }

        Some(c) => match state {
            0 => match c {
                'a' => &[100, 43],
                'o' => &[100, 44],
                'i' => &[100, 11],
                't' => &[100, 13],
                'e' => &[100, 17, 24],
                'd' => &[100, 22],
                'm' => &[100, 40],
                'b'..='z'| '_' => &[100],
                'A'..='Z' => &[28],
                ' ' | '\n' => &[2],
                '=' => &[4, 31],
                '>' => &[33, 34],
                '<' => &[35, 36],
                '(' => &[5],
                ')' => &[6],
                '+' => &[7],
                '-' => &[21, 29],
                '*' => &[8],
                '/' => &[39],
                '0'..='9' => &[9],
                ':' => &[27],
                ';' => &[10],
                _ => &[],
            },

            2 => match c {
                ' ' | '\n' => &[2],
                _ => &[],
            }

            29 => match c { '>' => &[30], _ => &[], }
            31 => match c { '=' => &[32], _ => &[], }
            34 => match c { '=' => &[37], _ => &[], }
            36 => match c { '=' => &[38], _ => &[], }

            9 => match c { '0'..='9' => &[9], _ => &[], }

            11 => match c { 'f' => &[12], _ => &[], }

            13 => match c { 'h' => &[14], _ => &[], }
            14 => match c { 'e' => &[15], _ => &[], }
            15 => match c { 'n' => &[16], _ => &[], }

            17 => match c { 'l' => &[18], _ => &[], }
            18 => match c { 's' => &[19], _ => &[], }
            19 => match c { 'e' => &[20], _ => &[], }

            22 => match c { 'o' => &[23], _ => &[], }

            24 => match c { 'n' => &[25], _ => &[], }
            25 => match c { 'd' => &[26], _ => &[], }

            40 => match c { 'o' => &[41], _ => &[], }
            41 => match c { 'd' => &[42], _ => &[], }

            43 => match c { 'n' => &[45], _ => &[], }
            45 => match c { 'd' => &[46], _ => &[], }

            44 => match c { 'r' => &[47], _ => &[], }

            28 => match c {
                'a'..='z'| 'A'..='Z' | '0'..='9' | '_' => &[28],
                _ => &[],
            }
            100 => match c {
                'a'..='z'| 'A'..='Z' | '0'..='9' | '_' => &[100],
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
        23 => &Some(TokenClass::Do),
        26 => &Some(TokenClass::End),
        27 => &Some(TokenClass::Colon),
        28 => &Some(TokenClass::TypeName),
        30 => &Some(TokenClass::Arrow),
        32 => &Some(TokenClass::Eq),
        33 => &Some(TokenClass::Gt),
        35 => &Some(TokenClass::Lt),
        37 => &Some(TokenClass::Gte),
        38 => &Some(TokenClass::Lte),
        39 => &Some(TokenClass::Div),
        42 => &Some(TokenClass::Mod),
        46 => &Some(TokenClass::And),
        47 => &Some(TokenClass::Or),
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

    let mut col = 1;
    let mut col_start = 1;
    let mut line = 1;
    let mut line_start = 1;
    let mut linebreak = false;

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
                    tokenized_input.push(Token(tc, "", Position(line_start, col_start)));
                    return Ok(tokenized_input)
                }

                tokenized_input.push(Token(tc, &input[pos_start..pos], Position(line_start, col_start)));

                ac.clear();
                states.clear();
                states.push(0);
                pos_start = pos;
                line_start = line;
                col_start = col;
            } else {
                return Err(format!("unexpected character {:?} at line {} col {}", chr, line, col));
            }
        } else {
            ac = states.iter()
                .flat_map(|&state| accepting(state).iter())
                .cloned()
                .collect();
            chr = i.next();
            pos += 1;
            if linebreak {
                col = 1;
                line += 1;
            } else {
                col += 1;
            }
            linebreak = chr == Some('\n');
        }
    }
}

pub fn trim_ws(ts: &mut Vec<Token>) {
    ts.retain(|tok| tok.0 != TokenClass::WS);
}


#[cfg(test)]
mod test {
    use super::{Token, TokenClass, Position, lex};

    #[test]
    fn sequence() {
        assert_eq!(lex("seq=(foo bar x1  1234+4 +- 5*90/1 mod 9);"), Ok(vec![
            Token(TokenClass::Symbol,   "seq",  Position(1, 1)),
            Token(TokenClass::Assign,   "=",    Position(1, 4)),
            Token(TokenClass::LParen,   "(",    Position(1, 5)),
            Token(TokenClass::Symbol,   "foo",  Position(1, 6)),
            Token(TokenClass::WS,       " ",    Position(1, 9)),
            Token(TokenClass::Symbol,   "bar",  Position(1, 10)),
            Token(TokenClass::WS,       " ",    Position(1, 13)),
            Token(TokenClass::Symbol,   "x1",   Position(1, 14)),
            Token(TokenClass::WS,       "  ",   Position(1, 16)),
            Token(TokenClass::Number,   "1234", Position(1, 18)),
            Token(TokenClass::Plus,     "+",    Position(1, 22)),
            Token(TokenClass::Number,   "4",    Position(1, 23)),
            Token(TokenClass::WS,       " ",    Position(1, 24)),
            Token(TokenClass::Plus,     "+",    Position(1, 25)),
            Token(TokenClass::Minus,    "-",    Position(1, 26)),
            Token(TokenClass::WS,       " ",    Position(1, 27)),
            Token(TokenClass::Number,   "5",    Position(1, 28)),
            Token(TokenClass::Times,    "*",    Position(1, 29)),
            Token(TokenClass::Number,   "90",   Position(1, 30)),
            Token(TokenClass::Div,      "/",    Position(1, 32)),
            Token(TokenClass::Number,   "1",    Position(1, 33)),
            Token(TokenClass::WS,       " ",    Position(1, 34)),
            Token(TokenClass::Mod,      "mod",  Position(1, 35)),
            Token(TokenClass::WS,       " ",    Position(1, 38)),
            Token(TokenClass::Number,   "9",    Position(1, 39)),
            Token(TokenClass::RParen,   ")",    Position(1, 40)),
            Token(TokenClass::Semicolon, ";",   Position(1, 41)),
            Token(TokenClass::EOF,       "",    Position(1, 42)),
        ]));
    }

    #[test]
    fn cond() {
        assert_eq!(lex("if iff then thenn else elsee do end"), Ok(vec![
            Token(TokenClass::If,     "if",    Position(1, 1)),
            Token(TokenClass::WS,     " ",     Position(1, 3)),
            Token(TokenClass::Symbol, "iff",   Position(1, 4)),
            Token(TokenClass::WS,     " ",     Position(1, 7)),
            Token(TokenClass::Then,   "then",  Position(1, 8)),
            Token(TokenClass::WS,     " ",     Position(1, 12)),
            Token(TokenClass::Symbol, "thenn", Position(1, 13)),
            Token(TokenClass::WS,     " ",     Position(1, 18)),
            Token(TokenClass::Else,   "else",  Position(1, 19)),
            Token(TokenClass::WS,     " ",     Position(1, 23)),
            Token(TokenClass::Symbol, "elsee", Position(1, 24)),
            Token(TokenClass::WS,     " ",     Position(1, 29)),
            Token(TokenClass::Do,     "do",    Position(1, 30)),
            Token(TokenClass::WS,     " ",     Position(1, 32)),
            Token(TokenClass::End,    "end",   Position(1, 33)),
            Token(TokenClass::EOF,    "",      Position(1, 36)),
        ]));
    }

    #[test]
    fn binop() {
        assert_eq!(lex("= ==<> <= >= and or"), Ok(vec![
            Token(TokenClass::Assign, "=",     Position(1, 1)),
            Token(TokenClass::WS,     " ",     Position(1, 2)),
            Token(TokenClass::Eq,     "==",    Position(1, 3)),
            Token(TokenClass::Lt,     "<",     Position(1, 5)),
            Token(TokenClass::Gt,     ">",     Position(1, 6)),
            Token(TokenClass::WS,     " ",     Position(1, 7)),
            Token(TokenClass::Lte,    "<=",    Position(1, 8)),
            Token(TokenClass::WS,     " ",     Position(1, 10)),
            Token(TokenClass::Gte,    ">=",    Position(1, 11)),
            Token(TokenClass::WS,     " ",     Position(1, 13)),
            Token(TokenClass::And,    "and",   Position(1, 14)),
            Token(TokenClass::WS,     " ",     Position(1, 17)),
            Token(TokenClass::Or,     "or",    Position(1, 18)),
            Token(TokenClass::EOF,    "",      Position(1, 20)),
        ]));
    }

    #[test]
    fn repeated_binops() {
        assert_eq!(lex("***+++;;;"), Ok(vec![
            Token(TokenClass::Times,     "*", Position(1, 1)),
            Token(TokenClass::Times,     "*", Position(1, 2)),
            Token(TokenClass::Times,     "*", Position(1, 3)),
            Token(TokenClass::Plus,      "+", Position(1, 4)),
            Token(TokenClass::Plus,      "+", Position(1, 5)),
            Token(TokenClass::Plus,      "+", Position(1, 6)),
            Token(TokenClass::Semicolon, ";", Position(1, 7)),
            Token(TokenClass::Semicolon, ";", Position(1, 8)),
            Token(TokenClass::Semicolon, ";", Position(1, 9)),
            Token(TokenClass::EOF,       "",  Position(1, 10)),
        ]));
    }

    #[test]
    fn newlines() {
        assert_eq!(lex("a=1;\nb=2;\n"), Ok(vec![
            Token(TokenClass::Symbol,    "a",  Position(1, 1)),
            Token(TokenClass::Assign,    "=",  Position(1, 2)),
            Token(TokenClass::Number,    "1",  Position(1, 3)),
            Token(TokenClass::Semicolon, ";",  Position(1, 4)),
            Token(TokenClass::WS,        "\n", Position(1, 5)),
            Token(TokenClass::Symbol,    "b",  Position(2, 1)),
            Token(TokenClass::Assign,    "=",  Position(2, 2)),
            Token(TokenClass::Number,    "2",  Position(2, 3)),
            Token(TokenClass::Semicolon, ";",  Position(2, 4)),
            Token(TokenClass::WS,        "\n", Position(2, 5)),
            Token(TokenClass::EOF,       "",   Position(3, 1)),
        ]));
    }

    #[test]
    fn typesig() {
        assert_eq!(lex("abc: ABc->D"), Ok(vec![
            Token(TokenClass::Symbol,    "abc",  Position(1, 1)),
            Token(TokenClass::Colon,     ":",    Position(1, 4)),
            Token(TokenClass::WS,        " ",    Position(1, 5)),
            Token(TokenClass::TypeName,  "ABc",  Position(1, 6)),
            Token(TokenClass::Arrow,     "->",   Position(1, 9)),
            Token(TokenClass::TypeName,  "D",    Position(1, 11)),
            Token(TokenClass::EOF,       "",     Position(1, 12)),
        ]));
    }
}
