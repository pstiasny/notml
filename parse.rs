use std::io::prelude::*;
use std::fs::File;

#[derive(Debug,PartialEq)]
enum TokenClass {
    Number,
    Plus,
    Times,
    LParen,
    RParen,
    WS,
    EOF,
}

#[derive(Debug)]
struct Token<'a>(TokenClass, &'a str);

struct CodegenEnv();

#[derive(Debug)]
enum Expr {
    Number(i32),
    Plus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn number(i: i32) -> Expr {
        Expr::Number(i)
    }
    fn plus(l: Expr, r: Expr) -> Expr {
        Expr::Plus(Box::new(l), Box::new(r))
    }
    fn times(l: Expr, r: Expr) -> Expr {
        Expr::Times(Box::new(l), Box::new(r))
    }
}

fn lex(input: &str) -> Vec<Token> {
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
            ' ' => TokenClass::WS,
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

// Grammar:
// E -> S eof
// S -> T + T | T
// T -> ( S ) * T | ( S ) | number * T | number

type ParseResult<'a, T> = Result<(T, &'a[Token<'a>]), &'static str>;

fn token<'a>(ts: &'a [Token<'a>], expected_tc: TokenClass) -> ParseResult<'a, &'a str> {
    let Token(ref tc, tstr) = ts[0];
    if *tc == expected_tc {
        Ok((tstr, &ts[1..]))
    } else {
        Err("unexpected token")
    }
}

fn n<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (i_str, rest) = token(ts, TokenClass::Number)?;
    match i_str.parse::<i32>() {
        Ok(i) => Ok((Expr::number(i), rest)),
        Err(_) => Err("Invalid integer")
    }
}

fn t1<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (_, rest) = token(ts, TokenClass::LParen)?;
    let (expr1, rest) = s(rest)?;
    let (_, rest) = token(rest, TokenClass::RParen)?;
    let (_, rest) = token(rest, TokenClass::Times)?;
    let (expr2, rest) = t(rest)?;
    Ok((Expr::times(expr1, expr2), rest))
}

fn t2<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (_, rest) = token(ts, TokenClass::LParen)?;
    let (expr, rest) = s(rest)?;
    let (_, rest) = token(rest, TokenClass::RParen)?;
    Ok((expr, rest))
}

fn t3<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (expr1, rest) = n(ts)?;
    let (_, rest) = token(rest, TokenClass::Times)?;
    let (expr2, rest) = t(rest)?;
    Ok((Expr::times(expr1, expr2), rest))
}

fn t4<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    n(ts)
}

fn t<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    t1(ts)
        .or_else(|_| {t2(ts)})
        .or_else(|_| {t3(ts)})
        .or_else(|_| {t4(ts)})
        .or(Err("bad t"))
}

fn s1<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (expr1, rest) = t(ts)?;
    let (_, rest) = token(rest, TokenClass::Plus)?;
    let (expr2, rest) = t(rest)?;
    let expr = Expr::plus(expr1, expr2);
    Ok((expr, rest))
}

fn s2<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let expr = t(ts);
    expr
}

fn s<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    s1(ts)
        .or_else(|_| {s2(ts)})
        .or(Err("bad s"))
}

fn e<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (expr, rest) = s(ts)?;
    let (_, rest) = token(rest, TokenClass::EOF)?;
    Ok((expr, rest))
}

fn parse(ts: &[Token]) -> Result<Expr, &'static str> {
    e(ts).map(|(expr, _)| expr)
}

fn eval(e: &Expr) -> i32 {
    match *e {
        Expr::Number(i) => i,
        Expr::Plus(ref l, ref r) => eval(l) + eval(r),
        Expr::Times(ref l, ref r) => eval(l) * eval(r),
    }
}

fn write_amd64_prologue(w: &mut Write) -> std::io::Result<()> {
    w.write_all(b"
global _calcmain

section .text
_calcmain:
push rbp
mov rbp, rsp
    ")?;
    Ok(())
}

fn write_amd64_epilogue(w: &mut Write) -> std::io::Result<()> {
    w.write_all(b"
leave
ret
    ")?;
    Ok(())
}

fn write_amd64(e: &Expr, env: &CodegenEnv, w: &mut Write) -> std::io::Result<()> {
    match *e {
        Expr::Number(i) => {
            writeln!(w, "mov rax, {}", i)?;
            w.write_all(b"push rax\n")?;
        },
        Expr::Plus(ref l, ref r) => {
            write_amd64(l, env, w)?;
            write_amd64(r, env, w)?;
            w.write_all(b"pop rax\n")?;
            w.write_all(b"pop rdi\n")?;
            w.write_all(b"add rax, rdi\n")?;
            w.write_all(b"push rax\n")?;
        },
        Expr::Times(ref l, ref r) => {
            write_amd64(l, env, w)?;
            write_amd64(r, env, w)?;
            w.write_all(b"pop rax\n")?;
            w.write_all(b"pop rdi\n")?;
            w.write_all(b"mul rdi\n")?;
            w.write_all(b"push rax\n")?;
        }
    }
    Ok(())
}

fn main() -> std::io::Result<()> {
    let input = "(1 + 1) * 2 + 10";

    let mut tokenized_input = lex(input);
    tokenized_input.retain(|tok| tok.0 != TokenClass::WS);
    println!("Tokens: {:?}", tokenized_input);

    let res = parse(&tokenized_input);
    println!("Parse: {:?}", res);
    if let Ok(e) = res {
        println!("Eval: {}", eval(&e));

        let created = File::create("out.asm");
        match created {
            Ok(mut outfile) => {
                // TODO: better error handling
                write_amd64_prologue(&mut outfile)?;
                write_amd64(&e, &CodegenEnv(), &mut outfile)?;
                write_amd64_epilogue(&mut outfile)?;
            },
            _ => {
                println!("Could not create output file");
            }
        }
    }

    Ok(())

}
