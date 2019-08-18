use crate::lexer::*;
use crate::ast::*;

// Grammar:
// E -> D ; E | eof
// D -> sym sym* = S
// S -> T + T | T
// T -> ( S ) * T | ( S ) | U * T | U
// U -> number | C
// C -> sym S*

type ParseResult<'a, T> = Result<(T, &'a[Token<'a>]), &'static str>;

fn token<'a>(ts: &'a [Token<'a>], expected_tc: TokenClass) -> ParseResult<'a, &'a str> {
    let Token(ref tc, tstr) = ts[0];
    if *tc == expected_tc {
        Ok((tstr, &ts[1..]))
    } else {
        Err("unexpected token")
    }
}

fn many<'a, T>(
    f: &Fn(&'a [Token<'a>]) -> ParseResult<'a, T>,
    ts: &'a [Token<'a>]
) -> ParseResult<'a, Vec<T>> {
    let mut results = Vec::<T>::new();
    let mut rest = ts;

    loop {
        let result = f(rest);
        match result {
            Ok((result_value, rest_)) => {
                results.push(result_value);
                rest = rest_;
            },
            Err(_) => break
        }
    }

    Ok((results, rest))
}

fn n<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (i_str, rest) = token(ts, TokenClass::Number)?;
    match i_str.parse::<i32>() {
        Ok(i) => Ok((Expr::number(i), rest)),
        Err(_) => Err("Invalid integer")
    }
}

fn c<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (fname, rest) = token(ts, TokenClass::Symbol)?;
    let (args, rest) = many(&s, rest)?;
    if args.len() == 0 {
        Ok((Expr::var(fname.to_string()), rest))
    } else {
        Ok((Expr::call(fname.to_string(), args), rest))
    }
}

fn u<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    n(ts)
        .or_else(|_| {c(ts)})
        .or(Err("bad u"))
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
    let (expr1, rest) = u(ts)?;
    let (_, rest) = token(rest, TokenClass::Times)?;
    let (expr2, rest) = t(rest)?;
    Ok((Expr::times(expr1, expr2), rest))
}

fn t4<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    u(ts)
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

fn d<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, FunDefinition> {
    let (sym, rest) = token(ts, TokenClass::Symbol)?;

    let (args, rest) = many(&|ts| {
        let (sym, rest) = token(ts, TokenClass::Symbol)?;
        Ok((sym.to_string(), rest))
    }, rest)?;

    let (_, rest) = token(rest, TokenClass::Assign)?;
    let (expr, rest) = s(rest)?;

    let fd = FunDefinition {
        fname: sym.to_string(),
        arg_names: args,
        code: Box::new(expr),
    };
    Ok((fd, rest))
}

fn e<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Program> {
    e1(ts)
        .or_else(|_| {e2(ts)})
        .or(Err("bad e"))
}

fn e1<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Program> {
    let (expr, rest) = d(ts)?;
    let (_, rest) = token(rest, TokenClass::Semicolon)?;
    let (mut p, rest) = e(rest)?;

    Ok((p.append(expr), rest))
}

fn e2<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Program>  {
    let (_, rest) = token(ts, TokenClass::EOF)?;
    Ok((Program::new(), rest))
}

pub fn parse(ts: &[Token]) -> Result<Program, &'static str> {
    e(ts).map(|(expr, _)| expr)
}
