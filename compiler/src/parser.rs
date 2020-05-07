use crate::lexer::*;
use crate::ast::*;

// Grammar:
// E -> D ; E | eof
// D -> sym sym* = S
// S -> Q | T + T | T - T | T
// Q -> if S then S else S
// T -> U * T | U
// U -> C | V
// V -> ( S ) | number | sym
// C -> sym V*

#[derive(Debug, PartialEq)]
pub struct ParseError(String, Position);

type ParseResult<'a, T> = Result<(T, &'a[Token<'a>]), ParseError>;

trait BestError {
    fn or_backtrack<O: FnOnce() -> Self>(self, op: O) -> Self where Self: std::marker::Sized;
}

impl<'a, T> BestError for ParseResult<'a, T> {
    fn or_backtrack<O: FnOnce() -> ParseResult<'a, T>>(
        self: ParseResult<'a, T>,
        op: O
    ) -> ParseResult<'a, T> {
        self.or_else(|e1| {
            let result2 = op();
            match result2 {
                Ok(x) => Ok(x),
                Err(e2) => {
                    if e1.1 < e2.1 { Err(e2) } else { Err(e1) }
                }
            }
        })
    }
}

fn token<'a>(ts: &'a [Token<'a>], expected_tc: TokenClass) -> ParseResult<'a, &'a str> {
    let Token(ref tc, tstr, pos) = &ts[0];
    if *tc == expected_tc {
        Ok((tstr, &ts[1..]))
    } else {
        Err(
            ParseError(
                format!("unexpected token {} at line {} col {}", &tstr, &pos.0, &pos.1),
                *pos))
    }
}

fn position<'a>(ts: &'a [Token<'a>]) -> Position {
    ts[0].2
}

fn many<'a, T>(
    f: &dyn Fn(&'a [Token<'a>]) -> ParseResult<'a, T>,
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
        Err(_) => Err(ParseError(format!("invalid integer: {:?}", i_str), position(ts))),
    }
}

fn c<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (fname, rest) = token(ts, TokenClass::Symbol)?;
    let (args, rest) = many(&v, rest)?;
    if args.len() == 0 {
        Ok((Expr::var(fname), rest))
    } else {
        Ok((Expr::call(fname, args), rest))
    }
}

fn u<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    c(ts).or_backtrack(|| v(ts))
}

fn v1<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (_, rest) = token(ts, TokenClass::LParen)?;
    let (expr, rest) = s(rest)?;
    let (_, rest) = token(rest, TokenClass::RParen)?;
    Ok((expr, rest))
}

fn v2<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    n(ts)
}

fn v3<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (s, rest) = token(ts, TokenClass::Symbol)?;
    Ok((Expr::var(s), rest))
}

fn v<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    v1(ts)
        .or_backtrack(|| {v2(ts)})
        .or_backtrack(|| {v3(ts)})
}

fn t1<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (expr1, rest) = u(ts)?;
    let (_, rest) = token(rest, TokenClass::Times)?;
    let (expr2, rest) = t(rest)?;
    Ok((Expr::times(expr1, expr2), rest))
}

fn t2<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    u(ts)
}

fn t<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    t1(ts)
        .or_backtrack(|| {t2(ts)})
}

fn s1<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    q(ts)
}

fn s2<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (expr1, rest) = t(ts)?;
    let (_, rest) = token(rest, TokenClass::Plus)?;
    let (expr2, rest) = t(rest)?;
    let expr = Expr::plus(expr1, expr2);
    Ok((expr, rest))
}

fn s3<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (expr1, rest) = t(ts)?;
    let (_, rest) = token(rest, TokenClass::Minus)?;
    let (expr2, rest) = t(rest)?;
    let expr = Expr::minus(expr1, expr2);
    Ok((expr, rest))
}

fn s4<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let expr = t(ts);
    expr
}

fn s<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    s1(ts)
        .or_backtrack(|| s2(ts))
        .or_backtrack(|| s3(ts))
        .or_backtrack(|| s4(ts))
}

fn q<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Expr> {
    let (_, rest) = token(ts, TokenClass::If)?;
    let (expr1, rest) = s(rest)?;
    let (_, rest) = token(rest, TokenClass::Then)?;
    let (expr2, rest) = s(rest)?;
    let (_, rest) = token(rest, TokenClass::Else)?;
    let (expr3, rest) = s(rest)?;
    let expr = Expr::cond(expr1, expr2, expr3);
    Ok((expr, rest))
}

fn d<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, FunDefinition> {
    let (sym, rest) = token(ts, TokenClass::Symbol)?;

    let (args, rest) = many(&|ts| {
        let (sym, rest) = token(ts, TokenClass::Symbol)?;
        Ok((sym.to_string(), rest))
    }, rest)?;

    let (_, rest) = token(rest, TokenClass::Assign)?;
    let (expr, rest) = s(rest)?;

    let fd = FunDefinition::new(sym, args, expr);
    Ok((fd, rest))
}

fn e<'a>(ts: &'a [Token<'a>]) -> ParseResult<'a, Program> {
    e1(ts).or_backtrack(|| e2(ts))
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

pub fn parse(ts: &[Token]) -> Result<Program, ParseError> {
    e(ts).map(|(expr, _)| expr)
}


#[cfg(test)]
mod test {
    use crate::lexer::{Position, lex, trim_ws};
    use crate::ast::{Program, FunDefinition, Expr};
    use super::{ParseError, parse};

    fn parse_str(s: &str) -> Program {
        let mut toks = lex(s).unwrap();
        trim_ws(&mut toks);
        parse(&toks).unwrap()
    }

    #[test]
    fn parses() {
        assert_eq!(parse_str("constant = 1;").definitions_vec(), vec![
            &FunDefinition::new("constant", vec![], Expr::number(1)),
        ]);

        assert_eq!(parse_str("fa = 1; fb = 2;").definitions_vec(), vec![
            &FunDefinition::new("fa", vec![], Expr::number(1)),
            &FunDefinition::new("fb", vec![], Expr::number(2)),
        ]);

        assert_eq!(parse_str("f = 1 - 2 * 3;").definitions_vec(), vec![
            &FunDefinition::new(
                "f",
                vec![],
                Expr::minus(
                    Expr::number(1),
                    Expr::times(Expr::number(2), Expr::number(3)))),
        ]);

        assert_eq!(parse_str("f = 1 * 2 - 3;").definitions_vec(), vec![
            &FunDefinition::new(
                "f",
                vec![],
                Expr::minus(
                    Expr::times(Expr::number(1), Expr::number(2)),
                    Expr::number(3))),
        ]);

        assert_eq!(parse_str("f = (1 - 2) * 3;").definitions_vec(), vec![
            &FunDefinition::new(
                "f",
                vec![],
                Expr::times(
                    Expr::minus(Expr::number(1), Expr::number(2)),
                    Expr::number(3))),
        ]);

        assert_eq!(parse_str("f = 1 * (2 - 3);").definitions_vec(), vec![
            &FunDefinition::new(
                "f",
                vec![],
                Expr::times(
                    Expr::number(1),
                    Expr::minus(Expr::number(2), Expr::number(3)))),
        ]);

        assert_eq!(parse_str("f = 1 + 2 * 3;").definitions_vec(), vec![
            &FunDefinition::new(
                "f",
                vec![],
                Expr::plus(
                    Expr::number(1),
                    Expr::times(Expr::number(2), Expr::number(3)))),
        ]);

        assert_eq!(parse_str("f = 1 * 2 + 3;").definitions_vec(), vec![
            &FunDefinition::new(
                "f",
                vec![],
                Expr::plus(
                    Expr::times(Expr::number(1), Expr::number(2)),
                    Expr::number(3))),
        ]);

        assert_eq!(parse_str("f = (1 + 2) * 3;").definitions_vec(), vec![
            &FunDefinition::new(
                "f",
                vec![],
                Expr::times(
                    Expr::plus(Expr::number(1), Expr::number(2)),
                    Expr::number(3))),
        ]);

        assert_eq!(parse_str("f = 1 * (2 + 3);").definitions_vec(), vec![
            &FunDefinition::new(
                "f",
                vec![],
                Expr::times(
                    Expr::number(1),
                    Expr::plus(Expr::number(2), Expr::number(3)))),
        ]);

        assert_eq!(parse_str("f x y = 2 * x + y;").definitions_vec(), vec![
            &FunDefinition::new(
                "f",
                vec!["x".to_string(), "y".to_string()],
                Expr::plus(
                    Expr::times(Expr::number(2), Expr::var("x")),
                    Expr::var("y"))),
        ]);

        assert_eq!(parse_str("main = fone x + 1;").definitions_vec(), vec![
            &FunDefinition::new(
                "main",
                vec![],
                Expr::plus(
                    Expr::call("fone", vec![Expr::var("x")]),
                    Expr::number(1))),
        ]);

        assert_eq!(parse_str("main = fone (x + 1);").definitions_vec(), vec![
            &FunDefinition::new(
                "main",
                vec![],
                Expr::call(
                    "fone",
                    vec![Expr::plus(Expr::var("x"), Expr::number(1))])),
        ]);

        assert_eq!(parse_str("main = ftwo fzero 1;").definitions_vec(), vec![
            &FunDefinition::new(
                "main",
                vec![],
                Expr::call("ftwo", vec![
                    Expr::var("fzero"),
                    Expr::number(1),
                ])),
        ]);

        assert_eq!(parse_str("main = if p then x else y + 3;").definitions_vec(), vec![
            &FunDefinition::new(
                "main",
                vec![],
                Expr::cond(
                    Expr::var("p"),
                    Expr::var("x"),
                    Expr::plus(
                        Expr::var("y"),
                        Expr::number(3))
                )),
        ]);
    }

    #[test]
    fn parse_error() {
        let mut toks = lex("foo x = ;").unwrap();
        trim_ws(&mut toks);
        assert_eq!(
            parse(&toks),
            Err(
                ParseError(
                    "unexpected token ; at line 1 col 9".to_string(),
                    Position(1, 9))));
    }
}
