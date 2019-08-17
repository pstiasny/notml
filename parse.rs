use std::io::prelude::*;
use std::io::{self, Read};
use std::fs::File;
use std::collections::LinkedList;
use std::collections::HashMap;

#[derive(Debug,PartialEq)]
enum TokenClass {
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
struct Token<'a>(TokenClass, &'a str);

#[derive(Debug)]
enum Expr {
    Number(i32),
    Plus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Var(String),
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
    fn call(name: String, args: Vec<Expr>) -> Expr {
        Expr::Call(name, args)
    }
    fn var(name: String) -> Expr {
        Expr::Var(name)
    }
}

#[derive(Debug)]
struct FunDefinition {
    fname: String,
    arg_names: Vec<String>,
    code: Box<Expr>,
}

struct CodegenEnv {
    local_offsets: HashMap<String, u8>,
}

impl CodegenEnv {
    fn new() -> CodegenEnv {
        CodegenEnv { local_offsets: HashMap::new() }
    }

    fn frame(&self, arg_names: &Vec<String>) -> CodegenEnv {
        let mut local_offsets = HashMap::new();
        for (i, arg_name) in arg_names.iter().rev().enumerate() {
            local_offsets.insert(
                arg_name.to_string(),
                i as u8 * 8 + 16);
        }
        CodegenEnv { local_offsets }
    }
}

struct EvalEnv<'a> {
    functions: &'a HashMap<String, &'a FunDefinition>,
    locals: HashMap<String, i32>,
}

impl<'a> EvalEnv<'a> {
    fn new(functions: &'a HashMap<String, &'a FunDefinition>) -> EvalEnv<'a> {
        EvalEnv { functions, locals: HashMap::new() }
    }

    fn frame(&self, arg_names: &'a Vec<String>, args: &'a Vec<i32>) -> EvalEnv<'a> {
        let mut locals = HashMap::new();
        for (arg_name, arg_value) in arg_names.iter().zip(args.iter()) {
            locals.insert(arg_name.to_string(), *arg_value);
        }
        EvalEnv { functions: &self.functions, locals }
    }

    fn get_definition(&self, fname: &str) -> Result<&FunDefinition, &'static str> {
        match self.functions.get(fname) {
            Some(fdef) => Ok(fdef),
            None => Err("Undefined function")
        }
    }
}

#[derive(Debug)]
struct Program(LinkedList<FunDefinition>);

impl Program {
    fn new() -> Program {
        Program(LinkedList::new())
    }
    fn append(&mut self, d: FunDefinition) -> Program {
        let mut l2 = LinkedList::new();
        l2.push_back(d);
        l2.append(&mut self.0);
        Program(l2)
    }
    fn definitions(&self) -> &LinkedList<FunDefinition> {
        &self.0
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

fn parse(ts: &[Token]) -> Result<Program, &'static str> {
    e(ts).map(|(expr, _)| expr)
}

fn eval_call<'a>(name: &str, args: &'a Vec<i32>, env: &'a EvalEnv<'a>) -> Result<i32, &'static str> {
    let ref def = env.get_definition(&name)?;
    if args.len() != def.arg_names.len() {
        return Err("invalid number of arguments");
    }
    let next_env = env.frame(&def.arg_names, &args);
    eval_expr(&def.code, &next_env)
}

fn eval_args<'a>(args: &'a Vec<Expr>, env: &'a EvalEnv<'a>) -> Result<Vec<i32>, &'static str> {
    let mut evargs = Vec::new();
    for e in args.iter() {
        let evarg = eval_expr(&e, &env)?;
        evargs.push(evarg);
    }
    Ok(evargs)
}

fn eval_expr<'a>(e: &'a Expr, env: &'a EvalEnv<'a>) -> Result<i32, &'static str> {
    match *e {
        Expr::Number(i) => Ok(i),
        Expr::Plus(ref l, ref r) =>
            Ok(eval_expr(l, env)? + eval_expr(r, env)?),
        Expr::Times(ref l, ref r) =>
            Ok(eval_expr(l, env)? * eval_expr(r, env)?),
        Expr::Call(ref name, ref args) => {
            let evargs = eval_args(&args, &env)?;
            eval_call(&name, &evargs, &env)
        }
        Expr::Var(ref name) => {
            match env.locals.get(name) {
                Some(val) => Ok(*val),
                // glbobal constant treated as nullary function call
                None => eval_call(name, &Vec::new(), &env)
            }
        }
    }
}

fn collect_functions<'a>(p: &'a Program) -> HashMap<String, &'a FunDefinition>
{
    let mut functions = HashMap::new();

    for d in p.definitions() {
        functions.insert(d.fname.to_string(), d);
    }

    functions
}

fn eval(p: &Program) -> Result<i32, &'static str> {
    let functions = collect_functions(p);
    let env = EvalEnv::new(&functions);

    eval_call("main", &Vec::new(), &env)
}

fn write_call_amd64<'a>(name: &str, args: &'a Vec<Expr>, env: &CodegenEnv, w: &mut Write)
 -> std::io::Result<()> {
    for arg in args {
        write_expr_amd64(arg, env, w)?;
    }
    writeln!(w, "call {}", &name)?;
    if args.len() > 0 {
        writeln!(w, "add rsp, {}", 8 * args.len())?;
    }
    w.write_all(b"push rax\n")?;
    Ok(())
}

fn write_expr_amd64(e: &Expr, env: &CodegenEnv, w: &mut Write) -> std::io::Result<()> {
    match *e {
        Expr::Number(i) => {
            writeln!(w, "mov rax, {}", i)?;
            w.write_all(b"push rax\n")?;
        },
        Expr::Plus(ref l, ref r) => {
            write_expr_amd64(l, env, w)?;
            write_expr_amd64(r, env, w)?;
            w.write_all(b"pop rax\n")?;
            w.write_all(b"pop rdi\n")?;
            w.write_all(b"add rax, rdi\n")?;
            w.write_all(b"push rax\n")?;
        },
        Expr::Times(ref l, ref r) => {
            write_expr_amd64(l, env, w)?;
            write_expr_amd64(r, env, w)?;
            w.write_all(b"pop rax\n")?;
            w.write_all(b"pop rdi\n")?;
            w.write_all(b"mul rdi\n")?;
            w.write_all(b"push rax\n")?;
        },
        Expr::Call(ref name, ref args) => {
            write_call_amd64(name, args, env, w)?;
        }
        Expr::Var(ref name) => {
            match env.local_offsets.get(name) {
                Some(ptr) => {
                    writeln!(w, "mov rax, [rbp+{}]", ptr)?;
                    w.write_all(b"push rax\n")?;
                }
                None => {
                    // glbobal constant treated as nullary function call
                    write_call_amd64(name, &Vec::new(), &env, w)?;
                }
            }
        }
    }
    Ok(())
}

fn write_function_amd64(d: &FunDefinition, env: &CodegenEnv, mut w: &mut Write) -> std::io::Result<()> {
    let inner_env = env.frame(&d.arg_names);

    writeln!(w, "\n{}:", &d.fname)?;
    w.write_all(b"push rbp\n")?;
    w.write_all(b"mov rbp, rsp\n")?;
    write_expr_amd64(&d.code, &inner_env, &mut w)?;
    w.write_all(b"pop rax\n")?;
    w.write_all(b"pop rbp\n")?;
    w.write_all(b"ret\n")?;
    Ok(())
}

fn write_amd64(p: &Program, env: &CodegenEnv, mut w: &mut Write) -> std::io::Result<()> {

    w.write_all(b"
global _calcmain

section .text
_calcmain:
push rbp
mov rbp, rsp

call main

mov rsp, rbp
pop rbp
ret

")?;

    for d in p.definitions() {
        write_function_amd64(&d, &env, &mut w)?;
    }
    Ok(())
}

fn main() -> std::io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut tokenized_input = lex(&input);
    tokenized_input.retain(|tok| tok.0 != TokenClass::WS);
    println!("Tokens: {:?}", tokenized_input);

    let res = parse(&tokenized_input);
    println!("Parse: {:#?}", res);
    if let Ok(e) = res {
        println!("Eval: {:?}", eval(&e));

        let created = File::create("out.asm");
        match created {
            Ok(mut outfile) => {
                // TODO: better error handling
                write_amd64(&e, &CodegenEnv::new(), &mut outfile)?;
            },
            _ => {
                println!("Could not create output file");
            }
        }
    }

    Ok(())

}
