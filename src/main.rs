#![deny(warnings)]

extern crate compiler;

use std::io::{self, Read};
use std::fs::File;

use compiler::lexer::{TokenClass, lex};
use compiler::parser::parse;
use compiler::codegen::{CodegenEnv, write_amd64};
use compiler::interpreter::eval;


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

        let mut outfile = File::create("out.asm")?;
        write_amd64(&e, &CodegenEnv::new(), &mut outfile)?;
    }

    Ok(())

}
