#![deny(warnings)]

extern crate compiler;

use std::io::{self, Read};
use std::fs::File;

use compiler::lexer::{lex, trim_ws};
use compiler::parser::parse;
use compiler::codegen::{CodegenEnv, write_amd64};
use compiler::interpreter::eval;


fn main() -> std::io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let lex_result = lex(&input);
    println!("Tokens: {:?}", lex_result);
    if let Ok(mut tokenized_input) = lex_result {
        trim_ws(&mut tokenized_input);

        let res = parse(&tokenized_input);
        println!("Parse: {:#?}", res);
        if let Ok(e) = res {
            println!("Eval: {:?}", eval(&e));

            let mut outfile = File::create("out.asm")?;
            write_amd64(&e, &CodegenEnv::new(), &mut outfile)?;
        }
    }

    Ok(())
}
