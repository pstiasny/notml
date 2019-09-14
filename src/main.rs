#![deny(warnings)]

extern crate compiler;

use std::io::{self, Read};
use std::fs::File;
use std::process::{Command, Stdio, exit};

use compiler::lexer::{lex, trim_ws};
use compiler::parser::parse;
use compiler::sem::annotate;
use compiler::codegen::write_amd64;
use compiler::interpreter::eval;


fn run_cmd(cmd: &str, args: &[&str]) -> std::io::Result<()> {
    let output = Command::new(cmd)
        .args(args)
        .stderr(Stdio::inherit())
        .stdout(Stdio::inherit())
        .output()?;

    if !output.status.success() {
        println!("command {} failed", cmd);
        exit(1);
    }

    return Ok(());
}


fn main() -> std::io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let lex_result = lex(&input);
    println!("Tokens: {:?}", lex_result);
    if let Ok(mut tokenized_input) = lex_result {
        trim_ws(&mut tokenized_input);

        let res = parse(&tokenized_input);
        println!("Parse: {:#?}", res);
        if let Ok(pt) = res {
            let ares = annotate(&pt);
            println!("Annotated: {:#?}", ares);

            if let Ok(at) = ares {
                println!("Eval: {:?}", eval(&pt));

                let mut outfile = File::create("out.asm")?;
                write_amd64(&at, &mut outfile)?;

                run_cmd(
                    "nasm",
                    &["-g", "-f", "macho64", "-F", "dwarf", "-o", "out.o", "out.asm"])?;

                run_cmd(
                    "ar",
                    &["rcs", "libout.a", "out.o"])?;

                run_cmd(
                    "rustc",
                    &["runtime.rs", "-L.", "-lout", "-o", "out"])?;

                println!("done");
                return Ok(());
            }
        }
    }

    exit(1);
}
