#![deny(warnings)]

extern crate compiler;
extern crate toml;

use std::io::{self, Read};
use std::fs::File;
use std::process::{Command, Stdio, exit};

use serde::{Deserialize};

use compiler::lexer::{lex, trim_ws};
use compiler::parser::parse;
use compiler::sem::annotate;
use compiler::codegen::write_amd64;
use compiler::interpreter::eval;


#[derive(Deserialize)]
struct AssemblerConfig {
    nasm_args: Vec<String>,
}

#[derive(Deserialize)]
struct LinkerConfig {
    ld_args: Vec<String>,
}

#[derive(Deserialize)]
struct Config {
    assembler: AssemblerConfig,
    linker: LinkerConfig,
}


fn run_cmd(cmd: &str, args: &[String]) -> std::io::Result<()> {
    println!("{} {:?}", cmd, args);

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


fn nasm_args<'a>(
    cfg: &'a Config,
    asm_file: &'a str,
    object_file: &'a str
) -> Vec<String> {
    let mut args: Vec<String> = Vec::new();
    args.extend(cfg.assembler.nasm_args.clone());
    args.push("-o".to_string());
    args.push(object_file.to_string());
    args.push(asm_file.to_string());
    args
}


fn linker_args<'a>(
    cfg: &'a Config,
    object_file: &'a str,
    out_file: &'a str
) -> Vec<String> {
    let mut args: Vec<String> = Vec::new();
    args.extend(cfg.linker.ld_args.clone());
    args.push(object_file.to_string());
    args.push("-o".to_string());
    args.push(out_file.to_string());
    args
}


fn main() -> std::io::Result<()> {
    let mut config_str = String::new();
    let mut config_file = File::open("config.toml")?;
    config_file.read_to_string(&mut config_str)?;
    let config: Config = toml::from_str(&config_str)?;

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

                run_cmd("nasm", &nasm_args(&config, "out.asm", "out.o"))?;
                run_cmd("ld", &linker_args(&config, "out.o", "out"))?;

                println!("done");
                return Ok(());
            }
        }
    }

    exit(1);
}
