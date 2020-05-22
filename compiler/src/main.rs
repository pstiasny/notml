#![deny(warnings)]

extern crate notmlc;
extern crate toml;
extern crate serde;
extern crate clap;

use std::io::{Read, Write};
use std::fs::File;
use std::process::{Command, Stdio, exit};
use std::rc::Rc;

use clap::{Arg, App};
use serde::{Deserialize};

use notmlc::lexer::{lex, trim_ws};
use notmlc::parser::parse;
use notmlc::sem::{AFunSig, annotate};
use notmlc::codegen::write_amd64;
use notmlc::llvm::emit_ir;


#[derive(Deserialize)]
enum Backend {
    LLVM,
    InternalAMD64,
}

#[derive(Deserialize)]
struct AssemblerConfig {
    nasm_args: Vec<String>,
}

#[derive(Deserialize)]
struct LinkerConfig {
    ld_args: Vec<String>,
}

#[derive(Deserialize)]
struct LLVMConfig {
    llc_path: String,
    llc_args: Vec<String>,
}

#[derive(Deserialize)]
struct Config {
    backend: Backend,
    assembler: AssemblerConfig,
    llvm: LLVMConfig,
    linker: LinkerConfig,
}


fn run_cmd(cmd: &str, args: &[String], verbose: bool) -> std::io::Result<()> {
    if verbose {
        println!("{} {:?}", cmd, args);
    }

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


fn llc_args<'a>(
    cfg: &'a Config,
    ll_file: &'a str,
    object_file: &'a str
) -> Vec<String> {
    let mut args: Vec<String> = Vec::new();
    args.extend(cfg.llvm.llc_args.clone());
    args.push("--filetype=obj".to_string());
    args.push(format!("-o={}", object_file));
    args.push(ll_file.to_string());
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


fn get_runtime_definitions() -> Vec<Rc<AFunSig>> {
    vec![
        Rc::new(AFunSig { name: "print".to_string(), arity: 1, native: true }),
        Rc::new(AFunSig { name: "pchar".to_string(), arity: 1, native: true }),
    ]
}


fn main() -> std::io::Result<()> {
    let arg_matches = App::new("NotML compiler")
        .author("Pawel Stiasny <pawelstiasny@gmail.com>")
        .about("Toy compiler for the toy NotML language")
        .arg(Arg::with_name("config")
             .short("-c")
             .value_name("FILE")
             .help("Compiler config TOML file")
             .takes_value(true))
        .arg(Arg::with_name("INPUT")
            .help("Source file")
            .required(true)
            .index(1))
        .arg(Arg::with_name("output")
             .short("-o")
             .value_name("FILE")
             .help("Binary file path"))
        .arg(Arg::with_name("verbose")
             .short("-v")
             .help("Verbose output"))
        .get_matches();

    let verbose = arg_matches.occurrences_of("verbose") > 0;

    let mut config_str = String::new();
    let config_path = arg_matches.value_of("config").unwrap_or("config.toml");
    let mut config_file = File::open(config_path)?;
    config_file.read_to_string(&mut config_str)?;
    let config: Config = toml::from_str(&config_str)?;

    let input_path: String = arg_matches.value_of("INPUT").unwrap().to_string();
    let mut input_file = File::open(&input_path)?;
    let mut input = String::new();
    input_file.read_to_string(&mut input)?;

    let base_path = if let Some(s) = arg_matches.value_of("output") {
        s.to_string()
    } else if input_path.ends_with(".notml") {
        input_path[0..input_path.len() - 6].to_string()
    } else {
        input_path.clone()
    };
    let asm_path = format!("{}.asm", base_path);
    let llvm_ir_path = format!("{}.ll", base_path);
    let object_path = format!("{}.o", base_path);
    let output_path = if base_path == input_path {
        format!("{}.out", base_path)
    } else {
        base_path.clone()
    };

    let lex_result = lex(&input);
    if verbose || lex_result.is_err() {
        println!("Tokens: {:?}", lex_result);
    }
    if let Ok(mut tokenized_input) = lex_result {
        trim_ws(&mut tokenized_input);

        let res = parse(&tokenized_input);
        if verbose || res.is_err() {
            println!("Parse: {:#?}", res);
        }
        if let Ok(pt) = res {
            let ares = annotate(&pt, &get_runtime_definitions());
            if verbose || ares.is_err() {
                println!("Annotated: {:#?}", ares);
            }

            if let Ok(at) = ares {
                match config.backend {
                    Backend::InternalAMD64 => {
                        let mut outfile = File::create(&asm_path)?;
                        write_amd64(&at, &mut outfile)?;

                        run_cmd(
                            "nasm",
                            &nasm_args(&config, &asm_path, &object_path),
                            verbose)?;
                    }
                    Backend::LLVM => {
                        let mut outstr = String::new();
                        emit_ir(&at, &mut outstr, &get_runtime_definitions());
                        let mut llvm_ir_file = File::create(&llvm_ir_path)?;
                        llvm_ir_file.write_all(outstr.as_bytes())?;

                        run_cmd(
                            &config.llvm.llc_path,
                            &llc_args(&config, &llvm_ir_path, &object_path),
                            verbose)?;
                    }
                }
                run_cmd(
                    "ld",
                    &linker_args(&config, &object_path, &output_path),
                    verbose)?;

                println!("Done 💖");
                return Ok(());
            }
        }
    }

    exit(1);
}
