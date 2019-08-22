use std::cell::Cell;
use std::io::prelude::Write;
use std::collections::HashMap;

use crate::ast::*;


struct ProgramEnv<'a> {
    #[allow(dead_code)]
    program: &'a Program,
    label_counter: Cell<u32>,
}

struct FunctionEnv<'a> {
    program_env: &'a ProgramEnv<'a>,
    local_offsets: HashMap<String, u8>,
}

impl ProgramEnv<'_> {
    pub fn new(program: &Program) -> ProgramEnv {
        ProgramEnv {
            program,
            label_counter: Cell::new(1),
        }
    }

    fn frame(&self, arg_names: &Vec<String>) -> FunctionEnv {
        let mut local_offsets = HashMap::new();
        for (i, arg_name) in arg_names.iter().rev().enumerate() {
            local_offsets.insert(
                arg_name.to_string(),
                i as u8 * 8 + 16);
        }
        FunctionEnv { program_env: &self, local_offsets }
    }

    fn next_label_idx(&self) -> u32 {
        let i = self.label_counter.get();
        self.label_counter.set(i+1);
        i
    }
}

fn write_call_amd64<'a>(name: &str, args: &'a Vec<Expr>, env: &FunctionEnv, w: &mut Write)
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

fn write_expr_amd64(e: &Expr, env: &FunctionEnv, w: &mut Write) -> std::io::Result<()> {
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
        Expr::Minus(ref l, ref r) => {
            write_expr_amd64(l, env, w)?;
            write_expr_amd64(r, env, w)?;
            w.write_all(b"pop rdi\n")?;
            w.write_all(b"pop rax\n")?;
            w.write_all(b"sub rax, rdi\n")?;
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
        Expr::Cond(ref c, ref cons, ref alt) => {
            let alt_lbl = env.program_env.next_label_idx();

            write_expr_amd64(c, env, w)?;
            w.write_all(b"pop rax\n")?;
            w.write_all(b"cmp rax, 0\n")?;
            writeln!(w, "jl alt{}", alt_lbl)?;
            write_expr_amd64(cons, env, w)?;
            writeln!(w, "jmp done{}", alt_lbl)?;
            writeln!(w, "alt{}:", alt_lbl)?;
            write_expr_amd64(alt, env, w)?;
            writeln!(w, "done{}:", alt_lbl)?;
        }
    }
    Ok(())
}

fn write_function_amd64(d: &FunDefinition, env: &ProgramEnv, mut w: &mut Write) -> std::io::Result<()> {
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

pub fn write_amd64(p: &Program, mut w: &mut Write) -> std::io::Result<()> {

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

    let env = ProgramEnv::new(&p);
    for d in p.definitions() {
        write_function_amd64(&d, &env, &mut w)?;
    }
    Ok(())
}
