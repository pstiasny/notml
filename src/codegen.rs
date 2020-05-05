use std::cell::Cell;
use std::io::prelude::Write;
use std::rc::Rc;

use crate::ast::BinOp;
use crate::sem::{AProgram, AFun, AExpr};


struct ProgramEnv {
    label_counter: Cell<u32>,
}

struct FunctionEnv<'a> {
    program_env: &'a ProgramEnv,
    function: &'a AFun,
}

impl ProgramEnv {
    pub fn new() -> ProgramEnv {
        ProgramEnv {
            label_counter: Cell::new(1),
        }
    }

    fn frame<'a>(&'a self, function: &'a AFun) -> FunctionEnv<'a> {
        FunctionEnv { program_env: &self, function }
    }

    fn next_label_idx(&self) -> u32 {
        let i = self.label_counter.get();
        self.label_counter.set(i+1);
        i
    }
}

fn arg_offset_to_rbp(arity: u8, idx: u8) -> u8 {
    16 + (arity - idx - 1) * 8
}

fn write_call_amd64<'a>(
    name: &str,
    args: &'a Vec<Rc<AExpr>>,
    tail: bool,
    env: &FunctionEnv,
    w: &mut dyn Write
) -> std::io::Result<()> {
    for arg in args {
        write_expr_amd64(arg, env, w)?;
    }

    if tail {
        for idx in (0 .. args.len()).rev() {
            writeln!(w, "pop rax")?;
            let offset = arg_offset_to_rbp(env.function.arity(), idx as u8);
            writeln!(w, "mov [rbp+{}], rax", offset)?;
        }
        writeln!(w, "mov rsp, rbp")?;
        writeln!(w, "jmp {}_body", &name)?;
    } else {
        writeln!(w, "call {}", &name)?;
        if args.len() > 0 {
            writeln!(w, "add rsp, {}", 8 * args.len())?;
        }
        w.write_all(b"push rax\n")?;
    }

    Ok(())
}

fn write_expr_amd64(e: &AExpr, env: &FunctionEnv, w: &mut dyn Write) -> std::io::Result<()> {
    match *e {
        AExpr::Number(i) => {
            writeln!(w, "mov rax, {}", i)?;
            w.write_all(b"push rax\n")?;
        },
        AExpr::BinOp(ref op, ref l, ref r) => {
            write_expr_amd64(l, env, w)?;
            write_expr_amd64(r, env, w)?;
            w.write_all(b"pop rdi\n")?;
            w.write_all(b"pop rax\n")?;
            match *op {
                BinOp::Plus => w.write_all(b"add rax, rdi\n"),
                BinOp::Minus => w.write_all(b"sub rax, rdi\n"),
                BinOp::Times => w.write_all(b"mul rdi\n"),
            }?;
            w.write_all(b"push rax\n")?;
        },
        AExpr::Call(ref name, ref args, ref tail) => {
            write_call_amd64(name, args, *tail, env, w)?;
        }
        AExpr::Arg(idx) => {
            let offset = arg_offset_to_rbp(env.function.arity(), idx);
            writeln!(w, "mov rax, [rbp+{}]", offset)?;
            w.write_all(b"push rax\n")?;
        }
        AExpr::Cond(ref c, ref cons, ref alt) => {
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

fn write_function_amd64(d: &AFun, env: &ProgramEnv, mut w: &mut dyn Write) -> std::io::Result<()> {
    let inner_env = env.frame(&d);

    writeln!(w, "\n{}:", &d.name())?;
    w.write_all(b"push rbp\n")?;
    w.write_all(b"mov rbp, rsp\n")?;
    writeln!(w, "{}_body:", &d.name())?;
    write_expr_amd64(&d.code(), &inner_env, &mut w)?;
    w.write_all(b"pop rax\n")?;
    w.write_all(b"pop rbp\n")?;
    w.write_all(b"ret\n")?;
    Ok(())
}

pub fn write_amd64(p: &AProgram, mut w: &mut dyn Write) -> std::io::Result<()> {

    w.write_all(b"
global _main
extern _rt_print

section .text
_main:

push rbp
mov rbp, rsp

call main

mov rdi, rax  ; assign main result to print arg 1
call _rt_print
mov rsp, rbp

mov rax, 0  ; exit code
pop rbp
ret
")?;

    let env = ProgramEnv::new();
    for d in p.values() {
        write_function_amd64(&d, &env, &mut w)?;
    }
    Ok(())
}
