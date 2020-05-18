#![deny(warnings)]

pub mod lexer;
pub mod ast;
pub mod parser;
pub mod sem;
pub mod interpreter;
pub mod codegen;
pub mod ssa;
pub mod llvm;
