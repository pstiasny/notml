#![deny(warnings)]

pub mod lexer;
pub mod ast;
pub mod parser;
pub mod sem;
pub mod typing;
pub mod codegen;
pub mod ssa;
pub mod llvm;
