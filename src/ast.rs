use std::collections::LinkedList;


#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Plus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Var(String),
}

impl Expr {
    pub fn number(i: i32) -> Expr {
        Expr::Number(i)
    }
    pub fn plus(l: Expr, r: Expr) -> Expr {
        Expr::Plus(Box::new(l), Box::new(r))
    }
    pub fn times(l: Expr, r: Expr) -> Expr {
        Expr::Times(Box::new(l), Box::new(r))
    }
    pub fn call(name: String, args: Vec<Expr>) -> Expr {
        Expr::Call(name, args)
    }
    pub fn var(name: String) -> Expr {
        Expr::Var(name)
    }
}

#[derive(Debug)]
pub struct FunDefinition {
    pub fname: String,
    pub arg_names: Vec<String>,
    pub code: Box<Expr>,
}

#[derive(Debug)]
pub struct Program(LinkedList<FunDefinition>);

impl Program {
    pub fn new() -> Program {
        Program(LinkedList::new())
    }
    pub fn append(&mut self, d: FunDefinition) -> Program {
        let mut l2 = LinkedList::new();
        l2.push_back(d);
        l2.append(&mut self.0);
        Program(l2)
    }
    pub fn definitions(&self) -> &LinkedList<FunDefinition> {
        &self.0
    }
}
