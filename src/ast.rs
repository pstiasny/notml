use std::collections::LinkedList;


#[derive(Debug, PartialEq)]
pub enum Expr {
    Number(i32),
    Plus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Var(String),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
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
    pub fn call(name: &str, args: Vec<Expr>) -> Expr {
        Expr::Call(name.to_string(), args)
    }
    pub fn var(name: &str) -> Expr {
        Expr::Var(name.to_string())
    }
    pub fn cond(c: Expr, cons: Expr, alt: Expr) -> Expr {
        Expr::Cond(Box::new(c), Box::new(cons), Box::new(alt))
    }
}

#[derive(Debug, PartialEq)]
pub struct FunDefinition {
    pub fname: String,
    pub arg_names: Vec<String>,
    pub code: Box<Expr>,
}

impl FunDefinition {
    pub fn new(fname: &str, arg_names: Vec<String>, code: Expr) -> FunDefinition {
        FunDefinition {
            fname: fname.to_string(),
            arg_names: arg_names,
            code: Box::new(code),
        }
    }
}

#[derive(Debug, PartialEq)]
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
    pub fn definitions_vec(&self) -> Vec<&FunDefinition> {
        self.0.iter().collect()
    }
}
