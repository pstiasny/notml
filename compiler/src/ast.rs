use std::collections::HashMap;
use std::collections::LinkedList;


#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    Eq,
    Gt,
    Gte,
    Lt,
    Lte,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Number(i32),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Var(String),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    Seq(Vec<Expr>),
}

impl Expr {
    pub fn number(i: i32) -> Expr {
        Expr::Number(i)
    }
    pub fn binop(op: BinOp, l: Expr, r: Expr) -> Expr {
        Expr::BinOp(op, Box::new(l), Box::new(r))
    }
    pub fn plus(l: Expr, r: Expr) -> Expr {
        Expr::BinOp(BinOp::Plus, Box::new(l), Box::new(r))
    }
    pub fn minus(l: Expr, r: Expr) -> Expr {
        Expr::BinOp(BinOp::Minus, Box::new(l), Box::new(r))
    }
    pub fn times(l: Expr, r: Expr) -> Expr {
        Expr::BinOp(BinOp::Times, Box::new(l), Box::new(r))
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
    pub fn seq(exprs: Vec<Expr>) -> Expr {
        Expr::Seq(exprs)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunDefinition {
    pub fname: String,
    pub arg_names: Vec<String>,
    pub arg_types: Vec<Option<String>>,
    pub return_type: Option<String>,
    pub code: Box<Expr>,
}

impl FunDefinition {
    pub fn new(fname: &str, arg_names: Vec<String>, code: Expr) -> FunDefinition {
        FunDefinition {
            fname: fname.to_string(),
            arg_types: vec![None; arg_names.len()],
            arg_names: arg_names,
            return_type: None,
            code: Box::new(code),
        }
    }
    pub fn new_typed(
        fname: &str,
        arg_names: Vec<String>,
        arg_types: Vec<Option<String>>,
        return_type: Option<String>,
        code: Expr
    ) -> FunDefinition {
        FunDefinition {
            fname: fname.to_string(),
            arg_names,
            arg_types,
            return_type,
            code: Box::new(code),
        }
    }

    pub fn arg_indexes(&self) -> HashMap<String, u8> {
        self.arg_names.iter()
            .enumerate()
            .map(|(i, arg_name)| (arg_name.to_string(), i as u8))
            .collect()
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
    pub fn define(&mut self, fname: &str, arg_names: Vec<&str>, code: Expr) -> Program {
        let fd = FunDefinition {
            fname: fname.to_string(),
            arg_names: arg_names.iter().map(|&s| s.to_string()).collect(),
            arg_types: vec![None; arg_names.len()],
            return_type: None,
            code: Box::new(code),
        };
        self.append(fd)
    }
    pub fn define_typed(
        &mut self,
        fname: &str,
        arg_names: Vec<&str>,
        arg_types: Vec<Option<String>>,
        return_type: Option<String>,
        code: Expr
    ) -> Program {
        let fd = FunDefinition {
            fname: fname.to_string(),
            arg_names: arg_names.iter().map(|&s| s.to_string()).collect(),
            arg_types: arg_types,
            return_type: return_type,
            code: Box::new(code),
        };
        self.append(fd)
    }
    pub fn definitions(&self) -> &LinkedList<FunDefinition> {
        &self.0
    }
    pub fn definitions_vec(&self) -> Vec<&FunDefinition> {
        self.0.iter().collect()
    }
    pub fn definitions_hash(&self) -> HashMap<String, &FunDefinition> {
        let mut functions = HashMap::new();

        for d in self.definitions() {
            functions.insert(d.fname.to_string(), d);
        }

        functions
    }
}
