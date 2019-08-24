use std::collections::HashMap;

use crate::ast::{BinOp, Expr, Program};


#[derive(Debug, PartialEq)]
pub enum AExpr {
    Number(i32),
    BinOp(BinOp, Box<AExpr>, Box<AExpr>),
    Call(String, Vec<AExpr>),
    Arg(u8),
    Cond(Box<AExpr>, Box<AExpr>, Box<AExpr>),
}

#[derive(Debug, PartialEq)]
pub struct AFun(String, u8, AExpr);

impl AFun {
    pub fn name(&self) -> &String {
        &self.0
    }
    pub fn arity(&self) -> u8 {
        self.1
    }
    pub fn code(&self) -> &AExpr {
        &self.2
    }
}
        

pub type AProgram = HashMap<String, AFun>;

pub fn aprogram(fs: Vec<AFun>) -> AProgram {
    fs.into_iter().map(move |f| (f.0.clone(), f)).collect()
}

pub fn afun(name: &str, arity: u8, code: AExpr) -> AFun {
    AFun(name.to_string(), arity, code)
}

pub fn aarg(i: u8) -> AExpr {
    AExpr::Arg(i)
}

pub fn anumber(i: i32) -> AExpr {
    AExpr::Number(i)
}

pub fn acall(fname: &str, args: Vec<AExpr>) -> AExpr {
    AExpr::Call(fname.to_string(), args)
}

pub fn abinop(op: BinOp, l: AExpr, r: AExpr) -> AExpr {
    AExpr::BinOp(op, Box::new(l), Box::new(r))
}

pub fn acond(cond: AExpr, cons: AExpr, alt: AExpr) -> AExpr {
    AExpr::Cond(Box::new(cond), Box::new(cons), Box::new(alt))
}

fn annotate_expr(args: &HashMap<String, u8>, e: &Expr) -> Result<AExpr, String> {
    let aexpr = match e {
        Expr::Number(i) => AExpr::Number(*i),
        Expr::BinOp(o, l, r) => abinop(
            *o,
            annotate_expr(&args, l)?,
            annotate_expr(&args, r)?),
        Expr::Call(fname, callargs) => {
            let acallargs: Result<Vec<AExpr>, String> = callargs.iter()
                .map(|e| annotate_expr(&args, &e))
                .collect();
            acall(fname, acallargs?)
        },
        Expr::Var(s) => {
            return args.get(s)
                .map(|&n| aarg(n))
                .or_else(|| Some(acall(s, vec![])))  // TODO: check function call
                .ok_or(format!("Undefined symbol: {}", s))
        },
        Expr::Cond(c, cons, alt) => acond(
            annotate_expr(&args, c)?,
            annotate_expr(&args, cons)?,
            annotate_expr(&args, alt)?)
    };
    Ok(aexpr)
}

pub fn annotate(p: &Program) -> Result<AProgram, String> {
    p.definitions().iter()
        .map(|f| {
            let args = f.arg_indexes();
            let ae = annotate_expr(&args, &f.code)?;
            Ok((f.fname.clone(), AFun(f.fname.clone(), f.arg_names.len() as u8, ae)))
        })
        .collect()
}

#[cfg(test)]
mod test {
    use crate::ast::{Program, Expr};
    use super::{AExpr, annotate, aprogram, afun, aarg, acall, anumber};

    #[test]
    fn simple() {
        assert_eq!(
            annotate(
                &Program::new()
                .define("f", vec![], Expr::number(1))),
            Ok(
                aprogram(vec![
                    afun("f", 0, AExpr::Number(1))
                ])
            )
        );
    }

    #[test]
    fn arguments() {
        assert_eq!(
            annotate(
                &Program::new()
                .define("f", vec!["x", "y"],
                        Expr::call("f", vec![Expr::var("y"), Expr::var("x")]))
            ),
            Ok(
                aprogram(vec![
                    afun("f", 2, acall("f", vec![aarg(1), aarg(0)]))
                ])
            )
        );
    }

    #[test]
    fn nullary() {
        let p = Program::new()
            .define("f", vec!["x"],
                    Expr::call("f", vec![Expr::var("y"), Expr::var("x")]))
            .define("y", vec![], Expr::number(1));
        assert_eq!(
            annotate(&p),
            Ok(
                aprogram(vec![
                    afun(
                        "f",
                        1,
                        acall("f", vec![
                            acall("y", vec![]),
                            aarg(0),
                        ])),
                    afun("y", 0, anumber(1)),
                ])
            )
        );
    }

    #[test]
    fn arg_over_fun_precedence() {
        let p = Program::new()
            .define("f", vec!["x"],
                    Expr::call("f", vec![Expr::var("x")]))
            .define("x", vec![], Expr::number(1));
        assert_eq!(
            annotate(&p),
            Ok(
                aprogram(vec![
                    afun(
                        "f",
                        1,
                        acall("f", vec![aarg(0)])),
                    afun("x", 0, anumber(1)),
                ])
            )
        );
    }
}
