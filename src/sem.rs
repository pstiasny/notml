use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinOp, FunDefinition, Expr, Program};


#[derive(Debug, PartialEq)]
pub enum AExpr {
    Number(i32),
    BinOp(BinOp, Rc<AExpr>, Rc<AExpr>),
    Call(String, Vec<Rc<AExpr>>),
    Arg(u8),
    Cond(Rc<AExpr>, Rc<AExpr>, Rc<AExpr>),
}

#[derive(Debug, PartialEq)]
pub struct AFun(String, u8, Rc<AExpr>);

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
    pub fn map(self, f: &dyn Fn(Rc<AExpr>) -> Rc<AExpr>) -> Self {
        AFun(self.0, self.1, f(self.2))
    }
    pub fn try_map<T>(
        &self,
        f: &dyn Fn(Rc<AExpr>) -> Result<Rc<AExpr>, T>
    ) -> Result<Rc<Self>, T> {
        Ok(Rc::new(AFun(self.0.clone(), self.1, f(Rc::clone(&self.2))?)))
    }
}

pub type AProgram = HashMap<String, Rc<AFun>>;

pub fn aprogram(fs: Vec<Rc<AFun>>) -> AProgram {
    fs.into_iter().map(move |f| (f.0.clone(), Rc::clone(&f))).collect()
}

pub fn afun(name: &str, arity: u8, code: Rc<AExpr>) -> Rc<AFun> {
    Rc::new(AFun(name.to_string(), arity, code))
}

pub fn aarg(i: u8) -> Rc<AExpr> {
    Rc::new(AExpr::Arg(i))
}

pub fn anumber(i: i32) -> Rc<AExpr> {
    Rc::new(AExpr::Number(i))
}

pub fn acall(fname: &str, args: Vec<Rc<AExpr>>) -> Rc<AExpr> {
    Rc::new(AExpr::Call(fname.to_string(), args))
}

pub fn abinop(op: BinOp, l: Rc<AExpr>, r: Rc<AExpr>) -> Rc<AExpr> {
    Rc::new(AExpr::BinOp(op, l, r))
}

pub fn acond(cond: Rc<AExpr>, cons: Rc<AExpr>, alt: Rc<AExpr>) -> Rc<AExpr> {
    Rc::new(AExpr::Cond(cond, cons, alt))
}

fn annotate_expr(
    p: &HashMap<String, &FunDefinition>,
    args: &HashMap<String, u8>,
    e: &Expr
) -> Result<Rc<AExpr>, String> {
    match e {
        Expr::Number(i) => Ok(anumber(*i)),
        Expr::BinOp(o, l, r) => Ok(abinop(
            *o,
            annotate_expr(&p, &args, l)?,
            annotate_expr(&p, &args, r)?)),
        Expr::Call(fname, callargs) => {
            let acallargs: Result<Vec<Rc<AExpr>>, String> = callargs.iter()
                .map(|e| annotate_expr(&p, &args, &e))
                .collect();
            let fd = p.get(fname).ok_or(format!("Undefined function: {}", fname))?;
            Ok(acall(&fd.fname, acallargs?))
        },
        Expr::Var(s) => {
            args.get(s).map(|&n| aarg(n))
            .or_else(|| p.get(s).map(|&fd| acall(&fd.fname, vec![])))
            .ok_or(format!("Undefined symbol: {}", s))
        },
        Expr::Cond(c, cons, alt) => Ok(acond(
            annotate_expr(&p, &args, c)?,
            annotate_expr(&p, &args, cons)?,
            annotate_expr(&p, &args, alt)?))
    }
}

fn map_expr(
    f: &dyn Fn(Rc<AExpr>) -> Result<Rc<AExpr>, String>,
    e: Rc<AExpr>
) -> Result<Rc<AExpr>, String> {
    let node = match *e {
        AExpr::Number(_) => e,
        AExpr::BinOp(ref op, ref l, ref r) => abinop(
            *op,
            f(Rc::clone(l))?,
            f(Rc::clone(r))?),
        AExpr::Call(ref name, ref args) => {
            let argsm: Result<Vec<Rc<AExpr>>, String> = args
                .iter().cloned()
                .map(f)
                .collect();
            acall(&name, argsm?)
        }
        AExpr::Arg(_) => e,
        AExpr::Cond(ref c, ref cons, ref alt) => acond(
            f(Rc::clone(c))?,
            f(Rc::clone(cons))?,
            f(Rc::clone(alt))?)
    };
    Ok(node)
}

fn traverse(
    f: &dyn Fn(Rc<AExpr>) -> Result<Rc<AExpr>, String>,
    e: Rc<AExpr>
) -> Result<Rc<AExpr>, String> {
    let mut node = map_expr(&|ei| traverse(f, ei), e)?;
    node = f(node)?;
    Ok(node)
}

fn check_calls(p: AProgram) -> Result<AProgram, String> {
    fn f(p: &AProgram, aexpr: Rc<AExpr>) -> Result<Rc<AExpr>, String> {
        match *aexpr {
            AExpr::Call(ref fname, ref args) => {
                let fd = p.get(fname).ok_or("undefined function")?;

                if fd.arity() == args.len() as u8 {
                    Ok(aexpr)
                } else {
                    Err(format!(
                        "function {} requires {} arguments, {} given",
                        fname, fd.arity(), args.len()))
                }
            },
            _ => Ok(aexpr)
        }
    }
    let funs: Vec<Rc<AFun>> = p.iter()
        .map(|(_, af)| (&af).try_map(&|root| traverse(&|x| f(&p, x), root)))
        .collect::<Result<Vec<Rc<AFun>>, String>>()?;
    Ok(aprogram(funs))
}

pub fn annotate(p: &Program) -> Result<AProgram, String> {
    let fdmap = p.definitions_hash();
    let mut ap: AProgram = p.definitions().iter()
        .map(|f| {
            let args = f.arg_indexes();
            let ae = annotate_expr(&fdmap, &args, &f.code)?;
            let fun = AFun(f.fname.clone(), f.arg_names.len() as u8, ae);
            Ok((f.fname.clone(), Rc::new(fun)))
        })
        .collect::<Result<AProgram, String>>()?;

    ap = check_calls(ap)?;
    // TODO: check main

    Ok(ap)
}

#[cfg(test)]
mod test {
    use crate::ast::{Program, Expr};
    use super::{annotate, aprogram, afun, aarg, acall, anumber};

    #[test]
    fn simple() {
        assert_eq!(
            annotate(
                &Program::new()
                .define("f", vec![], Expr::number(1))),
            Ok(
                aprogram(vec![
                    afun("f", 0, anumber(1))
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
            .define("f", vec!["x", "z"],
                    Expr::call("f", vec![Expr::var("y"), Expr::var("x")]))
            .define("y", vec![], Expr::number(1));
        assert_eq!(
            annotate(&p),
            Ok(
                aprogram(vec![
                    afun(
                        "f",
                        2,
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

    #[test]
    fn undefined() {
        assert_eq!(
            annotate(
                &Program::new()
                .define("main", vec![], Expr::var("x"))),
            Err("Undefined symbol: x".to_string())
        );

        assert_eq!(
            annotate(
                &Program::new()
                .define("main", vec![], Expr::call("f", vec![Expr::number(1)]))),
            Err("Undefined function: f".to_string())
        );
    }

    #[test]
    fn bad_call() {
        assert_eq!(
            annotate(
                &Program::new()
                .define("f", vec!["x", "y"], Expr::number(0))
                .define("main", vec![],
                        Expr::call("f", vec![Expr::number(1)]))),
            Err("function f requires 2 arguments, 1 given".to_string())
        );

        assert_eq!(
            annotate(
                &Program::new()
                .define("f", vec!["x", "y"], Expr::number(0))
                .define("main", vec![],
                        Expr::plus(
                            Expr::number(1),
                            Expr::call("f", vec![Expr::number(1)])))),
            Err("function f requires 2 arguments, 1 given".to_string())
        );

        assert_eq!(
            annotate(
                &Program::new()
                .define("f", vec![], Expr::number(0))
                .define("main", vec![],
                        Expr::call("f", vec![Expr::number(1)]))),
            Err("function f requires 0 arguments, 1 given".to_string())
        );

        assert_eq!(
            annotate(
                &Program::new()
                .define("f", vec!["x"], Expr::number(0))
                .define("main", vec![], Expr::var("f"))),
            Err("function f requires 1 arguments, 0 given".to_string())
        );
    }
}
