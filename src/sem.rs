use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinOp, FunDefinition, Expr, Program};


#[derive(Debug, PartialEq)]
pub enum AExpr {
    Number(i32),
    BinOp(BinOp, Rc<AExpr>, Rc<AExpr>),
    Call(String, Vec<Rc<AExpr>>, bool),
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

pub fn acall(fname: &str, args: Vec<Rc<AExpr>>, tail: bool) -> Rc<AExpr> {
    Rc::new(AExpr::Call(fname.to_string(), args, tail))
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
            Ok(acall(&fd.fname, acallargs?, false))
        },
        Expr::Var(s) => {
            args.get(s).map(|&n| aarg(n))
            .or_else(|| p.get(s).map(|&fd| acall(&fd.fname, vec![], false)))
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
        AExpr::Call(ref name, ref args, ref tail) => {
            let argsm: Result<Vec<Rc<AExpr>>, String> = args
                .iter().cloned()
                .map(f)
                .collect();
            acall(&name, argsm?, *tail)
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

fn map_program_functions(
    f: &dyn Fn(&AProgram, &AFun, Rc<AExpr>) -> Result<Rc<AExpr>, String>,
    p: AProgram
) -> Result<AProgram, String> {
    let funs: Vec<Rc<AFun>> = p.iter()
        .map(|(_, af)| (&af).try_map(&|root| f(&p, &af, Rc::clone(&root))))
        .collect::<Result<Vec<Rc<AFun>>, String>>()?;
    Ok(aprogram(funs))
}

fn check_calls(p: AProgram) -> Result<AProgram, String> {
    fn f(p: &AProgram, aexpr: Rc<AExpr>) -> Result<Rc<AExpr>, String> {
        match *aexpr {
            AExpr::Call(ref fname, ref args, _) => {
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
    map_program_functions(
        &|p, _, root| traverse(&|x| f(&p, x), root),
        p)
}

fn check_main(p: &AProgram) -> Result<(), String> {
    match p.get("main") {
        None => {
            return Err("main function must be defined".to_string());
        }
        Some(f) => {
            if f.arity() != 0 {
                return Err("main function must take no arguments".to_string());
            }
        }
    }
    Ok(())
}

fn replace_tailcalls(p: AProgram) -> Result<AProgram, String> {
    fn rec(p: &AProgram, f: &AFun, e: Rc<AExpr>) -> Result<Rc<AExpr>, String> {
        let res = match *e {
            AExpr::Call(ref fname, ref args, _) => {
                if fname == f.name() {
                    acall(fname, (*args).iter().cloned().collect(), true)
                } else {
                    e
                }
            },
            AExpr::Cond(ref c, ref cons, ref alt) => acond(
                Rc::clone(c),
                rec(p, f, Rc::clone(cons))?,
                rec(p, f, Rc::clone(alt))?),
            _ => e
        };
        Ok(res)
    }
    map_program_functions(&rec, p)
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
    check_main(&ap)?;
    ap = replace_tailcalls(ap)?;

    Ok(ap)
}

#[cfg(test)]
mod test {
    use crate::ast::{Program, Expr, BinOp};
    use super::{annotate, aprogram, afun, aarg, abinop, acall, acond, anumber};

    #[test]
    fn simple() {
        assert_eq!(
            annotate(
                &Program::new()
                .define("main", vec![], Expr::number(1))),
            Ok(
                aprogram(vec![
                    afun("main", 0, anumber(1))
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
                .define("main", vec![], Expr::number(1))
            ),
            Ok(
                aprogram(vec![
                    afun("f", 2, acall("f", vec![aarg(1), aarg(0)], true)),
                    afun("main", 0, anumber(1))
                ])
            )
        );
    }

    #[test]
    fn nullary() {
        let p = Program::new()
            .define("f", vec!["x", "z"],
                    Expr::call("f", vec![Expr::var("y"), Expr::var("x")]))
            .define("y", vec![], Expr::number(1))
            .define("main", vec![], Expr::number(1));
        assert_eq!(
            annotate(&p),
            Ok(
                aprogram(vec![
                    afun(
                        "f",
                        2,
                        acall("f", vec![
                            acall("y", vec![], false),
                            aarg(0),
                        ], true)),
                    afun("y", 0, anumber(1)),
                    afun("main", 0, anumber(1)),
                ])
            )
        );
    }

    #[test]
    fn arg_over_fun_precedence() {
        let p = Program::new()
            .define("f", vec!["x"],
                    Expr::call("f", vec![Expr::var("x")]))
            .define("x", vec![], Expr::number(1))
            .define("main", vec![], Expr::number(1));
        assert_eq!(
            annotate(&p),
            Ok(
                aprogram(vec![
                    afun(
                        "f",
                        1,
                        acall("f", vec![aarg(0)], true)),
                    afun("x", 0, anumber(1)),
                    afun("main", 0, anumber(1)),
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

    #[test]
    fn no_main() {
        assert_eq!(
            annotate(
                &Program::new()
                .define("f", vec!["x"], Expr::number(0))),
            Err("main function must be defined".to_string())
        );
    }

    #[test]
    fn bad_main_arity() {
        assert_eq!(
            annotate(
                &Program::new()
                .define("main", vec!["x"], Expr::number(0))),
            Err("main function must take no arguments".to_string())
        );
    }

    #[test]
    fn tail_calls() {
        let ap = annotate(
            &Program::new()
            .define("main", vec![], Expr::number(0))
            .define("f", vec!["x"],
                    Expr::call("f", vec![Expr::var("x")]))
            .define("g", vec!["x"],
                    Expr::cond(
                        Expr::call("g", vec![Expr::number(1)]),
                        Expr::call("g", vec![Expr::number(2)]),
                        Expr::call("g", vec![Expr::number(3)])))
            .define("h", vec!["x"],
                    Expr::cond(
                        Expr::number(1),
                        Expr::plus(
                            Expr::number(2),
                            Expr::call("h", vec![Expr::number(3)])),
                        Expr::plus(
                            Expr::number(4),
                            Expr::call("h", vec![Expr::number(5)]))))
            .define("i", vec!["x"],
                    Expr::plus(
                        Expr::call("i", vec![Expr::number(1)]),
                        Expr::call("i", vec![Expr::number(2)])))
            .define("j", vec!["x"],
                    Expr::call("j", vec![
                        Expr::call("j", vec![Expr::number(2)])
                    ]))
            .define("k", vec!["x"],
                    Expr::call("k", vec![
                        Expr::plus(
                            Expr::number(1),
                            Expr::call("k", vec![Expr::number(2)]))
                    ]))
        ).unwrap();

        assert_eq!(
            ap.get("f"),
            Some(&afun("f", 1, acall("f", vec![aarg(0)], true))));

        assert_eq!(
            ap.get("g"),
            Some(&afun("g", 1, acond(
                acall("g", vec![anumber(1)], false),
                acall("g", vec![anumber(2)], true),
                acall("g", vec![anumber(3)], true)))));

        assert_eq!(
            ap.get("h"),
            Some(&afun("h", 1, acond(
                anumber(1),
                abinop(
                    BinOp::Plus,
                    anumber(2),
                    acall("h", vec![anumber(3)], false)),
                abinop(
                    BinOp::Plus,
                    anumber(4),
                    acall("h", vec![anumber(5)], false))))));

        assert_eq!(
            ap.get("i"),
            Some(&afun("i", 1,
                abinop(
                    BinOp::Plus,
                    acall("i", vec![anumber(1)], false),
                    acall("i", vec![anumber(2)], false)))));

        assert_eq!(
            ap.get("j"),
            Some(&afun("j", 1,
                 acall("j",
                       vec![acall("j", vec![anumber(2)], false)],
                       true))));

        assert_eq!(
            ap.get("k"),
            Some(&afun("k", 1,
                acall("k",
                      vec![
                          abinop(
                              BinOp::Plus,
                              anumber(1),
                              acall("k", vec![anumber(2)], false))
                      ],
                      true))));
    }
}
