use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinOp, Expr, Program};


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CallType {
    Regular,
    Tail,
    Native,
}


#[derive(Debug, PartialEq)]
pub enum AExpr {
    Number(i32),
    BinOp(BinOp, Rc<AExpr>, Rc<AExpr>),
    Call(String, Vec<Rc<AExpr>>, CallType),
    Arg(u8),
    Cond(Rc<AExpr>, Rc<AExpr>, Rc<AExpr>),
    Seq(Vec<Rc<AExpr>>),
}

#[derive(Debug, PartialEq)]
pub struct AFunSig {
    pub name: String,
    pub arity: u8,
    pub native: bool,
}

#[derive(Debug, PartialEq)]
pub struct AFun(Rc<AFunSig>, Rc<AExpr>);

impl AFun {
    pub fn signature(&self) -> &AFunSig {
        &self.0
    }
    pub fn name(&self) -> &String {
        &self.0.name
    }
    pub fn arity(&self) -> u8 {
        self.0.arity
    }
    pub fn code(&self) -> &AExpr {
        &self.1
    }
    pub fn map(self, f: &dyn Fn(Rc<AExpr>) -> Rc<AExpr>) -> Self {
        AFun(Rc::clone(&self.0), f(self.1))
    }
    pub fn try_map<T>(
        &self,
        f: &dyn Fn(Rc<AExpr>) -> Result<Rc<AExpr>, T>
    ) -> Result<Rc<Self>, T> {
        Ok(Rc::new(AFun(Rc::clone(&self.0), f(Rc::clone(&self.1))?)))
    }
}

pub type AProgram = HashMap<String, Rc<AFun>>;

pub fn aprogram(fs: Vec<Rc<AFun>>) -> AProgram {
    fs.into_iter().map(move |f| (f.0.name.clone(), Rc::clone(&f))).collect()
}

pub fn afun(name: &str, arity: u8, code: Rc<AExpr>) -> Rc<AFun> {
    let fd = AFunSig {
        name: name.to_string(),
        arity,
        native: false,
    };
    Rc::new(AFun(Rc::new(fd), code))
}

pub fn aarg(i: u8) -> Rc<AExpr> {
    Rc::new(AExpr::Arg(i))
}

pub fn anumber(i: i32) -> Rc<AExpr> {
    Rc::new(AExpr::Number(i))
}

pub fn acall(fname: &str, args: Vec<Rc<AExpr>>, call_type: CallType) -> Rc<AExpr> {
    Rc::new(AExpr::Call(fname.to_string(), args, call_type))
}

pub fn abinop(op: BinOp, l: Rc<AExpr>, r: Rc<AExpr>) -> Rc<AExpr> {
    Rc::new(AExpr::BinOp(op, l, r))
}

pub fn acond(cond: Rc<AExpr>, cons: Rc<AExpr>, alt: Rc<AExpr>) -> Rc<AExpr> {
    Rc::new(AExpr::Cond(cond, cons, alt))
}

pub fn aseq(exprs: Vec<Rc<AExpr>>) -> Rc<AExpr> {
    Rc::new(AExpr::Seq(exprs))
}

fn annotate_expr(
    funs: &HashMap<String, Rc<AFunSig>>,
    args: &HashMap<String, u8>,
    e: &Expr
) -> Result<Rc<AExpr>, String> {
    match e {
        Expr::Number(i) => Ok(anumber(*i)),
        Expr::BinOp(o, l, r) => Ok(abinop(
            *o,
            annotate_expr(&funs, &args, l)?,
            annotate_expr(&funs, &args, r)?)),
        Expr::Call(fname, callargs) => {
            let acallargs: Result<Vec<Rc<AExpr>>, String> = callargs.iter()
                .map(|e| annotate_expr(&funs, &args, &e))
                .collect();
            let fd = funs.get(fname).ok_or(format!("Undefined function: {}", fname))?;
            Ok(acall(&fd.name, acallargs?,  if fd.native {CallType::Native} else {CallType::Regular}))
        },
        Expr::Var(s) => {
            args.get(s).map(|&n| aarg(n))
            .or_else(|| funs.get(s).map(|fd| acall(&fd.name, vec![], CallType::Regular)))
            .ok_or(format!("Undefined symbol: {}", s))
        },
        Expr::Cond(c, cons, alt) => Ok(acond(
            annotate_expr(&funs, &args, c)?,
            annotate_expr(&funs, &args, cons)?,
            annotate_expr(&funs, &args, alt)?)),
        Expr::Seq(exprs) => {
            let aexprs: Result<Vec<Rc<AExpr>>, String> = exprs.iter()
                .map(|e| annotate_expr(&funs, &args, &e))
                .collect();
            Ok(aseq(aexprs?))
        }
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
            f(Rc::clone(alt))?),
        AExpr::Seq(ref exprs) => {
            let exprsm: Result<Vec<Rc<AExpr>>, String> = 
                exprs.iter().cloned().map(f).collect();
            aseq(exprsm?)
        },
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

fn check_calls(
    funs: &HashMap<String, Rc<AFunSig>>,
    p: AProgram
) -> Result<AProgram, String> {
    fn f(funs: &HashMap<String, Rc<AFunSig>>, aexpr: Rc<AExpr>) -> Result<Rc<AExpr>, String> {
        match *aexpr {
            AExpr::Call(ref fname, ref args, _) => {
                let fd = funs.get(fname).ok_or(format!("Undefined function: {}", fname))?;

                if fd.arity == args.len() as u8 {
                    Ok(aexpr)
                } else {
                    Err(format!(
                        "function {} requires {} arguments, {} given",
                        fname, fd.arity, args.len()))
                }
            },
            _ => Ok(aexpr)
        }
    }
    map_program_functions(
        &|_, _, root| traverse(&|x| f(&funs, x), root),
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
            AExpr::Call(ref fname, ref args, CallType::Regular) => {
                if fname == f.name() {
                    acall(fname, (*args).iter().cloned().collect(), CallType::Tail)
                } else {
                    e
                }
            },
            AExpr::Cond(ref c, ref cons, ref alt) => acond(
                Rc::clone(c),
                rec(p, f, Rc::clone(cons))?,
                rec(p, f, Rc::clone(alt))?),
            AExpr::Seq(ref exprs) => {
                let mut replaced_exprs = exprs.clone();
                let last = replaced_exprs.len() - 1;
                replaced_exprs[last] = rec(p, f, Rc::clone(&replaced_exprs[last]))?;
                aseq(replaced_exprs)
            }
            _ => e
        };
        Ok(res)
    }
    map_program_functions(&rec, p)
}

pub fn annotate(p: &Program) -> Result<AProgram, String> {
    let mut funs: HashMap<String, Rc<AFunSig>> = p.definitions().iter()
        .map(|f| {
            (
                f.fname.clone(),
                Rc::new(AFunSig {
                    name: f.fname.clone(),
                    arity: f.arg_names.len() as u8,
                    native: false
                })
            )
        })
        .collect();

    // TODO: check for collisions with exisiting definitions
    funs.insert(
        "print".to_string(),
        Rc::new(AFunSig { name: "print".to_string(), arity: 1, native: true }));

    let mut ap: AProgram = p.definitions().iter()
        .map(|f| {
            let args = f.arg_indexes();
            let ae = annotate_expr(&funs, &args, &f.code)?;
            let sig = funs.get(&f.fname).unwrap();
            let fun = Rc::new(AFun(Rc::clone(sig), ae));
            Ok((f.fname.clone(), fun))
        })
        .collect::<Result<AProgram, String>>()?;

    ap = check_calls(&funs, ap)?;
    check_main(&ap)?;
    ap = replace_tailcalls(ap)?;

    Ok(ap)
}

#[cfg(test)]
mod test {
    use crate::ast::{Program, Expr, BinOp};
    use super::{CallType, annotate, aprogram, afun, aarg, abinop, acall, acond, anumber, aseq};

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
                    afun("f", 2, acall("f", vec![aarg(1), aarg(0)], CallType::Tail)),
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
                            acall("y", vec![], CallType::Regular),
                            aarg(0),
                        ], CallType::Tail)),
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
                        acall("f", vec![aarg(0)], CallType::Tail)),
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
            .define("l", vec!["x"],
                    Expr::seq(vec![
                        Expr::call("l", vec![Expr::number(1)]),
                        Expr::call("l", vec![Expr::number(2)]),
                    ]))
        ).unwrap();

        assert_eq!(
            ap.get("f"),
            Some(&afun("f", 1, acall("f", vec![aarg(0)], CallType::Tail))));

        assert_eq!(
            ap.get("g"),
            Some(&afun("g", 1, acond(
                acall("g", vec![anumber(1)], CallType::Regular),
                acall("g", vec![anumber(2)], CallType::Tail),
                acall("g", vec![anumber(3)], CallType::Tail)))));

        assert_eq!(
            ap.get("h"),
            Some(&afun("h", 1, acond(
                anumber(1),
                abinop(
                    BinOp::Plus,
                    anumber(2),
                    acall("h", vec![anumber(3)], CallType::Regular)),
                abinop(
                    BinOp::Plus,
                    anumber(4),
                    acall("h", vec![anumber(5)], CallType::Regular))))));

        assert_eq!(
            ap.get("i"),
            Some(&afun("i", 1,
                abinop(
                    BinOp::Plus,
                    acall("i", vec![anumber(1)], CallType::Regular),
                    acall("i", vec![anumber(2)], CallType::Regular)))));

        assert_eq!(
            ap.get("j"),
            Some(&afun("j", 1,
                 acall("j",
                       vec![acall("j", vec![anumber(2)], CallType::Regular)],
                       CallType::Tail))));

        assert_eq!(
            ap.get("k"),
            Some(&afun("k", 1,
                acall("k",
                      vec![
                          abinop(
                              BinOp::Plus,
                              anumber(1),
                              acall("k", vec![anumber(2)], CallType::Regular))
                      ],
                      CallType::Tail))));

        assert_eq!(
            ap.get("l"),
            Some(&afun("l", 1,
                aseq(vec![
                    acall("l", vec![anumber(1)], CallType::Regular),
                    acall("l", vec![anumber(2)], CallType::Tail),
                ]))));
    }

    #[test]
    fn native_calls() {
        assert_eq!(
            annotate(
                &Program::new()
                .define("main", vec![],
                        Expr::call("print", vec![Expr::number(1)]))),
            Ok(
                aprogram(vec![
                    afun("main",
                         0,
                         acall("print", vec![anumber(1)], CallType::Native))
                ])
            )
        );
    }
}
