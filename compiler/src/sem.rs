use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinOp, Expr, Program};


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CallType {
    Regular,
    Tail,
    Native,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AExpr<T> {
    Number(i32),
    BinOp(BinOp, T, T),
    Call(String, Vec<T>, CallType),
    Arg(u8),
    Cond(T, T, T),
    Seq(Vec<T>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AAST(pub Rc<AExpr<AAST>>);

#[derive(Debug, PartialEq)]
pub struct AFunSig {
    pub name: String,
    pub arity: u8,
    pub native: bool,
}

#[derive(Debug, PartialEq)]
pub struct AFun(Rc<AFunSig>, AAST);

pub type AProgram = HashMap<String, Rc<AFun>>;

impl<T> AExpr<T> {
    fn try_map<U, F>(&self, f: &F) -> Result<AExpr<U>, String>
        where F: Fn(&T) -> Result<U, String>
    {
        use AExpr::*;

        let node = match self {
            Number(x) => Number(*x),
            BinOp(op, l, r) => BinOp(*op, f(&l)?, f(&r)?),
            Call(ref name, ref args, ref tail) => {
                let argsm: Result<Vec<U>, String> = args
                    .iter()
                    .map(f)
                    .collect();
                Call(name.clone(), argsm?, *tail)
            }
            Arg(i) => Arg(*i),
            Cond(c, cons, alt) => Cond(f(&c)?, f(&cons)?, f(&alt)?),
            Seq(exprs) => {
                let exprsm: Result<Vec<U>, String> =
                    exprs.iter().map(f).collect();
                Seq(exprsm?)
            },
        };
        Ok(node)
    }
}

impl AFun {
    pub fn signature(&self) -> &AFunSig { &self.0 }
    pub fn name(&self) -> &String       { &self.0.name }
    pub fn arity(&self) -> u8           { self.0.arity }
    pub fn code(&self) -> &AAST         { &self.1 }

    pub fn map<F>(self, f: &F) -> Self
        where F: Fn(AAST) -> AAST
    {
        AFun(Rc::clone(&self.0), f(self.1))
    }
    pub fn try_map<F, T>(&self, f: &F) -> Result<Rc<Self>, T> 
        where F: Fn(&AAST) -> Result<AAST, T>
    {
        Ok(Rc::new(AFun(Rc::clone(&self.0), f(&self.1)?)))
    }
}

pub fn aprogram(fs: Vec<Rc<AFun>>) -> AProgram {
    fs.into_iter().map(move |f| (f.0.name.clone(), Rc::clone(&f))).collect()
}

pub fn afun(name: &str, arity: u8, code: AAST) -> Rc<AFun> {
    let fd = AFunSig {
        name: name.to_string(),
        arity,
        native: false,
    };
    Rc::new(AFun(Rc::new(fd), code))
}

pub fn aast(e: AExpr<AAST>) -> AAST                     { AAST(Rc::new(e)) }
pub fn aarg(i: u8) -> AAST                              { aast(AExpr::Arg(i)) }
pub fn anumber(i: i32) -> AAST                          { aast(AExpr::Number(i)) }
pub fn abinop(op: BinOp, l: AAST, r: AAST) -> AAST      { aast(AExpr::BinOp(op, l, r)) }
pub fn acond(cond: AAST, cons: AAST, alt: AAST) -> AAST { aast(AExpr::Cond(cond, cons, alt)) }
pub fn aseq(exprs: Vec<AAST>) -> AAST                   { aast(AExpr::Seq(exprs)) }
pub fn acall(fname: &str, args: Vec<AAST>, call_type: CallType) -> AAST {
    aast(AExpr::Call(fname.to_string(), args, call_type))
}

fn ast_to_sem(args: &HashMap<String, u8>, e: &Expr) -> AAST {
    use Expr::*;

    match e {
        Number(i) => anumber(*i),
        BinOp(o, l, r) => abinop(*o, ast_to_sem(&args, l), ast_to_sem(&args, r)),
        Call(fname, callargs) => {
            let acallargs = callargs.iter()
                .map(|e| ast_to_sem(&args, &e))
                .collect();
            acall(&fname, acallargs, CallType::Regular)
        },
        Var(s) => {
            args.get(s).map(|&n| aarg(n))
            .unwrap_or(acall(&s, vec![], CallType::Regular))
        },
        Cond(c, cons, alt) =>
            acond(ast_to_sem(&args, c), ast_to_sem(&args, cons), ast_to_sem(&args, alt)),
        Seq(exprs) => {
            let aexprs = exprs.iter()
                .map(|e| ast_to_sem(&args, &e))
                .collect();
            aseq(aexprs)
        }
    }
}

fn collapse<F, T>(f: &F, e: &AAST) -> Result<T, String>
    where F: Fn(&AExpr<T>) -> Result<T, String>
{
    f(&(*e.0).try_map(&|ei| collapse(f, ei))?)
}

fn map_program_functions<F>(f: &F, p: AProgram) -> Result<AProgram, String>
    where F: Fn(&AProgram, &AFun, &AAST) -> Result<AAST, String>
{
    let funs: Vec<Rc<AFun>> = p.iter()
        .map(|(_, af)| (&af).try_map(&|root| f(&p, &af, &root)))
        .collect::<Result<Vec<Rc<AFun>>, String>>()?;
    Ok(aprogram(funs))
}

fn check_calls(
    funs: &HashMap<String, Rc<AFunSig>>,
    p: AProgram
) -> Result<AProgram, String> {
    fn f(funs: &HashMap<String, Rc<AFunSig>>, aexpr: &AExpr<AAST>) -> Result<AAST, String> {
        match aexpr {
            AExpr::Call(fname, args, _) => {
                let fd = funs.get(fname).ok_or(format!("Undefined symbol: {}", fname))?;

                if fd.arity == args.len() as u8 {
                    Ok(
                        acall(&fname,
                              args.iter().cloned().collect(),
                              if fd.native { CallType::Native } else { CallType::Regular }))
                } else {
                    Err(format!(
                        "function {} requires {} arguments, {} given",
                        fname, fd.arity, args.len()))
                }
            },
            _ => Ok(aast(aexpr.clone()))
        }
    }
    map_program_functions(
        &|_, _, root| collapse(&|x| f(&funs, x), &root),
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
    fn rec(p: &AProgram, f: &AFun, t: &AAST) -> Result<AAST, String> {
        let res = match *t.0 {
            AExpr::Call(ref fname, ref args, CallType::Regular) => {
                if fname == f.name() {
                    acall(fname, (*args).iter().cloned().collect(), CallType::Tail)
                } else {
                    t.clone()
                }
            },
            AExpr::Cond(ref c, ref cons, ref alt) => acond(
                c.clone(),
                rec(p, f, cons)?,
                rec(p, f, alt)?),
            AExpr::Seq(ref exprs) => {
                let mut replaced_exprs = exprs.clone();
                let last = replaced_exprs.len() - 1;
                replaced_exprs[last] = rec(p, f, &replaced_exprs[last])?;
                aseq(replaced_exprs)
            }
            _ => t.clone()
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
    funs.insert(
        "pchar".to_string(),
        Rc::new(AFunSig { name: "pchar".to_string(), arity: 1, native: true }));

    let mut ap: AProgram = p.definitions().iter()
        .map(|f| {
            let args = f.arg_indexes();
            let ae = ast_to_sem(&args, &f.code);
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
            Err("Undefined symbol: f".to_string())
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
