use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::BinOp;
use crate::sem::{
    GAST, AAST, Type, AFunSig, AFun, AExpr, GFun, AProgram,
    map_program_functions, try_annotate,
};

pub type TAST = GAST<Type>;
pub type TFun = GFun<Type>;
pub type TProgram = HashMap<String, Rc<TFun>>;

impl TAST {
    pub fn expr_type(&self) -> Type { self.1 }
}

fn assert_type(tree: &TAST, ty: &Type) -> Result<(), String> {
    if tree.expr_type() == *ty {
        Ok(())
    } else {
        Err(format!("Expected {:?}, got {:?}: {:?}", ty, tree.expr_type(), tree))
    }
}

pub fn type_expr(e: &AAST, sig: &AFunSig, funs: &HashMap<String, Rc<AFunSig>>) -> Result<TAST, String> {
    
    fn f(t: &AExpr<TAST>, sig: &AFunSig, funs: &HashMap<String, Rc<AFunSig>>) -> Result<Type, String> {
        use Type::*;
        let res = match t {
            AExpr::Number(_) => Int,
            AExpr::BinOp(op, l, r) => {
                match op {
                    BinOp::Plus | BinOp::Minus | BinOp::Times | BinOp::Div | BinOp::Mod => {
                        assert_type(l, &Int)?;
                        assert_type(r, &Int)?;
                        Int
                    }
                    BinOp::Eq | BinOp::Gt | BinOp::Gte | BinOp::Lt | BinOp::Lte => {
                        assert_type(l, &Int)?;
                        assert_type(r, &Int)?;
                        Bool
                    }
                    BinOp::And | BinOp::Or => {
                        assert_type(l, &Bool)?;
                        assert_type(r, &Bool)?;
                        Bool
                    }
                }
            }
            AExpr::Call(fname, args, _call_type) => {
                let called_fun = funs.get(fname).unwrap();
                if args.len() as u8 != called_fun.arity {
                    return Err(format!(
                        "call to {} has {} arguments, expected {}",
                        fname, args.len(), called_fun.arity));
                }
                for (arg, ref fun_arg_type) in args.iter()
                    .zip(called_fun.arg_types.iter())
                {
                    assert_type(arg, fun_arg_type)?;
                }
                called_fun.return_type
            }
            AExpr::Arg(i) => sig.arg_types[*i as usize],
            AExpr::Cond(cond, cons, alt) => {
                assert_type(cond, &Bool)?;
                if cons.expr_type() != alt.expr_type() {
                    return Err("inconsistent consequent and alternative types".to_string())
                }
                cons.expr_type()
            }
            AExpr::Seq(exprs) => exprs.last().unwrap().expr_type(),
        };
        Ok(res)
    }

    try_annotate(|t| f(t, sig, funs), e)
}

pub fn type_program(prog: AProgram, runtime: &Vec<Rc<AFunSig>>) -> Result<TProgram, String> {
    let mut funs: HashMap<String, Rc<AFunSig>> = HashMap::new();
    funs.extend(prog.values().map(|f| (f.name().clone(), f.signature_rc())));
    funs.extend(runtime.iter().map(|fsig| (fsig.name.clone(), Rc::clone(fsig))));

    let typed_program = map_program_functions(
        &|_, fun: &AFun, expr| type_expr(expr, fun.signature(), &funs),
        prog)?;

    for fun in typed_program.values() {
        assert_type(fun.code(), &fun.signature().return_type)?;
    }

    Ok(typed_program)
}


#[cfg(test)]
mod test {
    use std::rc::Rc;
    use crate::ast::BinOp;
    use crate::sem::{
        Type, CallType, AFunSig,
        anumber, abinop, acall, aprogram, afun,
        gnumber, gbinop, gcall
    };
    use super::type_program;

    fn get_rt() -> Vec<Rc<AFunSig>> {
        vec![
            Rc::new(AFunSig {
                name: "print".to_string(),
                arity: 1,
                arg_types: vec![Type::Int],
                return_type: Type::Int,
                native: true
            }),
        ]
    }

    #[test]
    fn typing() {
        let prog = aprogram(vec![
            afun("foo", vec![Type::Int], Type::Int, anumber(3)),
            afun("bar", vec![Type::Int], Type::Int, acall(
                "foo",
                vec![abinop(BinOp::Plus, anumber(1), anumber(2))],
                CallType::Regular)),
        ]);

        let result = type_program(prog, &get_rt()).unwrap();

        assert_eq!(
            result.get("bar").unwrap().code(),
            &gcall(
                "foo",
                vec![
                    gbinop(
                        BinOp::Plus,
                        gnumber(1, Type::Int),
                        gnumber(2, Type::Int),
                        Type::Int)
                ],
                CallType::Regular,
                Type::Int));
    }
}
