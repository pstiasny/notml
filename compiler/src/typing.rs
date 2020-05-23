use std::collections::HashMap;
use std::rc::Rc;

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
        Err(format!("Expected {:?}, got {:?}", ty, tree.expr_type()))
    }
}

pub fn type_expr(e: &AAST, sig: &AFunSig, p: &AProgram) -> Result<TAST, String> {
    fn f(t: &AExpr<TAST>, sig: &AFunSig, p: &AProgram) -> Result<Type, String> {
        use AExpr::*;
        use Type::*;
        let res = match t {
            Number(_) => Int,
            BinOp(_op, l, r) => {
                assert_type(l, &Int)?;
                assert_type(r, &Int)?;
                Int
            }
            Call(fname, args, _call_type) => {
                let called_fun = p.get(fname).unwrap();
                if args.len() as u8 != called_fun.arity() {
                    return Err(format!(
                        "call to {} has {} arguments, expected {}",
                        fname, args.len(), called_fun.arity()));
                }
                for (arg, ref fun_arg_type) in args.iter()
                    .zip(called_fun.signature().arg_types.iter())
                {
                    assert_type(arg, fun_arg_type)?;
                }
                *called_fun.return_type()
            }
            Arg(i) => sig.arg_types[*i as usize],
            Cond(cond, cons, alt) => {
                assert_type(cond, &Int)?;
                if cons.expr_type() != alt.expr_type() {
                    return Err("inconsistent consequent and alternative types".to_string())
                }
                cons.expr_type()
            }
            Seq(exprs) => exprs.last().unwrap().expr_type(),
        };
        Ok(res)
    }

    try_annotate(|t| f(t, sig, p), e)
}

pub fn type_program(prog: AProgram) -> Result<TProgram, String> {
    let typed_program = map_program_functions(
        &|prog, fun: &AFun, expr| type_expr(expr, fun.signature(), prog),
        prog)?;

    for fun in typed_program.values() {
        assert_type(fun.code(), &fun.signature().return_type)?;
    }

    Ok(typed_program)
}


#[cfg(test)]
mod test {
    use crate::ast::BinOp;
    use crate::sem::{
        Type, CallType,
        anumber, abinop, acall, aprogram, afun,
        gnumber, gbinop, gcall
    };
    use super::type_program;

    #[test]
    fn typing() {
        let prog = aprogram(vec![
            afun("foo", vec![Type::Int], Type::Int, anumber(3)),
            afun("bar", vec![Type::Int], Type::Int, acall(
                "foo",
                vec![abinop(BinOp::Plus, anumber(1), anumber(2))],
                CallType::Regular)),
        ]);

        let result = type_program(prog).unwrap();

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
