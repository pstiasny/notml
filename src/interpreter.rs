use std::collections::HashMap;

use crate::ast::*;


struct EvalEnv<'a> {
    functions: &'a HashMap<String, &'a FunDefinition>,
    locals: HashMap<String, i32>,
}

impl<'a> EvalEnv<'a> {
    fn new(functions: &'a HashMap<String, &'a FunDefinition>) -> EvalEnv<'a> {
        EvalEnv { functions, locals: HashMap::new() }
    }

    fn frame(&self, arg_names: &'a Vec<String>, args: &'a Vec<i32>) -> EvalEnv<'a> {
        let mut locals = HashMap::new();
        for (arg_name, arg_value) in arg_names.iter().zip(args.iter()) {
            locals.insert(arg_name.to_string(), *arg_value);
        }
        EvalEnv { functions: &self.functions, locals }
    }

    fn get_definition(&self, fname: &str) -> Result<&FunDefinition, &'static str> {
        match self.functions.get(fname) {
            Some(fdef) => Ok(fdef),
            None => Err("Undefined function")
        }
    }
}


fn eval_call<'a>(name: &str, args: &'a Vec<i32>, env: &'a EvalEnv<'a>) -> Result<i32, &'static str> {
    let ref def = env.get_definition(&name)?;
    if args.len() != def.arg_names.len() {
        return Err("invalid number of arguments");
    }
    let next_env = env.frame(&def.arg_names, &args);
    eval_expr(&def.code, &next_env)
}

fn eval_args<'a>(args: &'a Vec<Expr>, env: &'a EvalEnv<'a>) -> Result<Vec<i32>, &'static str> {
    let mut evargs = Vec::new();
    for e in args.iter() {
        let evarg = eval_expr(&e, &env)?;
        evargs.push(evarg);
    }
    Ok(evargs)
}

fn eval_expr<'a>(e: &'a Expr, env: &'a EvalEnv<'a>) -> Result<i32, &'static str> {
    match *e {
        Expr::Number(i) => Ok(i),
        Expr::Plus(ref l, ref r) =>
            Ok(eval_expr(l, env)? + eval_expr(r, env)?),
        Expr::Minus(ref l, ref r) =>
            Ok(eval_expr(l, env)? - eval_expr(r, env)?),
        Expr::Times(ref l, ref r) =>
            Ok(eval_expr(l, env)? * eval_expr(r, env)?),
        Expr::Call(ref name, ref args) => {
            let evargs = eval_args(&args, &env)?;
            eval_call(&name, &evargs, &env)
        }
        Expr::Var(ref name) => {
            match env.locals.get(name) {
                Some(val) => Ok(*val),
                // glbobal constant treated as nullary function call
                None => eval_call(name, &Vec::new(), &env)
            }
        }
        Expr::Cond(ref c, ref cons, ref alt) => {
            if eval_expr(c, env)? > 0 {
                eval_expr(cons, env)
            } else {
                eval_expr(alt, env)
            }
        }
    }
}

fn collect_functions<'a>(p: &'a Program) -> HashMap<String, &'a FunDefinition>
{
    let mut functions = HashMap::new();

    for d in p.definitions() {
        functions.insert(d.fname.to_string(), d);
    }

    functions
}

pub fn eval(p: &Program) -> Result<i32, &'static str> {
    let functions = collect_functions(p);
    let env = EvalEnv::new(&functions);

    eval_call("main", &Vec::new(), &env)
}
