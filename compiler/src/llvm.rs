use std::rc::Rc;
use crate::ast::BinOp;
use crate::sem::{GProgram, Type, AFunSig, CallType};
use crate::ssa::{Op, Block, BlockExit};


fn type_str(t: &Type) -> &'static str {
    match *t {
        Type::Int => "i64",
        Type::Bool => "i1",
        Type::Object => "i8*",
    }
}


fn emit_function_ir(signature: &AFunSig, code: &Vec<Block>, out: &mut String) {
    // constants
    for ref block in code {
        for ref op in &block.ops {
            if let Op::Const { dst, value } = op {
                out.push_str(&format!(
                    "@{}.c{} = private unnamed_addr constant i64 {}\n",
                    signature.name, dst, value));
            }
        }
    }

    // function signature
    out.push_str(&format!("define {} @{}(", type_str(&signature.return_type), signature.name));
    out.push_str(
        &(0..signature.arity)
        .map(|i| format!("{} %t{}", type_str(&signature.arg_types[i as usize]), i))
        .collect::<Vec<_>>().join(", "));
    out.push_str(") {\n");

    // output blocks of ops
    for (block_id, ref block) in code.iter().enumerate() {
        out.push_str(&format!("B{}:\n", block_id));

        for ref op in &block.ops {
            match op {
                Op::Binary { dst, left, right, operator } => {
                    let op_str = match operator {
                        BinOp::Minus => "sub i64",
                        BinOp::Plus => "add i64",
                        BinOp::Times => "mul i64",
                        BinOp::Div => "sdiv i64",
                        BinOp::Mod => "srem i64",
                        BinOp::Eq => "icmp eq i64",
                        BinOp::Gt => "icmp sgt i64",
                        BinOp::Gte => "icmp sge i64",
                        BinOp::Lt => "icmp slt i64",
                        BinOp::Lte => "icmp sle i64",
                        BinOp::And => "and i1",
                        BinOp::Or => "or i1",
                    };
                    out.push_str(&format!(
                        "  %t{} = {} %t{}, %t{}\n",
                        dst, op_str, left, right));
                }
                Op::Phi { dst, left, left_block, right, right_block } => {
                    out.push_str(&format!(
                        "  %t{} = phi i64 [%t{}, %B{}], [%t{}, %B{}]\n",
                        dst,
                        left, left_block,
                        right, right_block));
                }
                Op::Const { dst, value: _ } => {
                    out.push_str(&format!(
                        "  %t{} = load i64, i64* @{}.c{}\n",
                        dst, signature.name, dst));
                }
                Op::Call { dst, function, args, call_type, arg_types, return_type } => {
                    let fname = match call_type {
                        CallType::Native => format!("rt_{}", function),
                        _ => function.clone(),
                    };
                    let args_str = arg_types.iter().zip(args.iter())
                        .map(|(arg_type, arg_reg)|
                            format!("{} %t{}", type_str(arg_type), arg_reg))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let prefix = match call_type {
                        CallType::Tail => "tail ",
                        _ => "",
                    };
                    out.push_str(&format!(
                        "  %t{} = {}call {} @{}({})\n",
                        dst, prefix, type_str(&return_type), fname, args_str));
                }
            }
        }

        match block.exit {
            BlockExit::Return(reg) => {
                out.push_str(&format!("  ret {} %t{}\n", type_str(&signature.return_type), reg));
            }
            BlockExit::UnconditionalJump(block_id) => {
                out.push_str(&format!("  br label %B{}\n", block_id));
            }
            BlockExit::Branch(reg, block_positive, block_negative) => {
                out.push_str(&format!(
                    "  br i1 %t{}, label %B{}, label %B{}\n",
                    reg, block_positive, block_negative));
            }
        }
    }

    out.push_str("}\n");
}

pub fn emit_runtime_declarations(out: &mut String, runtime: &Vec<Rc<AFunSig>>) {
    for fsig in runtime {
        let args_str = fsig.arg_types.iter().enumerate()
            .map(|(i, ty)| format!("{} %t{}", type_str(ty), i))
            .collect::<Vec<_>>()
            .join(", ");
        out.push_str(&format!(
            "declare {} @rt_{}({})\n",
            type_str(&fsig.return_type), fsig.name, args_str));
    }
}

pub fn emit_ir(p: &GProgram<Type>, mut out: &mut String, runtime: &Vec<Rc<AFunSig>>) {
    emit_runtime_declarations(out, runtime);
    out.push_str("\n");

    for ref function in p.values() {
        let blocks = crate::ssa::emit_function(&function);
        emit_function_ir(function.signature(), &blocks, &mut out);
    }
}


#[cfg(test)]
mod test {
    use crate::sem::{Type, AFunSig, CallType};
    use crate::ast::BinOp;
    use crate::ssa::{Block, Op, BlockExit};
    use super::emit_function_ir;

    #[test]
    fn simple() {
        let sig = AFunSig {
            name: "foo".to_string(),
            arity: 1,
            arg_types: vec![Type::Int],
            return_type: Type::Int,
            native: false,
        };
        let code = vec![
            Block {
                ops: vec![
                    Op::Const { dst: 2, value: 10 },
                    Op::Binary { dst: 1, left: 2, right: 0, operator: BinOp::Plus },
                ],
                exit: BlockExit::Return(1)
            },
        ];
        let mut out = String::new();

        emit_function_ir(&sig, &code, &mut out);

        assert_eq!(out, "@foo.c2 = private unnamed_addr constant i64 10
define i64 @foo(i64 %t0) {
B0:
  %t2 = load i64, i64* @foo.c2
  %t1 = add i64 %t2, %t0
  ret i64 %t1
}
");
    }

    #[test]
    fn call() {
        let sig = AFunSig {
            name: "cally".to_string(),
            arity: 1,
            arg_types: vec![Type::Int],
            return_type: Type::Int,
            native: false,
        };
        let code = vec![
            Block {
                ops: vec![
                    Op::Call {
                        dst: 3, function: "bar".to_string(), args: vec![0],
                        call_type: CallType::Regular,
                        arg_types: vec![Type::Int], return_type: Type::Bool,
                    },
                    Op::Call {
                        dst: 2, function: "foo".to_string(), args: vec![3],
                        call_type: CallType::Native,
                        arg_types: vec![Type::Bool], return_type: Type::Int,
                    },
                    Op::Call {
                        dst: 1, function: "cally".to_string(), args: vec![2],
                        call_type: CallType::Tail,
                        arg_types: vec![Type::Int], return_type: Type::Int,
                    },
                ],
                exit: BlockExit::Return(1)
            },
        ];
        let mut out = String::new();

        emit_function_ir(&sig, &code, &mut out);

        assert_eq!(out, "define i64 @cally(i64 %t0) {
B0:
  %t3 = call i1 @bar(i64 %t0)
  %t2 = call i64 @rt_foo(i1 %t3)
  %t1 = tail call i64 @cally(i64 %t2)
  ret i64 %t1
}
");
    }

    #[test]
    fn branch() {
        let sig = AFunSig {
            name: "branchy".to_string(),
            arity: 2,
            arg_types: vec![Type::Int, Type::Int],
            return_type: Type::Int,
            native: false,
        };
        let code = vec![
            Block {
                ops: vec![
                    Op::Binary { dst: 2, left: 0, right: 1, operator: BinOp::Gt },
                ],
                exit: BlockExit::Branch(2, 1, 2)
            },
            Block {
                ops: vec![
                    Op::Binary { dst: 3, left: 0, right: 1, operator: BinOp::Plus },
                ],
                exit: BlockExit::UnconditionalJump(3)
            },
            Block {
                ops: vec![],
                exit: BlockExit::UnconditionalJump(3)
            },
            Block {
                ops: vec![
                    Op::Phi { dst: 4, left: 3, left_block: 1, right: 0, right_block: 2 },
                ],
                exit: BlockExit::Return(4)
            },
        ];
        let mut out = String::new();

        emit_function_ir(&sig, &code, &mut out);

        assert_eq!(out, "define i64 @branchy(i64 %t0, i64 %t1) {
B0:
  %t2 = icmp sgt i64 %t0, %t1
  br i1 %t2, label %B1, label %B2
B1:
  %t3 = add i64 %t0, %t1
  br label %B3
B2:
  br label %B3
B3:
  %t4 = phi i64 [%t3, %B1], [%t0, %B2]
  ret i64 %t4
}
");
   }
}
