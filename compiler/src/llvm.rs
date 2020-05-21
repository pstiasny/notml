use crate::ast::BinOp;
use crate::sem::{AProgram, AFunSig, CallType};
use crate::ssa::{Op, Block, BlockExit};


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
    out.push_str(&format!("define i64 @{}(", signature.name));
    out.push_str(
        &(0..signature.arity).map(|i| format!("i64 %t{}", i)).collect::<Vec<_>>().join(", "));
    out.push_str(") {\n");

    // output blocks of ops
    for (block_id, ref block) in code.iter().enumerate() {
        out.push_str(&format!("B{}:\n", block_id));

        for ref op in &block.ops {
            match op {
                Op::Binary { dst, left, right, operator } => {
                    let op_str = match operator {
                        BinOp::Minus => "sub",
                        BinOp::Plus => "add",
                        BinOp::Times => "mul",
                    };
                    out.push_str(&format!("  %t{} = {} i64 %t{}, %t{}\n", dst, op_str, left, right));
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
                Op::Call { dst, function, args, call_type } => {
                    let fname = match call_type {
                        CallType::Native => format!("rt_{}", function),
                        _ => function.clone(),
                    };
                    let args_str = args.iter()
                        .map(|arg_reg| format!("i64 %t{}", arg_reg))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let prefix = match call_type {
                        CallType::Tail => "tail ",
                        _ => "",
                    };
                    out.push_str(&format!(
                        "  %t{} = {}call i64 @{}({})\n",
                        dst, prefix, fname, args_str));
                }
            }
        }

        match block.exit {
            BlockExit::Return(reg) => {
                out.push_str(&format!("  ret i64 %t{}\n", reg));
            }
            BlockExit::UnconditionalJump(block_id) => {
                out.push_str(&format!("  br label %B{}\n", block_id));
            }
            BlockExit::Branch(reg, block_positive, block_negative) => {
                out.push_str(&format!(
                    "  %B{}.cond = icmp sge i64 %t{}, 0\n",
                    block_id, reg));
                out.push_str(&format!(
                    "  br i1 %B{}.cond, label %B{}, label %B{}\n",
                    block_id, block_positive, block_negative));
            }
        }
    }

    out.push_str("}\n");
}

pub fn emit_ir(p: &AProgram, mut out: &mut String) {
    out.push_str("declare i64 @rt_print(i64)\ndeclare i64 @rt_pchar(i64)\n\n");

    for ref function in p.values() {
        let blocks = crate::ssa::emit_function(&function);
        emit_function_ir(function.signature(), &blocks, &mut out);
    }
}


#[cfg(test)]
mod test {
    use crate::sem::{AFunSig, CallType};
    use crate::ast::BinOp;
    use crate::ssa::{Block, Op, BlockExit};
    use super::emit_function_ir;

    #[test]
    fn simple() {
        let sig = AFunSig {
            name: "foo".to_string(),
            arity: 1,
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
            native: false,
        };
        let code = vec![
            Block {
                ops: vec![
                    Op::Call { dst: 3, function: "bar".to_string(), args: vec![0], call_type: CallType::Regular },
                    Op::Call { dst: 2, function: "print".to_string(), args: vec![3], call_type: CallType::Native },
                    Op::Call { dst: 1, function: "cally".to_string(), args: vec![2], call_type: CallType::Tail },
                ],
                exit: BlockExit::Return(1)
            },
        ];
        let mut out = String::new();

        emit_function_ir(&sig, &code, &mut out);

        assert_eq!(out, "define i64 @cally(i64 %t0) {
B0:
  %t3 = call i64 @bar(i64 %t0)
  %t2 = call i64 @rt_print(i64 %t3)
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
            native: false,
        };
        let code = vec![
            Block {
                ops: vec![],
                exit: BlockExit::Branch(0, 1, 2)
            },
            Block {
                ops: vec![
                    Op::Binary { dst: 2, left: 0, right: 1, operator: BinOp::Plus },
                ],
                exit: BlockExit::UnconditionalJump(3)
            },
            Block {
                ops: vec![],
                exit: BlockExit::UnconditionalJump(3)
            },
            Block {
                ops: vec![
                    Op::Phi { dst: 4, left: 2, left_block: 1, right: 0, right_block: 2 },
                ],
                exit: BlockExit::Return(4)
            },
        ];
        let mut out = String::new();

        emit_function_ir(&sig, &code, &mut out);

        assert_eq!(out, "define i64 @branchy(i64 %t0, i64 %t1) {
B0:
  %B0.cond = icmp sge i64 %t0, 0
  br i1 %B0.cond, label %B1, label %B2
B1:
  %t2 = add i64 %t0, %t1
  br label %B3
B2:
  br label %B3
B3:
  %t4 = phi i64 [%t2, %B1], [%t0, %B2]
  ret i64 %t4
}
");
   }
}
