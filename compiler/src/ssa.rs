use crate::ast::BinOp;
use crate::sem::{AFun, AExpr, CallType};


type Register = u64;
type BlockId = usize;

#[derive(Debug, PartialEq)]
pub enum Op {
    Binary { dst: Register, left: Register, right: Register, operator: BinOp },
    Phi { dst: Register,
          left: Register, left_block: BlockId,
          right: Register, right_block: BlockId },
    Const { dst: Register, value: i32 },
    Call { dst: Register, function: String, args: Vec<Register>, call_type: CallType },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BlockExit {
    Return(Register),
    UnconditionalJump(BlockId),
    Branch(Register, BlockId, BlockId),
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub ops: Vec<Op>,
    pub exit: BlockExit,
}

struct FunctionEnv {
    register_counter: u64,
    blocks: Vec<Block>,
}

impl Op {
    pub fn dst(&self) -> Register {
        match self {
            Self::Binary { dst, left: _, right: _, operator: _ } => *dst,
            Self::Phi { dst, left: _, left_block: _, right: _, right_block: _ } => *dst,
            Self::Const { dst, value: _ } => *dst,
            Self::Call { dst, function: _, args: _, call_type: _ } => *dst,
        }
    }
}

impl Block {
    fn new(exit: BlockExit) -> Block {
        Block {
            ops: Vec::new(),
            exit
        }
    }
}

impl FunctionEnv {
    fn new(function: &AFun) -> FunctionEnv {
        FunctionEnv {
            register_counter: function.arity() as u64,  // registers 0..arity will store arguments
            blocks: vec![ Block::new(BlockExit::Return(function.arity() as u64)) ],
        }
    }

    fn fresh_register(&mut self) -> Register {
        let c = self.register_counter;
        self.register_counter += 1;
        c
    }

    fn current_block_id(&self) -> BlockId {
        self.blocks.len() - 1
    }

    fn current_block_mut<'a>(&'a mut self) -> &'a mut Block {
        self.blocks.last_mut().unwrap()
    }

    fn split_block(&mut self) {
        let block = Block::new(self.blocks.last().unwrap().exit);
        self.blocks.push(block);
    }

    fn push_op(&mut self, op: Op) {
        self.current_block_mut().ops.push(op);
    }
}


fn emit_expr<'a>(e: &AExpr, mut env: &mut FunctionEnv) -> u64 {
    match *e {
        AExpr::Number(value) => {
            let dst = env.fresh_register();
            env.push_op(Op::Const { dst, value });
            dst
        }
        AExpr::BinOp(ref op, ref l, ref r) => {
            let dst = env.fresh_register();
            let left = emit_expr(&l, &mut env);
            let right = emit_expr(&r, &mut env);
            env.push_op(Op::Binary { dst, left, right, operator: *op });
            dst
        }
        AExpr::Call(ref name, ref arg_exprs, ref call_type) => {
            let dst = env.fresh_register();
            let mut args = Vec::new();
            for ref arg_expr in arg_exprs {
                args.push(emit_expr(&arg_expr, &mut env));
            }

            env.push_op(
                Op::Call { dst, function: (*name).clone(), args, call_type: *call_type });
            dst
        }
        AExpr::Arg(idx) => {
            idx as u64
        }
        AExpr::Cond(ref c, ref cons, ref alt) => {
            let dst = env.fresh_register();

            let reg_cond = emit_expr(&c, &mut env);
            let initial_block_id = env.current_block_id();

            env.split_block();
            let cons_first_block_id = env.current_block_id();
            let reg_cons = emit_expr(&cons, &mut env);
            let cons_last_block_id = env.current_block_id();

            env.split_block();
            let alt_first_block_id = env.current_block_id();
            let reg_alt = emit_expr(&alt, &mut env);
            let alt_last_block_id = env.current_block_id();

            env.split_block();
            env.blocks[initial_block_id].exit = BlockExit::Branch(reg_cond, cons_first_block_id, alt_first_block_id);
            env.blocks[cons_last_block_id].exit = BlockExit::UnconditionalJump(env.current_block_id());
            env.blocks[alt_last_block_id].exit = BlockExit::UnconditionalJump(env.current_block_id());
            env.push_op(Op::Phi {
                dst,
                left: reg_cons, left_block: cons_last_block_id,
                right: reg_alt, right_block: alt_last_block_id
            });
            dst
        }
        AExpr::Seq(ref exprs) => {
            let mut dst = 0;
            for ref expr in exprs {
                dst = emit_expr(&expr, &mut env);
            }
            dst
        }
    }
}


pub fn emit_function<'a>(function: &'a AFun) -> Vec<Block> {
    let mut env = FunctionEnv::new(&function);
    let res_reg = emit_expr(&function.code(), &mut env);
    env.current_block_mut().exit = BlockExit::Return(res_reg);
    env.blocks
}

#[cfg(test)]
mod test {
    use super::{Op, BlockExit, Block, emit_function};
    use crate::ast::BinOp;
    use crate::sem::{CallType, abinop, aarg, anumber, afun, acond, acall, aseq};

    #[test]
    fn simple() {
        let fun = afun("foo", 1,
            abinop(BinOp::Plus, anumber(1), aarg(0)));

        assert_eq!(
            emit_function(&fun),
            vec![
                Block {
                    ops: vec![
                        Op::Const { dst: 2, value: 1 },
                        Op::Binary { dst: 1, left: 2, right: 0, operator: BinOp::Plus }
                    ],
                    exit: BlockExit::Return(1),
                }
            ]);
    }

    #[test]
    fn conditional() {
        // foo x =
        //   if x
        //     then
        //       if 10 - x
        //         then 1
        //         else 2
        //     else
        //       if x + 10
        //         then 3
        //         else 4
        let fun = afun("foo", 1,
            acond(
                aarg(0),
                acond(
                    abinop(BinOp::Minus, anumber(10), aarg(0)),
                    anumber(1),
                    anumber(2)),
                acond(
                    abinop(BinOp::Plus, aarg(0), anumber(10)),
                    anumber(3),
                    anumber(4))));

        assert_eq!(
            emit_function(&fun),
            vec![
                Block {  // 0
                    ops: vec![
                    ],
                    exit: BlockExit::Branch(0, 1, 5),  // x > 0 ?
                },
                Block {  // 1
                    ops: vec![
                        Op::Const { dst: 4, value: 10 },
                        Op::Binary { dst: 3, left: 4, right: 0, operator: BinOp::Minus },
                    ],
                    exit: BlockExit::Branch(3, 2, 3)  // 10 - x > 0 ?
                },
                Block {  // 2
                    ops: vec![
                        Op::Const { dst: 5, value: 1 },
                    ],
                    exit: BlockExit::UnconditionalJump(4)
                },
                Block {  // 3
                    ops: vec![
                        Op::Const { dst: 6, value: 2 },
                    ],
                    exit: BlockExit::UnconditionalJump(4)
                },
                Block {  // 4
                    ops: vec![
                        Op::Phi { dst: 2, left: 5, left_block: 2, right: 6, right_block: 3 },
                    ],
                    exit: BlockExit::UnconditionalJump(9),
                },
                Block {  // 5
                    ops: vec![
                        Op::Const { dst: 9, value: 10 },
                        Op::Binary { dst: 8, left: 0, right: 9, operator: BinOp::Plus },
                    ],
                    exit: BlockExit::Branch(8, 6, 7)  // x + 10 > 0 ?
                },
                Block {  // 6
                    ops: vec![
                        Op::Const { dst: 10, value: 3 },
                    ],
                    exit: BlockExit::UnconditionalJump(8),
                },
                Block {  // 7
                    ops: vec![
                        Op::Const { dst: 11, value: 4 },
                    ],
                    exit: BlockExit::UnconditionalJump(8),
                },
                Block {  // 8
                    ops: vec![
                        Op::Phi { dst: 7, left: 10, left_block: 6, right: 11, right_block: 7 },
                    ],
                    exit: BlockExit::UnconditionalJump(9),
                },
                Block {  // 9
                    ops: vec![
                        Op::Phi { dst: 1, left: 2, left_block: 4, right: 7, right_block: 8 },
                    ],
                    exit: BlockExit::Return(1),
                },
            ]);
    }

    #[test]
    fn call() {
        let fun = afun("foo", 1,
            acall("bar", vec![anumber(10), anumber(20)], CallType::Regular));

        assert_eq!(
            emit_function(&fun),
            vec![
                Block {
                    ops: vec![
                        Op::Const { dst: 2, value: 10 },
                        Op::Const { dst: 3, value: 20 },
                        Op::Call { dst: 1, function: "bar".to_string(), args: vec![2, 3], call_type: CallType::Regular },
                    ],
                    exit: BlockExit::Return(1)
                },
            ]);
    }

    #[test]
    fn seq() {
        let fun = afun("foo", 1,
            aseq(vec![
                anumber(10),
                anumber(20),
            ]));

        assert_eq!(
            emit_function(&fun),
            vec![
                Block {
                    ops: vec![
                        Op::Const { dst: 1, value: 10 },
                        Op::Const { dst: 2, value: 20 },
                    ],
                    exit: BlockExit::Return(2)
                },
            ]);
    }
}
