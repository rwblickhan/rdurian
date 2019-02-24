extern crate byteorder;

use self::byteorder::{BigEndian, WriteBytesExt};

use ast::{Expr, Stmt};
use bytecode::{Bytecode, ConstantPoolIdx, Opcode, Tag};
use token::Token;

#[derive(Default)]
pub struct CodeGenerator {
    constant_pool: Vec<Bytecode>,
    curr_constant_idx: ConstantPoolIdx,
    output_bytecode: Vec<Bytecode>,
}

impl CodeGenerator {
    pub fn compile(&mut self, stmt: &Stmt) {
        self.emit(stmt);
    }

    fn emit(&mut self, stmt: &Stmt) {
        let mut out = self.emit_stmt_bytecode(stmt);
        self.output_bytecode.append(&mut out);
    }

    fn emit_stmt_bytecode(&mut self, stmt: &Stmt) -> Vec<Bytecode> {
        let mut out = Vec::new();
        match stmt {
            Stmt::Print { expr, .. } => {
                out.append(&mut self.emit_expr_bytecode(expr));
                out.push(Opcode::Print as u8);
            }
            Stmt::Err { expr, .. } => {
                out.append(&mut self.emit_expr_bytecode(expr));
                out.push(Opcode::Err as u8);
            }
            Stmt::Expr { expr } => {
                out.append(&mut self.emit_expr_bytecode(expr));
                out.push(Opcode::Pop as u8);
            }
            _ => {} // TODO
        };
        out
    }

    fn emit_expr_bytecode(&mut self, expr: &Expr) -> Vec<Bytecode> {
        let mut out = Vec::new();
        match expr {
            Expr::Binary { left, operator, right } => {
                out.append(&mut self.emit_expr_bytecode(left));
                out.append(&mut self.emit_expr_bytecode(right));
                match operator {
                    Token::Plus(_) => out.push(Opcode::Add as u8),
                    Token::Minus(_) => out.push(Opcode::Sub as u8),
                    _ => {} // TODO
                }
            }
            Expr::Literal { value } => {
                match value {
                    Token::Integer { literal, .. } => {
                        out.push(Opcode::Constant as u8);
                        let mut idx = Vec::new();
                        idx.write_u16::<BigEndian>(self.curr_constant_idx).unwrap();
                        out.append(&mut idx);
                        let mut lit_as_bytes: Vec<u8> = Vec::new();
                        lit_as_bytes.write_i32::<BigEndian>(*literal).unwrap();
                        self.constant_pool.push(Tag::Integer as u8);
                        self.constant_pool.append(&mut lit_as_bytes);
                        self.curr_constant_idx = self.curr_constant_idx + 5;
                    }
                    _ => {} // TODO
                }
            }
            _ => {} // TODO
        };
        out
    }

    pub fn retrieve_bytecode(&mut self) -> Vec<Bytecode> {
        self.output_bytecode.push(Opcode::Halt as u8);
        self.output_bytecode.clone()
    }

    pub fn retrieve_constant_pool(&self) -> Vec<Bytecode> {
        self.constant_pool.clone()
    }

    pub fn retrieve_constant_pool_size(&self) -> Vec<Bytecode> {
        let mut tmp = Vec::new();
        tmp.write_u16::<BigEndian>(self.curr_constant_idx).unwrap();
        tmp
    }
}

#[cfg(test)]
mod tests {
    extern crate byteorder;

    use super::*;
    use self::byteorder::{BigEndian, ReadBytesExt};
    use std::io::Cursor;
    use std::collections::VecDeque;

    fn read_constant_pool_size(constant_pool_size_raw: Vec<Bytecode>) -> u16 {
        let mut constant_pool_size_raw_queue = VecDeque::from(constant_pool_size_raw);
        return Cursor::new({
            let mut tmp = Vec::new();
            for _i in 0..2 {
                let out = constant_pool_size_raw_queue.pop_front();
                match out {
                    Some(byte) => tmp.push(byte),
                    _ => panic!()
                }
            }
            tmp
        }).read_u16::<BigEndian>().unwrap();
    }

    fn pop_constant_pool_index(instructions: &mut VecDeque<Bytecode>) -> u16 {
        return Cursor::new({
            let mut tmp = Vec::new();
            for _i in 0..2 {
                let out = instructions.pop_front();
                match out {
                    Some(byte) => tmp.push(byte),
                    _ => panic!()
                }
            }
            tmp
        }).read_u16::<BigEndian>().unwrap();
    }

    fn pop_integer_constant(constant_pool: &mut VecDeque<Bytecode>) -> i32 {
        return Cursor::new({
            let mut tmp = Vec::new();
            for _i in 0..4 {
                let out = constant_pool.pop_front();
                match out {
                    Some(byte) => tmp.push(byte),
                    _ => panic!()
                }
            }
            tmp
        }).read_i32::<BigEndian>().unwrap();
    }

    #[test]
    fn test_compile_int_literal() {
        let mut code_gen = CodeGenerator::default();
        let stmt = Stmt::Expr {
            expr: Box::new(Expr::Literal {
                value: Token::Integer {
                    line: 0,
                    literal: 1,
                }
            })
        };
        code_gen.compile(&stmt);
        let constant_pool_size = read_constant_pool_size(code_gen.retrieve_constant_pool_size());
        assert_eq!(constant_pool_size, 5); // 1 byte for tag + 4 byte int
        let mut constant_pool = VecDeque::from(code_gen.retrieve_constant_pool());
        let tag = Tag::from(constant_pool.pop_front().unwrap());
        assert_eq!(tag, Tag::Integer);
        let constant = pop_integer_constant(&mut constant_pool);
        assert_eq!(constant, 1);
        let mut instructions = VecDeque::from(code_gen.retrieve_bytecode());
        let const_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(const_instr, Opcode::Constant);
        let idx = pop_constant_pool_index(&mut instructions);
        assert_eq!(idx, 0);
        let pop_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(pop_instr, Opcode::Pop);
    }

    #[test]
    fn test_compile_add_two_int_literals() {
        let mut code_gen = CodeGenerator::default();
        let stmt = Stmt::Expr {
            expr: Box::new(Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Token::Integer {
                        line: 0,
                        literal: 2,
                    }
                }),
                operator: Token::Plus(0),
                right: Box::new(Expr::Literal {
                    value: Token::Integer {
                        line: 0,
                        literal: 1,
                    }
                }),
            })
        };
        code_gen.compile(&stmt);
        let constant_pool_size = read_constant_pool_size(code_gen.retrieve_constant_pool_size());
        assert_eq!(constant_pool_size, 10); // 5 per int constant * 2
        let mut constant_pool = VecDeque::from(code_gen.retrieve_constant_pool());
        let first_tag = Tag::from(constant_pool.pop_front().unwrap());
        assert_eq!(first_tag, Tag::Integer);
        let first_constant = pop_integer_constant(&mut constant_pool);
        assert_eq!(first_constant, 2);
        let second_tag = Tag::from(constant_pool.pop_front().unwrap());
        assert_eq!(second_tag, Tag::Integer);
        let second_constant = pop_integer_constant(&mut constant_pool);
        assert_eq!(second_constant, 1);
        let mut instructions = VecDeque::from(code_gen.retrieve_bytecode());
        let first_const_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(first_const_instr, Opcode::Constant);
        let first_idx = pop_constant_pool_index(&mut instructions);
        assert_eq!(first_idx, 0);
        let second_const_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(second_const_instr, Opcode::Constant);
        let second_idx = pop_constant_pool_index(&mut instructions);
        assert_eq!(second_idx, 5);
        let add_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(add_instr, Opcode::Add);
        let pop_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(pop_instr, Opcode::Pop);
    }

    #[test]
    fn test_compile_sub_two_int_literals() {
        let mut code_gen = CodeGenerator::default();
        let stmt = Stmt::Expr {
            expr: Box::new(Expr::Binary {
                left: Box::new(Expr::Literal {
                    value: Token::Integer {
                        line: 0,
                        literal: 2,
                    }
                }),
                operator: Token::Minus(0),
                right: Box::new(Expr::Literal {
                    value: Token::Integer {
                        line: 0,
                        literal: 1,
                    }
                }),
            })
        };
        code_gen.compile(&stmt);
        let constant_pool_size = read_constant_pool_size(code_gen.retrieve_constant_pool_size());
        assert_eq!(constant_pool_size, 10); // 5 per int constant * 2
        let mut constant_pool = VecDeque::from(code_gen.retrieve_constant_pool());
        let first_tag = Tag::from(constant_pool.pop_front().unwrap());
        assert_eq!(first_tag, Tag::Integer);
        let first_constant = pop_integer_constant(&mut constant_pool);
        assert_eq!(first_constant, 2);
        let second_tag = Tag::from(constant_pool.pop_front().unwrap());
        assert_eq!(second_tag, Tag::Integer);
        let second_constant = pop_integer_constant(&mut constant_pool);
        assert_eq!(second_constant, 1);
        let mut instructions = VecDeque::from(code_gen.retrieve_bytecode());
        let first_const_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(first_const_instr, Opcode::Constant);
        let first_idx = pop_constant_pool_index(&mut instructions);
        assert_eq!(first_idx, 0);
        let second_const_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(second_const_instr, Opcode::Constant);
        let second_idx = pop_constant_pool_index(&mut instructions);
        assert_eq!(second_idx, 5);
        let add_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(add_instr, Opcode::Sub);
        let pop_instr = Opcode::from(instructions.pop_front().unwrap());
        assert_eq!(pop_instr, Opcode::Pop);
    }
}

