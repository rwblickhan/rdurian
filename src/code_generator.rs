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
    pub fn gen_stmt_bytecode(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr { expr } => {
                self.gen_expr_bytecode(expr);
                self.output_bytecode.push(Opcode::Pop as u8);
            }
            _ => {} // TODO
        }
    }

    fn gen_expr_bytecode(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary { left, operator, right } => {
                self.gen_expr_bytecode(left);
                self.gen_expr_bytecode(right);
                match operator {
                    Token::Plus(_) => self.output_bytecode.push(Opcode::Add as u8),
                    Token::Minus(_) => self.output_bytecode.push(Opcode::Sub as u8),
                    _ => {} // TODO
                }
            }
            Expr::Literal { value } => {
                match value {
                    Token::Integer { literal, .. } => {
                        self.output_bytecode.push(Opcode::Constant as u8);
                        let mut idx = Vec::new();
                        idx.write_u16::<BigEndian>(self.curr_constant_idx).unwrap();
                        self.output_bytecode.append(&mut idx);
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
        }
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

