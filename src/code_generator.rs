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
    pub fn emit(&mut self, stmt: &Stmt) {
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

