pub type Bytecode = u8;
pub type ConstantPoolIdx = u16;

#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    Nop = 0x00,
    Pop = 0x01,
    // push the constant pointed at by the following 2 bytes onto the stack
    Constant = 0x02,
    // pop the top two objects on the stack, add them (if possible), and push the result onto the stack
    Add = 0x10,
    // pop the top two objects on the stack, subtract them (if possible), and push the result onto the stack
    Sub = 0x11,
    Halt = 0xFF,
}

impl From<u8> for Opcode {
    fn from(byte: u8) -> Self {
        match byte {
            0x00 => Opcode::Nop,
            0x01 => Opcode::Pop,
            0x02 => Opcode::Constant,
            0x10 => Opcode::Add,
            0x11 => Opcode::Sub,
            0xFF => Opcode::Halt,
            _ => Opcode::Nop,
        }
    }
}

// tag for constant pool
#[derive(Debug, Copy, Clone)]
pub enum Tag {
    Nil = 0x00,
    Integer = 0x01,
    Float = 0x02,
    String = 0x03,
    Function = 0x04,
    Error = 0xFF,
}

impl From<u8> for Tag {
    fn from(byte: u8) -> Self {
        match byte {
            0x00 => Tag::Nil,
            0x01 => Tag::Integer,
            0x02 => Tag::Float,
            0x03 => Tag::String,
            0x04 => Tag::Function,
            _ => Tag::Error
        }
    }
}