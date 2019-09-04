pub type Bytecode = u8;
pub type ConstantPoolIdx = u16;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
    Nop = 0x00,
    Pop = 0x01,
    // push the constant pointed at by the following 2 bytes onto the stack
    Constant = 0x02,
    // pop the top two objects on the stack, perform logical AND, and push the result onto the stack
    LogAnd = 0x10,
    // pop the top two objects on the stack, perform logical OR, and push the result onto the stack
    LogOr = 0x11,
    // pop the top two objects on the stack, add them (if possible), and push the result onto the stack
    Add = 0x12,
    // pop the top two objects on the stack, subtract them (if possible), and push the result onto the stack
    Sub = 0x13,
    // pop the top two objects on the stack, multiply them (if possible), and push the result onto the stack
    Mul = 0x14,
    Div = 0x15,
    Mod = 0x16,
    Exp = 0x17,
    // pop the top two objects on the stack, check for equality, and push the result onto the stack
    Eq = 0x18,
    // pop the top two objects on the stack, check for non-equality, and push the result onto the stack
    NotEq = 0x19,
    GreaterEq = 0x1A,
    Greater = 0x1B,
    LesserEq = 0x1C,
    Lesser = 0x1D,
    // TODO
    Concat = 0x1E,
    // TODO
    Numberify = 0x20,
    // pop the top object from the stack, negate (if possible), and push the result onto the stack
    Neg = 0x21,
    // TODO
    Stringify = 0x22,
    // pop the top object from the stack, logically negate (if possible), and push the result onto the stack
    LogNeg = 0x23,
    // pop the top object from the stack, take the square root (if possible), and push the result onto the stack
    Sqrt = 0x24,
    // TODO
    Env = 0x25,
    // pop the top object from the stack, output its string repr to stdout
    Print = 0xF0,
    // pop the top object from the stack, output its string repr to stderr
    Err = 0xF1,
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
            0xF0 => Opcode::Print,
            0xF1 => Opcode::Err,
            0xFF => Opcode::Halt,
            _ => Opcode::Nop,
        }
    }
}

// tag for constant pool
#[derive(Debug, Copy, Clone, PartialEq)]
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
            _ => Tag::Error,
        }
    }
}
