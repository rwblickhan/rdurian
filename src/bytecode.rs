pub type Bytecode = u8;
pub type ConstantPoolIdx = u16;

#[derive(Debug, Copy, Clone)]
pub enum Opcode {
    Nop = 0x00,
    Pop,
    // push the constant pointed at by the following 2 bytes onto the stack
    Constant,
    // pop the top two objects on the stack, add them (if possible), and push the result onto the stack
    Add,
    // pop the top two objects on the stack, subtract them (if possible), and push the result onto the stack
    Sub,
    Halt,
}

// tag for constant pool
pub enum Tag {
    Nil = 0x0,
    Integer,
    Float,
    String,
    Function,
    Error,
}
