use std::fmt;
use std::convert::TryFrom;

type Value = f64;

// In clox, OpCode is typedef uint8_t 
#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum OpCode {
  OpReturn = 1,
  OpConstant = 2,
  OpNegate = 3,
}

impl From<OpCode> for u8 {
    fn from(op: OpCode) -> u8 { op as u8 }
}

impl TryFrom<u8> for OpCode {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(OpCode::OpReturn),
            2 => Ok(OpCode::OpConstant),
            3 => Ok(OpCode::OpNegate),
            _ => Err(()),
        }
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self as u8)
    }
}

pub struct Chunk {
  // The C version is a pointer to a space on the heap
  // plus reallocation, which is all done by Vec
  // I imagine that it may complicate the GC part later since 
  code: Vec<u8>,
  value_array: Vec<Value>,
  pub line_numbers: Vec<usize>,
}

impl Chunk {
  pub fn init() -> Chunk {
    return Chunk {
      code: Vec::new(),
      value_array: Vec::new(),
      line_numbers: Vec::new(),
    }; 
  }

  pub fn get_codes(&self) -> &Vec<u8> {
    &self.code
  }

  pub fn write_chunk(&mut self, value: u8, line: usize) -> () {
    let _ = &self.code.push(value);
    let _ = &self.line_numbers.push(line);
  }

  // Returns the offset of the constant, which is its index
  pub fn add_constant(&mut self, value: Value) -> usize {
    self.value_array.push(value);
    self.value_array.len() - 1
  }

  pub fn get_value(&self, index: u8) -> Value {
    // Yet another unsafe widening
    self.value_array[index as usize]
  }
}
