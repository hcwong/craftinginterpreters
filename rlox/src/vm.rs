use std::convert::TryFrom;
use crate::chunk::Chunk;
use crate::chunk::OpCode;
use crate::debug;

pub struct VM {
  chunk: Chunk,
  code_index: usize
}

pub enum InterpretResult {
  InterpetOK,
  InterpetCompileError,
  InterpretRuntimeError,
}

impl VM {
  pub fn interpret(&mut self, chunk: Chunk) {
    self.chunk = chunk;
    self.code_index = 0;
  }

  #[inline(always)]
  pub fn run(&mut self) -> InterpretResult {
    while self.code_index < self.chunk.get_codes().len() {
      let instruction = self.chunk.get_codes()[self.code_index];
      self.code_index += 1;
      match OpCode::try_from(instruction) {
        Ok(OpCode::OpReturn) => return InterpretResult::InterpetOK,
        Ok(OpCode::OpConstant) => {
          let constant = self.chunk.get_value(self.chunk.get_codes()[self.code_index + 1]);
          self.code_index += 1;
          println!("{constant}");
          break;
        }
        Err(_) => panic!("Unrecognised opcode"),
      }
    }
    // Just for compilation reason
    return InterpretResult::InterpretRuntimeError
  }

  #[inline(always)] 
  fn disassemble_instruction(&self) {
    debug::disassemble_instruction(&self.chunk, self.chunk.get_codes()[self.code_index], self.code_index);
  }
}