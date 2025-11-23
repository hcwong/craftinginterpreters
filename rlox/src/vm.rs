use std::convert::TryFrom;
use crate::chunk::Chunk;
use crate::chunk::OpCode;
use crate::debug;

pub const STACK_MAX_SIZE: usize = 512;

pub struct VM {
    chunk: Option<Chunk>,
    code_index: usize,
    stack: [f64; STACK_MAX_SIZE],
    stack_top: usize,
}

pub enum InterpretResult {
    InterpetOK,
    InterpetCompileError,
    InterpretRuntimeError,
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: None,
            code_index: 0,
            stack: [0.0; STACK_MAX_SIZE],
            stack_top: 0,
        }
    }

    pub fn interpret(&mut self, chunk: Chunk) {
        self.chunk = Some(chunk);
        self.code_index = 0;
        self.stack = [0.0; STACK_MAX_SIZE];
        self.stack_top = 0;

        self.run();
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    fn push_stack(&mut self, value: f64) {
        // TODO: handle stack overflow
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop_stack(&mut self) -> Option<f64> {
        if self.stack_top == 0 {
            None
        } else {
            let top_value = self.stack[self.stack_top - 1];
            self.stack_top -= 1;
            Some(top_value)
        }
    }

    pub fn debug_stack(&self) {
        for i in 0..self.stack_top {
            print!("[");
            print!("{}", self.stack[i]);
            println!("]");
        }
    }

  #[inline(always)]
  pub fn run(&mut self) -> InterpretResult {
      loop {
          // --- Get next instruction in a short-lived borrow block ---
          let instruction = {
              let chunk = match self.chunk.as_ref() {
                  Some(c) => c,
                  None => {
                      eprintln!("VM error: no chunk loaded");
                      return InterpretResult::InterpretRuntimeError;
                  }
              };

              if self.code_index >= chunk.get_codes().len() {
                  return InterpretResult::InterpretRuntimeError;
              }

              let instr = chunk.get_codes()[self.code_index];
              self.code_index += 1;
              instr
          }; // <- `chunk` borrow ends here

          // Now we can freely borrow `&mut self` in match arms
          match OpCode::try_from(instruction) {
              Ok(OpCode::OpReturn) => {
                  if let Some(value) = self.pop_stack() {
                      println!("Return value: {}", value);
                  }
                  return InterpretResult::InterpetOK;
              }

              Ok(OpCode::OpConstant) => {
                  // Another short borrow to read the constant
                  let constant = {
                      let chunk = self.chunk.as_ref().expect("chunk vanished during run");

                      if self.code_index >= chunk.get_codes().len() {
                          eprintln!("Runtime error: missing constant index after OP_CONSTANT");
                          return InterpretResult::InterpretRuntimeError;
                      }

                      let constant_index = chunk.get_codes()[self.code_index];
                      self.code_index += 1;

                      chunk.get_value(constant_index)
                  }; // <- `chunk` borrow ends here

                  println!("Constant: {constant}");
                  self.push_stack(constant);
              }

              Ok(OpCode::OpNegate) => {
                  match self.pop_stack() {
                      Some(value) => {
                          self.push_stack(-value);
                      }
                      None => {
                          println!("Runtime error: stack underflow on NEGATE");
                      }
                  }
              }

              Err(_) => panic!("Unrecognised opcode"),
          }
      }
  }

    #[inline(always)]
    fn disassemble_instruction(&self) {
        if let Some(chunk) = &self.chunk {
            debug::disassemble_instruction(
                chunk,
                chunk.get_codes()[self.code_index],
                self.code_index,
            );
        } else {
            eprintln!("Cannot disassemble: no chunk loaded");
        }
    }
}
