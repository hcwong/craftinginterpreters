use std::convert::TryFrom;

use crate::chunk::Chunk;
use crate::chunk::OpCode;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
  print!("==== {name} ====\n");

  let mut index: usize = 0;

  while (index) < chunk.get_codes().len() {
    // Unsafe narrowing here
    index = disassemble_instruction(chunk, chunk.get_codes()[index], index)
  }
}

fn disassemble_instruction(chunk: &Chunk, opcode: u8, index: usize) -> usize {
  print!("{index} ");

  if index > 0 && chunk.line_numbers[index] == chunk.line_numbers[(index) - 1] {
    print!(" | ")
  } else {
    print!("{}", chunk.line_numbers[index])
  }

  match OpCode::try_from(opcode) {
    Ok(OpCode::OpReturn) => return simple_instruction("OP_RETURN", index),
    Ok(OpCode::OpConstant) => return constant_instruction("OP_CONSTANT", chunk, index),
    Err(_) => { 
      println!("Unknown instruction: {opcode}");
      return index + 1 
    },
  }
}

fn simple_instruction(name: &str, index: usize) -> usize {
   println!("{name}");
   return index + 1;
}

fn constant_instruction(name: &str, chunk: &Chunk, index: usize) -> usize {
   let constant = chunk.get_codes()[index + 1];
   print!(" {name} {constant} ");
   let value = chunk.get_value(constant);
   print!("'{value}'\n");

   return index + 2;
}