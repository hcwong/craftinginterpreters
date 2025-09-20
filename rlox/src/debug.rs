use std::convert::TryFrom;

use crate::chunk::Chunk;
use crate::chunk::OpCode;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
  print!("==== {name} ====\n");

  let mut index: u8 = 0;

  while (index as usize) < chunk.get_codes().len() {
    // Unsafe narrowing here
    index = disassemble_instruction(chunk, chunk.get_codes()[index as usize], index)
  }
}

fn disassemble_instruction(chunk: &Chunk, opcode: u8, index: u8) -> u8 {
  print!("{index} ");

  if index > 0 && chunk.line_numbers[index as usize] == chunk.line_numbers[(index as usize) - 1] {
    print!(" | ")
  } else {
    print!("{}", chunk.line_numbers[index as usize])
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

fn simple_instruction(name: &str, index: u8) -> u8 {
   println!("{name}");
   return index + 1;
}

fn constant_instruction(name: &str, chunk: &Chunk, index: u8) -> u8 {
   let constant = chunk.get_codes()[(index + 1) as usize];
   print!(" {name} {constant} ");
   let value = chunk.get_value(constant);
   print!("'{value}'\n");

   return index + 2;
}