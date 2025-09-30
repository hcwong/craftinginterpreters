mod chunk;
mod debug;
mod vm;

fn main() {
    use chunk::Chunk;
    use chunk::OpCode;

    let mut chunk = Chunk::init(); 
    let constant = chunk.add_constant(1.2);
    chunk.write_chunk(OpCode::OpConstant.into(), 123);
    chunk.write_chunk(constant as u8, 123);

    chunk.write_chunk(OpCode::OpReturn.into(), 123);

    
    debug::disassemble_chunk(&chunk, "test chunk");
}
