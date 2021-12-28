mod cpu;
mod opcode;
mod ops;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate bitflags;

use crate::cpu::CPU;


fn main() {
    let mut program = CPU::new();
    program.load_and_run(vec![0xE8, 0xAA, 0xE8, 0xa9]);
}
