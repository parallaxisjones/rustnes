pub mod ops;
pub mod opcode;
pub mod cpu;
pub mod adressing_mode;

pub type CPU = self::cpu::CPU;
pub type AddressingMode = self::adressing_mode::AddressingMode;

// const STACK: u16 = 0x0100;
// const STACK_RESET: u8 = 0xfd;

#[allow(dead_code)]
enum Register {
    A,
    X,
    Y,
}

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }
}
