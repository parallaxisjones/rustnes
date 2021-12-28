use crate::opcode::*;
use crate::ops::combine_u8_to_u16;
use std::collections::HashMap;
use wrapping_arithmetic::wrappit;

bitflags! {
    /// # Status Register (P) http://wiki.nesdev.com/w/index.php/Status_flags
    ///
    ///  7 6 5 4 3 2 1 0
    ///  N V _ B D I Z C
    ///  | |   | | | | +--- Carry Flag
    ///  | |   | | | +----- Zero Flag
    ///  | |   | | +------- Interrupt Disable
    ///  | |   | +--------- Decimal Mode (not used on NES)
    ///  | |   +----------- Break Command
    ///  | +--------------- Overflow Flag
    ///  +----------------- Negative Flag
    ///
    pub struct CpuFlags: u8 {
        const CARRY             = 0b00000001;
        const ZERO              = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        const DECIMAL_MODE      = 0b00001000;
        const BREAK             = 0b00010000;
        const BREAK2            = 0b00100000;
        const OVERFLOW          = 0b01000000;
        const NEGATIV           = 0b10000000;
    }
}
const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;
enum Register {
    A,
    X,
    Y,
}
pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,
    memory: [u8; 0xFFFF],
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

trait Mem {
    fn mem_read(&self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos);
        let hi = self.mem_read(pos + 1);
        combine_u8_to_u16(hi, lo)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }
}
impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            stack_pointer: STACK_RESET,
            program_counter: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            memory: [0; 0xFFFF],
        }
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        if result & 0b1000_0000 != 0 {
            self.status.insert(CpuFlags::NEGATIV);
        } else {
            self.status.remove(CpuFlags::NEGATIV);
        }
    }

    fn set_carry_flag(&mut self) {
        self.status.insert(CpuFlags::CARRY)
    }

    fn clear_carry_flag(&mut self) {
        self.status.remove(CpuFlags::CARRY)
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,

            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,

            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),

            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }

            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }

            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                combine_u8_to_u16(hi, lo)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = combine_u8_to_u16(hi, lo);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }

            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    pub fn run(&mut self) {
        let ref opcodes: HashMap<u8, &'static OpCode> = *OPCODES_MAP;

        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = opcodes
                .get(&code)
                .expect(&format!("OpCode {:x} is not recognized", code));

            match code {
                /* LDY */
                LDY_IMMEDIATE | LDY_ZERO_PAGE | LDY_ZERO_PAGE_X | LDY_ABSOLUTE | LDY_ABSOLUTE_X => {
                    self.ldy(&opcode.mode);
                }
                /* LDX */
                LDX_IMMEDIATE | LDX_ZERO_PAGE | LDX_ZERO_PAGE_Y | LDX_ABSOLUTE | LDX_ABSOLUTE_Y => {
                    self.ldx(&opcode.mode);
                }
                /* LDA */
                LDA_IMMEDIATE | LDA_ZERO_PAGE | LDA_ZERO_PAGE_X | LDA_ABSOLUTE | LDA_ABSOLUTE_X
                | LDA_ABSOLUTE_Y | LDA_INDIRECT_X | LDA_INDIRECT_Y => {
                    self.lda(&opcode.mode);
                }

                /* STA */
                STA_ZERO_PAGE | STA_ZERO_PAGE_X | STA_ABSOLUTE | STA_ABSOLUTE_X
                | STA_ABSOLUTE_Y | STA_INDIRECT_X | STA_INDIRECT_Y => {
                    self.sta(&opcode.mode);
                }
                /* STX */
                STX_ZERO_PAGE | STX_ZERO_PAGE_Y | STX_ABSOLUTE => {
                    self.stx(&opcode.mode);
                }

                /* STY */
                STY_ZERO_PAGE | STY_ZERO_PAGE_X | STY_ABSOLUTE => {
                    self.sty(&opcode.mode);
                }

                TAX => self.tax(),
                INX => self.inx(),
                NOP => { /* NOP */ }
                BRK => return,
                _ => todo!(),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status = CpuFlags::from_bits_truncate(0b100100);

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        // We load program code into memory, starting at 0x8000 address.
        // [0x8000 .. 0xFFFF] is reserved for Program ROM,
        // and we can assume that the instructions stream should start somewhere in this space (not necessarily at 0x8000).
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    fn set_register(&mut self, register: Register, value: u8) {
        match register {
            Register::A => {
                self.register_a = value;
                self.update_zero_and_negative_flags(self.register_a);
            }
            Register::X => {
                self.register_x = value;
                self.update_zero_and_negative_flags(self.register_x);
            }
            Register::Y => {
                self.register_y = value;
                self.update_zero_and_negative_flags(self.register_y);
            }
        }
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let data = self.mem_read(addr);
        self.set_register(Register::A, data);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_register(Register::Y, data);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_register(Register::X, data);
    }

    fn tax(&mut self) {
        self.set_register(Register::X, self.register_a);
    }

    #[wrappit]
    fn inx(&mut self) {
        // to protect against register overflows, we must wrap values
        // There's a rust internal method to help with this, however it's confusing and less readable
        // to demonstrate rust procedural macro usage a dependecy was used to make this more readable
        // https://crates.io/crates/wrapping_arithmetic
        self.register_x = self.register_x + 1;
        self.update_zero_and_negative_flags(self.register_x);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immidiate_load_data() {
        let progam = vec![LDA_IMMEDIATE, 0x05, BRK];
        let mut cpu = CPU::new();
        cpu.load_and_run(progam);
        assert_eq!(cpu.register_a, 5);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b00);
        assert!(cpu.status.bits() & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let program = vec![LDA_IMMEDIATE, 0x0a, TAX, BRK];
        let mut cpu = CPU::new();
        cpu.load_and_run(program);

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        let test_value: u8 = 0xc0;
        let program = vec![LDA_IMMEDIATE, test_value, TAX, INX, BRK];
        cpu.load_and_run(program);

        assert_eq!(cpu.register_x, test_value + 1)
    }

    #[test]
    fn test_inx_overflow() {
        let program = vec![INX, INX, BRK];
        let mut cpu = CPU::new();
        cpu.load(program);
        cpu.reset();
        cpu.set_register(Register::X, 0xff);
        cpu.run();

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let test_addy: u8 = 0x10;
        let test_value: u8 = 0x55;
        let program = vec![LDA_ZERO_PAGE, test_addy, BRK];
        let mut cpu = CPU::new();
        cpu.mem_write(test_addy.into(), test_value);
        cpu.load_and_run(program);

        assert_eq!(cpu.register_a, test_value);
    }

    #[test]
    fn test_ldy_immediate() {
        let program = vec![LDY_IMMEDIATE, 0xff, BRK];
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0xff);

        cpu.load_and_run(program);

        assert_eq!(cpu.register_y, 0xff);
    }

    #[test]
    fn test_ldy_from_memory() {
        let test_addy: u8 = 0x10;
        let test_value: u8 = 0xff;
        let program = vec![LDY_ZERO_PAGE, test_addy, BRK];
        let mut cpu = CPU::new();
        cpu.mem_write(test_addy.into(), test_value);

        cpu.load_and_run(program);

        assert_eq!(cpu.register_y, test_value);
    }

    #[test]
    fn test_ldx_immediate() {
        let program = vec![LDX_IMMEDIATE, 0xff, BRK];
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0xff);

        cpu.load_and_run(program);

        assert_eq!(cpu.register_x, 0xff);
    }

    #[test]
    fn test_ldx_from_memory() {
        let test_addy: u8 = 0x10;
        let test_value: u8 = 0xff;
        let program = vec![LDX_ZERO_PAGE, test_addy, BRK];
        let mut cpu = CPU::new();
        cpu.mem_write(test_addy.into(), test_value);

        cpu.load_and_run(program);

        assert_eq!(cpu.register_x, test_value);
    }
}
