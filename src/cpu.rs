use log::debug;

use crate::opcode::*;
use crate::ops::*;
use std::collections::HashMap;
use std::fmt::Display;

bitflags! {
    /// # Status Register (P) http://wiki.nesdev.com/w/index.php/Status_flags
    ///```
    ///  7 6 5 4 3 2 1 0
    ///  N V _ B D I Z C
    ///  | |   | | | | +--- Carry Flag
    ///  | |   | | | +----- Zero Flag
    ///  | |   | | +------- Interrupt Disable
    ///  | |   | +--------- Decimal Mode (not used on NES)
    ///  | |   +----------- Break Command
    ///  | +--------------- Overflow Flag
    ///  +----------------- Negative Flag
    ///```
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

impl Display for AddressingMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &AddressingMode::Absolute => write!(f, "Absolute"),
            &AddressingMode::Immediate => write!(f, "Immediate"),
            &AddressingMode::ZeroPage_X => write!(f, "ZeroPage_X"),
            &AddressingMode::ZeroPage_Y => write!(f, "ZeroPage_Y"),
            &AddressingMode::Absolute_X => write!(f, "Absolute_X"),
            &AddressingMode::Absolute_Y => write!(f, "Absolute_Y"),
            &AddressingMode::Indirect_X => write!(f, "Indirect_X"),
            &AddressingMode::Indirect_Y => write!(f, "Indirect_Y"),
            &AddressingMode::NoneAddressing => write!(f, "None"),
            &AddressingMode::ZeroPage => write!(f, "ZeroPage"),
        }
    }
}

pub trait Mem {
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
        debug!("wrote {:#04X?} at {:#04X?}", data, addr)
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

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        let ref opcodes: HashMap<u8, &'static OpCode> = *OPCODES_MAP;

        loop {
            callback(self);
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = opcodes
                .get(&code)
                .expect(&format!("OpCode {:x} is not recognized", code));
            debug!("op: {}", opcode);
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
                /* BNE */
                BNE => {
                    self.branch(!self.status.contains(CpuFlags::ZERO));
                }

                /* BVS */
                BVS => {
                    self.branch(self.status.contains(CpuFlags::OVERFLOW));
                }

                /* BVC */
                BVC => {
                    self.branch(!self.status.contains(CpuFlags::OVERFLOW));
                }

                /* BPL */
                BPL => {
                    self.branch(!self.status.contains(CpuFlags::NEGATIV));
                }

                /* BMI */
                BMI => {
                    self.branch(self.status.contains(CpuFlags::NEGATIV));
                }

                /* BEQ */
                BEQ => {
                    self.branch(self.status.contains(CpuFlags::ZERO));
                }

                /* BCS */
                BCS => {
                    self.branch(self.status.contains(CpuFlags::CARRY));
                }

                /* BCC */
                BCC => {
                    self.branch(!self.status.contains(CpuFlags::CARRY));
                }
                /* JMP Absolute */
                JMP_ABSOLUTE => {
                    let mem_address = self.mem_read_u16(self.program_counter);
                    self.program_counter = mem_address;
                }

                /* JMP Indirect */
                JMP_INDIRECT => {
                    let mem_address = self.mem_read_u16(self.program_counter);
                    // let indirect_ref = self.mem_read_u16(mem_address);
                    //6502 bug mode with with page boundary:
                    //  if address $3000 contains $40, $30FF contains $80, and $3100 contains $50,
                    // the result of JMP ($30FF) will be a transfer of control to $4080 rather than $5080 as you intended
                    // i.e. the 6502 took the low byte of the address from $30FF and the high byte from $3000

                    let indirect_ref = if mem_address & 0x00FF == 0x00FF {
                        let lo = self.mem_read(mem_address);
                        let hi = self.mem_read(mem_address & 0xFF00);
                        combine_u8_to_u16(hi, lo)
                    } else {
                        self.mem_read_u16(mem_address)
                    };

                    self.program_counter = indirect_ref;
                }
                /* CMP */
                CMP_IMMEDIATE | CMP_ZERO_PAGE | CMP_ZERO_PAGE_X | CMP_ABSOLUTE | CMP_ABSOLUTE_X
                | CMP_ABSOLUTE_Y | CMP_INDIRECT_X | CMP_INDIRECT_Y => {
                    self.compare(&opcode.mode, self.register_a);
                }

                /* CPY */
                CPY_IMMEDIATE | CPY_ZERO_PAGE | CPY_ABSOLUTE => {
                    self.compare(&opcode.mode, self.register_y);
                }

                /* CPX */
                CPX_IMMEDIATE | CPX_ZERO_PAGE | CPX_ABSOLUTE => {
                    self.compare(&opcode.mode, self.register_x)
                }
                PHP => self.php(),
                PLP => self.plp(),
                PLA => self.pla(),
                TAX => self.tax(),
                INX => self.inx(),
                INY => self.iny(),
                DEX => self.dex(),
                DEY => self.dey(),
                NOP => { /* NOP */ }
                BRK => return,
                CLD => self.status.remove(CpuFlags::DECIMAL_MODE),
                CLI => self.status.remove(CpuFlags::INTERRUPT_DISABLE),
                CLV => self.status.remove(CpuFlags::OVERFLOW),
                CLC => self.clear_carry_flag(),
                SEC => self.set_carry_flag(),
                SEI => self.status.insert(CpuFlags::INTERRUPT_DISABLE),
                SED => self.status.insert(CpuFlags::DECIMAL_MODE),
                PHA => self.stack_push(self.register_a),
                JSR => {
                    debug!("JSR");
                    debug!("stack pointer {:4X?}", self.stack_pointer);
                    debug!("program counter {:4X?}", self.program_counter);
                    self.stack_push_u16(self.program_counter + 2 - 1);
                    let target_address = self.mem_read_u16(self.program_counter);
                    self.program_counter = target_address;
                    debug!("stack pointer {:4X?}", self.stack_pointer);
                    debug!("program counter {:4X?}", self.program_counter);
                }
                RTS => {
                    self.program_counter = self.stack_pop_u16() + 1;
                }
                RTI => {
                    self.status.bits = self.stack_pop();
                    self.status.remove(CpuFlags::BREAK);
                    self.status.insert(CpuFlags::BREAK2);

                    self.program_counter = self.stack_pop_u16();
                }
                BIT_ABSOLUTE | BIT_ZERO_PAGE => {
                    self.bit(&opcode.mode);
                }
                TAY => {
                    self.register_y = self.register_a;
                    self.update_zero_and_negative_flags(self.register_y);
                }
                TSX => {
                    self.register_x = self.stack_pointer;
                    self.update_zero_and_negative_flags(self.register_x);
                }

                TXA => {
                    self.register_a = self.register_x;
                    self.update_zero_and_negative_flags(self.register_a);
                }
                TYA => {
                    self.register_a = self.register_y;
                    self.update_zero_and_negative_flags(self.register_a);
                }
                TXS => {
                    self.stack_pointer = self.register_x;
                }
                /* ADC */
                ADC_IMMEDIATE | ADC_ZERO_PAGE | ADC_ZERO_PAGE_X | ADC_ABSOLUTE | ADC_ABSOLUTE_X
                | ADC_ABSOLUTE_Y | ADC_INDIRECT_X | ADC_INDIRECT_Y => {
                    self.adc(&opcode.mode);
                }

                SBC_IMMEDIATE | SBC_ZERO_PAGE | SBC_ZERO_PAGE_X | SBC_ABSOLUTE | SBC_ABSOLUTE_X
                | SBC_ABSOLUTE_Y | SBC_INDIRECT_X | SBC_INDIRECT_Y => {
                    self.sbc(&opcode.mode);
                }

                AND_IMMEDIATE | AND_ZERO_PAGE | AND_ZERO_PAGE_X | AND_ABSOLUTE | AND_ABSOLUTE_X
                | AND_ABSOLUTE_Y | AND_INDIRECT_X | AND_INDIRECT_Y => {
                    self.and(&opcode.mode);
                }

                EOR_IMMEDIATE | EOR_ZERO_PAGE | EOR_ZERO_PAGE_X | EOR_ABSOLUTE | EOR_ABSOLUTE_X
                | EOR_ABSOLUTE_Y | EOR_INDIRECT_X | EOR_INDIRECT_Y => {
                    self.eor(&opcode.mode);
                }

                ORA_IMMEDIATE | ORA_ZERO_PAGE | ORA_ZERO_PAGE_X | ORA_ABSOLUTE | ORA_ABSOLUTE_X
                | ORA_ABSOLUTE_Y | ORA_INDIRECT_X | ORA_INDIRECT_Y => {
                    self.ora(&opcode.mode);
                }

                LSR_ACCUMULATOR => self.lsr_accumulator(),

                /* LSR */
                LSR_ZERO_PAGE | LSR_ZERO_PAGE_X | LSR_ABSOLUTE | LSR_ABSOLUTE_X => {
                    self.lsr(&opcode.mode);
                }

                ASL_ACCUMULATOR => self.asl_accumulator(),

                /* ASL */
                ASL_ZERO_PAGE | ASL_ZERO_PAGE_X | ASL_ABSOLUTE | ASL_ABSOLUTE_X => {
                    self.asl(&opcode.mode);
                }

                ROL_ACCUMULATOR => self.rol_accumulator(),

                /* ROL */
                ROL_ZERO_PAGE | ROL_ZERO_PAGE_X | ROL_ABSOLUTE | ROL_ABSOLUTE_X => {
                    self.rol(&opcode.mode);
                }

                ROR_ACCUMULATOR => self.ror_accumulator(),

                /* ROR */
                ROR_ZERO_PAGE | ROR_ZERO_PAGE_X | ROR_ABSOLUTE | ROR_ABSOLUTE_X => {
                    self.ror(&opcode.mode);
                }

                /* INC */
                INC_ZERO_PAGE | INC_ZERO_PAGE_X | INC_ABSOLUTE | INC_ABSOLUTE_X => {
                    self.inc(&opcode.mode);
                }
                _ => todo!(),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
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

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    //The "PuLl" operations are known as "POP" on most other microprocessors.
    //PuLl Accumulator
    fn pla(&mut self) {
        let data = self.stack_pop();
        self.set_register(Register::A, data);
    }

    //PLP (PuLl Processor status)
    fn plp(&mut self) {
        self.status.bits = self.stack_pop();
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);
    }

    //PHP (PusH Processor status)
    fn php(&mut self) {
        //http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
        let mut flags = self.status.clone();
        flags.insert(CpuFlags::BREAK);
        flags.insert(CpuFlags::BREAK2);
        self.stack_push(flags.bits());
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        let and = self.register_a & data;
        if and == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        self.status.set(CpuFlags::NEGATIV, data & 0b10000000 > 0);
        self.status.set(CpuFlags::OVERFLOW, data & 0b01000000 > 0);
    }

    fn compare(&mut self, mode: &AddressingMode, compare_with: u8) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        if data <= compare_with {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        self.update_zero_and_negative_flags(compare_with.wrapping_sub(data));
    }

    fn branch(&mut self, condition: bool) {
        if condition {
            let jump = self.mem_read(self.program_counter);
            let jump_addr = self
                .program_counter
                .wrapping_add(1)
                .wrapping_add(jump.into());

            self.program_counter = jump_addr;
        }
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_register(Register::A, data & self.register_a);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_register(Register::A, data ^ self.register_a);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_register(Register::A, data | self.register_a);
    }

    fn asl(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        if r_shift(data, 7) == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = l_shift(data, 1);
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn lsr(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        if is_one(data) {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = r_shift(data, 1);
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn rol(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if r_shift(data, 7) == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = l_shift(data, 1);
        if old_carry {
            data = data | 1;
        }
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn ror(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if is_one(data) {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = r_shift(data, 1);
        if old_carry {
            data = data | 0b10000000;
        }
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn asl_accumulator(&mut self) {
        let mut data = self.register_a;
        if r_shift(data, 7) == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = l_shift(data, 1);
        self.set_register(Register::A, data)
    }

    fn lsr_accumulator(&mut self) {
        let mut data = self.register_a;
        if is_one(data) {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = r_shift(data, 1);
        self.set_register(Register::A, data)
    }

    fn rol_accumulator(&mut self) {
        let mut data = self.register_a;
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if r_shift(data, 7) == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = l_shift(data, 1);
        if old_carry {
            data = data | 1;
        }
        self.set_register(Register::A, data);
    }

    fn ror_accumulator(&mut self) {
        let mut data = self.register_a;
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if is_one(data) {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = r_shift(data, 1);
        if old_carry {
            data = data | 0b10000000;
        }
        self.set_register(Register::A, data);
    }

    fn inc(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        data = data.wrapping_add(1);
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }
    /// note: ignoring decimal mode
    /// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    fn add_to_register_a(&mut self, data: u8) {
        let sum = self.register_a as u16
            + data as u16
            + (if self.status.contains(CpuFlags::CARRY) {
                1
            } else {
                0
            }) as u16;

        let carry = sum > 0xff;

        if carry {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        let result = sum as u8;

        if (data ^ result) & (result ^ self.register_a) & 0x80 != 0 {
            self.status.insert(CpuFlags::OVERFLOW);
        } else {
            self.status.remove(CpuFlags::OVERFLOW)
        }

        self.set_register(Register::A, result);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let data = self.mem_read(addr);
        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8);
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.add_to_register_a(value);
    }

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read((STACK as u16) + self.stack_pointer as u16)
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write((STACK as u16) + self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1)
    }

    fn stack_push_u16(&mut self, data: u16) {
        self.stack_push(((data >> 8) & 0xff) as u8);
        self.stack_push((data & 0xff) as u8);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop();
        let hi = self.stack_pop();

        combine_u8_to_u16(hi, lo)
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
        debug!("program loaded");
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
        debug!("program loaded: {}", program.len());
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
    fn test_iny_overflow() {
        let program = vec![INY, INY, BRK];
        let mut cpu = CPU::new();
        cpu.load(program);
        cpu.reset();
        cpu.set_register(Register::Y, 0xff);
        cpu.run();

        assert_eq!(cpu.register_y, 1)
    }

    #[test]
    fn test_dex_overflow() {
        let program = vec![DEX, BRK];
        let mut cpu = CPU::new();
        cpu.load(program);
        cpu.reset();
        cpu.set_register(Register::X, 0x00);
        cpu.run();

        assert_eq!(cpu.register_x, 0xff)
    }

    #[test]
    fn test_dey_overflow() {
        let program = vec![DEY, BRK];
        let mut cpu = CPU::new();
        cpu.load(program);
        cpu.reset();
        cpu.set_register(Register::Y, 0x00);
        cpu.run();

        assert_eq!(cpu.register_y, 0xff)
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
