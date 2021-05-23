use num_derive::FromPrimitive;    
use num_traits::FromPrimitive;
use wrapping_arithmetic::wrappit;

pub struct CPU {
    // Accumulator
    pub register_a: u8,
    // Index Register X
    pub register_x: u8,
    // Status Register
    pub status: u8,
    // Program Counter Register
    pub program_counter: u16,
}

#[derive(FromPrimitive)]
pub enum OpsCode {
    // Load Accumulator
    LDA = 0xA9,
    // Break
    BRK = 0x00,
    // Transfer Accumulator to X
    TAX = 0xAA,
    // Increment X Register
    INX = 0xE8
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            status: 0,
            program_counter: 0,
        }
    }

    pub fn interpret(&mut self, program: Vec<u8>) {
        self.program_counter = 0;

        loop {
            let opscode = program[self.program_counter as usize];
            self.program_counter += 1;

            match FromPrimitive::from_u8(opscode) {
                Some(OpsCode::LDA) => {
                    let param = program[self.program_counter as usize];
                    self.program_counter += 1;
                    self.lda(param);
                },
                Some(OpsCode::TAX) => self.tax(),
                Some(OpsCode::INX) => self.inx(),
                Some(OpsCode::BRK) => return,
                None => println!("unknown OpCode {}", opscode),
            }
        }
    }
    fn lda(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
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
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }

        if result & 0b1000_0000 != 0 {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
        }
    }
}

#[cfg(test)]
mod test {
   use super::*;
 
   #[test]
   fn test_0xa9_lda_immidiate_load_data() {
       let mut cpu = CPU::new();
       cpu.interpret(vec![0xa9, 0x05, 0x00]);
       assert_eq!(cpu.register_a, 0x05);
       assert!(cpu.status & 0b0000_0010 == 0b00);
       assert!(cpu.status & 0b1000_0000 == 0);
   }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;
        cpu.interpret(vec![0xaa, 0x00]);
  
        assert_eq!(cpu.register_x, 10)
    }
    #[test]
   fn test_5_ops_working_together() {
       let mut cpu = CPU::new();
       cpu.interpret(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
 
       assert_eq!(cpu.register_x, 0xc1)
   }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xff;
        cpu.interpret(vec![0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }
}