use crate::cpu::AddressingMode;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug)]
pub struct OpCode {
    pub code: u8,
    pub mnemonic: &'static str,
    pub len: u8,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:#04X?}, {}, {})", self.code, self.mnemonic, self.mode)
    }
}

impl OpCode {
    fn new(code: u8, mnemonic: &'static str, len: u8, cycles: u8, mode: AddressingMode) -> Self {
        OpCode {
            code: code,
            mnemonic: mnemonic,
            len: len,
            cycles: cycles,
            mode: mode,
        }
    }
}

lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
        OpCode::new(BRK, "BRK", 1, 7, AddressingMode::NoneAddressing),
        OpCode::new(NOP, "NOP", 1, 2, AddressingMode::NoneAddressing),

        /* Arithmetic */
        OpCode::new(ADC_IMMEDIATE, "ADC", 2, 2, AddressingMode::Immediate),
        OpCode::new(ADC_ZERO_PAGE, "ADC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(ADC_ZERO_PAGE_X, "ADC", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(ADC_ABSOLUTE, "ADC", 3, 4, AddressingMode::Absolute),
        OpCode::new(ADC_ABSOLUTE_X, "ADC", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(ADC_ABSOLUTE_Y, "ADC", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(ADC_INDIRECT_X, "ADC", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(ADC_INDIRECT_Y, "ADC", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(SBC_IMMEDIATE, "SBC", 2, 2, AddressingMode::Immediate),
        OpCode::new(SBC_ZERO_PAGE, "SBC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(SBC_ZERO_PAGE_X, "SBC", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(SBC_ABSOLUTE, "SBC", 3, 4, AddressingMode::Absolute),
        OpCode::new(SBC_ABSOLUTE_X, "SBC", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(SBC_ABSOLUTE_Y, "SBC", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(SBC_INDIRECT_X, "SBC", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(SBC_INDIRECT_Y, "SBC", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(AND_IMMEDIATE, "AND", 2, 2, AddressingMode::Immediate),
        OpCode::new(AND_ZERO_PAGE, "AND", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(AND_ZERO_PAGE_X, "AND", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(AND_ABSOLUTE, "AND", 3, 4, AddressingMode::Absolute),
        OpCode::new(AND_ABSOLUTE_X, "AND", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(AND_ABSOLUTE_Y, "AND", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(AND_INDIRECT_X, "AND", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(AND_INDIRECT_Y, "AND", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(EOR_IMMEDIATE, "EOR", 2, 2, AddressingMode::Immediate),
        OpCode::new(EOR_ZERO_PAGE, "EOR", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(EOR_ZERO_PAGE_X, "EOR", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(EOR_ABSOLUTE, "EOR", 3, 4, AddressingMode::Absolute),
        OpCode::new(EOR_ABSOLUTE_X, "EOR", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(EOR_ABSOLUTE_Y, "EOR", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(EOR_INDIRECT_X, "EOR", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(EOR_INDIRECT_Y, "EOR", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(ORA_IMMEDIATE, "ORA", 2, 2, AddressingMode::Immediate),
        OpCode::new(ORA_ZERO_PAGE, "ORA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(ORA_ZERO_PAGE_X, "ORA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(ORA_ABSOLUTE, "ORA", 3, 4, AddressingMode::Absolute),
        OpCode::new(ORA_ABSOLUTE_X, "ORA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(ORA_ABSOLUTE_Y, "ORA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(ORA_INDIRECT_X, "ORA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(ORA_INDIRECT_Y, "ORA", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        /* Shifts */
        OpCode::new(ASL_ACCUMULATOR, "ASL", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(ASL_ZERO_PAGE, "ASL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(ASL_ZERO_PAGE_X, "ASL", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(ASL_ABSOLUTE, "ASL", 3, 6, AddressingMode::Absolute),
        OpCode::new(ASL_ABSOLUTE_X, "ASL", 3, 7, AddressingMode::Absolute_X),

        OpCode::new(LSR_ACCUMULATOR, "LSR", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(LSR_ZERO_PAGE, "LSR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(LSR_ZERO_PAGE_X, "LSR", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(LSR_ABSOLUTE, "LSR", 3, 6, AddressingMode::Absolute),
        OpCode::new(LSR_ABSOLUTE_X, "LSR", 3, 7, AddressingMode::Absolute_X),

        OpCode::new(ROL_ACCUMULATOR, "ROL", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(ROL_ZERO_PAGE, "ROL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(ROL_ZERO_PAGE_X, "ROL", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(ROL_ABSOLUTE, "ROL", 3, 6, AddressingMode::Absolute),
        OpCode::new(ROL_ABSOLUTE_X, "ROL", 3, 7, AddressingMode::Absolute_X),

        OpCode::new(ROR_ACCUMULATOR, "ROR", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(ROR_ZERO_PAGE, "ROR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(ROR_ZERO_PAGE_X, "ROR", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(ROR_ABSOLUTE, "ROR", 3, 6, AddressingMode::Absolute),
        OpCode::new(ROR_ABSOLUTE_X, "ROR", 3, 7, AddressingMode::Absolute_X),

        OpCode::new(INC_ZERO_PAGE, "INC", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(INC_ZERO_PAGE_X, "INC", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(INC_ABSOLUTE, "INC", 3, 6, AddressingMode::Absolute),
        OpCode::new(INC_ABSOLUTE_X, "INC", 3, 7, AddressingMode::Absolute_X),

        OpCode::new(INX, "INX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(INY, "INY", 1, 2, AddressingMode::NoneAddressing),

        OpCode::new(DEC_ZERO_PAGE, "DEC", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(DEC_ZERO_PAGE_X, "DEC", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(DEC_ABSOLUTE, "DEC", 3, 6, AddressingMode::Absolute),
        OpCode::new(DEC_ABSOLUTE_X, "DEC", 3, 7, AddressingMode::Absolute_X),

        OpCode::new(DEX, "DEX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(DEY, "DEY", 1, 2, AddressingMode::NoneAddressing),

        OpCode::new(CMP_IMMEDIATE, "CMP", 2, 2, AddressingMode::Immediate),
        OpCode::new(CMP_ZERO_PAGE, "CMP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(CMP_ZERO_PAGE_X, "CMP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(CMP_ABSOLUTE, "CMP", 3, 4, AddressingMode::Absolute),
        OpCode::new(CMP_ABSOLUTE_X, "CMP", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(CMP_ABSOLUTE_Y, "CMP", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(CMP_INDIRECT_X, "CMP", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(CMP_INDIRECT_Y, "CMP", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(CPY_IMMEDIATE, "CPY", 2, 2, AddressingMode::Immediate),
        OpCode::new(CPY_ZERO_PAGE, "CPY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(CPY_ABSOLUTE, "CPY", 3, 4, AddressingMode::Absolute),

        OpCode::new(CPX_IMMEDIATE, "CPX", 2, 2, AddressingMode::Immediate),
        OpCode::new(CPX_ZERO_PAGE, "CPX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(CPX_ABSOLUTE, "CPX", 3, 4, AddressingMode::Absolute),


        /* Branching */

        OpCode::new(JMP_ABSOLUTE, "JMP", 3, 3, AddressingMode::NoneAddressing), //AddressingMode that acts as Immidiate
        OpCode::new(JMP_INDIRECT, "JMP", 3, 5, AddressingMode::NoneAddressing), //AddressingMode:Indirect with 6502 bug

        OpCode::new(JSR, "JSR", 3, 6, AddressingMode::NoneAddressing),
        OpCode::new(RTS, "RTS", 1, 6, AddressingMode::NoneAddressing),

        OpCode::new(RTI, "RTI", 1, 6, AddressingMode::NoneAddressing),

        OpCode::new(BNE, "BNE", 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing),
        OpCode::new(BVS, "BVS", 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing),
        OpCode::new(BVC, "BVC", 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing),
        OpCode::new(BMI, "BMI", 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing),
        OpCode::new(BEQ, "BEQ", 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing),
        OpCode::new(BCS, "BCS", 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing),
        OpCode::new(BCC, "BCC", 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing),
        OpCode::new(BPL, "BPL", 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, AddressingMode::NoneAddressing),

        OpCode::new(BIT_ZERO_PAGE, "BIT", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(BIT_ABSOLUTE, "BIT", 3, 4, AddressingMode::Absolute),


        /* Stores, Loads */
        OpCode::new(LDA_IMMEDIATE, "LDA", 2, 2, AddressingMode::Immediate),
        OpCode::new(LDA_ZERO_PAGE, "LDA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(LDA_ZERO_PAGE_X, "LDA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(LDA_ABSOLUTE, "LDA", 3, 4, AddressingMode::Absolute),
        OpCode::new(LDA_ABSOLUTE_X, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),
        OpCode::new(LDA_ABSOLUTE_Y, "LDA", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),
        OpCode::new(LDA_INDIRECT_X, "LDA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(LDA_INDIRECT_Y, "LDA", 2, 5/*+1 if page crossed*/, AddressingMode::Indirect_Y),

        OpCode::new(LDX_IMMEDIATE, "LDX", 2, 2, AddressingMode::Immediate),
        OpCode::new(LDX_ZERO_PAGE, "LDX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(LDX_ZERO_PAGE_Y, "LDX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(LDX_ABSOLUTE, "LDX", 3, 4, AddressingMode::Absolute),
        OpCode::new(LDX_ABSOLUTE_Y, "LDX", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_Y),

        OpCode::new(LDY_IMMEDIATE, "LDY", 2, 2, AddressingMode::Immediate),
        OpCode::new(LDY_ZERO_PAGE, "LDY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(LDY_ZERO_PAGE_X, "LDY", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(LDY_ABSOLUTE, "LDY", 3, 4, AddressingMode::Absolute),
        OpCode::new(LDY_ABSOLUTE_X, "LDY", 3, 4/*+1 if page crossed*/, AddressingMode::Absolute_X),


        OpCode::new(STA_ZERO_PAGE, "STA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(STA_ZERO_PAGE_X, "STA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(STA_ABSOLUTE, "STA", 3, 4, AddressingMode::Absolute),
        OpCode::new(STA_ABSOLUTE_X, "STA", 3, 5, AddressingMode::Absolute_X),
        OpCode::new(STA_ABSOLUTE_Y, "STA", 3, 5, AddressingMode::Absolute_Y),
        OpCode::new(STA_INDIRECT_X, "STA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(STA_INDIRECT_Y, "STA", 2, 6, AddressingMode::Indirect_Y),

        OpCode::new(STX_ZERO_PAGE, "STX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(STX_ZERO_PAGE_Y, "STX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(STX_ABSOLUTE, "STX", 3, 4, AddressingMode::Absolute),

        OpCode::new(STY_ZERO_PAGE, "STY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(STY_ZERO_PAGE_X, "STY", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(STY_ABSOLUTE, "STY", 3, 4, AddressingMode::Absolute),


        /* Flags clear */

        OpCode::new(CLD, "CLD", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(CLI, "CLI", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(CLV, "CLV", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(CLC, "CLC", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(SEC, "SEC", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(SEI, "SEI", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(SED, "SED", 1, 2, AddressingMode::NoneAddressing),

        OpCode::new(TAX, "TAX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(TAY, "TAY", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(TSX, "TSX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(TXA, "TXA", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(TXS, "TXS", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(TYA, "TYA", 1, 2, AddressingMode::NoneAddressing),

        /* Stack */
        OpCode::new(PHA, "PHA", 1, 3, AddressingMode::NoneAddressing),
        OpCode::new(PLA, "PLA", 1, 4, AddressingMode::NoneAddressing),
        OpCode::new(PHP, "PHP", 1, 3, AddressingMode::NoneAddressing),
        OpCode::new(PLP, "PLP", 1, 4, AddressingMode::NoneAddressing),

    ];


    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();
        for cpuop in &*CPU_OPS_CODES {
            map.insert(cpuop.code, cpuop);
        }
        map
    };
}

pub const NOP: u8 = 0xea;
/* Break */
pub const BRK: u8 = 0x00;
/* Increment Value in Register X */

///Register Instruction
///
///Affect Flags: N Z
///
///These instructions are implied mode, have a length of one byte and require two machine cycles.
///
///INY (INcrement Y)        $C8
///
/// http://www.6502.org/tutorials/6502opcodes.html#INY
pub const INY: u8 = 0xc8;
///Register Instruction
///
///Affect Flags: N Z
///
///These instructions are implied mode, have a length of one byte and require two machine cycles.
///
///INX (INcrement X)        $E8
///
/// http://www.6502.org/tutorials/6502opcodes.html#INX
pub const INX: u8 = 0xe8;
///Register Instruction
///
///Affect Flags: N Z
///
///These instructions are implied mode, have a length of one byte and require two machine cycles.
///
///TAX (Transfer A to X)    $AA
///
/// http://www.6502.org/tutorials/6502opcodes.html#TAX
pub const TAX: u8 = 0xaa;
///TAY (Transfer A to Y)    $A8
pub const TAY: u8 = 0xa8;
pub const TSX: u8 = 0xba;
///TXA (Transfer X to A)    $8A
pub const TXA: u8 = 0x8a;
pub const TXS: u8 = 0x9a;
///TYA (Transfer Y to A)    $98
pub const TYA: u8 = 0x98;

///DEX (DEcrement X)        $CA
///DEY (DEcrement Y)        $88

/* modal op codes */
pub const LDA_IMMEDIATE: u8 = 0xa9;
pub const LDA_ZERO_PAGE: u8 = 0xa5;
pub const LDA_ZERO_PAGE_X: u8 = 0xb5;
pub const LDA_ABSOLUTE: u8 = 0xad;
pub const LDA_ABSOLUTE_X: u8 = 0xbd;
pub const LDA_ABSOLUTE_Y: u8 = 0xb9;
pub const LDA_INDIRECT_X: u8 = 0xa1;
pub const LDA_INDIRECT_Y: u8 = 0xb1;

pub const LDX_IMMEDIATE: u8 = 0xa2;
pub const LDX_ZERO_PAGE: u8 = 0xa6;
pub const LDX_ZERO_PAGE_Y: u8 = 0xb6;
pub const LDX_ABSOLUTE: u8 = 0xae;
pub const LDX_ABSOLUTE_Y: u8 = 0xbe;

pub const LDY_IMMEDIATE: u8 = 0xa0;
pub const LDY_ZERO_PAGE: u8 = 0xa4;
pub const LDY_ZERO_PAGE_X: u8 = 0xb4;
pub const LDY_ABSOLUTE: u8 = 0xac;
pub const LDY_ABSOLUTE_X: u8 = 0xbc;

pub const STA_ZERO_PAGE: u8 = 0x85;
pub const STA_ZERO_PAGE_X: u8 = 0x95;
pub const STA_ABSOLUTE: u8 = 0x8d;
pub const STA_ABSOLUTE_X: u8 = 0x9d;
pub const STA_ABSOLUTE_Y: u8 = 0x99;
pub const STA_INDIRECT_X: u8 = 0x81;
pub const STA_INDIRECT_Y: u8 = 0x91;
/// STX (STore X register)
///
/// Affects Flags: none
///
/// MODE           SYNTAX       HEX LEN TIM
///
/// Zero Page     STX $44       $86  2   3
///
/// http://www.6502.org/tutorials/6502opcodes.html#STX
pub const STX_ZERO_PAGE: u8 = 0x86;
/// STX (STore X register)
///
/// Affects Flags: none
///
/// MODE           SYNTAX       HEX LEN TIM
///
/// Zero Page,Y   STX $44,Y     $96  2   4
///
/// http://www.6502.org/tutorials/6502opcodes.html#STX
pub const STX_ZERO_PAGE_Y: u8 = 0x96;
/// STX (STore X register)
///
/// Affects Flags: none
///
/// MODE           SYNTAX       HEX LEN TIM
///
/// Absolute      STX $4400     $8E  3   4
///
/// http://www.6502.org/tutorials/6502opcodes.html#STX
pub const STX_ABSOLUTE: u8 = 0x8e;
/// STY (STore Y register)
///
/// Affects Flags: none
///
/// MODE           SYNTAX       HEX LEN TIM
///
/// Zero Page     STY $44       $84  2   3
///
/// http://www.6502.org/tutorials/6502opcodes.html#STY
pub const STY_ZERO_PAGE: u8 = 0x84;
/// STY (STore Y register)
///
/// Affects Flags: none
///
/// MODE           SYNTAX       HEX LEN TIM
///
/// Zero Page,X   STY $44,X     $94  2   4
///
/// http://www.6502.org/tutorials/6502opcodes.html#STY
pub const STY_ZERO_PAGE_X: u8 = 0x94;
/// STY (STore Y register)
///
/// Affects Flags: none
///
/// MODE           SYNTAX       HEX LEN TIM
///
/// Absolute      STY $4400     $8C  3   4
///
/// http://www.6502.org/tutorials/6502opcodes.html#STY
pub const STY_ABSOLUTE: u8 = 0x8c;

pub const ADC_IMMEDIATE: u8 = 0x69;
pub const ADC_ZERO_PAGE: u8 = 0x65;
pub const ADC_ZERO_PAGE_X: u8 = 0x75;
pub const ADC_ABSOLUTE: u8 = 0x6d;
pub const ADC_ABSOLUTE_X: u8 = 0x7d;
pub const ADC_ABSOLUTE_Y: u8 = 0x79;
pub const ADC_INDIRECT_X: u8 = 0x61;
pub const ADC_INDIRECT_Y: u8 = 0x71;

pub const SBC_IMMEDIATE: u8 = 0xe9;
pub const SBC_ZERO_PAGE: u8 = 0xe5;
pub const SBC_ZERO_PAGE_X: u8 = 0xf5;
pub const SBC_ABSOLUTE: u8 = 0xed;
pub const SBC_ABSOLUTE_X: u8 = 0xfd;
pub const SBC_ABSOLUTE_Y: u8 = 0xf9;
pub const SBC_INDIRECT_X: u8 = 0xe1;
pub const SBC_INDIRECT_Y: u8 = 0xf1;

pub const CMP_IMMEDIATE: u8 = 0xc9;
pub const CMP_ZERO_PAGE: u8 = 0xc5;
pub const CMP_ZERO_PAGE_X: u8 = 0xd5;
pub const CMP_ABSOLUTE: u8 = 0xcd;
pub const CMP_ABSOLUTE_X: u8 = 0xdd;
pub const CMP_ABSOLUTE_Y: u8 = 0xd9;
pub const CMP_INDIRECT_X: u8 = 0xc1;
pub const CMP_INDIRECT_Y: u8 = 0xd1;

pub const CPX_IMMEDIATE: u8 = 0xe0;
pub const CPX_ZERO_PAGE: u8 = 0xe4;
pub const CPX_ABSOLUTE: u8 = 0xec;

pub const CPY_IMMEDIATE: u8 = 0xc0;
pub const CPY_ZERO_PAGE: u8 = 0xc4;
pub const CPY_ABSOLUTE: u8 = 0xcc;

pub const DEC_ZERO_PAGE: u8 = 0xc6;
pub const DEC_ZERO_PAGE_X: u8 = 0xd6;
pub const DEC_ABSOLUTE: u8 = 0xce;
pub const DEC_ABSOLUTE_X: u8 = 0xde;

pub const INC_ZERO_PAGE: u8 = 0xe6;
pub const INC_ZERO_PAGE_X: u8 = 0xf6;
pub const INC_ABSOLUTE: u8 = 0xee;
pub const INC_ABSOLUTE_X: u8 = 0xfe;

pub const ASL_ACCUMULATOR: u8 = 0x0a;
pub const ASL_ZERO_PAGE: u8 = 0x06;
pub const ASL_ZERO_PAGE_X: u8 = 0x16;
pub const ASL_ABSOLUTE: u8 = 0x0e;
pub const ASL_ABSOLUTE_X: u8 = 0x1e;

pub const LSR_ACCUMULATOR: u8 = 0x4a;
pub const LSR_ZERO_PAGE: u8 = 0x46;
pub const LSR_ZERO_PAGE_X: u8 = 0x56;
pub const LSR_ABSOLUTE: u8 = 0x4e;
pub const LSR_ABSOLUTE_X: u8 = 0x5e;

pub const ROL_ACCUMULATOR: u8 = 0x2a;
pub const ROL_ZERO_PAGE: u8 = 0x26;
pub const ROL_ZERO_PAGE_X: u8 = 0x36;
pub const ROL_ABSOLUTE: u8 = 0x2e;
pub const ROL_ABSOLUTE_X: u8 = 0x3e;

pub const ROR_ACCUMULATOR: u8 = 0x6a;
pub const ROR_ZERO_PAGE: u8 = 0x66;
pub const ROR_ZERO_PAGE_X: u8 = 0x76;
pub const ROR_ABSOLUTE: u8 = 0x6e;
pub const ROR_ABSOLUTE_X: u8 = 0x7e;

pub const BIT_ZERO_PAGE: u8 = 0x24;
pub const BIT_ABSOLUTE: u8 = 0x2c;

pub const JMP_ABSOLUTE: u8 = 0x4c;
pub const JMP_INDIRECT: u8 = 0x6c;

pub const JSR: u8 = 0x20;
pub const RTS: u8 = 0x60;
pub const RTI: u8 = 0x40;

pub const BNE: u8 = 0xd0;
pub const BEQ: u8 = 0xf0;
pub const BPL: u8 = 0x10;
pub const BMI: u8 = 0x30;
pub const BVC: u8 = 0x50;
pub const BVS: u8 = 0x70;
pub const BCC: u8 = 0x90;
pub const BCS: u8 = 0xb0;

pub const CLC: u8 = 0x18;
pub const SEC: u8 = 0x38;
pub const CLI: u8 = 0x58;
pub const SEI: u8 = 0x78;
pub const CLV: u8 = 0xb8;
pub const CLD: u8 = 0xd8;
pub const SED: u8 = 0xf8;

pub const PHA: u8 = 0x48;
pub const PLA: u8 = 0x68;
pub const PHP: u8 = 0x08;
pub const PLP: u8 = 0x28;

pub const DEX: u8 = 0xca;
pub const DEY: u8 = 0x88;

pub const AND_IMMEDIATE: u8 = 0x29;
pub const AND_ZERO_PAGE: u8 = 0x25;
pub const AND_ZERO_PAGE_X: u8 = 0x35;
pub const AND_ABSOLUTE: u8 = 0x2d;
pub const AND_ABSOLUTE_X: u8 = 0x3d;
pub const AND_ABSOLUTE_Y: u8 = 0x39;
pub const AND_INDIRECT_X: u8 = 0x21;
pub const AND_INDIRECT_Y: u8 = 0x31;

pub const EOR_IMMEDIATE: u8 = 0x49;
pub const EOR_ZERO_PAGE: u8 = 0x45;
pub const EOR_ZERO_PAGE_X: u8 = 0x55;
pub const EOR_ABSOLUTE: u8 = 0x4d;
pub const EOR_ABSOLUTE_X: u8 = 0x5d;
pub const EOR_ABSOLUTE_Y: u8 = 0x59;
pub const EOR_INDIRECT_X: u8 = 0x41;
pub const EOR_INDIRECT_Y: u8 = 0x51;

pub const ORA_IMMEDIATE: u8 = 0x09;
pub const ORA_ZERO_PAGE: u8 = 0x05;
pub const ORA_ZERO_PAGE_X: u8 = 0x15;
pub const ORA_ABSOLUTE: u8 = 0x0d;
pub const ORA_ABSOLUTE_X: u8 = 0x1d;
pub const ORA_ABSOLUTE_Y: u8 = 0x19;
pub const ORA_INDIRECT_X: u8 = 0x01;
pub const ORA_INDIRECT_Y: u8 = 0x11;
