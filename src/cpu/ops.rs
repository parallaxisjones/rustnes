pub const NOP: u8 = 0xea;

/// Break
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

///TSX (Transfer S to X)    $BA
pub const TSX: u8 = 0xba;

///TXA (Transfer X to A)    $8A
pub const TXA: u8 = 0x8a;

///TXS (Transfer X to S)    $9A
pub const TXS: u8 = 0x9a;

/// TYA (Transfer Y to A)    $98
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

/// ADC (ADd with Carry)
/// Affects Flags: S V Z C
/// MODE           SYNTAX       HEX LEN TIM
/// Immediate     ADC #$44      $69  2   2
/// Zero Page     ADC $44       $65  2   3
/// Zero Page,X   ADC $44,X     $75  2   4
/// Absolute      ADC $4400     $6D  3   4
/// Absolute,X    ADC $4400,X   $7D  3   4+
/// Absolute,Y    ADC $4400,Y   $79  3   4+
/// Indirect,X    ADC ($44,X)   $61  2   6
/// Indirect,Y    ADC ($44),Y   $71  2   5+
/// + add 1 cycle if page boundary crossed
/// http://www.6502.org/tutorials/6502opcodes.html#ADC
pub const ADC_IMMEDIATE: u8 = 0x69;
pub const ADC_ZERO_PAGE: u8 = 0x65;
pub const ADC_ZERO_PAGE_X: u8 = 0x75;
pub const ADC_ABSOLUTE: u8 = 0x6d;
pub const ADC_ABSOLUTE_X: u8 = 0x7d;
pub const ADC_ABSOLUTE_Y: u8 = 0x79;
pub const ADC_INDIRECT_X: u8 = 0x61;
pub const ADC_INDIRECT_Y: u8 = 0x71;

/// SBC (SuBtract with Carry)
/// Affects Flags: S V Z C
/// MODE           SYNTAX       HEX LEN TIM
/// Immediate     SBC #$44      $E9  2   2
/// Zero Page     SBC $44       $E5  2   3
/// Zero Page,X   SBC $44,X     $F5  2   4
/// Absolute      SBC $4400     $ED  3   4
/// Absolute,X    SBC $4400,X   $FD  3   4+
/// Absolute,Y    SBC $4400,Y   $F9  3   4+
/// Indirect,X    SBC ($44,X)   $E1  2   6
/// Indirect,Y    SBC ($44),Y   $F1  2   5+
/// + add 1 cycle if page boundary crossed
/// http://www.6502.org/tutorials/6502opcodes.html#SBC
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