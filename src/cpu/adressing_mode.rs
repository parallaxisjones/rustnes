use std::fmt::Display;

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