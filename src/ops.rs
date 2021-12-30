//these operations break syntax highlighting and that's why they are here
pub fn combine_u8_to_u16(hi: u8, lo: u8) -> u16 {
    (hi as u16) << 8 | (lo as u16)
}

pub fn split_u16_to_u8(val: u16) -> (u8, u8) {
    //hi, lo
    ((val >> 8) as u8, (val & 0xff) as u8)
}

pub fn is_one(data: u8) -> bool {
    data & 1 == 1
}

pub fn r_shift(data: u8, shift: u8) -> u8 {
    data >> shift //& 0xff
}

pub fn l_shift(data: u8, shift: u8) -> u8 {
    data << shift //& 0xff
}
