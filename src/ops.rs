pub fn combine_u8_to_u16(hi: u8, lo: u8) -> u16 {
    (hi as u16) << 8 | (lo as u16)
}

pub fn split_u16_to_u8(val: u16) -> (u8, u8) {
    //hi, lo
    ((val >> 8) as u8, (val & 0xff) as u8)
}
