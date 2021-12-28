pub fn combine_u8_to_u16(hi: u8, lo: u8) -> u16 {
    (hi as u16) << 8 | (lo as u16)
}
