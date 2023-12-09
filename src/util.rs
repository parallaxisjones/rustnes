use std::fs::File;
use std::io::Read;

pub fn load_bytes_from_file(path: &str) -> std::io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

pub fn snake_game_code() -> Vec<u8> {
    let mut snake_game = load_bytes_from_file("roms/snake.ch8").unwrap();
    snake_game.append(&mut vec![0; 4096 - snake_game.len()]);
    snake_game
}