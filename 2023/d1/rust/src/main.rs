use std::io::{self, Read};

const VALUES: [&str; 18] = [
    "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
];

const NUM: [u32; 18] = [
    1, 2, 3, 4, 5, 6, 7, 8, 9,
    1, 2, 3, 4, 5, 6, 7, 8, 9,
];

fn get_calibration_value(line: &str) -> Option<u32> {
    get_first_value(line).map(|fv| get_last_value(line).map(|lv| 10 * fv + lv)).flatten()
}

fn get_first_value(line: &str) -> Option<u32> {
    let mut i = 0;
    while i < line.len() {
        for (index, value) in VALUES.iter().enumerate() { 
            if line[i..].starts_with(value) {
                return Some(NUM[index])
            }
        }
        i += 1;
    }
    
    None
}

fn get_last_value(line: &str) -> Option<u32> {
    let mut i = 0;
    
    while i < line.len() {
        for (index, value) in VALUES.iter().enumerate() { 
            if line[..line.len()-i].ends_with(value) {
                return Some(NUM[index])
            }
        }
        i += 1;
    }
    
    None
}

fn p1(input: &str) -> Result<u32, String> {
    input.lines().enumerate().map(|(index, line)| {
        get_calibration_value(line).ok_or(format!("Calibration value not found line {}", index))
    }).sum()
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);
    println!("{}", p1(&input).unwrap())
}
