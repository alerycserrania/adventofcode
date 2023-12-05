use std::io::{self, Read};

mod p1;
mod p2;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);
    println!(
        "p1: {:?}",
        p1::Almanac::parse(&input)
            .get_locations()
            .iter()
            .min()
            .unwrap()
    );

    println!(
        "p2: {:?}",
        p2::Almanac::parse(&input)
            .get_locations()
            .iter()
            .min_by(|a, b| a.start.cmp(&b.start))
            .unwrap()
            .start
    );
}
