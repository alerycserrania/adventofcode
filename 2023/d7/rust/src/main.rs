use std::io::{self, Read};

mod p1;
mod p2;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);

    let mut hands = p1::Hands::parse(&input);

    println!(
        "P1 {:?}",
        hands
            .sort()
            .iter()
            .rev()
            .enumerate()
            .map(|(rank, hand)| (rank + 1) as u64 * hand.bid())
            .sum::<u64>()
    );
    
    let mut hands = p2::Hands::parse(&input);

    println!(
        "P2 {:?}",
        hands
            .sort()
            .iter()
            .rev()
            .enumerate()
            .map(|(rank, hand)| (rank + 1) as u64 * hand.bid())
            .sum::<u64>()
    );
}
