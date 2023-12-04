use std::io::{self, Read};

#[derive(Debug)]
struct Card {
    id: u32,
    winning_numbers: Vec<u8>,
    my_numbers: Vec<u8>,
}

impl Card {
    fn parse(input: &str) -> Self {
        let (card_name, remainings) = input.split_once(": ").unwrap();
        let (winning_numbers, my_numbers) = remainings.split_once(" | ").unwrap();
        
        Card {
            id: str::parse(card_name.split_whitespace().last().unwrap()).unwrap(),
            winning_numbers: winning_numbers.split_whitespace().map(str::parse).collect::<Result<_, _>>().unwrap(),
            my_numbers: my_numbers.split_whitespace().map(str::parse).collect::<Result<_, _>>().unwrap(),
        }
    }

    fn matches(&self) -> Vec<&u8> {
        self.winning_numbers.iter().filter(|winning_number| self.my_numbers.contains(winning_number)).collect()
    }
}

struct Cards {
    cards: Vec<Card>,
}

impl Cards {
    fn parse(input: &str) -> Self {
        Self {
            cards: input.lines().map(Card::parse).collect()
        }
    }

    fn get_nb_copies(&self) -> Vec<u32> {
        let mut nb_copies = vec![1u32; self.cards.len()];

        for (index, card_matches) in self.cards.iter().map(Card::matches).enumerate() {
            for offset in 0..card_matches.len() {
                nb_copies[index + offset + 1] += nb_copies[index];
            }
        }

        nb_copies
    }
}


fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);
    let cards = Cards::parse(&mut input);
    let nb_copies = cards.get_nb_copies();

    // println!("{:?}", cards.cards.iter().map(Card::matches).filter(|numbers| numbers.len() > 0).map(|numbers| 1 << (numbers.len()-1)).sum::<u64>());
    println!("{:?}", nb_copies.iter().sum::<u32>());
}
