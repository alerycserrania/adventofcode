use core::panic;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Card {
    value: u8,
}

impl Card {
    fn parse(chr: char) -> Self {
        Self {
            value: match chr {
                'A' => 14,
                'K' => 13,
                'Q' => 12,
                'J' => 11,
                'T' => 10,
                '2'..='9' => chr.to_digit(10).unwrap().try_into().unwrap(),
                _ => panic!("{} not expected", chr),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum HandType {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

#[derive(Debug, PartialEq, Eq)]
struct Cards {
    cards: Vec<Card>,
}

impl Cards {
    fn parse(input: &str) -> Self {
        Self {
            cards: input.chars().map(Card::parse).collect(),
        }
    }

    fn hand_type(&self) -> HandType {
        let counter = self.counts();

        match counter.len() {
            1 => HandType::FiveOfAKind,
            2 => {
                if counter.values().any(|count| count == &1 || count == &4) {
                    HandType::FourOfAKind
                } else {
                    HandType::FullHouse
                }
            }
            3 => {
                if counter.values().any(|count| count == &3) {
                    HandType::ThreeOfAKind
                } else {
                    HandType::TwoPair
                }
            }
            4 => HandType::OnePair,
            5 => HandType::HighCard,
            _ => panic!("should not happen: {:?}", counter),
        }
    }

    fn counts(&self) -> HashMap<&Card, u8> {
        let mut counter = HashMap::new();

        for card in self.cards.iter() {
            counter
                .entry(card)
                .and_modify(|counter| *counter += 1)
                .or_insert(1);
        }

        counter
    }
}

impl PartialOrd for Cards {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.hand_type()
            .partial_cmp(&other.hand_type())
            .map(|ord| match ord {
                std::cmp::Ordering::Less => Some(std::cmp::Ordering::Less),
                std::cmp::Ordering::Greater => Some(std::cmp::Ordering::Greater),
                std::cmp::Ordering::Equal => other.cards.partial_cmp(&self.cards),
            })
            .flatten()
    }
}

#[derive(Debug)]
pub struct Hand {
    cards: Cards,
    bid: u64,
}

impl Hand {
    fn parse(input: &str) -> Self {
        let (hand, bid) = input.split_once(' ').unwrap();
        Self {
            cards: Cards::parse(hand),
            bid: str::parse(bid).unwrap(),
        }
    }

    pub fn bid(&self) -> u64 {
        self.bid
    }
}

pub struct Hands {
    hands: Vec<Hand>,
}

impl Hands {
    pub fn parse(input: &str) -> Self {
        Self {
            hands: input.lines().map(Hand::parse).collect(),
        }
    }

    pub fn sort(&mut self) -> &Vec<Hand> {
        self.hands
            .sort_by(|hand_a, hand_b| hand_a.cards.partial_cmp(&hand_b.cards).unwrap());
        &self.hands
    }
}
