use core::panic;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
struct Card {
    value: u8,
}

impl Card {
    fn parse(chr: char) -> Self {
        Self {
            value: match chr {
                'A' => 13,
                'K' => 12,
                'Q' => 11,
                'T' => 10,
                '2'..='9' => chr.to_digit(10).unwrap().try_into().unwrap(),
                'J' => 1,
                _ => panic!("{} not expected", chr),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum HandType {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
        let best_hand = self.get_best_possible_hand();
        let counter = best_hand.counts();

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

    fn get_best_possible_hand(&self) -> Self {
        let counter = self.counts();

        let nb_jokers = *counter.get(&Card { value: 1 }).unwrap_or(&0);

        if nb_jokers == 0 {
            return self.clone();
        }

        let mes = format!("{:?}", counter);
        let best_card = counter
            .into_iter()
            .filter(|(card, count)| card.value != 1)
            .max_by(
                |(card_a, count_a), (card_b, count_b)| match count_a.cmp(count_b) {
                    std::cmp::Ordering::Less => std::cmp::Ordering::Less,
                    std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
                    std::cmp::Ordering::Equal => card_b.cmp(card_a),
                },
            )
            .map(|(card, _)| card);

        let mut new_cards = self
            .cards
            .iter()
            .filter(|card| card.value != 1)
            .map(|card| card.clone())
            .collect::<Vec<_>>();

        match best_card {
            Some(best_card) => new_cards.extend((0..nb_jokers).map(|_| best_card.clone())),
            None => new_cards.extend((0..nb_jokers).map(|_| Card { value: 13 })),
        };

        Self { cards: new_cards }
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
