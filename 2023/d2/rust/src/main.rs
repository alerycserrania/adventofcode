use std::io::{self, Read};

#[derive(Debug)]
struct GameSet {
    red: u64,
    blue: u64,
    green: u64,
}

#[derive(Debug)]
struct Game {
    id: u64,
    sets: Vec<GameSet>,
}

impl GameSet {
    fn parse(input: &str) -> Self {
        let mut set = GameSet {
            red: 0,
            blue: 0,
            green: 0,
        };
        for el in input.split(", ") {
            let (nb, color) = el.split_once(" ").unwrap();
            match color {
                "red" => set.red = str::parse(nb).unwrap(),
                "blue" => set.blue = str::parse(nb).unwrap(),
                "green" => set.green = str::parse(nb).unwrap(),
                _ => panic!("Unpexcted color {}", color),
            }
        }
        set
    }

    fn can_contain(&self, other_set: &Self) -> bool {
        self.red <= other_set.red && self.blue <= other_set.blue && self.green <= other_set.green
    }
}

impl Game {
    fn parse(input: &str) -> Self {
        let (game_input, remainings) = input.split_once(": ").unwrap();
        Self {
            id: str::parse(game_input.split_once(" ").unwrap().1).unwrap(),
            sets: remainings.split("; ").map(GameSet::parse).collect(),
        }
    }

    fn can_contain(&self, set: &GameSet) -> bool {
        self.sets.iter().all(|game_set| game_set.can_contain(set))
    }

    fn get_min_set(&self) -> GameSet {
        GameSet {
            red: self.sets.iter().map(|set| set.red).max().unwrap_or(0),
            blue: self.sets.iter().map(|set| set.blue).max().unwrap_or(0),
            green: self.sets.iter().map(|set| set.green).max().unwrap_or(0),
        }
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);
    println!(
        "P1: {:?}",
        input
            .lines()
            .map(Game::parse)
            .filter(|game| game.can_contain(&GameSet {
                red: 12,
                green: 13,
                blue: 14
            }))
            .map(|game| game.id)
            .sum::<u64>()
    );
    println!(
        "P2: {:?}",
        input
            .lines()
            .map(Game::parse)
            .map(|game| game.get_min_set())
            .map(|game| game.red * game.blue * game.green)
            .sum::<u64>()
    );
}
