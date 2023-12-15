use std::io::{self, Read};

#[derive(PartialEq)]
enum Thing {
    Ash,
    Rock,
}

impl Thing {
    fn parse(input: char) -> Self {
        match input {
            '.' => Self::Ash,
            '#' => Self::Rock,
            _ => panic!(),
        }
    }
}

struct Pattern {
    map: Vec<Vec<Thing>>,
}

impl Pattern {
    fn parse(input: &str) -> Self {
        Self {
            map: input
                .lines()
                .map(|line| line.chars().map(Thing::parse).collect())
                .collect(),
        }
    }

    fn find_vertical_reflexion(&self, nb_req_smudges: usize) -> Option<usize> {
        for pos in 0..self.map[0].len() - 1 {
            if self.is_vertical_reflexion(pos, nb_req_smudges) {
                return Some(pos);
            }
        }

        None
    }

    fn find_horizontal_reflexion(&self, nb_req_smudges: usize) -> Option<usize> {
        for pos in 0..self.map.len() - 1 {
            if self.is_horizontal_reflexion(pos, nb_req_smudges) {
                return Some(pos);
            }
        }

        None
    }

    fn is_vertical_reflexion(&self, pos: usize, nb_req_smudges: usize) -> bool {
        let mut i = 0;
        let mut smudges = 0;
        while pos >= i && pos + i + 1 < self.map[0].len() {
            for k in 0..self.map.len() {
                if self.map[k][pos - i] != self.map[k][pos + i + 1] {
                    smudges += 1;
                    if smudges > nb_req_smudges {
                        return false;
                    }
                }
            }
            i += 1;
        }

        nb_req_smudges == smudges
    }

    fn is_horizontal_reflexion(&self, pos: usize, nb_req_smudges: usize) -> bool {
        let mut i = 0;
        let mut smudges = 0;
        while pos >= i && pos + i + 1 < self.map.len() {
            for k in 0..self.map[0].len() {
                if self.map[pos - i][k] != self.map[pos + i + 1][k] {
                    smudges += 1;
                    if smudges > nb_req_smudges {
                        return false;
                    }
                }
            }
            i += 1
        }

        nb_req_smudges == smudges
    }
}

struct Patterns {
    patterns: Vec<Pattern>,
}

impl Patterns {
    fn parse(input: &str) -> Self {
        Self {
            patterns: input
                .lines()
                .collect::<Vec<_>>()
                .split(|line| line.is_empty())
                .map(|lines| Pattern::parse(&lines.join("\n")))
                .collect(),
        }
    }

    fn get_note(&self, nb_req_smudges: usize) -> usize {
        self.patterns
            .iter()
            .map(|pattern| {
                pattern
                    .find_horizontal_reflexion(nb_req_smudges)
                    .map(|pos| 100 * (pos + 1))
                    .or_else(|| pattern.find_vertical_reflexion(nb_req_smudges).map(|pos| pos + 1))
                    .unwrap()
            })
            .sum()
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);
    println!("{}", Patterns::parse(&input).get_note(0));
    println!("{}", Patterns::parse(&input).get_note(1));
}
