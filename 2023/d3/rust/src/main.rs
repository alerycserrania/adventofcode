use std::{
    collections::{HashMap, HashSet},
    io::{self, Read},
    ops::Range,
};

use itertools::Itertools;
use regex::Regex;

struct Schematic {
    map: Vec<String>,
}

#[derive(Debug)]
struct SchematicNumber<'a> {
    number: &'a str,
    row: usize,
    col_range: Range<usize>,
}

#[derive(Debug)]
struct SchematicSymbol {
    symbol: char,
    row: usize,
    col: usize,
}
#[derive(Debug)]
struct SchematicNumberWithGears<'a> {
    number: SchematicNumber<'a>,
    gears: Vec<SchematicSymbol>,
}

fn get_adjacent(row: usize, col: usize) -> Vec<(usize, usize)> {
    let mut adjacents = Vec::new();

    if row > 0 {
        adjacents.push((row - 1, col));
        adjacents.push((row - 1, col + 1));

        if col > 0 {
            adjacents.push((row - 1, col - 1));
        }
    }

    if col > 0 {
        adjacents.push((row + 1, col - 1));
        adjacents.push((row, col - 1));
    }

    adjacents.push((row + 1, col));
    adjacents.push((row + 1, col + 1));
    adjacents.push((row, col + 1));

    adjacents
}

impl Schematic {
    fn parse(input: &str) -> Self {
        Self {
            map: input.lines().map(String::from).collect(),
        }
    }

    fn get_numbers(&self) -> Vec<SchematicNumber> {
        let num_regex = Regex::new(r"\d+").unwrap();
        self.map
            .iter()
            .enumerate()
            .map(|(row, line)| {
                num_regex.find_iter(line).map(move |m| SchematicNumber {
                    number: m.as_str(),
                    row,
                    col_range: m.range(),
                })
            })
            .flatten()
            .collect()
    }

    fn get_part_numbers(&self) -> Vec<SchematicNumber> {
        self.get_numbers()
            .into_iter()
            .filter(|number| {
                number
                    .col_range
                    .clone()
                    .map(|col| get_adjacent(number.row, col))
                    .flatten()
                    .any(|(row, col)| {
                        self.map.get(row).map_or(false, |line| {
                            line.chars()
                                .nth(col)
                                .map_or(false, |chr| !chr.is_digit(10) && chr != '.')
                        })
                    })
            })
            .collect()
    }

    fn get_gears(&self) -> HashMap<(usize, usize), Vec<u64>> {
        let numbers = self
            .get_numbers()
            .into_iter()
            .map(|number| SchematicNumberWithGears {
                gears: number
                    .col_range
                    .clone()
                    .map(|col| get_adjacent(number.row, col))
                    .flatten()
                    .unique()
                    .filter(|(row, col)| {
                        self.map.get(*row).map_or(false, |line| {
                            line.chars().nth(*col).map_or(false, |chr| chr == '*')
                        })
                    })
                    .map(|(row, col)| SchematicSymbol {
                        symbol: '*',
                        row,
                        col,
                    })
                    .collect(),
                number,
            });

        let mut gears = HashMap::<(usize, usize), Vec<u64>>::new();

        for number in numbers {
            for gear in number.gears {
                gears
                    .entry((gear.row, gear.col))
                    .or_insert(Vec::new())
                    .push(str::parse::<u64>(number.number.number).unwrap());
            }
        }

        gears
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);

    println!(
        "{}",
        Schematic::parse(&input)
            .get_gears()
            .values()
            .filter(|values| values.len() == 2)
            .map(|values| values.iter().fold(1, |acc, v| acc * v))
            .sum::<u64>()
    );
}
