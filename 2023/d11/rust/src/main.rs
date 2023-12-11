use std::io::{self, Read};

#[derive(PartialEq)]
enum Space {
    Empty,
    Galaxy,
}

impl Space {
    fn parse(input: char) -> Self {
        match input {
            '.' => Self::Empty,
            '#' => Self::Galaxy,
            _ => panic!(),
        }
    }
}

struct Universe {
    map: Vec<Vec<Space>>,
}

impl Universe {
    fn parse(input: &str) -> Self {
        Self {
            map: input
                .lines()
                .map(|line| line.chars().map(Space::parse).collect())
                .collect(),
        }
    }

    fn get_galaxies(&self) -> Vec<(usize, usize)> {
        let mut galaxies = vec![];

        for row in 0..self.map.len() {
            for col in 0..self.map[row].len() {
                if self.map[row][col] == Space::Galaxy {
                    galaxies.push((row, col))
                }
            }
        }

        galaxies
    }

    fn get_empty_rows(&self) -> Vec<usize> {
        let mut empty_rows = vec![];

        for (row, line) in self.map.iter().enumerate() {
            if line.iter().all(|space| space == &Space::Empty) {
                empty_rows.push(row)
            }
        }

        empty_rows
    }

    fn get_empty_cols(&self) -> Vec<usize> {
        let mut empty_cols = vec![];

        for col in 0..self.map[0].len() {
            let mut empty = true;
            for row in 0..self.map.len() {
                if self.map[row][col] == Space::Galaxy {
                    empty = false;
                    break;
                }
            }
            if empty {
                empty_cols.push(col);
            }
        }

        empty_cols
    }

    fn get_shortest_paths_lengths(&mut self, expansion_rate: usize) -> usize {
        let galaxies = self.get_galaxies();
        let empty_rows = self.get_empty_rows();
        let empty_cols = self.get_empty_cols();

        let mut sum = 0;

        for p1 in 0..galaxies.len() {
            for p2 in p1..galaxies.len() {
                sum += self.get_shortest_path_length(
                    &galaxies[p1],
                    &galaxies[p2],
                    &empty_rows,
                    &empty_cols,
                    expansion_rate
                )
            }
        }

        sum
    }

    fn get_shortest_path_length(
        &self,
        from: &(usize, usize),
        to: &(usize, usize),
        empty_rows: &Vec<usize>,
        empty_cols: &Vec<usize>,
        expansion_rate: usize
    ) -> usize {
        let dy = std::cmp::min(from.0, to.0)..std::cmp::max(from.0, to.0);
        let dx = std::cmp::min(from.1, to.1)..std::cmp::max(from.1, to.1);
        let dist = dy.end - dy.start + dx.end - dx.start;

        dist + (expansion_rate - 1) * empty_rows.iter().filter(|row| dy.contains(row)).count()
            + (expansion_rate - 1) * empty_cols.iter().filter(|col| dx.contains(col)).count()
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);
    println!("{}", Universe::parse(&input).get_shortest_paths_lengths(2));
    println!("{}", Universe::parse(&input).get_shortest_paths_lengths(10));
    println!("{}", Universe::parse(&input).get_shortest_paths_lengths(100));
    println!("{}", Universe::parse(&input).get_shortest_paths_lengths(1_000_000));
}
