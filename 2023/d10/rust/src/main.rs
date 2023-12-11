use std::{
    collections::HashSet,
    io::{self, Read},
};

#[derive(Debug)]
enum Direction {
    East,
    West,
    North,
    South,
}

impl Direction {
    fn go_from(&self, (row, col): &(usize, usize)) -> (usize, usize) {
        match self {
            Direction::East => (*row, *col + 1),
            Direction::West => (*row, *col - 1),
            Direction::North => (*row - 1, *col),
            Direction::South => (*row + 1, *col),
        }
    }
}

#[derive(PartialEq)]
enum Pipe {
    NS,
    EW,
    NE,
    NW,
    SW,
    SE,
    Ground,
    Start,
}

impl Pipe {
    fn parse(input: char) -> Self {
        match input {
            '|' => Pipe::NS,
            '-' => Pipe::EW,
            'L' => Pipe::NE,
            'J' => Pipe::NW,
            '7' => Pipe::SW,
            'F' => Pipe::SE,
            '.' => Pipe::Ground,
            'S' => Pipe::Start,
            _ => panic!("unexpected char: {}", input),
        }
    }

    fn is_south(&self) -> bool {
        [Pipe::NS, Pipe::SE, Pipe::SW].contains(self)
    }

    fn is_north(&self) -> bool {
        [Pipe::NS, Pipe::NE, Pipe::NW].contains(self)
    }

    fn is_east(&self) -> bool {
        [Pipe::EW, Pipe::NE, Pipe::SE].contains(self)
    }

    fn is_west(&self) -> bool {
        [Pipe::EW, Pipe::NW, Pipe::SW].contains(self)
    }

    fn next_from(&self, dir: &Direction) -> Direction {
        match self {
            Pipe::NS => match dir {
                Direction::North => Direction::North,
                Direction::South => Direction::South,
                _ => panic!(),
            },
            Pipe::EW => match dir {
                Direction::East => Direction::East,
                Direction::West => Direction::West,
                _ => panic!(),
            },
            Pipe::NE => match dir {
                Direction::South => Direction::East,
                Direction::West => Direction::North,
                _ => panic!(),
            },
            Pipe::NW => match dir {
                Direction::East => Direction::North,
                Direction::South => Direction::West,
                _ => panic!(),
            },
            Pipe::SW => match dir {
                Direction::North => Direction::West,
                Direction::East => Direction::South,
                _ => panic!(),
            },
            Pipe::SE => match dir {
                Direction::West => Direction::South,
                Direction::North => Direction::East,
                _ => panic!(),
            },
            Pipe::Ground => panic!(),
            Pipe::Start => panic!(),
        }
    }
}

struct Map {
    map: Vec<Vec<Pipe>>,
}

impl Map {
    fn parse(input: &str) -> Self {
        Self {
            map: input
                .lines()
                .map(|line| line.chars().map(Pipe::parse).collect())
                .collect(),
        }
    }

    fn find_start(&self) -> Option<(usize, usize)> {
        for (y, line) in self.map.iter().enumerate() {
            for (x, pipe) in line.iter().enumerate() {
                if pipe == &Pipe::Start {
                    return Some((y, x));
                }
            }
        }

        None
    }

    fn get_loop_points(&self) -> Vec<(usize, usize)> {
        let current_pos = self.find_start().unwrap();
        let mut next_dir = self.next_from(&current_pos);
        let mut next_pos = next_dir.go_from(&current_pos);
        let mut loop_points = vec![current_pos, next_pos];

        while self.map[next_pos.0][next_pos.1] != Pipe::Start {
            next_dir = self.map[next_pos.0][next_pos.1].next_from(&next_dir);
            next_pos = next_dir.go_from(&next_pos);
            loop_points.push(next_pos);
        }

        loop_points
    }

    fn get_loop_size(&self) -> usize {
        (self.get_loop_points().len() + 1) / 2
    }

    fn next_from(&self, pos: &(usize, usize)) -> Direction {
        if pos.0 > 0 && self.map[pos.0 - 1][pos.1].is_south() {
            Direction::North
        } else if pos.0 + 1 < self.map.len() && self.map[pos.0 + 1][pos.1].is_north() {
            Direction::South
        } else if pos.1 > 0 && self.map[pos.0][pos.1 - 1].is_east() {
            Direction::West
        } else if pos.1 + 1 < self.map[pos.0].len() && self.map[pos.0][pos.1 + 1].is_west() {
            Direction::East
        } else {
            panic!("should not happen")
        }
    }

    fn get_points_inside_loop(&self) -> Vec<(usize, usize)> {
        let loop_points = self.get_loop_points();
        let loop_points = loop_points.iter().collect::<HashSet<_>>();
        let mut points_inside = vec![];

        for (row, line) in self.map.iter().enumerate() {
            let mut inside = false;
            for (col, pipe) in line.iter().enumerate() {
                if loop_points.contains(&(row, col)) {
                    if self.is_intersection(&self.map[row][col]) {
                        inside = !inside;
                    }
                } else {
                    if inside {
                        points_inside.push((row, col))
                    }
                }
            }
        }

        points_inside
    }

    fn is_intersection(&self, cur_pipe: &Pipe) -> bool {
        match cur_pipe {
            Pipe::NS => true,
            Pipe::EW => false,
            Pipe::NE => false,
            Pipe::NW => false,
            Pipe::SW => true,
            Pipe::SE => true,
            Pipe::Ground => false,
            Pipe::Start => true, // We need to find the corresponding tile but works for now
        }
    }

    fn get_nb_points_inside_loop(&self) -> usize {
        self.get_points_inside_loop().len()
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);

    println!("{}", Map::parse(&input).get_loop_size());
    println!("{}", Map::parse(&input).get_nb_points_inside_loop());
}
