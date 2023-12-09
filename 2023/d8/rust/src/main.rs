use std::{
    collections::HashMap,
    io::{self, Read},
    vec,
};

mod gcd;

enum Input {
    Left,
    Right,
}

impl Input {
    fn parse(input: char) -> Self {
        match input {
            'L' => Self::Left,
            'R' => Self::Right,
            _ => panic!("{}", input),
        }
    }
}

struct Instructions {
    inputs: Vec<Input>,
}

impl Instructions {
    fn parse(input: &str) -> Self {
        Self {
            inputs: input.chars().map(Input::parse).collect(),
        }
    }
}

struct Node {
    name: String,
    left: String,
    right: String,
}

impl Node {
    fn parse(input: &str) -> Self {
        let (name, left_right) = input.split_once(" = ").expect(input);
        let (left, right) = left_right[1..left_right.len() - 1]
            .split_once(", ")
            .expect(left_right);

        Self {
            name: name.to_owned(),
            left: left.to_owned(),
            right: right.to_owned(),
        }
    }
}

struct Network {
    instructions: Instructions,
    nodes: HashMap<String, Node>,
}

impl Network {
    fn parse(input: &str) -> Self {
        let mut lines = input.lines();
        let instructions = Instructions::parse(lines.next().expect("instructions"));
        lines.next().expect("line break after instructions");
        let nodes = lines
            .map(Node::parse)
            .map(|node| (node.name.clone(), node))
            .collect();

        Self {
            instructions,
            nodes,
        }
    }

    fn nb_steps_to_zzz(&self) -> u64 {
        let mut node = "AAA";
        let mut nb_steps = 0;
        let mut instr_cycle = self.instructions.inputs.iter().cycle();

        while node != "ZZZ" {
            node = instr_cycle
                .next()
                .map(|instr| match instr {
                    Input::Left => &self.nodes.get(node).unwrap().left,
                    Input::Right => &self.nodes.get(node).unwrap().right,
                })
                .expect("instruction");

            nb_steps += 1;
        }

        nb_steps
    }

    fn cycle<'a>(&'a self, mut node: &'a str) -> (Vec<i64>, i64, i64) {
        let mut visited: Vec<&'a str> = vec![node];
        let mut instr_cycle = self.instructions.inputs.iter().cycle();

        loop {
            node = instr_cycle
                .next()
                .map(|instr| match instr {
                    Input::Left => &self.nodes.get(node).unwrap().left,
                    Input::Right => &self.nodes.get(node).unwrap().right,
                })
                .expect("instruction");

            let mut i = visited.len();

            while i >= self.instructions.inputs.len() {
                i -= self.instructions.inputs.len();
                if visited[i] == node {
                    let cycle_len = visited.len();
                    let offset = i;
                    let zs = visited
                        .iter()
                        .enumerate()
                        .filter(|(i, node)| node.ends_with("Z"))
                        .map(|(i, _)| i.try_into().unwrap())
                        .collect::<Vec<i64>>();
                    return (
                        zs,
                        cycle_len.try_into().unwrap(),
                        offset.try_into().unwrap(),
                    );
                }
            }

            visited.push(node);
        }
    }

    fn steps_to_z(&self) -> i64 {
        // not satisfied at all because it only works in this specific case:
        // - there is only one z ending per a node
        // - the cycle repeats after finding that one z ending
        self
            .nodes
            .keys()
            .filter(|node| node.ends_with("A"))
            .map(|node| self.cycle(node))
            .map(|node| node.1 - node.2)
            .reduce(|acc, e| gcd::lcd(acc, e))
            .unwrap()

        
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);

    // println!("{:?}", Network::parse(&input).cycle("BGA"));
    // println!("{:?}", Network::parse(&input).cycle("SLA"));
    // println!("{:?}", Network::parse(&input).cycle("PTA"));
    // println!("{:?}", Network::parse(&input).cycle("AAA"));
    // println!("{:?}", Network::parse(&input).cycle("XJA"));
    // println!("{:?}", Network::parse(&input).cycle("JNA"));
    println!("{:?}", Network::parse(&input).steps_to_z());

    // println!("{:?}", Network::parse(&input).cycle("11A"));
    // println!("{:?}", Network::parse(&input).cycle("22A"));
}
