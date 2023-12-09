use std::io::{self, Read};

fn get_next(seq: &Vec<i64>) -> i64 {
    element(0, 0, seq)
}

fn element(height: usize, offset: usize, seq: &Vec<i64>) -> i64 {
    if offset == 0 {
        if height + 1 == seq.len() {
            0
        } else {
            element(height, offset + 1, seq) + element(height + 1, offset, seq)
        }
    } else {
        if height == 0 {
            seq[seq.len() - offset]
        } else {
            element(height - 1, offset, seq) - element(height - 1, offset + 1, seq)
        }
    }
}

fn get_prev(seq: &Vec<i64>) -> i64 {
    element2(0, 0, seq)
}

fn element2(height: usize, offset: usize, seq: &Vec<i64>) -> i64 {
    if offset == 0 {
        if height + 1 == seq.len() {
            0
        } else {
            element2(height, offset + 1, seq) + element2(height + 1, offset, seq)
        }
    } else {
        if height == 0 {
            seq[offset - 1]
        } else {
            element2(height - 1, offset, seq) - element2(height - 1, offset + 1, seq)
        }
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input);

    println!(
        "{:?}",
        input
            .lines()
            .map(|line| line
                .split_whitespace()
                .map(str::parse)
                .map(Result::unwrap)
                .collect())
            .map(|seq| get_next(&seq))
            .sum::<i64>()
    );

    println!(
        "{:?}",
        input
            .lines()
            .map(|line| line
                .split_whitespace()
                .map(str::parse)
                .map(Result::unwrap)
                .collect())
            .map(|seq| get_prev(&seq))
            .sum::<i64>()
    );
}
