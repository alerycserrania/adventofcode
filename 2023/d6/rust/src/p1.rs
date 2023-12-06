use std::io;

#[derive(Debug)]
pub struct Race {
    time: u64,
    distance: u64,
}

impl Race {
    pub fn get_wins(&self) -> Vec<u64> {
        (1..self.time)
            .map(|wait_time| {
                let remaining_time = self.time - wait_time;
                remaining_time * wait_time
            })
            .filter(|distance| self.distance < *distance)
            .collect()
    }
}

pub fn parse() -> Vec<Race> {
    let mut times = String::new();
    let mut distances = String::new();

    io::stdin().read_line(&mut times);
    io::stdin().read_line(&mut distances);

    let times = times
        .split_whitespace()
        .skip(1)
        .map(str::parse)
        .map(Result::unwrap);
    let distances = distances
        .split_whitespace()
        .skip(1)
        .map(str::parse)
        .map(Result::unwrap);

    times
        .zip(distances)
        .map(|(time, distance)| Race { time, distance })
        .collect::<Vec<_>>()
}