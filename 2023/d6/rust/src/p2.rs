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

pub fn parse() -> Race {
    let mut time = String::new();
    let mut distance = String::new();

    io::stdin().read_line(&mut time);
    io::stdin().read_line(&mut distance);
    
    let time =
        str::parse::<u64>(&time.split_whitespace().skip(1).collect::<Vec<_>>().join("")).unwrap();

    let distance = str::parse::<u64>(
        &distance
            .split_whitespace()
            .skip(1)
            .collect::<Vec<_>>()
            .join(""),
    )
    .unwrap();

    Race { time, distance }
}
