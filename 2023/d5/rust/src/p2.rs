use std::{ops::Range, thread::current, vec};

#[derive(Debug)]
struct MappingUnit {
    destination: u64,
    source: u64,
    range: u64,
}

impl MappingUnit {
    fn parse(input: &str) -> Self {
        let p = input
            .split_whitespace()
            .map(str::parse)
            .take(3)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        MappingUnit {
            destination: p[0],
            source: p[1],
            range: p[2],
        }
    }
}
#[derive(Debug)]
struct Mapping {
    units: Vec<MappingUnit>,
}

impl Mapping {
    fn get_dest(&self, seed_ranges: &Vec<Range<u64>>) -> Vec<Range<u64>> {
        seed_ranges
            .iter()
            .map(|seed_range| self.get_dest_range(seed_range))
            .flatten()
            .collect()
    }

    fn get_dest_range(&self, seed_range: &Range<u64>) -> Vec<Range<u64>> {
        let mut iter = self.units.iter();

        let mut dest = vec![];
        let mut current_range = seed_range.clone();

        while let Some(unit) = iter.next() {
            let range = unit.source..unit.source + unit.range - 1;
            if current_range.start < range.start {
                if current_range.end < range.start {
                    dest.push(current_range.clone());
                    return dest;
                } else if current_range.end <= range.end {
                    dest.push(current_range.start..range.start - 1);
                    dest.push(
                        unit.destination..unit.destination + (current_range.end - range.start),
                    );
                    return dest;
                } else {
                    dest.push(current_range.start..range.start - 1);
                    dest.push(unit.destination..unit.destination + unit.range);
                    current_range = range.end + 1..current_range.end;
                }
            } else if current_range.start <= range.end {
                if current_range.end <= range.end {
                    dest.push(
                        unit.destination + (current_range.start - range.start)
                            ..unit.destination + (current_range.end - range.start),
                    );
                    return dest;
                } else {
                    dest.push(
                        unit.destination + (current_range.start - range.start)
                            ..unit.destination + (range.end - range.start),
                    );
                    current_range = range.end + 1..current_range.end;
                }
            } else {
                continue;
            }
        }
        dest.push(current_range);
        dest
    }
}

#[derive(Debug)]
pub struct Almanac {
    seeds: Vec<Range<u64>>,
    mappings: Vec<Mapping>,
}

impl Almanac {
    pub fn parse(input: &str) -> Self {
        let mut iter = input.lines();

        let seeds = iter.next().unwrap()[7..]
            .split_whitespace()
            .map(str::parse)
            .collect::<Result<Vec<u64>, _>>()
            .unwrap()
            .chunks(2)
            .map(|chunk| (chunk[0]..chunk[0] + chunk[1] - 1))
            .collect();

        iter.next().unwrap(); // new line

        let mut mappings = vec![];

        loop {
            if iter.next().is_none() {
                break;
            }

            let mut units = vec![];
            while let Some(line) = iter.next() {
                if line.is_empty() {
                    break;
                }

                units.push(MappingUnit::parse(line))
            }
            units.sort_by(|a, b| (a.source).cmp(&b.source));
            mappings.push(Mapping { units });
        }

        Self { seeds, mappings }
    }

    pub fn get_locations(&mut self) -> Vec<Range<u64>> {
        self.seeds
            .iter()
            .map(|seed| {
                self.mappings
                    .iter()
                    .fold(vec![seed.clone()], |ranges, mapping| {
                        mapping.get_dest(&ranges)
                    })
            })
            .flatten()
            .collect()
    }
}
