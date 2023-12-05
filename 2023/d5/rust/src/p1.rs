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
    fn get_dest(&self, source: u64) -> u64 {
        self.units
            .iter()
            .filter(|unit| (unit.source..unit.source + unit.range).contains(&source))
            .map(|unit| source - unit.source + unit.destination)
            .next()
            .unwrap_or(source)
    }
}

#[derive(Debug)]
pub struct Almanac {
    seeds: Vec<u64>,
    mappings: Vec<Mapping>,
}

impl Almanac {
    pub fn parse(input: &str) -> Self {
        let mut iter = input.lines();

        let seeds = iter.next().unwrap()[7..]
            .split_whitespace()
            .map(str::parse)
            .collect::<Result<_, _>>()
            .unwrap();
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
            mappings.push(Mapping { units });
        }

        Self { seeds, mappings }
    }

    pub fn get_locations(&self) -> Vec<u64> {
        self.seeds
            .iter()
            .map(|seed| {
                self.mappings
                    .iter()
                    .fold(*seed, |source, mapping| mapping.get_dest(source))
            })
            .collect()
    }
}
