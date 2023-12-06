mod p1;
mod p2;

fn main() {
    // let races = p1::parse();

    // println!(
    //     "{:?}",
    //     races.iter().map(p1::Race::get_wins).map(|wins| wins.len()).reduce(|acc, e| acc * e).unwrap()
    // );

    let race = p2::parse();

    println!(
        "{:?}",
        race.get_wins().len()
    );
}
