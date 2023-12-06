use itertools::Itertools;

type SeedMap = Vec<Vec<(u64, u64, u64)>>;

pub fn main(input: String) {
    let mut fragments = input.split("\n\n");
    let seeds: Vec<u64> = fragments
        .next()
        .unwrap()
        .split_once(": ")
        .unwrap()
        .1
        .split(" ")
        .map(|s| s.parse::<u64>().unwrap())
        .collect();
    let maps: SeedMap = fragments
        .map(|fragment| {
            fragment
                .lines()
                .skip(1)
                .map(|line| {
                    line.split(" ")
                        .map(|s| s.parse::<u64>().unwrap())
                        .collect_tuple()
                        .unwrap()
                })
                .collect()
        })
        .collect();
    let p1 = seeds
        .iter()
        .map(|seed| map_seed(*seed, &maps))
        .min()
        .unwrap();
    println!("Part 1: {}", p1);
    let p2 = seeds
        .iter()
        .tuples()
        .map(|(&from, &len)| from..(from + len))
        .flatten()
        .map(|seed| map_seed(seed, &maps))
        .min()
        .unwrap();
    println!("Part 2: {}", p2)
}

fn map_seed(seed: u64, map: &SeedMap) -> u64 {
    map.iter().fold(seed, |acc, layer| {
        let Some((dst, src, _)) = layer
            .iter()
            .filter(|(_, src, len)| *src <= acc && acc <= src + len)
            .next() else { return acc };
        acc - src + dst
    })
}
