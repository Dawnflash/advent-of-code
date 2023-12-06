use itertools::Itertools;

pub fn main(input: String) {
    let (times, dists) = input
        .lines()
        .map(|l| {
            l.split_once(":")
                .unwrap()
                .1
                .split_whitespace()
                .map(|s| s.parse::<u64>().unwrap())
                .collect_vec()
        })
        .collect_tuple()
        .unwrap();
    let p1: usize = times
        .iter()
        .zip(dists.iter())
        .map(|(&t, &d)| (1..t).filter(|&n| n * (t - n) > d).count())
        .product();
    println!("Part 1: {}", p1);
    let (p2time, p2distance) = input
        .lines()
        .map(|l| {
            l.split_once(":")
                .unwrap()
                .1
                .replace(" ", "")
                .parse::<u64>()
                .unwrap()
        })
        .collect_tuple()
        .unwrap();
    let p2 = (1..p2time)
        .filter(|&n| n * (p2time - n) > p2distance)
        .count();
    println!("Part 2: {}", p2);
}
