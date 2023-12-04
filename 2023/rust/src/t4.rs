use regex::Regex;
use std::collections::HashSet;

pub fn main(input: String) {
    let nre = Regex::new(r"\d+").unwrap();
    let mut copies: Vec<usize> = vec![1; input.lines().count()];
    let p1: u64 = input
        .lines()
        .enumerate()
        .map(|(id, line)| {
            let (_, content) = line.split_once(": ").unwrap();
            let (winning, guesses) = content.split_once(" | ").unwrap();
            let winning: HashSet<u64> = nre
                .find_iter(winning)
                .map(|m| m.as_str().parse().unwrap())
                .collect();
            let guesses: HashSet<u64> = nre
                .find_iter(guesses)
                .map(|m| m.as_str().parse().unwrap())
                .collect();
            let hits = guesses.iter().filter(|g| winning.contains(g)).count();
            for i in id + 1..id + 1 + hits {
                copies[i] += copies[id];
            }
            if hits == 0 {
                0
            } else {
                1 << (hits - 1)
            }
        })
        .sum();
    println!("Part 1: {}", p1);
    println!("Part 2: {}", copies.iter().sum::<usize>());
}
