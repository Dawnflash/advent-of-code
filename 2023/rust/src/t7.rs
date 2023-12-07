use std::collections::{HashMap, HashSet};

use itertools::Itertools;

const CARDS: [char; 13] = [
    '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A',
];
const CARDS_2: [char; 13] = [
    'J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A',
];

pub fn main(input: String) {
    println!("Part 1: {}", part(input.as_str(), 1));
    println!("Part 2: {}", part(input.as_str(), 2));
}

fn part(input: &str, part: usize) -> u64 {
    let cards = if part == 1 { &CARDS } else { &CARDS_2 };
    let card_strengths: HashMap<char, u64> = cards
        .iter()
        .enumerate()
        .map(|(n, c)| (*c, n as u64))
        .collect();
    let hands = input
        .lines()
        .map(|line| {
            let (cards, bid) = line.split_once(" ").unwrap();
            let bid = bid.parse::<u64>().unwrap();
            let strength = hand_strength(&cards, &card_strengths, part == 2);
            (strength, bid)
        })
        .collect_vec();
    let score_map: HashMap<u64, u64> = hands
        .iter()
        .map(|(s, _)| *s)
        .collect::<HashSet<u64>>()
        .iter()
        .sorted()
        .enumerate()
        .map(|(n, s)| (*s, n as u64 + 1))
        .collect();
    hands
        .iter()
        .map(|(s, b)| score_map.get(s).unwrap() * b)
        .sum()
}

fn hand_strength(hand: &str, card_strengths: &HashMap<char, u64>, jokers: bool) -> u64 {
    let type_strength: u64 = if jokers {
        CARDS
            .iter()
            .map(|c| score_hand_types(&hand.replace("J", &c.to_string())))
            .max()
            .unwrap()
    } else {
        score_hand_types(hand)
    };
    13u64.pow(5) * type_strength
        + hand
            .chars()
            .rev()
            .enumerate()
            .map(|(n, c)| 13u64.pow(n as u32) * card_strengths.get(&c).unwrap())
            .sum::<u64>()
}

fn score_hand_types(hand: &str) -> u64 {
    let counts: HashMap<char, u64> = hand.chars().fold(HashMap::new(), |mut m, c| {
        *m.entry(c).or_insert(0) += 1;
        m
    });
    let inv_counts: HashMap<u64, char> = counts.iter().map(|(&a, &b)| (b, a)).collect();
    match counts.len() {
        5 => 1,
        4 => 2,
        3 => {
            if inv_counts.contains_key(&3) {
                4
            } else {
                3
            }
        }
        2 => {
            if inv_counts.contains_key(&4) {
                6
            } else {
                5
            }
        }
        _ => 7,
    }
}
