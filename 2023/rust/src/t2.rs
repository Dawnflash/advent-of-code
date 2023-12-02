use std::{cmp::max, collections::HashMap, str::FromStr};

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
enum Color {
    Green,
    Red,
    Blue,
}
impl FromStr for Color {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "green" => Ok(Color::Green),
            "red" => Ok(Color::Red),
            "blue" => Ok(Color::Blue),
            _ => Err(()),
        }
    }
}

type Reqs = HashMap<Color, u32>;
struct Game {
    id: usize,
    records: Vec<Reqs>,
}
impl FromStr for Game {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (header, records) = s.split_once(": ").unwrap();
        let id = header.split_once(" ").unwrap().1.parse().unwrap();
        let records = records
            .split("; ")
            .map(|set| {
                set.split(", ")
                    .map(|s| {
                        let (num, color) = s.split_once(" ").unwrap();
                        (color.parse().unwrap(), num.parse().unwrap())
                    })
                    .collect()
            })
            .collect();
        Ok(Game { id, records })
    }
}

fn feasible(reqs: &Reqs, game: &Game) -> bool {
    game.records
        .iter()
        .all(|set| set.iter().all(|(c, n)| reqs.get(c).unwrap_or(&0) >= n))
}

fn min_count(game: &Game) -> Reqs {
    let init_reqs = HashMap::from_iter([(Color::Red, 0), (Color::Green, 0), (Color::Blue, 0)]);
    game.records.iter().fold(init_reqs, |acc, set| {
        acc.iter()
            .map(|(c, n)| (c.clone(), max(*set.get(c).unwrap_or(&0), *n)))
            .collect()
    })
}

pub fn main(input: String) {
    let games: Vec<Game> = input.lines().map(|line| line.parse().unwrap()).collect();

    let p1_reqs = HashMap::from_iter([(Color::Red, 12), (Color::Green, 13), (Color::Blue, 14)]);

    let p1 = games
        .iter()
        .filter(|game| feasible(&p1_reqs, game))
        .map(|game| game.id)
        .sum::<usize>();
    println!("Part 1: {}", p1);

    let p2 = games
        .iter()
        .map(|game| min_count(game).values().product::<u32>())
        .sum::<u32>();
    println!("Part 2: {}", p2);
}
