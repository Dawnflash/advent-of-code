use aoc2023::*;
use nom::{character::complete::char, sequence::separated_pair};

#[derive(Debug)]
struct Range(u64, u64);

pub fn main(input: String) {
    let mut part1 = 0;
    let mut part2 = 0;
    for line in input.lines() {
        let (a, b) = parse(line).expect("parse error").1;
        if contains(&a, &b) || contains(&b, &a) {
            part1 += 1;
        }
        if overlaps(&a, &b) || overlaps(&b, &a) {
            part2 += 1;
        }
    }
    println!("{part1}");
    println!("{part2}");
}

fn contains(a: &Range, b: &Range) -> bool {
    a.0 >= b.0 && a.1 <= b.1
}

fn overlaps(a: &Range, b: &Range) -> bool {
    a.0 >= b.0 && a.0 <= b.1
}

fn parse_tuple(i: &str) -> nom::IResult<&str, (u64, u64)> {
    separated_pair(parse_int, char('-'), parse_int)(i)
}

fn parse_range(i: &str) -> nom::IResult<&str, Range> {
    nom::combinator::map(&parse_tuple, |(a, b)| Range(a, b))(i)
}

fn parse(i: &str) -> nom::IResult<&str, (Range, Range)> {
    separated_pair(parse_range, char(','), parse_range)(i)
}
