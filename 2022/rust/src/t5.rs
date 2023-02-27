use aoc2022::*;
use itertools::Itertools;
use nom::{
    bytes::complete::tag, character::complete::anychar, combinator::map, sequence::delimited,
};

#[derive(Debug)]
struct Move(usize, usize, usize);

pub fn main(input: String) {
    let (crates, moves) = input.split_once("\n\n").unwrap();
    let mut crates = gen_crates(crates.lines().rev().skip(1).collect_vec());
    let mut crates2 = crates.clone();
    for line in moves.lines() {
        let mov = parse_move(line).unwrap().1;
        // part 1 = one crate at a time
        for _ in 0..mov.0 {
            let c = crates[mov.1].pop().unwrap();
            crates[mov.2].push(c);
        }
        // part 2 = all crates at once
        let l = crates2[mov.1].len();
        let src = crates2[mov.1][l - mov.0..l].to_vec();
        crates2[mov.1].truncate(l - mov.0);
        crates2[mov.2].extend(src);
    }
    result(crates); // part 1
    result(crates2); // part 2
}

fn result(crates: Vec<Vec<char>>) {
    for cr in crates {
        print!("{}", cr.last().unwrap())
    }
    println!()
}

fn parse_crate(i: &str) -> nom::IResult<&str, Option<char>> {
    nom::branch::alt((
        map(tag("   "), |_| None),
        map(delimited(tag("["), anychar, tag("]")), |c| Some(c)),
    ))(i)
}

fn parse_move(i: &str) -> nom::IResult<&str, Move> {
    map(
        nom::sequence::tuple((
            nom::sequence::preceded(tag("move "), parse_int),
            nom::sequence::preceded(tag(" from "), parse_int::<usize>),
            nom::sequence::preceded(tag(" to "), parse_int::<usize>),
        )),
        |(a, b, c)| Move(a, b - 1, c - 1),
    )(i)
}

fn parse_crates(i: &str) -> nom::IResult<&str, Vec<Option<char>>> {
    nom::multi::separated_list1(tag(" "), parse_crate)(i)
}

fn gen_crates(lines: Vec<&str>) -> Vec<Vec<char>> {
    let l = parse_crates(lines[0]).unwrap().1.len();
    let mut out = vec![vec![]; l];
    for line in lines {
        for (i, cr) in parse_crates(line).unwrap().1.iter().enumerate() {
            if let Some(c) = cr {
                out[i].push(*c);
            }
        }
    }
    out
}
