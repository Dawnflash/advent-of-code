use aoc2023::parse_int;
use itertools::Itertools;

#[derive(PartialEq, Eq, Debug, Clone)]
enum Packet {
    PInt(i32),
    PList(Vec<Packet>),
}

fn parse_packet(s: &str) -> nom::IResult<&str, Packet> {
    nom::branch::alt((
        nom::combinator::map(
            nom::sequence::delimited(
                nom::character::complete::char('['),
                nom::multi::separated_list0(nom::character::complete::char(','), parse_packet),
                nom::character::complete::char(']'),
            ),
            |v| Packet::PList(v),
        ),
        nom::combinator::map(parse_int, |i| Packet::PInt(i)),
    ))(s)
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Packet::PInt(s), Packet::PInt(o)) => s.cmp(o),
            (Packet::PList(s), Packet::PList(o)) => s.cmp(o),
            (s, Packet::PInt(o)) => {
                let op = Packet::PList(vec![Packet::PInt(*o)]);
                s.cmp(&op)
            }
            (Packet::PInt(s), o) => {
                let sp = Packet::PList(vec![Packet::PInt(*s)]);
                sp.cmp(o)
            }
        }
    }
}

pub fn main(input: String) {
    let p1: usize = input
        .split("\n\n")
        .enumerate()
        .map(|(i, chunk)| {
            let (l, r) = chunk
                .lines()
                .map(|s| parse_packet(s).expect("parse error").1)
                .collect_tuple::<(Packet, Packet)>()
                .unwrap();
            if l < r {
                i + 1
            } else {
                0
            }
        })
        .sum();
    println!("{p1}");

    let p2 = Packet::PList(vec![Packet::PList(vec![Packet::PInt(2)])]);
    let p6 = Packet::PList(vec![Packet::PList(vec![Packet::PInt(6)])]);
    let mut p2_packets = vec![p2.clone(), p6.clone()];
    p2_packets.extend(
        input
            .lines()
            .filter(|s| s.len() > 0)
            .map(|s| parse_packet(s).expect("parse error").1),
    );
    p2_packets.sort();
    let p2_ix = p2_packets.iter().position(|e| e == &p2).unwrap() + 1;
    let p6_ix = p2_packets.iter().position(|e| e == &p6).unwrap() + 1;
    println!("{}", p2_ix * p6_ix);
}
