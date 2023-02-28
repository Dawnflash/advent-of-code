use std::str::FromStr;

use aoc2022::parse_int;

#[derive(Debug)]
enum Instr {
    Noop,
    Addx(i32),
}

type RunSim = Vec<i32>;

impl Instr {
    fn eval(self, sim: &mut RunSim) {
        let last = *sim.last().unwrap();
        match self {
            Instr::Noop => sim.push(last),
            Instr::Addx(i) => {
                sim.push(last);
                sim.push(last + i);
            }
        }
    }
}

impl FromStr for Instr {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match nom::branch::alt((
            nom::combinator::map(nom::bytes::complete::tag("noop"), |_| Instr::Noop),
            nom::combinator::map(
                nom::sequence::preceded(nom::bytes::complete::tag("addx "), parse_int),
                |i| Instr::Addx(i),
            ),
        ))(s)
        {
            Ok((_, instr)) => Ok(instr),
            Err(_) => Err(()),
        }
    }
}

pub fn main(input: String) {
    let mut sim: RunSim = vec![1];
    for line in input.lines() {
        Instr::from_str(line).unwrap().eval(&mut sim);
    }
    sim.truncate(sim.len() - 1); // strip the last one
    println!(
        "{}",
        [20, 60, 100, 140, 180, 220]
            .iter()
            .map(|&i| i * sim[i as usize - 1])
            .sum::<i32>()
    );
    for (i, reg) in sim.iter().enumerate() {
        //println!("{} {} {}", i, reg, num::abs(reg - (i % 40) as i32));
        if num::abs(reg - (i % 40) as i32) < 2 {
            print!("█");
        } else {
            print!("░");
        }
        if i % 40 == 39 {
            println!();
        }
    }
}
