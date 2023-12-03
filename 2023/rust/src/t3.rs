use std::{cmp::min, collections::HashMap};

use itertools::Itertools;
use regex::Regex;

pub fn main(input: String) {
    let nre = Regex::new(r"\d+").unwrap();
    let sre = Regex::new(r"[^\d.]").unwrap();
    let mut gearmap: HashMap<usize, Vec<u64>> = HashMap::new();
    let lines = input.lines().collect_vec();
    let llen = lines[0].len();
    let sum: u64 = lines
        .iter()
        .enumerate()
        .map(|(nline, line)| {
            let loffset = nline * llen;
            nre.find_iter(line)
                .map(|m| {
                    let n: u64 = m.as_str().parse().unwrap();
                    let prev = if m.start() > 0 { m.start() - 1 } else { 0 };
                    let next = min(llen, m.end() + 1);
                    let mut found = false;
                    // check surrounding characters
                    if m.start() > 0 {
                        found |= process_adjacent(
                            &mut gearmap,
                            &sre,
                            &line[prev..m.start()],
                            loffset + prev,
                            n,
                        );
                    }
                    if m.end() < llen {
                        found |= process_adjacent(
                            &mut gearmap,
                            &sre,
                            &line[m.end()..next],
                            loffset + m.end(),
                            n,
                        );
                    }
                    // check adjacent lines
                    if nline > 0 {
                        found |= process_adjacent(
                            &mut gearmap,
                            &sre,
                            &lines[nline - 1][prev..next],
                            loffset - llen + prev,
                            n,
                        );
                    }
                    if nline < lines.len() - 1 {
                        found |= process_adjacent(
                            &mut gearmap,
                            &sre,
                            &lines[nline + 1][prev..next],
                            loffset + llen + prev,
                            n,
                        );
                    }
                    n * found as u64
                })
                .sum::<u64>()
        })
        .sum();
    println!("Part 1: {}", sum);
    println!(
        "Part 2: {}",
        gearmap
            .values()
            .map(|v| if v.len() == 2 { v.iter().product() } else { 0 })
            .sum::<u64>()
    );
}

fn process_adjacent(
    gearmap: &mut HashMap<usize, Vec<u64>>,
    regex: &Regex,
    data: &str,
    offset: usize,
    n: u64,
) -> bool {
    if !regex.is_match(data) {
        return false;
    }
    data.chars().enumerate().for_each(|(pos, c)| {
        if c == '*' {
            gearmap.entry(offset + pos).or_insert(Vec::new()).push(n);
        }
    });
    return true;
}
