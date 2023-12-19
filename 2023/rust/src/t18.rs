use std::collections::BTreeSet;

use aoc2023::{Direction2D as Dir, Point2D};
use itertools::Itertools;

#[derive(Debug, Clone, Copy)]
enum Bound {
    Single(i32),
    Range(i32, i32, bool),
}

impl PartialEq for Bound {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Single(a), Self::Single(b)) => a == b,
            (Self::Single(a), Self::Range(b, _, _)) => a == b,
            (Self::Range(a, _, _), Self::Single(b)) => a == b,
            (Self::Range(a, _, _), Self::Range(b, _, _)) => a == b,
        }
    }
}

impl Ord for Bound {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Single(a), Self::Single(b)) => a.cmp(b),
            (Self::Single(a), Self::Range(b, _, _)) => a.cmp(b),
            (Self::Range(a, _, _), Self::Single(b)) => a.cmp(b),
            (Self::Range(a, _, _), Self::Range(b, _, _)) => a.cmp(b),
        }
    }
}

impl Eq for Bound {}

impl PartialOrd for Bound {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub fn main(input: String) {
    println!("Part 1: {}", stage(input.clone(), false));
    println!("Part 2: {}", stage(input, true));
}

fn stage(input: String, p2: bool) -> usize {
    let (instrs, y_bound, boundary_size) = get_instrs(input, p2);
    let mut col_bounds: Vec<BTreeSet<Bound>> =
        vec![BTreeSet::new(); (y_bound.1 - y_bound.0 + 1) as usize];
    let mut cur = Point2D::origin();
    // get column bounds
    for i in 0..instrs.len() {
        let (d, n) = instrs[i];
        let prev_i = if i == 0 { instrs.len() - 1 } else { i - 1 };
        let next_i = (i + 1) % instrs.len();
        let (prev_d, next_d) = (instrs[prev_i].0, instrs[next_i].0);
        let is_extreme = prev_d == next_d.invert();
        let next = cur.move_2d_unchecked(d, n as i32);
        if d == Dir::U || d == Dir::D {
            for y in cur.y.min(next.y) + 1..cur.y.max(next.y) {
                col_bounds[(y - y_bound.0) as usize].insert(Bound::Single(cur.x));
            }
        } else {
            col_bounds[(cur.y - y_bound.0) as usize].insert(Bound::Range(
                cur.x.min(next.x),
                cur.x.max(next.x),
                is_extreme,
            ));
        }
        cur = next;
    }
    let mut size = boundary_size;
    for bounds in col_bounds.iter() {
        let mut first_x = 0;
        let mut inside = false;
        for bound in bounds.iter() {
            match bound {
                Bound::Single(x) => {
                    if inside {
                        size += (x - first_x) as usize;
                    } else {
                        first_x = *x + 1;
                    }
                    inside = !inside;
                }
                Bound::Range(beg, end, extreme) => {
                    if *extreme {
                        if inside {
                            size += (beg - first_x) as usize;
                            first_x = *end + 1;
                        }
                    } else {
                        if inside {
                            size += (beg - first_x) as usize;
                        } else {
                            first_x = *end + 1;
                        }
                        inside = !inside;
                    }
                }
            };
        }
    }
    size
}

fn get_instrs(input: String, p2: bool) -> (Vec<(Dir, usize)>, (i32, i32), usize) {
    let mut cur = Point2D::origin();
    let mut boundary_size = 0;
    let mut y_bound = (0, 0);
    let instrs = input
        .lines()
        .map(|line| {
            let (d, n, color) = line.split(" ").collect_tuple().unwrap();
            let (d, n) = if p2 {
                (
                    match &color[7..8] {
                        "0" => Dir::R,
                        "1" => Dir::D,
                        "2" => Dir::L,
                        "3" => Dir::U,
                        _ => panic!("invalid direction"),
                    },
                    usize::from_str_radix(&color[2..7], 16).unwrap(),
                )
            } else {
                (d.parse().unwrap(), n.parse().unwrap())
            };
            boundary_size += n;
            cur = cur.move_2d_unchecked(d, n as i32); // 20bit numbers
            y_bound.0 = y_bound.0.min(cur.y);
            y_bound.1 = y_bound.1.max(cur.y);
            (d, n)
        })
        .collect();
    (instrs, y_bound, boundary_size)
}
