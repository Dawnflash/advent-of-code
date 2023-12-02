use std::cmp::{max, min};

use aoc2023::*;
use bit_set::BitSet;
use itertools::Itertools;

const START: Point2D = Point2D { x: 500, y: 0 };

pub fn main(input: String) {
    let shapes = input
        .lines()
        .map(|l| parse_shape(l).expect("parse error").1)
        .collect_vec();
    let floor_y = 2 + shapes.iter().flatten().map(|&p| p.y).max().unwrap() as usize;
    let mut rocks = BitSet::with_capacity(floor_y * (floor_y * 2 - 1));
    for shape in shapes.iter() {
        for pair in shape.windows(2) {
            expand(pair[0], pair[1], &mut rocks, floor_y);
        }
    }
    let mut rocks2 = rocks.clone();
    println!("{}", sim(&mut rocks, floor_y, false, 0));
    println!("{}", sim(&mut rocks2, floor_y, true, 0) + 1);
}

fn sim(rocks: &mut BitSet, floor_y: usize, has_floor: bool, acc: usize) -> usize {
    let mut p = START;
    'main: loop {
        for dir in [Direction2D::D, Direction2D::DL, Direction2D::DR] {
            let np = p.step_2d_unchecked(dir);
            if !rocks.contains(dim_flatten(np.x as usize, np.y as usize, floor_y)) {
                p = np;
                if has_floor && p.y + 1 == floor_y as i32 {
                    break 'main; // floor reached, end
                }
                if p.y + 1 >= floor_y as i32 {
                    return acc; // fall-through
                }
                continue 'main;
            }
        }
        break;
    }
    rocks.insert(dim_flatten(p.x as usize, p.y as usize, floor_y));
    if p != START {
        sim(rocks, floor_y, has_floor, acc + 1)
    } else {
        acc
    }
}

fn dim_flatten(x: usize, y: usize, floor: usize) -> usize {
    let x_tr = x + floor - 1 - 500;
    x_tr + y * (2 * floor - 1)
}

fn expand(from: Point2D, to: Point2D, points: &mut BitSet, floor: usize) {
    for x in min(from.x, to.x)..=max(from.x, to.x) {
        for y in min(from.y, to.y)..=max(from.y, to.y) {
            points.insert(dim_flatten(x as usize, y as usize, floor));
        }
    }
}

fn parse_shape(i: &str) -> nom::IResult<&str, Vec<Point2D>> {
    nom::multi::separated_list1(
        nom::bytes::complete::tag(" -> "),
        nom::combinator::map(
            nom::sequence::separated_pair(
                parse_int,
                nom::character::complete::char(','),
                parse_int,
            ),
            |(x, y)| Point2D::new(x, y),
        ),
    )(i)
}
