use std::collections::HashSet;

use aoc2023::{Direction2D as Dir, Point2D};

type Map = Vec<Vec<char>>;

pub fn main(input: String) {
    let mut start = Point2D::origin();
    let map: Map = input
        .lines()
        .enumerate()
        .map(|(y, l)| {
            l.chars()
                .enumerate()
                .map(|(x, c)| {
                    if c == 'S' {
                        start = Point2D::new(x as i32, y as i32);
                        '.'
                    } else {
                        c
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let bounds = (
        Point2D::origin(),
        Point2D::new(map[0].len() as i32, map.len() as i32),
    );
    println!("Part 1: {}", reachable_in(&map, bounds.1, start, 64));
    // polynomial analysis
    // for i in 0..100 {
    //     let n = (bounds.1.x - 1) / 2 + i * bounds.1.x;
    //     println!("{}: {}", n, reachable_in(&map, bounds.1, start, n as usize));
    // }
    println!("Part 2: {}", part2(26501365));
}

fn reachable_in(map: &Map, size: Point2D, start: Point2D, steps: usize) -> usize {
    let mut queue = HashSet::from([start]);
    let mut queue2 = HashSet::new();
    for _ in 0..steps {
        for p in queue.drain() {
            for d in [Dir::U, Dir::D, Dir::L, Dir::R] {
                let next = p.step_2d_unchecked(d);
                if map[(next.y.rem_euclid(size.y)) as usize][(next.x.rem_euclid(size.x)) as usize]
                    == '.'
                {
                    queue2.insert(next);
                }
            }
        }
        std::mem::swap(&mut queue, &mut queue2);
    }
    queue.len()
}

// thanks wolfram alpha
fn part2(steps: usize) -> usize {
    ((15615 * steps * steps) + (27143 * steps) - 106169) / 17161
}
