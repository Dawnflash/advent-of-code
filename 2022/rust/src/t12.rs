use std::collections::HashSet;

use aoc2022::*;
use itertools::Itertools;

struct Map {
    start: Point2D,
    end: Point2D,
    dims: (usize, usize),
    data: Vec<Vec<u8>>,
}

fn elevation(c: u8) -> u8 {
    match c {
        b'S' => elevation(b'a'),
        b'E' => elevation(b'z'),
        _ => c - b'a',
    }
}

fn distances(map: &Map) -> Vec<Vec<usize>> {
    let mut dists = vec![vec![usize::MAX; map.dims.0]; map.dims.1];
    let mut cur = HashSet::new();
    let mut buf = cur.clone();
    let mut cur_dist = 0;
    let bound = Point2D::new(map.dims.0 as i32, map.dims.1 as i32);

    cur.insert(map.end); // starting point
    while !cur.is_empty() {
        for p in cur.drain() {
            index_2d!(dists, p) = cur_dist;
            buf.extend(p.adjacent_cross().iter().filter(|&&n| {
                if !n.is_bounded(Point2D::origin(), bound) {
                    return false;
                }
                let ph = index_2d!(map.data, p);
                let nh = index_2d!(map.data, n);
                index_2d!(dists, n) == usize::MAX && (ph < nh || ph - nh <= 1)
            }));
        }
        cur.extend(buf.drain());
        cur_dist += 1;
    }
    dists
}

pub fn main(input: String) {
    let lines = input.lines().collect_vec();
    let mut start = Point2D::origin();
    let mut end = Point2D::origin();
    let dims = (lines[0].len(), lines.len());
    let mut data = vec![vec![]; dims.1];
    for (y, line) in input.lines().enumerate() {
        if let Some(x) = line.find('S') {
            start = Point2D::new(x as i32, y as i32);
        }
        if let Some(x) = line.find('E') {
            end = Point2D::new(x as i32, y as i32);
        }
        data[y].extend(line.bytes().map(elevation));
    }
    let map = Map {
        start: start,
        end: end,
        dims: dims,
        data: data,
    };
    let dists = distances(&map);

    println!("{}", index_2d!(dists, map.start)); // part 1

    let closest_start = map
        .data
        .iter()
        .flatten()
        .zip(dists.iter().flatten())
        .filter_map(|(&h, &d)| if h == 0 { Some(d) } else { None })
        .min()
        .unwrap();

    println!("{}", closest_start); // part 2
}
