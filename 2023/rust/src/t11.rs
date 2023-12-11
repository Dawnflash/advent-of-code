use aoc2023::Point2D;
use itertools::Itertools;

pub fn main(input: String) {
    let size = (input.lines().next().unwrap().len(), input.lines().count());
    let galaxy: Vec<Point2D> = input
        .lines()
        .enumerate()
        .map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(move |(x, c)| match c {
                    '#' => Some(Point2D::new(x as i32, y as i32)),
                    _ => None,
                })
                .flatten()
        })
        .flatten()
        .collect();

    let mut galaxy_1 = galaxy.clone();
    expand(&mut galaxy_1, size, 1);
    let p1 = galaxy_1
        .iter()
        .combinations(2)
        .map(|pair| pair[0].manhattan_distance(*pair[1]))
        .sum::<i32>();
    println!("Part 1: {}", p1);
    let mut galaxy_2 = galaxy.clone();
    expand(&mut galaxy_2, size, 999999);
    let p2: i64 = galaxy_2
        .iter()
        .combinations(2)
        .map(|pair| pair[0].manhattan_distance(*pair[1]) as i64)
        .sum::<i64>();
    println!("Part 2: {}", p2);
}

fn expand(galaxy: &mut Vec<Point2D>, size: (usize, usize), magnitude: i32) {
    let galaxy_orig = galaxy.clone();
    (0..size.1 as i32)
        .filter(|y| galaxy_orig.iter().all(|p| p.y != *y))
        .for_each(|y| {
            galaxy_orig
                .iter()
                .enumerate()
                .filter(|(_, p)| p.y > y)
                .for_each(|(n, _)| galaxy[n].y += magnitude)
        });
    (0..size.0 as i32)
        .filter(|x| galaxy_orig.iter().all(|p| p.x != *x))
        .for_each(|x| {
            galaxy_orig
                .iter()
                .enumerate()
                .filter(|(_, p)| p.x > x)
                .for_each(|(n, _)| galaxy[n].x += magnitude)
        });
}
