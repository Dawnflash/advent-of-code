use std::{collections::HashSet, str::FromStr};

use aoc2022::*;

type Move = (Direction2D, usize);
type Snake = Vec<Point2D>;
type PointSet = HashSet<Point2D>;

pub fn main(input: String) {
    let moves = input.lines().map(|l| parse_move(l).unwrap().1);
    let mut snake1: Snake = vec![Point2D::origin(); 2];
    let mut snake2: Snake = vec![Point2D::origin(); 10];
    let mut visited1: PointSet = HashSet::new();
    visited1.insert(Point2D::origin());
    let mut visited2 = visited1.clone();
    for (dir, n) in moves {
        for _ in 0..n {
            make_move(&mut snake1, dir, &mut visited1);
            make_move(&mut snake2, dir, &mut visited2);
        }
    }
    println!("{}\n{}", visited1.len(), visited2.len());
}

fn make_move(snake: &mut Snake, dir: Direction2D, visited: &mut PointSet) {
    let mut old_head = snake[0];
    snake[0] = old_head.step_2d_unchecked(dir);
    for i in 1..snake.len() {
        let p = follow(snake[i - 1], snake[i], old_head);
        old_head = snake[i];
        snake[i] = p;
        if i + 1 == snake.len() {
            visited.insert(p);
        }
    }
}

fn follow(head: Point2D, tail: Point2D, old_head: Point2D) -> Point2D {
    let dh = head - old_head;
    let dt = head - tail;
    if head.is_neighbor(tail) {
        tail // stay
    } else if dh.x == 0 || dh.y == 0 {
        old_head // go where the head was
    } else if dt.x == 0 || dt.y == 0 {
        tail + Point2D::new(dt.x / 2, dt.y / 2) // v-move, one diff coord is 2, another 0
    } else {
        tail + dh // copy head's move
    }
}

fn parse_move(s: &str) -> nom::IResult<&str, Move> {
    nom::sequence::separated_pair(
        nom::combinator::map_res(nom::character::complete::anychar, |s| {
            Direction2D::from_str(s.to_string().as_str())
        }),
        nom::character::complete::char(' '),
        parse_int,
    )(s)
}
