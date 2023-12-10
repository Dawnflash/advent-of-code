use std::iter;

use aoc2023::{Direction2D as Dir, Point2D};
use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Pipe {
    UD,
    LR,
    UL,
    UR,
    DL,
    DR,
    GROUND,
    START,
}
#[derive(Debug, Clone, Copy, PartialEq)]
enum Tile {
    Loop(Pipe),
    Inside,
    Outside,
    Unknown(Pipe),
}
type Map = Vec<Vec<Tile>>;
impl Tile {
    fn unwrap(self) -> Pipe {
        match self {
            Self::Loop(p) => p,
            Self::Inside => Pipe::GROUND,
            Self::Outside => Pipe::GROUND,
            Self::Unknown(p) => p,
        }
    }
    fn draw(self) -> char {
        match self {
            Self::Loop(Pipe::UD) => '║',
            Self::Loop(Pipe::LR) => '═',
            Self::Loop(Pipe::UL) => '╝',
            Self::Loop(Pipe::UR) => '╚',
            Self::Loop(Pipe::DL) => '╗',
            Self::Loop(Pipe::DR) => '╔',
            Self::Inside => 'I',
            Self::Outside => 'O',
            Self::Unknown(_) => '?',
            _ => '.',
        }
    }
}
impl Pipe {
    fn from_char(s: char) -> Self {
        match s {
            '|' => Self::UD,
            '-' => Self::LR,
            'L' => Self::UR,
            'J' => Self::UL,
            '7' => Self::DL,
            'F' => Self::DR,
            'S' => Self::START,
            _ => Self::GROUND,
        }
    }

    fn from_dirs(dirs: (Dir, Dir)) -> Self {
        match dirs {
            (Dir::U, Dir::D) => Self::UD,
            (Dir::L, Dir::R) => Self::LR,
            (Dir::U, Dir::L) => Self::UL,
            (Dir::U, Dir::R) => Self::UR,
            (Dir::D, Dir::L) => Self::DL,
            (Dir::D, Dir::R) => Self::DR,
            (a, b) => {
                if a == b {
                    panic!("Invalid pipe");
                } else {
                    Self::from_dirs((b, a))
                }
            }
        }
    }

    fn goes_dir(self, d: Dir) -> bool {
        match (self, d) {
            (Pipe::UD, Dir::U) => true,
            (Pipe::UD, Dir::D) => true,
            (Pipe::LR, Dir::L) => true,
            (Pipe::LR, Dir::R) => true,
            (Pipe::UL, Dir::U) => true,
            (Pipe::UL, Dir::L) => true,
            (Pipe::UR, Dir::U) => true,
            (Pipe::UR, Dir::R) => true,
            (Pipe::DL, Dir::D) => true,
            (Pipe::DL, Dir::L) => true,
            (Pipe::DR, Dir::D) => true,
            (Pipe::DR, Dir::R) => true,
            _ => false,
        }
    }

    fn dir_out(self, dir_in: Dir) -> Dir {
        match (self, dir_in.invert()) {
            (Pipe::UD, Dir::U) => Dir::D,
            (Pipe::UD, Dir::D) => Dir::U,
            (Pipe::LR, Dir::L) => Dir::R,
            (Pipe::LR, Dir::R) => Dir::L,
            (Pipe::UL, Dir::U) => Dir::L,
            (Pipe::UL, Dir::L) => Dir::U,
            (Pipe::UR, Dir::U) => Dir::R,
            (Pipe::UR, Dir::R) => Dir::U,
            (Pipe::DL, Dir::D) => Dir::L,
            (Pipe::DL, Dir::L) => Dir::D,
            (Pipe::DR, Dir::D) => Dir::R,
            (Pipe::DR, Dir::R) => Dir::D,
            _ => panic!("Invalid pipe"),
        }
    }
}

fn init_dirs(map: &Map, size: (usize, usize), pos: Point2D) -> (Dir, Dir) {
    let (bound_a, bound_b) = (
        Point2D::origin(),
        Point2D::new(size.0 as i32, size.1 as i32),
    );
    let (up, down, left, right) = [Dir::U, Dir::D, Dir::L, Dir::R]
        .iter()
        .map(|&dir| {
            pos.step_2d(bound_a, bound_b, dir)
                .map_or(Pipe::GROUND, |p| at(map, p).unwrap())
        })
        .collect_tuple()
        .unwrap();
    if up.goes_dir(Dir::D) && down.goes_dir(Dir::U) {
        (Dir::U, Dir::D)
    } else if left.goes_dir(Dir::R) && right.goes_dir(Dir::L) {
        (Dir::L, Dir::R)
    } else if up.goes_dir(Dir::D) && left.goes_dir(Dir::R) {
        (Dir::U, Dir::L)
    } else if up.goes_dir(Dir::D) && right.goes_dir(Dir::L) {
        (Dir::U, Dir::R)
    } else if down.goes_dir(Dir::U) && left.goes_dir(Dir::R) {
        (Dir::D, Dir::L)
    } else if down.goes_dir(Dir::U) && right.goes_dir(Dir::L) {
        (Dir::D, Dir::R)
    } else {
        panic!("Invalid starting pipe");
    }
}

fn at(map: &Map, p: Point2D) -> &Tile {
    &map[p.y as usize][p.x as usize]
}

fn update(map: &mut Map, p: Point2D, tile: Tile) {
    map[p.y as usize][p.x as usize] = tile;
}

fn draw(map: &Map) {
    for line in map {
        for tile in line {
            print!("{}", tile.draw());
        }
        println!();
    }
}

// returns the half-circumference of the loop (farthest point from start)
fn find_loop(
    map: &mut Map,
    (next_a, dir_a): (Point2D, Dir),
    (next_b, dir_b): (Point2D, Dir),
    dist: usize,
) -> usize {
    let (next_a_p, next_b_p) = (at(map, next_a).unwrap(), at(map, next_b).unwrap());
    // mark the main loop
    update(map, next_a, Tile::Loop(next_a_p));
    update(map, next_b, Tile::Loop(next_b_p));
    if next_a == next_b {
        return dist + 1;
    }
    let (ndir_a, ndir_b) = (next_a_p.dir_out(dir_a), next_b_p.dir_out(dir_b));
    find_loop(
        map,
        (next_a.step_2d_unchecked(ndir_a), ndir_a),
        (next_b.step_2d_unchecked(ndir_b), ndir_b),
        dist + 1,
    )
}

// returns a loop point with a direction pointing to the outside
fn flood_outer(map: &mut Map, size: (usize, usize), p: Point2D) -> Option<(Point2D, Dir)> {
    let Tile::Unknown(_) = at(map, p) else { return None };
    let (bound_a, bound_b) = (
        Point2D::origin(),
        Point2D::new(size.0 as i32, size.1 as i32),
    );

    update(map, p, Tile::Outside);
    let mut res: Option<(Point2D, Dir)> = None;
    for dir in [Dir::U, Dir::D, Dir::L, Dir::R].iter() {
        match p.step_2d(bound_a, bound_b, *dir) {
            Some(next) => match at(map, next) {
                Tile::Unknown(_) => {
                    res = res.or(flood_outer(map, size, next));
                }
                Tile::Loop(_) => {
                    res = res.or(Some((next, dir.invert())));
                }
                _ => {}
            },
            None => {}
        }
    }
    res
}

// get the in direction for outer loop iteration given the outer hint
fn outer_hint_init_dir(map: &Map, p: Point2D, dir_outer: Dir) -> Dir {
    let best = dir_outer.rotate(-1);
    let pipe = at(map, p).unwrap();
    if pipe.goes_dir(best) {
        best.invert()
    } else {
        best.rotate(-1).invert()
    }
}

fn loop_outer_detect_clockwise(
    map: &mut Map,
    size: (usize, usize),
    start: Point2D,
    cur: Point2D,
    dir_in: Dir,
) {
    let pipe = at(map, cur).unwrap();
    let dir_out = pipe.dir_out(dir_in);
    let next = cur.step_2d_unchecked(dir_out);
    let (bound_a, bound_b) = (
        Point2D::origin(),
        Point2D::new(size.0 as i32, size.1 as i32),
    );
    // outside is on the left, mark adjacent unknown left tiles as outside
    [dir_out.rotate(-1), dir_in.rotate(-1)]
        .iter()
        .for_each(|&dir| {
            cur.step_2d(bound_a, bound_b, dir).map(|p| {
                flood_outer(map, size, p);
            });
        });
    if start == next {
        return; // end of loop
    }
    loop_outer_detect_clockwise(map, size, start, next, dir_out);
}

pub fn main(input: String) {
    let mut start = Point2D::origin();
    let mut map: Map = input
        .lines()
        .enumerate()
        .map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(|(x, c)| {
                    let p = Pipe::from_char(c);
                    match p {
                        Pipe::START => {
                            start = Point2D::new(x as i32, y as i32);
                            Tile::Unknown(p)
                        }
                        _ => Tile::Unknown(p),
                    }
                })
                .collect()
        })
        .collect();
    let size = (map[0].len(), map.len());
    let (start_a, start_b) = init_dirs(&map, size, start);
    update(
        &mut map,
        start,
        Tile::Loop(Pipe::from_dirs((start_a, start_b))),
    );
    println!("start: {:?}", start);
    println!("start dirs: {:?}", (start_a, start_b));
    let p1 = find_loop(
        &mut map,
        (start.step_2d_unchecked(start_a), start_a),
        (start.step_2d_unchecked(start_b), start_b),
        0,
    );
    // flood from the map boundary
    let mut outer_hint: Option<(Point2D, Dir)> = None;
    for y in 0..size.1 {
        for x in 0..size.0 {
            if y != 0 && y != size.1 - 1 && x != 0 && x != size.0 - 1 {
                continue; // skip inner
            }
            outer_hint = outer_hint.or(flood_outer(
                &mut map,
                size,
                Point2D::new(x as i32, y as i32),
            ));
        }
    }
    let (hint_p, hint_dir) = outer_hint.unwrap();
    let dir_in = outer_hint_init_dir(&map, hint_p, hint_dir);
    println!("outer hint: {:?}/{:?}/{:?}", hint_p, hint_dir, dir_in);
    loop_outer_detect_clockwise(&mut map, size, hint_p, hint_p, dir_in);
    draw(&map);
    println!("Part 1: {}", p1);
    let p2 = map
        .iter()
        .flatten()
        .filter(|&&t| {
            if let Tile::Unknown(_) = t {
                true
            } else {
                false
            }
        })
        .count();
    println!("Part 2: {}", p2);
}
