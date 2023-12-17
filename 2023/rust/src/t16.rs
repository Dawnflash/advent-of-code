use aoc2023::{Direction2D as Dir, Point2D};

#[derive(Debug, Clone, Copy, PartialEq)]
struct Tile {
    pub kind: char,
    pub lasers: [bool; 4],
}
enum TileEffect {
    Single(Dir),
    Split(Dir, Dir),
}

impl Tile {
    pub fn new(kind: char) -> Self {
        Self {
            kind,
            lasers: [false; 4],
        }
    }
    pub fn is_energized(&self) -> bool {
        self.lasers.iter().any(|&l| l)
    }
    pub fn has_laser_from(&self, dir_in: Dir) -> bool {
        match self.laser_out_dirs(dir_in) {
            TileEffect::Single(out) => self.has_laser_to(out),
            TileEffect::Split(a, _) => self.has_laser_to(a),
        }
    }
    pub fn has_laser_to(&self, dir: Dir) -> bool {
        self.lasers[dir as usize / 2]
    }
    pub fn set_laser_to(&mut self, dir: Dir) {
        self.lasers[dir as usize / 2] = true;
    }
    pub fn laser_out_dirs(&self, dir_in: Dir) -> TileEffect {
        match self.kind {
            '.' => TileEffect::Single(dir_in),
            '\\' => TileEffect::Single(match dir_in {
                Dir::U => Dir::L,
                Dir::R => Dir::D,
                Dir::D => Dir::R,
                Dir::L => Dir::U,
                _ => panic!("Invalid direction: {:?}", dir_in),
            }),
            '/' => TileEffect::Single(match dir_in {
                Dir::U => Dir::R,
                Dir::R => Dir::U,
                Dir::D => Dir::L,
                Dir::L => Dir::D,
                _ => panic!("Invalid direction: {:?}", dir_in),
            }),
            '|' => match dir_in {
                Dir::R | Dir::L => TileEffect::Split(Dir::U, Dir::D),
                Dir::U | Dir::D => TileEffect::Single(dir_in),
                _ => panic!("Invalid direction: {:?}", dir_in),
            },
            '-' => match dir_in {
                Dir::U | Dir::D => TileEffect::Split(Dir::L, Dir::R),
                Dir::R | Dir::L => TileEffect::Single(dir_in),
                _ => panic!("Invalid direction: {:?}", dir_in),
            },
            _ => panic!("Invalid tile kind: {}", self.kind),
        }
    }
}

pub fn main(input: String) {
    let map: Vec<Vec<Tile>> = input
        .lines()
        .map(|l| l.chars().map(Tile::new).collect())
        .collect();
    let bound = (
        Point2D::origin(),
        Point2D::new(map[0].len() as i32, map.len() as i32),
    );
    let p1 = run_from(&map, bound, (Point2D::new(-1, 0), Dir::R));
    println!("Part 1: {}", p1);
    let r = (0..bound.1.y).map(|y| run_from(&map, bound, (Point2D::new(-1, y), Dir::R)));
    let l = (0..bound.1.y).map(|y| run_from(&map, bound, (Point2D::new(bound.1.x, y), Dir::L)));
    let d = (0..bound.1.x).map(|x| run_from(&map, bound, (Point2D::new(x, -1), Dir::D)));
    let u = (0..bound.1.x).map(|x| run_from(&map, bound, (Point2D::new(x, bound.1.y), Dir::U)));
    let p2 = r.chain(l).chain(d).chain(u).max().unwrap();
    println!("Part 2: {}", p2);
}

fn run_from(map: &Vec<Vec<Tile>>, bound: (Point2D, Point2D), from: (Point2D, Dir)) -> usize {
    let mut map = map.clone();
    run(&mut map, bound, from);
    map.iter().flatten().filter(|t| t.is_energized()).count()
}

fn run(map: &mut Vec<Vec<Tile>>, bound: (Point2D, Point2D), head: (Point2D, Dir)) {
    let mut heads = vec![head];
    while !heads.is_empty() {
        let (p, dir) = heads.pop().unwrap();
        if let Some(np) = p.step_2d(bound.0, bound.1, dir) {
            let tile = &mut map[np.y as usize][np.x as usize];
            if tile.has_laser_from(dir) {
                continue;
            }
            match tile.laser_out_dirs(dir) {
                TileEffect::Single(dir_out) => {
                    tile.set_laser_to(dir_out);
                    heads.push((np, dir_out));
                }
                TileEffect::Split(dir_out_a, dir_out_b) => {
                    tile.set_laser_to(dir_out_a);
                    tile.set_laser_to(dir_out_b);
                    heads.push((np, dir_out_a));
                    heads.push((np, dir_out_b));
                }
            }
        }
    }
}
