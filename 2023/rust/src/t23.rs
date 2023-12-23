use std::collections::{HashMap, HashSet, VecDeque};

use aoc2023::{Direction2D as Dir, Point2D};
use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Tile {
    Empty,
    Wall,
    Slope(Dir),
}

#[derive(Debug)]
struct Graph {
    nodes: Vec<Node>,
}
#[derive(Debug)]
struct Node {
    edges: Vec<Edge>,
}
#[derive(Debug)]
struct Edge {
    target: usize,
    weight: usize,
}

impl Tile {
    pub fn from_char(c: char) -> Self {
        match c {
            '.' => Self::Empty,
            '#' => Self::Wall,
            '>' => Self::Slope(Dir::R),
            '<' => Self::Slope(Dir::L),
            '^' => Self::Slope(Dir::U),
            'v' => Self::Slope(Dir::D),
            _ => panic!("invalid tile"),
        }
    }
    pub fn can_enter(&self, dir_in: Dir) -> bool {
        match self {
            Self::Empty => true,
            Self::Wall => false,
            Self::Slope(d) => *d != dir_in.invert(),
        }
    }
}

pub fn main(input: String) {
    let map: Vec<Vec<Tile>> = input
        .lines()
        .map(|line| line.chars().map(|c| Tile::from_char(c)).collect())
        .collect();
    let bounds = (
        Point2D::origin(),
        Point2D::new(map[0].len() as i32, map.len() as i32),
    );
    let start = Point2D::new(1, 0);
    let end = Point2D::new(bounds.1.x - 2, bounds.1.y - 1);
    let graph = compress(&map, bounds, start, end);
    println!("Part 1: {}", path(&graph, HashSet::new(), 0).unwrap());
    let p2_map: Vec<Vec<Tile>> = map
        .iter()
        .map(|row| {
            row.iter()
                .map(|tile| match tile {
                    Tile::Slope(_) => Tile::Empty,
                    _ => *tile,
                })
                .collect()
        })
        .collect();
    let p2_graph = compress(&p2_map, bounds, start, end);
    println!("Part 2: {}", path(&p2_graph, HashSet::new(), 0).unwrap());
}

fn compress(
    map: &Vec<Vec<Tile>>,
    bounds: (Point2D, Point2D),
    start: Point2D,
    end: Point2D,
) -> Graph {
    let mut graph = Graph { nodes: vec![] };
    let mut nodes: HashMap<Point2D, usize> = HashMap::new(); // loc-node lookup
    map.iter().enumerate().for_each(|(y, row)| {
        row.iter()
            .enumerate()
            .filter(|(_, t)| **t == Tile::Empty)
            .for_each(|(x, _)| {
                let cur = Point2D::new(x as i32, y as i32);
                let is_intersection = [Dir::U, Dir::D, Dir::L, Dir::R]
                    .into_iter()
                    .filter(|&dir| {
                        let p = cur.step_2d_unchecked(dir);
                        if !p.is_bounded(bounds.0, bounds.1) {
                            return false;
                        }
                        let accessible = match map[p.y as usize][p.x as usize] {
                            Tile::Empty => true,
                            Tile::Slope(d) => d == dir.invert(),
                            _ => false,
                        };
                        if !accessible {
                            return false;
                        }
                        [dir.invert(), dir.rotate(Dir::L), dir.rotate(Dir::R)]
                            .into_iter()
                            .filter(|&dir| {
                                let p = cur.step_2d_unchecked(dir);
                                p.is_bounded(bounds.0, bounds.1)
                                    && map[p.y as usize][p.x as usize].can_enter(dir)
                            })
                            .count()
                            > 1
                    })
                    .count()
                    > 0;
                if cur == start || cur == end || is_intersection {
                    // node
                    let node_id = graph.nodes.len();
                    graph.nodes.push(Node { edges: Vec::new() });
                    nodes.insert(cur, node_id);
                }
            });
    });
    for (k, v) in nodes.iter().sorted_by(|(_, n1), (_, n2)| n1.cmp(n2)) {
        let mut queue: VecDeque<(Point2D, Dir, usize)> = VecDeque::new();
        [Dir::U, Dir::D, Dir::L, Dir::R]
            .into_iter()
            .filter(|&dir| {
                let p = k.step_2d_unchecked(dir);
                p.is_bounded(bounds.0, bounds.1) && map[p.y as usize][p.x as usize].can_enter(dir)
            })
            .for_each(|dir| queue.push_back((k.step_2d_unchecked(dir), dir, 1)));
        while let Some((cur, dir_in, dist)) = queue.pop_front() {
            if let Some(&node_id) = nodes.get(&cur) {
                graph.nodes[*v].edges.push(Edge {
                    target: node_id,
                    weight: dist,
                });
                continue;
            }
            [dir_in, dir_in.rotate(Dir::L), dir_in.rotate(Dir::R)]
                .into_iter()
                .filter(|&dir| {
                    let p = cur.step_2d_unchecked(dir);
                    p.is_bounded(bounds.0, bounds.1)
                        && map[p.y as usize][p.x as usize].can_enter(dir)
                })
                .for_each(|dir| {
                    queue.push_back((cur.step_2d_unchecked(dir), dir, dist + 1));
                });
        }
    }
    graph
}

fn path(graph: &Graph, mut visited: HashSet<usize>, cur: usize) -> Option<usize> {
    let end = graph.nodes.len() - 1;
    if end == cur {
        return Some(0);
    }
    visited.insert(cur);
    let cur = &graph.nodes[cur];
    cur.edges
        .iter()
        .filter(|edge| !visited.contains(&edge.target))
        .map(|edge| path(graph, visited.clone(), edge.target).map(|dist| dist + edge.weight))
        .flatten()
        .max()
}
