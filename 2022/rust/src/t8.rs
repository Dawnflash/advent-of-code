use aoc2022::*;
use itertools::Itertools;

struct Forest {
    data: Vec<Vec<u8>>,
    dims: Point2D,
}

impl Forest {
    fn new() -> Self {
        Self {
            data: vec![],
            dims: Point2D::new(0, 0),
        }
    }
}

pub fn main(input: String) {
    let mut forest: Forest = Forest::new();
    for line in input.lines() {
        forest
            .data
            .push(line.bytes().map(|b| b - b'0').collect_vec());
    }
    forest.dims.x = forest.data[0].len().try_into().unwrap();
    forest.dims.y = forest.data.len().try_into().unwrap();
    let results = (0..forest.dims.x)
        .cartesian_product(0..forest.dims.y)
        .map(|(x, y)| inspect(&forest, Point2D::new(x, y)))
        .collect_vec();
    println!("{}", results.iter().filter(|&b| b.0).count());
    println!("{}", results.iter().map(|&r| r.1).max().unwrap());
}

fn inspect(forest: &Forest, p: Point2D) -> (bool, usize) {
    let dirs = [
        Direction2D::U,
        Direction2D::R,
        Direction2D::D,
        Direction2D::L,
    ];
    let zero = Point2D::origin();
    let ht: u8 = index_2d!(forest.data, p);
    let mut vis = false;
    let mut score = 1;
    for dir in dirs {
        let mut dvis = true;
        let mut dscore = 0;
        let mut np = p;
        while let Some(nnp) = np.step_2d(zero, forest.dims, dir) {
            np = nnp;
            dscore += 1;
            if index_2d!(forest.data, np) >= ht {
                dvis = false;
                break;
            }
        }
        score *= dscore;
        vis |= dvis;
    }
    (vis, score)
}
