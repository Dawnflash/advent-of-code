use itertools::Itertools;

type Coords = (usize, usize, usize);
type BlockMap = Vec<Vec<Vec<Option<Block>>>>;
#[derive(Debug, Clone)]
struct Block {
    coords: Coords,
    brick: usize,
}
#[derive(Debug, Clone)]
struct Brick {
    blocks: Vec<Block>,
}

impl Block {
    pub fn is_supported(&self, map: &BlockMap) -> bool {
        self.coords.2 == 1
            || map[self.coords.0][self.coords.1][self.coords.2 - 1]
                .as_ref()
                .map_or(false, |b| b.brick != self.brick)
    }
}

impl Brick {
    pub fn new(start: Coords, end: Coords, brick: usize) -> Self {
        Self {
            blocks: (start.0..=end.0)
                .cartesian_product(start.1..=end.1)
                .cartesian_product(start.2..=end.2)
                .map(|((x, y), z)| Block {
                    coords: (x, y, z),
                    brick,
                })
                .collect(),
        }
    }
    pub fn is_supported(&self, map: &BlockMap) -> bool {
        self.blocks.iter().any(|b| b.is_supported(map))
    }
}

fn parse_coords(s: &str) -> Coords {
    s.split(",")
        .map(|s| s.parse().unwrap())
        .collect_tuple()
        .unwrap()
}

// returns the number of fallen bricks
fn simulate(bricks: &mut Vec<Brick>, blocks: &mut BlockMap) -> usize {
    bricks
        .iter_mut()
        .filter_map(|brick| {
            if brick.is_supported(blocks) {
                return None;
            }
            while !brick.is_supported(blocks) {
                for block in brick.blocks.iter_mut() {
                    blocks[block.coords.0][block.coords.1][block.coords.2] = None;
                    block.coords.2 -= 1;
                    blocks[block.coords.0][block.coords.1][block.coords.2] = Some(block.clone());
                }
            }
            Some(brick)
        })
        .count()
}

pub fn main(input: String) {
    let mut dims: Coords = (0, 0, 0);
    let input: Vec<(Coords, Coords)> = input
        .lines()
        .map(|l| {
            let (start, end) = l.split_once("~").unwrap();
            let (start, end) = (parse_coords(start), parse_coords(end));
            dims.0 = dims.0.max(end.0 + 1);
            dims.1 = dims.1.max(end.1 + 1);
            dims.2 = dims.2.max(end.2 + 1);
            (start, end)
        })
        .sorted_by(|(s1, _), (s2, _)| s1.2.cmp(&s2.2))
        .collect();
    let mut blocks = vec![vec![vec![None; dims.2]; dims.1]; dims.0];
    let mut bricks = input
        .iter()
        .enumerate()
        .map(|(i, (start, end))| {
            let brick = Brick::new(*start, *end, i);
            brick.blocks.iter().for_each(|b| {
                blocks[b.coords.0][b.coords.1][b.coords.2] = Some(b.clone());
            });
            brick
        })
        .collect_vec();
    simulate(&mut bricks, &mut blocks); // condense tower initially

    // println!("{:?}", bricks);
    let (mut p1, mut p2) = (0, 0);
    for i in 0..bricks.len() {
        let mut cur_bricks = bricks.clone();
        let mut cur_blocks = blocks.clone();
        let brick = cur_bricks.remove(i);
        for block in brick.blocks.iter() {
            cur_blocks[block.coords.0][block.coords.1][block.coords.2] = None;
        }
        let fallen = simulate(&mut cur_bricks, &mut cur_blocks);
        p1 += (fallen == 0) as usize;
        p2 += fallen;
    }
    println!("Part 1: {}", p1);
    println!("Part 2: {}", p2);
}
