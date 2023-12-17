use std::collections::{hash_map::Entry, HashMap, VecDeque};

use aoc2023::{Direction2D as Dir, Point2D};

#[derive(Debug, Clone)]
struct State {
    pub pos: Point2D,
    pub loss: u32,
    pub dir_in: Dir,
    pub segment: u8,
}

pub fn main(input: String) {
    let tiles: Vec<Vec<u32>> = input
        .lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect();
    let bounds = (
        Point2D::origin(),
        Point2D::new(tiles[0].len() as i32, tiles.len() as i32),
    );
    let init_states = VecDeque::from(vec![
        State {
            pos: Point2D::new(1, 0),
            loss: 0,
            dir_in: Dir::R,
            segment: 0,
        },
        State {
            pos: Point2D::new(0, 1),
            loss: 0,
            dir_in: Dir::D,
            segment: 0,
        },
    ]);
    let p1 = min_loss(tiles.clone(), bounds, init_states.clone(), 0, 3);
    println!("Part 1: {}", p1);
    let p2 = min_loss(tiles, bounds, init_states, 4, 10);
    println!("Part 2: {}", p2);
}

fn min_loss(
    tiles: Vec<Vec<u32>>,
    bounds: (Point2D, Point2D),
    init: VecDeque<State>,
    min_steps: u8,
    max_steps: u8,
) -> u32 {
    let end_pos = bounds.1 - Point2D::new(1, 1);
    let mut mins: Vec<Vec<HashMap<(Dir, u8), u32>>> =
        vec![vec![HashMap::new(); bounds.1.x as usize]; bounds.1.y as usize];
    let mut cur = init;
    while let Some(state) = cur.pop_front() {
        let cur_loss: u32 = &tiles[state.pos.y as usize][state.pos.x as usize] + state.loss;
        let best = &mut mins[state.pos.y as usize][state.pos.x as usize];

        // DEBUG
        // if state.pos.y == 0 && state.pos.x < 10 && state.pos.x > 3 {
        //     println!("{:?} {} {:?}", state, cur_loss, best);
        // }

        match best.entry((state.dir_in, state.segment)) {
            Entry::Occupied(mut entry) => {
                if cur_loss >= *entry.get() {
                    continue;
                }
                entry.insert(cur_loss);
            }
            Entry::Vacant(entry) => {
                entry.insert(cur_loss);
            }
        }
        if state.pos == end_pos {
            continue; // no point in exploring further
        }
        for (dir, segment) in [
            (state.dir_in, state.segment + 1),
            (state.dir_in.rotate(Dir::R), 0),
            (state.dir_in.rotate(Dir::L), 0),
        ] {
            if segment >= max_steps {
                continue;
            }
            if state.dir_in != dir && state.segment + 1 < min_steps {
                continue;
            }
            if let Some(loc) = state.pos.step_2d(bounds.0, bounds.1, dir) {
                cur.push_back(State {
                    pos: loc,
                    loss: cur_loss,
                    dir_in: dir,
                    segment,
                });
            }
        }
    }
    let end = &mins[end_pos.y as usize][end_pos.x as usize];
    *end.values().min().unwrap()
}
