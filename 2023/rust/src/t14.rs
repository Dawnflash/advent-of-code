use std::collections::HashMap;

type Map = Vec<Vec<char>>;
type Memo = HashMap<Map, usize>;

pub fn main(input: String) {
    let map: Map = input.lines().map(|line| line.chars().collect()).collect();
    let size = (map[0].len(), map.len());

    let mut p1 = map.clone();
    tilt_up(&mut p1, size);
    println!("Part 1: {}", north_load(&p1));

    let mut p2 = map.clone();
    let mut memo: Memo = HashMap::from([(p2.clone(), 0)]);
    let mut cur_cycle: usize = 0;
    let p2cycles: usize = 1000000000;
    loop {
        cur_cycle += 1;
        cycle(&mut p2, size);
        match memo.get(&p2) {
            Some(prev_cycle) => {
                let rem = (p2cycles - cur_cycle) % (cur_cycle - prev_cycle);
                println!("Cycle: {}-{}: rem {}", prev_cycle, cur_cycle, rem);
                for _ in 0..rem {
                    cycle(&mut p2, size);
                }
                break;
            }
            _ => (),
        };
        memo.insert(p2.clone(), cur_cycle);
    }
    println!("Part 2: {}", north_load(&p2));
}

fn cycle(map: &mut Map, size: (usize, usize)) {
    tilt_up(map, size);
    tilt_left(map, size);
    tilt_down(map, size);
    tilt_right(map, size);
}

fn tilt_up(map: &mut Map, size: (usize, usize)) {
    for y in 0..size.1 {
        for x in 0..size.0 {
            if map[y][x] != 'O' {
                continue;
            }
            let mut ny = y;
            for cy in (0..y).rev() {
                if map[cy][x] == '.' {
                    ny = cy;
                } else {
                    break;
                }
            }
            if y != ny {
                map[y][x] = '.';
                map[ny][x] = 'O';
            }
        }
    }
}
fn tilt_down(map: &mut Map, size: (usize, usize)) {
    for y in (0..size.1).rev() {
        for x in (0..size.0).rev() {
            if map[y][x] != 'O' {
                continue;
            }
            let mut ny = y;
            for cy in y + 1..size.1 {
                if map[cy][x] == '.' {
                    ny = cy;
                } else {
                    break;
                }
            }
            if y != ny {
                map[y][x] = '.';
                map[ny][x] = 'O';
            }
        }
    }
}
fn tilt_left(map: &mut Map, size: (usize, usize)) {
    for x in 0..size.0 {
        for y in 0..size.0 {
            if map[y][x] != 'O' {
                continue;
            }
            let mut nx = x;
            for cx in (0..x).rev() {
                if map[y][cx] == '.' {
                    nx = cx;
                } else {
                    break;
                }
            }
            if x != nx {
                map[y][x] = '.';
                map[y][nx] = 'O';
            }
        }
    }
}
fn tilt_right(map: &mut Map, size: (usize, usize)) {
    for x in (0..size.0).rev() {
        for y in (0..size.0).rev() {
            if map[y][x] != 'O' {
                continue;
            }
            let mut nx = x;
            for cx in x + 1..size.0 {
                if map[y][cx] == '.' {
                    nx = cx;
                } else {
                    break;
                }
            }
            if x != nx {
                map[y][x] = '.';
                map[y][nx] = 'O';
            }
        }
    }
}

fn north_load(map: &Map) -> usize {
    map.iter()
        .enumerate()
        .flat_map(|(y, line)| {
            line.iter()
                .filter(|&&c| c == 'O')
                .map(move |_| map.len() - y)
        })
        .sum()
}
