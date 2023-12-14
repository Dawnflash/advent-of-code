use std::collections::HashMap;

type Memo = HashMap<(String, Vec<usize>), usize>;

pub fn main(input: String) {
    let mut memo: Memo = HashMap::new();
    let mut hits: f64 = 0.0;
    let p1: usize = input
        .lines()
        .map(|line| {
            let (layout, runs) = line.split_once(" ").unwrap();
            let runs: Vec<usize> = runs.split(",").map(|c| c.parse().unwrap()).collect();
            let (res, hit) = analyze(&layout, &runs, &mut memo);
            hits += if hit { 1.0 } else { 0.0 };
            res
        })
        .sum();
    println!("Part 1: {}", p1);
    let p2: usize = input
        .lines()
        .map(|line| {
            let (_layout, _runs) = line.split_once(" ").unwrap();
            let _runs: Vec<usize> = _runs.split(",").map(|c| c.parse().unwrap()).collect();
            let runs = (0..5).fold(Vec::new(), |mut acc, _| {
                acc.extend(_runs.iter());
                acc
            });
            let layout = [_layout, _layout, _layout, _layout, _layout].join("?");
            let (res, hit) = analyze(&layout, &runs, &mut memo);
            hits += if hit { 1.0 } else { 0.0 };
            res
        })
        .sum();
    println!("Part 2: {}", p2);
    println!(
        "Cache hits: {}, {}%\nCache lines: {}",
        hits,
        hits / (input.lines().count() as f64 * 2.0) * 100.0,
        memo.len()
    );
}

fn analyze(layout: &str, runs: &Vec<usize>, memo: &mut Memo) -> (usize, bool) {
    let run_sum = runs.iter().sum::<usize>();
    let slots = layout.chars().filter(|c| *c == '?').count();
    let pending = run_sum - layout.chars().filter(|c| *c == '#').count();
    let res = analyze_step(
        layout.to_string(),
        runs.clone(),
        false,
        slots,
        pending,
        memo,
    );
    memo.insert((layout.to_string(), runs.clone()), res.0);
    res
}

// DFS
fn analyze_step(
    layout: String,
    runs: Vec<usize>,
    in_run: bool,
    slots: usize,
    pending: usize,
    memo: &mut Memo,
) -> (usize, bool) {
    if !in_run {
        if let Some(&res) = memo.get(&(layout.to_string(), runs.clone())) {
            return (res, true);
        }
    }
    let cur = layout.chars().next();
    // deterministic checks
    if pending == 0 && cur.is_some() && !in_run {
        let full = layout.replace("?", ".");
        return if analyze_det(&full, &runs) {
            (1, false)
        } else {
            (0, false)
        };
    }
    if slots == pending && cur.is_some() && !in_run {
        let full = layout.replace("?", "#");
        return if analyze_det(&full, &runs) {
            (1, false)
        } else {
            (0, false)
        };
    }
    let runs_copy = runs.clone();
    let res = match (cur, runs.get(0)) {
        (None, None) => (1, false),
        (None, Some(_)) => {
            // # ending
            if runs.iter().all(|&r| r == 0) {
                (1, false)
            } else {
                (0, false)
            }
        }
        (Some(_), None) => {
            // . tail
            if layout.chars().all(|c| c != '#') {
                (1, false)
            } else {
                (0, false)
            }
        }
        (Some(c), Some(&r)) => match c {
            '.' => {
                if in_run {
                    if r != 0 {
                        (0, false)
                    } else {
                        analyze_step(
                            layout.chars().skip(1).collect(),
                            runs.iter().skip(1).cloned().collect(),
                            false,
                            slots,
                            pending,
                            memo,
                        )
                    }
                } else {
                    analyze_step(
                        layout.chars().skip(1).collect(),
                        runs,
                        false,
                        slots,
                        pending,
                        memo,
                    )
                }
            }
            '#' => {
                if r == 0 {
                    (0, false)
                } else {
                    let mut runs = runs.clone();
                    runs[0] -= 1;
                    analyze_step(
                        layout.chars().skip(1).collect(),
                        runs,
                        true,
                        slots,
                        pending,
                        memo,
                    )
                }
            }
            '?' => {
                if pending == slots {
                    analyze_step(
                        String::from('#') + &layout[1..],
                        runs,
                        in_run,
                        slots - 1,
                        pending - 1,
                        memo,
                    )
                } else if pending == 0 {
                    analyze_step(
                        String::from('.') + &layout[1..],
                        runs.clone(),
                        in_run,
                        slots - 1,
                        pending,
                        memo,
                    )
                } else {
                    let a = analyze_step(
                        String::from('.') + &layout[1..],
                        runs.clone(),
                        in_run,
                        slots - 1,
                        pending,
                        memo,
                    );
                    let b = analyze_step(
                        String::from('#') + &layout[1..],
                        runs,
                        in_run,
                        slots - 1,
                        pending - 1,
                        memo,
                    );
                    (a.0 + b.0, a.1 || b.1)
                }
            }
            _ => panic!("unexpected char"),
        },
    };
    if !in_run {
        memo.insert((layout, runs_copy), res.0);
    }
    res
}

// fast deterministic check
fn analyze_det(layout: &str, runs: &Vec<usize>) -> bool {
    let res = layout
        .split(".")
        .filter(|s| !s.is_empty())
        .map(|s| s.len())
        .eq(runs.iter().filter(|&&v| v != 0).copied());
    res
}
