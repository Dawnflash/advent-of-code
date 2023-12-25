use itertools::Itertools;
use regex::Regex;
use z3::{self, ast::Ast};

type Line = ((i64, i64, i64), (i64, i64, i64));

pub fn main(input: String) {
    let re = Regex::new(r"(-?\d+), +(-?\d+), +(-?\d+) +@ +(-?\d+), +(-?\d+), +(-?\d+)").unwrap();
    let lines: Vec<Line> = input
        .lines()
        .map(|l| {
            let caps: Vec<i64> = re
                .captures(l)
                .unwrap()
                .iter()
                .skip(1)
                .flatten()
                .map(|c| c.as_str().parse().unwrap())
                .collect();
            ((caps[0], caps[1], caps[2]), (caps[3], caps[4], caps[5]))
        })
        .collect();
    let bounds = (200000000000000.0, 400000000000000.0);
    // let bounds = (7.0, 27.0);
    let p1 = lines
        .iter()
        .combinations(2)
        .map(|c| intersection_2d(*c[0], *c[1]))
        .flatten()
        .filter(|(x, y)| *x >= bounds.0 && *x <= bounds.1 && *y >= bounds.0 && *y <= bounds.1)
        .count();
    println!("Part 1: {}", p1);
    let p2 = part2(&lines);
    println!("Part 2: {}", p2);
}

// x  = x1+n1dx1
// y  = y1+n1dy1
// n1 = (dx2(y1-y2)-dy2(x1-x2))/(dx1dy2-dy1dx2)
// n2 = (dx1(y2-y1)-dy1(x2-x1))/(dx2dy1-dy2dx1)
fn intersection_2d(
    ((x1, y1, _z1), (dx1, dy1, _dz1)): Line,
    ((x2, y2, _z2), (dx2, dy2, _dz2)): Line,
) -> Option<(f64, f64)> {
    let denom1 = dx1 * dy2 - dy1 * dx2;
    let denom2 = dx2 * dy1 - dy2 * dx1;
    if denom1 == 0 {
        return None;
    }
    let n1 = (dx2 * (y1 - y2) - dy2 * (x1 - x2)) as f64 / denom1 as f64;
    let n2 = (dx1 * (y2 - y1) - dy1 * (x2 - x1)) as f64 / denom2 as f64;
    if n1 < 0.0 || n2 < 0.0 {
        return None; // we only need "future" intersections
    }
    Some((x1 as f64 + n1 * dx1 as f64, y1 as f64 + n1 * dy1 as f64))
}

fn part2(lines: &Vec<Line>) -> u64 {
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let x = z3::ast::Int::new_const(&ctx, "x");
    let y = z3::ast::Int::new_const(&ctx, "y");
    let z = z3::ast::Int::new_const(&ctx, "z");
    let vx = z3::ast::Int::new_const(&ctx, "vx");
    let vy = z3::ast::Int::new_const(&ctx, "vy");
    let vz = z3::ast::Int::new_const(&ctx, "vz");
    let solver = z3::Solver::new(&ctx);
    for (i, (p, v)) in lines.iter().take(3).enumerate() {
        let t = z3::ast::Int::new_const(&ctx, format!("t{}", i));

        solver.assert(&(&x + &vx * &t)._eq(&(p.0 + v.0 * &t)));
        solver.assert(&(&y + &vy * &t)._eq(&(p.1 + v.1 * &t)));
        solver.assert(&(&z + &vz * &t)._eq(&(p.2 + v.2 * &t)));
    }
    solver.check();
    let sol = solver
        .get_model()
        .unwrap()
        .eval(&(x + y + z), true)
        .unwrap();
    sol.as_u64().unwrap()
}
