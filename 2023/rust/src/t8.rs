use std::collections::HashMap;

use itertools::Itertools;
use num::integer::lcm;

pub fn main(input: String) {
    let (sdir, sgraph) = input.split_once("\n\n").unwrap();
    let graph: HashMap<&str, (&str, &str)> = sgraph
        .lines()
        .map(|l: &str| {
            let (node, edges) = l.split_once(" = ").unwrap();
            let (l, r) = edges[1..edges.len() - 1].split_once(", ").unwrap();
            (node, (l, r))
        })
        .collect();
    let (p1, _) = sdir
        .chars()
        .cycle()
        .fold_while((0, "AAA"), |(n, node), dir| {
            if node == "ZZZ" {
                return itertools::FoldWhile::Done((n, node));
            }
            let (l, r) = graph[node];
            let next = match dir {
                'L' => l,
                'R' => r,
                _ => panic!("Invalid direction"),
            };
            itertools::FoldWhile::Continue((n + 1, next))
        })
        .into_inner();
    println!("Part 1: {}", p1);

    let p2keys = graph
        .keys()
        .filter(|&&k| k.ends_with("A"))
        .cloned()
        .collect_vec();
    let mut p2loop_steps: HashMap<&str, u64> = p2keys.iter().map(|&k| (k, 0)).collect();
    let p2 = p2keys
        .iter()
        .map(|&k| {
            let res = sdir
                .chars()
                .cycle()
                .fold_while((0, k), |(n, node), dir| {
                    if node.ends_with("Z") {
                        if p2loop_steps[k] == 0 {
                            p2loop_steps.insert(k, n);
                        } else {
                            println!(
                                "{}: {} % {} = {}",
                                k,
                                n,
                                p2loop_steps[k],
                                n % p2loop_steps[k]
                            );
                            return itertools::FoldWhile::Done((n - p2loop_steps[k], node));
                        }
                    }
                    let (l, r) = graph[node];
                    let next = match dir {
                        'L' => l,
                        'R' => r,
                        _ => panic!("Invalid direction"),
                    };
                    itertools::FoldWhile::Continue((n + 1, next))
                })
                .into_inner()
                .0;
            res
        })
        .fold(1, |acc, n| lcm(acc, n));
    println!("Part 2: {}", p2);
}
