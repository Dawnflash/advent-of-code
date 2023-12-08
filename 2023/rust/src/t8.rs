use std::collections::HashMap;

use itertools::Itertools;
use num::integer::lcm;

type Graph<'a> = HashMap<&'a str, (&'a str, &'a str)>;

pub fn main(input: String) {
    let (dirs, graph) = input.split_once("\n\n").unwrap();
    let graph: Graph = graph
        .lines()
        .map(|l: &str| {
            let (node, edges) = l.split_once(" = ").unwrap();
            let (l, r) = edges[1..edges.len() - 1].split_once(", ").unwrap();
            (node, (l, r))
        })
        .collect();
    let p1 = steps_until(&dirs, &graph, "AAA", |node| node == "ZZZ");
    println!("Part 1: {}", p1);

    let p2 = graph
        .keys()
        .filter(|&&k| k.ends_with("A"))
        .map(|&k| steps_until(&dirs, &graph, k, |node| node.ends_with("Z")))
        .fold(1, |acc, n| lcm(acc, n));
    println!("Part 2: {}", p2);
}

fn steps_until(dirs: &str, graph: &Graph, start: &str, end: impl Fn(&str) -> bool) -> u64 {
    dirs.chars()
        .cycle()
        .fold_while((0u64, start), |(n, node), dir| {
            if end(node) {
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
        .into_inner()
        .0
}
