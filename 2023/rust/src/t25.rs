use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
struct Graph {
    nodes: HashMap<String, Vec<String>>,
}

pub fn main(input: String) {
    // _dot_out(input.clone());
    // optical algorithm thanks to neato
    let p1_cut = [("lcm", "ddl"), ("pcs", "rrl"), ("qnd", "mbk")];
    let mut graph = Graph {
        nodes: input
            .lines()
            .flat_map(|line| {
                let (src, dsts) = line.split_once(": ").unwrap();
                dsts.split(" ")
                    .chain([src])
                    .map(move |s| (s.to_owned(), vec![]))
            })
            .collect(),
    };
    for line in input.lines() {
        let (src, dsts) = line.split_once(": ").unwrap();
        for dst in dsts.split(" ") {
            graph.nodes.get_mut(src).unwrap().push(dst.to_owned());
            graph.nodes.get_mut(dst).unwrap().push(src.to_owned());
        }
    }
    let mut cut_graph = graph.clone();
    for (src, dst) in p1_cut.iter() {
        cut_graph
            .nodes
            .get_mut(src.to_owned())
            .unwrap()
            .retain(|n| n != dst);
        cut_graph
            .nodes
            .get_mut(dst.to_owned())
            .unwrap()
            .retain(|n| n != src);
    }
    println!(
        "Part 1: {}",
        reachable(&cut_graph, p1_cut[0].0.to_owned())
            * reachable(&cut_graph, p1_cut[0].1.to_owned())
    );
}

fn reachable(graph: &Graph, start: String) -> usize {
    let mut visited = HashSet::new();
    let mut cur = vec![start];
    while !cur.is_empty() {
        let mut next = vec![];
        for node in cur {
            if visited.contains(&node) {
                continue;
            }
            visited.insert(node.clone());
            next.extend(graph.nodes.get(&node).unwrap().iter().cloned());
        }
        cur = next;
    }
    visited.len()
}

fn _dot_out(input: String) {
    println!("graph {{");
    input.lines().for_each(|line| {
        let (src, dsts) = line.split_once(": ").unwrap();
        for dst in dsts.split(" ") {
            println!("{} -- {};", src, dst);
        }
    });
    println!("}}");
}
