use aoc2023::*;
use std::collections::HashMap;

struct Dir {
    children: HashMap<String, Dir>,
    size: u64,
}

impl Dir {
    fn new() -> Self {
        Self {
            children: HashMap::new(),
            size: 0,
        }
    }
}

pub fn main(input: String) {
    let mut root = Dir::new();
    eval(input.lines().skip(1).by_ref(), &mut root);
    let mut sizes = vec![];
    post_eval(&mut root, &mut sizes);
    println!("{}", sizes.iter().filter(|&&s| s <= 100000).sum::<u64>()); // part 1
    let needed = 30000000 - (70000000 - root.size);
    println!("{}", sizes.iter().filter(|&&s| s >= needed).min().unwrap()); // part 2
}

// correct directory sizes and return them in a vector
fn post_eval(dir: &mut Dir, sizes: &mut Vec<u64>) {
    for (_, child) in dir.children.iter_mut() {
        post_eval(child, sizes);
        dir.size += child.size;
    }
    sizes.push(dir.size);
}

// populate directory tree
fn eval<'a, I>(lines: &mut I, dir: &mut Dir)
where
    I: Iterator<Item = &'a str>,
{
    let line = lines.next();
    if line.is_none() {
        return;
    }
    let line = line.unwrap();
    if line.starts_with("$ ") {
        // commands
        let cmd = &line[2..];
        if cmd.starts_with("cd") {
            if cmd == "cd .." {
                return;
            } else {
                let dirname = &cmd[3..];
                eval(lines, dir.children.get_mut(dirname).unwrap());
            }
        }
    } else if line.starts_with("dir") {
        // files
        let dirname = &line[4..];
        dir.children.insert(dirname.to_string(), Dir::new());
    } else {
        let (s, _) = parse_file(line).unwrap().1;
        dir.size += s;
    }
    eval(lines, dir)
}

fn parse_file(i: &str) -> nom::IResult<&str, (u64, &str)> {
    nom::sequence::separated_pair(
        parse_int,
        nom::character::complete::char(' '),
        nom::combinator::rest,
    )(i)
}
