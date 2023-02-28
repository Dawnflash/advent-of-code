use std::rc::Rc;
use std::str::FromStr;

use aoc2022::parse_int;
use itertools::Itertools;
use num::integer::lcm;

#[derive(Clone)]
struct MTest {
    div: u64,
    monkey_t: usize,
    monkey_f: usize,
}
#[derive(Clone)]
struct Monkey {
    items: Vec<u64>,
    op: Rc<dyn Fn(u64) -> u64>,
    test: MTest,
    inspections: u64,
}

impl std::fmt::Debug for Monkey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Monkey [{}] {:?}", self.inspections, self.items)
    }
}

fn parse_op(s: &str) -> Rc<dyn Fn(u64) -> u64> {
    let (sop, sr) = s.split(" ").collect_tuple().unwrap();
    let op = match sop {
        "+" => |a, b| a + b,
        "*" => |a, b| a * b,
        _ => unreachable!("Unknown op!"),
    };

    match sr.parse::<u64>() {
        Err(_) => Rc::new(move |x| op(x, x)),
        Ok(i) => Rc::new(move |x| op(x, i)),
    }
}

impl FromStr for Monkey {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines = s.lines().skip(1).map(|s| s.trim()).collect_vec();
        let split =
            |n: usize, s: &str| lines[n].split(s).collect_tuple::<(&str, &str)>().unwrap().1;
        let sitems = split(0, ": ");
        let sop = split(1, "new = old ");
        let t: u64 = split(2, "divisible by ").parse().unwrap();
        let tt: usize = split(3, "monkey ").parse().unwrap();
        let tf: usize = split(4, "monkey ").parse().unwrap();
        let test = MTest {
            div: t,
            monkey_t: tt,
            monkey_f: tf,
        };

        let items = nom::multi::separated_list1(nom::bytes::complete::tag(", "), parse_int)(sitems)
            .unwrap()
            .1;

        Ok(Monkey {
            items: items,
            op: parse_op(sop),
            test: test,
            inspections: 0,
        })
    }
}

fn rounds(n: usize, monkeys: &mut Vec<Monkey>, div: bool) {
    let divisor = if div { 3 } else { 1 };
    let modulus = monkeys
        .iter()
        .map(|m| m.test.div)
        .reduce(|acc, e| lcm(acc, e))
        .unwrap();
    let mut item_bufs = vec![vec![]; monkeys.len()];
    for _ in 0..n {
        for (i, monkey) in monkeys.iter_mut().enumerate() {
            monkey.items.append(&mut item_bufs[i]);
            monkey.inspections += monkey.items.len() as u64;
            for item in monkey.items.drain(..) {
                let worry = (monkey.op)(item) / divisor % modulus;
                let next = if worry % monkey.test.div == 0 {
                    monkey.test.monkey_t
                } else {
                    monkey.test.monkey_f
                };
                item_bufs[next].push(worry);
            }
        }
    }
}

fn monkey_business(monkeys: &Vec<Monkey>) -> u64 {
    monkeys
        .iter()
        .map(|m| m.inspections)
        .sorted()
        .rev()
        .take(2)
        .product()
}

pub fn main(input: String) {
    let mut monkeys = input
        .split("\n\n")
        .map(|s| Monkey::from_str(s).unwrap())
        .collect_vec();
    let mut monkeys2 = monkeys.clone();
    rounds(20, &mut monkeys, true);
    println!("{}", monkey_business(&monkeys)); // part 1
    rounds(10000, &mut monkeys2, false);
    println!("{}", monkey_business(&monkeys2)); // part 2
}
