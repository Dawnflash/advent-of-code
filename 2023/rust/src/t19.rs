use gcollections::ops::*;
use interval::{ops::Range, Interval};
use std::collections::HashMap;

type Workflows = HashMap<String, Vec<WorkflowStep>>;

#[derive(Debug, PartialEq, Eq, Hash)]
enum WorkflowDest {
    Accept,
    Reject,
    Label(String),
}
#[derive(Debug, PartialEq, Eq, Hash)]
enum WorkflowStep {
    JumpGT(u8, u32, WorkflowDest),
    JumpLT(u8, u32, WorkflowDest),
    Jump(WorkflowDest),
}

pub fn main(input: String) {
    let (workflows, items) = input.split_once("\n\n").unwrap();
    let items: Vec<Vec<u32>> = items
        .lines()
        .map(|l| {
            l[1..l.len() - 1]
                .split(",")
                .map(|s| (s.split("=").nth(1).unwrap().parse::<u32>().unwrap()))
                .collect()
        })
        .collect();
    let workflows: Workflows = workflows
        .lines()
        .map(|l| {
            let (label, steps) = l[..l.len() - 1].split_once("{").unwrap();
            let steps = steps
                .split(",")
                .map(|step| {
                    if step.contains(":") {
                        let (cond, label) = step.split_once(":").unwrap();
                        if cond.contains(">") {
                            let (idx, val) = cond.split_once(">").unwrap();
                            WorkflowStep::JumpGT(
                                xmas_index(idx),
                                val.parse().unwrap(),
                                parse_wf_dest(label),
                            )
                        } else {
                            let (idx, val) = cond.split_once("<").unwrap();
                            WorkflowStep::JumpLT(
                                xmas_index(idx),
                                val.parse().unwrap(),
                                parse_wf_dest(label),
                            )
                        }
                    } else {
                        WorkflowStep::Jump(parse_wf_dest(step))
                    }
                })
                .collect();
            (label.to_string(), steps)
        })
        .collect();

    let p1: u32 = items
        .iter()
        .filter(|item| filter(item, &workflows, &workflows["in"]))
        .map(|item| item.iter().sum::<u32>())
        .sum();
    println!("Part 1: {}", p1);
    let p2 = accepted_combos(&workflows);
    println!("Part 2: {}", p2);
}

fn parse_wf_dest(s: &str) -> WorkflowDest {
    match s {
        "A" => WorkflowDest::Accept,
        "R" => WorkflowDest::Reject,
        _ => WorkflowDest::Label(s.to_string()),
    }
}

fn xmas_index(s: &str) -> u8 {
    match s {
        "x" => 0,
        "m" => 1,
        "a" => 2,
        "s" => 3,
        _ => panic!("Expected xmas!"),
    }
}

fn filter(item: &Vec<u32>, workflows: &Workflows, workflow: &Vec<WorkflowStep>) -> bool {
    for step in workflow {
        match step {
            WorkflowStep::JumpGT(idx, val, dest) => {
                if item[*idx as usize] > *val {
                    return filter_dest(item, workflows, dest);
                }
            }
            WorkflowStep::JumpLT(idx, val, dest) => {
                if item[*idx as usize] < *val {
                    return filter_dest(item, workflows, dest);
                }
            }
            WorkflowStep::Jump(dest) => {
                return filter_dest(item, workflows, dest);
            }
        }
    }
    false
}

fn filter_dest(item: &Vec<u32>, workflows: &Workflows, dest: &WorkflowDest) -> bool {
    match dest {
        WorkflowDest::Accept => true,
        WorkflowDest::Reject => false,
        WorkflowDest::Label(label) => filter(item, workflows, workflows.get(label).unwrap()),
    }
}

// part 2

fn accepted_combos(workflows: &Workflows) -> u64 {
    // inclusive bounds
    let bounds: Vec<Interval<u32>> = vec![Interval::new(1, 4000); 4];
    let mut stack: Vec<(String, Vec<Interval<u32>>)> = vec![("in".to_string(), bounds)];
    let mut accepting: u64 = 0;

    while let Some((label, bounds)) = stack.pop() {
        let wf = workflows.get(&label).unwrap();
        let mut bounds = bounds;
        for step in wf {
            match step {
                WorkflowStep::Jump(dest) => {
                    match dest {
                        WorkflowDest::Accept => {
                            accepting += bounds.iter().map(|b| b.size() as u64).product::<u64>();
                        }
                        WorkflowDest::Reject => {}
                        WorkflowDest::Label(label) => {
                            stack.push((label.to_string(), bounds));
                        }
                    }
                    break;
                }
                WorkflowStep::JumpGT(idx, val, dest) => {
                    let mut nbounds = bounds.clone();
                    nbounds[*idx as usize] =
                        bounds[*idx as usize].intersection(&Interval::new(*val + 1, 4000));
                    bounds[*idx as usize] =
                        bounds[*idx as usize].intersection(&Interval::new(1, *val));
                    match dest {
                        WorkflowDest::Accept => {
                            accepting += nbounds.iter().map(|b| b.size() as u64).product::<u64>();
                        }
                        WorkflowDest::Reject => {}
                        WorkflowDest::Label(label) => {
                            stack.push((label.to_string(), nbounds));
                        }
                    };
                }
                WorkflowStep::JumpLT(idx, val, dest) => {
                    let mut nbounds = bounds.clone();
                    nbounds[*idx as usize] =
                        bounds[*idx as usize].intersection(&Interval::new(1, *val - 1));
                    bounds[*idx as usize] =
                        bounds[*idx as usize].intersection(&Interval::new(*val, 4000));
                    match dest {
                        WorkflowDest::Accept => {
                            accepting += nbounds.iter().map(|b| b.size() as u64).product::<u64>();
                        }
                        WorkflowDest::Reject => {}
                        WorkflowDest::Label(label) => {
                            stack.push((label.to_string(), nbounds));
                        }
                    };
                }
            }
        }
    }
    accepting
}
