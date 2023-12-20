use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
enum GateType {
    Broadcast,
    FlipFlop(bool),
    Nand(HashMap<String, bool>),
}
#[derive(Debug, Clone)]
struct Gate {
    kind: GateType,
    label: String,
    targets: Vec<String>,
}
type SignalQueue = VecDeque<(String, String, bool)>;

pub fn main(input: String) {
    let mut rev_gates: HashMap<String, Vec<String>> = HashMap::new();
    let mut gates: HashMap<String, Gate> = input
        .lines()
        .map(|line| {
            let (label, targets) = line.split_once(" -> ").unwrap();
            let kind = match label.as_bytes()[0] {
                b'%' => GateType::FlipFlop(false),
                b'&' => GateType::Nand(HashMap::new()),
                _ => GateType::Broadcast,
            };
            let label = match kind {
                GateType::Broadcast => label,
                _ => &label[1..],
            }
            .to_string();
            let targets: Vec<String> = targets.split(", ").map(String::from).collect();
            targets.iter().for_each(|target| {
                rev_gates
                    .entry(target.clone())
                    .or_insert_with(Vec::new)
                    .push(label.clone());
            });
            (
                label.clone(),
                Gate {
                    kind,
                    label,
                    targets,
                },
            )
        })
        .collect();
    for (label, gate) in gates.iter_mut() {
        match &mut gate.kind {
            GateType::Nand(map) => {
                for target in rev_gates.get(label).unwrap().iter() {
                    map.insert(target.clone(), false);
                }
            }
            _ => {}
        }
    }
    println!("Part 1: {}", part1(&mut gates.clone()));
    println!("Part 2: {}", part2(&mut gates));
}

fn part1(gates: &mut HashMap<String, Gate>) -> u64 {
    (0..1000)
        .map(|_| {
            let mut counter = [0, 0];
            let mut pulses: SignalQueue =
                VecDeque::from(vec![(String::new(), "broadcaster".to_string(), false)]);
            while let Some((from, to, pulse_in)) = pulses.pop_front() {
                counter[pulse_in as usize] += 1;
                if let Some(gate) = gates.get_mut(&to) {
                    pulse(pulse_in, from, gate, &mut pulses);
                }
            }
            counter
        })
        .fold([0, 0], |acc, n| [acc[0] + n[0], acc[1] + n[1]])
        .iter()
        .product()
}

fn part2(gates: &mut HashMap<String, Gate>) -> u64 {
    let mut cycles: HashMap<String, u64> = gates
        .values()
        .find_map(|gate| {
            if let GateType::Nand(map) = &gate.kind {
                if gate.targets.contains(&"rx".to_string()) {
                    return Some(map.iter().map(|(k, _)| (k.clone(), 0)).collect());
                }
            };
            None
        })
        .unwrap();
    for i in 1.. {
        let mut pulses: SignalQueue =
            VecDeque::from(vec![(String::new(), "broadcaster".to_string(), false)]); // initial pulse
        while let Some((from, to, pulse_in)) = pulses.pop_front() {
            if !pulse_in && cycles.get(&to) == Some(&0) {
                cycles.insert(to.clone(), i);
                if cycles.values().all(|v| *v != 0) {
                    return cycles.values().fold(1, |acc, n| num::integer::lcm(acc, *n));
                }
            }
            if let Some(gate) = gates.get_mut(&to) {
                // println!("{}: {} > {:?}", from, pulse_in, gate);
                pulse(pulse_in, from, gate, &mut pulses);
            }
        }
    }
    0
}

fn pulse(pulse_in: bool, from: String, gate: &mut Gate, pulses: &mut SignalQueue) {
    let out = match &mut gate.kind {
        GateType::Broadcast => Some(pulse_in),
        GateType::FlipFlop(state) => {
            if pulse_in {
                None
            } else {
                *state = !*state;
                Some(*state)
            }
        }
        GateType::Nand(map) => {
            map.insert(from, pulse_in);
            Some(!map.values().all(|v| *v))
        }
    };
    if let Some(signal) = out {
        for target in gate.targets.iter() {
            pulses.push_back((gate.label.clone(), target.clone(), signal));
        }
    }
}
