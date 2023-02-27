use std::collections::BinaryHeap;

pub fn main(input: String) {
    let mut sums = BinaryHeap::new();
    let mut sum = 0;
    for line in input.lines() {
        if line == "" {
            sums.push(sum);
            sum = 0;
        } else {
            sum += line.parse::<u32>().expect("parse error");
        }
    }
    println!("{}", sums.peek().unwrap());
    println!("{}", sums.iter().take(3).sum::<u32>());
}
