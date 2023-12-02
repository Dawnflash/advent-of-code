static NUMERALS: &[(&str, &str)] = &[
    ("one", "o1e"),
    ("two", "t2o"),
    ("three", "t3ree"),
    ("four", "f4ur"),
    ("five", "f5ve"),
    ("six", "s6x"),
    ("seven", "s7ven"),
    ("eight", "e8ght"),
    ("nine", "n9ne"),
];

pub fn main(input: String) {
    let r1: u32 = input.lines().map(numerize).sum();
    println!("Part 1: {}", r1);
    let r2: u32 = input.lines().map(numerize2).sum();
    println!("Part 2: {}", r2);
}

fn numerize(s: &str) -> u32 {
    let mut nums = s
        .chars()
        .filter(|c| c.is_numeric())
        .map(|c| c.to_digit(10))
        .peekable();
    let res = match nums.peek() {
        Some(n) => 10 * n.unwrap() + nums.last().unwrap().unwrap(),
        _ => 0,
    };
    // println!("numerize({}) = {}", s, res);
    res
}

fn numerize2(s: &str) -> u32 {
    let s2 = NUMERALS
        .iter()
        .fold(String::from(s), |acc, (src, dst)| acc.replace(src, dst));
    numerize(s2.as_str())
}
