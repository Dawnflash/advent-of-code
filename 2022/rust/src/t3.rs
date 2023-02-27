pub fn main(input: String) {
    let sum: u64 = input
        .lines()
        .map(|line| {
            let half = line.len() / 2;
            ((bset(&line[..half]) & bset(&line[half..])) as f64)
                .log2()
                .round() as u64
        })
        .sum();
    println!("{sum}");
    // part 2
    let sum: u64 = input
        .lines()
        .array_chunks()
        .map(|[a, b, c]| ((bset(a) & bset(b) & bset(c)) as f64).log2().round() as u64)
        .sum();
    println!("{sum}");
}

fn bset(s: &str) -> u64 {
    s.as_bytes()
        .iter()
        .map(|c| 1 << prio(*c))
        .reduce(|acc, p| acc | p)
        .expect("forming bset failed")
}

fn prio(item: u8) -> u8 {
    match item {
        b'a'..=b'z' => item - b'a' + 1,
        b'A'..=b'Z' => item - b'A' + 27,
        _ => unreachable!("invalid item"),
    }
}
