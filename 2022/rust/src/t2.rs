pub fn main(input: String) {
    let mut sum: u32 = 0;
    let mut sum2: u32 = 0;
    for line in input.lines() {
        let (foe, me) = match line.as_bytes() {
            [foe, b' ', me] => (foe - b'A', me - b'X'),
            _ => unreachable!("parse error"),
        };
        sum += score(foe, me) as u32;
        sum2 += score(foe, p2move(foe, me)) as u32;
    }
    println!("{sum}");
    println!("{sum2}");
}

fn p2move(foe: u8, me: u8) -> u8 {
    (foe + me + 2) % 3
}

fn score(foe: u8, me: u8) -> u8 {
    1 + me + 3 * ((4 + me - foe) % 3)
}
