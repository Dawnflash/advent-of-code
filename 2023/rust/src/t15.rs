type HBox = Vec<(String, u8)>;

pub fn main(input: String) {
    let p1: u64 = input
        .trim_end()
        .split(",")
        .map(hash)
        .fold(0, |acc, h| acc + h as u64);
    println!("Part 1: {}", p1);

    let mut boxes: Vec<HBox> = vec![vec![]; 256];
    input.trim_end().split(",").for_each(|s| {
        if s.ends_with("-") {
            let l = &s[..s.len() - 1];
            boxes[hash(l) as usize].retain(|(s, _)| s != l);
        } else {
            let (l, flen) = s.split_once("=").unwrap();
            let flen: u8 = flen.parse().unwrap();
            let b = &mut boxes[hash(l) as usize];
            if let Some((_, len)) = b.iter_mut().find(|(s, _)| s == l) {
                *len = flen;
            } else {
                b.push((l.to_string(), flen));
            }
        }
    });
    let p2: usize = boxes
        .iter()
        .enumerate()
        .flat_map(|(bn, b)| {
            b.iter()
                .enumerate()
                .map(move |(ln, (_, flen))| (1 + bn) * (1 + ln) * *flen as usize)
        })
        .sum();
    println!("Part 2: {}", p2);
}

fn hash(s: &str) -> u8 {
    s.as_bytes()
        .iter()
        .fold(0u32, |acc, &c| (acc + c as u32) * 17 % 256) as u8
}
