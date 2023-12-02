use itertools::Itertools;

pub fn main(input: String) {
    let b = input.as_bytes();
    println!("{}\n{}", detect(b, 4), detect(b, 14));
}

fn detect(s: &[u8], sz: usize) -> usize {
    sz + s.windows(sz).position(|w| w.iter().all_unique()).unwrap()
}
