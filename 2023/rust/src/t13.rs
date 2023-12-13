pub fn main(input: String) {
    let (p1, p2): (usize, usize) = input
        .split("\n\n")
        .map(|pattern| {
            let mut rows: Vec<String> = pattern.lines().map(|s| s.to_string()).collect();
            let orig = reflect(&rows, None, false);
            for y in 0..rows.len() {
                for x in 0..rows[0].len() {
                    let (o, c) = if rows[y].as_bytes()[x] == b'#' {
                        ("#", ".")
                    } else {
                        (".", "#")
                    };
                    rows[y].replace_range(x..x + 1, c);
                    let alt = reflect(&rows, Some(orig), false);
                    rows[y].replace_range(x..x + 1, o);
                    if alt != orig && alt != 0 {
                        return (orig, alt);
                    }
                }
            }
            println!("orig: {}", orig);
            for row in rows.iter() {
                println!("{}", row);
            }
            panic!("No alt found")
        })
        .fold((0, 0), |(acc1, acc2), (n1, n2)| (acc1 + n1, acc2 + n2));
    println!("Part 1: {}", p1);
    println!("Part 2: {}", p2);
}

fn reflect(pattern: &Vec<String>, orig: Option<usize>, transposed: bool) -> usize {
    let mult = if transposed { 1 } else { 100 };
    (1..pattern.len())
        .find(|&n| {
            if Some(n * mult) == orig {
                return false;
            }
            let rows = std::cmp::min(n, pattern.len() - n);
            pattern[n - rows..n]
                .iter()
                .eq(pattern[n..n + rows].iter().rev())
        })
        .map(|v| v * mult)
        .unwrap_or_else(|| {
            if transposed {
                0
            } else {
                let cols: Vec<String> = (0..pattern[0].len())
                    .map(|col| {
                        pattern
                            .iter()
                            .map(|row| row.chars().nth(col).unwrap())
                            .collect()
                    })
                    .collect();
                reflect(&cols, orig, true)
            }
        })
}
