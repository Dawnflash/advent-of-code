pub fn main(input: String) {
    let (p1, p2) = input
        .lines()
        .map(|line| {
            let nums: Vec<i64> = line.split(" ").map(|s| s.parse().unwrap()).collect();
            (extrapolate(&nums, false), extrapolate(&nums, true))
        })
        .fold((0, 0), |(acc1, acc2), (n1, n2)| (acc1 + n1, acc2 + n2));
    println!("Part 1: {}", p1);
    println!("Part 2: {}", p2);
}

fn extrapolate(vals: &Vec<i64>, inv: bool) -> i64 {
    let (constant, last, diffs) =
        vals.iter()
            .skip(1)
            .fold((true, vals[0], vec![]), |(c, last, mut ds), &v| {
                ds.push(v - last);
                (c && v == last, v, ds)
            });
    if inv {
        if constant {
            return vals[0];
        } else {
            return vals[0] - extrapolate(&diffs, inv);
        }
    } else {
        if constant {
            return last;
        } else {
            return last + extrapolate(&diffs, inv);
        }
    }
}
