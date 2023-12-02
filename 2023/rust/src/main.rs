#![feature(iter_array_chunks)]

pub mod t1;
pub mod t10;
pub mod t11;
pub mod t12;
pub mod t13;
pub mod t14;
pub mod t15;
pub mod t16;
pub mod t17;
pub mod t18;
pub mod t19;
pub mod t2;
pub mod t20;
pub mod t21;
pub mod t22;
pub mod t23;
pub mod t24;
pub mod t25;
pub mod t3;
pub mod t4;
pub mod t5;
pub mod t6;
pub mod t7;
pub mod t8;
pub mod t9;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Supply a task number!");
        std::process::exit(1);
    }
    let stage: i32 = args[1].trim().parse().expect("stage is numeric");
    let prefix = match &args[2..] {
        [] => "",
        [x, ..] => x,
    };
    let input =
        std::fs::read_to_string(format!("../inputs/{}{}", prefix, stage)).expect("file read error");
    match stage {
        1 => t1::main(input),
        2 => t2::main(input),
        3 => t3::main(input),
        4 => t4::main(input),
        5 => t5::main(input),
        6 => t6::main(input),
        7 => t7::main(input),
        8 => t8::main(input),
        9 => t9::main(input),
        10 => t10::main(input),
        11 => t11::main(input),
        12 => t12::main(input),
        13 => t13::main(input),
        14 => t14::main(input),
        15 => t15::main(input),
        16 => t16::main(input),
        17 => t17::main(input),
        18 => t18::main(input),
        19 => t19::main(input),
        20 => t20::main(input),
        21 => t21::main(input),
        22 => t22::main(input),
        23 => t23::main(input),
        24 => t24::main(input),
        25 => t25::main(input),
        _ => {
            eprintln!("Stages are 1-25");
            std::process::exit(1);
        }
    }
}
