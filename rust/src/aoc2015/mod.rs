use crate::models::aoc_answer::AocAnswer;

// pub mod day01;
// pub mod day02;
// pub mod day03;
// pub mod day04;
// pub mod day05;
// pub mod day06;
pub mod day07;
// pub mod day08;
// pub mod day09;
// pub mod day10;
// pub mod day11;
// pub mod day12;
// pub mod day13;
// pub mod day14;
// pub mod day15;
// pub mod day16;
// pub mod day17;
// pub mod day18;
// pub mod day19;
// pub mod day20;
// pub mod day21;
// pub mod day22;
// pub mod day23;
// pub mod day24;
// pub mod day25;

const YEAR: i32 = 2015;

pub async fn parse_day(day: i32) -> AocAnswer {
    match day {
        // 1 => day01::solve().await,
        // 2 => day02::solve().await,
        // 3 => day03::solve().await,
        // 4 => day04::solve().await,
        // 5 => day05::solve().await,
        // 6 => day06::solve().await,
        7 => day07::solve().await,
        // 8 => day08::solve().await,
        // 9 => day09::solve().await,
        // 10 => day10::solve().await,
        // 11 => day11::solve().await,
        // 12 => day12::solve().await,
        // 13 => day13::solve().await,
        // 14 => day14::solve().await,
        // 15 => day15::solve().await,
        // 16 => day16::solve().await,
        // 17 => day17::solve().await,
        // 18 => day18::solve().await,
        // 19 => day19::solve().await,
        // 20 => day20::solve().await,
        // 21 => day21::solve().await,
        // 22 => day22::solve().await,
        // 23 => day23::solve().await,
        // 24 => day24::solve().await,
        // 25 => day25::solve().await,
        _ => panic!("This day is not yet implemented for {YEAR}"),
    }
}
