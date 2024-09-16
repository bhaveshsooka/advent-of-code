use std::io::stdin;

use apply::Apply;
use dotenvy::dotenv;
use models::aoc_answer::AocAnswer;

mod aoc2015;
mod aoc2023;

pub mod models;
pub mod utils;

#[tokio::main]
async fn main() {
    dotenv().ok();

    println!("Advent of Code 2023");
    println!("Which day would you like to solve?");

    let mut day = String::new();
    let _ = stdin().read_line(&mut day);
    let day: i32 = day.trim().parse().expect("Please type a number!");
    let answer: AocAnswer = day.apply(parse_day).await;

    println!("answer: {}", answer);
}

async fn parse_day(day: i32) -> AocAnswer {
    match day {
        1 => aoc2023::day01::solve().await,
        2 => aoc2023::day02::solve().await,
        3 => aoc2023::day03::solve().await,
        4 => aoc2023::day04::solve().await,
        5 => aoc2023::day05::solve().await,
        6 => aoc2023::day06::solve().await,
        7 => aoc2023::day07::solve().await,
        8 => aoc2023::day08::solve().await,
        9 => aoc2023::day09::solve().await,
        10 => aoc2023::day10::solve().await,
        11 => aoc2023::day11::solve().await,
        12 => aoc2023::day12::solve().await,
        15 => aoc2023::day15::solve().await,
        19 => aoc2023::day19::solve().await,
        20 => aoc2023::day20::solve().await,
        _ => {
            if day > 0 && day < 50 {
                panic!("This day is not yet implemented")
            } else {
                panic!("Unknown day")
            }
        }
    }
}
