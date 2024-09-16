use dotenvy::dotenv;

mod aoc2015;
mod aoc2023;

pub mod models;
pub mod utils;

#[tokio::main]
async fn main() {
    dotenv().ok();

    println!("Advent of Code");
    println!("Which year would you like to solve? (e.g., 2015, 2023)");

    let mut year = String::new();
    let _ = std::io::stdin().read_line(&mut year);
    let year: i32 = year.trim().parse().expect("Please type a valid year!");

    println!("Which day would you like to solve?");

    let mut day = String::new();
    let _ = std::io::stdin().read_line(&mut day);
    let day: i32 = day.trim().parse().expect("Please type a valid number!");

    let answer = match year {
        2015 => aoc2015::parse_day(day).await,
        2023 => aoc2023::parse_day(day).await,
        _ => panic!("This year is not yet implemented"),
    };

    println!("answer: {}", answer);
}
