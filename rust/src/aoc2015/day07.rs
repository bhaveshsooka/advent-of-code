use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

use super::YEAR;
const DAY: i32 = 7;

pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(YEAR, DAY).await;

    let answer: AocAnswer = AocAnswer {
        day: DAY,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    "Not yet implemented".to_string()
}

fn part2(input_data: &String) -> String {
    "Not yet implemented".to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i");

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i");

    part2(&input_data)
}
