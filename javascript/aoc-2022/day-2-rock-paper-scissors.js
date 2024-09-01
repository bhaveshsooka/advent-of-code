"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/2

export function day2() {
  let data = getQuestionData("https://adventofcode.com/2022/day/2/input");
  return data
    .then((response) => {
      let rpsPlays = response.data.split("\n");
      let result = processPlays(rpsPlays);
      return {
        day2Part1: result.part1Result,
        day2Part2: result.part2Result,
        test: test()
      };
    })
    .catch((error) => {
      console.log("Problem getting day 2 answer" + error);
    });
}

function test() {
  let testData = ["A Y", "B X", "C Z"];
  return processPlays(testData);
}

function processPlays(rpsPlays) {
  let sumPart1 = 0;
  let sumPart2 = 0;
  rpsPlays.forEach((play) => {
    let player1 = play.split(" ")[0];
    let player2 = play.split(" ")[1];
    sumPart1 += getPart1Score(player1, player2);
    sumPart2 += getPart2Score(player1, player2);
  });
  return {
    part1Result: sumPart1,
    part2Result: sumPart2,
  };
}

function getPart1Score(player1, player2) {
  let score = getRPSScorePart1(player2);
  switch (player1) {
    case "A": // rock
      if (player2 === "X") return score + 3;
      else if (player2 === "Y") return score + 6;
      else return score;
    case "B": // paper
      if (player2 === "X") return score;
      else if (player2 === "Y") return score + 3;
      else return score + 6;
    case "C": //scissors
      if (player2 === "X") return score + 6;
      else if (player2 === "Y") return score;
      else return score + 3;
    default:
      return score;
  }
}

function getPart2Score(player1, outcome) {
  let score = getRPSScorePart2(outcome);
  switch (player1) {
    case "A": // rock
      if (outcome === "X") return score + 3;
      else if (outcome === "Y") return score + 1;
      else return score + 2;
    case "B": // paper
      if (outcome === "X") return score + 1;
      else if (outcome === "Y") return score + 2;
      else return score + 3;
    case "C": //scissors
      if (outcome === "X") return score + 2;
      else if (outcome === "Y") return score + 3;
      else return score + 1;
    default:
      return score;
  }
}

function getRPSScorePart1(play) {
  if (play === "A" || play === "X") return 1;
  else if (play === "B" || play === "Y") return 2;
  else if (play === "C" || play === "Z") return 3;
  else return 0;
}

function getRPSScorePart2(play) {
  if (play === "X") return 0;
  else if (play === "Y") return 3;
  else if (play === "Z") return 6;
  else return 0;
}
