"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/1

export function day1() {
  let data = getQuestionData("https://adventofcode.com/2022/day/1/input");
  return data
    .then((response) => {
      let caloriesArray = response.data.split("\n");
      let result = processCaloriesArray(caloriesArray);
      return {
        day1Part1: result.part1Result,
        day1Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 1 answer" + error);
    });
}

function test() {
  let testData = [
    "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000",
    ""
  ];

  return processCaloriesArray(testData);
}

function processCaloriesArray(array) {
  let sum = 0;
  let maxes = [];
  array.forEach((item) => {
    if (item === "") {
      maxes.push(sum);
      sum = 0;
    } else {
      let calories = Number.parseInt(item);
      sum += calories;
    }
  });

  let sorted = maxes.sort((a, b) => b - a);

  return {
    part1Result: sorted[0],
    part2Result: sorted[0] + sorted[1] + sorted[2],
  };
}
