"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/4

export function day4() {
  let data = getQuestionData("https://adventofcode.com/2022/day/4/input");
  return data
    .then((response) => {
      let sectionAssignmentArray = response.data.split("\n");
      let result = processSectionAssignmentArray(sectionAssignmentArray);
      return {
        day4Part1: result.part1Result,
        day4Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 4 answer" + error);
    });
}

function test() {
  let testData = [
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8",
  ];

  return processSectionAssignmentArray(testData);
}

function processSectionAssignmentArray(array) {
  let countPart1 = 0;
  let countPart2 = 0;
  array.forEach((assignments) => {
    if (assignments === "") return;

    let assignmentsArray = assignments.split(",");

    let firstElf = assignmentsArray[0].split("-");
    let secondElf = assignmentsArray[1].split("-");

    let a = Number.parseInt(firstElf[0]) - Number.parseInt(secondElf[0]);
    let b = Number.parseInt(firstElf[1]) - Number.parseInt(secondElf[1]);

    let c = Number.parseInt(firstElf[0]) <= Number.parseInt(secondElf[1]);
    let d = Number.parseInt(firstElf[1]) >= Number.parseInt(secondElf[0]);

    if ((a <= 0 && b >= 0) || (a >= 0 && b <= 0)) countPart1++;

    if (c && d) countPart2++;
  });

  return {
    part1Result: countPart1,
    part2Result: countPart2,
  };
}
