"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/5

export function day5() {
  let data = getQuestionData("https://adventofcode.com/2022/day/5/input");
  return data
    .then((response) => {
      let stacksArray = response.data.split("\n");
      let result = processStacks(stacksArray);
      return {
        day5Part1: result.part1Result,
        day5Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 5 answer" + error);
    });
}

function test() {
  let testData = [
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2",
  ];

  return processStacks(testData);
}

function processStacks(stacksArray) {
  let stacksInput = [];
  let rules = [];
  let found = false;
  stacksArray.forEach((element) => {
    if (element === "") {
      found = true;
      return;
    }
    if (found) rules.push(element);
    else stacksInput.push(element);
  });
  let stacksPart1 = parseStacksInput(stacksInput);
  let stacksPart2 = parseStacksInput(stacksInput);

  rules.forEach((rule) => {
    let parsedRule = rule.split(" ");
    let quantity = Number.parseInt(parsedRule[1]);
    let fromStack = Number.parseInt(parsedRule[3]) - 1;
    let toStack = Number.parseInt(parsedRule[5]) - 1;

    for (let index = 0; index < quantity; index++) {
      stacksPart1[toStack].push(stacksPart1[fromStack].pop());
    }

    let toMove = [];
    for (let index = 0; index < quantity; index++) {
      toMove.push(stacksPart2[fromStack].pop());
    }
    stacksPart2[toStack].push(...toMove.reverse());
  });

  let stackTopsPart1 = "";
  stacksPart1.forEach((stack) => {
    stackTopsPart1 += stack[stack.length - 1];
  });

  let stackTopsPart2 = "";
  stacksPart2.forEach((stack) => {
    stackTopsPart2 += stack[stack.length - 1];
  });

  return {
    part1Result: stackTopsPart1,
    part2Result: stackTopsPart2,
  };
}

function parseStacksInput(stacksInput) {
  let parsedStacks = [];
  let numStacks = (stacksInput[0].length + 1) / 4;
  for (let stackIndex = 0; stackIndex < numStacks; stackIndex++)
    parsedStacks.push([]);

  for (let index = stacksInput.length - 1; index >= 0; index--) {
    if (index == stacksInput.length - 1) continue; // skip row

    const elementArray = stacksInput[index].split("");

    for (let charIndex = 0; charIndex < elementArray.length; charIndex++) {
      if (elementArray[charIndex] === "[") {
        let stackIndex = charIndex / 4;
        parsedStacks[stackIndex].push(elementArray[charIndex + 1]);
      }
    }
  }
  return parsedStacks;
}

// INPUT                                    LEN   []    Stack
// ----------------------------------------------------------
// [W] [T] [P] [J] [C] [G] [W] [P] [J]      35    17    9
// [W] [T] [P] [J] [C] [G] [W] [P]          31    15    8
// [W] [T] [P] [J] [C] [G] [W]              27    13    7
// [W] [T] [P] [J] [C] [G]                  23    11    6
// [W] [T] [P] [J] [C]                      19    9     5
// [W] [T] [P] [J]                          15    7     4
// [W] [T] [P]                              11    5     3
// [W] [T]                                  7     3     2
// [W]                                      3     1     1

// input    [W] [T] [P] [J]
// index     1   5   9   13
// bucket    0   2   3   4
// len = 15
