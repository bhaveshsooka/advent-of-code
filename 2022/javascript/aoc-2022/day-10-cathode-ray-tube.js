"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/10

export function day10() {
  let data = getQuestionData("https://adventofcode.com/2022/day/10/input");
  return data
    .then((response) => {
      let instructionsArray = response.data.split("\n");
      let result = processInstructionsArray(instructionsArray);
      return {
        day10Part1: result.part1Result,
        day10Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 10 answer" + error);
    });
}

function test() {
  let testData = [
    "addx 15",
    "addx -11",
    "addx 6",
    "addx -3",
    "addx 5",
    "addx -1",
    "addx -8",
    "addx 13",
    "addx 4",
    "noop",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx -35",
    "addx 1",
    "addx 24",
    "addx -19",
    "addx 1",
    "addx 16",
    "addx -11",
    "noop",
    "noop",
    "addx 21",
    "addx -15",
    "noop",
    "noop",
    "addx -3",
    "addx 9",
    "addx 1",
    "addx -3",
    "addx 8",
    "addx 1",
    "addx 5",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx -36",
    "noop",
    "addx 1",
    "addx 7",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "addx 6",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx 7",
    "addx 1",
    "noop",
    "addx -13",
    "addx 13",
    "addx 7",
    "noop",
    "addx 1",
    "addx -33",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "noop",
    "noop",
    "noop",
    "addx 8",
    "noop",
    "addx -1",
    "addx 2",
    "addx 1",
    "noop",
    "addx 17",
    "addx -9",
    "addx 1",
    "addx 1",
    "addx -3",
    "addx 11",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx -13",
    "addx -19",
    "addx 1",
    "addx 3",
    "addx 26",
    "addx -30",
    "addx 12",
    "addx -1",
    "addx 3",
    "addx 1",
    "noop",
    "noop",
    "noop",
    "addx -9",
    "addx 18",
    "addx 1",
    "addx 2",
    "noop",
    "noop",
    "addx 9",
    "noop",
    "noop",
    "noop",
    "addx -1",
    "addx 2",
    "addx -37",
    "addx 1",
    "addx 3",
    "noop",
    "addx 15",
    "addx -21",
    "addx 22",
    "addx -6",
    "addx 1",
    "noop",
    "addx 2",
    "addx 1",
    "noop",
    "addx -10",
    "noop",
    "noop",
    "addx 20",
    "addx 1",
    "addx 2",
    "addx 2",
    "addx -6",
    "addx -11",
    "noop",
    "noop",
    "noop",
    "",
  ];

  return processInstructionsArray(testData);
}

function processInstructionsArray(instructionsArray) {
  let cycle = 0;
  let registerX = 1;
  let registerXHistory = [];
  let sprite = [registerX - 1, registerX, registerX + 1];
  let crtRow = "";
  let crtRows = [];

  let index = 20;
  let increment = 40;
  instructionsArray.forEach((instruction) => {
    if (instruction === "") return;

    let instructionArray = instruction.split(" ");

    if (instructionArray[0] === "noop") {
      crtRow += writePixel(sprite, crtRow);
      if (crtRow.length === 40) {
        crtRows.push(crtRow);
        crtRow = "";
      }
      registerXHistory.push(registerX);
      sprite = [registerX - 1, registerX, registerX + 1];
      cycle++;
    } else if (instructionArray[0] == "addx") {
      crtRow += writePixel(sprite, crtRow);
      if (crtRow.length === 40) {
        crtRows.push(crtRow);
        crtRow = "";
      }
      registerXHistory.push(registerX);
      sprite = [registerX - 1, registerX, registerX + 1];
      cycle++;

      registerX += Number.parseInt(instructionArray[1]);

      crtRow += writePixel(sprite, crtRow);
      if (crtRow.length === 40) {
        crtRows.push(crtRow);
        crtRow = "";
      }
      registerXHistory.push(registerX);
      sprite = [registerX - 1, registerX, registerX + 1];
      cycle++;
    } else return;
  });

  let sumSignalStrength = 0;
  let limit = 6;
  while (limit > 0) {
    let signalStrength = registerXHistory[index - 2];
    sumSignalStrength += signalStrength * index;
    index += increment;
    limit--;
  }

  return {
    part1Result: sumSignalStrength,
    part2Result: crtRows,
  };
}

function writePixel(sprite, crtRow) {
  if (sprite.includes(crtRow.length)) return "#";
  else return ".";
}
