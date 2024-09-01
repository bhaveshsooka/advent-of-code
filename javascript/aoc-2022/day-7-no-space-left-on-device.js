"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/7

export function day7() {
  let data = getQuestionData("https://adventofcode.com/2022/day/7/input");
  return data
    .then((response) => {
      let terminalOutputArray = response.data.split("\n");
      let result = processTerminalOutputArray(terminalOutputArray);
      return {
        day7Part1: result.part1Result,
        day7Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 7 answer" + error);
    });
}

function test() {
  let testData = [
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k",
  ];

  return processTerminalOutputArray(testData);
}

function processTerminalOutputArray(terminalOutputArray) {
  let directories = {};
  let currentDirFullPath = [];
  terminalOutputArray.forEach((outputLine) => {
    if (outputLine === "") return;
    if (outputLine[0] === "$") {
      let commandArgs = outputLine.split(" ");
      if (commandArgs[1] === "cd") {
        if (commandArgs[2] === "..") {
          currentDirFullPath.pop();
        } else {
          currentDirFullPath.push(commandArgs[2]);
          directories[currentDirFullPath.join("-")] = { size: 0, subDirs: [] };
        }
      }
    } else {
      let outputArgs = outputLine.split(" ");
      let currentDir = currentDirFullPath.join("-");

      if (outputArgs[0] === "dir") {
        directories[currentDir].subDirs.push(
          currentDirFullPath.join("-") + "-" + outputArgs[1]
        );
      } else {
        let size = Number.parseInt(outputArgs[0]);
        directories[currentDir].size += size;
      }
    }
  });

  let state = JSON.parse(JSON.stringify(directories));
  let rootSpace = calcDirsSize(state, "/", 0);
  let minReq = 30000000 - (70000000 - rootSpace);

  let part1Sum = 0;
  let part2Min = 70000000;
  for (let key in directories) {
    let state = JSON.parse(JSON.stringify(directories));
    let dirSize = calcDirsSize(state, key, 0);
    if (dirSize <= 100000) {
      part1Sum += dirSize;
    }

    if (dirSize >= minReq && dirSize <= part2Min) part2Min = dirSize;
  }

  return {
    part1Result: part1Sum,
    part2Result: part2Min,
  };
}

function calcDirsSize(state, key, accSum) {
  if (state[key].subDirs.length == 0) return state[key].size + accSum;

  let nextKey = state[key].subDirs.pop();
  let nextKeySize = calcDirsSize(state, nextKey, accSum);

  return calcDirsSize(state, key, nextKeySize);
}
