"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/6

export function day6() {
  let data = getQuestionData("https://adventofcode.com/2022/day/6/input");
  return data
    .then((response) => {
      let signalsArray = response.data.split("\n");
      let result = processSignalsArray(signalsArray);
      return {
        day6Part1: result.part1Result,
        day6Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 6 answer" + error);
    });
}

function test() {
  let testData = [
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",
  ];

  return processSignalsArray(testData);
}

function processSignalsArray(signalsArray) {
  let startOfPacketIndexes = [];
  let startOfMessageIndexes = [];

  signalsArray.forEach((signal) => {
    startOfPacketIndexes.push(findNonRepeatedStringIndices(signal, 4));
    startOfMessageIndexes.push(findNonRepeatedStringIndices(signal, 14));
  });

  return {
    part1Result: startOfPacketIndexes.join(","),
    part2Result: startOfMessageIndexes.join(","),
  };
}

function findNonRepeatedStringIndices(string, markerLength) {
  let array = string.split("");
  let repeatedIndices = [];
  let foundIndex = -1;
  for (let index = 0; index < array.length - markerLength - 1; index++) {
    let marker = string.substring(index, index + markerLength);

    if (!hasRepeats(marker)) {
      foundIndex = index + markerLength;
      break;
    }
  }
  if (foundIndex != -1) {
    repeatedIndices.push(foundIndex);
  }

  return repeatedIndices;
}

// Not my function, but very cool
function hasRepeats(str) {
  return /(.).*\1/.test(str);
}
