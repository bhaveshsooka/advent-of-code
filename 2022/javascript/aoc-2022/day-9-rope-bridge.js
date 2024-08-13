"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/9

export function day9() {
  let data = getQuestionData("https://adventofcode.com/2022/day/9/input");
  return data
    .then((response) => {
      let motionsArray = response.data.split("\n");
      let result = processMovesArray(motionsArray);
      return {
        day9Part1: result.part1Result,
        day9Part2: result.part2Result,
        test: test(),
        test2: test2(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 9 answer" + error);
    });
}

function test() {
  let testData = ["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"];

  return processMovesArray(testData);
}

function test2() {
  let testData = ["R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20"];

  return processMovesArray(testData);
}

function processMovesArray(array) {
  // init state
  let snakeTails = 9;
  let snakeState = {
    headPath: [[11, 5]],
    tailPaths: [],
  };
  for (let i = 0; i < snakeTails; i++) snakeState.tailPaths.push([[11, 5]]);

  for (let index = 0; index < array.length; index++) {
    let motion = array[index].split(" ");

    let direction = motion[0];
    let num = Number.parseInt(motion[1]);

    processMove(snakeState, direction, num);
  }

  let behindHeadVisited = countVisited(snakeState.tailPaths[0]);
  let tailVisited = countVisited(
    snakeState.tailPaths[snakeState.tailPaths.length - 1]
  );

  return {
    part1Result: behindHeadVisited,
    part2Result: tailVisited,
  };
}

function countVisited(array) {
  let visited = [];
  array.forEach((element) => {
    let item = element.join("-");
    if (!visited.includes(item)) visited.push(item);
  });

  return visited.length;
}

function processMove(snakeState, direction, num) {
  // Move all snake pieces this many times (including the head)
  for (let i = 0; i < num; i++) {
    // get the new head
    let headPath = snakeState.headPath;
    let currentHeadX = headPath[headPath.length - 1][0];
    let currentHeadY = headPath[headPath.length - 1][1];
    let newHead = calcNextHead(direction, currentHeadX, currentHeadY);
    snakeState.headPath.push(newHead);

    // get the paths for each tail
    for (let tailIdx = 0; tailIdx < snakeState.tailPaths.length; tailIdx++) {
      // get current tail info
      let currentTailPath = snakeState.tailPaths[tailIdx];
      let currentTailX = currentTailPath[currentTailPath.length - 1][0];
      let currentTailY = currentTailPath[currentTailPath.length - 1][1];

      // cal new tail
      let newTail = calcNextTail(
        direction,
        currentTailX,
        currentTailY,
        newHead[0],
        newHead[1]
      );

      // update
      newHead = newTail
      snakeState.tailPaths[tailIdx].push(newTail);
    }
  }

  return snakeState;
}

function calcNextHead(direction, currentHeadX, currentHeadY) {
  switch (direction) {
    case "R":
      return [currentHeadX + 1, currentHeadY];
    case "L":
      return [currentHeadX - 1, currentHeadY];
    case "U":
      return [currentHeadX, currentHeadY + 1];
    case "D":
      return [currentHeadX, currentHeadY - 1];
    default:
      return [currentHeadX, currentHeadY];
  }
}

function calcNextTail(dir, currentTailX, currentTailY, newHeadX, newHeadY) {
  let xDiff = Math.abs(currentTailX - newHeadX);
  let yDiff = Math.abs(currentTailY - newHeadY);

  if (xDiff <= 1 && yDiff <= 1) return [currentTailX, currentTailY];

  let newTailX = align(currentTailX, newHeadX);
  let newTailY = align(currentTailY, newHeadY);
  return [newTailX, newTailY];
}

function align(toAlign, alignTo) {
  if (toAlign < alignTo) return toAlign + 1;
  else if (toAlign > alignTo) return toAlign - 1;
  else return toAlign;
}
