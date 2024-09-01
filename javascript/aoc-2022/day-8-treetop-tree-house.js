"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/8

export function day8() {
  let data = getQuestionData("https://adventofcode.com/2022/day/8/input");
  return data
    .then((response) => {
      let treesArray = response.data.split("\n");
      let result = processTreesArray(treesArray);
      return {
        day8Part1: result.part1Result,
        day8Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 8 answer" + error);
    });
}

function test() {
  let testData = ["30373", "25512", "65332", "33549", "35390"];

  return processTreesArray(testData);
}

function processTreesArray(treesArray) {
  let forest = [];
  treesArray.forEach((treeRow) => {
    if(treeRow == "") return ;
    let rowArray = treeRow.split("");
    let iRowArray = [];
    rowArray.forEach((element) => {
      let i = Number.parseInt(element);
      iRowArray.push(i);
    });
    forest.push(iRowArray);
  });

  let numVisibleTrees = 0;
  let maxScenicScore = 0;

  for (let rowIndex = 0; rowIndex < forest.length; rowIndex++) {
    const row = forest[rowIndex];
    for (let columnIndex = 0; columnIndex < row.length; columnIndex++) {
      // const tree = forest[rowIndex][columnIndex];

      if (rowIndex == 0 || rowIndex == forest.length - 1) {
        numVisibleTrees++;
        continue;
      } else if (columnIndex == 0 || columnIndex == row.length - 1) {
        numVisibleTrees++;
        continue;
      } else {
        let visibilityFromOutside = checkVisibleOutside(
          forest,
          rowIndex,
          columnIndex
        );
        if (
          visibilityFromOutside.top ||
          visibilityFromOutside.bottom ||
          visibilityFromOutside.left ||
          visibilityFromOutside.right
        )
          numVisibleTrees++;

        let visibilityFromInside = checkVisibleInside(
          forest,
          rowIndex,
          columnIndex
        );

        let scenicScore =
          visibilityFromInside.left *
          visibilityFromInside.right *
          visibilityFromInside.top *
          visibilityFromInside.bottom;

        if (scenicScore > maxScenicScore) maxScenicScore = scenicScore;
      }
    }
  }

  return {
    part1Result: numVisibleTrees,
    part2Result: maxScenicScore,
  };
}

function checkVisibleOutside(forest, treeRow, treeColumn) {
  let visibleFrom = {
    top: true,
    right: true,
    bottom: true,
    left: true,
  };

  let treeInQuestion = forest[treeRow][treeColumn];

  // check row
  for (let colIndex = 0; colIndex < forest[0].length; colIndex++) {
    if (treeColumn == colIndex) continue;

    let currentTree = forest[treeRow][colIndex];

    if (currentTree >= treeInQuestion && treeColumn < colIndex)
      visibleFrom.left = false;
    else if (currentTree >= treeInQuestion && treeColumn > colIndex)
      visibleFrom.right = false;
  }

  // check col
  for (let rowIndex = 0; rowIndex < forest.length; rowIndex++) {
    if (treeRow == rowIndex) continue;

    let currentTree = forest[rowIndex][treeColumn];

    if (currentTree >= treeInQuestion && treeRow < rowIndex)
      visibleFrom.top = false;
    else if (currentTree >= treeInQuestion && treeRow > rowIndex)
      visibleFrom.bottom = false;
  }

  return visibleFrom;
}

function checkVisibleInside(forest, treeRow, treeColumn) {
  let visibleTrees = {
    top: 0,
    right: 0,
    bottom: 0,
    left: 0,
  };

  let treeInQuestion = forest[treeRow][treeColumn];

  // check left
  for (let colIndex = treeColumn - 1; colIndex >= 0; colIndex--) {
    let currentTree = forest[treeRow][colIndex];
    if (currentTree >= treeInQuestion) {
      visibleTrees.left++;
      break;
    } else {
      visibleTrees.left++;
    }
  }
  // check right
  for (let colIndex = treeColumn + 1; colIndex < forest[0].length; colIndex++) {
    let currentTree = forest[treeRow][colIndex];
    if (currentTree >= treeInQuestion) {
      visibleTrees.right++;
      break;
    } else {
      visibleTrees.right++;
    }
  }
  // check top
  for (let rowIndex = treeRow - 1; rowIndex >= 0; rowIndex--) {
    let currentTree = forest[rowIndex][treeColumn];
    if (currentTree >= treeInQuestion) {
      visibleTrees.top++;
      break;
    } else {
      visibleTrees.top++;
    }
  }
  // check bottom
  for (let rowIndex = treeRow + 1; rowIndex < forest.length; rowIndex++) {
    let currentTree = forest[rowIndex][treeColumn];
    if (currentTree >= treeInQuestion) {
      visibleTrees.bottom++;
      break;
    } else {
      visibleTrees.bottom++;
    }
  }

  return visibleTrees;
}
