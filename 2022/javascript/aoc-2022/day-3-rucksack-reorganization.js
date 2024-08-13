"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/3

export function day3() {
  let data = getQuestionData("https://adventofcode.com/2022/day/3/input");
  return data
    .then((response) => {
      let rucksacksArray = response.data.split("\n");
      let result = processRucksacksArray(rucksacksArray);
      return {
        day3Part1: result.part1Result,
        day3Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 3 answer" + error);
    });
}

function test() {
  let testData = [
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw",
  ];
  return processRucksacksArray(testData);
}

function processRucksacksArray(rucksacksArray) {
  let sumPart1 = 0;
  let sumPart2 = 0;
  let groupCount = 0;
  let group = [];
  rucksacksArray.forEach((rucksack) => {
    if (rucksack.trim() == "") return;
    groupCount++;
    group.push(rucksack);

    let prioritySingle = getPriorityScore(checkSingleRucksack(rucksack));
    sumPart1 += prioritySingle;

    if (groupCount == 3) {
      let priorityGroup = getPriorityScore(
        checkGroupRucksack(group, group.join(""))
      );
      sumPart2 += priorityGroup;
      groupCount = 0;
      group = [];
    }
  });
  return {
    part1Result: sumPart1,
    part2Result: sumPart2,
  };
}

function checkSingleRucksack(rucksack) {
  let rucksackItems = rucksack.split("");

  let compartment1 = rucksack.substring(0, rucksack.length / 2);
  let compartment2 = rucksack.substring(rucksack.length / 2, rucksack.length);

  let flag = "";
  rucksackItems.forEach((item) => {
    if (compartment1.includes(item) && compartment2.includes(item)) {
      flag = item;
    }
  });
  return flag;
}

function checkGroupRucksack(rucksacks, rucksacksJoined) {
  let allRucksackItems = rucksacksJoined.split("");

  let duplicateItem = "";
  allRucksackItems.forEach((item) => {
    let flaggedItem = item;
    rucksacks.forEach((rucksack) => {
      if (!rucksack.includes(flaggedItem)) {
        flaggedItem = "";
      }
    });
    if (flaggedItem != "") {
      duplicateItem = flaggedItem;
      return;
    }
  });
  return duplicateItem;
}

function getPriorityScore(item) {
  let itemTypes = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  let idx = itemTypes.indexOf(item);
  return idx + 1;
}
