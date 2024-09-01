"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/11

export function day11() {
  let data = getQuestionData("https://adventofcode.com/2022/day/11/input");
  return data
    .then((response) => {
      let notesArray = response.data.split("\n");
      let result = processNotesArray(notesArray);
      return {
        day11Part1: result.part1Result,
        day11Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 11 answer" + error);
    });
}

function test() {
  let testData = [
    "Monkey 0:",
    "  Starting items: 79, 98",
    "  Operation: new = old * 19",
    "  Test: divisible by 23",
    "    If true: throw to monkey 2",
    "    If false: throw to monkey 3",
    "",
    "Monkey 1:",
    "  Starting items: 54, 65, 75, 74",
    "  Operation: new = old + 6",
    "  Test: divisible by 19",
    "    If true: throw to monkey 2",
    "    If false: throw to monkey 0",
    "",
    "Monkey 2:",
    "  Starting items: 79, 60, 97",
    "  Operation: new = old * old",
    "  Test: divisible by 13",
    "    If true: throw to monkey 1",
    "    If false: throw to monkey 3",
    "",
    "Monkey 3:",
    "  Starting items: 74",
    "  Operation: new = old + 3",
    "  Test: divisible by 17",
    "    If true: throw to monkey 0",
    "    If false: throw to monkey 1",
  ];

  return processNotesArray(testData);
}

function processNotesArray(notesArray) {
  // Part 1
  let monkeysStatePart1 = simulateRounds(parseInput(notesArray), 20, 3n);
  monkeysStatePart1.sort((a, b) => b.numItemsInspected - a.numItemsInspected);
  let numItemsInspectedWithRelief =
    monkeysStatePart1[0].numItemsInspected *
    monkeysStatePart1[1].numItemsInspected;

  // Part 2
  let monkeysStatePart2 = simulateRounds(parseInput(notesArray), 10000, 1n);
  monkeysStatePart2.sort((a, b) => b.numItemsInspected - a.numItemsInspected);
  let numItemsInspectedWithoutRelief =
    monkeysStatePart2[0].numItemsInspected *
    monkeysStatePart2[1].numItemsInspected;

  return {
    part1Result: numItemsInspectedWithRelief,
    part2Result: numItemsInspectedWithoutRelief,
  };
}

function simulateRounds(monkeysState, rounds, reliefFactor) {
  let round = 1;
  while (round <= rounds) {
    for (let monkeyIdx = 0; monkeyIdx < monkeysState.length; monkeyIdx++) {
      let monkeyWorryFunction = monkeysState[monkeyIdx].worryFunction;
      let monkeyThrowFunction = monkeysState[monkeyIdx].throwFunction;

      while (monkeysState[monkeyIdx].items.length > 0) {
        let item = monkeysState[monkeyIdx].items.shift();

        // Calc new worry level
        let newItemWorryLevel = monkeyWorryFunction(item);
        newItemWorryLevel = applyRelief(newItemWorryLevel, reliefFactor);

        // normalize
        newItemWorryLevel = newItemWorryLevel % monkeysState[monkeyIdx].lcm;

        // Calc who to throw to
        let destinationMonkey = monkeyThrowFunction(newItemWorryLevel);

        // Update state
        monkeysState[destinationMonkey].items.push(newItemWorryLevel);
        monkeysState[monkeyIdx].numItemsInspected++;
      }
    }

    round++;
  }

  return monkeysState;
}

function applyRelief(worryLevel, reduceBy) {
  return divide(worryLevel, reduceBy);
}

function parseInput(array) {
  let lcm = 1n;
  let state = [];
  for (let index = 0; index < array.length - 1; index += 7) {
    let idArray = splitLine(array[index]);
    let itemsArray = splitLine(array[index + 1]);
    let operationArray = splitLine(array[index + 2]);
    let throwFnArray = splitLine(array[index + 3]);
    let throwTrueArray = splitLine(array[index + 4]);
    let throwFalseArray = splitLine(array[index + 5]);

    let monkeyState = {
      index: idArray[0][1],
      items: itemsArray[1],
      numItemsInspected: 0,
      lcm: -1n,
      worryFunction: getWorryFunction(operationArray[1]),
      throwFunction: getThrowFunction(
        throwFnArray[1],
        throwTrueArray[1][3],
        throwFalseArray[1][3]
      ),
    };

    lcm *= throwFnArray[1][2];

    state.push(monkeyState);
  }

  state.map((e) => (e.lcm = lcm));

  return state;
}

function getThrowFunction(throwFnArray, monkeyThrowTrue, monkeyThrowFalse) {
  switch (throwFnArray[0]) {
    case "divisible":
      return (worryLevel) =>
        mod(worryLevel, throwFnArray[2]) == 0n
          ? monkeyThrowTrue
          : monkeyThrowFalse;
  }
}

function getWorryFunction(stressFunctionArray) {
  let val1 = stressFunctionArray[2];
  let val2 = stressFunctionArray[4];
  let sOperation = stressFunctionArray[3];

  switch (sOperation) {
    case "+":
      return getCurriedFunction(val1, val2, add);
    case "-":
      return getCurriedFunction(val1, val2, subtract);
    case "*":
      return getCurriedFunction(val1, val2, multiply);
    case "/":
      return getCurriedFunction(val1, val2, divide);
  }
}

function getCurriedFunction(val1, val2, opFunction) {
  if (val2 === "old") return (a) => opFunction(a, a);
  else return (a) => opFunction(a, val2);
}

function add(val1, val2) {
  return val1 + val2;
}
function subtract(val1, val2) {
  return val1 - val2;
}
function multiply(val1, val2) {
  return val1 * val2;
}
function divide(val1, val2) {
  return val1 / val2;
}
function mod(val1, val2) {
  return val1 % val2;
}

function splitLine(line) {
  return line.split(":").map((section) =>
    section
      .split(" ")
      .filter((el) => el !== "")
      .map((el) => el.replace(",", ""))
      .map((el) => convertIfNumber(el))
  );
}

function convertIfNumber(string) {
  let converted;
  try {
    converted = BigInt(string);
  } catch (e) {
    converted = string;
  }
  return converted;
}
