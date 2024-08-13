"use strict";

import { day1 } from "./aoc-2022/day-1-calorie-counting.js";
import { day10 } from "./aoc-2022/day-10-cathode-ray-tube.js";
import { day11 } from "./aoc-2022/day-11-monkey-in-the-middle.js";
import { day12 } from "./aoc-2022/day-12-hill-climbing-algorithm.js";
import { day2 } from "./aoc-2022/day-2-rock-paper-scissors.js";
import { day3 } from "./aoc-2022/day-3-rucksack-reorganization.js";
import { day4 } from "./aoc-2022/day-4-camp-cleanup.js";
import { day5 } from "./aoc-2022/day-5-supply-stacks.js";
import { day6 } from "./aoc-2022/day-6-tuning-trouble.js";
import { day7 } from "./aoc-2022/day-7-no-space-left-on-device.js";
import { day8 } from "./aoc-2022/day-8-treetop-tree-house.js";
import { day9 } from "./aoc-2022/day-9-rope-bridge.js";

let answers = [];

await day1().then((answer) => answers.push(answer));
await day2().then((answer) => answers.push(answer));
await day3().then((answer) => answers.push(answer));
await day4().then((answer) => answers.push(answer));
await day5().then((answer) => answers.push(answer));
await day6().then((answer) => answers.push(answer));
await day7().then((answer) => answers.push(answer));
await day8().then((answer) => answers.push(answer));
await day9().then((answer) => answers.push(answer));
await day10().then((answer) => answers.push(answer));
await day11().then((answer) => answers.push(answer));
await day12().then((answer) => answers.push(answer));

console.log(answers);
