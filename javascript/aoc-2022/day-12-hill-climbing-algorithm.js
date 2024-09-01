"use strict";

import { getQuestionData } from "../common.js";

// https://adventofcode.com/2022/day/12

export function day12() {
  let data = getQuestionData("https://adventofcode.com/2022/day/12/input");
  return data
    .then((response) => {
      let heightMapArray = response.data.split("\n");
      let result = processHeightMapArray(heightMapArray);
      return {
        day12Part1: result.part1Result,
        day12Part2: result.part2Result,
        test: test(),
      };
    })
    .catch((error) => {
      console.log("Problem getting day 12 answer" + error);
      test();
    });
}

function test() {
  let testData = ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"];

  return processHeightMapArray(testData);
}

function processHeightMapArray(heightMapArray) {
  let map = parseInput(heightMapArray);
  let startCoord = findStart(map);
  let path = bfs(map, startCoord);

  let minAPath = findMinAPath(parseInput(heightMapArray), path);

  return {
    part1Result: path.length,
    part2Result: minAPath.length,
  };
}

function findMinAPath(map, minAPath) {
  let aStarts = findStartA(structuredClone(map));
  aStarts.forEach((aStartCoord) => {
    let aPath = bfs(structuredClone(map), aStartCoord);
    if (aPath == null) return;
    if (aPath.length < minAPath.length) minAPath = aPath;
  });

  return minAPath;
}

function bfs(map, startCoord) {
  let destination = null;
  let movesToCheck = [];
  let start = map[startCoord[0]][startCoord[1]];
  start.distance = 0;
  start.visited = true;
  movesToCheck.push(start);

  while (movesToCheck.length > 0) {
    let current = movesToCheck.shift();

    // found destination
    if (current.elevation == "E") {
      destination = current;
      break;
    }

    // check next moves
    checkNeighbours(map, current, movesToCheck);
  }

  return buildPath(destination);
}

function buildPath(destination) {
  if (destination == null) return null;

  let path = [];
  let p = destination;
  // path.unshift([p.x, p.y]); // include destination
  while ((p = p.parent) != null) {
    path.unshift([p.x, p.y]);
  }

  return path;
}

function checkNeighbours(map, current, movesToCheck) {
  let dirs = [
    [0, 1],
    [1, 0],
    [0, -1],
    [-1, 0],
  ];
  let row = current.x;
  let col = current.y;

  dirs.forEach((dir) => {
    let src = [row, col];
    let dest = [row + dir[0], col + dir[1]];

    if (!inBounds(map, dest)) return;
    if (seen(map, dest)) return;

    if (canStep(map, src, dest)) {
      let next = map[dest[0]][dest[1]];
      if (current.distance + 1 < next.distance) {
        next.distance = current.distance + 1;
        next.parent = current;
        next.visited = true;
        movesToCheck.push(next);
      }
    }
  });
}

function canStep(map, source, destination) {
  let sRow = source[0];
  let sCol = source[1];
  let dRow = destination[0];
  let dCol = destination[1];
  let sourceElevation = getElevationScore(map[sRow][sCol].elevation);
  let destinationElevation = getElevationScore(map[dRow][dCol].elevation);

  return destinationElevation <= sourceElevation + 1;
}

function seen(map, destination) {
  let dRow = destination[0];
  let dCol = destination[1];
  return map[dRow][dCol].visited;
}

function inBounds(map, destination) {
  let dRow = destination[0];
  let dCol = destination[1];
  return dRow >= 0 && dRow < map.length && dCol >= 0 && dCol < map[0].length;
}

function findStart(map) {
  for (const row in map)
    for (const col in map[row])
      if (map[row][col].elevation == "S")
        return [Number.parseInt(row), Number.parseInt(col)];
  return [0, 0];
}

function findStartA(map) {
  let paths = [];
  for (const row in map)
    for (const col in map[row])
      if (map[row][col].elevation === "a")
        paths.push([Number.parseInt(row), Number.parseInt(col)]);
  return paths;
}

function parseInput(input) {
  let map = [];
  for (let row = 0; row < input.length; row++) {
    if (input[row] == "") continue;
    map[row] = [];
    let rowSplit = input[row].split("");
    for (let col = 0; col < rowSplit.length; col++) {
      let data = {
        x: row,
        y: col,
        distance: Number.MAX_VALUE,
        parent: null,
        elevation: rowSplit[col],
        visited: false,
      };
      map[row][col] = data;
    }
  }
  return map;
}

function getElevationScore(elevation) {
  let elevationTypes = "abcdefghijklmnopqrstuvwxyz";

  if (elevation === "S") return elevationTypes.indexOf("a");
  if (elevation === "E") return elevationTypes.indexOf("z");
  return elevationTypes.indexOf(elevation);
}
