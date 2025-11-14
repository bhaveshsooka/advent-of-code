with open("input.txt", 'r') as infile:
  data = infile.read().splitlines()

equations = [(int(x.split(": ")[0]), [int(y)
              for y in x.split(": ")[1].split(" ")]) for x in data]


def solve(equation: tuple[int, list[int]], n: int) -> bool:
  solution, numbers = equation
  if (numbers == []):
    return solution == n
  x, xs = numbers[0], numbers[1:]
  return solve((solution, xs), n + x) or solve((solution, xs), n * x)


def solve2(equation: tuple[int, list[int]], n: int) -> bool:
  solution, numbers = equation
  if (numbers == []):
    return solution == n
  x, xs = numbers[0], numbers[1:]
  return (solve((solution, xs), n + x) or solve((solution, xs), n * x) or solve((solution, xs), int(str(x) + str(n))))


part1 = sum([x[0] for x in equations if solve(x, 0)])
part2 = sum([x[0] for x in equations if solve2(x, 0)])

print(part1)
print(part2)
