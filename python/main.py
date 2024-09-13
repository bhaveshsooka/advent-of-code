def solve_day(day_module):
    try:
        return day_module.solve()
    except Exception as error:
        # print(error)
        return {"part1": "No impl found", "part2": "No impl found"}


def main():
    years = [
        2015,
        2016,
        2017,
        2018,
        2019,
        2020,
        2021,
        2022,
        2023,
        2024,
    ]  # Add the years you want to process
    N = 25

    for year in years:
        print(f"Processing Advent of Code for the year {year}...")

        for i in range(1, N + 1):
            answer = {
                "sampleDataPart1": "",
                "sampleDataPart2": "",
                "part1": "",
                "part2": "",
            }
            question = str(i).zfill(2)
            try:
                day_module = __import__(
                    f"aoc_{year}.days.day{question}", fromlist=["solve"]
                )

                answer = solve_day(day_module)
            except ImportError:
                answer["part1"] = "No impl found"
                answer["part2"] = "No impl found"

            print(
                f"Year {year} - Day {question} - Sample Part 1: {answer['sampleDataPart1']}"
            )
            print(
                f"Year {year} - Day {question} - Sample Part 2: {answer['sampleDataPart2']}"
            )
            print(f"Year {year} - Day {question} - Part 1: {answer['part1']}")
            print(f"Year {year} - Day {question} - Part 2: {answer['part2']}")
            print()


if __name__ == "__main__":
    main()
