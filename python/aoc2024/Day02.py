def adjLvlDiff(report):
  return all([1 <= abs(report[i] - report[i + 1]) <= 3 for i in range(len(report) - 1)])


def getDir(a, b):
  return 'inc' if a < b else 'dec'


def isMonotonic(report):
  initDir = getDir(report[0], report[1])

  def sameDir(a, b):
    return getDir(a, b) == initDir
  return all([sameDir(report[i], report[i + 1]) for i in range(1, len(report) - 1)])


def isReportSafe(report):
  return isMonotonic(report) and adjLvlDiff(report)


def isReportVarientSafe(report):
  return any([isReportSafe(report[:i] + report[i + 1:]) for i in range(len(report))])


# Main prog
with open("input.txt", 'r') as infile:
  data = infile.read().splitlines()

reports = [[int(lvl) for lvl in reportStr.split()] for reportStr in data]

safeReports = [r for r in reports if isReportSafe(r)]
print("part1: " + str(len(safeReports)))

toleratedReports = [r for r in reports if isReportVarientSafe(r)]
print("part2: " + str(len(toleratedReports)))
