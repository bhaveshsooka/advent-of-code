from random import randint

with open("input.txt", 'r') as infile:
  data = infile.readlines()

# with open("input-test1.txt", 'r') as infile:
#   data = infile.readlines()

data = [int(x) for x in data]

sum = 0
for s in data:
  sum += int(s)
print(sum)

trackingSums = {}
sum = 0
idx = 0
while True:
  if sum in trackingSums.keys():
    trackingSums[sum] += 1
  else:
    trackingSums[sum] = 1
  if trackingSums[sum] > 1:
    break
  sum += data[idx]
  idx = (idx + 1) % len(data)

print(sum)
