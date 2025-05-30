alphabetMap = {chr(i + ord('a')): i for i in range(26)}


def group(string: str, currentGroup: str, groups: list) -> list:
  if len(string) == 0:
    return groups
  if len(string) == 1:
    if string == currentGroup[-1]:
      groups.append(currentGroup + string)
    else:
      groups.append(currentGroup)
      groups.append(string)
    return groups
  head, tail = string[0], string[1:]
  if currentGroup == '':
    return group(tail, head, groups)
  if head not in currentGroup:
    groups.append(currentGroup)
    return group(tail, head, groups)
  else:
    return group(tail, str(currentGroup + head), groups)


def increasingStraight(password: str) -> bool:
  if len(password) <= 2:
    return False
  [first, second, third], tail = password[:3], password[3:]
  if (alphabetMap[first] + 1 == alphabetMap[second] and
          alphabetMap[second] + 1 == alphabetMap[third]):
    return True
  return increasingStraight(password[1:])


def hasPairs(password) -> bool:
  groups = group(password, '', [])
  groupsLen2 = [group for group in groups if len(group) == 2]
  return len(groupsLen2) >= 2


def hasForbidden(password) -> bool:
  return any(forbidden in password for forbidden in ['i', 'o', 'l'])


def isValid(password: str) -> bool:
  return (not hasForbidden(password) and
          hasPairs(password) and
          increasingStraight(password))


string = "vzbxkghb"
def nextPassword(password: str) -> str:
  password = list(password)
  i = len(password) - 1
  while i >= 0:
    if password[i] == 'z':
      password[i] = 'a'
      i -= 1
    else:
      password[i] = chr(ord(password[i]) + 1)
      break
  return ''.join(password)

def nextValidPassword(password: str) -> str:
  password = nextPassword(password)
  while not isValid(password):
    password = nextPassword(password)
  return password

print(nextValidPassword(string))
print(nextValidPassword(nextValidPassword(string)))

