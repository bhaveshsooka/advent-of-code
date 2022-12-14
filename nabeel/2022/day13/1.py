import sys
import math

file = sys.argv[1] if len(sys.argv)>1 else '0.in'
lines = open(file).readlines()
lines = [line.strip() for line in lines if len(line)>1]
# print(*lines, sep='\n')
ordered_idx = []

def LvR(left, right,tab=0):

    print(tab*'\t',f'Compare {left} vs {right}')

    for list_idx in range(len(left)):
        l = left[list_idx]
        try:
            r = right[list_idx]
        except:
            print(tab*'\t','Right side ran out of items, so inputs are in the wrong order')
            return False
        print(tab*'\t',f'Compare {left[list_idx]} vs {right[list_idx]}')

        if type(l) == int and type(r) == int:
            if l<r : 
                print(tab*'\t',"Left side is smaller, so inputs are in the right order")
                return True
            if l>r : 
                print(tab*'\t','Right side is smaller, so inputs are in the wrong order')
                return False
            if l==r: continue
        
        if type(l) == list and type(r) == list:
            decision = LvR(l,r,tab+1)
            if decision is not None:
                return decision
            
        if type(l) == int:
            print(tab*'\t',f'Mixed types; convert left to {[l]} and retry comparison')
            decision = LvR([l],r, tab+1)
            if decision is not None:
                return decision

        if type(r) == int:
            print(tab*'\t',f'Mixed types; convert right to {[r]} and retry comparison')
            decision = LvR(l,[r], tab+1)
            if decision is not None:
                return decision
    if len(left)<len(right):
        print(tab*'\t','Left side ran out of items, so inputs are in the right order')
        return True
    if left == right:
        print(tab*'\t','Lists are identical, check next input')
        return None



for pkt_idx in range(int(len(lines)/2)):
    print(f'\n\n== Pair {pkt_idx+1} ==')
    pair = lines[pkt_idx*2:(pkt_idx+1)*2]
    [left, right] = pair
    exec(f'left = {left}')
    exec(f'right = {right}')

    if LvR(left, right):
        ordered_idx.append(pkt_idx+1)


print(ordered_idx)
print(sum(ordered_idx))
