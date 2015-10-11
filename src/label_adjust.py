# in the diversifying transformation, BB_XX label is inserted to the beginning
# of certain instructions. However, as they are inserted in front of function
# label, function label would not be assembled into binary. For example:
#
# BB_xxx:
# S_0xxxx:  -> this is a function name
# mov ...
#
# after assembling and disassembing on the unstripped binary, only BB_xxx can
# be found
#
# <BB_xxx>:
# mov ...
#
# we want to keep the function label instead of BB label, so let's adjust it in
# this way
# S_0xxxx:
# BB_xxx:
# mov ...


import re


lines = []
with open('faddr.txt') as f:
    lines = f.readlines()

funcs = map(lambda l:int(l,16), lines)

lines = []
with open('final.s') as f:
    lines = f.readlines()


regex = re.compile(r'S_(0x[0-9A-F]{7}):', re.I)

is_text = False
for i in range(len(lines)):
    l = lines[i]
    m = regex.search(l)
    if m:
        d = m.groups()[0]
        d1 = int(d,16)
        if d1 in funcs:
            # we found one
            #pl = lines[i-1]
            #if "BB_" in pl:
            #    lines[i] = pl
            #    lines[i-1] = l
            lines[i] = ".globl S_"+ d + "\n" + l

    elif ".section" in l and is_text == False:
        is_text = True
    elif ".section" in l and is_text == True:
        break

with open('final.s', 'w') as f:
    f.writelines(lines)
