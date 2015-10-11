import sys

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "ELF 64-bit" in lines[0]:
        return False
    else:
        return True

is_32 = check_32()

if is_32 == False:
    pass
else:
    lines = []
    with open('instrs.info') as f:
        lines = f.readlines()

    for i in range(len(lines)):
        l = lines[i]
        l = l.strip()
        if 'nop' in l:
            items = l.split()
            if 'nop' == items[-1]:
                #l = l.split(':')[0] + " :"
                l = l

        lines[i] = l+"\n"


    with open('instrs.info', 'w') as f:
        f.writelines(lines)
