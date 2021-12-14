import os
import re

lines = []

with open("final.s") as f:
    lines = f.readlines()

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "ELF 64-bit" in lines[0]:
        return False
    else:
        return True

is_32 = check_32()

ll = len(lines)
main_symbol = ""

find_text = False

if is_32:
    for i in range(ll):
        m = re.search(r'jmp\s+\*(%e\w{2})',lines[i])
        if m:
            if re.search(r'add\s+%e\w{2},'+m.group(1),lines[i-1]):
                lines[i-1] = "nop\n"
            elif re.search(r'add\s+%e\w{2},'+m.group(1),lines[i-2]):
                lines[i-2] = "nop\n"
else:
    for i in range(ll):
        m = re.search(r'jmpq\s+\*(%r\w{2})',lines[i])
        if m and re.search(r'add\s+%r\w{2},'+m.group(1),lines[i-1]):
            lines[i-1] = "nop\n"

pre_symbol = ''
pre_idx = -1
for i in range(ll):
    l = lines[i]
    if l.startswith('BB_'):
        if len(lines[i+1].strip()) == 0 or lines[i+1].startswith('S_0x'):
            lines[i] = ""
    if l.startswith('S_0x'):
        if l == pre_symbol:
            lines[pre_idx] = ''
        pre_symbol = l
        pre_idx = i


for i in range(ll):
    l = lines[i]
    if ".text" in l:
        if find_text == False:
            find_text = True
        else:
            l = l.replace(".text:","")
    if "lea 0x0(%esi," in l:
	if ':' in l:
	    label = l.split(':')[0]   # label:  lea 0x0....
            l = label + " : nop;nop;nop;nop;nop;nop;nop;\n"
	else:
            l = "nop;nop;nop;nop;nop;nop;nop;\n"
    elif "lea 0x0(%edi," in l:
	if ':' in l:
	    label = l.split(':')[0]   # label:  lea 0x0....
            l = label + " : nop;nop;nop;nop;nop;nop;nop;\n"
	else:
	    label = ""
            l = "nop;nop;nop;nop;nop;nop;nop;\n"
    # __gmon_start__ symbol is resolved by the linked program itself, it surely can not be resolved
    # in our final.s code, just remove it
    elif "__gmon_start__" in l:
        l = ""
    elif "lea 0x7FFFFFFC(,%ebx,0x4),%edi" in l:
        l = l.replace('0x7FFFFFFC', '0x7FFFFFFFFFFFFFFC')
    elif "repz retq" in l:
        l = l.replace("repz retq", "repz\nretq\n")
    elif "repz ret" in l:
        l = l.replace("repz ret", "repz\nret\n")
    elif "repz pop" in l:
        l = l.replace("repz pop", "repz\npop")
    elif "movzbl $S_" in l:
        l =  l.replace("movzbl $S_","movzbl S_")
    #  Warning: indirect jmp without `*'
    # the exe crashes at this instruction
    # adjust it into jmp S_0x4006C1
    elif "jmpq " in l and "*" not in l:
        l = l.replace('jmpq ', 'jmp ')
    elif "__libc_start_main" in l and is_32 == True:
	main_symbol = lines[i-1].split()[1]
	lines[i-1] = lines[i-1].replace(main_symbol, "main")
	main_symbol = main_symbol[1:].strip()
    elif is_32 == False and "__libc_start_main" in l:
        main_symbol = lines[i-1].split()[-1].split(',')[0]
	lines[i-1] = lines[i-1].replace(main_symbol, "main")
	main_symbol = main_symbol[1:].strip()
	#print main_symbol

## Some of the PIC code/module rely on typical pattern to locate
## such as:

##	804c460: push   %ebx
##	804c461: call   804c452 <__i686.get_pc_thunk.bx>
##	804c466: add    $0x2b8e,%ebx
##	804c46c: sub    $0x18,%esp

## What we can do this pattern match `<__i686.get_pc_thunk.bx>` and calculate
## the address by plusing 0x2b8e and  0x804c466, which equals to the begin address of GOT.PLT table

## symbols can be leveraged in re-assemble are
##	_GLOBAL_OFFSET_TABLE_   ==    ** .got.plt **
##	....


    lines[i] = l


def check_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "LSB shared object" in lines[0]:
        return False
    else:
        return True

is_exe = check_exe()

if is_exe == False: # share library
    pass
else:
    mains = []
    main_symbol1 = ""

    with open('main.info') as f:
        mains = f.readlines()

    main_symbol1 = mains[0].strip()


    def help(l):
        if main_symbol1 != "" and main_symbol1 in l and l.startswith(main_symbol1):
    	    l = ".globl main\nmain:\n"+l
        return l
    #print lines
    lines = map(lambda l : help(l), lines)

#branch_routine :pop global_des
#jmp *branch_des
    #lines.append('switch_bb: jmp *branch_des\n')


with open("final.s", 'w') as f:
    map(lambda l : f.write(l), lines)


if os.path.isfile('inline_symbols.txt'):
    os.system('python inline_update.py')
