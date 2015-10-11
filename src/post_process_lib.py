# this is a post modifying script of (shared) library code

# four tasks are handled:
#    1. callq xxxx <puts@plt> is transformed as callq puts in AIL, however, in
#    lib we have to use this format:
#            callq puts@plt
#    We parse the plts.info file and create a set, containing all the symbols
#    (function names) that should be rewritten in above format
#
#    2. some of the symbols should be removed. They are attached by the
#    original compile  process and they are rejected by the linker as they are
#    somehow not PIC symbols.
#    target symbol list:
#    __bss_start
#

#    3. we should scan the symbol table to collect all the exported symbols
#    (functions, global variables), and adding .globl macro for them
#
#
#    4. type macro should also be updated for each exported symbol.
#    for functions, insert a macro like this:  .type   func_name, @function
#    for variables , insert a macro like this:  .type   variable_name, @variable
#
#
#    5. when scanning the data sections (probably just .data section), identify
#    certain memory address label and substitue it with variable names
#    according to export datas.
#
#

import os, sys, re

def check_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "LSB shared object" in lines[0]:
        return False
    else:
        return True

is_exe = check_exe()


if is_exe == True: # executable
    pass
else:
    contents = []
    with open('final.s') as f:
        contents = f.readlines()

    def task1():
        # collect symbols
        global contents

        lines = []
        with open('plts.info') as f:
            lines = f.readlines()

        symbols = []
        pat = r"<(.*)@plt>"
        for l in lines:
            m = re.search(pat, l)
            if m:
                symbols.append(m.group(1))

        symbols = set(symbols)

        # rewrite
        pat = r"(callq|jmp)  *(.*)"
        for i in range(len(contents)):
            l = contents[i]
            m = re.search(pat, l)
            if m:
                fn = m.group(2)
                if fn in symbols: # we only transform these symbols
                    l = l.replace(fn, fn+"@plt")

            contents[i] = l


    def task2(): 
        global contents

        # passes for the reason of KISS
        for i in range(len(contents)):
            l = contents[i]
            if '__bss_start' in l:
                contents[i] = ""

    #  combine task3 and task4 as they both rely on symbol tables
    def task3_4():
        global contents

        export_sym = {}

        lines = []
        with open('export_tbl.info') as f:
            lines = f.readlines()

#      9: 0000000000201020     4 OBJECT  GLOBAL DEFAULT   22 d
        for l in lines:
            items = l.split()
            if 'NOTYPE' in items or 'LOCAL' in items or '@' in l:
                pass
            else:
                ty = items[3]
                nm = items[-1]
                if nm.startswith('_'):
                    pass
                else:
                    export_sym[nm] = ty

        globls = []
        types = []
        for nm, ty in export_sym.items():
            if ty == "FUNC":
                types.append('.type   ' + nm + ', @function\n')
            if ty == "OBJECT":
                types.append('.type   ' + nm + ', @object\n')

            globls.append('.globl ' + nm + '\n')

        contents = globls + types + contents

    def task5():
        lines = []
        with open('export_tbl.info') as f:
            lines = f.readlines()

        data_dic = {}
        addrs = []
        for l in lines:
            items = l.split()
            if "OBJECT" in items:
                addr = int(items[1], 16)
                data_dic[addr] = items[7]
                addrs.append(addr)

        is_text = True
        for i in range(len(contents)):
            l = contents[i]
            if '.section' in l:
                if '.text' in l:
                    is_text = True
                else:
                    is_text = False
            elif is_text == False:
                if ':' in l and ".long" not in l:
                    label1 = l.split(':')[0].strip()
                    if "S_" in label1:
                        addr = int(label1[2:], 16)
                        if addr in addrs:
                            name = data_dic[addr]
                            l = l.replace(label1, name)

                if ":" in l and ".long" in l:
                    label1 = l.split(':')[0].strip()
                    label2 = l.split('long')[1].strip()
                    if "S_" in label1:
                        addr = int(label1[2:], 16)
                        if addr in addrs:
                            name = data_dic[addr]
                            l = l.replace(label1, name)
                    if "S_" in label2:
                        addr = int(label2[2:], 16)
                        if addr in addrs:
                            name = data_dic[addr]
                            l = l.replace(label2, name)

            contents[i] = l


    task1()
    task2()
    task3_4()
  #  task5()


    # write back
    with open('final.s', 'w') as f:
        f.writelines(contents)
