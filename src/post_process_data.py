import os,sys

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "ELF 64-bit" in lines[0]:
        return False
    else:
        return True

def check_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "LSB shared object" in lines[0]:
        return False
    else:
        return True


is_32 = check_32()


if is_32 == False:
    pass
else:
    is_exe = check_exe()
    if is_exe == True:

        lines = []

        with open("final_data.s") as f:
            lines = f.readlines()

        ll = len(lines)


        in_rodata = False
        in_data = False

            #if ".section .rodata" in l:
            #    for j in range(i+1, i+10):
            #          lines[j] = ""
            #if ".section .data" in l:
            #    for j in range(i+1, i+10):
            #          lines[j] = ""

         # this variable is used by basic block flattern diverisfy


        #        lines[i+1] = "global_des:\n"
        #        lines[i+2] = ".byte 0x00\n"
        #        lines[i+3] = ".byte 0x00\n"
        #        lines[i+4] = ".byte 0x00\n"
        #        lines[i+5] = ".byte 0x00\n"
        #        lines[i+6] = "branch_des:\n"
        #        lines[i+7] = ".byte 0x00\n"
        #        lines[i+8] = ".byte 0x00\n"
        #        lines[i+9] = ".byte 0x00\n"
        #        lines[i+10] = ".byte 0x00\n"

        for i in range(len(lines)):
            l = lines[i]
            if in_data == False and ".data" in l:
                in_data = True
                #lines[i+2] = ""
                #lines[i+3] = ""
                #lines[i+4] = ""
                #lines[i+5] = ""
                #lines[i+6] = ""
                #lines[i+7] = ""
                #lines[i+8] = ""
                #lines[i+9] = ""
        # this variable is used by basic block flattern diversify
        # please comment these lines if no bb flattern diversifying used
        #        lines[i+1] = "global_des:\n"
        #        lines[i+2] = ".byte 0x00\n"
        #        lines[i+3] = ".byte 0x00\n"
        #        lines[i+4] = ".byte 0x00\n"
        #        lines[i+5] = ".byte 0x00\n"
        #        lines[i+6] = "branch_des:\n.byte 0x00\n"
        #        lines[i+7] = ".byte 0x00\n"
        #        lines[i+8] = ".byte 0x00\n"
        #        lines[i+9] = ".byte 0x00\n"

            elif in_rodata == False and ".rodata" in l:
		# add two instructions 
		# branch_routine :pop global_des
		# jmp *branch_des
                in_rodata = True
                #lines[i+2] = ""
                #lines[i+3] = ""
                #lines[i+4] = ""
                #lines[i+5] = ""
                #lines[i+6] = ""
                #lines[i+7] = ""
                #lines[i+8] = ""
                #lines[i+9] = ""
                #lines[i+10] = ""
            elif ".bss" in l:
                #for j in range(i+2,i+44+2):
                #    lines[j] = ""
                break


        with open('final_data.s', 'w') as f:
            f.writelines(lines)


#contents = []
#with open('final_data.s') as f:
#    contents = f.readlines()

# solve export symbol issue : rename certain S_0xaddr into its corresponding
# export symbol name
def solve():
    lines = []
    with open('export_tbl.info') as f:
        lines = f.readlines()

    lines = []
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
            if ':' in l:
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

#solve()

# write back
#with open('final_data.s', 'w') as f:
#    f.writelines(contents)
