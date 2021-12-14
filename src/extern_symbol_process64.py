# this code aims at solving glibc global variables issue
#
# some of the code contains comments like this:
# 401599:       48 8b 3d 70 6c 20 00    mov    0x206c70(%rip),%rdi        # 608210 <stdout>
# instructions like this should be translated into
#    mov stdout,%rdi

# in 32 bit ELF, same libc external variables usually disassembled in this
# format:
#     mov stdout@@GLIBC_2XXX, %rdi
# and it is handled in reassemble.ml
#

import sys, os, re

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "ELF 64-bit" in lines[0]:
        return False
    else:
        return True

is_32 = check_32()

# no need in 32 bit binaries
if is_32 == True:
    pass
else:
    lines = []
    fn = sys.argv[1]

    with open(fn+'.temp') as f:
        lines = f.readlines()

    symbols = []

    pat_d = r'0x[0-9a-f]+\(%rip\)'
    pat_s = r'<(.*)>'

    for i in range(len(lines)):
        l = lines[i]
        m_s = re.search(pat_s, l)
        if "#" in l and not "+" in l and m_s:
            m_d = re.search(pat_d, l)
            try:
                src = m_s.group(1) # let it crash it not
                des = m_d.group(0) # let it crash it not
                if '@@' in src: src = src.split('@@')[0]
                l = l.split('#')[0]
                l = l.replace(des, src)
                lines[i] = l+"\n"
            except Exception:
                print("exception in external symbols processing of 64-bit ELF")
                print(l)
    with open(fn + '.temp', 'w') as f:
        f.writelines(lines)

    #with open('rip_symbols.txt', 'w') as f:
    #    f.writelines(sorted(set(symbols)))
