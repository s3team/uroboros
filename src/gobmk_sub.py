import sys, os

# subsititution for binary gobmk
# this script mainly supports two subsitution.
# 1. code section:

#S_0x80B22E9 : mov S_0x80E0400(,%edi,0x4),%eax
#mov S_0x80E0404(,%edi,0x4),%edx
#mov S_0x80E0408(,%edi,0x4),%ebx
#mov S_0x80E040C(,%edi,0x4),%ecx

# should be subsititued to

#mov S_0x80F6380-0x15f80(,%edi,0x4),%eax
#mov S_0x80F6380-0x15f7c(,%edi,0x5),%edx
#mov S_0x80F6380-0x15f78(,%edi,0x4),%ebx
#mov S_0x80F6380-0x15f74(,%edi,0x4),%ecx


# 2. data section

# all .long S_0x80C0000 appearred in data and rodata sections should be
# substituted to byte sequences unless this situation
#
# .long S_0x80C0000
#
#
#
# .byte 0x03
# .byte 0x00
# .byte 0x00
# .byte 0x00
# .byte 0x48
# .byte 0xe1
# .byte 0xfa
# .byte 0x3f
# .byte 0x20
# .byte 0xf1
# .byte 0x21
# .byte 0x08


# these two rules preserve in the loop processing.

lines = []
with open ("final.s") as f:
    lines = f.readlines()


for i in range(len(lines)):
    l = lines[i]
    if "S_0x80E0400(" in l: # we found it
        label = l.split(':')[0]
        lines[i] = label + " : mov S_0x80F6380-0x15f80(,%edi,0x4),%eax\n"
        lines[i+1] = "mov S_0x80F6380-0x15f7c(,%edi,0x4),%edx\n"
        lines[i+2] = "mov S_0x80F6380-0x15f78(,%edi,0x4),%ebx\n"
        lines[i+3] = "mov S_0x80F6380-0x15f74(,%edi,0x4),%ecx\n"
    elif ".long S_0x80C0000" in l:
        if "03" in lines[i+4] and "00" in lines[i+5] and "48" in lines[i+8] and "e1" in lines[i+9] and "fa" in lines[i+10] and "3f" in lines[i+11]:
            # found the right one
            continue
        else:
            lines[i] = ".byte 0x00\n"
            lines[i+1] = ".byte 0x00\n"
            lines[i+2] = ".byte 0x0C\n"
            lines[i+3] = ".byte 0x08\n"

with open('final.s', 'w') as f:
    f.writelines(lines)
