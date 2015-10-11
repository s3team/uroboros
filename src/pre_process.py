lines = []

with open("instrs.info") as f:
    lines = f.readlines()


def help(l):
   # if "repnz scas %es:(%edi),%al" in l:
   #     l = l.replace("repnz scas %es:(%edi),%al","repnz scas")
   # if "repz cmpsb %es:(%edi),%ds:(%esi)" in l:
   #     l = l.replace("repz cmpsb %es:(%edi),%ds:(%esi)", "repz cmpsb")
   # if "rep stos %eax,%es:(%edi)" in l:
        #l = l.replace("rep stos %eax,%es:(%edi)", "rep stos")
   # if "rep movsl %ds:(%esi),%es:(%edi)" in l:
   #     l = l.replace("rep movsl %ds:(%esi),%es:(%edi)", "rep movsl")
   # a preprocess step on clang produced binary code
   # typical instruction pattern can be found in objdump produced assembly code like this:
   #  8049391:       data32 data32 data32 data32 data32 nopw %cs:0x0(%eax,%eax,1)
   # I strongly suspect it is a data padding, let's just remove it now
    if "data32" in l:
        l = ""
    return l
lines = map(help, lines)

with open("instrs.info", 'w') as f:
    map(lambda l : f.write(l), lines)
