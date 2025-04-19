import re


lines = []
is_32 = False
with open("elf.info") as f:
    lines = f.readlines()
if "32-bit" in lines[0]:
    is_32 = True

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
    if "cs nop" in l:
        l=""
    # detect use of %eiz register in a LEA statement, which is basically 7 NOP statements
    # but gcc cannot understand the %eiz register. Therefore, we substite the LEA statement
    # with a NOP statement
    if is_32:
        #m = re.search(r"([0-9A-Fa-f]+).+leal?\s+0x[0-9A-Fa-f]+\((%e\w{2})?,%eiz,1\),%e\w{2}", l)
        m = re.search(r"([0-9A-Fa-f]+).+leal?\s+((%cs:)?0x[0-9A-Fa-f]+)\((%e\w{2})?,%eiz,1\),%e\w{2}", l)
        if m:
        #if "lea" in l and "eiz" in l:
            addr = l.split(":")[0]
            #print(f"~~~ replace: {addr} at {l}")
            l = f"{addr}:\tnop\n"
            return l
    return l

lines = list(map(help, lines))

with open("instrs.info", 'w') as f:
    for l in lines:
        f.write(l)
    # map(lambda l : f.write(l), lines)
