lines = []

with open("sections.info") as f:
    lines = f.readlines()


size = 0

for l in lines:
    if ".bss" in l:
        size = l.split()[3]

size = int(size, 16)

strs = ".byte 0x00\n" * size

with open("bss.info", 'w') as f:
    f.write(strs)
