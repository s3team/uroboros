import sys

lines = []
section_name = sys.argv[1]

with open("sections.info") as f:
    lines = f.readlines()


size = 0
sec_found = False

for l in lines:
    if section_name in l:
        size = l.split()[3]
        sec_found = True

if not sec_found:
    sys.exit(1)

size = int(size, 16)

strs = ".byte 0x00\n" * size

out_file = section_name+".info"
out_file = out_file.lstrip(".")
with open(out_file, 'w') as f:
    f.write(strs)
