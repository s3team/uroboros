import os, sys

def get_binname():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    l = lines[0]
    return l.split(':')[0].strip()

lines = []
with open("init_sec.info") as f:
   lines = f.readlines()

l = lines[0]

if ']' not in l:
    pass
else:
    fn = get_binname()
    os.system('readelf -SW '+fn+' > temp')

    lines = []
    with open("temp") as f:
       lines = f.readlines()

    for l in lines:
        items = l.split()
        if '.init' in items:
            # this is the line
# [ 9] .init             PROGBITS        00000000000006e8 0006e8 000018 ....
            s = items[2] + ' ' + items[4] + ' ' + items[5] + ' ' + items[6] + '\n'

            lines = []
            with open("init_sec.info", 'w') as f:
                f.write(s)
