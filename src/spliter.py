import os, sys

from pathlib import Path


def check_static():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "statically linked" in lines[0]:
        return True
    else:
        return False

lines = []
with open('text.info') as f:
    lines = f.readlines()

s = []
for l in (lines):
    items = l.split()
    for item in (items):
        l = len(item)
        for i in range(0,l,2):
            s.append(".byte 0x"+item[i:i+2])
s = '\n'.join(reversed(s))

with open('text_split.info', 'w') as f:
    f.write(s+'\n')

lines = []
with open('rodata.info') as f:
    lines = f.readlines()

s = []
for l in (lines):
    items = l.split()
    for item in (items):
        l = len(item)
        for i in range(0,l,2):
            s.append(".byte 0x"+item[i:i+2])


      #  s.append(".byte 0x"+item[6:])
      #  s.append(".byte 0x"+item[4:6])
      #  s.append(".byte 0x"+item[2:4])
      #  s.append(".byte 0x"+item[0:2])

s = '\n'.join(reversed(s))

with open('rodata_split.info', 'w') as f:
    f.write(s+'\n')

if Path('__libc_IO_vtables.info').exists():
    lines = []
    with open('__libc_IO_vtables.info') as f:
        lines = f.readlines()

    s = []
    for l in (lines):
        items = l.split()
        for item in (items):
            l = len(item)
            for i in range(0,l,2):
                s.append(".byte 0x"+item[i:i+2])

    s = '\n'.join(reversed(s))

    with open('__libc_IO_vtables_split.info', 'w') as f:
        f.write(s+'\n')

lines = []
with open('data.info') as f:
    lines = f.readlines()

s = []
for l in (lines):
    items = l.split()
    for item in (items):
        l = len(item)
        for i in range(0,l,2):
            s.append(".byte 0x"+item[i:i+2])
       # s.append(".byte 0x"+item[6:])
       # s.append(".byte 0x"+item[4:6])
       # s.append(".byte 0x"+item[2:4])
       # s.append(".byte 0x"+item[0:2])

s = '\n'.join(reversed(s))

with open('data_split.info', 'w') as f:
    f.write(s+'\n')

# .data.rel.ro sections
lines = []
with open('data_rel_ro.info') as f:
    lines = f.readlines()

s = []
for l in lines:
    items = l.split()
    for item in items:
        l = len(item)
        for i in range(0,l,2):
            s.append(".byte 0x"+item[i:i+2])

s = '\n'.join(reversed(s))

with open('data_rel_ro_split.info', 'w') as f:
    f.write(s+'\n')

# rodata.cst32 sections
lines = []
with open('rodata_cst32.info') as f:
    lines = f.readlines()

s = []
for l in lines:
    items = l.split()
    for item in items:
        l = len(item)
        for i in range(0,l,2):
            s.append(".byte 0x"+item[i:i+2])

s = '\n'.join(reversed(s))

with open('rodata_cst32_split.info', 'w') as f:
    f.write(s+'\n')

lines = []
with open('got.info') as f:
    lines = f.readlines()

s = []
for l in (lines):
    items = l.split()
    for item in (items):
        l = len(item)
        for i in range(0,l,2):
            s.append(".byte 0x"+item[i:i+2])
       # s.append(".byte 0x"+item[6:])
       # s.append(".byte 0x"+item[4:6])
       # s.append(".byte 0x"+item[2:4])
       # s.append(".byte 0x"+item[0:2])

s = '\n'.join(reversed(s))

with open('got_split.info', 'w') as f:
    f.write(s+'\n')

if check_static():
    lines = []
    with open('got_plt.info') as f:
        lines = f.readlines()

    s = []
    for l in (lines):
        items = l.split()
        for item in (items):
            l = len(item)
            for i in range(0,l,2):
                s.append(".byte 0x"+item[i:i+2])
        # s.append(".byte 0x"+item[6:])
        # s.append(".byte 0x"+item[4:6])
        # s.append(".byte 0x"+item[2:4])
        # s.append(".byte 0x"+item[0:2])

    s = '\n'.join(reversed(s))

    with open('got_plt_split.info', 'w') as f:
        f.write(s+'\n')

lines = []
with open('eh_frame.info') as f:
    lines = f.readlines()

s = []
for l in (lines):
    items = l.split()
    for item in (items):
        l = len(item)
        for i in range(0,l,2):
            s.append(".byte 0x"+item[i:i+2])
       # s.append(".byte 0x"+item[6:])
       # s.append(".byte 0x"+item[4:6])
       # s.append(".byte 0x"+item[2:4])
       # s.append(".byte 0x"+item[0:2])

s = '\n'.join(reversed(s))

with open('eh_frame_split.info', 'w') as f:
    f.write(s+'\n')


lines = []
with open('eh_frame_hdr.info') as f:
    lines = f.readlines()

s = []
for l in (lines):
    items = l.split()
    for item in (items):
        l = len(item)
        for i in range(0,l,2):
            s.append(".byte 0x"+item[i:i+2])
       # s.append(".byte 0x"+item[6:])
       # s.append(".byte 0x"+item[4:6])
       # s.append(".byte 0x"+item[2:4])
       # s.append(".byte 0x"+item[0:2])

s = '\n'.join(reversed(s))

with open('eh_frame_hdr_split.info', 'w') as f:
    f.write(s+'\n')


