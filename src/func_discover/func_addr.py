import os, sys, re, os.path


fn = sys.argv[1]
c = sys.argv[2]  # reassembly iteration number. 0 is the final iteration
arch = sys.argv[3]

c = int(c)

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "ELF 64-bit" in lines[0]:
        return False
    else:
        return True

is_32bit_binary = check_32()
objdump_command = ""

if arch == "intel":
    objdump_command = "objdump"
elif arch == "arm":
    if is_32bit_binary:
        objdump_command = "arm-linux-gnueabihf-objdump"
    else:
        objdump_command = "aarch64-linux-gnu-objdump"
else:
    print("func_addr.py: Invalid arch")
    exit(1)

# For this objdump, we don't need to use "-M force-thumb" for ARM binaries
# because we are only interested in the function names and their addresses.
# In addition, if binaries are not stripped, objdump will automatically
# detect the correct mode (thumb or arm32).
os.system(f"{objdump_command} -Dr -j .text {fn} > dump.s")

# get function names, if exists
# Example:
# 0000000000401170 <__libc_csu_init>:
os.system('grep ">:" dump.s > fl')
os.system('grep "call" dump.s > fl_calls')

fnl = []
with open('fl') as f:
    fnl = f.readlines()

fnl_old = []
if os.path.isfile('faddr_old.txt'):
    with open('faddr_old.txt') as f:
        fnl_old = f.readlines()

fnl_old = list(map(lambda l : int(l.split()[0],16), fnl_old))

# after one round of reassembly, glibc function names exist in the reassembled binary
blacklist = ['__libc_csu_init', '__libc_csu_fini', '__i686.get_pc_thunk.bx', '__do_global_ctors_aux', '_start', '__do_global_dtors_aux', 'frame_dummy', '__cpu_indicator_init']
addrs = []
addrs_2 = []
addrs_call = []
regex = re.compile(r'S_(0x[0-9A-F]{7})',re.I)
regex1 = re.compile(r'<(.*)>:',re.I)

for fn in fnl:
    # ad-hoc solution, we don't consider basic block labels as functions
    if not "BB_" in fn:
        if "S_" in fn:
            m = regex.search(fn)
            if m:
                d = m.groups()[0]
                d1 = int(d,16)
                if d1 in fnl_old:
                    addr = fn.split('<')[0].strip()
                    addrs.append("0x" + addr + '\n')
                    addrs_2.append(fn)
        elif c > 0:
            m = regex1.search(fn)
            if m:
                d = m.groups()[0]
                if not d in blacklist:
                    addr = fn.split('<')[0].strip()
                    addrs.append("0x" + addr + '\n')
                    addrs_2.append(fn)
        else:
            addr = fn.split('<')[0].strip()
            addrs.append("0x" + addr + '\n')
            addrs_2.append(fn)

fl_calls = []
with open('fl_calls') as f:
    fl_calls = f.readlines()

for call in fl_calls:
    callee = call.split()[-1]
    pattern = r'^0x[a-z0-9]{2,}$'
    if re.match(pattern, callee):
        addrs_call.append(callee + '\n')

with open('faddr_call.txt', 'w') as f:
    f.writelines(addrs_call)

with open('faddr.txt', 'w') as f:
    f.writelines(addrs)

with open('faddr_old.txt', 'w') as f:
    f.writelines(addrs_2)

os.system('cp faddr.txt faddr.txt.'+str(c))
os.system('cp faddr_old.txt faddr_old.txt.'+str(c))
