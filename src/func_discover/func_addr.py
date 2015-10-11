import os, sys, re, os.path


fn = sys.argv[1]
c = sys.argv[2]

c = int(c)

os.system('objdump -Dr -j .text '+fn+" > dump.s")
os.system('grep ">:" dump.s > fl')

fnl = []
with open('fl') as f:
    fnl = f.readlines()

fnl_old = []
if os.path.isfile('faddr_old.txt'):
    with open('faddr_old.txt') as f:
        fnl_old = f.readlines()

fnl_old = map(lambda l : int(l.split()[0],16), fnl_old)
#fnl_old = map(lambda l : l.split()[0], fnl_old)
#print fnl_old




blacklist = ['__libc_csu_init', '__libc_csu_fini', '__i686.get_pc_thunk.bx', '__do_global_ctors_aux', '_start', '__do_global_dtors_aux', 'frame_dummy']
addrs = []
addrs_2 = []
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

with open('faddr.txt', 'w') as f:
    f.writelines(addrs)

with open('faddr_old.txt', 'w') as f:
    f.writelines(addrs_2)

os.system('cp faddr.txt faddr.txt.'+str(c))
os.system('cp faddr_old.txt faddr_old.txt.'+str(c))
