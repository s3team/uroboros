import os, sys

fn = sys.argv[1]
arch = sys.argv[2]

black_list = ('_start', '__do_global_dtors_aux', 'frame_dummy', '__do_global_ctors_aux', '__i686.get_pc_thunk.bx', '__libc_csu_fini', '__libc_csu_init', '__cpu_indicator_init')

if arch == "intel":
    os.system(f"objdump -Dr -j .text {fn} > {fn}.temp")
elif arch == "thumb":
    os.system(f"arm-linux-gnueabihf-objdump -Dr -j .text -M force-thumb {fn} > {fn}.temp")
elif arch == "arm":
    os.system(f"arm-linux-gnueabihf-objdump -Dr -j .text {fn} > {fn}.temp")
else:
    print("useless_func_discover.py: Invalid arch")
    exit(1)

lines = []
with open(fn+'.temp') as f:
    lines = f.readlines()

lines.append('')

start_addr = 0
end_addr = 0

in_func = "NULL"

last_addr = 0

def check (l):
    for b in black_list:
        # print b
        if "<"+b+">:" in l:
            # print b
            return b
        return "NULL"

res = {}


for l in lines:
    if l.strip() == "":
        if in_func != "NULL":
            end_addr = last_addr
            if end_addr[-1] == ':':
                end_addr = end_addr[:-1]
            # start and end addrs of a useless func, i.e., a func in black_list
            res[in_func] = (start_addr, end_addr)
            in_func = "NULL"
    else:
        if check(l) != "NULL":
            in_func = check(l)
            start_addr = l.split()[0]
            last_addr = start_addr
        else:
            last_addr = l.split()[0]

#print res

res_list = []
for key, value in res.items():
    res_list.append(key + " " + value[0] + " " + value[1] + '\n')


with open("useless_func.info", 'w') as f:
    f.writelines(res_list)

