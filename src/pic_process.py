## Some of the PIC code/module rely on typical pattern to locate
## such as:

##  804c460: push   %ebx
##  804c461: call   804c452 <__i686.get_pc_thunk.bx>
##  804c466: add    $0x2b8e,%ebx
##  804c46c: sub    $0x18,%esp

## What we can do this pattern match `<__i686.get_pc_thunk.bx>` and calculate
## the address by plusing 0x2b8e and  0x804c466, which equals to the begin address of GOT.PLT table

## symbols can be leveraged in re-assemble are
##  _GLOBAL_OFFSET_TABLE_   ==    .got.plt
##  ....



import os, sys
import re

sec_symb = {".got.plt":"$_GLOBAL_OFFSET_TABLE_"}

def info_dump(f):
    os.system("readelf -S "+ f +" | awk '$2==\".got.plt\" {print $2,$4,$5,$6}' > pic_secs.info")

step = 1
def text_collect(f):
    fn = f+'.temp'
    with open(fn) as fd:
       return fd.read().splitlines()

def info_collect():
    ls = []
    pic_map = {}
    with open("pic_secs.info") as fd:
       ls = fd.read().splitlines()

    def help(l):
        items = l.split()
        # name ;  begin addr; ... ; size
        pic_map[items[0]] = (int(items[1], 16), int(items[3], 16))
    map(lambda l: help(l), ls)

    return pic_map


def check_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "LSB shared object" in lines[0]:
        return False
    else:
        return True

is_exe = check_exe()



#  here is the tricky thing, in unstripped binary,
#   we can safely identify the symbol "__i686.get_pc_thunk.bx"
#   however, we have to pattern match __i686.get_pc_thunk.bx function
#   manually, then get the address

def text_process_unstrip(f):
    ls = text_collect(f)
    info_dump(f)
    pic_map = info_collect()

    for i in range(1,len(ls)):
        l = ls[i]
        if "<__i686.get_pc_thunk.bx>" in l and "call" in l:
            t = ls[i+1]
            # typically, t should look like this
            # 804c466: add    $0x2b8e,%ebx
            # if not, just let it crash
            last = t.split()[-1]
            addr_s = t.split(':')[0]
            off_s = last.split(',')[0][1:]
            off = int(off_s, 16)
            addr = int(addr_s, 16)

            baddr = addr + off

            for key, value in pic_map.iteritems():
                if value[0] == baddr:
                    # OK, we find it!
                    symb = sec_symb[key]
                    ls[i+1] = t.replace('$'+off_s, symb)
                elif value[0] < baddr and baddr < (value[0]+value[1]):
                    print("unhandled situation")

    with open(f+'.temp', 'w') as fd:
        map(lambda l: fd.write(l+ "\n"), ls)

def thunk_identify(ls):
    global step

    for i in range(step, len(ls)):
        l = ls[i]
        m = re.search(r'([0-9A-Fa-f]*)(.+)(mov\s+\(%esp\),)(%e\w{2})', l.strip())
        if m != None:
            t = ls[i+1]
            if "ret" in t.split()[-1]:
                step = i + 1
                return m.group(1), m.group(4)
    # print "PIC position location can not be found!!"
    return None, None

def adjust_offset(start, instrs, base, init_reg):
    i = start
    regs = [init_reg]
    while i < len(instrs):
        instr = instrs[i]
        if re.search(r'ret(n|q)*|leave(q)*',instr): break
        n = len(regs)
        k = 0
        while k < n:
            reg = regs[k]
            if len(reg) == 4:
                m = re.search(r'([\-]*0x[0-9A-Fa-f]+)\((.*)'+reg+'(.*)\)',instr)
                if m:
                    offset_s = m.group(1)
                    offset = int(offset_s,16)
                    if m.group(2) and m.group(3):
                        target = hex(base + offset) + '(' + m.group(2).strip(',') + ')'
                    elif m.group(3):
                        target = hex(base + offset) + '(' + m.group(3) + ')'
                    else:
                        target = hex(base + offset)
                    instrs[i] = instr.replace(m.group(0),target)
                reg4re = reg
            else:
                reg4re = reg.replace('(','\(').replace(')','\)')
            
            m = re.search(r'mov\s+' + reg4re + ',(.+)', instr)
            if m:
                new_reg = m.group(1)
                if new_reg not in regs: regs.append(new_reg)
            if re.search(r'.+' + reg4re + '$',instr):
                m = re.search(r'mov\s+(.+),' + reg4re +'$', instr)
                src_reg = ''
                if m: src_reg = m.group(1)
                if src_reg not in regs:
                    regs.remove(reg)
                    n -= 1
                    continue
            k += 1
        if len(regs)==0: break
        i += 1

def text_process_strip(f):
    ls = text_collect(f)
    info_dump(f)
    pic_map = info_collect()

    while True:
        pc_thunk_addr, register = thunk_identify(ls)
        if pc_thunk_addr == None: break
        for i in range(1,len(ls)):
            l = ls[i]
            if re.search(r'call\s+'+pc_thunk_addr, l) != None:
                t = ls[i+1]
                m = re.search(r'([0-9A-Fa-f]+)(.+)add\s+\$(0x[0-9A-Fa-f]+),'+register,t.strip())
                if m == None: break
                # typically, t should look like this
                # 804c466: add    $0x2b8e,%ebx
                # if not, just let it crash
                off_s = m.group(3)
                addr_s = m.group(1)
                off = int(off_s, 16)
                addr = int(addr_s, 16)

                baddr = addr + off

                for key, value in pic_map.iteritems():
                    if value[0] == baddr:
                        # OK, we find it!
                        symb = sec_symb[key]
                        ls[i+1] = t.replace('$'+off_s, symb)
                        adjust_offset(i+2,ls,baddr,register)
                    elif value[0] < baddr and baddr < (value[0]+value[1]):
                        print("unhandled situation")

    with open(f+'.temp', 'w') as fd:
        map(lambda l: fd.write(l+ "\n"), ls)


if __name__ == '__main__':
    if len(sys.argv) == 3:
        is_32 = sys.argv[2]
        if is_32 == "true":
            if is_exe == True: # executable
                binary = sys.argv[1]
                t = text_process_strip(binary)
            else:
                # shared object don't translate target addrs into
                # GLOBAL_OFFSET_TABLE, instead, dump the thunk addr for
                # analysis in share_lib_helper.ml
                binary = sys.argv[1]
                addr, _ = thunk_identify(text_collect(binary)).strip()
                with open('pic_thunk.info', 'w') as f:
                    f.write(addr+'\n')

        else:
            pass
    else:
        print("usage: python pic_process.py binary is_32")
