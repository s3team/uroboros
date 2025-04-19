import sys
import os
import subprocess

import arm_preprocess

fn = sys.argv[1]
arch = sys.argv[2]
THUMB_OFFSET = 1

os.system('file ' + fn + ' > elf.info')


def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "ELF 64-bit" in lines[0]:
        return False
    else:
        return True


def check_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "executable" in lines[0]:
        return True
    elif "LSB shared object" in lines[0] and "ld-linux" in lines[0]:
        return True
    else:
        return False


def check_pie():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "LSB shared object" in lines[0] and "ld-linux" in lines[0]:
        return True
    else:
        return False

def check_main_symbol():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    return "not stripped" in lines[0]

def check_thumb_mode(arch: str, entry_point: int) -> bool:
    # if entry point is odd, then it is thumb mode
    if arch == "arm" and entry_point % 2 == 1:
        return True
    else:
        return False

def get_entry_point_address() -> str:
    output = subprocess.getoutput("readelf -h " + fn)
    entry_point = ""

    for line in output.split('\n'):
        if "Entry point address" in line:
            entry_point = line.split()[-1][2:]
            break

    return entry_point

def find_intel_start_section(lines, entry_point_str):
    ll = len(lines)
    for i in range(ll):
        if entry_point_str == lines[i].strip().split(':')[0]:
            ln = i
            while (ln < ll) and "Cannot identify main function using entry point":
                if "nop" in lines[ln] or "ret" in lines[ln]:
                    return lines[i:ln+1]
                ln += 1


def find_arm_start_section(lines, entry_point_str, is_32bit_binary, is_thumb_mode):
    # if thumb mode, we need to subtract 1 from entry point
    if is_thumb_mode:
        entry_point = int(entry_point_str, 16)
        entry_point -= 1
        entry_point_str = hex(entry_point)[2:]

    ll = len(lines)
    for i in range(ll):
        if entry_point_str == lines[i].strip().split(':')[0]:
            ln = i
            if is_32bit_binary:
                while (ln < ll) and "Cannot identify main function using entry point":
                    # There could be other forms of nop for ARM such as "nop", "movs r8, r8", etc.
                    # So far, did not handle those cases.
                    if "movs	r0, r0" in lines[ln]:
                        return lines[i:ln+1]
                    ln += 1
            else:
                while (ln < ll) and "Cannot identify main function using entry point":
                    # Find the following pattern and return the lines until the "ret" instruction:
                    # bl      400490 <abort@plt>
                    # nop
                    # b       4005c4 <printf@plt+0x124>
                    # nop
                    # ret
                    if "ret" in lines[ln] and "nop" in lines[ln-1]:
                        return lines[i:ln+1]
                    ln += 1


def find_start_section(lines, entry_point_str, is_32bit_binary, is_thumb_mode):
    if arch == "intel":
        return find_intel_start_section(lines, entry_point_str)
    elif arch == "arm":
        return find_arm_start_section(lines, entry_point_str, is_32bit_binary, is_thumb_mode)
    else:
        raise Exception("Unknown architecture")

def get_arm_main_symbol(start_section, is_32bit_binary):
    main_symbol = None
    if is_32bit_binary:
        # Find the following pattern to
        #   1) replace [sl, r0] with the main symbol and
        #   2) calculate the main symbol address with symbolic execution
        #
        # 10340:	f8df a018 	ldr.w	sl, [pc, #24]
        # 10344:	a305      	add	r3, pc, #20
        # 10346:	449a      	add	sl, r3
        # 10348:	f04f 0300 	mov.w	r3, #0
        # 1034c:	b408      	push	{r3}
        # 1034e:	4804      	ldr	r0, [pc, #16]
        # 10350:	f85a 0000 	ldr.w	r0, [sl, r0] ; where the main symbol is loaded
        # 10354:	f7ff efd4 	blx	10300 <__libc_start_main@plt>
        # 10358:	f7ff efe4 	blx	10324 <abort@plt>

        start_addr = int(start_section[0].split(':')[0].strip(), 16)
        end_addr = None
        for i, line in enumerate(start_section):
            if (
                "ldr" in start_section[i-2]
                and "blx" in start_section[i-1]
                and "blx" in start_section[i]
            ):
                end_addr = int(start_section[i-1].split(':')[0].strip(), 16)
                break

        # TODO: ARM main symbol

    else:
        # The "b" instruction has the address of main function:
        # bl      400490 <abort@plt>
        # nop
        # b       4005c4 <printf@plt+0x124>
        # nop
        # ret
        for i, line in enumerate(start_section):
            if ("ret" in line and
                "nop" in start_section[i-1]):
                split_line = start_section[i-2].split('\t')
                main_symbol = split_line[-1].split()[0].strip()
                break

    return main_symbol

def get_intel_main_symbol(start_section):
    i = len(start_section) - 1
    while i > 0:
        if 'call' in start_section[i] or 'callq' in start_section[i]:
            main_symbol = start_section[i-1].split()[-1].split(',')[0]
            if '0x' not in main_symbol:
                main_symbol = start_section[i-2].split()[-1].split(',')[0]
            break
        i -= 1

    return main_symbol

def get_main_symbol(start_section, is_32bit_binary):
    if arch == "intel":
        return get_intel_main_symbol(start_section)
    elif arch == "arm":
        return get_arm_main_symbol(start_section, is_32bit_binary)
    else:
        raise Exception("Unknown architecture")


is_exe = check_exe()

if is_exe == False:  # share library
    pass
else:
    entry_point_str = get_entry_point_address()
    is_thumb = check_thumb_mode(arch, int(entry_point_str, 16))
    is_32bit_binary = check_32()

    lines = []

    with open(fn+".temp") as f:
        lines = f.readlines()

    main_symbol = ""

    if check_main_symbol():
        # For not stripped binary, take advantage of the function discover result
        with open('faddr_old.txt') as f:
            faddr_lines = f.readlines()
            for line in faddr_lines:
                if '<main>' in line:
                    main_symbol = '0x'+line.split()[0].lstrip('0')
                    break
    elif check_pie():
        if is_32bit_binary:
            pass
        else:
            # For stripped 64 bit PIE binary, find the main function using entry point
            #   109d:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
            #   10a1:	50                   	push   %rax
            #   10a2:	54                   	push   %rsp
            #   10a3:	4c 8d 05 46 01 00 00 	lea    0x146(%rip),%r8        # 11f0 <__libc_csu_fini>
            #   10aa:	48 8d 0d cf 00 00 00 	lea    0xcf(%rip),%rcx        # 1180 <__libc_csu_init>
            #   10b1:	48 8d 3d a8 ff ff ff 	lea    -0x58(%rip),%rdi        # 1060 <main>
            #   10b8:	ff 15 22 2f 00 00    	callq  *0x2f22(%rip)        # 3fe0 <__libc_start_main@GLIBC_2.2.5>
            #   10be:	f4                   	hlt
            #   10bf:	90                   	nop
            start_section = find_start_section(lines, entry_point_str, is_32bit_binary, False)
            i = len(start_section) - 1

            while i > 0:
                if "call" in start_section[i] or "callq" in start_section[i]:
                    base_addr = int(start_section[i].split(":")[0], 16)
                    offset = int(start_section[i - 1].split("(")[0].split()[-1], 16)
                    main_symbol = hex(base_addr + offset)
                    break
                i -= 1

    else:
        # For stripped non-PIE binary, find the main function manually
        start_section = find_start_section(lines, entry_point_str, is_32bit_binary, is_thumb)
        main_symbol = get_main_symbol(start_section, is_32bit_binary)
        i = len(start_section)-1

    # Some of the PIC code/module rely on typical pattern to locate
    # such as:

    # 804c460: push   %ebx
    # 804c461: call   804c452 <__i686.get_pc_thunk.bx>
    # 804c466: add    $0x2b8e,%ebx
    # 804c46c: sub    $0x18,%esp

    # What we can do this pattern match `<__i686.get_pc_thunk.bx>` and calculate
    # the address by adding 0x2b8e and 0x804c466, which equals to the begin address of GOT.PLT table

    # symbols can be leveraged in re-assemble are
    # _GLOBAL_OFFSET_TABLE_   ==    ** .got.plt **
    # ....

    os.system('rm main.info')

    if '0x' in main_symbol:
        main_symbol = main_symbol.split('0x')[1]

    with open("main.info", 'w') as f:
        f.writelines('S_0x'+main_symbol.upper()+"\n")
