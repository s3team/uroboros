import os
import subprocess
import sys

arch = sys.argv[1]

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

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "32-bit" in lines[0]:
        return True
    else:
        return False



def reassemble():
    compiler = ""
    compile_option = ""
    is_32bit_binary = check_32()

    if arch == "intel":
        compiler = "gcc"
        if is_32bit_binary:
            compile_option = "-m32"
        else:
            compile_option = "-m64"
    elif arch == "thumb":
        compiler = "arm-linux-gnueabihf-gcc"
        compile_option = "-mthumb "
    elif arch == "arm":
        if is_32bit_binary:
            compiler = "arm-linux-gnueabihf-gcc"
            compile_option = "-marm "
        else:
            compiler = "aarch64-linux-gnu-gcc"

    compiler_version = subprocess.getoutput(f'{compiler} --version').split('\n')[0].split()[-1]

    if is_32bit_binary == True:
        # 32-bit binary
        os.system(f'{compiler} -no-pie final.s -lm -lrt -lpthread {compile_option} 2> final.error')
    else:
        # 64-bit binary
        os.system(f'{compiler} -no-pie final.s -lm -lrt -lpthread -lcrypt -lgmp {compile_option} 2> final.error')


def parse_error():
    errors = []
    if os.path.isfile('final.error'):
        with open("final.error") as f:
            errors = f.readlines()
            addrs = []
        def help(l):
            if 'In function' in l:
                pass
            elif 'undefined reference' in l and 'S_0x' in l:
                addrs.append(l.split()[-1][1:-1])
        for l in errors:
            help(l)
        # map(lambda l : help(l), errors)


        return set(addrs)


def modify(errors):
    lines = []
    with open("final.s") as f:
        lines = f.readlines()
    def help(l):
        e = list(filter(lambda e : e in l, errors))
        if e != []:
            addr = e[0][2:]
            # print("undefined label : "+addr)
            l = l.replace(e[0], addr)
        return l
    lines = list(map(lambda l : help(l), lines))
    with open("final.s", 'w') as f:
        f.writelines(lines)


def main():
    print("\tmodify final.s to adjust redundant symbols")
    reassemble()
    errors = parse_error()
    modify(errors)
    print("\tmodify finished")

if __name__ == '__main__':
    main()
