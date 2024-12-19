import argparse
import sys, os
import commands

from argparse import RawTextHelpFormatter
from argparse import ArgumentParser




# keep the imtermediate binary/final.s or not.
k = False
f_dic = ""

iter_num = 0

def check_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "LSB shared object" in lines[0]:
        return False
    else:
        return True

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "32-bit" in lines[0]:
        return True
    else:
        return False

def check_strip():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "not stripped" in lines[0]:
        return True
    else:
        return False




def reassemble():
    gcc_version = commands.getoutput('gcc --version').split('\n')[0].split()[-1]
    if check_32() == True:
        # 32-bit binary
        if gcc_version < '6': os.system('gcc final.s -lm -lrt -lpthread -m32')
        else: os.system('gcc -no-pie final.s -lm -lrt -lpthread -m32')
    else:
        # 64-bit binary
        if gcc_version < '6': os.system('gcc final.s -lm -lrt -lpthread -lcrypt -m64')
        else: os.system('gcc -no-pie final.s -lm -lrt -lpthread -lcrypt -lgmp -m64')


def process(f, i):
    try:
        os.system("rm final_*.txt")

        # suppose we use this method to obtain function information
        os.system("cp " + f + " func_discover/")
        os.system("python func_discover/func_addr.py func_discover/"+f + " " + str(i))
        os.system("rm final_data.s")
        os.system('rm useless_func.info')
        if i > 0:
            os.system("python useless_func_discover.py " + f)

        os.system('echo \"' + str(i) + '\" > count.txt')
        os.system("strip " + f)
        os.system("python main_discover.py " + f)

        os.system("./init.native " + f)
        if not os.path.isfile("final.s"):
            return False

        os.system("python post_process_data.py")

        os.system('echo ".section .eh_frame" >> final_data.s')
        os.system('cat eh_frame_split.info >> final_data.s')
        os.system('echo ".section .eh_frame_hdr" >> final_data.s')
        os.system('cat eh_frame_hdr_split.info >> final_data.s')

        os.system('cat final_data.s >> final.s')

        if k:
            os.system("cp final.s final.s." + str(i))

        if "gobmk" in f:
            # FIXME!
            os.system("python gobmk_sub.py")

        os.system("python compile_process.py")
        os.system("python label_adjust.py")

        reassemble()

        if iter_num > 0:
            os.system("cp a.out " + f)

        if k:
            print(f_dic)
            os.system("cp a.out " + f_dic + "/" + f + "." + str(i+1))
            os.system("mv final.s." + str(i) + " " + f_dic)

    except :
        return False
    else:

        os.system('rm ' + "faddr_old.txt." + str(i))
        os.system('rm ' + "faddr.txt." + str(i))


        return True




def iterate (f, iterations):
    print("start to process binary: " + f)

    for i in range(0, iterations):
        print ("########## iteration round "+str(i+1) + " begin ! ###########")
        if process(f, i):
            pass
        else:
            return False
        print ("########## iteration round "+str(i+1) + " finish ! ##########")

    return True


def check (b, f, al):
    if not al:
        al = []

    if not os.path.isfile(b):
        print("cannot find input binary")
        return False

    if '/' in b:
        # not in current directory
        os.system('cp ' + b + ' .')


    os.system('file ' + f + ' > elf.info')
    if check_exe() == False:
        print("Uroboros doesn't support shared library")
        return False

    # if assumption three is utilized, then input binary must be unstripped.
    if '3' in al and check_strip() == False:
        print('''Uroboros doesn't support stripped binaries when using assumption three''')
        return False

    return True


import datetime
import time


def fold_withtamp (f):
    global f_dic
    ts = time.time()
    st = datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d_%H:%M:%S')

    f_dic = "test_fold_" + f + '_' + st

    os.system('mkdir ' + f_dic)


def set_assumption (l):
    # 2 -> assumption two: fix data section starting address
    # Note that assumption two require linker script to reassemble!
    # Some of the examples can be found at ./ld_scripts/*
    # 3 -> assumption three: function starting address + jump table
    # _ -> not defined.

    a = 0
    b = 0

    if not l:
        with open('assumption_set.info', 'w') as f:
            f.writelines(["1\n"])

    else:
        chk = (i in ['2', '3'] for i in l)


        if any(chk) == False:
            print("assumption undefined!")
            print("accecpt assumptions: 2 for assumption two and 3 for assumption three")
            return False

        l = set(l)

        l = ' '.join(l)
        l += "\n"

        with open('assumption_set.info', 'w') as f:
            f.writelines(l)

    return True


if __name__ == "__main__":
    p = ArgumentParser(formatter_class=RawTextHelpFormatter)
    p.add_argument("binary",
                   help="path to the input binary, for example, /usr/bin/ls")
    p.add_argument("-i", "--iteration", type=int,
                   help="the number of disassemble-(instrument)-reassemble iterations")
    p.add_argument("-k", "--keep", action="count",
                   help="if multiple iteration processing, whether to keep itermediate binaries")
    p.add_argument("-a", "--assumption", action="append",
                   help='''this option configures three addtional assumption,
note that two basic assumptions and addtional assumption one
(n-byte alignment) are set by default,
while assumption two and three need to be configured. For example, setting
assumption two and three: -a 2 -a 3''')
    p.add_argument('--version', action='version', version='Uroboros 0.4')

    args = p.parse_args()
    b = args.binary
    i = args.iteration
    iter_num = i
    k = (args.keep > 0)


    f = os.path.basename(b)
    if check(b, f, args.assumption) == False or set_assumption(args.assumption) == False:
        pass

    else:
        if k:
            fold_withtamp(f)

        if args.iteration:
            if iterate(f, i):
                print("processing succeeded")
            else:
                print("exception, processing failed")
        else:
            if process(f, 0):
                print("processing succeeded")
            else:
                print("exception, processing failed")
