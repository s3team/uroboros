#!/usr/bin/env python3
import datetime
import os
import subprocess
import time
from argparse import ArgumentParser, RawTextHelpFormatter

# keep the intermediate binary/final.s or not.
k = False
f_dic = ""
iter_num = 0


def is_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "executable" in lines[0]:
        return True
    elif "LSB shared object" in lines[0] and "ld-linux" in lines[0]:
        return True
    else:
        return False


def is_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "32-bit" in lines[0]:
        return True
    else:
        return False


def is_strip():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "not stripped" in lines[0]:
        return True
    else:
        return False


def is_static():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "statically linked" in lines[0]:
        return True
    else:
        return False


def get_custom_objects(cleanup):
    result = subprocess.run(
        "ls | grep '\\.o$'", shell=True, capture_output=True, text=True
    )
    result = result.stdout.strip().split("\n") if result.stdout else []
    if cleanup or not os.path.exists("points.ins"):
        # dict.fromkeys ensure deterministic object linking order
        return list(dict.fromkeys(result))
    with open("points.ins") as points:
        p = points.readlines()
        for line in p:
            if not line.startswith("INCLUDE"):
                continue
            line_nocmd = line.split('"')[0]
            libs = [os.path.basename(l) for l in line_nocmd[8:].split() if l]
            result.extend(libs)
    # dict.fromkey sensure deterministic object linking order
    return list(dict.fromkeys(result))


def reassemble(assembly_file, arch):
    compiler = ""
    compile_option = "-no-pie -lm -lrt -lpthread "
    is_32bit_binary = is_32()
    custom_objects = get_custom_objects(cleanup=False)

    if arch == "intel":
        compiler = "gcc"
        if is_32bit_binary:
            compile_option += "-m32 "
        else:
            compile_option += "-m64 -lcrypto -lselinux -lgmp "
    elif arch == "thumb":
        compiler = "arm-linux-gnueabihf-gcc"
        compile_option += "-mthumb -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard "
    elif arch == "arm":
        if is_32bit_binary:
            compiler = "arm-linux-gnueabihf-gcc"
            compile_option += "-marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard "
        else:
            compiler = "aarch64-linux-gnu-gcc"
            compile_option += "-march=armv9-a "

    if is_static() == True:
        compile_option += "-static "

    for obj in custom_objects:
        print(f"custom_object: {obj}")
        compile_option += f" {obj} "

    print(f"reassemble: {compiler} {assembly_file} {compile_option}")
    os.system(f"{compiler} {assembly_file} {compile_option} 2> final.error")


def parse_errors():
    addrs = []
    if not os.path.isfile("final.error"):
        return set(addrs)
    with open("final.error") as f:
        for l in f.readlines():
            if "undefined reference" in l and "S_0x" in l:
                addrs.append(l.split()[-1][1:-1])
    return set(addrs)


def modify_error(errors):
    if not errors:
        return
    with open("final.s") as f:
        lines = f.readlines()

    def help(l):
        e = list(filter(lambda e: e in l, errors))
        if e != []:
            addr = e[0][2:]
            l = l.replace(e[0], addr)
        return l

    lines = list(map(help, lines))
    with open("final.s", "w") as f:
        f.writelines(lines)


def process(f, i, arch):
    is_32bit_binary = is_32()
    strip_command = ""
    objdump_commamnd = ""
    if arch == "intel":
        strip_command = "strip"
        objdump_commamnd = "objdump"
    elif arch == "thumb":
        strip_command = "arm-linux-gnueabihf-strip"
        objdump_commamnd = "arm-linux-gnueabihf-objdump"
    elif arch == "arm":
        if is_32bit_binary:
            strip_command = "arm-linux-gnueabihf-strip"
            objdump_commamnd = "arm-linux-gnueabihf-objdump"
        else:
            strip_command = "aarch64-linux-gnu-strip"
            objdump_commamnd = "aarch64-linux-gnu-objdump"

    try:
        os.system("rm final_*.txt")

        # suppose we use this method to obtain function information
        os.system("cp " + f + " func_discover/")
        os.system(
            f"python3 func_discover/func_addr.py func_discover/{f} {str(i)} {arch}"
        )
        if os.path.exists("final_data.s"):
            os.system("rm final_data.s")
        if os.path.exists("useless_func.info"):
            os.system("rm useless_func.info")
        os.system(f"rm func_discover/{f}")
        if i > 0:
            os.system(f"python3 useless_func_discover.py {f} {arch}")

        os.system(f'echo "{str(i)}" > count.txt')
        os.system(f"cp {f} {f}.sym")
        os.system(f"nm {f}.sym > nm.info 2> /dev/null")
        if is_static():
            os.system(
                f"{objdump_commamnd} -d --section=.plt {f} > plt_whole.info 2> /dev/null"
            )
            os.system(f"readelf -r {f} > rela_plt.info")
            os.system(f"readelf -l {f} > headers.info")
        os.system(f"{strip_command} {f}")

        bit_mode = "32" if is_32bit_binary else "64"
        cmd = f"opam exec -- dune exec init {f} {arch} {bit_mode}"
        init_result = subprocess.run(cmd, shell=True)
        if init_result.returncode != 0:
            print("init failed")
            return False

        if not os.path.isfile("final.s"):
            return False

        os.system("python3 post_process_data.py")

        os.system('echo ".section .eh_frame" >> final_data.s')
        os.system("cat eh_frame_split.info >> final_data.s")
        os.system('echo ".section .eh_frame_hdr" >> final_data.s')
        os.system("cat eh_frame_hdr_split.info >> final_data.s")

        os.system("cat final_data.s >> final.s")

        if k:
            os.system("cp final.s final.s." + str(i))

        if "gobmk" in f:
            # FIXME!
            os.system("python3 gobmk_sub.py")

        if arch == "thumb" or arch == "arm":
            os.system(f"python3 arm_postprocess.py final.s {arch}")

        reassemble("final.s", arch)
        modify_error(parse_errors())
        reassemble("final.s", arch)

        custom_objects = get_custom_objects(cleanup=True)
        for obj in custom_objects:
            # remove .o object files from instrumentation
            print(f"rm {obj}")
            os.system(f"rm {obj}")

        if iter_num > 0:
            os.system("cp a.out " + f)

        if k:
            print(f_dic)
            os.system("cp a.out " + f_dic + "/" + f + "." + str(i + 1))
            os.system("mv final.s." + str(i) + " " + f_dic)

    except:
        return False
    else:
        os.system("rm " + "faddr_old.txt." + str(i))
        os.system("rm " + "faddr.txt." + str(i))

        return True


def iterate(f, iterations, arch):
    print("start to process binary: " + f)

    for i in range(0, iterations):
        print("########## iteration round " + str(i + 1) + " begin ! ###########")
        if process(f, i, arch):
            pass
        else:
            return False
        print("########## iteration round " + str(i + 1) + " finish ! ##########")

    return True


def check(b, f, al):
    if not al:
        al = []

    if not os.path.isfile(b):
        print("cannot find input binary")
        return False

    if "/" in b:
        # not in current directory
        os.system("cp " + b + " .")

    os.system("file " + f + " > elf.info")
    if is_exe() == False:
        print("Uroboros doesn't support shared library")
        return False

    # if assumption three is utilized, then input binary must be unstripped.
    if "3" in al and is_strip() == False:
        print(
            """Uroboros doesn't support stripped binaries when using assumption three"""
        )
        return False

    return True


def fold_withtamp(f):
    global f_dic
    ts = time.time()
    st = datetime.datetime.fromtimestamp(ts).strftime("%Y-%m-%d_%H:%M:%S")

    f_dic = "test_fold_" + f + "_" + st

    os.system("mkdir " + f_dic)


def set_assumption(l):
    # 2 -> assumption two: fix data section starting address
    # Note that assumption two require linker script to reassemble!
    # Some of the examples can be found at ./ld_scripts/*
    # 3 -> assumption three: function starting address + jump table
    # _ -> not defined.

    a = 0
    b = 0

    if not l:
        with open("assumption_set.info", "w") as f:
            f.writelines(["1\n"])

    else:
        chk = (i in ["2", "3"] for i in l)

        if any(chk) == False:
            print("assumption undefined!")
            print(
                "accecpt assumptions: 2 for assumption two and 3 for assumption three"
            )
            return False

        l = set(l)

        l = " ".join(l)
        l += "\n"

        with open("assumption_set.info", "w") as f:
            f.writelines(l)

    return True


def is_unstripped(b):
    try:
        result = os.popen(f"file {b}").read()
        return "not stripped" in result
    except Exception as e:
        print(f"Error checking if binary is unstripped: {e}")
        return False


def strip_elf(f):
    stripped_path = os.path.basename(f) + "_stripped"
    os.system(f"strip -o {stripped_path} {f}")
    return stripped_path


if __name__ == "__main__":
    p = ArgumentParser(formatter_class=RawTextHelpFormatter)
    p.add_argument(
        "binary", help="path to the input binary, for example, /usr/bin/ls", default=0
    )
    p.add_argument(
        "-i",
        "--iteration",
        type=int,
        help="the number of disassemble-(instrument)-reassemble iterations",
        default=0,
    )
    p.add_argument(
        "-k",
        "--keep",
        action="count",
        help="if multiple iteration processing, whether to keep itermediate binaries",
        default=0,
    )
    p.add_argument(
        "-a",
        "--assumption",
        action="append",
        help="""this option configures three additional assumption,
note that two basic assumptions and additional assumption one
(n-byte alignment) are set by default,
while assumption two and three need to be configured. For example, setting
assumption two and three: -a 2 -a 3""",
        default=0,
    )
    p.add_argument(
        "-u",
        "--unstripped",
        action="store_true",
        help="enable unstripped mode; expects the input binary to be unstripped",
        default=0,
    )
    p.add_argument(
        "--arch",
        help="specify the architecture of the input binary, intel or arm. default=intel",
        default="intel",
    )

    args = p.parse_args()
    b = args.binary
    i = args.iteration
    iter_num = i
    arch = args.arch
    k = args.keep > 0
    u = args.unstripped

    # if unstripped mode is enabled, check if the input binary is unstripped
    if u:
        if is_unstripped(b):
            print("unstripped mode")
        else:
            print(
                "Error: you are in unstripped mode, but the input binary is not unstripped."
            )
            exit(1)

    # temporal solution: use stripped mode to generate final.s first, then replace the symbols
    f = os.path.basename(b)
    if u:
        f = strip_elf(f)

    if (
        check(b, f, args.assumption) == False
        or set_assumption(args.assumption) == False
    ):
        pass
    else:
        if k:
            fold_withtamp(f)

        if args.iteration:
            if iterate(f, i, arch):
                print("processing succeeded")
            else:
                print("exception, processing failed")
        else:
            if process(f, 0, arch):
                print("processing succeeded")
            else:
                print("exception, processing failed")
