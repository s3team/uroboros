#!/usr/bin/env python3
import time
import datetime
import argparse
import sys
import os
import subprocess
import shutil
import tempfile
import arm_preprocess

from argparse import RawTextHelpFormatter
from argparse import ArgumentParser


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
    result = subprocess.run("ls | grep '\\.o$'", shell=True, capture_output=True, text=True)
    result = result.stdout.strip().split('\n') if result.stdout else []
    if cleanup or not os.path.exists("points.ins"):
        return list(set(result))
    with open("points.ins") as points:
        p = points.readlines()
        for line in p:
            if not line.startswith("INCLUDE"):
                continue
            line_nocmd = line.split('"')[0]
            libs = [os.path.basename(l) for l in line_nocmd[8:].split() if l]
            result.extend(libs)
    return list(set(result))


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
        compile_option += "-march=armv7-a -mthumb -mfpu=vfpv3-d16 -mfloat-abi=hard"
    elif arch == "arm":
        if is_32bit_binary:
            compiler = "arm-linux-gnueabihf-gcc"
            compile_option += "-marm "
        else:
            compiler = "aarch64-linux-gnu-gcc"

    if is_static() == True:
        compile_option += "-static"

    for obj in custom_objects:
        print(f"custom_object: {obj}")
        compile_option += f" {obj} "

    print(f"reassemble: {compiler} {assembly_file} {compile_option}")
    os.system(f"{compiler} {assembly_file} {compile_option}")


def plt_changed():
    with open("plt_sec.info") as plt_sec, open("plt_sec_re.info") as plt_sec_re:
        plt_sec_line = plt_sec.readlines()[0]
        plt_sec_re_line = plt_sec_re.readlines()[0]
        plt_sec_size = plt_sec_line.split()[3]
        plt_sec_re_size = plt_sec_re_line.split()[3]
        return plt_sec_size != plt_sec_re_size


def get_plt_info(plt_sec_file):
    with open(plt_sec_file) as plt_sec, open("plt_entries.info") as plt_entries:
        plt_sec_line = plt_sec.readlines()[0]
        plt_sec_start = int("0x"+plt_sec_line.split()[1], 16)
        plt_sec_size = int("0x"+plt_sec_line.split()[3], 16)
        plt_sec_entry_num = int(plt_entries.readlines()[0])
        size_per_entry = int(plt_sec_size / plt_sec_entry_num)
        return plt_sec_start, plt_sec_entry_num, size_per_entry


def update_final(plt_addr2entry, plt_new_entry2addr):
    lines = list()
    with open("final.s", "r") as f:
        lines = f.readlines()
    with open("final2.s", "w") as f:
        for line in lines:
            if not line.startswith("call "):
                f.write(line)
                continue
            op, addr = line.split()
            if addr not in plt_addr2entry:
                f.write(line)
                continue
            entry = plt_addr2entry[addr]
            new_addr = plt_new_entry2addr[entry]
            f.write(f"{op} {new_addr}\n")


def check_thumb_mode(arch: str, entry_point: int) -> bool:
    return arch == "thumb"


def get_entry_point_address(fn) -> str:
    output = subprocess.getoutput("readelf -h " + fn)
    entry_point = ""

    for line in output.split('\n'):
        if "Entry point address" in line:
            entry_point = line.split()[-1][2:]
            break

    return entry_point


def dump(fn):
    entry_point_str = get_entry_point_address(fn)
    is_thumb = check_thumb_mode(arch, int(entry_point_str, 16))
    is_32bit_binary = check_32()

    if arch == "intel":
        os.system(f"objdump -Dr -j .text {fn} > {fn}.temp")
    elif arch == "thumb" or arch == "arm":
        if is_32bit_binary:
            if is_thumb:
                arm_preprocess.disassemble_arm_thumb_binary(fn, None)
            else:
                arm_preprocess.disassemble_arm32_binary(fn, None)
        else:
            os.system(f"aarch64-linux-gnu-objdump -Dr -j .text {fn} > {fn}.temp")
    else:
        print("dump: Invalid architecture")
        raise Exception("Invalid architecture")


def process(f, i, arch):
    is_32bit_binary = is_32()
    strip_command = ""
    if arch == "intel":
        strip_command = "strip"
    elif arch == "thumb":
        strip_command = "arm-linux-gnueabihf-strip"
    elif arch == "arm":
        if is_32bit_binary:
            strip_command = "arm-linux-gnueabihf-strip"
        else:
            strip_command = "aarch64-linux-gnu-strip"

    try:
        os.system("rm final_*.txt")

        # suppose we use this method to obtain function information
        os.system("cp " + f + " func_discover/")
        os.system(f"python3 func_discover/func_addr.py func_discover/{f} {str(i)} {arch}")
        if os.path.exists("final_data.s"):
            os.system("rm final_data.s")
        if os.path.exists("useless_func.info"):
            os.system("rm useless_func.info")
        os.system(f"rm func_discover/{f}")
        if i > 0:
            os.system(f"python3 useless_func_discover.py {f} {arch}")

        os.system(f"echo \"{str(i)}\" > count.txt")
        os.system(f"cp {f} {f}.sym")
        os.system(f"nm {f}.sym > nm.info")
        if is_static():
            os.system(f"objdump -d --section=.plt {f} > plt_whole.info")
            os.system(f"readelf -r {f} > rela_plt.info")
            os.system(f"readelf -l {f} > headers.info")
        os.system(f"{strip_command} {f}")

        dump(f)

        bit_mode = "32" if is_32bit_binary else "64"
        cmd = f"opam exec -- dune exec init {f} {arch} {bit_mode}"
        init_result = subprocess.run(cmd, shell=True)
        if init_result.returncode != 0:
            print("init failed")
            return False


        os.system(f"python3 main_discover.py {f} {arch}")

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

        os.system(f"python3 compile_process.py {arch}")
        os.system("python3 label_adjust.py")

        if arch == "thumb" or arch == "arm":
            os.system("python3 arm_postprocess.py final.s")

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


def iterate (f, iterations, arch):
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
    p.add_argument("--arch", help="specify the architecture of the input binary, intel or arm. default=intel", default="intel")

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
                if u:
                    os.system("python3 symbolize.py " + b)
                print("processing succeeded")
            else:
                print("exception, processing failed")
