from pathlib import Path


def check_static():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "statically linked" in lines[0]:
        return True
    else:
        return False


def split_text_info():
    lines = []
    with open("text.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])
    s = "\n".join(reversed(s))

    with open("text_split.info", "w") as f:
        f.write(s + "\n")


def split_rodata_info():
    lines = []
    with open("rodata.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])
    s = "\n".join(reversed(s))

    with open("rodata_split.info", "w") as f:
        f.write(s + "\n")


def split_libc_io_vtables_info():
    lines = []
    with open("__libc_IO_vtables.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])
    s = "\n".join(reversed(s))

    with open("__libc_IO_vtables_split.info", "w") as f:
        f.write(s + "\n")


def split_data_info():
    lines = []
    with open("data.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])
    s = "\n".join(reversed(s))

    with open("data_split.info", "w") as f:
        f.write(s + "\n")


def split_tdata_info():
    # .tdata TLS template section
    lines = []
    with open("tdata.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])
    s = "\n".join(reversed(s))

    with open("tdata_split.info", "w") as f:
        f.write(s + "\n")


def split_data_rel_ro_info():
    # data.rel.ro sections
    lines = []
    with open("data_rel_ro.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])
    s = "\n".join(reversed(s))

    with open("data_rel_ro_split.info", "w") as f:
        f.write(s + "\n")


def split_rodata_cst32_info():
    # rodata.cst32 sections
    lines = []
    with open("rodata_cst32.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])

    s = "\n".join(reversed(s))

    with open("rodata_cst32_split.info", "w") as f:
        f.write(s + "\n")


def split_got_info():
    lines = []
    with open("got.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])

    s = "\n".join(reversed(s))

    with open("got_split.info", "w") as f:
        f.write(s + "\n")


def split_got_plt_info():
    lines = []
    with open("got_plt.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])
    s = "\n".join(reversed(s))

    with open("got_plt_split.info", "w") as f:
        f.write(s + "\n")


def split_eh_frame_info():
    lines = []
    with open("eh_frame.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])
    s = "\n".join(reversed(s))

    with open("eh_frame_split.info", "w") as f:
        f.write(s + "\n")


def split_eh_frame_hdr_info():
    lines = []
    with open("eh_frame_hdr.info") as f:
        lines = f.readlines()

    s = []
    for l in lines:
        items = l.split()
        for item in items:
            l = len(item)
            for i in range(0, l, 2):
                s.append(".byte 0x" + item[i : i + 2])
    s = "\n".join(reversed(s))

    with open("eh_frame_hdr_split.info", "w") as f:
        f.write(s + "\n")


if __name__ == "__main__":
    split_text_info()
    split_rodata_info()

    if Path("__libc_IO_vtables.info").exists():
        split_libc_io_vtables_info()

    split_data_info()
    split_data_rel_ro_info()
    split_rodata_cst32_info()
    split_got_info()

    if check_static():
        split_got_plt_info()
        if Path("tdata.info").exists():
            split_tdata_info()

    split_eh_frame_info()
    split_eh_frame_hdr_info()
