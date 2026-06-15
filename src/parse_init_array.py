import os


def check_32():
    with open("elf.info") as f:
        return "32-bit" in f.readlines()[0]


def not_hex(d):
    try:
        int(d, 16)
        return False
    except Exception:
        return True


def parse(item):
    h1 = item[7:].upper()
    h2 = item[4:6].upper()
    h3 = item[2:4].upper()
    h4 = item[0:2].upper()
    addr = h1 + h2 + h3 + h4

    # bit 0 of an array entry marks a Thumb function pointer
    addr_int = int(addr, 16)
    thumb_bit = addr_int % 2
    if thumb_bit == 1:
        addr_int -= 1
    addr = format(addr_int, "08X")

    return "S_0x" + addr.lstrip("0"), thumb_bit


def parse_array(infile):
    """Parse an `objdump -s` dump of an array section into (labels, emit) lists.

    [labels] are bare `S_0x<addr>` used to create the target symbols; [emit] is
    the same with the Thumb bit folded back as `+ 1` for re-emission.
    """
    try:
        with open(infile) as f:
            lines = f.readlines()
    except FileNotFoundError:
        return [], []

    start_index = 0
    for i, l in enumerate(lines):
        if "not found" in l:
            return [], []
        elif "Contents of section" in l:
            start_index = i + 1

    labels = []
    emit = []
    for l in lines[start_index:]:
        items = l.strip().split()
        for item in items[1:]:
            if len(item) != 8 or not_hex(item):
                break
            label, thumb_bit = parse(item)
            if len(label) == 4:
                continue
            labels.append(label + "\n")
            emit.append(label + (" + 1" if thumb_bit else "") + "\n")
    return labels, emit


rtp = os.path.exists("vxworks_rtp.info")

labels, emit = parse_array("init_array.info")
with open("init_array_new.info", "w") as f:
    f.writelines(labels)
with open("init_array_emit.info", "w") as f:
    f.writelines(emit if rtp else emit[1:])

if rtp:
    flabels, femit = parse_array("fini_array.info")
    with open("fini_array_new.info", "w") as f:
        f.writelines(flabels)
    with open("fini_array_emit.info", "w") as f:
        f.writelines(femit)
