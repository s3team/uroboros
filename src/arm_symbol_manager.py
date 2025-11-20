#!/usr/bin/env python3

import sys


# Pre-compute all valid branch opcodes for O(1) lookup
_VALID_BRANCH_OPCODES = None


def _init_branch_opcodes():
    """Initialize the set of valid branch opcodes (called once)."""
    branch_opcodes = ["b", "bl", "bx", "blx"]
    cond_suff = [
        "",
        "eq",
        "ne",
        "cs",
        "cc",
        "mi",
        "pl",
        "vs",
        "vc",
        "lo",
        "hi",
        "ls",
        "ge",
        "lt",
        "gt",
        "le",
        "al",
        "hs",
    ]
    suffixes = ["", ".w", ".n"]

    opcodes = set()
    for b in branch_opcodes:
        for c in cond_suff:
            for s in suffixes:
                opcodes.add(b + c + s)
    return opcodes


def is_branch_opcode(opcode: str) -> bool:
    """
    Check if opcode is a valid ARM branch instruction.
    """
    global _VALID_BRANCH_OPCODES
    if _VALID_BRANCH_OPCODES is None:
        _VALID_BRANCH_OPCODES = _init_branch_opcodes()

    return opcode in _VALID_BRANCH_OPCODES


def branch_target_extractor(filename) -> list[tuple[int, str, int]]:
    """
    Return:
        A list of an address, opcode and target address of a branch instruction.
    """

    # Filter if target addresses are out of text range
    # .text 00010668 000668 000e0c
    text_start = None
    text_size = None
    with open("text_sec.info", "r") as f:
        line = f.readline()
        parts = line.strip().split()
        if len(parts) != 4:
            print("Error: Invalid text_sec.info format")
            sys.exit(1)

        text_start = int(parts[1], 16)
        text_size = int(parts[3], 16)
    text_end = text_start + text_size

    targets = []
    with open(filename, "r") as f:
        lines = f.readlines()
        for line in lines:
            parsed_line = line.strip().split("\t")
            if len(parsed_line) < 4:
                continue

            addr = parsed_line[0].strip(":")
            opcode = parsed_line[2]
            operands = parsed_line[3]
            if not is_branch_opcode(opcode):
                continue

            # # Extract target address
            target = operands.split()[0]
            target_addr = None
            if "0x" in target:
                target_addr = int(target, 16)
            else:
                try:
                    target_addr = int(target)
                except ValueError:
                    # print(f"Warning: Unable to parse target address '{target}'")
                    continue
            if target_addr > text_end:
                continue

            targets.append((addr, opcode, target_addr))

    return targets


def extract_arm32_instructions(program_name):
    filename = f"{program_name}.temp"
    from_to_address_pair = []
    blx_addresses = set()
    bl_addresses = set()
    targets = branch_target_extractor(filename)
    for _, opcode, target_addr in targets:
        if "blx" in opcode:
            blx_addresses.add(target_addr)
        elif "bl" in opcode:
            bl_addresses.add(target_addr)

    sorted_blx_addresses = sorted(blx_addresses)
    sorted_bl_addresses = sorted(bl_addresses)
    for blx_addr in sorted_blx_addresses:
        for bl_addr in sorted_bl_addresses:
            if bl_addr > blx_addr:
                from_to_address_pair.append((blx_addr, bl_addr))
                break

    # Get text start address
    text_start = None
    with open("text_sec.info", "r") as f:
        line = f.readline()
        parts = line.strip().split()
        if len(parts) != 4:
            print("Error: Invalid text_sec.info format")
            sys.exit(1)

        text_start = int(parts[1], 16)

    # Remove duplicate address ranges and those before text_start
    # e.g., (0x1000, 0x1100), (0x1050, 0x1100) -> (0x1000, 0x1100)
    filtered_address_pair = []
    for pair in from_to_address_pair:
        if pair[0] < text_start:
            continue

        if not filtered_address_pair:
            filtered_address_pair.append(pair)
            continue

        last_pair = filtered_address_pair[-1]
        if pair[0] >= last_pair[0] and pair[1] <= last_pair[1]:
            # Duplicate range, skip
            continue
        else:
            filtered_address_pair.append(pair)

    # debug print
    # for pair in filtered_address_pair:
    #     print(f"From {hex(pair[0])} to {hex(pair[1])}")

    # open and extract instructions between address ranges
    arm32_filename = f"{program_name}.temp.arm32"
    arm32_instructions = []
    with open(arm32_filename, "r") as f:
        lines = f.readlines()
        for pair in filtered_address_pair:
            for line in lines:
                parsed_line = line.strip().split("\t")
                if len(parsed_line) < 4:
                    continue

                addr = parsed_line[0].strip(":")
                opcode = parsed_line[2]
                operands = parsed_line[3]

                try:
                    addr_int = int(addr, 16)
                except ValueError:
                    continue

                if addr_int >= pair[0] and addr_int < pair[1]:
                    arm32_instructions.append(line.strip())

    if not filtered_address_pair:
        print("No ARM32 instructions found to replace.")
        return

    # debug print
    # for i in arm32_instructions:
    #     print(i)

    # replace arm32 instructions into thumb disassembly file
    # if an instruction address within the range matches, replace it
    new_content = []
    temp_filename = f"{program_name}.temp"
    with open(temp_filename, "r") as f:
        lines = f.readlines()
        pair = filtered_address_pair.pop(0)
        for line in lines:
            # print("LINE:", line)
            parsed_line = line.strip().split("\t")
            if len(parsed_line) < 4:
                continue

            addr = parsed_line[0].strip(":")
            addr_int = None
            try:
                addr_int = int(addr, 16)
            except ValueError:
                continue

            if addr_int < pair[0]:
                new_content.append(line)
                continue
            elif addr_int >= pair[0] and addr_int < pair[1]:
                # within range
                continue
            elif addr_int >= pair[1]:
                # replace logic
                while arm32_instructions:
                    inst_line = arm32_instructions.pop(0)
                    inst_parsed = inst_line.strip().split("\t")
                    inst_addr = inst_parsed[0].strip(":")
                    inst_addr_int = None
                    try:
                        inst_addr_int = int(inst_addr, 16)
                    except ValueError:
                        continue

                    if addr_int >= inst_addr_int:
                        new_content.append("   " + inst_line + "\n")
                    else:
                        break

                new_content.append(line)

    with open(temp_filename, "w") as f:
        f.writelines(new_content)


def test_branch_target_extractor(program_name):
    filename = f"{program_name}.temp"
    targets = branch_target_extractor(filename)
    blx_target_addr_set = set()
    bl_target_addr_set = set()
    for _, opcode, target_addr in targets:
        if "blx" in opcode:
            # Skip duplicates
            if target_addr in blx_target_addr_set:
                continue

            blx_target_addr_set.add(target_addr)
        elif "bl" in opcode:
            if target_addr in bl_target_addr_set:
                continue

            bl_target_addr_set.add(target_addr)

    for target_addr in sorted(blx_target_addr_set):
        hexaddr = hex(target_addr).lstrip("0x")
        print(f"blx: {hexaddr}")
    for target_addr in sorted(bl_target_addr_set):
        hexaddr = hex(target_addr).lstrip("0x")
        print(f"bl: {hexaddr}")


def main(argv):
    if len(argv) != 2:
        print(f"Usage: {argv[0]} <assembly_file>")
        sys.exit(1)

    # test_branch_target_extractor(argv[1])
    extract_arm32_instructions(argv[1])


if __name__ == "__main__":
    main(sys.argv)
