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

            # Extract target address
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
        original_line = f.readline()
        parts = original_line.strip().split()
        if len(parts) != 4:
            print("Error: Invalid text_sec.info format")
            sys.exit(1)

        text_start = int(parts[1], 16)

    # Remove duplicate address ranges and those before text_start
    # e.g., (0x1000, 0x1100), (0x1050, 0x1100) -> (0x1000, 0x1100)
    filtered_address_pair = []
    for arm32_range_pair in from_to_address_pair:
        if arm32_range_pair[0] < text_start:
            continue

        if not filtered_address_pair:
            filtered_address_pair.append(arm32_range_pair)
            continue

        last_pair = filtered_address_pair[-1]
        if arm32_range_pair[0] >= last_pair[0] and arm32_range_pair[1] <= last_pair[1]:
            # Duplicate range, skip
            continue
        else:
            filtered_address_pair.append(arm32_range_pair)

    # debug print
    # for pair in filtered_address_pair:
    #     print(f"From {hex(pair[0])} to {hex(pair[1])}")

    # Open and extract instructions between address ranges
    arm32_filename = f"{program_name}.temp.arm32"
    arm32_instructions = []
    with open(arm32_filename, "r") as f:
        lines = f.readlines()
        for arm32_range_pair in filtered_address_pair:
            for original_line in lines:
                parsed_line = original_line.strip().split("\t")
                if len(parsed_line) < 4:
                    continue

                inst_addr_str = parsed_line[0].strip(":")
                opcode = parsed_line[2]
                operands = parsed_line[3]

                try:
                    inst_addr = int(inst_addr_str, 16)
                except ValueError:
                    continue

                if inst_addr >= arm32_range_pair[0] and inst_addr < arm32_range_pair[1]:
                    arm32_instructions.append(original_line.strip())

    if not filtered_address_pair:
        print("No ARM32 instructions found to replace.")
        return

    # Store metadata about replaced ARM32 ranges
    metadata_filename = f"arm32_replaced.info"
    with open(metadata_filename, "w") as f:
        for arm32_range_pair in filtered_address_pair:
            f.write(f"{hex(arm32_range_pair[0])}:{hex(arm32_range_pair[1])}\n")

    # debug print
    # for i in arm32_instructions:
    #     print(i)

    # Replace arm32 instructions into thumb disassembly file.
    # If an instruction address within the range matches, replace it.
    new_content = []
    temp_filename = f"{program_name}.temp"
    with open(temp_filename, "r") as f:
        lines = f.readlines()
        arm32_range_pair = filtered_address_pair.pop(0)
        for original_line in lines:
            # print("LINE:", line)
            parsed_line = original_line.strip().split("\t")
            if len(parsed_line) < 4:
                continue

            inst_addr_str = parsed_line[0].strip(":")
            inst_addr = None
            try:
                inst_addr = int(inst_addr_str, 16)
            except ValueError:
                continue

            arm32_start_addr = arm32_range_pair[0]
            arm32_end_addr = arm32_range_pair[1]
            if inst_addr < arm32_start_addr:
                new_content.append(original_line)
                continue
            elif inst_addr >= arm32_start_addr and inst_addr < arm32_end_addr:
                # within range
                continue
            elif inst_addr >= arm32_end_addr:
                if arm32_instructions:
                    # replace logic
                    while arm32_instructions:
                        arm32_inst_line = arm32_instructions.pop(0)
                        arm32_inst_parsed = arm32_inst_line.strip().split("\t")
                        arm32_inst_addr_str = arm32_inst_parsed[0].strip(":")
                        arm32_instr_addr = None
                        try:
                            arm32_instr_addr = int(arm32_inst_addr_str, 16)
                        except ValueError:
                            continue

                        if inst_addr >= arm32_instr_addr:
                            new_content.append("   " + arm32_inst_line + "\n")
                        else:
                            break
                new_content.append(original_line)

    # Write back to the temp file
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
