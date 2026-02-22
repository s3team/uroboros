def is_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "32-bit" in lines[0]:
        return True
    else:
        return False

def check_switch_pattern(lines):
    """
    Check if lines contain a switch table pattern.

    Returns:
        tuple: (bool, int) indicating if the pattern was found and the span length.
    """
    if len(lines) < 6:
        return (False, 0)

    # Filter out label lines (e.g., "BB_17:") and track their positions
    instruction_indices = []
    for i, line in enumerate(lines):
        stripped = line.strip()
        # Skip lines that are only labels (end with : and don't contain instruction keywords)
        if stripped.endswith(':') and not any(pattern in line.lower() for pattern in ['cmp', 'bhi', 'add', 'ldr', 'bx', 'mov', 'sub']):
            continue
        instruction_indices.append(i)

    # Need at least 6 instruction lines for the pattern
    if len(instruction_indices) < 6:
        return (False, 0)

    # Check the pattern against the first 6 instructions
    instruction_lines = [lines[i] for i in instruction_indices[:6]]

    if (
        "cmp" in instruction_lines[0]
        and "bhi" in instruction_lines[1]
        and "add" in instruction_lines[2]
        and "ldr" in instruction_lines[3]
        and "add" in instruction_lines[4]
        and "bx" in instruction_lines[5]
    ):
        # Calculate the span: from start to the end of the 6th instruction
        pattern_length = instruction_indices[5] + 1
        return (True, pattern_length)

    return (False, 0)


def handle_switch_tables(filename):
    """
    Before:
    cmp r3,#0x79
    bhi.w S_0x12C54
    add r2,pc,#0x8
    ldr.w r3,[r2,r3,lsl #2]
    add r2,r3
    bx r2
    lsls r7,r1,#0xd
    ...

    After:
    cmp r3,#0x79
    bhi.w S_0x12C54
    ldr r3,=switch_table_1
    ldr.w r3,[r2,r3,lsl #2]
    add r2,r3
    lsls r7,r1,#0xd
    bx r2
    .align 2
    switch_table_1:
    ...

    Note that final.s might have labels like "BB_17:" or "nop" within switch pattern.
    e.g.:
    cmp r3,#0x79
    bhi.w S_0x12C54
    BB_17: # need to ignore this label
    add r2,pc,#0x8
    ldr.w r3,[r2,r3,lsl #2]
    add r2,r3
    bx r2
    BB_18:
    nop
    lsls r7,r1,#0xd
    """

    switch_table_counter = 0
    new_content = []
    with open(filename, "r") as f:
        lines = f.readlines()
        nop_offset = None
        for index, line in enumerate(lines):
            new_content.append(line)

            if nop_offset is not None and nop_offset > 0:
                nop_offset -= 1
                continue
            elif nop_offset == 0:
                new_content.append(".align 2\n")
                new_content.append(f"{switch_label}:\n")

                # find the add pc line
                start_index = index - 7
                add_pc_line = None
                while True:
                    if "add" in lines[start_index] and "pc" in lines[start_index]:
                        add_pc_line = lines[start_index]
                        break
                    start_index += 1

                # replace the add line with ldr line
                dst_reg = add_pc_line.split(",")[0].strip().split(" ")[-1]
                new_ldr_line = f"ldr {dst_reg},={switch_label}\n"
                start_index_in_new_content = len(new_content) - (index - start_index) - 3
                new_content[start_index_in_new_content] = new_ldr_line
                nop_offset = None

            # start from the last line of the pattern (i.e., bx instruction)
            if index < 6:
                continue

            if "bx" in line:
                is_pattern_found, pattern_length = check_switch_pattern(lines[index - 6:])
                if is_pattern_found:
                    # find nop from start_index_to_find_nop to 3 lines after the pattern
                    start_index_to_find_nop = index - 7 + pattern_length
                    nop_offset = None
                    for i in range(4):
                        if "nop" in lines[start_index_to_find_nop + i]:
                            nop_offset = i - 1
                            break

                    switch_table_counter += 1
                    switch_label = f"switch_table_{switch_table_counter}"

    with open(filename, "w") as f:
        f.writelines(new_content)


def remove_unused_literal_pools_in_data(filename):
    """
    Remove unused data in the second text section.
    The data is used to symbolize literal pools from the original binary.
    """

    new_content = []
    with open(filename, "r") as f:
        text_section_count = 0
        is_going_through_second_text_section = False
        symbol_detected = 0
        lines = f.readlines()
        for line in lines:
            if ".section" in line and ".text" in line:
                text_section_count += 1

            if text_section_count == 2:
                is_going_through_second_text_section = True

            if not is_going_through_second_text_section:
                new_content.append(line)
            else:
                if ".section" in line and ".text" in line:
                    new_content.append(line)
                elif "S_0x" in line:
                    symbol_detected += 4
                    new_content.append(line)
                elif symbol_detected > 0:
                    symbol_detected -= 1
                    new_content.append(line)
                elif ".section" in line and ".rodata" in line:
                    is_going_through_second_text_section = False
                    text_section_count += 1
                    new_content.append(line)
                else:
                    # skip the line because it is not a symbol
                    continue

    with open(filename, "w") as f:
        f.writelines(new_content)


def remove_caret(filename):
    """
    Remove the caret (^) from the assembly file.
    The caret is used to indicate inline shifts in ARM assembly.
    """

    new_content = []
    with open(filename, "r") as f:
        lines = f.readlines()
        for line in lines:
            # Remove the caret from the line
            new_line = line.replace("^", " ")
            new_content.append(new_line)

    with open(filename, "w") as f:
        f.writelines(new_content)


def insert_ltorg_directive(filename):
    """
    Add .ltorg directive right after "b.w S_:" labels in the text section.
    """

    is_text_section = False
    # main_detected = False
    new_content = []
    ltorg_line = ".ltorg\n"
    with open(filename, "r") as f:
        lines = f.readlines()
        for line in lines:
            # detect text section
            if ".section" in line and ".text" in line:
                is_text_section = True
            elif is_text_section and ".section" in line:
                is_text_section = False

            # if ".globl main" in line:
            #     main_detected = True
            #     new_content.append(ltorg_line)

            # if is_text_section and "S_" in line and ":" in line and "b.w" in line:
            #     if main_detected:
            #         # do not add ltorg again
            #         main_detected = False
            #     else:
            #         new_content.append(ltorg_line)

            if is_text_section and "S_" in line and "b.w" in line:
                new_content.append(line)
                new_content.append(ltorg_line)
            else:
                new_content.append(line)

    with open(filename, "w") as f:
        f.writelines(new_content)


def remove_invalid_d2c_labels(filename):
    new_content = []
    with open(filename, "r") as f:
        in_text_section = False
        in_rodata_section = False
        labels = []

        lines = f.readlines()
        for line in lines:
            if ".section .text" in line:
                in_text_section = True
            if ".section .rodata" in line:
                in_rodata_section = True

            if in_text_section and "S_" in line and ":" in line:
                labels.append(line.strip().rstrip(":"))

            if in_rodata_section and ".long S_" in line:
                label_found = False
                for label in labels:
                    if label in line:
                        label_found = True
                        break

                if not label_found:
                    # skip this line
                    continue

            new_content.append(line)

    with open(filename, "w") as f:
        f.writelines(new_content)


def remove_unused_literal_pools_in_data(filename):
    """
    Remove unused data in the second text section.
    The data is used to symbolize literal pools from the original binary.
    """

    new_content = []
    with open(filename, "r") as f:
        text_section_count = 0
        is_going_through_second_text_section = False
        symbol_detected = 0
        lines = f.readlines()
        for line in lines:
            if ".section" in line and ".text" in line:
                text_section_count += 1

            if text_section_count == 2:
                is_going_through_second_text_section = True

            if not is_going_through_second_text_section:
                new_content.append(line)
            else:
                if ".section" in line and ".text" in line:
                    new_content.append(line)
                elif "S_0x" in line:
                    symbol_detected += 4
                    new_content.append(line)
                elif symbol_detected > 0:
                    symbol_detected -= 1
                    new_content.append(line)
                elif ".section" in line and ".rodata" in line:
                    is_going_through_second_text_section = False
                    text_section_count += 1
                    new_content.append(line)
                else:
                    # skip the line because it is not a symbol
                    continue

    with open(filename, "w") as f:
        f.writelines(new_content)


def main(argv):
    if len(argv) != 2:
        print("Usage: python arm_postprocess.py <path_to_assembly_file>")
        sys.exit(1)

    assembly_path = argv[1]

    # The order of these functions could matter
    handle_switch_tables(assembly_path)
    remove_unused_literal_pools_in_data(assembly_path)
    remove_caret(assembly_path)
    if is_32():
        insert_ltorg_directive(assembly_path)
    # remove_invalid_d2c_labels(assembly_path)


if __name__ == "__main__":
    import sys

    main(sys.argv)
