def check_switch_pattern(lines):
    if len(lines) < 6:
        return False

    if (
        "cmp" in lines[0]
        and "bhi" in lines[1]
        and "add" in lines[2]
        and "ldr" in lines[3]
        and "add" in lines[4]
        and "bx" in lines[5]
    ):
        return True


def handle_switch_tables(filename):
    """
    Before:
    cmp r3,#0x79
    bhi.w S_0x12C54
    add r2,pc,#0x8
    ldr.w r3,[r2,r3,lsl #2]
    add r2,r3
    lsls r7,r1,#0xd
    ...

    After:
    cmp r3,#0x79
    bhi.w S_0x12C54
    ldr r3,=switch_table_1
    ldr.w r3,[r2,r3,lsl #2]
    add r2,r3
    lsls r7,r1,#0xd
    .align 2
    switch_table_1:
    ...
    """

    switch_table_counter = 0
    new_content = []
    switch_table_detected = False
    with open(filename, "r") as f:
        lines = f.readlines()
        for index, line in enumerate(lines):
            new_content.append(line)
            # if "bhi" in line:
            #     # align "bhi" in case it is related to swtich statement
            #     new_content.append(".align 4\n")

            if index > 6 and check_switch_pattern(lines[index - 5:index + 1]):
                switch_table_detected = True

            if switch_table_detected:
                if "nop" in lines[index + 1]:
                    continue
                else:
                    switch_table_counter += 1
                    switch_table_detected = False
                    switch_label = f"switch_table_{switch_table_counter}"
                    new_content.append(".align 2\n")
                    new_content.append(f"{switch_label}:\n")

                    # find the add pc line
                    start_index = index - 5
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


def main(argv):
    if len(argv) != 2:
        print("Usage: python arm_postprocess.py <path_to_assembly_file>")
        sys.exit(1)

    assembly_path = argv[1]

    # The order of these functions could matter
    handle_switch_tables(assembly_path)
    remove_unused_literal_pools_in_data(assembly_path)
    remove_caret(assembly_path)
    insert_ltorg_directive(assembly_path)
    # remove_invalid_d2c_labels(assembly_path)


if __name__ == "__main__":
    import sys

    main(sys.argv)
