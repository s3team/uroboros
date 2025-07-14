def adjust_alignment(filename):
    new_content = []
    with open(filename, "r") as f:
        lines = f.readlines()
        for line in lines:
            new_content.append(line)
            if "bhi" in line:
                # align "bhi" in case it is related to swtich statement
                new_content.append(".align 4\n")

    with open(filename, "w") as f:
        f.writelines(new_content)


def remove_unused_literal_pools(filename):
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
            if ".section .text" in line:
                text_section_count += 1

            if text_section_count == 2:
                is_going_through_second_text_section = True

            if not is_going_through_second_text_section:
                new_content.append(line)
            else:
                if "S_0x" in line:
                    symbol_detected += 4
                    new_content.append(line)
                elif symbol_detected > 0:
                    symbol_detected -= 1
                    new_content.append(line)
                elif ".section .rodata" in line:
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


def main(argv):
    if len(argv) != 2:
        print("Usage: python arm_postprocess.py <path_to_assembly_file>")
        sys.exit(1)

    assembly_path = argv[1]
    adjust_alignment(assembly_path)
    remove_unused_literal_pools(assembly_path)
    remove_caret(assembly_path)


if __name__ == "__main__":
    import sys

    main(sys.argv)
