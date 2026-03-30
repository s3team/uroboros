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
        if stripped.endswith(":") and not any(
            pattern in line.lower()
            for pattern in ["cmp", "bhi", "add", "ldr", "bx", "mov", "sub"]
        ):
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
                start_index_in_new_content = (
                    len(new_content) - (index - start_index) - 3
                )
                new_content[start_index_in_new_content] = new_ldr_line
                nop_offset = None

            # start from the last line of the pattern (i.e., bx instruction)
            if index < 6:
                continue

            if "bx" in line:
                is_pattern_found, pattern_length = check_switch_pattern(
                    lines[index - 6 :]
                )
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


def remove_width_specifiers(filename):
    new_content = []
    with open(filename, "r") as f:
        lines = f.readlines()
        for line in lines:
            # Remove .w and .n width specifiers from instructions
            new_line = line.replace(".w", "").replace(".n", "")
            new_content.append(new_line)

    with open(filename, "w") as f:
        f.writelines(new_content)


def convert_symbol_to_use_movw_movt(filename):
    """
    Use movw/movt to load the address of a symbol,
    instead of using `.ltorg` directive which is harder to handle,
    Below are two cases that we want to convert:

    Case 1)
    Before:
    ldr r3,=S_0x179F4

    After:
    movw r3,#:lower16:S_0x179F4
    movt r3,#:upper16:S_0x179F4

    Case 2)
    Before:
    b S_0x12744

    After:
    ldr pc,=S_0x12744
    """
    new_content = []
    with open(filename, "r") as f:
        lines = f.readlines()
        for line in lines:
            if "ldr" in line and "=" in line:
                parts = line.split()
                opcode = parts[0].strip()
                operands = parts[1].split(",")
                dst_reg = operands[0].strip()
                symbol = operands[1].lstrip("=").strip()
                new_content.append(f"movw {dst_reg},#:lower16:{symbol}\n")
                new_content.append(f"movt {dst_reg},#:upper16:{symbol}\n")
            elif line.strip().startswith("b ") and "S_" in line:
                parts = line.split()
                symbol = parts[1]
                new_content.append(f"ldr pc,={symbol}\n")
            else:
                new_content.append(line)

    with open(filename, "w") as f:
        f.writelines(new_content)


import re as _re

# Matches cbz/cbnz instructions, capturing indent, mnemonic, register, and target.
_CBZ_RE = _re.compile(r'^(\s*)(cbnz|cbz)\s+(\w+),(.+?)\s*$')
# Matches a label definition at the start of a (stripped) line.
_LABEL_DEF_RE = _re.compile(r'^([A-Za-z_.][A-Za-z0-9_.]*):\s*')


def _thumb_instr_size(opcode, operands_str):
    """Return estimated byte size of a Thumb-2 instruction.

    Returns 2 for instructions that are always 16-bit, 4 otherwise.
    """
    op = opcode.lower()
    operands = [o.strip() for o in operands_str.split(',')]

    if op in ('cbz', 'cbnz', 'nop', 'bkpt'):
        return 2
    if op.endswith('.n'):
        return 2
    if op.endswith('.w'):
        return 4
    if op in ('bl', 'blx') and (not operands or not _re.match(r'^r\d+$', operands[0])):
        return 4  # branch-to-label form
    # bx rN / blx rN (register form) → 16-bit
    if op in ('bx', 'blx') and operands and _re.match(r'^r\d+$', operands[0]):
        return 2
    # movs rN, rM  (low regs) or  movs rN, #imm8 → 16-bit
    if op == 'movs' and len(operands) >= 2:
        dst, src = operands[0], operands[1]
        if _re.match(r'^r[0-7]$', dst) and _re.match(r'^r[0-7]$', src):
            return 2
        if _re.match(r'^r[0-7]$', dst) and _re.match(r'^#(?:0x[\da-fA-F]+|\d+)$', src):
            try:
                if 0 <= int(src.lstrip('#'), 0) <= 255:
                    return 2
            except ValueError:
                pass
    return 4  # conservative default: assume 32-bit


def _build_offset_map(lines):
    """Estimate byte offsets for every line and collect label→offset mappings.

    Tracks literal-pool entries queued by  ldr r,=symbol  instructions and
    flushes them (4 bytes each, 4-byte aligned) at each .ltorg directive.

    Returns:
        line_offsets  – list[int]: estimated byte offset at the start of each line
        label_offsets – dict[str, int]: label name → estimated byte offset
    """
    label_offsets = {}
    line_offsets = []
    pending_literals = 0
    current_offset = 0
    in_text = False

    for line in lines:
        line_offsets.append(current_offset)
        stripped = line.strip()

        if '.section .text' in line:
            in_text = True
        elif in_text and '.section' in line and '.text' not in line:
            in_text = False

        if not in_text or not stripped:
            continue
        if stripped[0] in ('@', '#'):
            continue

        # Extract any label prefix (e.g. "S_0x1234: movs r0,r1" or "S_0x1234:")
        lm = _LABEL_DEF_RE.match(stripped)
        if lm:
            label_offsets[lm.group(1)] = current_offset
            stripped = stripped[lm.end():].strip()
            if not stripped:
                continue

        if stripped.startswith('.'):
            if stripped.startswith('.ltorg'):
                if current_offset % 4:
                    current_offset += 4 - (current_offset % 4)
                current_offset += pending_literals * 4
                pending_literals = 0
            elif _re.match(r'\.(long|word|4byte)\b', stripped):
                current_offset += 4
            elif _re.match(r'\.(hword|short|2byte)\b', stripped):
                current_offset += 2
            elif stripped.startswith('.byte'):
                current_offset += 1
            elif _re.match(r'\.align\b', stripped):
                try:
                    align = 1 << int(stripped.split()[1])
                    if current_offset % align:
                        current_offset += align - (current_offset % align)
                except Exception:
                    pass
            continue

        # ldr r,=symbol  queues one 4-byte literal-pool entry
        if _re.match(r'ldr\S*\s+\w+\s*,\s*=', stripped):
            pending_literals += 1
            current_offset += 4
            continue

        parts = stripped.split(None, 1)
        opcode = parts[0]
        ops_str = parts[1] if len(parts) > 1 else ''
        current_offset += _thumb_instr_size(opcode, ops_str)

    return line_offsets, label_offsets


def expand_out_of_range_cbz_cbnz(filename):
    """Expand cbz/cbnz instructions whose branch targets exceed the 126-byte limit.

    cbz/cbnz in Thumb can only branch forward up to 126 bytes from the instruction's
    PC (instruction address + 4).  Literal-pool insertions during reassembly can push
    some targets beyond that limit.

    Transformation applied when the target is out of range:

        cbz  rN, far_target        →   cbnz rN, .Lcbz_skip_K
                                        b.w  far_target
                                        .Lcbz_skip_K:

        cbnz rN, far_target        →   cbz  rN, .Lcbz_skip_K
                                        b.w  far_target
                                        .Lcbz_skip_K:

    The generated  b.w  instructions will receive a .ltorg directive from the
    subsequent  insert_ltorg_directive  pass (which must run after this function).

    Note: cbz/cbnz that reference labels not present in the file (a separate
    labelling bug in the OCaml pipeline) are also expanded here so that the
    assembler reports "undefined symbol" rather than "branch out of range",
    making the root cause easier to identify.
    """
    with open(filename, 'r') as f:
        lines = f.readlines()

    line_offsets, label_offsets = _build_offset_map(lines)

    new_lines = []
    skip_counter = 0

    for i, line in enumerate(lines):
        m = _CBZ_RE.match(line)
        if not m:
            new_lines.append(line)
            continue

        indent, mnemonic, reg, target = (
            m.group(1), m.group(2), m.group(3), m.group(4)
        )
        instr_off = line_offsets[i]

        out_of_range = False
        if _re.match(r'^S_0x[0-9A-Fa-f]+$', target):
            tgt_off = label_offsets.get(target)
            if tgt_off is None:
                # Label defined in the file but not yet assembled (missing label
                # from the OCaml pipeline).  Expand so the assembler reports
                # "undefined symbol" rather than "branch out of range".
                out_of_range = True
            else:
                # cbz/cbnz are 16-bit; PC = instruction_address + 4.
                # Valid forward range: offset ∈ [0, 126].
                offset = tgt_off - (instr_off + 4)
                if offset < 0 or offset > 126:
                    out_of_range = True
        else:
            # Raw absolute-address target (e.g. #0x2ea56).  The binary may have
            # shifted due to inserted literal pools, making the offset negative.
            # Always expand so b.w handles the wider range.
            out_of_range = True

        if out_of_range:
            skip_counter += 1
            skip_label = f'.Lcbz_skip_{skip_counter}'
            inverse = 'cbz' if mnemonic == 'cbnz' else 'cbnz'
            # Strip leading '#' from raw hex targets: b.w does not use the '#' prefix.
            bw_target = target.lstrip('#')
            new_lines.append(f'{indent}{inverse} {reg},{skip_label}\n')
            new_lines.append(f'{indent}b.w {bw_target}\n')
            new_lines.append(f'{indent}{skip_label}:\n')
        else:
            new_lines.append(line)

    with open(filename, 'w') as f:
        f.writelines(new_lines)


def main(argv):
    if len(argv) != 3:
        print("Usage: python arm_postprocess.py <path_to_assembly_file> <arch>")
        sys.exit(1)

    assembly_path = argv[1]
    arch = argv[2]

    if arch == "thumb":
        # The order of these functions could matter
        handle_switch_tables(assembly_path)
        remove_unused_literal_pools_in_data(assembly_path)
        expand_out_of_range_cbz_cbnz(assembly_path)
        insert_ltorg_directive(assembly_path)
        remove_caret(assembly_path)
        # remove_invalid_d2c_labels(assembly_path)
    elif arch == "arm" and is_32():
        remove_width_specifiers(assembly_path)
        convert_symbol_to_use_movw_movt(assembly_path)
        remove_caret(assembly_path)
    elif arch == "arm" and not is_32():
        remove_caret(assembly_path)
    else:
        print("Unsupported architecture. Supported architectures are: thumb, arm.")
        sys.exit(1)


if __name__ == "__main__":
    import sys

    main(sys.argv)
