import os
import sys


def get_call_weak_fn_lines_from_arm32(filename) -> list[str]:
    """
    Get the call_weak_fn pattern in arm32 mode.

    Args:
        filename: A filename of a binary

    Returns:
        list[str]: A list of call_weak_fn instructions
    """

    os.system(
        f"arm-linux-gnueabihf-objdump -Dr -j .text {filename} > {filename}.temp.arm32"
    )
    # Find call_weak_fn pattern in arm32 mode,
    # then replace the call_weak_fn in thumb mode with the one in arm32 mode.
    call_weak_fn_lines_from_arm32 = []
    with open(f"{filename}.temp.arm32", "r") as f:
        is_going_through_call_weak_fn = False
        lines = f.readlines()
        for i, line in enumerate(lines):
            if "ldr" in lines[i] and "ldr" in lines[i + 1]:
                is_going_through_call_weak_fn = True

            # Collect the call_weak_fn instructions in arm32 mode
            if is_going_through_call_weak_fn:
                call_weak_fn_lines_from_arm32.append(line)

            if (
                is_going_through_call_weak_fn
                and "andeq" in lines[i - 1]
                and "andeq" in lines[i]
            ):
                break
    os.system(f"rm {filename}.temp.thumb {filename}.temp.arm32")

    return call_weak_fn_lines_from_arm32


def disassemble_arm_thumb_binary(filename, output_dir):
    """
    Create a disassembled file for ARM thumb mode binaries.

    For ARM thumb mode, we need to disassemble the binary in thumb mode and arm32 mode separately.:
    1. Disassemble the binary with thumb mode option.
    2. Disassemble the binary without thumb mode option.
    3. Combine the two disassembled files properly.

    This is because when we use objdump with "-M force-thumb" option for stripped binaries,
    it does not disassemble the call_weak_fn pattern correctly.
    See glibc-2.35 code:
    https://elixir.bootlin.com/glibc/glibc-2.35/source/sysdeps/arm/crti.S#L63
    https://github.com/bminor/glibc/blob/release/2.35/master/sysdeps/arm/crti.S#L63


    Args:
        fn (str): A filename
        output_dir (str): An output directory. Default is the current directory.
    """

    # For now, remove the call_weak_fn in thumb mode rather than replacing it with the one in arm32 mode.
    call_weak_fn_lines_from_arm32 = []
    # call_weak_fn_lines_from_arm32 = get_call_weak_fn_lines_from_arm32(filename)

    os.system(
        f"arm-linux-gnueabihf-objdump -Dr -j .text -M force-thumb {filename} > {filename}.temp.thumb"
    )
    result_lines = []

    def pattern_found(prev_prev_line, prev_line, line):
        return ("movs" in prev_prev_line
                and ("movs" in prev_line or "lsls" in prev_line)
                and "movs" in line)

    with open(f"{filename}.temp.thumb") as f:
        is_going_through_call_weak_fn = False
        is_call_weak_fn_already_passed = False
        lines = f.readlines()
        for i, line in enumerate(lines):
            if (
                not is_call_weak_fn_already_passed
                and i > 2
                and pattern_found(lines[i - 3], lines[i - 2], lines[i - 1])
            ):
                # Find the start of call_weak_fn pattern in thumb mode:
                # movs	r1, r0
                # movs	r0, r4
                # movs	r0, r0
                # adds	r0, #20 // call_weak_fn starts here
                is_going_through_call_weak_fn = True

            # Collect instructions, excluding those in call_weak_fn in thumb mode
            if not is_going_through_call_weak_fn:
                result_lines.append(line)

            if (
                is_going_through_call_weak_fn
                and pattern_found(lines[i - 2], lines[i - 1], lines[i])
            ):
                # End of call_weak_fn
                is_going_through_call_weak_fn = False
                # Prevent misdetection of similar patterns after collecting call_weak_fn
                is_call_weak_fn_already_passed = True
                result_lines.extend(call_weak_fn_lines_from_arm32)

    output_path = None
    if not output_dir:
        output_path = f"{filename}.temp"
    else:
        fn_without_path = os.path.basename(filename)
        output_path = f"{output_dir}/{fn_without_path}.temp"

    with open(f"{output_path}", "w") as f:
        f.writelines(result_lines)


def disassemble_text_section_as_data(fn):
    """
    Disassemble the text section as data.
    This function is used to handle inline data in the text section.
    For example,
    000103f4 <main>:
        103f4:	b580      	push	{r7, lr}
        103f6:	b082      	sub	sp, #8
        103f8:	af00      	add	r7, sp, #0
        103fa:	230a      	movs	r3, #10
        103fc:	607b      	str	r3, [r7, #4]
        103fe:	6879      	ldr	r1, [r7, #4]
        10400:	4b04      	ldr	r3, [pc, #16]	; (10414 <main+0x20>)
        10402:	447b      	add	r3, pc
        10404:	4618      	mov	r0, r3
        10406:	f7ff ef82 	blx	1030c <printf@plt>
        1040a:	2300      	movs	r3, #0
        1040c:	4618      	mov	r0, r3
        1040e:	3708      	adds	r7, #8
        10410:	46bd      	mov	sp, r7
        10412:	bd80      	pop	{r7, pc}
        10414:	000000ae 	andeq	r0, r0, lr, lsr #1

    The instruction at 0x10400 uses data at [pc, #16] which is 0x10414.
    Then, the instruction at 0x10402 calculates r3 as 0xae + pc which returns 0x104b4.

    See Also:
        arm_reassemble_symbol_get.ml
    """
    cmd = f'arm-linux-gnueabihf-objdump -s -j .text {fn}'
    cmd += ' | grep "^ "'
    cmd += ' | cut -d " " -f2,3,4,5,6'
    cmd += ' > text_section_as_data.temp'
    os.system(cmd)
    lines = []
    with open("text_section_as_data.temp", "r") as f:
        raw_lines = f.readlines()
        # start when the line has "<.text>:"
        for i, line in enumerate(raw_lines):
            line = line.strip().rstrip('\n')
            arr = line.split(" ")
            lines.append(arr)

    with open("text_section_as_data.txt", "w") as f:
        for i, arr in enumerate(lines):
            start_addr = arr[0]
            start_addr = int(start_addr, 16)
            offset = 0
            for data in arr[1:]:
                data = data.strip()
                ordered_data = data[6:8] + data[4:6] + data[2:4] + data[0:2]
                addr = start_addr + offset
                hex_addr = hex(addr).lstrip("0x")
                f.write(f"{hex_addr}:{ordered_data}\n")
                offset += 4

    os.system("rm text_section_as_data.temp")


def disassemble_got_section_as_data(fn):
    os.system(
        f"arm-linux-gnueabihf-objdump -Dr -j .got {fn} > got_section_as_data.temp"
    )
    lines = []
    with open("got_section_as_data.temp", "r") as f:
        raw_lines = f.readlines()
        # start when the line has "<.got>:"
        for i, line in enumerate(raw_lines):
            if "<.got>:" in line:
                # skip the first line
                lines = raw_lines[i + 1 :]
                break

    with open("got_section_as_data.txt", "w") as f:
        for i, line in enumerate(lines):
            line = line.strip()
            line = line.split("\t")
            if len(line) < 2:
                continue

            addr = line[0]
            data = line[1].strip()
            f.write(f"{addr}{data}\n")  # 21000:00020f18

    os.system("rm got_section_as_data.temp")


def add_tag(line: str, tag: str) -> str:
    """
    Take a line from objdump and add an tag to it.
    """

    line_parts = line.split("\t")
    if "\n" in line_parts[3]:
        line_parts[3] = line_parts[3].strip().rstrip("\n")
        line_parts[3] += " " + tag
        line_parts[3] += "\n"
    else:
        line_parts[3] += " " + tag

    return "\t".join(line_parts)


if __name__ == "__main__":
    filename = sys.argv[1]
    output_dir = sys.argv[2] if len(sys.argv) > 2 else None
    disassemble_arm_thumb_binary(filename, output_dir)
    disassemble_text_section_as_data(filename)
    disassemble_got_section_as_data(filename)
