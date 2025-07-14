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
        return (
            "movs" in prev_prev_line
            and ("movs" in prev_line or "lsls" in prev_line)
            and "movs" in line
        )

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

            if is_going_through_call_weak_fn and pattern_found(
                lines[i - 2], lines[i - 1], lines[i]
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


def get_call_weak_fn_pattern_addr(filename):
    def is_call_weak_fn(lines):
        # b __gmon_start__@plt varies depending on the binary.
        return (
            "ldr" in lines[0]
            and "ldr" in lines[1]
            and "add" in lines[2]
            and "ldr" in lines[3]
            and "cmp" in lines[4]
            and "bxeq" in lines[5]
            and "b" in lines[6]
        )

    call_weak_fn_addrs = []
    with open(f"{filename}.temp", "r") as f:
        lines = f.readlines()
        for i, line in enumerate(lines):
            if i < len(lines) - 9 and is_call_weak_fn(lines[i : i + 9]):
                for j in range(9):
                    addr = lines[i + j].split(":")[0].strip()
                    call_weak_fn_addrs.append(addr)
                return call_weak_fn_addrs


def get_start_fn_addrs(filename):
    """
    10668:       f04f 0b00       mov.w   fp, #0
    1066c:       f04f 0e00       mov.w   lr, #0
    10670:       bc02            pop     {r1}
    10672:       466a            mov     r2, sp
    10674:       b404            push    {r2}
    10676:       b401            push    {r0}
    10678:       f8df a018       ldr.w   sl, [pc, #24]   ; 10694 <_start+0x2c>
    1067c:       a305            add     r3, pc, #20     ; (adr r3, 10694 <_start+0x2c>)
    1067e:       449a            add     sl, r3
    10680:       f04f 0300       mov.w   r3, #0
    10684:       b408            push    {r3}
    10686:       4804            ldr     r0, [pc, #16]   ; (10698 <_start+0x30>)
    10688:       f85a 0000       ldr.w   r0, [sl, r0]
    1068c:       f7ff ef84       blx     10598 <__libc_start_main@plt>
    10690:       f7ff efe2       blx     10658 <abort@plt>
    10694:       0001196c        andeq   r1, r1, ip, ror #18
    10698:       00000060        andeq   r0, r0, r0, rrx
    """

    def is_start_fn(lines):
        return (
            "mov.w" in lines[0]
            and "mov.w" in lines[1]
            and "pop" in lines[2]
            and "mov" in lines[3]
            and "push" in lines[4]
            and "push" in lines[5]
            and "ldr.w" in lines[6]
            and "add" in lines[7]
            and "add" in lines[8]
            and "mov.w" in lines[9]
            and "push" in lines[10]
            and "ldr" in lines[11]
            and "ldr.w" in lines[12]
            and "blx" in lines[13]
            and "blx" in lines[14]
        )

    start_fn_addrs = []
    with open(f"{filename}.temp.thumb", "r") as f:
        lines = f.readlines()
        for i, line in enumerate(lines):
            if i < len(lines) - 16 and is_start_fn(lines[i : i + 17]):
                # return the address of the start function
                #   10668:       f04f 0b00       mov.w   fp, #0
                for j in range(18):
                    addr = lines[i + j].split(":")[0].strip()
                    start_fn_addrs.append(addr)
                return start_fn_addrs


def get_register_tm_clones_fn_addrs(filename):
    """
    103ac:       f241 002c       movw    r0, #4140       ; 0x102c
    103b0:       f2c0 0002       movt    r0, #2
    103b4:       f241 032c       movw    r3, #4140       ; 0x102c
    103b8:       f2c0 0302       movt    r3, #2
    103bc:       1a1b            subs    r3, r3, r0
    103be:       0fd9            lsrs    r1, r3, #31
    103c0:       eb01 01a3       add.w   r1, r1, r3, asr #2
    103c4:       1049            asrs    r1, r1, #1
    103c6:       d005            beq.n   103d4 <register_tm_clones+0x28>
    103c8:       f240 0300       movw    r3, #0
    103cc:       f2c0 0300       movt    r3, #0
    103d0:       b103            cbz     r3, 103d4 <register_tm_clones+0x28>
    103d2:       4718            bx      r3
    103d4:       4770            bx      lr
    103d6:       bf00            nop
    """

    def is_register_tm_clones_fn(lines):
        return (
            "movw" in lines[0]
            and "movt" in lines[1]
            and "movw" in lines[2]
            and "movt" in lines[3]
            and "subs" in lines[4]
            and "lsrs" in lines[5]
            and "add.w" in lines[6]
            and "asrs" in lines[7]
            and "beq.n" in lines[8]
            and "movw" in lines[9]
            and "movt" in lines[10]
            and "cbz" in lines[11]
            and "bx" in lines[12]
            and "bx" in lines[13]
            and "nop" in lines[14]
        )

    register_tm_clones_fn_addrs = []
    with open(f"{filename}.temp.thumb", "r") as f:
        lines = f.readlines()
        for i, line in enumerate(lines):
            if i < len(lines) - 15 and is_register_tm_clones_fn(lines[i : i + 16]):
                for j in range(16):
                    addr = lines[i + j].split(":")[0].strip()
                    register_tm_clones_fn_addrs.append(addr)
                return register_tm_clones_fn_addrs


def get_deregister_tm_clones_fn_addrs(filename):
    """
    10388:       f241 002c       movw    r0, #4140       ; 0x102c
    1038c:       f2c0 0002       movt    r0, #2
    10390:       f241 032c       movw    r3, #4140       ; 0x102c
    10394:       f2c0 0302       movt    r3, #2
    10398:       4283            cmp     r3, r0
    1039a:       d005            beq.n   103a8 <deregister_tm_clones+0x20>
    1039c:       f240 0300       movw    r3, #0
    103a0:       f2c0 0300       movt    r3, #0
    103a4:       b103            cbz     r3, 103a8 <deregister_tm_clones+0x20>
    103a6:       4718            bx      r3
    103a8:       4770            bx      lr
    103aa:       bf00            nop
    """

    def is_deregister_tm_clones_fn(lines):
        return (
            "movw" in lines[0]
            and "movt" in lines[1]
            and "movw" in lines[2]
            and "movt" in lines[3]
            and "cmp" in lines[4]
            and "beq.n" in lines[5]
            and "movw" in lines[6]
            and "movt" in lines[7]
            and "cbz" in lines[8]
            and "bx" in lines[9]
            and "bx" in lines[10]
            and "nop" in lines[11]
        )

    deregister_tm_clones_fn_addrs = []
    with open(f"{filename}.temp.thumb", "r") as f:
        lines = f.readlines()
        for i, line in enumerate(lines):
            if i < len(lines) - 12 and is_deregister_tm_clones_fn(lines[i : i + 13]):
                for j in range(13):
                    addr = lines[i + j].split(":")[0].strip()
                    deregister_tm_clones_fn_addrs.append(addr)
                return deregister_tm_clones_fn_addrs


def get_do_global_dtors_aux_fn_addrs(filename):
    """
    103d8:       b510            push    {r4, lr}
    103da:       f241 042c       movw    r4, #4140       ; 0x102c
    103de:       f2c0 0402       movt    r4, #2
    103e2:       7823            ldrb    r3, [r4, #0]
    103e4:       b91b            cbnz    r3, 103ee <__do_global_dtors_aux+0x16>
    103e6:       f7ff ffcf       bl      10388 <deregister_tm_clones>
    103ea:       2301            movs    r3, #1
    103ec:       7023            strb    r3, [r4, #0]
    103ee:       bd10            pop     {r4, pc}
    """

    def is_do_global_dtors_aux_fn(lines):
        return (
            "push" in lines[0]
            and "movw" in lines[1]
            and "movt" in lines[2]
            and "ldrb" in lines[3]
            and "cbnz" in lines[4]
            and "bl" in lines[5]
            and "movs" in lines[6]
            and "strb" in lines[7]
            and "pop" in lines[8]
        )

    do_global_dtors_aux_fn_addrs = []
    with open(f"{filename}.temp.thumb", "r") as f:
        lines = f.readlines()
        for i, line in enumerate(lines):
            if i < len(lines) - 8 and is_do_global_dtors_aux_fn(lines[i : i + 9]):
                for j in range(9):
                    addr = lines[i + j].split(":")[0].strip()
                    do_global_dtors_aux_fn_addrs.append(addr)
                return do_global_dtors_aux_fn_addrs


def disassemble_arm32_binary(filename, output_dir):
    """
    Disassemble ARM32 binary and remove functions added by gcc compiler.
    """

    os.system(f"arm-linux-gnueabihf-objdump -Dr -j .text {filename} > {filename}.temp")
    os.system(
        f"arm-linux-gnueabihf-objdump -Dr -j .text -M force-thumb {filename} > {filename}.temp.thumb"
    )

    call_weak_fn_addrs = get_call_weak_fn_pattern_addr(filename)
    start_fn_addrs = get_start_fn_addrs(filename)
    deregister_tm_clones_fn_addrs = get_deregister_tm_clones_fn_addrs(filename)
    register_tm_clones_fn_addrs = get_register_tm_clones_fn_addrs(filename)
    do_global_dtors_aux_fn_addrs = get_do_global_dtors_aux_fn_addrs(filename)
    init_fn_addrs = (
        call_weak_fn_addrs
        + start_fn_addrs
        + deregister_tm_clones_fn_addrs
        + register_tm_clones_fn_addrs
        + do_global_dtors_aux_fn_addrs
    )

    def is_between(addr: str, addr_list: list) -> bool:
        try:
            addr = int(addr, 16)
            addr -= 2
            addr = hex(addr).lstrip("0x")
        except ValueError:
            return False

        if addr in addr_list:
            return True

        return False

    new_content = []
    with open(f"{filename}.temp", "r") as f:
        lines = f.readlines()
        for line in lines:
            addr = line.split(":")[0].strip()
            if addr in init_fn_addrs:
                continue

            if is_between(addr, init_fn_addrs):
                continue

            new_content.append(line)

    with open(f"{filename}.temp", "w") as f:
        f.writelines(new_content)


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
    cmd = f"arm-linux-gnueabihf-objdump -s -j .text {fn}"
    cmd += ' | grep "^ "'
    cmd += ' | cut -d " " -f2,3,4,5,6'
    cmd += " > text_section_as_data.temp"
    os.system(cmd)
    lines = []
    with open("text_section_as_data.temp", "r") as f:
        raw_lines = f.readlines()
        # start when the line has "<.text>:"
        for i, line in enumerate(raw_lines):
            line = line.strip().rstrip("\n")
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
    arch = sys.argv[2]
    output_dir = sys.argv[3] if len(sys.argv) > 3 else None
    if arch == "thumb":
        disassemble_arm_thumb_binary(filename, output_dir)
    elif arch == "arm":
        disassemble_arm32_binary(filename, output_dir)
    disassemble_text_section_as_data(filename)
    disassemble_got_section_as_data(filename)
