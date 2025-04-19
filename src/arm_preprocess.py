import os
import sys


def disassemble_arm_thumb_binary(fn, output_dir):
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

    os.system(
        f"arm-linux-gnueabihf-objdump -Dr -j .text -M force-thumb {fn} > {fn}.temp.thumb"
    )
    os.system(f"arm-linux-gnueabihf-objdump -Dr -j .text {fn} > {fn}.temp.arm32")

    # Find call_weak_fn pattern in arm32 mode,
    # then replace the call_weak_fn in thumb mode with the one in arm32 mode.
    call_weak_fn_lines_from_arm32 = []
    with open(f"{fn}.temp.arm32", "r") as f:
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

    result_lines = []
    with open(f"{fn}.temp.thumb") as f:
        is_going_through_call_weak_fn = False
        is_call_weak_fn_already_passed = False
        lines = f.readlines()
        for i, line in enumerate(lines):
            if (
                not is_call_weak_fn_already_passed
                and i > 2
                and "movs" in lines[i - 3]
                and "movs" in lines[i - 2]
                and "movs" in lines[i - 1]
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
                and "movs" in lines[i - 2]
                and "movs" in lines[i - 1]
                and "movs" in lines[i]
            ):
                # End of call_weak_fn
                is_going_through_call_weak_fn = False
                # Prevent misdetection of similar patterns after collecting call_weak_fn
                is_call_weak_fn_already_passed = True
                result_lines.extend(call_weak_fn_lines_from_arm32)

    output_path = None
    if not output_dir:
        output_path = f"{fn}.temp"
    else:
        fn_without_path = os.path.basename(fn)
        output_path = f"{output_dir}/{fn_without_path}.temp"

    with open(f"{output_path}", "w") as f:
        f.writelines(result_lines)

    os.system(f"rm {fn}.temp.thumb {fn}.temp.arm32")


if __name__ == "__main__":
    fn = sys.argv[1]
    output_dir = sys.argv[2] if len(sys.argv) > 2 else None
    disassemble_arm_thumb_binary(fn, output_dir)
