#!/usr/bin/env python3

from __future__ import annotations

import bisect
import os
from dataclasses import dataclass

import main_discover

_COND_CODES = {
    "",
    "eq",
    "ne",
    "cs",
    "cc",
    "mi",
    "pl",
    "vs",
    "vc",
    "hi",
    "ls",
    "ge",
    "lt",
    "gt",
    "le",
    "al",
    "hs",
    "lo",
}

_BL_OPS = frozenset("bl" + c for c in _COND_CODES)
_BLX_OPS = frozenset("blx" + c for c in _COND_CODES)
_BCC_OPS = frozenset("b" + c for c in _COND_CODES if c != "al")
_BAL_OPS = frozenset({"b", "bal"})


@dataclass
class ParsedInstruction:
    addr: int
    opcode: str
    operands: str
    size: int
    line: str
    is_bl: bool = False
    is_blx: bool = False
    is_cond_jump: bool = False
    is_uncond_jump: bool = False
    is_return: bool = False
    target_addr: int | None = None


def _extract_size(hex_column: str) -> int:
    compact_hex = "".join(hex_column.strip().split())
    if not compact_hex:
        return 2
    return max(1, len(compact_hex) // 2)


def _is_return_opcode(op: str, operands_lower: str) -> bool:
    if op == "bx" and ("lr" in operands_lower or "r14" in operands_lower):
        return True
    if op.startswith("pop") and "pc" in operands_lower:
        return True
    if op.startswith("ldmia") and "sp" in operands_lower and "pc" in operands_lower:
        return True
    return False


def _extract_target_addr(operands: str) -> int | None:
    if not operands:
        return None

    first_token = operands.split()[0].strip().strip(",")
    if first_token.startswith("#"):
        first_token = first_token[1:]
    if first_token.startswith("0x"):
        first_token = first_token[2:]

    try:
        return int(first_token, 16)
    except ValueError:
        return None


def _parse_instruction_line(line: str) -> ParsedInstruction | None:
    parts = line.rstrip("\n").split("\t")
    if len(parts) < 4 and not (len(parts) == 3 and "nop" in parts[2].strip().lower()):
        return None

    addr_str = parts[0].strip().rstrip(":")
    try:
        addr = int(addr_str, 16)
    except ValueError:
        return None

    raw_opcode = parts[2].strip().lower()
    # For `nop` instructions, there is no operands column.
    operands = parts[3].strip() if len(parts) > 3 else ""
    size = _extract_size(parts[1])

    op = raw_opcode.split(".", 1)[0]
    operands_lower = operands.lower()
    is_bl = op in _BL_OPS
    is_blx = op in _BLX_OPS
    is_cond_jump = (not is_bl) and (not is_blx) and op in _BCC_OPS
    is_uncond_jump = op in _BAL_OPS
    is_return = _is_return_opcode(op, operands_lower)
    target_addr = (
        _extract_target_addr(operands)
        if is_bl or is_blx or is_cond_jump or is_uncond_jump
        else None
    )

    return ParsedInstruction(
        addr=addr,
        opcode=raw_opcode,
        operands=operands,
        size=size,
        line=line,
        is_bl=is_bl,
        is_blx=is_blx,
        is_cond_jump=is_cond_jump,
        is_uncond_jump=is_uncond_jump,
        is_return=is_return,
        target_addr=target_addr,
    )


def _load_disassembly(
    path: str,
) -> tuple[list[tuple[str, "ParsedInstruction | None"]], dict[int, ParsedInstruction]]:
    """Read and parse a disassembly file once.

    Returns the ordered (line, parsed) pairs (parsed is None for non-instruction
    lines) and an addr→ParsedInstruction map. Callers that only need the map
    can discard the ordered list.
    """
    ordered: list[tuple[str, ParsedInstruction | None]] = []
    addr_map: dict[int, ParsedInstruction] = {}

    with open(path, "r") as file_obj:
        for line in file_obj:
            parsed = _parse_instruction_line(line)
            ordered.append((line, parsed))
            if parsed is not None and parsed.addr not in addr_map:
                addr_map[parsed.addr] = parsed

    return ordered, addr_map


def _parse_main_info_addr() -> int | None:
    if not os.path.exists("main.info"):
        print("Cannot get main symbol address from main.info")
        return None

    with open("main.info", "r") as file_obj:
        print("Get main symbol address from main.info")
        line = file_obj.readline().strip()

    if not line:
        return None

    if line.startswith("S_0x"):
        hex_part = line[4:]
    elif line.startswith("0x"):
        hex_part = line[2:]
    else:
        hex_part = line

    try:
        return int(hex_part, 16)
    except ValueError:
        return None


def _find_main_addr(
    program_name: str, arm32_path: str, thumb_path: str, arch: str = "arm"
) -> int | None:
    main_addr = main_discover.main(program_name, arch)
    return main_addr


def _next_mode(current_mode: str, is_blx: bool) -> str:
    if not is_blx:
        return current_mode
    return "thumb" if current_mode == "arm32" else "arm32"


def _get_text_section_addrs() -> tuple[int, int]:
    """
    Read text_sec.info and parse the start address and end address of the .text section.
    The text_sec.info file has lines in the format:
        .text 00010668 000668 000e0c

    """
    filename = "text_sec.info"
    start_addr = 0
    end_addr = 0
    with open(filename, "r") as f:
        for line in f:
            if line.startswith(".text"):
                parts = line.split()
                if len(parts) >= 3:
                    start_addr = int(parts[1], 16)
                    end_addr = start_addr + int(parts[3], 16)
                    break
    return start_addr, end_addr


def build_mixed_disassembly(
    program_name: str,
    output_dir: str | None = None,
    frame_dummy_end_addr: int | None = None,
    extra_thumb_addrs: list[int] | None = None,
    initial_mode: str = "arm32",
    init_fn_addrs: list[str] | None = None,
) -> list[str]:
    """
    ARM32 binaries may contain both ARM and Thumb functions
    (e.g., library routines such as memcpy or __udivsi3).

    This function constructs a mixed-mode disassembly starting
    from the main symbol and recursively following the control-flow graph (CFG).

    While traversing the CFG, it distinguishes ARM and Thumb functions
    based on call instructions:

    - In ARM mode, `bl` performs a function call within the same mode.
    - `blx` may perform a function call that switches between ARM and Thumb modes.

    By analyzing `bl` and `blx` targets during CFG traversal,
    this function identifies and disassembles functions in the correct mode.

    Some functions may be unreachable from the main symbol.
    To address this, we push the next instruction to cover the entire .text section.
    In addition, we push the next instruction before push a call target,
    so to cover control-flow first and then cover the rest of the .text section.

    Returns:
        list[str]: The mixed disassembly formatted in the style of objdump output.
    """
    text_start, text_end = _get_text_section_addrs()

    arm32_path = f"{program_name}.temp.arm32"
    thumb_path = f"{program_name}.temp.thumb"

    if not os.path.exists(arm32_path):
        raise FileNotFoundError(f"missing arm32 disassembly file: {arm32_path}")
    if not os.path.exists(thumb_path):
        raise FileNotFoundError(f"missing thumb disassembly file: {thumb_path}")

    arm32_ordered, arm32_map = _load_disassembly(arm32_path)
    _, thumb_map = _load_disassembly(thumb_path)

    # Pre-compute init function address set and sorted list for range checks.
    init_fn_addr_ints: set[int] = set()
    sorted_init_ints: list[int] = []
    if init_fn_addrs:
        init_fn_addr_ints = {int(a, 16) for a in init_fn_addrs}
        sorted_init_ints = sorted(init_fn_addr_ints)

    def _is_init_addr(addr: int) -> bool:
        """Return True if addr is an init function instruction address."""
        if addr in init_fn_addr_ints:
            return True
        if not sorted_init_ints:
            return False
        idx = bisect.bisect_left(sorted_init_ints, addr)
        if idx == 0 or idx >= len(sorted_init_ints):
            return False
        prev = sorted_init_ints[idx - 1]
        next_ = sorted_init_ints[idx]
        if next_ - prev >= 6:
            return False
        return prev < addr < next_

    def _seed_start_addrs(
        a32: set[int], th: set[int], from_mode_map: dict[int, str] | None
    ) -> None:
        """Seed arm32/thumb start address sets from bl/blx targets.

        When from_mode_map is None, scan the raw arm32_map and thumb_map.
        Otherwise, scan only the addresses present in from_mode_map using
        their correct (previously determined) modes.
        """
        if from_mode_map is None:
            for addr, instr in arm32_map.items():
                if _is_init_addr(addr):
                    continue
                t = instr.target_addr
                if t is None:
                    continue
                if instr.is_bl:
                    a32.add(t)
                elif instr.is_blx:
                    th.add(t)
            for addr, instr in thumb_map.items():
                if _is_init_addr(addr):
                    continue
                t = instr.target_addr
                if t is None:
                    continue
                if instr.is_bl:
                    th.add(t)
                elif instr.is_blx:
                    a32.add(t)
        else:
            for addr, mode in from_mode_map.items():
                if _is_init_addr(addr):
                    continue
                current_map = arm32_map if mode == "arm32" else thumb_map
                instr = current_map.get(addr)
                if instr is None:
                    continue
                t = instr.target_addr
                if t is None:
                    continue
                if mode == "arm32":
                    if instr.is_bl:
                        a32.add(t)
                    elif instr.is_blx:
                        th.add(t)
                else:
                    if instr.is_bl:
                        th.add(t)
                    elif instr.is_blx:
                        a32.add(t)

    # Get the main symbol address.
    arch = "thumb" if initial_mode == "thumb" else "arm"
    main_addr = _find_main_addr(program_name, arm32_path, thumb_path, arch)
    if main_addr is None:
        if not arm32_map:
            raise RuntimeError("failed to locate main symbol and arm32 map is empty")
        main_addr = min(arm32_map.keys())

    output_path = (
        f"{program_name}.temp"
        if not output_dir
        else f"{output_dir}/{os.path.basename(program_name)}.temp"
    )

    arm32_start_addrs: set[int] = set()
    thumb_start_addrs: set[int] = set()
    instruction_addr_mode_map: dict[int, str] = {}
    visited: dict[int, tuple[str, int]] = {}
    selected_lines: dict[int, str] = {}

    # Sorted address lists used to skip over zero-padding gaps (shown as `...`
    # in objdump output) when instruction lookup returns None.
    arm32_sorted_addrs = sorted(arm32_map.keys())
    thumb_sorted_addrs = sorted(thumb_map.keys())

    # First iteration uses raw maps; subsequent iterations use instruction_addr_mode_map
    # from the previous pass so that misparse-induced bl/blx targets are corrected.
    mode_map_for_rescan: dict[int, str] | None = None

    while True:
        # Build start address sets from bl/blx targets.
        arm32_start_addrs = set()
        thumb_start_addrs = set()
        _seed_start_addrs(arm32_start_addrs, thumb_start_addrs, mode_map_for_rescan)

        # Seed fixed entry points.
        if initial_mode == "thumb":
            thumb_start_addrs.add(main_addr)
        else:
            arm32_start_addrs.add(main_addr)

        # Seed thumb start addresses that are known a priori (e.g. init functions
        # identified in arm_preprocess.py that may be located before main and are
        # therefore unreachable via normal CFG fallthrough from main).
        if extra_thumb_addrs:
            for addr in extra_thumb_addrs:
                thumb_start_addrs.add(addr)

        # Register the first instruction after frame_dummy with the appropriate mode so
        # that pp_print emits the correct .arm/.thumb directive at that address.
        if frame_dummy_end_addr is not None:
            if initial_mode == "thumb":
                thumb_start_addrs.add(frame_dummy_end_addr)
            else:
                arm32_start_addrs.add(frame_dummy_end_addr)

        # Start CFG traversal from the main symbol address.
        stack: list[tuple[int, str]] = []
        stack.append((main_addr & ~1, initial_mode))

        # Push extra thumb functions so they are visited even if unreachable from main.
        if extra_thumb_addrs:
            for addr in extra_thumb_addrs:
                stack.insert(0, (addr & ~1, "thumb"))

        # Push the address immediately after frame_dummy so that functions that
        # follow it in the text section but are never called via a direct bl/blx
        # (e.g. newfile, freefilelist) are discovered through fall-through in
        # the correct mode.
        if frame_dummy_end_addr is not None:
            fd_mode = "thumb" if initial_mode == "thumb" else "arm32"
            stack.insert(0, (frame_dummy_end_addr & ~1, fd_mode))

        visited: dict[int, tuple[str, int]] = {}
        selected_lines: dict[int, str] = {}
        instruction_addr_mode_map = {}

        while stack:
            start_addr, mode = stack.pop()
            current_addr = start_addr & ~1

            # Change mode if the current address is in the start addresses of the other mode.
            if current_addr in thumb_start_addrs:
                mode = "thumb"
            elif current_addr in arm32_start_addrs:
                mode = "arm32"
            elif (
                frame_dummy_end_addr is not None
                and current_addr >= frame_dummy_end_addr
            ):
                # After frame_dummy, fall back to initial_mode unless explicitly identified
                # as thumb or arm32 via bl/blx targets above.
                mode = initial_mode

            # Get an instruction corresponding to the current address and mode.
            current_map = arm32_map if mode == "arm32" else thumb_map
            instruction = current_map.get(current_addr)
            if instruction is None:
                # current_addr falls inside a zero-padding gap (`...` in objdump).
                # Skip forward to the next valid address in the current map.
                sorted_addrs = (
                    arm32_sorted_addrs if mode == "arm32" else thumb_sorted_addrs
                )
                idx = bisect.bisect_left(sorted_addrs, current_addr)
                if idx < len(sorted_addrs):
                    stack.append((sorted_addrs[idx], mode))
                continue

            if current_addr in visited:
                continue

            visited[current_addr] = (mode, instruction.size)
            selected_lines[current_addr] = instruction.line
            instruction_addr_mode_map[current_addr] = mode

            next_addr = (current_addr + instruction.size) & ~1
            if instruction.is_bl or instruction.is_blx:
                target_addr = instruction.target_addr
                stack.append((next_addr, mode))
                if target_addr is not None:
                    target_mode = _next_mode(mode, instruction.is_blx)
                    if target_mode == "thumb":
                        thumb_start_addrs.add(target_addr)
                    else:
                        arm32_start_addrs.add(target_addr)
                    stack.append((target_addr & ~1, target_mode))
            elif instruction.is_return:
                # Push to back to prioritize covering the control-flow first
                stack.insert(0, (next_addr, mode))
                continue
            elif instruction.is_cond_jump:
                target_addr = instruction.target_addr
                stack.append((next_addr, mode))
                if target_addr is not None:
                    stack.append((target_addr & ~1, mode))
            else:
                # Unconditional jump or non-control-flow: fall through.
                stack.append((next_addr, mode))

        # Check for convergence using the mode map, which fully determines the
        # output: if instruction_addr_mode_map is unchanged then visited and
        # selected_lines are unchanged, so the merged output would be unchanged
        # too. Comparing mode maps avoids rebuilding the (~85k-line) output on
        # every iteration just to compare it.
        if (
            mode_map_for_rescan is not None
            and instruction_addr_mode_map == mode_map_for_rescan
        ):
            break

        mode_map_for_rescan = instruction_addr_mode_map

    # Build the merged output once after convergence using the cached
    # arm32_ordered list (avoids re-reading and re-parsing arm32_path
    # on every fixed-point iteration).
    merged_lines = []
    selected_covered_addrs: set[int] = set()
    for addr, (_, size) in visited.items():
        for byte_addr in range(addr, addr + size):
            selected_covered_addrs.add(byte_addr)

    selected_addrs_sorted = sorted(selected_lines.keys())
    selected_idx = 0
    emitted_selected_addrs: set[int] = set()

    def _emit_selected(addr_int: int) -> None:
        if init_fn_addr_ints and _is_init_addr(addr_int):
            return
        merged_lines.append(selected_lines[addr_int])

    for line, parsed in arm32_ordered:
        if parsed is None:
            merged_lines.append(line)
            continue

        while (
            selected_idx < len(selected_addrs_sorted)
            and selected_addrs_sorted[selected_idx] < parsed.addr
        ):
            selected_addr = selected_addrs_sorted[selected_idx]
            if selected_addr not in emitted_selected_addrs:
                _emit_selected(selected_addr)
                emitted_selected_addrs.add(selected_addr)
            selected_idx += 1

        if parsed.addr in selected_lines and parsed.addr not in emitted_selected_addrs:
            _emit_selected(parsed.addr)
            emitted_selected_addrs.add(parsed.addr)
            if (
                selected_idx < len(selected_addrs_sorted)
                and selected_addrs_sorted[selected_idx] == parsed.addr
            ):
                selected_idx += 1
            continue

        overlaps_selected = any(
            (parsed.addr + offset) in selected_covered_addrs
            for offset in range(parsed.size)
        )
        if overlaps_selected:
            continue

        if init_fn_addr_ints and _is_init_addr(parsed.addr):
            continue

        merged_lines.append(line)

    while selected_idx < len(selected_addrs_sorted):
        selected_addr = selected_addrs_sorted[selected_idx]
        if selected_addr not in emitted_selected_addrs:
            _emit_selected(selected_addr)
            emitted_selected_addrs.add(selected_addr)
        selected_idx += 1

    with open(output_path, "w") as out_file:
        out_file.writelines(merged_lines)

    # Write the start addresses of ARM and Thumb functions for later use
    # Filter target_addr >= text_start and target_addr < text_end:
    arm32_start_addrs_list = sorted(
        addr for addr in arm32_start_addrs if text_start <= addr < text_end
    )
    with open("arm32_start_addrs.txt", "w") as f:
        for addr in arm32_start_addrs_list:
            f.write(f"{addr:#x}\n")

    # Filter out addresses outside of the .text section.
    thumb_start_addrs_list = sorted(
        addr for addr in thumb_start_addrs if text_start <= addr < text_end
    )
    with open("thumb_start_addrs.txt", "w") as f:
        for addr in thumb_start_addrs_list:
            f.write(f"{addr:#x}\n")

    # Write the final address-mode map for later use
    with open("instruction_addr_mode_map.txt", "w") as f:
        for addr in sorted(instruction_addr_mode_map.keys()):
            mode = instruction_addr_mode_map[addr]
            f.write(f"{addr:#x}:{mode}\n")

    # For debugging
    # with open("arm32_visit_map.info", "w") as meta_file:
    #     for addr in sorted(visited.keys()):
    #         mode, size = visited[addr]
    #         meta_file.write(f"{addr:#x}\t{mode}\t{size}\n")

    return merged_lines
