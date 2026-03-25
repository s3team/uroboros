#!/usr/bin/env python3

from __future__ import annotations

import os
import re
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


@dataclass
class ParsedInstruction:
    addr: int
    opcode: str
    operands: str
    size: int
    line: str


def _normalize_opcode(opcode: str) -> str:
    return opcode.strip().lower().split(".")[0]


def _extract_size(hex_column: str) -> int:
    compact_hex = "".join(hex_column.strip().split())
    if not compact_hex:
        return 2
    return max(1, len(compact_hex) // 2)


def _parse_instruction_line(line: str) -> ParsedInstruction | None:
    parts = line.rstrip("\n").split("\t")
    if len(parts) < 4 and not (len(parts) == 3 and "nop" in parts[2].strip().lower()):
        return None

    addr_str = parts[0].strip().rstrip(":")
    if not re.fullmatch(r"[0-9a-fA-F]+", addr_str):
        return None

    try:
        addr = int(addr_str, 16)
    except ValueError:
        return None

    opcode = parts[2].strip().lower()
    # For `nop` instructions, there is no operands column.
    operands = parts[3].strip() if len(parts) > 3 else ""
    size = _extract_size(parts[1])
    return ParsedInstruction(
        addr=addr, opcode=opcode, operands=operands, size=size, line=line
    )


def _load_disassembly(path: str) -> tuple[list[str], dict[int, ParsedInstruction]]:
    header_lines: list[str] = []
    addr_map: dict[int, ParsedInstruction] = {}

    with open(path, "r") as file_obj:
        for line in file_obj:
            parsed = _parse_instruction_line(line)
            if parsed is None:
                header_lines.append(line)
                continue

            if parsed.addr not in addr_map:
                addr_map[parsed.addr] = parsed

    return header_lines, addr_map


def _is_bl(opcode: str) -> bool:
    op = _normalize_opcode(opcode)
    return op in {"bl" + cond for cond in _COND_CODES}


def _is_blx(opcode: str) -> bool:
    op = _normalize_opcode(opcode)
    return op in {"blx" + cond for cond in _COND_CODES}


def _is_function_call(opcode: str) -> bool:
    return _is_bl(opcode) or _is_blx(opcode)


def _is_conditional_jump(opcode: str) -> bool:
    op = _normalize_opcode(opcode)
    return op in {"b" + cond for cond in _COND_CODES if cond != "al"}


def _is_unconditional_jump(opcode: str) -> bool:
    op = _normalize_opcode(opcode)
    return op in {"b", "bal"}


def _is_return_instruction(instruction: ParsedInstruction) -> bool:
    op = _normalize_opcode(instruction.opcode)
    operands = instruction.operands.lower()

    if op in {"bx"} and ("lr" in operands or "r14" in operands):
        return True
    if op.startswith("pop") and "pc" in operands:
        return True
    if op.startswith("ldmia") and "sp" in operands and "pc" in operands:
        return True

    return False


def _extract_target_addr(operands: str) -> int | None:
    if not operands:
        return None

    first_token = operands.split()[0].strip().strip(",")
    if first_token.startswith("#"):
        first_token = first_token[1:]

    try:
        if first_token.startswith("0x"):
            return int(first_token, 16)
        return int(first_token, 16)
    except ValueError:
        return None


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

    if not re.fullmatch(r"[0-9a-fA-F]+", hex_part):
        return None

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

    arm32_start_addrs = set()
    thumb_start_addrs = set()
    # For pc-relative instructions, we need to know the mode.
    instruction_addr_mode_map: dict[int, str] = {}

    if not os.path.exists(arm32_path):
        raise FileNotFoundError(f"missing arm32 disassembly file: {arm32_path}")
    if not os.path.exists(thumb_path):
        raise FileNotFoundError(f"missing thumb disassembly file: {thumb_path}")

    _, arm32_map = _load_disassembly(arm32_path)
    _, thumb_map = _load_disassembly(thumb_path)

    # Before CFG traversal, read raw arm32 and thumb disassemble
    # and find bl and blx instructions to get potential function entry points.
    for addr, instruction in arm32_map.items():
        if _is_bl(instruction.opcode):
            target_addr = _extract_target_addr(instruction.operands)
            if target_addr is not None:
                arm32_start_addrs.add(target_addr)
        elif _is_blx(instruction.opcode):
            target_addr = _extract_target_addr(instruction.operands)
            if target_addr is not None:
                thumb_start_addrs.add(target_addr)

    for addr, instruction in thumb_map.items():
        if _is_bl(instruction.opcode):
            target_addr = _extract_target_addr(instruction.operands)
            if target_addr is not None:
                thumb_start_addrs.add(target_addr)
        elif _is_blx(instruction.opcode):
            target_addr = _extract_target_addr(instruction.operands)
            if target_addr is not None:
                arm32_start_addrs.add(target_addr)

    # Get the main symbol address.
    arch = "thumb" if initial_mode == "thumb" else "arm"
    main_addr = _find_main_addr(program_name, arm32_path, thumb_path, arch)
    if main_addr is None:
        if not arm32_map:
            raise RuntimeError("failed to locate main symbol and arm32 map is empty")
        main_addr = min(arm32_map.keys())
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

    visited: dict[int, tuple[str, int]] = {}
    selected_lines: dict[int, str] = {}

    while stack:
        start_addr, mode = stack.pop()
        current_addr = start_addr & ~1

        # Change mode if the current address is in the start addresses of the other mode.
        if current_addr in thumb_start_addrs:
            mode = "thumb"
        elif current_addr in arm32_start_addrs:
            mode = "arm32"
        elif frame_dummy_end_addr is not None and current_addr >= frame_dummy_end_addr:
            # After frame_dummy, fall back to initial_mode unless explicitly identified
            # as thumb or arm32 via bl/blx targets above.
            mode = initial_mode

        # Get an instruction corresponding to the current address and mode.
        current_map = arm32_map if mode == "arm32" else thumb_map
        instruction = current_map.get(current_addr)
        if instruction is None:
            continue

        if current_addr in visited:
            continue

        visited[current_addr] = (mode, instruction.size)
        selected_lines[current_addr] = instruction.line
        instruction_addr_mode_map[current_addr] = mode

        if _is_function_call(instruction.opcode):
            # Push both the call target and the next instruction.
            target_addr = _extract_target_addr(instruction.operands)
            next_addr = (current_addr + instruction.size) & ~1
            stack.append((next_addr, mode))
            if target_addr is not None:
                target_mode = _next_mode(mode, _is_blx(instruction.opcode))
                if target_mode == "thumb":
                    thumb_start_addrs.add(target_addr)
                else:
                    arm32_start_addrs.add(target_addr)
                stack.append((target_addr & ~1, target_mode))
        elif _is_return_instruction(instruction):
            # Push to back to prioritize covering the control-flow first
            stack.insert(0, ((current_addr + instruction.size) & ~1, mode))
            continue
        elif _is_conditional_jump(instruction.opcode):
            # Push both the jump target and the next instruction.
            target_addr = _extract_target_addr(instruction.operands)
            next_addr = (current_addr + instruction.size) & ~1
            stack.append((next_addr, mode))
            if target_addr is not None:
                stack.append((target_addr & ~1, mode))
        elif _is_unconditional_jump(instruction.opcode):
            # Push only next instruction
            # since the jump target will eventually be handled.
            next_addr = (current_addr + instruction.size) & ~1
            stack.append((next_addr, mode))
        else:
            next_addr = (current_addr + instruction.size) & ~1
            stack.append((next_addr, mode))

    output_path = (
        f"{program_name}.temp"
        if not output_dir
        else f"{output_dir}/{os.path.basename(program_name)}.temp"
    )

    merged_lines: list[str] = []

    selected_covered_addrs: set[int] = set()
    for addr, (_, size) in visited.items():
        for byte_addr in range(addr, addr + size):
            selected_covered_addrs.add(byte_addr)

    selected_addrs_sorted = sorted(selected_lines.keys())
    selected_idx = 0
    emitted_selected_addrs: set[int] = set()

    with open(arm32_path, "r") as file_obj:
        for line in file_obj:
            parsed = _parse_instruction_line(line)
            if parsed is None:
                # Not an instruction line, just copy it
                merged_lines.append(line)
                continue

            while (
                selected_idx < len(selected_addrs_sorted)
                and selected_addrs_sorted[selected_idx] < parsed.addr
            ):
                selected_addr = selected_addrs_sorted[selected_idx]
                if selected_addr not in emitted_selected_addrs:
                    merged_lines.append(selected_lines[selected_addr])
                    emitted_selected_addrs.add(selected_addr)
                selected_idx += 1

            if (
                parsed.addr in selected_lines
                and parsed.addr not in emitted_selected_addrs
            ):
                merged_lines.append(selected_lines[parsed.addr])
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

            merged_lines.append(line)

    while selected_idx < len(selected_addrs_sorted):
        selected_addr = selected_addrs_sorted[selected_idx]
        if selected_addr not in emitted_selected_addrs:
            merged_lines.append(selected_lines[selected_addr])
            emitted_selected_addrs.add(selected_addr)
        selected_idx += 1

    with open(output_path, "w") as out_file:
        out_file.writelines(merged_lines)

    # Write the start addresses of ARM and Thumb functions for later use
    # Filter target_addr >= text_start and target_addr < text_end:
    arm32_start_addrs = [
        addr for addr in arm32_start_addrs if text_start <= addr < text_end
    ]
    arm32_start_addrs = sorted(set(arm32_start_addrs))
    with open("arm32_start_addrs.txt", "w") as f:
        for addr in arm32_start_addrs:
            f.write(f"{addr:#x}\n")

    # Filter out addresses outside of the .text section.
    thumb_start_addrs = [
        addr for addr in thumb_start_addrs if text_start <= addr < text_end
    ]
    thumb_start_addrs = sorted(set(thumb_start_addrs))
    with open("thumb_start_addrs.txt", "w") as f:
        for addr in thumb_start_addrs:
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
