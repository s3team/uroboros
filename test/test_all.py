#!/usr/bin/env python3
#  -*-coding:utf-8 -*-

"""
use this script to compile and test all test0*.c
"""

import subprocess
from pathlib import Path
import argparse
import logging
import os

lib_list = ["test08.c"]


def get_inputs(args):
    target_list = []
    if args.all:
        # add all test**.c to target_list
        for test_file in test_dir.glob("test[0-9][0-9].c"):
            if test_file.name not in lib_list:
                target_list.append(test_file)
    else:
        input = Path(args.input).resolve()
        if input.exists() is False:
            logger.error(f"Input file not found at {input}")
            return
        target_list.append(input)

    return target_list


def compile_bin(source: Path, binary: Path, arch: int, pie: bool, static: bool):
    if source.exists() is False:
        logger.error(f"Source code not found at {source}")
        return

    compile_args = [
        "gcc",
        "-m32" if arch == 32 else "-m64",
        "-no-pie" if pie is False else "-pie",
        "-o",
        str(binary),
        str(source),
    ]

    if static:
        compile_args.append("-static")

    if source.stem == "test03":
        compile_args.append("-D_GNU_SOURCE")

    logger.debug(f"Compiling {source} with args: {compile_args}")
    subprocess.run(compile_args, check=True)
    subprocess.run(["cp", str(binary), str(binary)+".sym"], check=True)
    subprocess.run(["strip", str(binary)], check=True)
    if binary.exists() is False:
        logger.error(f"Compilation failed for {source}")


def run_uroboros(binary: Path):
    if binary.exists() is False:
        logger.error(f"Target file not found at {binary}")
        return
    if "test00" in str(binary):
        return

    uroboros_args = [uroboros, binary]
    logger.debug(f"Running Uroboros with args: {uroboros_args}")
    res = subprocess.run(uroboros_args, capture_output=True, cwd=uroboros_dir)
    output = res.stdout.decode("utf-8")
    output_bin = uroboros_dir / "a.out"
    if "processing succeeded" in output and output_bin.exists():
        output_bin = output_bin.replace(binary.with_name(binary.name + ".out"))
        if test_recompiled_binary(binary, output_bin):
            logger.info(f"Uroboros succeeded on {binary.name}")
            compare_size(binary, output_bin)
    else:
        logger.error(f"Uroboros failed on {binary.name}: recompile failed")
        logger.error(res.stderr.decode("utf-8"))

    # Clean up
    tmp_files = [
        (uroboros_dir / binary.name),
        (uroboros_dir / (binary.name + ".temp")),
        (uroboros_dir / (binary.name + ".disassemble")),
        (uroboros_dir / binary.name),
        (uroboros_dir / "func_discover" / binary.name),
        (test_dir / "count.txt"),
    ]
    for tmp in tmp_files:
        if tmp.exists():
            tmp.unlink()


def test_recompiled_binary(raw: Path, recompiled: Path):
    args = []

    if raw.name.startswith("test04"):
        args = ["0"]

    # using script -q -c to capture all console outputs
    # for statically-linked testcases, they output correctly to console
    # though the outputs are not captured by stdout
    # this may or may not be an issue in the future
    raw_output = subprocess.run(["script", "-q", "-c", str(raw)] + args, capture_output=True)
    recompiled_output = subprocess.run(["script", "-q", "-c", str(recompiled)] + args, capture_output=True)

    if raw_output.returncode != recompiled_output.returncode:
        logger.error(f"Return code mismatch for {raw.name}")
        return False

    elif raw.name.startswith("test02") or raw.name.startswith("test03"):
        if str(recompiled) not in recompiled_output.stdout.decode("utf-8"):
            logger.error(f"Output mismatch for {raw.name}")
            logger.info(
                f"Recompiled output: {recompiled_output.stdout.decode('utf-8')}"
            )
            return False

    elif raw_output.stdout != recompiled_output.stdout:
        logger.error(f"Output mismatch for {raw.name}")
        logger.info(f"Raw output: {raw_output.stdout.decode('utf-8')}")
        logger.info(f"Recompiled output: {recompiled_output.stdout.decode('utf-8')}")
        return False

    return True

def human_readable_size(size, decimal_places=2):
    for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
        if size < 1024:
            return f"{size:.{decimal_places}f} {unit}"
        size /= 1024

def compare_size(raw: Path, recompiled: Path):
    raw_size = raw.stat().st_size
    recompiled_size = recompiled.stat().st_size
    size_inc = recompiled_size - raw_size

    logger.info(f"Raw size:\t{human_readable_size(raw_size)}")
    logger.info(f"Recompiled size:\t{human_readable_size(recompiled_size)}")
    logger.info(f"Size increment:\t{human_readable_size(recompiled_size - raw_size)}\t{size_inc/100/raw_size:.2%}")

def uroboros_all(targets: list, args):
    arches = [args.m] if args.m is not None else [32, 64]
    pies = [not args.no_pie] if args.no_pie is True else [True, False]
    statics = [args.static] if args.static is True else [True, False]
    for arch in arches:
        for pie in pies:
            for static in statics:
                for source in targets:
                    binary_dir = source.parent / source.stem
                    if binary_dir.exists() is False:
                        binary_dir.mkdir()
                    binary = (
                        binary_dir
                        / f"{source.stem}.{arch}.{'pie' if pie else 'nopie'}.{'static' if static else 'dynamic'}"
                    )

                    # always compile
                    #if binary.exists() is False or args.force:
                    compile_bin(source, binary, arch, pie, static)
                    if not args.compile:
                        run_uroboros(binary)


if __name__ == "__main__":
    # Parse args
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--dir",
        "-d",
        type=str,
        help="Path of Uroboros project root",
        default="..",
    )
    parser.add_argument(
        "--force", "-f", action="store_true", help="Force recompile", default=False
    )
    parser.add_argument(
        "--compile", "-c", action="store_true", help="Compile only", default=False
    )

    input_args = parser.add_argument_group(
        "Input options"
    ).add_mutually_exclusive_group(required=True)
    input_args.add_argument(
        "--all", "-a", action="store_true", help="Run all tests on all programs"
    )
    input_args.add_argument(
        "--input", "-i", type=str, help="Run all tests on given program"
    )

    test_args = parser.add_argument_group("Test options")
    test_args.add_argument("-m", type=int, choices=[32, 64], help="32 bits or 64 bits")
    test_args.add_argument("--no-pie", action="store_true", help="Disable PIE flag")
    test_args.add_argument("--stripped", action="store_true", help="Strip binary")
    test_args.add_argument("--static", action="store_true", help="Static binary")
    test_args.add_argument(
        "-g", "--debug-symbol", action="store_true", help="Compile with debug symbols"
    )
    # test_args.add_argument(
    #     "--lib",
    #     action="store_true",
    #     help="Compile as library if possible",
    #     default=False,
    # )

    args = parser.parse_args()

    logger = logging.getLogger("test_uroboros")
    logger.setLevel(logging.DEBUG)

    # Create a console handler and set its log level to INFO
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.INFO)

    # Create a formatter and add it to the console handler
    formatter = logging.Formatter("{%(name)s - %(levelname)s} %(message)s")
    console_handler.setFormatter(formatter)

    # Add the console handler to the logger
    logger.addHandler(console_handler)

    # Set directories
    root_dir = Path(args.dir).resolve()
    if os.getenv("GITHUB_ACTIONS") == "true":
        root_dir = root_dir / "AIL"
    test_dir = root_dir / "test"
    uroboros_dir = root_dir / "src"
    uroboros = uroboros_dir / "uroboros.py"
    if uroboros.exists() is False:
        logger.error(f"Uroboros not found at {uroboros}")
        exit(1)

    logger.info(f"Test directory: {test_dir}")

    uroboros_all(get_inputs(args), args)
