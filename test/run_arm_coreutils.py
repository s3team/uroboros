#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import glob
import os
import sys
import argparse
import subprocess
import time
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
PROJ_ROOT_PATH = str(SCRIPT_DIR.parent)


def run(executable_binaries: list[str], arch, fail_fast: bool = False):
    def _fail(msg):
        print(msg, flush=True)
        if fail_fast:
            sys.exit(1)

    for binary in executable_binaries:
        binary_only_name = os.path.basename(binary)
        result_path = f"{PROJ_ROOT_PATH}/src/{binary_only_name}.result"
        print(f"python3 uroboros.py {binary} --arch {arch}", flush=True)

        # If runtime exceed 600 seconds, terminate the process
        timeout = 600
        start_time = time.time()
        try:
            result = subprocess.run(
                ["python3", "uroboros.py", binary, "--arch", arch],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=timeout,
                cwd=PROJ_ROOT_PATH + "/src",
            )
        except subprocess.TimeoutExpired:
            with open(result_path, "w") as f:
                f.write(f"Timeout expired: {binary_only_name}")
            _fail(f"Timeout expired: {binary_only_name}")
            continue
        end_time = time.time()

        # Save result.stdout to src
        with open(result_path, "w") as f:
            f.write(result.stdout.decode())
            # Save the time diff
            time_diff = end_time - start_time
            f.write(f"Time taken: %.2fs\n" % time_diff)

        print(f"Time taken: %.2fs" % time_diff)

        # Check if final.s and a.out are generated
        if not os.path.exists(f"{PROJ_ROOT_PATH}/src/final.s"):
            _fail(f"final.s not generated for {binary_only_name}\n")
            continue

        if not os.path.exists(f"{PROJ_ROOT_PATH}/src/a.out"):
            _fail(f"a.out not generated for {binary_only_name}\n")
            continue

        # Run qemu-arm -L /usr/arm-linux-gnueabihf/ a.out --help
        # and record if it runs successfully or not
        print("Running qemu-arm to verify the binary...", flush=True)
        print(
            f"qemu-arm -L /usr/arm-linux-gnueabihf/ {PROJ_ROOT_PATH}/src/a.out --help",
            flush=True,
        )
        qemu_result = subprocess.run(
            [
                "qemu-arm",
                "-L",
                "/usr/arm-linux-gnueabihf/",
                f"{PROJ_ROOT_PATH}/src/a.out",
                "--help",
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

        qemu_ok = (binary_only_name != "false" and qemu_result.returncode == 0) or (
            binary_only_name == "false" and qemu_result.returncode == 1
        )

        # Remove the a.out and final.s files
        os.remove(f"{PROJ_ROOT_PATH}/src/a.out")
        os.remove(f"{PROJ_ROOT_PATH}/src/final.s")

        if qemu_ok:
            print(f"qemu-arm succeeded: {binary_only_name}\n")
        else:
            _fail(f"qemu-arm failed: {binary_only_name}\n")


def get_default_dir_path(arch: str) -> str | None:
    # install/install_coreutils.arm.sh writes these dirs at the project root
    if arch == "thumb":
        return os.path.join(PROJ_ROOT_PATH, "coreutils-arm-thumb", "src")
    if arch == "arm":
        return os.path.join(PROJ_ROOT_PATH, "coreutils-arm-32bit", "src")
    if arch == "aarch64":
        return os.path.join(PROJ_ROOT_PATH, "coreutils-arm-64bit", "src")
    return None


def ensure_coreutils_built(dir_path: str) -> None:
    if os.path.isdir(dir_path):
        return
    print(f"Coreutils directory not found: {dir_path}", flush=True)
    print("Running install/install_coreutils.arm.sh ...", flush=True)
    result = subprocess.run(
        ["./install/install_coreutils.arm.sh"],
        cwd=PROJ_ROOT_PATH,
    )
    if result.returncode != 0 or not os.path.isdir(dir_path):
        print(f"install_coreutils.arm.sh did not produce {dir_path}", flush=True)
        sys.exit(1)


def get_binaries(arch: str, programs: list[str], dir_path: str = None) -> list[str]:
    if dir_path is None:
        dir_path = get_default_dir_path(arch)

    # Get all executable binaries in the `dir_path`
    executable_binaries = glob.glob(os.path.join(dir_path, "*"))
    executable_binaries = [f for f in executable_binaries if os.access(f, os.X_OK)]

    # Filter binaries if specific programs are provided
    if programs:
        executable_binaries = [
            binary
            for binary in executable_binaries
            if os.path.basename(binary) in programs
        ]

    # Remove excluded binaries from the list
    exclude_list = ["dcgen", "blake2", "libstdbuf.so"]
    executable_binaries = [
        binary
        for binary in executable_binaries
        if os.path.basename(binary) not in exclude_list
    ]

    # Sort the binaries in ascending order on sizes
    executable_binaries.sort(key=os.path.getsize)

    return executable_binaries


def dry_run(uroboros_path: str, executable_binaries: list[str], arch):
    command = ["python3", uroboros_path, "--arch", arch]
    for binary in executable_binaries:
        command.append(binary)
        print(f"Dry run command: {' '.join(command)}")


def main(args):
    arch = args.arch
    programs = args.programs
    target_dir_path = args.dir_path
    if target_dir_path is not None:
        target_dir_path = os.path.abspath(target_dir_path)
    else:
        target_dir_path = get_default_dir_path(arch)

    ensure_coreutils_built(target_dir_path)
    executable_binaries = get_binaries(arch, programs, target_dir_path)

    print(f"Running tests with architecture: {arch}")
    # dry_run(uroboros_path, executable_binaries, arch)
    run(executable_binaries, arch, fail_fast=args.fail_fast)


if __name__ == "__main__":
    # Get arguments options from command line like --arch, -m
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--arch",
        type=str,
        default="thumb",
        help="Architecture to use (default: thumb), options: thumb, arm, aarch64",
    )
    # Get binary names to test them only
    parser.add_argument(
        "--programs",
        type=str,
        nargs="*",
        help="List of binary names to test (default: all binaries)",
    )
    parser.add_argument(
        "--dir-path",
        type=str,
        default=None,
        help="Directory path to get binaries (default: install/coreutils*)",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Exit with non-zero status on the first failure (recompile or qemu)",
    )
    args = parser.parse_args()
    main(args)
