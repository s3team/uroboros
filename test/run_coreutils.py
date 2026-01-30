#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import glob
import os
import argparse
import subprocess
import time
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
PROJ_ROOT_PATH = str(SCRIPT_DIR.parent)


def run(executable_binaries: list[str], arch):
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
            print(f"Timeout expired: {binary_only_name}")
            with open(result_path, "w") as f:
                f.write(f"Timeout expired: {binary_only_name}")
            continue
        end_time = time.time()

        # Save result.stdout to /home/gpk5233/work/research/Uroboros_ws/AIL/src
        with open(result_path, "w") as f:
            f.write(result.stdout.decode())
            # Save the time diff
            time_diff = end_time - start_time
            f.write(f"Time taken: %.2f\n" % time_diff)

        print(f"Time taken: %.2f" % time_diff)

        # Check if final.s and a.out are generated
        if not os.path.exists(f"{PROJ_ROOT_PATH}/src/final.s"):
            print(f"final.s not generated for {binary_only_name}\n")
            continue

        if not os.path.exists(f"{PROJ_ROOT_PATH}/src/a.out"):
            print(f"a.out not generated for {binary_only_name}\n")
            continue

        # Run qemu-arm -L /usr/arm-linux-gnueabihf/ ~/work/research/Uroboros_ws/AIL/src/a.out --help
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

        if (binary_only_name != "false" and qemu_result.returncode == 0) or (
            binary_only_name == "false" and qemu_result.returncode == 1
        ):
            print(f"qemu-arm succeeded: {binary_only_name}\n")
        else:
            print(f"qemu-arm failed: {binary_only_name}\n")

        # Remove the a.out and final.s files
        os.remove(f"{PROJ_ROOT_PATH}/src/a.out")
        os.remove(f"{PROJ_ROOT_PATH}/src/final.s")


def get_binaries(arch: str, programs: list[str], dir_path: str = None) -> list[str]:
    # Set the directory path based on architecture
    if dir_path is None:
        if arch == "thumb":
            dir_path = os.path.join(PROJ_ROOT_PATH, "install", "coreutils-arm-thumb", "src")
        elif arch == "arm":
            dir_path = os.path.join(PROJ_ROOT_PATH, "install", "coreutils-arm-32bit", "src")
        elif arch == "aarch64":
            dir_path = os.path.join(PROJ_ROOT_PATH, "install", "coreutils-arm-64bit", "src")

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

    executable_binaries = get_binaries(arch, programs, target_dir_path)

    print(f"Running tests with architecture: {arch}")
    # dry_run(uroboros_path, executable_binaries, arch)
    run(executable_binaries, arch)


if __name__ == "__main__":
    # get arguments options from command line like --arch, -m
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
    args = parser.parse_args()
    main(args)
