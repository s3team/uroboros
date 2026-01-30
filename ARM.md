# ARM Support

This document describes Uroboros’s support for **ARM Thumb** binaries.

ARM support is currently limited to ARM Thumb and is under active development.
Some binaries may not be handled correctly due to architecture-specific constraints.

## Supported Architectures
- ARM Thumb

## Requirements

### Cross-Compiler Toolchains

To build and test ARM binaries, the following cross-compilers may be required,
depending on the target environment:

```
$ sudo apt update

# For bare-metal ARM binaries
$ sudo apt install gcc-arm-none-eabi

# For 32-bit ARM Linux binaries
$ sudo apt install gcc-arm-linux-gnueabihf

# For 32-bit libraries (optional)
$ sudo apt install gcc-multilib libc6-dev-i386
```

### QEMU Installation

QEMU is recommended for executing and testing ARM binaries:

```
$ sudo apt install qemu-user
```

### Usage Example

```
python3 uroboros.py <binary_name> --arch thumb
```
