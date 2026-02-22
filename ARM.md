# ARM Support

This document describes Uroboros’s support for **ARM Thumb** and **ARM64** binaries.

ARM support is currently limited to ARM Thumb and ARM64, and is under active development.
Some binaries may not be handled correctly due to architecture-specific constraints.

## Supported Architectures
- ARM Thumb
- ARM64

## Requirements

### Cross-Compiler Toolchains

To build and test ARM binaries, the following cross-compilers may be required,
depending on the target environment:

```
$ sudo apt update

# For bare-metal ARM binaries
$ sudo apt install gcc-arm-none-eabi

# For ARM32 and ARM Thumb Linux binaries
$ sudo apt install gcc-arm-linux-gnueabihf

# For ARM64 Linux binaries
$ sudo apt install gcc-aarch64-linux-gnu

# For 32-bit libraries (optional)
$ sudo apt install gcc-multilib libc6-dev-i386
```

### QEMU Installation

QEMU is recommended for executing and testing ARM binaries:

```
$ sudo apt install qemu-user
```

### Usage Example

To analyze an ARM Thumb binary called **test**:
```
$ python3 uroboros.py test --arch thumb
```

To analyze an ARM64 binary called **test**:
```
$ python3 uroboros.py test --arch arm
```
