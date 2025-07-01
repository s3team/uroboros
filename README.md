# Uroboros: Infrastructure for Reassembleable Disassembling and Transformation

## Installation

### Docker

Uroboros is available as a docker image, you can check the details in [docker](docker).

### Build

The OCaml compiler and dependent libraries can be obtained through
[opam](https://opam.ocaml.org/):
```
$ sudo apt install opam
$ opam init
$ opam switch create 5.3.0
$ opam install -y ppx_deriving.6.0.3 parmap.1.2.5 batteries.3.9.0
$ eval $(opam env)
```

## Usage

Uroboros can take a 32-bit or 64-bit, statically-linked or dynamically-linked ELF executable as the
input.  To use Uroboros:
```
$ python3 uroboros.py bzip
```

The disassembled output can be found at current directory, named
**final.s**. Uroboros will also reassemble it back into an executable, named
**a.out**.

Python script uroboros.py provides options to manipulate the
disassemble-reassemble process.

1. -i (iteration):

The disassemble-reassemble process can be iterated
multiple times:
```
$ python3 uroboros.py bzip -i 500
```

2. -k (keep):

This option will create a folder to store the assembly code and binary
generated from each iteration.  This is only effective together with -i:
```
$ python3 uroboros.py bzip -i 500 -k
```

A subfolder will be created in `./src` folder, with input binary name and
timestamp (e.g., `test_fold_bzip_2021-09-19_05:51:00`).

## Instrumentation

One application of Uroboros is binary code instrumentation.
The documentation for Uroboros' instrumentation framework
can be found in [INSTRUMENTATION.md](INSTRUMENTATION.md).

## Publication
```
@inproceedings {190920,
author = {Shuai Wang and Pei Wang and Dinghao Wu},
title = {Reassembleable Disassembling},
booktitle = {24th {USENIX} Security Symposium ({USENIX} Security 15)},
year = {2015},
isbn = {978-1-939133-11-3},
address = {Washington, D.C.},
pages = {627--642},
url = {https://www.usenix.org/conference/usenixsecurity15/technical-sessions/presentation/wang-shuai},
publisher = {{USENIX} Association},
month = aug,
}
```
