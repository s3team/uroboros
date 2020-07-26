# Uroboros: Infrastructure for Reassembleable Disassembling and Transformation (v 0.11)

## Installation

Uroboros uses the following utilities:
(Version numbers indicate the versions that we use in our development and tested with ubuntu 18.04 LTS system.)
```
  objdump 2.30
  readelf 2.30
  awk 4.1.4
```
Now the Uroboros is updated and it can be built by the OCaml compiler (version 4.05.0 or later)   
with the following libraries:
```
  deriving 
  ocamlfind 
  parmap 
  batteries 
  ocamlbuild 
```
We recommend to use the utilities distributed with 64-bit Ubuntu 18.0.4 LTS. 
The OCaml compiler and libraries can be obtained through
[opam](https://opam.ocaml.org/).
The the initial version of Uroboros was released with OCaml 4.01.0 version of the compilar in 2015 and that can be obtained through [uroboros ZIP file](https://github.com/s3team/uroboros/blob/master/uroboros-OCaml%20_4.01.0.zip). 
## Build

To build Uroboros, run the command below at the src folder.
```
    ./build
```
## Usage: Disassembling

Uroboros can take 64-bit and 32-bit ELF executable binaries as the
input. To use Uroboros for disassembling:
```
    python uroboros.py bzip
```
The disassembled output can be found at ./src/workdir dicrectory, named
**final.s**. Uroboros will also assemble it back into an executable,
**a.out**.

Python script uroboros.py provides multiple options to manipulate the
disassembling process.

1. -i (iteration):

The disassemble-reassemble process can be iterated for
multiple times. For example.
```
    python uroboros.py bzip -i 500
```
2. -k (keep):

This option will create a folder to store the assembly code and binary
generated from each iteration.  This is only effective together with -i.
```
    python uroboros.py bzip -i 500 -k
```
A subfolder will be created in ./src/workdir folder, with input binary name and
timestamp. For example:
```
    test_fold_bzip_2020-07-24_11:11:50
```

3. -a (assumption):

This option configures the three symbolization assumptions proposed in
the original Uroboros paper [1]. Note that in the current version, the
first assumption (**n-byte alignment**) are set by default. The other
two assumptions can be set by users.

Assumption two:
```
    python uroboros.py bzip -a 2
```
Note that by accepting this assumption, we need to put data sections (.data,
.rodata and .bss) to its original starting addresses. Linker scripts can be
used during reassembling. For exmaple:
```
    gcc -Tld_gobmk.sty final.s
```
Users may write their own linker script, some examples are given at
*./src/ld_script* folder.


Assumption three:
```
    python uroboros.py bzip -a 3
```

This assumption requires to know the function starting addresses. To
obtain this information, Uroboros can take unstripped binaries
as input. The function starting address information is obtained from
the input, which is then stripped before disassembling.


These assumptions can be used together.
```
    python uroboros.py bzip -a 3 -a 2
```

## Usage: Use Uroboros to instrument binaries

Instrumentation tools process the internal data structure of
Uroboros. Some examples are shown in the *./src/plugins* folder. You
may start with **mem_write.ml**, which instruments every memory write
operation.

In order to register instrumentation code, users need to add some
code at *./src/ail.ml*, starting from line 138. For example, in order to
resgiter the "mem_write" tool, three lines of code need to be added as follows:
```
    let open Mem_write in
    let module MW = Mem_write in
    let il' = MW.process il in
```
We will provide a better way in our next release.

## Publications
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
