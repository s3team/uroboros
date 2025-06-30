# Instrumentation

One application of Uroboros is binary code instrumentation,
which is the arbitrary addition of user-specified code at a set of instrumentation
points or memory addresses.
Binary code instrumentation is useful for many reasons, such as
retrofitting security mitigations and sanitizations (e.g., [ASan](https://github.com/google/sanitizers/wiki/addresssanitizer));
or enabling the collection of runtime information that is otherwise
inaccessible (e.g., argument values before a library function call).

A user needs to have knowledge of the internal data structures in Uroboros.
The new instrumentation design with a domain specific language alleviates
this burden and frees users from laborious instrumentation scripts.
The backward compatibility is maintained with a special command in
the new design. Below we specify the instrumentation language.

To use Uroboros for instrumentation,
in the file `points.ins` (placed in `src/`), a user specifies the set of
instrumentation points in which an instrumentation point is specified 
per-line in one of the two formats described below.
`src/` contains multiple example `points.ins` files:
`points.test00.32.ins` and `points.test00.64.ins`
for executables `test00.32.nopie.dynamic.sym` and `test00.64.nopie.dynamic.sym`;
`points.test01.32.ins` and `points.test01.64.ins`
for executables `test01.32.nopie.dynamic.sym` and `test01.64.nopie.dynamic.sym`;
`points.test05.32.ins` and `points.test05.64.ins`
for executables `test05.32.nopie.dynamic.sym` and `test05.64.nopie.dynamic.sym`; and
`points.test07.32.ins` and `points.test07.64.ins`
for executables `test07.32.nopie.dynamic.sym` and `test07.64.nopie.dynamic.sym`.

The first format allows a user to specify the instrumentation points using
a natural language-like, domain-specific language.
The second format frees a user from the constraints of our
domain-specific language but requires the user to understand the internal
data structures of Uroboros.
As a starter, we recommend instrumenting using the
first format before the second format
since the former, provided the domain-specific language, is easier to use.
`points.ins` can contain both formats (i.e., instrumentation points do not need to all be in format 1 or all be in format 2).

## Format 1
```
action direction loc loc-modifier stack cmd code-entry-point code;
```

`action` is one of `{insert, insertcall, delete, replace, printargs}`.
`action` is case-insensitive, e.g., `insert` or `INSERT`.

`direction` is one of `{before, after}`.
`direction` is case-insensitive.
If not used, then `x`.
It is not used for action `delete` or `replace`.

`loc` is a list like `[address, symbol, address1-address2]`.

`loc-modifier` is one of `{self, callsite, funentry, funexit}`.
`loc-modifier` is case-insensitive.

`stack` is a list like `[int:10,char*:DEFINED_IN_CODE]` where an argument
is specified with its type and value, separated by colon.
`DEFINED_IN_CODE` is a string (`char*`) or integer variable (`int*`) defined as a global variable in the binary.
It is used for action `insertcall` and `printargs`.
For `printargs`, the argument value is not required and can be left as `-`,
e.g., `[int:-,char*:-]`.
If not used, then it is empty `[]`.

`cmd` is the compiler command and options to compile the instrumentation code. The command is surrounded by quotes like `"gcc -no-pie -c to_insert.c"` which compiles `to_insert.c` to `to_insert.o`. If not used, then it is empty `""`.

`language` is one of `{asm, C, OCaml}`. If not used, then `x`.
`language` is case-insensitive.
For `asm`, it needs to be written in AT&T syntax.

`code-entry-point` is the function to call in file name `code`. If not used, then `x`.

`code` is either the code snippet or a file name.
For code snippet, surround it with quotes and place each instrument of the snippet on its own line like the following:
```assembly
"xor %eax, %eax
xor %ebx, %ebx
xor %ecx, %ecx"
```
If `code` is not used, then `x`.

Semicolon denotes the end of an instrumentation point.

__NOTE:__ language `OCaml` is work-in-progress. All valid combinations
without it is available to use.

### Examples

#### :point_right: example 1 (points.test05.64.ins)

```
INSERT BEFORE [print_info] CALLSITE [] "gcc -no-pie -c /root/fun.c" C before_print_info /root/fun.c;
```
The above will insert before any call to function `print_info` with call to `before_print_info` defined in `/root/fun.c`.

```
INSERT BEFORE [0x4011b5,0x401188] SELF [] "gcc -no-pie -c /root/fun.c" C log_execution_times /root/fun.c;
```
The above will insert before memory addresses 0x4011b5 and 0x401188 with call to `log_execution_times` defined in `/root/fun.c`.

```
INSERT BEFORE [0x401177] SELF [] "" asm x "movl $30, %eax";
```
The above will insert before the instruction with memory address 0x401177 with the assembly instruction that assigns %eax with 30.

```
INSERT AFTER [print_info] FUNENTRY [] "" asm x
"xor %eax, %eax
xor %ebx, %ebx
xor %ecx, %ecx";
```
The above will insert after the function entry of `print_info` with the three `xor` instructions.

```
INSERT BEFORE [print_info] FUNEXIT [] "" asm x
"xor %eax, %eax
xor %ebx, %ebx
xor %ecx, %ecx";
```
The above will insert before the function exit of `print_info` (i.e., before the `ret` instruction in `print_info`) with the three `xor` instructions.

```
DELETE NONE [0x40116f] SELF [] "" x x x;
```
The above will delete the instruction at memory address 0x40116f.

```
REPLACE NONE [0x40118d-0x4011a5] SELF [] "" asm x /root/fun.asm;
```
The above will replace instructions at addresses 0x40118d to 0x4011a5 with the assembly code defined in `/root/fun.asm`.

Overall, the combined examples can be found in the provided `points.test05.64.ins` (located in `src/`). For the binary `/test/test05/test05.64.nopie.dynamic.sym` (source is located at `/test/test05.c`), its output is the following
```
name: Jinquan Zhang
age: 26
gender: m
```
After renaming the provided `points.test05.64.ins` to `points.ins` and instrumentation by Uroboros:
```
cp points.test05.64.ins points.ins
python3 uroboros.py test/test05/test05.64.nopie.dynamic.sym
```
the output of the recompiled `a.out` is the following:
```
called 1 times
before call to print_info
age: 30
called 2 times
called 3 times
```

#### :point_right: example 2 (points.test00.64.ins)

In the following, we will discuss examples for `INSERTCALL` to insert user-defined functions that takes arguments.

```
INSERTCALL BEFORE [0x401175] SELF [] "gcc -c ./fun.c" C print_args ./fun.c;
```
The above will insert a call to `print_args` (defined in `fun.c`) using arguments already assigned in code (0x401175 is before a call to printf where printf's two arguments are already assigned); `print_args` requires two arguments and the two arguments are those assigned for printf. 

```
INSERTCALL BEFORE [0x401170] SELF [var:A] "gcc -c ./fun.c" C print_int ./fun.c;
```
The above will insert a call to `print_int` (defined in `fun.c`) with argument `A` (defined in the binary) before memory address 0x401170.

```
INSERTCALL BEFORE [0x401184] SELF [var:X] "gcc -c ./fun.c" C print_string ./fun.c;
```
The above will insert a call to `print_string` (defined in `fun.c`) with argument `X` (defined in the binary) before memory address 0x401184.

Overall, the combined examples can be found in the provided `points.test00.64.ins` (located in `src/`). For the binary `/test/test00/test00.64.nopie.dynamic.sym` (source is located at `/test/test00.c`), its output is the following
```
10
hello world
```
After renaming the provided `points.test00.64.ins` to `points.ins` and instrumentation by Uroboros:
```
cp points.test00.64.ins points.ins
python3 uroboros.py test/test00/test00.64.nopie.dynamic.sym
```
the output of the recompiled `a.out` is the following:
```
passed int: 10
^passed: %d
 10^
10
passed string: hello world
hello world
```

#### :point_right: example 3 (points.test07.64.ins)

In the following, we will discuss examples for `PRINTARGS` to print arguments of
a function before the callsite.

```
PRINTARGS BEFORE [0x401170] SELF [int:-] "" C x x;
```
Supposed at memory address 0x401170 is a function call that takes one argument
of type integer. The above will insert instructions to print the
integer argument.

```
PRINTARGS BEFORE [0x401186] SELF [char*:-,int:-] "" C x x;
```
Similarly, the above assumed that 0x401186 is a function call that takes two
arguments of type string (`char*`) and integer, and inserts instructions
to print the two arguments before the call.

Overall, the combined examples can be found in the provided `points.test07.64.ins` (located in `src/`). For the binary `/test/test07/test07.64.nopie.dynamic.sym` (source is located at `/test/test07.c`), its output is the following
```
3628800
```
After renaming the provided `points.test07.64.ins` to `points.ins` and instrumentation by Uroboros:
```
int arg: 10
int arg: 3628800
char* arg: %d

3628800
```

## Format 2
```
user module_path
```
Format 2 must always begin with `user` and follow by module path `module_path`.
Format 2 executes the OCaml module found in the module path for an arbitrary user-written instrumentation that operates on Uroboros' internal data structure. Function `instrument` must be defined inside `module_path` and is assumed to be the entrypoint. Examples modules can be found in the folder `src/plugins/`.

### Examples

```
user plugins/plugins/instr_asm.ml
```
`instr_asm.ml` inserts the assembly code defined in the file `generic_instr_asm.asm`
at locations defined in the file `instrument_locs.ins`.

```
user plugins/plugins/instr_c.ml
```
`instr_c.ml` inserts the compiled C code for the C source defined in the file `generic_instr_fun.c` at locations defined in the file `instrument_locs.ins`.

The instrumentation files `points.test01.32.ins` and `points.test01.64.ins`
contain the above two instrumentation points to insert both user-defined C and
assembly code in the executables `test01.32.nopie.dynamic.sym` and `test01.64.nopie.dynamic.sym`, respectively:
```
user plugins/plugins/instr_asm.ml
user plugins/plugins/instr_c.ml
```
The dependent files for `instr_asm.ml` and `instr_c.ml` (e.g., `generic_instr_fun.c`, `generic_instr_asm.asm`, and
`instrument_locs.ins`) are provided in the folder `src/`.