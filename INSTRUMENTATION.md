# Instrumentation

One application of Uroboros is binary code instrumentation,
which is the arbitrary addition of user-specified code at a set of instrumentation
points or memory addresses.
Binary code instrumentation is useful for many reasons, such as
retrofitting security mitigations and sanitizations (e.g., [ASan](https://github.com/google/sanitizers/wiki/addresssanitizer)),
or enabling the collection of runtime information that is otherwise
inaccessible (e.g., argument values before a library function call).

A user needs to have knowledge of the internal data structures in Uroboros.
The new instrumentation design with a domain specific language alleviates
this burden and frees users from laborious instrumentation scripts.
The backward compatibility is maintained with a special command in
the new design. Below we specify the instrumentation language.

To use Uroboros for instrumentation,
specify the set of instrumentation points in a file placed in `src/points/`.
`src/point_examples/` contains multiple example instrumentation files:

- `points.test00.32.ins` and `points.test00.64.ins`
for executables `test00.32.nopie.dynamic.sym` and `test00.64.nopie.dynamic.sym`
- `points.test01.32.ins` and `points.test01.64.ins`
for executables `test01.32.nopie.dynamic.sym` and `test01.64.nopie.dynamic.sym`
- `points.test05.32.ins` and `points.test05.64.ins`
for executables `test05.32.nopie.dynamic.sym` and `test05.64.nopie.dynamic.sym`
- `points.test07.32.ins` and `points.test07.64.ins`
for executables `test07.32.nopie.dynamic.sym` and `test07.64.nopie.dynamic.sym`

To use an example instrumentation file, copy the file to `src/points/` before executing `uroboros.py`.
To generate all the `test*` executables, execute the following command in the project root: `./test/test_all.py -a -c`.
As an example, the executable `test00.32.nopie.dynamic.sym` can be found in `src/test/test00/` after executing the previous command.

Following is the Table of Contents:

- [Instrumentation with the Domain-Specific Language](#instrumentation-with-the-domain-specific-language)
- [Instrumentation with OCaml Modules](#instrumentation-with-ocaml-modules)
- [Debugging](#debugging)

## Instrumentation with the Domain-Specific Language
Following is the format of an instrumentation point
using Uroboros' domain-specific language:
```
action direction loc loc-modifier stack cmd language code-entry-point code; # comment here
```
An instrumentation point must ends with a semicolon ";" in which a comment
beginning with a hash "#" may follow.

`action` is one of `{insert, insertcall, delete, replace, printargs, include}`.
`action` is case-insensitive, e.g., `insert` or `INSERT`.
* `insert`: insert user-specified code.
* `insertcall`: insert user-specified code that is specifically a function call.
* `delete`: delete existing code.
* `replace`: delete existing code and insert user-specified code in its place.
* `printargs`: print the argument values of an existing function call.
* `include`: include additional C, assembly, or object files into the instrumented executable binary. Following is an example for including c or assembly files file1.c, file2.c, and file3.s, located in different folders: `INCLUDE file1.o file2.o file3.o "gcc -no-pie -c a/b/c/file1.c && gcc -no-pie -c a/b/c/file2.c && gcc -c file3.s -o file3.o"`

`direction` is one of `{before, after, at}`.
`direction` is case-insensitive.
If not used, then `x`.
It is not used for action `delete` or `replace`.
* `before`: instrument at location before the specified address.
* `after`: instrument at location after the specified address.
* `at`: instrument at location that is the specified address. If the instruction at the specified address contains a label, the label will be moved to the beginning of the instrumented instructions.

`loc` is a list like `[address, symbol, address1-address2]`.
* `address`: the specified address for the instrumentation.
* `symbol`: the specified symbol for the instrumentation. This option is only available for executable binaries that are not stripped.
* `address1-address2`: the specified range of addresses.

`loc-modifier` is one of `{self, callsite, funentry, funexit}`.
`loc-modifier` is case-insensitive.
* `self`: instrument at the specified address.
* `callsite`: instrument at the callsites of the specified function symbol.
* `funentry`: instrument at the function entry of the specified function symbol.
* `funexit`: instrument at the function exit of the specified function symbol.

`stack` is a list like `[int:10,char*:DEFINED_IN_CODE]` where an argument
is specified with its type and value, separated by colon.
`DEFINED_IN_CODE` is a string (`char*`) or integer variable (`int*`) defined as a global variable in the binary.
It is used for action `insertcall` and `printargs`.
For `printargs`, the argument value is not required and can be left as `-`,
e.g., `[int:-,char*:-]`.
If not used, then it is empty `[]`.

`cmd` is the compiler command and options to compile the source code to instrument. The command is surrounded by quotes like `"gcc -no-pie -c to_insert.c"` which compiles `to_insert.c` to `to_insert.o`. If not used, then it is empty `""`.

`language` is one of `{asm, C}`. If not used, then `x`.
`language` is case-insensitive.
For `asm`, it must be written in AT&T syntax.

`code-entry-point` is the function to call in file name `code`. If not used, then `x`.

`code` is either the code snippet or a filepath.
For code snippet, surround it with quotes and place each instrument of the snippet on its own line like the following:
```assembly
"xor %eax, %eax
xor %ebx, %ebx
xor %ecx, %ecx"
```
If `code` is not used, then `x`.

Semicolon denotes the end of an instrumentation point.

### Examples

<details><summary>:point_right: <b>example 1 (points.test05.32.ins and points.test05.64.ins)</b></summary>

```
INSERT BEFORE [print_info] CALLSITE [] "gcc -no-pie -c ./instr_modules/c/fun.c" C before_print_info ./instr_modules/c/fun.c;
```
The above will insert before any call to function `print_info` with call to `before_print_info` defined in `fun.c`.

```
INSERT AFTER [print_info] FUNENTRY [] "gcc -no-pie -c ./instr_modules/c/fun.c" C log_execution_times ./instr_modules/c/fun.c;
```
The above will insert at the start of print_info with call to `log_execution_times` defined in `fun.c`.

```
INSERT BEFORE [print_info] FUNEXIT [] "gcc -no-pie -c ./instr_modules/c/fun.c" C log_execution_times ./instr_modules/c/fun.c;
```
The above will insert at the end of print_info with call to `log_execution_times` defined in `fun.c`.

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

Overall, the combined examples can be found in the provided `points.test05.64.ins` (located in `src/point_examples/`). For the binary `test/test05/test05.64.nopie.dynamic.sym` (source is located at `test/test05.c`), its output is the following
```
name: Jinquan Zhang
age: 26
gender: m
```
After placing the provided `points.test05.64.ins` in `src/points/` and running Uroboros:
```
cp src/point_examples/points.test05.64.ins src/points/
python3 uroboros.py test/test05/test05.64.nopie.dynamic.sym
```
the output of the recompiled `a.out` is the following:
```
before call to print_info
called 1 times
name: Jinquan Zhang
age: 26
gender: m
called 2 times
```

</details>

<details><summary>:point_right: <b>example 2 (points.test00.32.ins and points.test00.64.ins)</b></summary>

In the following, we will discuss examples for `INSERTCALL` to insert user-defined functions that takes arguments.

```
INSERTCALL BEFORE [printf] CALLSITE [] "gcc -c ./instr_modules/c/fun.c" C print_args ./instr_modules/c/fun.c;
```
The above will insert a call to `print_args` (defined in `fun.c`) using arguments already assigned in code, which are the arguments for printf, before printf's callsites.

```
INSERTCALL BEFORE [printf] CALLSITE [var:A] "gcc -c ./instr_modules/c/fun.c" C print_int ./instr_modules/c/fun.c;
```
The above will insert a call to `print_int` (defined in `fun.c`) with argument `A` (defined in the binary) before printf's callsites.

```
INSERTCALL BEFORE [puts] CALLSITE [var:X] "gcc -c ./instr_modules/c/fun.c" C print_string ./instr_modules/c/fun.c;
```
The above will insert a call to `print_string` (defined in `fun.c`) with argument `X` (defined in the binary) before puts' callsites.

Overall, the combined examples can be found in the provided `points.test00.64.ins` (located in `src/point_examples/`). For the binary `test/test00/test00.64.nopie.dynamic.sym` (source is located at `test/test00.c`), its output is the following
```
10
hello world
```
After placing the provided `points.test00.64.ins` in `src/points/` and running Uroboros:
```
cp src/point_examples/points.test00.64.ins src/points
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

</details>

<details><summary>:point_right: <b>example 3 (points.test07.32.ins and points.test07.64.ins)</b></summary>

In the following, we will discuss examples for `PRINTARGS` to print arguments of
a function before the callsite.

```
PRINTARGS BEFORE [fac] CALLSITE [int:-] "" C x x;
```
The above will insert instructions to print the
integer argument of fac before the call to fac.

```
PRINTARGS BEFORE [printf] CALLSITE [char*:-,int:-] "" C x x;
```
Similarly, the above ainserts instructions to print the two arguments of printf before its call.

Overall, the combined examples can be found in the provided `points.test07.64.ins`
(located in `src/point_examples/`). For the binary `test/test07/test07.64.nopie.dynamic.sym`
(source is located at `test/test07.c`), its output is the following
```
3628800
```
After placing the provided `points.test07.64.ins` in `src/points/` and running Uroboros:
```
int arg: 10
int arg: 3628800
char* arg: %d

3628800
```

</details>

## Instrumentation with OCaml Modules
Following is the format of an instrumentation point
to invoke instrumentation with an OCaml module:
```
user module_path; # comment here
```
Like the previous format, the instrumentation point must ends with a
semicolon ";" in which a comment beginning with a hash "#" may follow.

In addition to the domain-specific language for instrumentation, one can also
write custom OCaml module to perform the instrumentation. Writing custom OCaml
module gives user full access to the internal data structures of Uroboros in the
expense of a higher learning curve.
To invoke a custom OCaml module, the instrumentation point must always begin with `user` and follow by filepath to OCaml module `module_path`.
Function `instrument` must be defined inside `module_path` and is assumed to be the
entrypoint. Examples OCaml modules can be found in the folder `src/instr_modules/`.

### Examples

<details><summary>:point_right: <b>example 1 (points.test01.32.ins and points.test01.64.ins)</b></summary>

```
user instr_modules/instr_asm.ml;
```
`instr_asm.ml` inserts the assembly code defined in the file `generic_instr_asm.asm`
at locations defined in the file `instrument_locs.ins`.

```
user instr_modules/instr_c.ml;
```
`instr_c.ml` inserts the compiled C code for the C source defined in the file `generic_instr_fun.c` at locations defined in the file `instrument_locs.ins`.

The instrumentation files `points.test01.32.ins` and `points.test01.64.ins`
contain the above two instrumentation points to insert both user-defined C and
assembly code in the executables `test01.32.nopie.dynamic.sym` and `test01.64.nopie.dynamic.sym`, respectively:
```
user instr_modules/instr_asm.ml;
user instr_modules/instr_c.ml;
```
The dependent files for `instr_asm.ml` and `instr_c.ml` (e.g., `generic_instr_fun.c`, `generic_instr_asm.asm`, and
`instrument_locs.ins`) are provided in the folder `src/`.

</details>

## Debugging
Static reassembly or, in general, binary analysis is extremely difficult to perform automatically
since two different compilers, or the same compiler in under different flags, can generate binaries that
use completely different conventions (e.g., calling convention), making any heuristic unreliable.
Additionally, the prevalence of indirect calls, indirect jumps, and lack of useful source-level information
make reliable analysis difficult to scale and further exaberate the binary analysis problem.
Therefore, it is possible that Uroboros may generate a reassembled binary whose runtime behaviors
divert from the original binary, e.g., the reassembled binary segfaults but the original binary does not.

To make it easy to identify and subsequently fix Uroboros-generated errors,
a comment follows each Uroboros' inserted instruction in the symbolized assembly file,
`final.s`. 
A comment contains three parts: `filename`, `line`, and `instrumentation point`.
`filename` is the filename of the instrumentation file (recall that there can be multiple instrumentation files in `src/points/`).
`line` is the line number of the corresponding instrumentation point in the instrumentation file.
`instrumentation point` is the instrumentation point index. 
If each instrumentation point in the file is only one line long, then the line number and corresponding instrumentation are the same.

Under a debugger such as GDB, if the reassembled binary segfaults,
the user can identify whether if the segfaulting instruction is inserted by
Uroboros by referencing the symbolized assembly file `final.s`.