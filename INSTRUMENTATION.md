Instrumentation

In a file, a user specifies the set of instrumentation points in which
an instrumentation point is specified per-line in the following
format:

action direction location loc-modifier stack cmd code code-entry-point
or
user module function

where action is one of {insert, delete, replace, printstack, user}, and
direction is one of {before, after, count(n), range(loc)}, and
location is one of {address, symbol}, and
loc-modifier is one of {self, callsite, funentry, funexit}, and
stack is either a list like {int,int,int*}, it could be {}, and
cmd is the compiler command and options to compile the instrumentation code, and 
# language is one of {asm, C, OCaml, -}, and
code is either the code snippet or a file name.

user m f
call a function f in an Ocaml module m, for an arbitrary user written instrumentation.

Examples

INSERT BEFORE foo callsite C foo.c f
insert before any call to function foo with foo.c (assuming a default function name)

INSERT BEFORE loc self {int,int,int*} C foo.c

delete, replace: with an address range, instruction count,

modify stack
call fun f with args on stack at a callsite to g, f has the same prototype as g
