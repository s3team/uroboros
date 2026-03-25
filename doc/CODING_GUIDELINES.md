# Coding Guidelines

The following is the OCaml coding guidelines we follow.
We are also inspired by these other coding guidelines:
[OCaml Programming Guidelines](https://ocaml.org/docs/guidelines) and
[OCaml Towards Clarity and Grace](https://github.com/lindig/ocaml-style/tree/master).

## Prefer Compact and Simple Comments

Documentation comments, enclosed in `(**` and `*)`, are for API documentation (e.g., modules, functions, and types).

Regular comments, enclosed in `(*` and `*)`, are for all other comments.

In a documentation comment, use `[]` to inline code to refer to function name and arguments:
```ocaml
(** [add x y] returns [x] plus [y] *)
add x y
```

The opening parenthesis, `(*`, is always at the beginning of the first line of a comment, and not on its own line.
Similarly, the closing parenthesis, `*)`, is always at the end of the last line of a comment, and not on its own line:
```ocaml
(** [add x y] returns [x] plus [y] *)
```
instead of
```ocaml
(**
    [add x y] returns [x] plus [y]
*)

(** [add x y] returns [x] plus [y]
*)

(**
    [add x y] returns [x] plus [y] *)
```

For a multi-line comment, use asterisk, `*`, only in the first and last lines of a comment:
```ocaml
(* This is the first line.
   This is the second line. *)
```
instead of 
```ocaml
(* This is the first line.
 * This is the second line. *)
```

## Avoid Deeply Nested Code

Generally, we try to avoid deeply nested code (e.g., nested exception handling `try ... with`, nested pattern matching `match ... with`, nested grouping `( ... )`) since deeply nested code hurt code readability and comprehension. 

To mitigate deeply nested pattern matching, use let-operators (e.g., `let*`, `let+`) or `Option.value`:
```ocaml
let add (x : int option) (y : int option) : int option =
  let* x' = x in
  let* y' = y in
  Some (x' + y')
```
or
```ocaml
let add (x : int option) (y : int option) : int option =
  let+ x' = x in
  let+ y' = y in
  x' + y'
```
instead of
```ocaml
let add (x : int option) (y : int option) : int option =
  match int_of_string_opt x with
  | None -> None
  | Some x' ->
    match int_of_string_opt y with
    | None -> None
    | Some y' -> Some (x' + y')
```

To mitigate deeply nested grouping, use application operator `@@`, pipe operation `|>`, and function composition operators such as `>>` and `<<`:
```ocaml
let fgh = h >> g >> f in
fgh x
```
or
```ocaml
let fgh = f << g << h in
fgh x
```
or
```ocaml
h x |> g |> f
```
or
```ocaml
f @@ g @@ h x
```
instead of
```ocaml
f (g (h x))
```

## Use Option Whenever Applicable

`Hashtbl.find_opt` instead of `Hashtbl.find`

`List.find_opt` instead of `List.find`

`int_of_string_opt` instead of `int_of_string`
