(*basically, lexer consume a string: string_lexer and create a lex:lexeme
 assemble code's parser should be quite easy*)

open Batteries
open Printf

open Ail_utils

type lexeme = | Lop of string
              | Lexp of string
              | Lloc of string
              | Lend
exception LexerError

let pp_print l =
  let rec help l =
    match l with
    | h::t -> (printf "item: %s") h; help t
    | [] -> print_string "end\n" in
    help l


let check_assist exp =
  let assist_list = ["cmpsb"; "scas"; "stos"; "movsl"; "movsb"; "cmpsw";
                    "movsq"; "pop"] in
    try ignore (List.find (fun i-> i = exp) assist_list); true
    with Not_found -> false

let assist_exp op ass exp loc =
   let items = Str.split (Str.regexp_string ",") exp in
    let lexm = Array.create 6 Lend in
    if List.length items = 1 then
    begin
    lexm.(0) <- Lop op;
    lexm.(1) <- Lexp ass;
    lexm.(2) <- Lexp (List.nth items 0);
    lexm.(3) <- Lloc loc;
    lexm
    end
    else
    begin
    lexm.(0) <- Lop op;
    lexm.(1) <- Lexp ass;
    lexm.(2) <- Lexp (List.nth items 0);
    lexm.(3) <- Lexp (List.nth items 1);
    lexm.(4) <- Lloc loc;
    lexm
    end

let char_collect (s : string) (f : int) (c : char) =
  try
	  Some (String.index_from s f c)
  with
  | _ -> None

let bracket_collect (s : string) : (int*int) list =
  let rec help s f c acc =
    match char_collect s f c with
    | None -> List.rev acc
    | Some i -> help s (i+1) c (i::acc)
  in
  zip (help s 0 '(' []) (help s 0 ')' [])

let comma_collect (s : string) : int list =
  let rec help s f c acc =
    match char_collect s f c with
    | None -> List.rev acc
    | Some i -> help s (i+1) c (i::acc)
  in
  help s 0 ',' []

let comma_in_brackets (e : string) : int list =
  let clist = comma_collect e
  and blist = bracket_collect e in
    List.filter (
        fun comma ->
          (
            List.exists (fun (f,r) -> comma > f && comma < r)  blist == false
          )
      ) clist

let single_instr (op:string) (l : string) =
  let lexm = Array.create 3 Lend in
    lexm.(0) <- Lop op;
    lexm.(1) <- Lloc l;
    lexm

let double_instr (op : string) (e : string) (l : string) =
  let lexm = Array.create 4 Lend in
    lexm.(0) <- Lop op;
    lexm.(1) <- Lexp (String.trim e);
    lexm.(2) <- Lloc l;
    lexm

let triple_instr (op : string) (el : string list) (l : string) =
  let lexm = Array.create 5 Lend in
    lexm.(0) <- Lop op;
    lexm.(1) <- Lexp (String.trim (List.nth el 0));
    lexm.(2) <- Lexp (String.trim (List.nth el 1));
    lexm.(3) <- Lloc l;
    lexm

let fourth_instr (op : string) (el : string list) (l : string) =
  let lexm = Array.create 6 Lend in
    lexm.(0) <- Lop op;
    lexm.(1) <- Lexp (List.nth el 0);
    lexm.(2) <- Lexp (List.nth el 1);
    lexm.(3) <- Lexp (List.nth el 2);
    lexm.(4) <- Lloc l;
    lexm

let fifth_instr (op : string) (el : string list) (l : string) =
  let lexm = Array.create 7 Lend in
    lexm.(0) <- Lop op;
    lexm.(1) <- Lexp (List.nth el 0);
    lexm.(2) <- Lexp (List.nth el 1);
    lexm.(3) <- Lexp (List.nth el 2);
    lexm.(4) <- Lexp (List.nth el 3);
    lexm.(5) <- Lloc l;
    lexm

let do_exp (e : string) (op : string) (l : string) =
  let cl = comma_in_brackets e in
    let cl_len = List.length cl in
      if cl_len == 0 then (* single exp instr *)
        double_instr op e l
      else if cl_len == 1 then (*double exp instr*)
        let el = split_by_list e cl in
          triple_instr op el l
      else if cl_len == 2 then (*triple exp instr*)
        let el = split_by_list e cl in
          fourth_instr op el l
      else if cl_len == 3 then (*fourth exp instr*)
        let el = split_by_list e cl in
          fifth_instr op el l
      else
        failwith "unsupport exp length"

let lexer' instr' location' =
  let instr = String.trim instr'
  and location = "0x"^(String.trim location')
  and split = Str.split (Str.regexp " +") in
    let rec remove l = 
      match l with
       | "notrack"::l -> remove l
       | "data16"::l-> remove l
       | _ -> l in
    let tokens = remove @@ split instr in
      let op_str = List.nth tokens 0 in
        if List.length tokens = 1 then (*single op instr *)
          single_instr op_str location
        else if check_assist (List.nth tokens 1) then
          assist_exp op_str (List.nth tokens 1) (cat_from tokens 2 " ") location
        else
          do_exp (cat_from tokens 1 " ") op_str location

let p_print lexme =
    match lexme with
    | Lend -> print_string " (End)\n"
    | Lop s -> print_string (" (Op "^s^")")
    | Lexp s -> print_string (" (Exp "^s^")")
    | Lloc  s -> print_string (" (Loc "^s^")")

(* typically, x86 instruction prefix looks like this
 *  lock incl (%ecx)
 *  right now, a ad-hoc way to identify prefix is impelmented below
 *
 *   *)
let prefix_identify instr =
  try
     ignore(String.find instr "lock "); true
   with Not_found -> false

let prefix_sub instr =
    try
     ignore(String.find instr "lock ");
     Str.global_replace (Str.regexp_string "lock ") "" instr
   with Not_found -> instr

let lexer instr location =
  (*print_string (instr^"\n"); *)
  let tokens = lexer' instr location in
  (* Array.iter p_print tokens; *)
    tokens
