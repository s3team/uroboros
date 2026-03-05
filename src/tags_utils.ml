open Type
open Printf
open Pp_print

let get_tags (i : instr) : (string, tags_val) Hashtbl.t =
  match i with
  | SingleInstr (_, _, _, tags) -> tags
  | DoubleInstr (_, _, _, _, tags) -> tags
  | TripleInstr (_, _, _, _, _, tags) -> tags
  | FourInstr (_, _, _, _, _, _, tags) -> tags
  | FifInstr (_, _, _, _, _, _, _, tags) -> tags

let set_tags (i : instr) (new_tags : (string, tags_val) Hashtbl.t) : instr =
  match i with
  | SingleInstr (p, l, pre, _) -> SingleInstr (p, l, pre, new_tags)
  | DoubleInstr (p, e1, l, pre, _) -> DoubleInstr (p, e1, l, pre, new_tags)
  | TripleInstr (p, e1, e2, l, pre, _) ->
      TripleInstr (p, e1, e2, l, pre, new_tags)
  | FourInstr (p, e1, e2, e3, l, pre, _) ->
      FourInstr (p, e1, e2, e3, l, pre, new_tags)
  | FifInstr (p, e1, e2, e3, e4, l, pre, _) ->
      FifInstr (p, e1, e2, e3, e4, l, pre, new_tags)

let get_instr_tag (i : instr) : instr_tag option =
  match Hashtbl.find_opt (get_tags i) "instr_tag" with
  | Some (InstrTag t) -> Some t
  | _ -> None

let remove_instr_tag (i : instr) : instr =
  Hashtbl.remove (get_tags i) "instr_tag";
  i

let replace_instr_tag (i : instr) (new_tag : instr_tag option) : instr =
  let tags = get_tags i in
  (match new_tag with
  | Some t -> Hashtbl.replace tags "instr_tag" (InstrTag t)
  | None -> Hashtbl.remove tags "instr_tag");
  i

let set_tag_by_kv k v tags =
  let new_tags =
    Hashtbl.fold
      (fun k v acc ->
        Hashtbl.add acc k v;
        acc)
      tags
      (Hashtbl.create (Hashtbl.length tags))
  in
  let _ = Hashtbl.add new_tags k v in
  new_tags

let has_sym_instr_tag (i : instr) : bool =
  match get_instr_tag i with Some (Sym _) -> true | _ -> false

let get_sym_addr (i : instr) : int option =
  match get_instr_tag i with Some (Sym addr) -> Some addr | _ -> None

let p_hex s = "0x" ^ Printf.sprintf "%x" s

let convert_to_symbol (i : instr) (addr : int) : instr =
  match i with
  | TripleInstr (p, e1, e2, loc, prefix, tags) ->
      let sym_str = "S_" ^ p_hex addr in
      TripleInstr (p, Label sym_str, e2, loc, prefix, tags)
  | _ -> i

(** Sym instr_tag is handled in [Arm_reassemble_symbol_get#vinst2]. *)
let handle_sym (ilist : instr list) =
  let rec help ilist acc =
    match ilist with
    | [] -> List.rev acc
    | i :: t -> begin
        match get_instr_tag i with
        | Some (Sym addr) -> help t (convert_to_symbol i addr :: acc)
        | _ -> help t (i :: acc)
      end
  in
  help ilist []

let handle_deref (ilist : instr list) =
  let rec help ilist acc =
    match ilist with
    | [] -> List.rev acc
    | i :: t -> begin
        match i with
        | TripleInstr (op, e1, Reg reg, l, pre, tags)
          when get_instr_tag i = Some Deref ->
            let deref_instr =
              TripleInstr (op, Ptr (UnOP reg), Reg reg, l, pre, tags)
            in
            let _ = remove_instr_tag i in
            help t (deref_instr :: i :: acc)
        | _ -> help t (i :: acc)
      end
  in
  help ilist []

let handle_del (ilist : instr list) =
  let rec help ilist acc =
    match ilist with
    | [] -> List.rev acc
    | i :: t -> begin
        match get_instr_tag i with
        | Some Del -> begin
            let _ = Printf.printf "Del: %s\n" (pp_print_instr' i) in
            help t acc (* Skip instructions with Del instr_tag *)
          end
        | _ -> help t (i :: acc)
      end
  in
  help ilist []

let process_tags (ilist : instr list) : instr list =
  handle_deref ilist |> handle_del
