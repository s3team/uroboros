open Type
open Printf

module TagUtils = struct
  let get_tag (i : instr) : tag option =
    match i with
    | SingleInstr (_, _, _, tag, _) -> tag
    | DoubleInstr (_, _, _, _, tag, _) -> tag
    | TripleInstr (_, _, _, _, _, tag, _) -> tag
    | FourInstr (_, _, _, _, _, _, tag, _) -> tag
    | FifInstr (_, _, _, _, _, _, _, tag, _) -> tag

  let remove_tag (i : instr) : instr =
    match i with
    | SingleInstr (op, l, pre, _, tags) -> SingleInstr (op, l, pre, None, tags)
    | DoubleInstr (op, e, l, pre, _, tags) -> DoubleInstr (op, e, l, pre, None, tags)
    | TripleInstr (op, e1, e2, l, pre, _, tags) ->
        TripleInstr (op, e1, e2, l, pre, None, tags)
    | FourInstr (op, e1, e2, e3, l, pre, _, tags) ->
        FourInstr (op, e1, e2, e3, l, pre, None, tags)
    | FifInstr (op, e1, e2, e3, e4, l, pre, _, tags) ->
        FifInstr (op, e1, e2, e3, e4, l, pre, None, tags)

  let replace_tag (i : instr) (new_tag : tag option) : instr =
    match i with
    | SingleInstr (op, l, pre, _, tags) -> SingleInstr (op, l, pre, new_tag, tags)
    | DoubleInstr (op, e, l, pre, _, tags) -> DoubleInstr (op, e, l, pre, new_tag, tags)
    | TripleInstr (op, e1, e2, l, pre, _, tags) ->
        TripleInstr (op, e1, e2, l, pre, new_tag, tags)
    | FourInstr (op, e1, e2, e3, l, pre, _, tags) ->
        FourInstr (op, e1, e2, e3, l, pre, new_tag, tags)
    | FifInstr (op, e1, e2, e3, e4, l, pre, _, tags) ->
        FifInstr (op, e1, e2, e3, e4, l, pre, new_tag, tags)
    | _ -> failwith "Unsupported instruction type for tag replacement"

  let p_hex s = "0x" ^ Printf.sprintf "%x" s

  let convert_to_symbol (i : instr) (addr : int) : instr =
    match i with
    | TripleInstr (p, e1, e2, loc, prefix, _, tags) ->
        let sym_str = "S_" ^ p_hex addr in
        let new_i = TripleInstr (p, Label sym_str, e2, loc, prefix, None, tags) in
        new_i
    | _ -> i

  (** Sym tag is handled in [Arm_reassemble_symbol_get#vinst2]. *)
  let handle_sym (ilist : instr list) =
    let rec help ilist acc =
      match ilist with
      | [] -> List.rev acc
      | i :: t -> begin
          match get_tag i with
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
          | TripleInstr (op, e1, Reg reg, l, pre, Some Deref, tags) ->
              let deref_instr =
                TripleInstr (op, Ptr (UnOP reg), Reg reg, l, pre, None, tags)
              in
              let tag_removed_i = remove_tag i in
              help t (deref_instr :: tag_removed_i :: acc)
          | _ -> help t (i :: acc)
        end
    in
    help ilist []

  let handle_del (ilist : instr list) =
    let rec help ilist acc =
      match ilist with
      | [] -> List.rev acc
      | i :: t -> begin
          match get_tag i with
          | Some Del -> help t acc (* Skip instructions with Del tag *)
          | _ -> help t (i :: acc)
        end
    in
    help ilist []

  let process_tags (ilist : instr list) : instr list =
    handle_deref ilist |> handle_del
end
