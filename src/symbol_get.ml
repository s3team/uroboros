open Ail_utils
open Type
open Pp_print


let dec_hex (s : int) : string = "0x"^(Printf.sprintf "%X" s)

let strip_symbol_prefix (s : string) : string =
  let len = String.length s in
  assert (len > 2);
  String.sub s 2 (len - 2)

let replace_at (c : char) : char =
  (* assembler rejects label containing @ *)
  match c with
  | '@' -> '_'
  | _ -> c

let parse () : (int, string) Hashtbl.t =
  let sym_addr2label = Hashtbl.create 100 in
  let seen = Hashtbl.create 100 in
  let filelines = read_lines "nm.info" in
  List.iter (
    fun l ->
      if not (contains l " U ")
        && not (contains l " w ")
        && not (contains l " v ")
        && not (contains l " V ")
      then
        let items = Str.split (Str.regexp " +") l in
        (* nm.info contains blank symbol as follows for some ARM thumb binaries:
        * 000345e1 ta
        * 00011804 t $a
        *
        * ignore the line that has no symbol by checking the length of `items`
        *)
        if List.length items = 3 then
          let addr = List.nth items 0 in
          let symbol_type = List.nth items 1 in
          let symbol_name = List.nth items 2 in
          let symbol_name' =
            symbol_name |> String.to_seq
                        |> Seq.map replace_at
                        |> String.of_seq
          in
          let addr' = int_of_string ("0x"^addr) in
          if symbol_name' <> "main" then
            if not (Hashtbl.mem seen symbol_name') then
              (Hashtbl.add sym_addr2label addr' symbol_name';
              Hashtbl.add seen symbol_name' "")
  ) filelines;
  (*
  Hashtbl.iter (
    fun k v ->
      print_endline ("@ "^dec_hex k^" "^v)
  ) sym_addr2label;
  *)
  sym_addr2label

let is_call (i : instr) : bool =
  let op = get_op i in
  match op with
  | Intel_OP io ->
    begin
      match io with
      | Intel_ControlOP ico ->
        begin
          match ico with
          | CALL -> true
          | _ -> false
        end
      | _ -> false
    end
  | Arm_OP (ao, _, _) ->
    begin
      match ao with
      | Arm_ControlOP _ -> true
      | _ -> false
    end
  | _ -> false

let update_loc (i : instr) (new_loc : loc) : instr =
  match i with
  | SingleInstr (op, loc, prefix_op, tag_op) -> SingleInstr (op, new_loc, prefix_op, tag_op)
  | DoubleInstr (op, exp, loc, prefix_op, tag_op) -> DoubleInstr (op, exp, new_loc, prefix_op, tag_op)
  | TripleInstr (op, exp1, exp2, loc, prefix_op, tag_op) -> TripleInstr (op, exp1, exp2, new_loc, prefix_op, tag_op)
  | FourInstr (op, exp1, exp2, exp3, loc, prefix_op, tag_op) -> FourInstr (op, exp1, exp2, exp3, new_loc, prefix_op, tag_op)
  | FifInstr (op, exp1, exp2, exp3, exp4, loc, prefix_op, tag_op) -> FifInstr (op, exp1, exp2, exp3, exp4, new_loc, prefix_op, tag_op)

let apply (il : instr list) : instr list =
  let sym_addr2label = parse () in
  let rec add_existing_symbols il acc =
    match il with
    | [] -> List.rev acc
    | i::rest ->
      let i' =
        let i_loc = get_loc i in
        let i_addr = i_loc.loc_addr in
        let i_label = i_loc.loc_label in
        match Hashtbl.find_opt sym_addr2label i_addr with
        | Some sym_name -> update_loc i {i_loc with loc_label = sym_name^":\n"^i_label}
        | None -> i
      in
      match i' with
      | DoubleInstr (Intel_OP (Intel_ControlOP CALL), Label func_name, loc, prefix_op, tag_op) ->
        begin
          let func_name' = strip_symbol_prefix func_name |> int_of_string in
          match Hashtbl.find_opt sym_addr2label func_name' with
          | Some sym_name ->
            let new_acc = DoubleInstr (Intel_OP (Intel_ControlOP CALL), Label sym_name, loc, prefix_op, tag_op) :: acc in
            add_existing_symbols rest new_acc
          | None -> add_existing_symbols rest (i' :: acc)
        end
      | _ -> add_existing_symbols rest (i' :: acc)
  in
  add_existing_symbols il []
