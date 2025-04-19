open Type

let flip f x y = f y x

let rec last_ele = (function
| [h] -> h
| h::t -> last_ele t
| [] -> failwith "error in last_ele")

let p_op p = match p with
  | Intel_OP ip ->
    show_intel_op ip
  | Arm_OP (ap, suffix) ->
    (* concatenate ap and suffix *)
    let ap_str =
      show_arm_op ap
    in
    match suffix with
    | None -> ap_str
    | Some (Arm_Opqualifier opqualifier) ->
      let opqualifier_str =
        show_arm_opqualifier opqualifier
        |> Str.split (Str.regexp " +")
        |> last_ele
        |> String.lowercase_ascii
      in
        (ap_str^"."^opqualifier_str)
    | Some (Arm_Condsuff condsuff) ->
      let condsuff_str =
        show_arm_condsuff condsuff
        |> Str.split (Str.regexp " +")
        |> last_ele
        |> String.lowercase_ascii
      in
        (ap_str^"."^condsuff_str)

let p_reg p =
  let p_intel_reg' p =
    show_intel_reg p
  in
  let p_arm_reg' p =
    show_arm_reg p
  in
  match p with
  | Intel_Reg r -> "%"^(p_intel_reg' r)
  | Arm_Reg r -> ""^(p_arm_reg' r)

let p_intel_reg p =
  let intel_reg_to_string p =
    show_intel_reg p
    |> Str.split (Str.regexp " +")
    |> last_ele
    |> String.lowercase_ascii
  in
  "%"^(intel_reg_to_string p)

let p_arm_reg p =
  let arm_reg_to_string p =
    show_arm_reg p
    |> Str.split (Str.regexp " +")
    |> last_ele
    |> String.lowercase_ascii
  in
  ""^(arm_reg_to_string p)

let p_intel_reg p =
  let intel_reg_to_string p =
    show_intel_reg p
    |> Str.split (Str.regexp " +")
    |> last_ele
    |> String.lowercase_ascii
  in
  "%"^(intel_reg_to_string p)

let p_arm_reg p =
  let arm_reg_to_string p =
    show_arm_reg p
    |> Str.split (Str.regexp " +")
    |> last_ele
    |> String.lowercase_ascii
  in
  ""^(arm_reg_to_string p)

let p_seg' p =
    show_intel_seg p

let p_seg s =
  let s = p_seg' s in
    "%"^s

and p_typ p =
    show_intel_ptrtyp p

and p_assist p =
    show_assistop p

and p_mathop p =
  let help s = if s = "mathadd" then "+" else "-" in
    show_intel_mathop p
    |> help

and p_loc l =
  let dec_hex_integer s =
    "0x"^(Printf.sprintf "%X" s) in
    let v = dec_hex_integer l in
	String.lowercase_ascii v

and p_fun (f: func) =
    f.func_name

and p_sec s =
    if s.sec_name = ".rodata" then
       "ds"
    else if s.sec_name = ".data" then
       "ds"
    else if s.sec_name = ".bss" then
       "ds"
    else failwith "p sec"

and p_hex s =
    "0x"^(Printf.sprintf "%x" s)

let p_ptraddr = function
  | UnOP r ->
    begin
      match r with
      | Intel_Reg r -> "("^(p_intel_reg r)^")"
      | Arm_Reg r -> "["^(p_arm_reg r)^"]"
    end
  | UnOP_WB r -> "["^(p_arm_reg r)^"]!"
  | BinOP_PLUS (r, i) ->
    begin
      match r with
      | Intel_Reg r -> (p_loc i)^"("^(p_intel_reg r)^")"
      | Arm_Reg r -> "["^(p_arm_reg r)^", #"^(p_hex i)^"]"
    end
  | BinOP_PLUS_S (r, s) -> s^"("^(p_reg r)^")"
  | BinOP_PLUS_WB (r, i) -> "["^(p_arm_reg r)^", #"^(p_hex i)^"]!"
  | BinOP_MINUS (r, i) ->
    begin
      match r with
      | Intel_Reg r -> "-"^(p_loc i)^"("^(p_intel_reg r)^")"
      | Arm_Reg r -> "["^(p_arm_reg r)^", #-"^(p_hex i)^"]"
    end
  | BinOP_MINUS_S (r, s) -> "-"^s^"("^(p_reg r)^")"
  | BinOP_MINUS_WB (r, i) -> "["^(p_arm_reg r)^", #-"^(p_hex i)^"]!"
  | ThreeOP (r1, r2, i) -> "("^(p_reg r1)^","^(p_reg r2)^","^(p_loc i)^")"
  | FourOP_PLUS (r1,r2,i1,i2) -> (p_loc i2)^"("^(p_reg r1)^","^(p_reg r2)^","^(p_loc i1)^")"
  | FourOP_MINUS (r1,r2,i1,i2) -> "-"^(p_loc i2)^"("^(p_reg r1)^","^(p_reg r2)^","^(string_of_int i1)^")"
  | FourOP_PLUS_S (r1,r2,i1,s1) -> s1^"("^(p_reg r1)^","^(p_reg r2)^","^(p_loc i1)^")"
  | FourOP_MINUS_S (r1,r2,i1,s1) -> "-"^s1^"("^(p_reg r1)^","^(p_reg r2)^","^(string_of_int i1)^")"
  | JmpTable_PLUS (addr, r, offset) -> (p_loc addr)^"(,"^(p_reg r)^","^(p_loc offset)^")"
  | JmpTable_MINUS (addr, r, offset) -> "-"^(p_loc addr)^"(,"^(p_reg r)^","^(p_loc offset)^")"
  | JmpTable_PLUS_S (s, r, offset) -> s^"(,"^(p_reg r)^","^(p_loc offset)^")"
  | JmpTable_MINUS_S (s, r, offset) -> "-"^s^"(,"^(p_reg r)^","^(p_loc offset)^")"
  | SegRef (s, r) -> (p_seg s)^":"^(p_reg r)

let p_const l =
  let d2h s =
    "$0x"^(Printf.sprintf "%X" s)
  and d2h_p s =
    "0x"^(Printf.sprintf "%X" s)
  and d2h_lower s =
    "#0x"^(Printf.sprintf "%x" s)
  in
  match l with
  | Normal s -> d2h s
  | Point s -> d2h_p s
  | Immediate s -> d2h_lower s

let rec p_exp e =
  let p_symbol = function
  | CallDes f -> p_fun f
  | JumpDes (des) -> p_loc des
  | StarDes e -> "*"^(p_exp e) in
  match e with
  | Const s -> p_const s
  | Symbol s -> p_symbol s
  | Reg s -> p_reg s
  | Assist s -> p_assist s
  | Ptr addr -> p_ptraddr addr
  | Label s -> s

let dec_hex (s:int) : string =
  "0x"^(Printf.sprintf "%X" s)

let p_single p =
  let p_str = p_op p in
    p_str

and p_double p e =
  let p_str = p_op p
  and e_str = p_exp e in
    (p_str^" "^e_str)

and p_triple p e1 e2 =
  let assist_list = ["pop"] in
  let check_assist exp =
    List.mem exp assist_list in
  let p_str = p_op p
  and e1_str = p_exp e1
  and e2_str = p_exp e2 in
    if check_assist e2_str then
    (p_str^" "^e2_str^" "^e1_str)
  else
    (p_str^" "^e2_str^","^e1_str)

and p_four p e1 e2 e3 =
  let assist_list = ["cmpsb"; "scas"; "stos"; "movsl"; "movsb"; "cmpsw";
                    "movsq"] in
  let check_assist exp =
    List.mem exp assist_list in
  let p_str = p_op p
  and e1_str = p_exp e1
  and e2_str = p_exp e2
  and e3_str = p_exp e3 in
    if check_assist e3_str then
    (p_str^" "^e3_str^" "^e2_str^","^e1_str)
  else
    (p_str^" "^e3_str^","^e2_str^","^e1_str)

and p_five p e1 e2 e3 e4 =
    let p_str = p_op p
    and e1_str = p_exp e1
    and e2_str = p_exp e2
    and e3_str = p_exp e3
    and e4_str = p_exp e4 in
    (p_str^" "^e4_str^","^e3_str^","^e2_str^","^e1_str)


and p_location l =
  l.loc_label

and p_addr l =
"0x"^(Printf.sprintf "%X: " l.loc_addr)

and p_prefix pre =
  match pre with
  | Some LOCK -> " lock "
  | Some ADDR32 -> " addr32 "
  | Some BND -> " bnd "
  | None -> ""

let get_loc i =
  match i with
  | SingleInstr (_, l, _) -> l
  | DoubleInstr (_, _, l, _) -> l
  | TripleInstr (_, _, _, l, _) -> l
  | FourInstr (_, _, _, _, l, _) -> l
  | FifInstr (_, _, _, _, _, l, _) -> l

let pp_print_instr i =
  match get_loc i with
  (* | {loc_dummy = true} -> ((get_loc i |> p_location)^("nop"))*)
  | {loc_visible = false; _} -> (get_loc i |> p_location)
  | _ ->
     begin
       match i with
       | SingleInstr (p, l, pre) -> ((p_location l)^(p_prefix pre)^(p_single p))
       | DoubleInstr (p, exp1, l, pre) -> ((p_location l)^(p_prefix pre)^(p_double p exp1))
       | TripleInstr (p, exp1, exp2, l, pre) -> ((p_location l)^(p_prefix pre)^(p_triple p exp1 exp2))
       | FourInstr (p, exp1, exp2, exp3, l, pre) -> ((p_location l)^(p_prefix pre)^(p_four p exp1 exp2 exp3))
       | FifInstr (p, exp1, exp2, exp3, exp4, l, pre) -> ((p_location l)^(p_prefix pre)^(p_five p exp1 exp2 exp3 exp4))
     end

let pp_print_instr' i =
  match get_loc i with
  (* | {loc_dummy = true} -> ((get_loc i |> p_location)^("nop"))*)
  | {loc_visible = false; _} -> (get_loc i |> p_location)
  | _ ->
     begin
       match i with
       | SingleInstr (p, l, pre) -> ((p_addr l)^(p_location l)^(p_prefix pre)^(p_single p))
       | DoubleInstr (p, exp1, l, pre) -> ((p_addr l)^(p_location l)^(p_prefix pre)^(p_double p exp1))
       | TripleInstr (p, exp1, exp2, l, pre) -> ((p_addr l)^(p_location l)^(p_prefix pre)^(p_triple p exp1 exp2))
       | FourInstr (p, exp1, exp2, exp3, l, pre) -> ((p_addr l)^(p_location l)^(p_prefix pre)^(p_four p exp1 exp2 exp3))
       | FifInstr (p, exp1, exp2, exp3, exp4, l, pre) -> ((p_addr l)^(p_location l)^(p_prefix pre)^(p_five p exp1 exp2 exp3 exp4))
     end



let pp_print_list instr_list =
  let rec help acc l =
    match l with
	| h::t ->
		let s = pp_print_instr h in
		  help (s::acc) t
	| []   -> List.rev acc in
    help [] instr_list


let pp_print_file instr_list =
    let oc = open_out_gen [Open_append; Open_creat] 0o666 "final.s" in
      Printf.fprintf oc ".section .text\n";
      (* List.iter (fun l -> Printf.fprintf oc "%s\n" l) instr_list; *)
      List.iter (fun l -> output_string oc l; output_char oc '\n') instr_list;
      close_out oc
