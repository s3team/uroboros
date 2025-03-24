open Semantic_analysis

module ArmGotAbs : DfaAbs = struct
  open Ail_utils
  open Arm_reassemble_symbol_get
  open Arm_utils
  open Batteries
  open Pp_print
  open Type

  let initial = ExpSet.empty
  let equal = ExpSet.equal
  let union = ExpSet.union
  let result = Hashtbl.create 100
  let (initialized : bool ref) = ref false
  let registers : (string, int) Hashtbl.t = Hashtbl.create 30
  let literal_pools : (int, string) Hashtbl.t = Hashtbl.create 200
  let (got_addr : int ref) = ref 0

  let init_literal_pools () =
    let filelines = File.lines_of "text_section_as_data.txt"
    and add_lines_to_map l =
      let items = Str.split (Str.regexp ":") l in
      let addr = int_of_string ("0x" ^ List.nth items 0) in
      let data = String.trim (List.nth items 1) in
      (* insert addr * data to text_as_data_set *)
      Hashtbl.replace literal_pools addr data
    in
    Enum.iter add_lines_to_map filelines;
    ()

  let init_got_addr () =
    let filelines = File.lines_of "sections.info"
    and parse_section_info l =
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x" ^ List.nth items 1)
      and size = int_of_string ("0x" ^ List.nth items 3)
      and secname = List.nth items 0 in
      if contains secname ".got" then got_addr := addr else ()
    in
    Enum.iter parse_section_info filelines

  (** Initialize ArmGotAbs struct *)
  let init () =
    if not !initialized then begin
      initialized := true;
      Hashtbl.clear result;
      init_literal_pools ();
      init_got_addr ()
    end
    else ()

  let update_registers (reg_name : string) (value : int) =
    Hashtbl.replace registers reg_name value

  let has_register_value (reg_name : string) : bool =
    try
      let _ = Hashtbl.find registers reg_name in
      true
    with Not_found -> false

  let get_register_value (reg_name : string) : int option =
    try
      let value = Hashtbl.find registers reg_name in
      Some value
    with Not_found -> None

  let has_literal_pool_data (addr : int) : bool =
    Hashtbl.mem literal_pools addr = true

  let get_literal_pool_data (addr : int) : string option =
    try
      let value = Hashtbl.find literal_pools addr in
      Some value
    with Not_found -> None

  let remove_leading_zeros s =
    let rec aux s =
      if String.length s > 0 && String.get s 0 = '0' then
        aux (String.sub s 1 (String.length s - 1))
      else s
    in
    aux s

  (** Return the union of all values in [input_map] for the keys in [preds]. *)
  let merge (preds : instr option list)
      (input_map : (instr, ExpSet.t) Hashtbl.t) : ExpSet.t =
    let union_preds =
     fun acc (p_op : instr option) ->
      match p_op with
      | Some p ->
          let out = Hashtbl.find input_map p in
          union acc out
      | None -> acc
    in
    List.fold_left union_preds ExpSet.empty preds

  let remove_exps exps set =
    List.fold_left (fun acc reg_exp -> ExpSet.remove reg_exp acc) set exps

  let flow_through (i : instr) (ins : ExpSet.t) : ExpSet.t =
    init ();
    let module AU = ArmUtils in
    let outs = ins in
    let arm_parser = new Arm_parser.arm_parse in
    match i with
    | DoubleInstr (p, e, loc, _, _) -> begin
        match (p, e) with
        | Arm_OP (Arm_StackOP POP, _), Label label ->
            (* pop {r4,r7,pc}
             * Currently, we handle {...} exp as a Label.
             * See [Arm_parser.exp_symb] for details. *)
            let parse_label_to_regs l =
              let l = String.trim (String.sub l 1 (String.length l - 2)) in
              let items = Str.split (Str.regexp ",") l in
              List.map String.trim items
            in
            let regs = parse_label_to_regs label in
            let reg_exps = List.map arm_parser#exp_symb regs in
            let remove_exps exps set =
              List.fold_left
                (fun acc reg_exp -> ExpSet.remove reg_exp acc)
                set exps
            in
            remove_exps reg_exps outs
        | _ -> outs
      end
    | TripleInstr (p, e1, e2, loc, prefix, _) -> begin
        match (p, e1, e2) with
        | ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _),
            Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg _), imm)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            let pc_relative_addr = AU.get_pc_relative_addr "thumb" i in
            if has_literal_pool_data pc_relative_addr then begin
              (* update the register value *)
              let (Some data_str) = get_literal_pool_data pc_relative_addr in
              let data = int_of_string ("0x" ^ data_str) in
              let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
              let _ = update_registers dst_reg_name data in
              (* check if the value is got address.
               * haven't seen this case where we can get the got address
               * with just one pc-relative ldr instruction *)
              if data = !got_addr then
                (* gen *)
                let _ = ExpSet.remove e2 outs in
                ExpSet.add e2 outs
              else outs
            end
            else
              let _ =
                Printf.printf "This case should not happen: 0x%x\n"
                  pc_relative_addr
              in
              failwith "check literal pools."
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _),
            Ptr (BinOP_PLUS (Arm_Reg (Arm_CommonReg src), imm)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            (* a case that a common reg points to a literal pool *)
            let src_reg_name = p_arm_reg (Arm_CommonReg src) in
            match get_register_value src_reg_name with
            | Some src_reg_value ->
                if has_literal_pool_data src_reg_value then
                  (* update the register value *)
                  let (Some data_str) = get_literal_pool_data src_reg_value in
                  let data = int_of_string ("0x" ^ data_str) in
                  let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
                  let _ = update_registers dst_reg_name data in
                  if data = !got_addr then
                    (* gen *)
                    let _ = ExpSet.remove e2 outs in
                    ExpSet.add e2 outs
                  else
                    (* kill *)
                    ExpSet.remove e2 outs
                else
                  (* src_reg_value points to another section. *)
                  outs
            | None ->
                (* cannot get the value of the src register *)
                outs
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _),
            _,
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            ExpSet.remove e2 outs
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), _),
            Reg (Arm_Reg (Arm_PCReg _)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
            match get_register_value dst_reg_name with
            | Some dst_reg_value ->
                let help addr =
                  let module AR = Arm_reassemble_symbol_get in
                  let arm_reassemble = new AR.arm_reassemble in
                  let sec = arm_reassemble#check_sec addr in
                  match sec with
                  | Some s ->
                      if s.sec_name = ".rodata" then
                        let tag = Some (Sym addr) in
                        let ldr_op =
                          Arm_OP (Arm_CommonOP (Arm_Assign LDR), None)
                        in
                        let new_instr =
                          TripleInstr
                            (ldr_op, Const (Point addr), e2, loc, prefix, None)
                        in
                        Hashtbl.replace result loc.loc_addr new_instr
                  | None -> ()
                in
                (* update the register value *)
                let pc = loc.loc_addr + 4 in
                let sum = pc + dst_reg_value in
                let mod_sum = sum - (sum mod 4) in
                let _ = update_registers dst_reg_name mod_sum in
                let _ = help mod_sum in
                if mod_sum = !got_addr then
                  (* gen *)
                  let _ = ExpSet.remove e2 outs in
                  ExpSet.add e2 outs
                else
                  (* kill *)
                  ExpSet.remove e2 outs
            | None -> outs
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign mov_op), _),
            Reg (Arm_Reg (Arm_CommonReg src)),
            Reg (Arm_Reg (Arm_CommonReg dst)) )
          when mov_op = MOV || mov_op = MOVS -> begin
            let src_reg_name = p_arm_reg (Arm_CommonReg src) in
            let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
            match get_register_value src_reg_name with
            | Some src_reg_value ->
                (* update the register value *)
                let _ = update_registers dst_reg_name src_reg_value in
                if src_reg_value = !got_addr then
                  (* gen dst *)
                  let _ = ExpSet.remove e2 outs in
                  ExpSet.add e2 outs
                else
                  (* kill dst *)
                  ExpSet.remove e2 outs
            | None ->
                (* kill dst *)
                ExpSet.remove e2 outs
          end
        | _ -> outs
      end
    | FourInstr (p, e1, e2, e3, loc, _, _) -> begin
        match (p, e1, e2, e3) with
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), _),
            Reg (Arm_Reg (Arm_CommonReg src1)),
            Reg (Arm_Reg (Arm_CommonReg src2)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            let src1_reg_name = p_arm_reg (Arm_CommonReg src1) in
            let src2_reg_name = p_arm_reg (Arm_CommonReg src2) in
            if
              has_register_value src1_reg_name
              && has_register_value src2_reg_name
            then
              let (Some src1_reg_value) = get_register_value src1_reg_name in
              let (Some src2_reg_value) = get_register_value src2_reg_name in
              (* update the register value *)
              let sum = src1_reg_value + src2_reg_value in
              let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
              let _ = update_registers dst_reg_name sum in
              (* check if the value is the got address *)
              if sum = !got_addr then
                (* gen *)
                let _ = ExpSet.remove e3 outs in
                ExpSet.add e3 outs
              else
                (* kill *)
                ExpSet.remove e3 outs
            else
              (* cannot get the values of the registers *)
              outs
          end
        | _ -> outs
      end
    (* push (kill) -> but only happens at a function entry and exit *)
    | _ -> outs

  let process_instr (i : instr) (ins : ExpSet.t) : unit =
    let has_got_addr (reg_name : string) =
      match get_register_value reg_name with
      | Some v -> v = !got_addr
      | None -> false
    in
    match i with
    | TripleInstr (p, e1, e2, loc, prefix, _) -> begin
        match (p, e1, e2) with
        | ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _),
            Ptr (BinOP_PLUS_R (Arm_CommonReg src1, Arm_CommonReg src2)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            let src1_name = p_arm_reg (Arm_CommonReg src1) in
            let src2_name = p_arm_reg (Arm_CommonReg src2) in
            if has_got_addr src1_name || has_got_addr src2_name then begin
              let sym_addr =
                if has_got_addr src1_name then
                  let (Some v) = get_register_value src2_name in
                  v + !got_addr
                else
                  let (Some v) = get_register_value src1_name in
                  v + !got_addr
              in
              let sym_tag = Some (Sym sym_addr) in
              (* If new_instr needs to be not only symbolized but dereferenced,
               * dereference it in arm_reassemble_symbol_get.ml#v_exp2 *)
              let new_instr = TripleInstr (p, e1, e2, loc, prefix, sym_tag) in
              Hashtbl.replace result (get_loc i).loc_addr new_instr;
              ()
            end
            else ()
          end
        | _ -> ()
      end
    | _ -> ()
end
