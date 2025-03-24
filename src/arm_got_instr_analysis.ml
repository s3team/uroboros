open Semantic_analysis

(* Value-based abstract state for ARM GOT analysis *)
module ArmGotAbsWithValues : DfaAbsWithValues = struct
  open Ail_utils
  open Arm_parser
  open Arm_reassemble_symbol_get
  open Arm_utils
  open Batteries
  open Pp_print
  open Type

  type abs_state = int RegMap.t (* register name -> value *)

  let initial = RegMap.empty
  let equal = RegMap.equal ( = )
  let result = Hashtbl.create 100
  let (initialized : bool ref) = ref false
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
      if contains ~str:secname ~sub:".got" then got_addr := addr else ()
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

  (** Get register value from the abstract state *)
  let get_register_value (state : abs_state) (reg_name : string) : int option =
    RegMap.find_opt reg_name state

  (** Update register value in the abstract state *)
  let update_register (state : abs_state) (reg_name : string) (value : int) :
      abs_state =
    RegMap.add reg_name value state

  (** Remove register from the abstract state *)
  let remove_register (state : abs_state) (reg_name : string) : abs_state =
    RegMap.remove reg_name state

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

  (** Merge register value maps from predecessors. Keep register-value bindings
      where all predecessors that define the register agree on its value. If a
      predecessor doesn't define a register, it doesn't cause disagreement. Only
      drop a register if different predecessors have different values for it. *)
  let merge (preds : instr option list)
      (input_map : (instr, abs_state) Hashtbl.t) : abs_state =
    (* Get all non-None predecessor states *)
    let pred_states =
      List.filter_map
        (fun p_op ->
          match p_op with
          | Some p -> (
              try Some (Hashtbl.find input_map p) with Not_found -> None)
          | None -> None)
        preds
    in
    match pred_states with
    | [] -> RegMap.empty
    | states ->
        (* Collect all registers that appear in at least one state *)
        let all_regs =
          List.fold_left
            (fun acc state ->
              RegMap.fold
                (fun reg _ acc -> if List.mem reg acc then acc else reg :: acc)
                state acc)
            [] states
        in
        (* For each register, keep it if all states that define it agree on the value *)
        List.fold_left
          (fun acc reg ->
            (* Get all values for this register across all states *)
            let values =
              List.filter_map (fun state -> RegMap.find_opt reg state) states
            in
            match values with
            | [] -> acc (* No state has this register *)
            | v :: rest ->
                (* Keep the register if all states that have it agree on the value *)
                if List.for_all (fun x -> x = v) rest then RegMap.add reg v acc
                else acc (* States disagree, drop the register *))
          RegMap.empty all_regs

  (** Transfer function: compute output state from input state *)
  let flow_through (i : instr) (ins : abs_state) : abs_state =
    init ();
    let module AU = ArmUtils in
    let outs = ins in
    let arm_parser = new Arm_parser.arm_parse in
    match i with
    | DoubleInstr (p, e, loc, _, _, _) -> begin
        match (p, e) with
        | Arm_OP (Arm_StackOP POP, _, _), Label label ->
            (* pop {r4,r7,pc}
             * Currently, we handle {...} exp as a Label.
             * See [Arm_parser.exp_symb] for details.
             * Kill all popped registers from the state. *)
            let parse_label_to_regs l =
              let l = String.trim (String.sub l 1 (String.length l - 2)) in
              let items = Str.split (Str.regexp ",") l in
              List.map String.trim items
            in
            let regs = parse_label_to_regs label in
            List.fold_left (fun state reg -> RegMap.remove reg state) outs regs
        | _ -> outs
      end
    | TripleInstr (p, e1, e2, loc, prefix, _, _) -> begin
        match (p, e1, e2) with
        | ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
            Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg _), imm)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            let pc_relative_addr = AU.get_pc_relative_addr "thumb" i in
            if has_literal_pool_data pc_relative_addr then begin
              (* update the register value in the output state *)
              let (Some data_str) = get_literal_pool_data pc_relative_addr in
              let data = int_of_string ("0x" ^ data_str) in
              let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
              update_register outs dst_reg_name data
            end
            else
              let _ =
                Printf.printf "This case should not happen: %x\n"
                  pc_relative_addr
              in
              let _ = Printf.printf "\t%s\n" (pp_print_instr' i) in
              failwith "No literal pool data found"
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
            Ptr (BinOP_PLUS (Arm_Reg (Arm_CommonReg src), imm)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            (* a case that a common reg points to a literal pool *)
            let src_reg_name = p_arm_reg (Arm_CommonReg src) in
            match get_register_value ins src_reg_name with
            | Some src_reg_value ->
                if has_literal_pool_data src_reg_value then begin
                  (* update the register value in output state *)
                  let (Some data_str) = get_literal_pool_data src_reg_value in
                  let data = int_of_string ("0x" ^ data_str) in
                  let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
                  update_register outs dst_reg_name data
                end
                else
                  (* src_reg_value points to another section, kill dst *)
                  remove_register outs (p_arm_reg (Arm_CommonReg dst))
            | None ->
                (* cannot get the value of the src register, kill dst *)
                remove_register outs (p_arm_reg (Arm_CommonReg dst))
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
            _,
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            (* Generic LDR: kill dst register *)
            remove_register outs (p_arm_reg (Arm_CommonReg dst))
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), _, _),
            Reg (Arm_Reg (Arm_PCReg _)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            (* ADD rX, pc - compute address by adding PC to register value *)
            let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
            match get_register_value ins dst_reg_name with
            | Some dst_reg_value ->
                let check_and_replace_instr addr =
                  let module AR = Arm_reassemble_symbol_get in
                  let arm_reassemble = new AR.arm_reassemble in
                  let sec = arm_reassemble#check_sec addr in
                  match sec with
                  | Some s ->
                      if
                        s.sec_name = ".rodata" || s.sec_name = ".bss"
                        || s.sec_name = ".data.rel.ro"
                        || s.sec_name = ".data"
                      then
                        let tag = Some (Sym addr) in
                        let ldr_op =
                          Arm_OP (Arm_CommonOP (Arm_Assign LDR), None, None)
                        in
                        let new_instr =
                          TripleInstr
                            ( ldr_op,
                              Const (Point addr),
                              e2,
                              loc,
                              prefix,
                              None,
                              Hashtbl.create 0 )
                        in
                        Hashtbl.replace result loc.loc_addr new_instr
                  | None ->
                      if arm_reassemble#check_text addr then
                        let tag = Some (Sym addr) in
                        let ldr_op =
                          Arm_OP (Arm_CommonOP (Arm_Assign LDR), None, None)
                        in
                        let new_instr =
                          TripleInstr
                            ( ldr_op,
                              Const (Point addr),
                              e2,
                              loc,
                              prefix,
                              None,
                              Hashtbl.create 0 )
                        in
                        Hashtbl.replace result loc.loc_addr new_instr
                      else ()
                in
                (* compute the new register value *)
                let pc = loc.loc_addr + 4 in
                let dst_reg_value' = AU.to_signed_int dst_reg_value in
                let sum = pc + dst_reg_value' in
                let aligned_sum =
                  if sum mod 2 = 1 then sum else sum - (sum mod 4)
                in
                let _ = check_and_replace_instr aligned_sum in
                (* update output state with new value *)
                update_register outs dst_reg_name aligned_sum
            | None ->
                (* dst register value unknown, kill it *)
                remove_register outs dst_reg_name
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign mov_op), _, _),
            Reg (Arm_Reg (Arm_CommonReg src)),
            Reg (Arm_Reg (Arm_CommonReg dst)) )
          when mov_op = MOV || mov_op = MOVS -> begin
            (* MOV dst, src - copy register value *)
            let src_reg_name = p_arm_reg (Arm_CommonReg src) in
            let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
            match get_register_value ins src_reg_name with
            | Some src_reg_value ->
                (* copy value from src to dst *)
                update_register outs dst_reg_name src_reg_value
            | None ->
                (* src value unknown, kill dst *)
                remove_register outs dst_reg_name
          end
        | _ -> outs
      end
    | FourInstr (p, e1, e2, e3, loc, _, _, _) -> begin
        match (p, e1, e2, e3) with
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), _, _),
            Reg (Arm_Reg (Arm_CommonReg src1)),
            Reg (Arm_Reg (Arm_CommonReg src2)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            (* ADD dst, src1, src2 - add two registers *)
            let src1_reg_name = p_arm_reg (Arm_CommonReg src1) in
            let src2_reg_name = p_arm_reg (Arm_CommonReg src2) in
            let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
            match
              ( get_register_value ins src1_reg_name,
                get_register_value ins src2_reg_name )
            with
            | Some src1_reg_value, Some src2_reg_value ->
                (* both values available, compute sum *)
                let sum = src1_reg_value + src2_reg_value in
                update_register outs dst_reg_name sum
            | _ ->
                (* at least one value unknown, kill dst *)
                remove_register outs dst_reg_name
          end
        | _ -> outs
      end
    (* push (kill) -> but only happens at a function entry and exit *)
    | _ -> outs

  (** Process instruction to detect GOT-based memory accesses and rewrite them
  *)
  let process_instr (i : instr) (ins : abs_state) : unit =
    let loc = get_loc i in
    let has_got_addr (reg_name : string) =
      match get_register_value ins reg_name with
      | Some v -> v = !got_addr
      | None -> false
    in
    match i with
    | TripleInstr (p, e1, e2, loc, prefix, _, tags) -> begin
        match (p, e1, e2) with
        | ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
            Ptr (BinOP_PLUS_R (Arm_CommonReg src1, Arm_CommonReg src2)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            (* LDR dst, [src1, src2] - check if src1 or src2 contains GOT address *)
            let src1_name = p_arm_reg (Arm_CommonReg src1) in
            let src2_name = p_arm_reg (Arm_CommonReg src2) in

            (* debug *)
            (* let src1_value = get_register_value ins src1_name in
            let src2_value = get_register_value ins src2_name in
            let print_reg_values () =
              let _ = Printf.printf "%s\n" (pp_print_instr' i) in
              let _ = Printf.printf "src1: %s, src2: %s\n" src1_name src2_name in
              match (src1_value, src2_value) with
              | (Some src1_val, Some src2_val) ->
                Printf.printf "src1: %x, src2: %x\n\n" src1_val src2_val
              | (Some src1_val, None) -> Printf.printf "src1: %x\n\n" src1_val
              | (None, Some src2_val) -> Printf.printf "src2: %x\n\n" src2_val
              | _ -> Printf.printf "none\n\n"
            in
            let _ = print_reg_values () in *)
            (* end debug *)
            if has_got_addr src1_name || has_got_addr src2_name then begin
              let sym_addr =
                if has_got_addr src1_name then
                  match get_register_value ins src2_name with
                  | Some v -> v + !got_addr
                  | None ->
                      let _ = Printf.printf "%s\n" (pp_print_instr' i) in
                      failwith "Expected src2 value but got None"
                else
                  match get_register_value ins src1_name with
                  | Some v -> v + !got_addr
                  | None ->
                      let _ = Printf.printf "%s\n" (pp_print_instr' i) in
                      failwith "Expected src1 value but got None"
              in
              let sym_tag = Some (Sym sym_addr) in
              let new_instr =
                TripleInstr (p, e1, e2, loc, prefix, sym_tag, tags)
              in
              Hashtbl.replace result (get_loc i).loc_addr new_instr;
              ()
            end
            else ()
          end
        | _ -> ()
      end
    | _ -> ()
end
