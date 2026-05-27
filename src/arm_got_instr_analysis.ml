open Semantic_analysis

let arch = ref "arm"
let init_arch (_arch : string) = arch := _arch

(* Module for register value tracking *)
(* New DFA signature for tracking register values, not just names *)
(* Value-based abstract state for ARM GOT analysis *)
module ArmGotAbsWithValues : DfaAbs = struct
  open Ail_utils
  open Utils
  open Arm_parser
  open Arm_reassemble_symbol_get
  open Arm_utils
  open Pp_print
  open Type
  module RegMap = Map.Make (String)

  (* A tracked register is in one of three states:
    - absent in the map: not yet computed (bottom)
    - [Val (v, src)]: known to hold the concrete value [v]; [src] is the
      loc_addr of the LDR that loaded the value (when it came from a
      PC-relative literal-pool load), else [None]
    - [Top]: oscillating / killed / unknown *)
  type reg_val = Top | Val of int * int option
  type abs_state = reg_val RegMap.t
  type abstraction = abs_state

  let initial = RegMap.empty
  let equal = RegMap.equal ( = )
  let result = Hashtbl.create 100
  let (initialized : bool ref) = ref false
  let literal_pools : (int, string) Hashtbl.t = Hashtbl.create 200
  let data_pools : (int, string) Hashtbl.t = Hashtbl.create 200
  let (got_addr : int ref) = ref 0
  let arm_reassemble_lazy = lazy (new arm_reassemble)

  let init_literal_pools () =
    let filelines = read_file "text_section_as_data.txt"
    and add_lines_to_map l =
      let items = Str.split (Str.regexp ":") l in
      let addr = int_of_string ("0x" ^ List.nth items 0) in
      let data = String.trim (List.nth items 1) in
      (* insert (addr * data) to [literal_pools] *)
      Hashtbl.replace literal_pools addr data
    in
    List.iter add_lines_to_map filelines;
    ()

  let init_data_pools () =
    let filelines = read_file "data_section_as_data.txt"
    and add_lines_to_map l =
      let items = Str.split (Str.regexp ":") l in
      let addr = int_of_string ("0x" ^ List.nth items 0) in
      let data = String.trim (List.nth items 1) in
      (* insert (addr * data) to [data_pools] *)
      Hashtbl.replace data_pools addr data
    in
    List.iter add_lines_to_map filelines;
    ()

  let init_got_addr () =
    let filelines = read_file "sections.info"
    and parse_section_info l =
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x" ^ List.nth items 1)
      and size = int_of_string ("0x" ^ List.nth items 3)
      and secname = List.nth items 0 in
      if contains ~str:secname ~sub:".got" then got_addr := addr else ()
    in
    List.iter parse_section_info filelines

  (** Initialize ArmGotAbs struct *)
  let init () =
    let module EU = Elf_utils in
    if not !initialized then begin
      initialized := true;
      Hashtbl.clear result;
      if EU.elf_32 () then begin
        init_literal_pools ();
        init_data_pools ()
      end;
      init_got_addr ()
    end
    else ()

  let get_register_value (state : abs_state) (reg_name : string) : int option =
    match RegMap.find_opt reg_name state with
    | Some (Val (v, _)) -> Some v
    | Some Top | None -> None

  let get_register_source (state : abs_state) (reg_name : string) : int option =
    match RegMap.find_opt reg_name state with
    | Some (Val (_, src)) -> src
    | Some Top | None -> None

  let update_register (state : abs_state) (reg_name : string) (value : int) :
      abs_state =
    RegMap.add reg_name (Val (value, None)) state

  let update_register_with_source (state : abs_state) (reg_name : string)
      (value : int) (src_ldr_loc : int) : abs_state =
    RegMap.add reg_name (Val (value, Some src_ldr_loc)) state

  let remove_register (state : abs_state) (reg_name : string) : abs_state =
    RegMap.add reg_name Top state

  let tracked_reg_name_of_arm_reg (reg : arm_reg) : string option =
    match reg with
    | Arm_CommonReg dst -> Some (p_arm_reg (Arm_CommonReg dst))
    | Arm_StackReg dst -> Some (p_arm_reg (Arm_StackReg dst))
    | Arm_LinkReg dst -> Some (p_arm_reg (Arm_LinkReg dst))
    | _ -> None

  let tracked_reg_kind_of_arm_reg (reg : arm_reg) : string option =
    match reg with
    | Arm_CommonReg _ -> Some "common"
    | Arm_StackReg _ -> Some "stack"
    | Arm_LinkReg _ -> Some "link"
    | _ -> None

  let has_literal_pool_data (addr : int) : bool =
    Hashtbl.mem literal_pools addr = true

  let get_literal_pool_data (addr : int) : string option =
    Hashtbl.find_opt literal_pools addr

  let has_data_pool_data (addr : int) : bool =
    Hashtbl.mem data_pools addr = true

  let get_data_pool_data (addr : int) : string option =
    try
      let value = Hashtbl.find data_pools addr in
      Some value
    with Not_found -> None

  let remove_leading_zeros s =
    let rec aux s =
      if String.length s > 0 && String.get s 0 = '0' then
        aux (String.sub s 1 (String.length s - 1))
      else s
    in
    aux s

  (** Merge predecessors' register-value maps. *)
  let merge (preds : instr option list)
      (input_map : (instr, abs_state) Hashtbl.t) : abs_state =
    let pred_states =
      List.filter_map
        (fun p_op ->
          let* p = p_op in
          Hashtbl.find_opt input_map p)
        preds
    in
    match pred_states with
    | [] -> RegMap.empty
    | states ->
        let all_regs =
          List.fold_left
            (fun acc state ->
              RegMap.fold
                (fun reg _ acc -> if List.mem reg acc then acc else reg :: acc)
                state acc)
            [] states
        in
        List.fold_left
          (fun acc reg ->
            let values =
              List.filter_map (fun state -> RegMap.find_opt reg state) states
            in
            match values with
            | [] -> acc
            | first :: rest ->
                let joined =
                  List.fold_left
                    (fun cur next ->
                      match (cur, next) with
                      | Top, _ | _, Top -> Top
                      | Val (a, sa), Val (b, sb) ->
                          if a <> b then Top
                          else
                            let src = if sa = sb then sa else None in
                            Val (a, src))
                    first rest
                in
                RegMap.add reg joined acc)
          RegMap.empty all_regs

  let check_and_replace_instr (addr : int) (dst_e : exp) (loc : loc)
      (prefix : prefix option) =
    let arm_reassemble = Lazy.force arm_reassemble_lazy in
    (* Strip the LSB from Thumb function pointers.
    See arm_postprocess.ml#adjust_thumb_function_pointer *)
    let target_addr = if addr mod 2 = 1 then addr - 1 else addr in
    let sec = arm_reassemble#check_sec target_addr in
    match sec with
    | Some _s ->
        let ldr_op = Arm_OP (Arm_CommonOP (Arm_Assign LDR), None, None) in
        let new_instr =
          TripleInstr
            ( ldr_op,
              Const (Point target_addr),
              dst_e,
              loc,
              prefix,
              Hashtbl.create 0 )
        in
        Hashtbl.replace result loc.loc_addr [ new_instr ]
    | None ->
        if arm_reassemble#check_text target_addr then
          let ldr_op = Arm_OP (Arm_CommonOP (Arm_Assign LDR), None, None) in
          let new_instr =
            TripleInstr
              ( ldr_op,
                Const (Point target_addr),
                dst_e,
                loc,
                prefix,
                Hashtbl.create 0 )
          in
          Hashtbl.replace result loc.loc_addr [ new_instr ]
        else ()

  (** Transfer function: compute output state from input state *)
  let flow_through (i : instr) (ins : abs_state) : abs_state =
    init ();
    let module AU = ArmUtils in
    let outs = ins in
    match i with
    | DoubleInstr (p, e, loc, _, _) ->
        begin match (p, e) with
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
        | Arm_OP (Arm_ControlOP (BL | BLX), _, _), _ ->
            (* AAPCS: kill caller-saved registers across a call so that stale
               values (e.g. a .rodata string pointer prepared in r0 before a
               gettext call) don't get propagated through subsequent
               mov r4, r0 instructions and end up impersonating the GOT
               anchor at later GOT loads. *)
            let caller_saved = [ "r0"; "r1"; "r2"; "r3"; "r12"; "ip"; "lr" ] in
            List.fold_left
              (fun state reg -> RegMap.remove reg state)
              outs caller_saved
        | _ -> outs
        end
    | TripleInstr (p, e1, e2, loc, prefix, _) ->
        begin match (p, e1, e2) with
        | ( Arm_OP (Arm_CommonOP (Arm_Assign (LDR | LDRB)), _, _),
            Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg _), imm)),
            Reg (Arm_Reg dst) ) ->
            begin match tracked_reg_name_of_arm_reg dst with
            | Some dst_reg_name ->
                (* No need to pass [imm] to [AU.get_pc_relative_addr]. *)
                let pc_relative_addr = AU.get_pc_relative_addr !arch i in
                if has_literal_pool_data pc_relative_addr then begin
                  (* update the register value in the output state, and
                  record THIS LDR's loc as the source so a later
                  [add Rd, pc] can flag this LDR as a GOT-pool load. *)
                  let (Some data_str) =
                    get_literal_pool_data pc_relative_addr
                  in
                  let data = int_of_string ("0x" ^ data_str) in
                  update_register_with_source outs dst_reg_name data
                    loc.loc_addr
                end
                else
                  (* pc-relative address not in literal pool (e.g. points to
                     .data section); kill the destination register *)
                  remove_register outs dst_reg_name
            | None -> outs
            end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign (LDR | LDRB)), _, _),
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
        | ( Arm_OP (Arm_CommonOP (Arm_Assign (LDR | LDRB)), _, _),
            Ptr (BinOP_PLUS_R (Arm_PCReg _, Arm_CommonReg src)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            (* ldr r0, [pc, r3] *)
            let src_reg_name = p_arm_reg (Arm_CommonReg src) in
            match get_register_value ins src_reg_name with
            | Some src_reg_value -> begin
                let pc_relative_addr =
                  AU.get_pc_relative_addr_with_offset !arch i src_reg_value
                in
                if has_data_pool_data pc_relative_addr then begin
                  let (Some data_str) = get_data_pool_data pc_relative_addr in
                  let data = int_of_string ("0x" ^ data_str) in
                  let _ = check_and_replace_instr data e2 loc prefix in
                  let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
                  update_register outs dst_reg_name data
                end
                else
                  (* pc-relative address not in data pool; kill dst *)
                  remove_register outs (p_arm_reg (Arm_CommonReg dst))
              end
            | None ->
                (* cannot get the value of the src register, kill dst *)
                remove_register outs (p_arm_reg (Arm_CommonReg dst))
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign (LDR | LDRB)), _, _),
            _,
            Reg (Arm_Reg dst) ) ->
            begin match tracked_reg_name_of_arm_reg dst with
            | Some dst_reg_name -> remove_register outs dst_reg_name
            | None -> outs
            end
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), _, _),
            Reg (Arm_Reg (Arm_PCReg _)),
            Reg (Arm_Reg dst) ) ->
            (* ADD <tracked>, pc - handle common/stack/link dst registers uniformly *)
            begin match tracked_reg_name_of_arm_reg dst with
            | Some dst_reg_name ->
                begin match get_register_value ins dst_reg_name with
                | Some dst_reg_value ->
                    let pc = loc.loc_addr + 4 in
                    let dst_reg_value' = AU.to_signed_int dst_reg_value in
                    let sum = pc + dst_reg_value' in
                    let aligned_sum =
                      if sum mod 2 = 1 then sum else sum - (sum mod 4)
                    in
                    let _ = check_and_replace_instr aligned_sum e2 loc prefix in
                    (* Flag the source LDR (the PC-relative load that
                    produced [dst_reg_value]) as a GOT-pool load, so the
                    LDR-fold classifier in vinst2 treats its pool value
                    as opaque rather than as an absolute symbol address. *)
                    (match get_register_source ins dst_reg_name with
                    | Some src_ldr_loc ->
                        Hashtbl.replace got_pool_load_ldrs src_ldr_loc ()
                    | None -> ());
                    update_register outs dst_reg_name aligned_sum
                | None -> remove_register outs dst_reg_name
                end
            | None -> outs
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
        | ( Arm_OP (Arm_CommonOP (Arm_Assign MOVW), None, _),
            Const (Immediate imm),
            Reg (Arm_Reg dst) ) ->
            begin match tracked_reg_name_of_arm_reg dst with
            | Some dst_reg_name ->
                update_register outs dst_reg_name (imm land 0xffff)
            | None -> outs
            end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign MOVT), None, _),
            Const (Immediate imm),
            Reg (Arm_Reg dst) ) ->
            begin match tracked_reg_name_of_arm_reg dst with
            | Some dst_reg_name ->
                begin match get_register_value ins dst_reg_name with
                | Some low ->
                    let combined =
                      ((imm land 0xffff) lsl 16) lor (low land 0xffff)
                    in
                    update_register outs dst_reg_name combined
                | None -> remove_register outs dst_reg_name
                end
            | None -> outs
            end
        | ( Arm_OP (Arm_CommonOP (Arm_Assign (MOV | MOVS)), None, _),
            Const (Immediate _),
            Reg (Arm_Reg dst) ) ->
            begin match tracked_reg_name_of_arm_reg dst with
            | Some dst_reg_name -> remove_register outs dst_reg_name
            | None -> outs
            end
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm _ | Arm_Logic _ | Arm_Rol _), _, _),
            _,
            Reg (Arm_Reg dst) ) ->
            (* Arithmetic/logical/shift ops writing to a tracked reg: kill the dest *)
            begin match tracked_reg_name_of_arm_reg dst with
            | Some dst_reg_name -> remove_register outs dst_reg_name
            | None -> outs
            end
        | _ -> outs
        end
    | FourInstr (p, e1, e2, e3, loc, prefix, _) ->
        begin match (p, e1, e2, e3) with
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
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), _, _),
            Reg (Arm_Reg src),
            Reg (Arm_Reg (Arm_PCReg _)),
            Reg (Arm_Reg dst) ) ->
            (* add <tracked>, pc, <tracked> where src/dst are same reg kind *)
            begin match
              ( tracked_reg_name_of_arm_reg src,
                tracked_reg_name_of_arm_reg dst,
                tracked_reg_kind_of_arm_reg src,
                tracked_reg_kind_of_arm_reg dst )
            with
            | Some src_reg_name, Some dst_reg_name, Some src_kind, Some dst_kind
              when src_kind = dst_kind ->
                begin match get_register_value ins src_reg_name with
                | Some src_reg_value ->
                    let pc =
                      if !arch = "arm" then loc.loc_addr + 8
                      else loc.loc_addr + 4
                    in
                    let sum = pc + src_reg_value in
                    let aligned_sum =
                      if sum mod 2 = 1 then sum else sum - (sum mod 4)
                    in
                    let _ = check_and_replace_instr aligned_sum e3 loc prefix in
                    update_register outs dst_reg_name aligned_sum
                | None -> remove_register outs dst_reg_name
                end
            | _ -> outs
            end
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), _, _),
            Const (Immediate imm),
            Reg (Arm_Reg (Arm_LinkReg src)),
            Reg (Arm_Reg (Arm_CommonReg dst)) ) -> begin
            (* add r4, lr, #0x28 *)
            let src_reg_name = p_arm_reg (Arm_LinkReg src) in
            let dst_reg_name = p_arm_reg (Arm_CommonReg dst) in
            match get_register_value ins src_reg_name with
            | Some src_reg_value ->
                let sum = src_reg_value + imm in
                let _ = check_and_replace_instr sum e3 loc prefix in
                update_register outs dst_reg_name sum
            | None -> remove_register outs dst_reg_name
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), _, _),
            Const (Immediate imm),
            Reg (Arm_Reg (Arm_LinkReg src)),
            Reg (Arm_Reg (Arm_LinkReg dst)) ) -> begin
            (* add lr, lr, #0x40 *)
            let src_reg_name = p_arm_reg (Arm_LinkReg src) in
            let dst_reg_name = p_arm_reg (Arm_LinkReg dst) in
            match get_register_value ins src_reg_name with
            | Some src_reg_value ->
                let sum = src_reg_value + imm in
                let _ = check_and_replace_instr sum e3 loc prefix in
                update_register outs dst_reg_name sum
            | None -> remove_register outs dst_reg_name
          end
        | ( Arm_OP (Arm_CommonOP (Arm_Arithm _ | Arm_Logic _ | Arm_Rol _), _, _),
            _,
            _,
            Reg (Arm_Reg dst) ) ->
            (* Arithmetic/logical/shift ops writing to a tracked reg: kill the dest *)
            begin match tracked_reg_name_of_arm_reg dst with
            | Some dst_reg_name -> remove_register outs dst_reg_name
            | None -> outs
            end
        | _ -> outs
        end
    | _ -> outs

  (** Process instruction to detect GOT-based memory accesses and rewrite them
  *)
  let process_instr (i : instr) (ins : abs_state) : unit =
    let module IU = Instr_utils in
    let loc = IU.get_loc i in
    let has_got_addr (reg_name : string) =
      match get_register_value ins reg_name with
      | Some v -> v = !got_addr
      | None -> false
    in
    match i with
    | TripleInstr (p, e1, e2, loc, prefix, tags) ->
        begin match (p, e1, e2) with
        | ( Arm_OP (Arm_CommonOP (Arm_Assign (LDR | LDRB)), _, _),
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
              match src1_value, src2_value with
              | (Some src1_val, Some src2_val) ->
                Printf.printf "src1: %x, src2: %x\n\n" src1_val src2_val
              | (Some src1_val, None) -> Printf.printf "src1: %x\n\n" src1_val
              | (None, Some src2_val) -> Printf.printf "src2: %x\n\n" src2_val
              | _ -> Printf.printf "none\n\n"
            in
            let _ = print_reg_values () in *)
            (* end debug *)
            if has_got_addr src1_name || has_got_addr src2_name then begin
              let sym_addr_opt =
                if has_got_addr src1_name then
                  match get_register_value ins src2_name with
                  | Some v -> Some (v + !got_addr)
                  | None -> None
                else
                  match get_register_value ins src1_name with
                  | Some v -> Some (v + !got_addr)
                  | None -> None
              in
              match sym_addr_opt with
              | None -> ()
              | Some sym_addr ->
                  let sym_instr_tag = InstrTag (Sym sym_addr) in
                  let new_tags = Hashtbl.copy tags in
                  Hashtbl.replace new_tags "instr_tag" sym_instr_tag;
                  let new_instr =
                    TripleInstr (p, e1, e2, loc, prefix, new_tags)
                  in
                  Hashtbl.replace result (IU.get_loc i).loc_addr [ new_instr ]
            end
            else ()
          end
        | _ -> ()
        end
    | _ -> ()

  let print_abstraction abs =
    RegMap.iter
      (fun k v ->
        let s = match v with Top -> "TOP" | Val (n, _) -> dec_hex n in
        print_endline (k ^ " -> " ^ s))
      abs
end
