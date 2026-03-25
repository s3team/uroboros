open Batteries
open Ail_utils
open Pp_print
open Tags_utils
open Type

module ArmUtils = struct
  (** Return an offset in `[pc, #imm]` *)
  let get_offset_in_bracket (s : string) =
    let s = String.trim (String.sub s 1 (String.length s - 2)) in
    let items = Str.split (Str.regexp ",") s in
    let imm_str = List.nth items 1 in
    let offset_str =
      String.trim (String.sub imm_str 1 (String.length imm_str - 1))
    in
    let offset = int_of_string offset_str in
    offset

  let get_pc_value_depending_on_mode (i : instr) (arch : string) : int =
    let loc = get_loc i in
    let instr_addr = loc.loc_addr in
    let instr_tags = get_tags i in
    if arch = "thumb" then instr_addr + 4
    else if arch = "arm" then begin
      let instr_addr = loc.loc_addr in
      let instr_tags = get_tags i in
      let instr_mode = Hashtbl.find_opt instr_tags "mode" in
      match instr_mode with
      | Some (Str "thumb") -> instr_addr + 4
      | Some (Str "arm32") -> instr_addr + 8
      | _ ->
          (* Since we don't handle initialization functions,
           * apply "thumb" mode by default.
           *)
          instr_addr + 4
    end
    else failwith "Unknown mode in get_pc_relative_addr"

  let get_pc_relative_addr (arch : string) (i : instr) : int =
    match i with
    | DoubleInstr (Arm_OP (_, _, _), exp, loc, _, _) -> begin
        match exp with
        | Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg _), offset)) ->
            let instr_addr = loc.loc_addr in
            let pc = get_pc_value_depending_on_mode i arch in
            let aligned_pc = if pc mod 4 = 0 then pc else pc - 2 in
            aligned_pc + offset
        | _ -> failwith "Unhandled expression type in DoubleInstr"
      end
    | TripleInstr
        (Arm_OP (Arm_CommonOP (Arm_Assign _), _, _), exp2, _, loc, _, _) ->
      begin
        match exp2 with
        | Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg _), offset)) ->
            let instr_addr = loc.loc_addr in
            let pc = get_pc_value_depending_on_mode i arch in
            let aligned_pc = if pc mod 4 = 0 then pc else pc - 2 in
            aligned_pc + offset
        | _ -> failwith "Only takes label as operand in Arm_Assign"
      end
    | _ -> failwith "Unhandled instruction type in get_pc_relative_addr"

  let get_pc_relative_addr_with_offset (arch : string) (i : instr)
      (offset : int) : int =
    match i with
    | TripleInstr
        (Arm_OP (Arm_CommonOP (Arm_Assign _), _, _), exp2, _, loc, _, _) ->
      begin
        match exp2 with
        | Ptr (BinOP_PLUS_R (Arm_PCReg _, Arm_CommonReg src)) ->
            (* ldr r0, [pc, r3] *)
            let instr_addr = loc.loc_addr in
            let pc = get_pc_value_depending_on_mode i arch in
            let aligned_pc = if pc mod 4 = 0 then pc else pc - 2 in
            aligned_pc + offset
        | _ -> failwith "Only takes label as operand in Arm_Assign"
      end
    | _ -> failwith "Unhandled instruction type in get_pc_relative_addr"

  let to_signed_int (n : int) : int =
    if n land (1 lsl 31) <> 0 then n - (1 lsl 32) else n
end
