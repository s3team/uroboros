open Batteries
open Ail_utils
open Pp_print
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

  let get_pc_relative_addr (mode : string) (i : instr) : int =
    match i with
    | DoubleInstr (Arm_OP (_, _, _), exp, loc, _, _, _) -> begin
        match exp with
        | Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg _), offset)) ->
            let instr_addr = loc.loc_addr in
            let pc =
              if mode = "thumb" then instr_addr + 4 else instr_addr + 8
            in
            let aligned_pc = if pc mod 4 = 0 then pc else pc - 2 in
            aligned_pc + offset
        | _ -> failwith "Unhandled expression type in DoubleInstr"
      end
    | TripleInstr
        (Arm_OP (Arm_CommonOP (Arm_Assign _), _, _), exp2, _, loc, _, _, _) ->
      begin
        match exp2 with
        | Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg _), offset)) ->
            let instr_addr = loc.loc_addr in
            let pc =
              if mode = "thumb" then instr_addr + 4 else instr_addr + 8
            in
            let aligned_pc = if pc mod 4 = 0 then pc else pc - 2 in
            (* For now, use pc instead of aligned_pc, and just print the debug message *)
            aligned_pc + offset
        | _ -> failwith "Only takes label as operand in Arm_Assign"
      end
    | _ -> failwith "Unhandled instruction type in get_pc_relative_addr"

  let to_signed_int (n : int) : int =
    if n land (1 lsl 31) <> 0 then n - (1 lsl 32) else n
end
