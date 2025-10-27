open Batteries
open Type
open Pp_print
open Common_func_slicer
open Ail_utils

class arm_func_slicer instrs funcs =
  object (self)
    inherit common_func_slicer instrs funcs

    method process2 =
      let has_pc_reg (lab : label) : bool =
        let has_brackets = String.contains lab '{' && String.contains lab '}' in
        let has_pc = String.exists lab "pc" in
        has_brackets && has_pc
      in
      let has_lr_reg (lab : label) : bool =
        let has_brackets = String.contains lab '{' && String.contains lab '}' in
        let has_lr = String.exists lab "lr" in
        has_brackets && has_lr
      in
      let has_lsls_opcode (inst : instr) : bool =
        let op_str = pp_print_instr' inst in
        let op_str = String.lowercase_ascii op_str in
        String.exists op_str "lsls"
      in
      let has_lsrs_opcode (inst : instr) : bool =
        let op_str = pp_print_instr' inst in
        let op_str = String.lowercase_ascii op_str in
        String.exists op_str "lsrs"
      in
      let get_loc_addr (l : loc) : int = l.loc_addr in
      let rec help (inst : instr) =
        match inst with
        (* ARM Thumb *)
        | SingleInstr (Arm_OP (Arm_CommonOP (Arm_Other NOP), _), _, _, _, _) ->
            last_nop <- true;
            last_ret <- false;
            last_special <- false
        (* movs r0, r0 *)
        | TripleInstr
            ( Arm_OP (Arm_CommonOP (Arm_Assign MOV), _),
              Reg (Arm_Reg (Arm_CommonReg R0)),
              Reg (Arm_Reg (Arm_CommonReg R0)),
              l,
              _,
              _,
              _ ) ->
            last_nop <- true;
            last_ret <- false;
            last_special <- false
        (* pop {r7, pc} *)
        | DoubleInstr (Arm_OP (Arm_StackOP POP, _), Label lab, _, _, _, _)
          when has_pc_reg lab ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- true
        (* add a function *)
        (* push {r4, lr} *)
        | DoubleInstr (Arm_OP (Arm_StackOP PUSH, _), Label lab, _, _, _, _)
          when has_lr_reg lab ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false;
            func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
        (* add a function *)
        (* last_pop but not lsls opcode *)
        | _
          when last_pop
               && (not (has_lsls_opcode inst))
               && not (has_lsrs_opcode inst) ->
            (* [has_lsls_opcode] function was introduced to avoid misclassifying
            the instruction, which is actually data, as a function begin.
            pop {r7,pc}
            BB_21:
            lsls r6,r5,#0x2 *)
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false;
            func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
        (* add a function *)
        | TripleInstr (_, _, _, _, _, _, _) when last_nop ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false;
            func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
        (* add a function *)
        | FourInstr (_, _, _, _, _, _, _, _) when last_nop ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false;
            func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
        (* AArch64 *)
        | SingleInstr (Arm_OP (Arm_ControlOP RET, _), _, _, _, _) ->
            last_nop <- false;
            last_ret <- true;
            last_special <- false;
            last_pop <- false
        (* ret
         * nop
         * nop
         * S_0x400520:
         * adrp x0,0x411000 *)
        | TripleInstr (_, _, _, _, _, _, _) when last_ret ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false;
            func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
        (* ret
         * nop
         * S_0x400590:
         * stp x29,x30,[sp, #-0x20]! *)
        | FourInstr (_, _, _, _, _, _, _, _) when last_ret ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false;
            func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
        (* movs r0, r0 *)
        | _ ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false
      in
      List.iter help (List.rev instrs)
  end
