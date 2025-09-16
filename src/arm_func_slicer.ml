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
      let has_sp_reg (lab : label) : bool =
        let has_sp = String.exists lab "sp" in
        has_sp
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
      let is_push (op : arm_stackop) : bool =
        match op with PUSH | VPUSH | STMDB -> true | _ -> false
      in
      let is_pop (op : arm_stackop) : bool =
        match op with POP | VPOP | LDMIA -> true | _ -> false
      in
      let ordered_il = List.rev instrs in
      let rec help (idx : int) (inst : instr) =
        match inst with
        (* pop {r7, pc} *)
        | DoubleInstr
            ( Arm_OP (Arm_ControlOP BX, _, _),
              Reg (Arm_Reg (Arm_LinkReg LR)),
              _,
              _,
              _, _) ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- true
        | DoubleInstr (Arm_OP (Arm_StackOP op, _), Label lab, _, _, _, _)
          when is_pop op && has_pc_reg lab ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- true
        (* add a function *)
        (* push {r4, lr} *)
        | DoubleInstr (Arm_OP (Arm_StackOP op, _), Label lab, _, _, _, _)
          when is_push op && has_lr_reg lab ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false;
            func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
        (* add a function *)
        (* push	{r7}
         * sub	sp, #12
         *)
        | DoubleInstr (Arm_OP (Arm_StackOP PUSH, _, _), Label lab, _, _, _)
          when not (has_lr_reg lab) -> begin
              let next_instr = List.nth ordered_il (idx + 1) in
              match next_instr with
              | TripleInstr (op, Const (Immediate _), Reg (Arm_Reg (Arm_StackReg SP)), l, _, _) ->
                  last_nop <- false;
                  last_ret <- false;
                  last_special <- false;
                  last_pop <- false;
                  func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
              | _ -> ()
            end
        (* add a function *)
        | TripleInstr (Arm_OP (Arm_StackOP STMDB, _, _), _, Label lab, _, _, _, _)
          when has_sp_reg lab ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false;
            func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
        (* add a function *)
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
      List.iteri (fun idx instr -> help idx instr) ordered_il;
  end
