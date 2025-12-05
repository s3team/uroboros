open Batteries
open Type
open Pp_print
open Common_func_slicer
open Ail_utils

class arm_func_slicer instrs funcs =
  object (self)
    inherit common_func_slicer instrs funcs
    val mutable push_lr_added : bool = false

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
      let has_fp_reg (lab : label) : bool =
        let has_brackets = String.contains lab '{' && String.contains lab '}' in
        let has_fp = String.exists lab "fp" in
        has_brackets && has_fp
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
      let is_func_prologue (inst : instr) : bool =
        match inst with
        | DoubleInstr (Arm_OP (Arm_StackOP PUSH, _, _), Label lab, _, _, _, _)
        | TripleInstr
            ( Arm_OP (Arm_StackOP STMDB, _, _),
              Label lab,
              Ptr (WB (Arm_StackReg SP)),
              _,
              _,
              _,
              _ )
          when has_lr_reg lab ->
            true
        | _ -> false
      in
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
              _,
              _ ) ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- true
        | DoubleInstr (Arm_OP (Arm_StackOP op, _, _), Label lab, _, _, _, _)
          when is_pop op && has_pc_reg lab ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- true
        (* add a function *)
        (* push {r4, lr} *)
        | DoubleInstr (Arm_OP (Arm_StackOP PUSH, _, _), Label lab, _, _, _, _)
        (* stmdb sp!, {r4, r5, lr} *)
        | TripleInstr
            ( Arm_OP (Arm_StackOP STMDB, _, _),
              Label lab,
              Ptr (WB (Arm_StackReg SP)),
              _,
              _,
              _,
              _ )
          when has_lr_reg lab || has_fp_reg lab
          (* The above when condition was commented out due to the following case:
           * in make-prime-list:
           * 1072c:       e92d 0ff0       stmdb   sp!, {r4, r5, r6, r7, r8, r9, sl, fp}
           * should be recognized as a function beginning
           *)
          ->
            last_nop <- false;
            last_ret <- false;
            last_special <- false;
            last_pop <- false;
            if not push_lr_added then begin
              func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
            end
            else begin
              push_lr_added <- false
            end
        (* add a function *)
        (* push	{r7}
         * sub	sp, #12
         *)
        | DoubleInstr (Arm_OP (Arm_StackOP PUSH, _, _), Label lab, _, _, _, _)
          when not (has_lr_reg lab) -> begin
            let next_instr = List.nth ordered_il (idx + 1) in
            match next_instr with
            | TripleInstr
                ( op,
                  Const (Immediate _),
                  Reg (Arm_Reg (Arm_StackReg SP)),
                  l,
                  _,
                  _,
                  _ ) ->
                last_nop <- false;
                last_ret <- false;
                last_special <- false;
                last_pop <- false;
                func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
            | _ -> begin
                (* When there is a `push {..., lr}` within the next two instructions,
                 * add the current instruction as a function beginning
                 *)
                let check_push_lr (instr_list : instr list) : bool =
                  List.exists (fun i -> is_func_prologue i) instr_list
                in
                let next_next_instr = List.nth ordered_il (idx + 2) in
                let instrs_to_check = [ next_instr; next_next_instr ] in
                if check_push_lr instrs_to_check then begin
                  last_nop <- false;
                  last_ret <- false;
                  last_special <- false;
                  last_pop <- false;
                  push_lr_added <- true;
                  func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
                end
                else ()
              end
          end
        (* S_0x14FB4:
         * ldr r3,[pc, #0x8]
         * movs r1,#0x0
         * add r3,pc
         * ldr r2,[r3, #0x0]
         * b.w __cxa_atexit *)
        | TripleInstr
            ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
              Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg r), offset)),
              exp2,
              loc,
              _,
              _,
              _ )
          when List.length ordered_il > idx + 5 ->
            let n_inst = List.nth ordered_il (idx + 1) in
            let nn_inst = List.nth ordered_il (idx + 2) in
            let nnn_inst = List.nth ordered_il (idx + 3) in
            let nnnn_inst = List.nth ordered_il (idx + 4) in
            let is_atexit_pattern () =
              let op = get_op inst in
              let n_op = get_op n_inst in
              let nn_op = get_op nn_inst in
              let nnn_op = get_op nnn_inst in
              match (n_op, nn_op, nnn_op, nnnn_inst) with
              | ( Arm_OP (Arm_CommonOP (Arm_Assign MOVS), _, _),
                  Arm_OP (Arm_CommonOP (Arm_Arithm ADD), _, _),
                  Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
                  DoubleInstr
                    ( Arm_OP (Arm_ControlOP B, _, _),
                      Symbol (CallDes func),
                      _,
                      _,
                      _,
                      _ ) )
                when contains ~str:func.func_name ~sub:"__cxa_atexit" ->
                  true
              | _ -> false
            in
            if is_atexit_pattern () then begin
              last_nop <- false;
              last_ret <- false;
              last_special <- false;
              last_pop <- false;
              func_begins <- (get_loc inst |> get_loc_addr) :: func_begins
            end
            else ()
        (* add a function *)
        (* AArch64 *)
        | SingleInstr (Arm_OP (Arm_ControlOP RET, _, _), _, _, _, _) ->
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
      List.iteri (fun idx instr -> help idx instr) ordered_il
  end
