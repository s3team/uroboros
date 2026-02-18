open Type
open Pp_print
open Common_func_slicer
open Ail_utils

class func_slicer instrs funcs =
  object (self)
    inherit common_func_slicer instrs funcs as super

    method process =
      let is_begin op e =
        let is_stack =
          match op with
          | Intel_OP io -> (
              match io with Intel_StackOP PUSH -> true | _ -> false)
          | Arm_OP (ao, _, _) -> (
              match ao with Arm_StackOP PUSH -> true | _ -> false)
          | _ -> false
        in
        let is_ebp =
          match e with
          | Reg (Intel_Reg (Intel_StackReg EBP)) -> true
          | Reg (Intel_Reg (Intel_StackReg EBP)) -> true
          | _ -> false
        in
        is_stack && is_ebp
      in
      let rec help l =
        match l with
        | h::[] ->
           begin
             let loc' = get_loc h in
             if eaddr = (-1) then
               failwith "no function recognized at all"
             else
               eaddr <- (loc'.loc_addr);
             self#update
           end
        | h::t ->
           begin
             match h with
             | DoubleInstr (p, e, l, _, _, _) when (is_begin p e) ->
                if eaddr = (-1) then (* first function *)
                  (
                  baddr <- l.loc_addr;
                  eaddr <- l.loc_addr;
                  help t)
                else (
                  eaddr <- l.loc_addr;
                  super#update;
                  baddr <- l.loc_addr;
                  help t)
            | _ -> ()
          end
        | _ -> failwith "process "
      in
      help (List.rev instrs)

    method process2 =
      let is_s_begin op e =
        let is_stack =
          match op with
          | Intel_OP io -> (
              match io with Intel_StackOP PUSH -> true | _ -> false)
          | Arm_OP (ao, _, _) -> (
              match ao with Arm_StackOP PUSH -> true | _ -> false)
          | _ -> false
        in
        let is_ebp =
          match e with
          | Reg (Intel_Reg (Intel_StackReg EBP)) -> true
          | Reg (Intel_Reg (Intel_StackReg EBP)) -> true
          | _ -> false
        in
        is_stack && is_ebp
      in
      let is_s_begin2 op e1 e2 i =
        let is_sub =
          match op with
          | Intel_OP io -> (
              match io with
              | Intel_CommonOP (Intel_Arithm SUB) -> true
              | _ -> false)
          | Arm_OP (ao, _, _) -> (
              match ao with Arm_CommonOP (Arm_Arithm SUB) -> true | _ -> false)
          | _ -> false
        in
        let is_const = match e1 with Const (Normal _) -> true | _ -> false in
        let is_sp =
          match e2 with
          | Reg (Intel_Reg (Intel_StackReg ESP)) -> true
          | Reg (Intel_Reg (Intel_StackReg RSP)) -> true
          | _ -> false
        in
        is_sub && is_const && is_sp
      in
      (* match mov    0x4(%esp),%_ *)
      (* match mov    0x8(%esp),%_ *)
      (* match mov    0x10(%esp),%_ *)
      (*let is_begin3 op e1 e2 =
        begin
          let c1 =
            match op with
            | Intel_OP io ->
              begin
                match io with
                | Intel_CommonOP (Intel_Assign MOV) -> true
                | Intel_CommonOP (Intel_Assign MOVZBL) -> true
                | _ -> false
              end
            | Arm_OP (ao, _) -> failwith "TODO: ARM support: is_begin3" in
          let c2 =
            match e1 with
            | Ptr (BinOP_PLUS(Intel_Reg (Intel_StackReg ESP),4)) -> true
            | Ptr (BinOP_PLUS(Intel_Reg (Intel_StackReg ESP),8)) -> true
            | Ptr (BinOP_PLUS(Intel_Reg (Intel_StackReg ESP),0x10)) -> true
            | Ptr (BinOP_PLUS(Intel_Reg (Intel_StackReg ESP),0xc)) -> true
            | _ -> false in
          let c3 =
            match e2 with
            | Reg (Intel_Reg (Intel_CommonReg _)) -> true
            | _ -> false in
          c1 && c2 && c3
        end in*)
      (*
          match for lea 0x0(%esi,%eiz, 1)
          lea 0x0(%edi,...)
          lea 0x0(%rsi,...)
          lea 0x0(%rdi,...)
       *)
      let is_nop1 op e =
        let is_lea =
          match op with
          | Intel_OP io -> (
              match io with
              | Intel_CommonOP (Intel_Assign LEA) -> true
              | _ -> false)
          | Arm_OP (_, _, _) -> false
          | _ -> false
        in
        let is_fourp =
          match e with
          | Ptr (FourOP_PLUS (r1, r2, off1, off2)) ->
              let aux1 =
                match r1 with
                | Intel_Reg ir -> (
                    match ir with
                    | Intel_CommonReg ESI -> true
                    | Intel_CommonReg EDI -> true
                    | Intel_CommonReg RSI -> true
                    | Intel_CommonReg RDI -> true
                    | _ -> false)
                | Arm_Reg _ -> false
                | _ -> false
              in
              let aux2 =
                match r2 with
                | Intel_Reg ir -> (
                    match ir with Intel_OtherReg EIZ -> true | _ -> false)
                | Arm_Reg _ -> false
                | _ -> false
              in
              aux1 && aux2 && off2 = 0 && off1 = 1
          | _ -> false
        in
        is_lea && is_fourp
      in
      let is_nop2 op e1 e2 =
        let is_lea =
          match op with
          | Intel_OP io -> (
              match io with
              | Intel_CommonOP (Intel_Assign LEA) -> true
              | _ -> false)
          | Arm_OP (_, _, _) -> false
          | _ -> false
        in
        let is_onep =
          match e1 with
          | Ptr (BinOP_PLUS (r, off)) ->
              let aux1 =
                match r with
                | Intel_Reg ir -> (
                    match ir with
                    | Intel_CommonReg EDI -> true
                    | Intel_CommonReg ESI -> true
                    | Intel_CommonReg RSI -> true
                    | Intel_CommonReg RDI -> true
                    | _ -> false)
                | Arm_Reg _ -> false
                | _ -> false
              in
              aux1 && off = 0
          | _ -> false
        in
        let is_reg =
          match e2 with
          | Reg (Intel_Reg (Intel_CommonReg EDI)) -> true
          | Reg (Intel_Reg (Intel_CommonReg ESI)) -> true
          | Reg (Intel_Reg (Intel_CommonReg RSI)) -> true
          | Reg (Intel_Reg (Intel_CommonReg RDI)) -> true
          | _ -> false
        in
        is_lea && is_reg && is_onep
      in
      let is_c_begin op e =
        let is_call =
          match op with
          | Intel_OP io -> (
              match io with
              | Intel_ControlOP c -> ( match c with CALL -> true | _ -> false)
              | _ -> false)
          | Arm_OP (ao, _, _) -> (
              match ao with
              | Arm_ControlOP aco -> (
                  match aco with BL | BLX -> true | _ -> false)
              | _ -> false)
          | _ -> false
        and is_local =
          match e with
          | Symbol s -> (
              match s with
              | CallDes c -> if c.is_lib = false then true else false
              | _ -> false)
          | _ -> false
        in
        is_call && is_local && super#check_text e
      and call_des e =
        match e with
        | Symbol s -> (
            match s with
            | CallDes c -> (
                if c.func_begin_addr <> 0 then c.func_begin_addr
                else
                  let n = c.func_name in
                  let n' = String.sub n 2 (String.length n - 2) in
                  try int_of_string n'
                  with
                  (* it might be function name symbols, with uninitized function begin addr *)
                  | _ ->
                    failwith ("uninitized function begin addr " ^ n))
            | _ -> failwith "didn't support in call_des")
        | _ -> failwith "didn't support in call_des"
      in
      let is_cet_begin op =
        match op with
        | Intel_OP (Intel_CommonOP (Intel_Other other_op)) ->(
            match other_op with ENDBR32 | ENDBR64 -> true | _ -> false)
        | _ -> false
      in
      let rec help h =
        match h with
        (* multiple lea ... can be used to align together *)
        | TripleInstr (p, e1, e2, l, _, _, _) when is_nop1 p e2 ->
            last_nop <- false;
            last_ret <- false;
            last_xchg <- false;
            last_jmp <- false;
            last_special <- true
        (* multiple lea    0x0(%edi),%edi can be used to align together *)
        | TripleInstr (p, e1, e2, l, _, _, _) when is_nop2 p e2 e1 ->
            last_nop <- false;
            last_ret <- false;
            last_xchg <- false;
            last_jmp <- false;
            last_special <- true
        (* here is our trategy, whenever detect the instruction
             lea    0x0(%esi,%eiz,1),%esi
           consider the next instruction as function beginning.
         *)
        | SingleInstr (Intel_OP (Intel_CommonOP (Intel_Other NOP)), _, _, _, _) ->
            last_nop <- true;
            last_ret <- false;
            last_xchg <- false;
            last_jmp <- false;
            last_special <- false
        | TripleInstr
            ( Intel_OP (Intel_CommonOP (Intel_Assign XCHG)),
              Reg (Intel_Reg (Intel_CommonReg AX)),
              Reg (Intel_Reg (Intel_CommonReg AX)),
              _,
              _, _, _) ->
            last_nop <- false;
            last_ret <- false;
            last_xchg <- true;
            last_jmp <- false;
            last_special <- false
        | SingleInstr (Intel_OP (Intel_ControlOP RET), _, _, _, _) ->
            last_nop <- false;
            last_special <- false;
            last_xchg <- false;
            last_jmp <- false;
            last_ret <- true
        | DoubleInstr (Intel_OP (Intel_ControlOP (Intel_Jump JMP)), _, l, _, _, _)
          when last_nop ->
            last_nop <- false;
            last_xchg <- false;
            last_ret <- false;
            last_special <- false;
            last_jmp <- false;
            func_begins <- l.loc_addr :: func_begins
        | DoubleInstr (Intel_OP (Intel_ControlOP (Intel_Jump JMP)), _, _, _, _, _) ->
            last_nop <- false;
            last_special <- false;
            last_xchg <- false;
            last_jmp <- true;
            last_ret <- false
        (* | DoubleInstr (CommonOP (Assign REPZ), Label "ret", l, _) when *)
        (* last_nop || last_special || last_ret -> *)
        | DoubleInstr
            (Intel_OP (Intel_CommonOP (Intel_Assign REPZ)), Label "ret", l, _, _, _)
          when last_nop || last_special ->
            last_nop <- false;
            last_xchg <- false;
            last_ret <- false;
            last_special <- false;
            last_jmp <- false;
            func_begins <- l.loc_addr :: func_begins
        (* xor %eax,%eax *)
        | TripleInstr
            ( Intel_OP (Intel_CommonOP (Intel_Logic XOR)),
              Reg (Intel_Reg (Intel_CommonReg EAX)),
              Reg (Intel_Reg (Intel_CommonReg EAX)),
              l,
              _, _, _ )
          when last_special ->
            last_nop <- false;
            last_xchg <- false;
            last_ret <- false;
            last_jmp <- false;
            last_special <- false;
            func_begins <- l.loc_addr :: func_begins
        | h when last_special = true ->
           begin
             let c = match h with
               | DoubleInstr (p, _, _, _, _, _) ->
                  begin
                    match p with
                    | Intel_OP io ->
                      begin
                        match io with
                        | Intel_ControlOP (_) -> false
                        (*  | CommonOP (Assign _) -> true *)
                        | Intel_CommonOP _ -> false
                        | Intel_SystemOP _ -> false
                        | Intel_StackOP _ -> true
                        | Intel_ErrorOP _ -> false
                        | _ -> false
                      end
                    | Arm_OP (_, _, _) -> failwith "TODO: ARM support: c"
                  end
              (*| TripleInstr (Intel_OP (Intel_CommonOP (Intel_Assign MOV)), e1,e2,_,_) ->
                  is_begin3 (Intel_OP (Intel_CommonOP (Intel_Assign MOV))) e2 e1
               | TripleInstr (Intel_OP (Intel_CommonOP (Intel_Assign MOVZBL)), e1,e2,_,_) ->
                  is_begin3 (Intel_OP (Intel_CommonOP (Intel_Assign MOVZBL))) e2 e1*)
               | TripleInstr (Intel_OP (Intel_CommonOP (Intel_Arithm SUB)), e1, e2, _, _, _, _) ->
                  is_s_begin2 (Intel_OP (Intel_CommonOP (Intel_Arithm SUB))) e2 e1 h
               | _ -> false in
             if c = false then
               begin
                 last_special <- false;
             	 last_jmp <- false;
                 last_xchg <- false;
             	 last_ret <- false;
                 last_nop <- false
               end
             else
               begin
                 last_special <- false;
                 last_xchg <- false;
             	 last_jmp <- false;
                 last_ret <- false;
                 last_nop <- false;
                 let l = get_loc h in
                 func_begins <- l.loc_addr::func_begins
               end
           end
        (*| TripleInstr (p, e1, e2, l,_) when ( last_nop || last_jmp) && is_begin3 p e2 e1 ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_ret <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end*)
        | SingleInstr (Intel_OP (Intel_CommonOP (Intel_Assign (FLDZ))), l, _, _, _) when last_special ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_ret <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | (DoubleInstr (Intel_OP (Intel_StackOP PUSH), Reg (Intel_Reg (Intel_CommonReg ESI)), l, _, _, _)
          ) when last_nop || last_xchg || last_special || last_ret  || last_jmp ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_ret <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | (DoubleInstr (Intel_OP (Intel_StackOP PUSH), Reg (Intel_Reg (Intel_CommonReg EDI)), l, _, _, _)
          ) when last_nop || last_ret || last_xchg || last_special ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_ret <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | DoubleInstr (Intel_OP (Intel_StackOP PUSH), Reg (Intel_Reg (Intel_CommonReg EBX)), l, _, _, _) when last_ret || last_ret || last_nop || last_special ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_ret <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        (*| TripleInstr (p, e1, e2, l, _) when (is_s_begin2 p e2 e1 h) && (last_ret || last_nop || last_special || last_xchg || last_jmp) ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end*)
        | TripleInstr (Intel_OP (Intel_CommonOP (Intel_Assign MOV)), Reg (Intel_Reg (Intel_CommonReg EAX)), Const (Normal 1), l, _, _, _) when last_special ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        (*| DoubleInstr (p, e, l, _, _, _) when (is_s_begin p e) ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end*)
        (*| SingleInstr (p, l, _, _, _) when (is_cet_begin p) ->
          begin
            let int_to_hex n =
              Printf.sprintf "0x%X" n
            in
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
          end*)
        | DoubleInstr (p, e, l, _, _, _) when (is_c_begin p e) ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false;
             let c_des = call_des e in
             func_begins <- c_des::func_begins
           end
        | _ ->
            last_special <- false;
            last_xchg <- false;
            last_ret <- false;
            last_jmp <- false;
            last_nop <- false
      in
      List.iter help (List.rev instrs)
  end
