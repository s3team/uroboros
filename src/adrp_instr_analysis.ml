open Semantic_analysis

module AdrpAbs : DfaAbs = struct
  open Type
  open Ail_utils
  open Utils
  open Pp_print
  module AdrpMap = Map.Make (String)

  (* adrp assigned register -> adrp base address *)
  type abs_state = int AdrpMap.t
  type abstraction = abs_state

  let initial = AdrpMap.empty
  let equal a b = AdrpMap.equal ( = ) a b
  let result = Hashtbl.create 100
  let process_instr (i : instr) (ins : abs_state) : unit = ()

  let print_abstraction abs =
    AdrpMap.iter (fun k v -> print_endline (k ^ " -> " ^ dec_hex v)) abs

  let promote_reg_for_load reg =
    match reg with
    | Arm_CommonReg W0 -> Arm_Reg (Arm_CommonReg X0)
    | Arm_CommonReg W1 -> Arm_Reg (Arm_CommonReg X1)
    | Arm_CommonReg W2 -> Arm_Reg (Arm_CommonReg X2)
    | Arm_CommonReg W3 -> Arm_Reg (Arm_CommonReg X3)
    | Arm_CommonReg W4 -> Arm_Reg (Arm_CommonReg X4)
    | Arm_CommonReg W5 -> Arm_Reg (Arm_CommonReg X5)
    | Arm_CommonReg W6 -> Arm_Reg (Arm_CommonReg X6)
    | Arm_CommonReg W7 -> Arm_Reg (Arm_CommonReg X7)
    | Arm_CommonReg W8 -> Arm_Reg (Arm_CommonReg X8)
    | Arm_CommonReg W9 -> Arm_Reg (Arm_CommonReg X9)
    | Arm_CommonReg W10 -> Arm_Reg (Arm_CommonReg X10)
    | Arm_CommonReg W11 -> Arm_Reg (Arm_CommonReg X11)
    | Arm_CommonReg W12 -> Arm_Reg (Arm_CommonReg X12)
    | Arm_CommonReg W13 -> Arm_Reg (Arm_CommonReg X13)
    | Arm_CommonReg W14 -> Arm_Reg (Arm_CommonReg X14)
    | Arm_CommonReg W15 -> Arm_Reg (Arm_CommonReg X15)
    | Arm_CommonReg W16 -> Arm_Reg (Arm_CommonReg X16)
    | Arm_CommonReg W17 -> Arm_Reg (Arm_CommonReg X17)
    | Arm_CommonReg W18 -> Arm_Reg (Arm_CommonReg X18)
    | Arm_CommonReg W19 -> Arm_Reg (Arm_CommonReg X19)
    | Arm_CommonReg W20 -> Arm_Reg (Arm_CommonReg X20)
    | Arm_CommonReg W21 -> Arm_Reg (Arm_CommonReg X21)
    | Arm_CommonReg W22 -> Arm_Reg (Arm_CommonReg X22)
    | Arm_CommonReg W23 -> Arm_Reg (Arm_CommonReg X23)
    | Arm_CommonReg W24 -> Arm_Reg (Arm_CommonReg X24)
    | Arm_CommonReg W25 -> Arm_Reg (Arm_CommonReg X25)
    | Arm_CommonReg W26 -> Arm_Reg (Arm_CommonReg X26)
    | Arm_CommonReg W27 -> Arm_Reg (Arm_CommonReg X27)
    | Arm_CommonReg W28 -> Arm_Reg (Arm_CommonReg X28)
    | _ -> Arm_Reg reg

  let merge (preds : instr option list)
      (input_map : (instr, abs_state) Hashtbl.t) : abs_state =
    List.fold_left
      (fun acc p_op ->
        match p_op with
        | None -> acc
        | Some p ->
            let p_abs_state = Hashtbl.find input_map p in
            AdrpMap.union (fun k v1 v2 -> Some v1) acc p_abs_state)
      AdrpMap.empty preds

  let is_store_op : op -> bool = function
    | Arm_OP (Arm_CommonOP (Arm_Assign op), _, _) ->
        begin match op with
        | STR | STRB | STRBT | STRD | STREX | STREXB | STREXD | STREXH | STRH
        | STRHT | STRT | STP | STUR | STURB | STURH | STLR | STXR | STLXR | VSTR
        | VST4 | VSTM | VSTMDB | VPUSH | VSTMIA | FSTMIAX | STM | STMDA | STMED
        | STMIB | STMDB | STMFD | STMIA | STMEA | ST1B ->
            true
        | _ -> false
        end
    | _ -> false

  let is_compare_op : op -> bool = function
    | Arm_OP (Arm_CommonOP (Arm_Compare _), _, _) -> true
    | _ -> false

  let is_read_last_reg p = is_store_op p || is_compare_op p

  let flow_through (i : instr) (ins : abs_state) : abs_state =
    let module TU = Tags_utils in
    let module EU = Elf_utils in
    let outs = ins in
    match i with
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Arithm ADR), condsuff, widthsuff),
          (Symbol (JumpDes base_addr) | Const (Immediate base_addr)),
          Reg exp_reg,
          loc,
          pre,
          tags ) -> begin
        (* adr has a plus/minus of 1MB range.
         * Replace adr with adrp + add for larger range *)
        let adrp_add_instrs : instr list =
          [
            TripleInstr
              ( Arm_OP (Arm_CommonOP (Arm_Arithm ADRP), condsuff, widthsuff),
                Const (Immediate base_addr),
                Reg exp_reg,
                loc,
                pre,
                tags );
            FourInstr
              ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), condsuff, widthsuff),
                Const (Immediate base_addr),
                Reg exp_reg,
                Reg exp_reg,
                loc,
                pre,
                TU.set_tag_by_kv ":lo12:" (Str "") tags );
          ]
        in
        Hashtbl.replace result loc.loc_addr adrp_add_instrs;
        outs
      end
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Arithm ADRP), condsuff, widthsuff),
          (Symbol (JumpDes base_addr) | Const (Immediate base_addr)),
          Reg exp_reg,
          loc,
          pre,
          tags ) -> begin
        let nop_instr : instr =
          SingleInstr
            ( Arm_OP (Arm_CommonOP (Arm_Other NOP), condsuff, widthsuff),
              loc,
              pre,
              tags )
        in
        Hashtbl.replace result loc.loc_addr [ nop_instr ];
        AdrpMap.add (p_reg exp_reg) base_addr outs
      end
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
          Ptr (UnOP exp_offset_reg),
          Reg (Arm_Reg arm_exp_reg as ldr_dest_exp),
          loc,
          pre,
          tags )
      when AdrpMap.mem (p_reg exp_offset_reg) outs ->
        begin match AdrpMap.find_opt (p_reg exp_offset_reg) outs with
        | Some base_addr -> begin
            let ldr_dest_exp = promote_reg_for_load arm_exp_reg in
            let abs_addr : int = base_addr in
            let pseudo_qx_instrs : instr list =
              [
                FourInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign STP), condsuff, widthsuff),
                    Ptr (BinOP_PLUS_WB (Arm_StackReg SP, -0x10)),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Arithm ADRP), condsuff, widthsuff),
                    Const (Immediate abs_addr),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                    Ptr (BinOP_PLUS (Arm_Reg (Arm_CommonReg X10), abs_addr)),
                    Reg ldr_dest_exp,
                    loc,
                    pre,
                    TU.set_tag_by_kv ":lo12:" (Str "") tags
                    |> TU.set_tag_by_kv "align" (Str "") );
                FifInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDP), condsuff, widthsuff),
                    Const (Immediate 0x10),
                    Ptr (UnOP (Arm_Reg (Arm_StackReg SP))),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
              ]
            in
            let pseudo_norm_instrs : instr list =
              if EU.elf_static () then
                [
                  TripleInstr
                    ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                      Const (Point abs_addr),
                      Reg ldr_dest_exp,
                      loc,
                      pre,
                      tags );
                  TripleInstr
                    ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                      Ptr (UnOP ldr_dest_exp),
                      Reg ldr_dest_exp,
                      loc,
                      pre,
                      tags );
                ]
              else
                [
                  TripleInstr
                    ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                      Const (Point abs_addr),
                      Reg ldr_dest_exp,
                      loc,
                      pre,
                      tags );
                ]
            in
            let pseudo_instrs =
              match arm_exp_reg with
              | Arm_SpecialReg
                  ( Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11
                  | Q12 | Q13 | Q14 | Q15 ) ->
                  pseudo_qx_instrs
              | _ -> pseudo_norm_instrs
            in
            Hashtbl.replace result loc.loc_addr pseudo_instrs;
            match AdrpMap.find_opt (p_reg ldr_dest_exp) outs with
            | Some _ -> AdrpMap.remove (p_reg ldr_dest_exp) outs
            | None -> outs
          end
        | None -> outs
        end
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
          Ptr (BinOP_PLUS (exp_offset_reg, offset)),
          Reg (Arm_Reg arm_exp_reg as ldr_dest_exp),
          loc,
          pre,
          tags )
      when AdrpMap.mem (p_reg exp_offset_reg) outs ->
        begin match AdrpMap.find_opt (p_reg exp_offset_reg) outs with
        | Some base_addr -> begin
            let ldr_dest_exp = promote_reg_for_load arm_exp_reg in
            let abs_addr : int = base_addr + offset in
            let pseudo_qx_instrs : instr list =
              [
                FourInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign STP), condsuff, widthsuff),
                    Ptr (BinOP_PLUS_WB (Arm_StackReg SP, -0x10)),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Arithm ADRP), condsuff, widthsuff),
                    Const (Immediate abs_addr),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                    Ptr (BinOP_PLUS (Arm_Reg (Arm_CommonReg X10), abs_addr)),
                    Reg ldr_dest_exp,
                    loc,
                    pre,
                    TU.set_tag_by_kv ":lo12:" (Str "") tags
                    |> TU.set_tag_by_kv "align" (Str "") );
                FifInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDP), condsuff, widthsuff),
                    Const (Immediate 0x10),
                    Ptr (UnOP (Arm_Reg (Arm_StackReg SP))),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
              ]
            in
            let pseudo_norm_instrs : instr list =
              if EU.elf_static () then
                if p_reg ldr_dest_exp = p_reg (Arm_Reg (Arm_CommonReg X0)) then
                  [
                    TripleInstr
                      ( Arm_OP
                          (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                        Const (Point abs_addr),
                        Reg (Arm_Reg (Arm_CommonReg X0)),
                        loc,
                        pre,
                        tags );
                    TripleInstr
                      ( Arm_OP
                          (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                        Ptr (UnOP (Arm_Reg (Arm_CommonReg X0))),
                        Reg ldr_dest_exp,
                        loc,
                        pre,
                        tags );
                  ]
                else
                  [
                    TripleInstr
                      ( Arm_OP
                          (Arm_CommonOP (Arm_Assign STR), condsuff, widthsuff),
                        Ptr (BinOP_PLUS_WB (Arm_StackReg SP, -16)),
                        Reg (Arm_Reg (Arm_CommonReg X0)),
                        loc,
                        pre,
                        tags );
                    TripleInstr
                      ( Arm_OP
                          (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                        Const (Point abs_addr),
                        Reg (Arm_Reg (Arm_CommonReg X0)),
                        loc,
                        pre,
                        tags );
                    TripleInstr
                      ( Arm_OP
                          (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                        Ptr (UnOP (Arm_Reg (Arm_CommonReg X0))),
                        Reg ldr_dest_exp,
                        loc,
                        pre,
                        tags );
                    FourInstr
                      ( Arm_OP
                          (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                        Const (Immediate 16),
                        Ptr (UnOP (Arm_Reg (Arm_StackReg SP))),
                        Reg (Arm_Reg (Arm_CommonReg X0)),
                        loc,
                        pre,
                        tags );
                  ]
              else
                [
                  TripleInstr
                    ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                      Const (Point abs_addr),
                      Reg ldr_dest_exp,
                      loc,
                      pre,
                      tags );
                ]
            in
            let pseudo_instrs =
              match arm_exp_reg with
              | Arm_SpecialReg
                  ( Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11
                  | Q12 | Q13 | Q14 | Q15 ) ->
                  pseudo_qx_instrs
              | _ -> pseudo_norm_instrs
            in
            Hashtbl.replace result loc.loc_addr pseudo_instrs;
            match AdrpMap.find_opt (p_reg ldr_dest_exp) outs with
            | Some _ -> AdrpMap.remove (p_reg ldr_dest_exp) outs
            | None -> outs
          end
        | None -> outs
        end
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Assign load_op), condsuff, widthsuff),
          Ptr (BinOP_PLUS (exp_offset_reg, offset)),
          Reg (Arm_Reg arm_exp_reg as ldr_dest_exp),
          loc,
          pre,
          tags )
      when (load_op = LDRB || load_op = LDRSW || load_op = LDRH
          || load_op = LDRSH || load_op = LDRSB)
           && AdrpMap.mem (p_reg exp_offset_reg) outs ->
        begin match AdrpMap.find_opt (p_reg exp_offset_reg) outs with
        | Some base_addr -> begin
            let abs_addr : int = base_addr + offset in
            let pseudo_qx_instrs : instr list =
              [
                FourInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign STP), condsuff, widthsuff),
                    Ptr (BinOP_PLUS_WB (Arm_StackReg SP, -0x10)),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Arithm ADRP), condsuff, widthsuff),
                    Const (Immediate abs_addr),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                    Ptr (BinOP_PLUS (Arm_Reg (Arm_CommonReg X10), abs_addr)),
                    Reg ldr_dest_exp,
                    loc,
                    pre,
                    TU.set_tag_by_kv ":lo12:" (Str "") tags
                    |> TU.set_tag_by_kv "align" (Str "") );
                FifInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDP), condsuff, widthsuff),
                    Const (Immediate 0x10),
                    Ptr (UnOP (Arm_Reg (Arm_StackReg SP))),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
              ]
            in
            let pseudo_norm_instrs : instr list =
              if EU.elf_static () then
                [
                  TripleInstr
                    ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                      Const (Point abs_addr),
                      Reg exp_offset_reg,
                      loc,
                      pre,
                      tags );
                  TripleInstr
                    ( Arm_OP
                        (Arm_CommonOP (Arm_Assign load_op), condsuff, widthsuff),
                      Ptr (UnOP exp_offset_reg),
                      Reg ldr_dest_exp,
                      loc,
                      pre,
                      tags );
                ]
              else
                [
                  TripleInstr
                    ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                      Const (Point abs_addr),
                      Reg ldr_dest_exp,
                      loc,
                      pre,
                      tags );
                ]
            in
            let pseudo_instrs =
              match arm_exp_reg with
              | Arm_SpecialReg
                  ( Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11
                  | Q12 | Q13 | Q14 | Q15 ) ->
                  pseudo_qx_instrs
              | _ -> pseudo_norm_instrs
            in
            Hashtbl.replace result loc.loc_addr pseudo_instrs;
            match AdrpMap.find_opt (p_reg ldr_dest_exp) outs with
            | Some _ -> AdrpMap.remove (p_reg ldr_dest_exp) outs
            | None -> outs
          end
        | None -> outs
        end
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Assign store_op), condsuff, widthsuff),
          Ptr (BinOP_PLUS (exp_offset_reg, offset)),
          Reg (Arm_Reg arm_exp_reg as ldr_dest_exp),
          loc,
          pre,
          tags )
      when (store_op = STR || store_op = STRB || store_op = STRH)
           && AdrpMap.mem (p_reg exp_offset_reg) outs ->
        begin match AdrpMap.find_opt (p_reg exp_offset_reg) outs with
        | Some base_addr -> begin
            let abs_addr : int = base_addr + offset in
            let pseudo_qx_instrs : instr list =
              [
                FourInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign STP), condsuff, widthsuff),
                    Ptr (BinOP_PLUS_WB (Arm_StackReg SP, -0x10)),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                    Const (Point abs_addr),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP
                      (Arm_CommonOP (Arm_Assign store_op), condsuff, widthsuff),
                    Ptr (UnOP (Arm_Reg (Arm_CommonReg X10))),
                    Reg ldr_dest_exp,
                    loc,
                    pre,
                    tags );
                FifInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDP), condsuff, widthsuff),
                    Const (Immediate 0x10),
                    Ptr (UnOP (Arm_Reg (Arm_StackReg SP))),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
              ]
            in
            let scratch_reg =
              if p_reg ldr_dest_exp = p_reg (Arm_Reg (Arm_CommonReg X10)) then
                Arm_CommonReg X9
              else Arm_CommonReg X10
            in
            let pseudo_norm_instrs : instr list =
              [
                FourInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign STP), condsuff, widthsuff),
                    Ptr (BinOP_PLUS_WB (Arm_StackReg SP, -0x10)),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                    Const (Point abs_addr),
                    Reg (Arm_Reg scratch_reg),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP
                      (Arm_CommonOP (Arm_Assign store_op), condsuff, widthsuff),
                    Ptr (UnOP (Arm_Reg scratch_reg)),
                    Reg ldr_dest_exp,
                    loc,
                    pre,
                    tags );
                FifInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDP), condsuff, widthsuff),
                    Const (Immediate 0x10),
                    Ptr (UnOP (Arm_Reg (Arm_StackReg SP))),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
              ]
            in
            let pseudo_instrs =
              match arm_exp_reg with
              | Arm_SpecialReg
                  ( Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11
                  | Q12 | Q13 | Q14 | Q15 ) ->
                  pseudo_qx_instrs
              | _ -> pseudo_norm_instrs
            in
            Hashtbl.replace result loc.loc_addr pseudo_instrs;
            match AdrpMap.find_opt (p_reg ldr_dest_exp) outs with
            | Some _ -> AdrpMap.remove (p_reg ldr_dest_exp) outs
            | None -> outs
          end
        | None -> outs
        end
    | FourInstr
        ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), condsuff, widthsuff),
          Const (Immediate offset),
          Reg exp_reg1,
          Reg (Arm_Reg arm_exp_reg as add_dest_exp),
          loc,
          pre,
          tags )
    | FourInstr
        ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), condsuff, widthsuff),
          Reg exp_reg1,
          Const (Immediate offset),
          Reg (Arm_Reg arm_exp_reg as add_dest_exp),
          loc,
          pre,
          tags )
      when AdrpMap.mem (p_reg exp_reg1) outs ->
        begin match AdrpMap.find_opt (p_reg exp_reg1) outs with
        | Some base_addr -> begin
            let abs_addr : int = base_addr + offset in
            let pseudo_qx_instrs : instr list =
              [
                FourInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign STP), condsuff, widthsuff),
                    Ptr (BinOP_PLUS_WB (Arm_StackReg SP, -0x10)),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Arithm ADRP), condsuff, widthsuff),
                    Const (Immediate abs_addr),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    loc,
                    pre,
                    tags );
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                    Ptr (BinOP_PLUS (Arm_Reg (Arm_CommonReg X10), abs_addr)),
                    Reg add_dest_exp,
                    loc,
                    pre,
                    TU.set_tag_by_kv ":lo12:" (Str "") tags
                    |> TU.set_tag_by_kv "align" (Str "") );
                FifInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDP), condsuff, widthsuff),
                    Const (Immediate 0x10),
                    Ptr (UnOP (Arm_Reg (Arm_StackReg SP))),
                    Reg (Arm_Reg (Arm_CommonReg X10)),
                    Reg (Arm_Reg (Arm_CommonReg X9)),
                    loc,
                    pre,
                    tags );
              ]
            in
            let pseudo_norm_instrs : instr list =
              [
                TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                    Const (Point abs_addr),
                    Reg add_dest_exp,
                    loc,
                    pre,
                    tags );
              ]
            in
            let pseudo_instrs =
              match arm_exp_reg with
              | Arm_SpecialReg
                  ( Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11
                  | Q12 | Q13 | Q14 | Q15 ) ->
                  pseudo_qx_instrs
              | _ -> pseudo_norm_instrs
            in
            Hashtbl.replace result loc.loc_addr pseudo_instrs;
            match AdrpMap.find_opt (p_reg add_dest_exp) outs with
            | Some _ -> AdrpMap.remove (p_reg add_dest_exp) outs
            | None -> outs
          end
        | None -> outs
        end
    | TripleInstr (p, _, Reg dest_exp, _, _, _) when not (is_read_last_reg p) ->
        begin match AdrpMap.find_opt (p_reg dest_exp) outs with
        | Some _ -> AdrpMap.remove (p_reg dest_exp) outs
        | None -> outs
        end
    | FourInstr (p, _, _, Reg dest_exp, _, _, _) when not (is_read_last_reg p)
      ->
        begin match AdrpMap.find_opt (p_reg dest_exp) outs with
        | Some _ -> AdrpMap.remove (p_reg dest_exp) outs
        | None -> outs
        end
    | FifInstr (p, _, _, _, Reg dest_exp, _, _, _) when not (is_read_last_reg p)
      ->
        begin match AdrpMap.find_opt (p_reg dest_exp) outs with
        | Some _ -> AdrpMap.remove (p_reg dest_exp) outs
        | None -> outs
        end
    | _ -> outs
end
