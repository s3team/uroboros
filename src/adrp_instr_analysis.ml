open Semantic_analysis

module AdrpAbs : DfaAbs = struct
  open Type
  open Utils
  open Pp_print
  open Ail_utils
  module AdrpMap = Map.Make (String)

  (* adrp-assigned register -> base address + the adrp instruction itself.
     The adrp instruction is kept so that, when a consumer is found, the
     adrp's operand can be retargeted to the consumer's full address. *)
  type adrp_info = { base : int; adrp : instr }
  type abs_state = adrp_info AdrpMap.t
  type abstraction = abs_state

  let initial = AdrpMap.empty
  let equal a b = AdrpMap.equal (fun x y -> x.base = y.base) a b
  let result = Hashtbl.create 100

  let callee_saved_regs =
    List.map
      (fun r -> p_reg (Arm_Reg (Arm_CommonReg r)))
      [ X19; X20; X21; X22; X23; X24; X25; X26; X27; X28 ]

  let process_instr (i : instr) (ins : abs_state) : unit = ()

  let print_abstraction abs =
    AdrpMap.iter (fun k v -> print_endline (k ^ " -> " ^ dec_hex v.base)) abs

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
    | Arm_OP (Arm_CommonOP (Arm_Assign op), _, _) -> begin
        match op with
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

  let rewrite_adrp_operand (adrp : instr) (full : int) : instr =
    match adrp with
    | TripleInstr (op, _, dst, loc, pre, tags) ->
        TripleInstr (op, Const (Immediate full), dst, loc, pre, tags)
    | _ -> adrp

  (* Materialize an absolute constant into [dst] with movz/movk. Used for the
     `adrp xN,0 ; add xN,xN,#k` idiom where the adrp page target is 0. *)
  let const_load_directives (dst : exp) (value : int) (loc : loc) : instr list =
    let reg = match dst with Reg r -> p_reg r | _ -> "x0" in
    let g sh = (value lsr sh) land 0xffff in
    let movz = Printf.sprintf "\tmovz %s, #0x%x\n" reg (g 0) in
    let movks =
      List.filter_map
        (fun sh ->
          if g sh <> 0 then
            Some (Printf.sprintf "\tmovk %s, #0x%x, lsl #%d\n" reg (g sh) sh)
          else None)
        [ 16; 32; 48 ]
    in
    let text = String.concat "" (movz :: movks) in
    [
      SingleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Other NOP), None, None),
          { loc with loc_label = text; loc_visible = false },
          None,
          Hashtbl.create 1 );
    ]

  let emit_const (adrp : instr) (dst : exp) (value : int) (consumer_loc : loc)
      (kill_reg : string) (outs : abs_state) : abs_state =
    Hashtbl.replace result (get_loc adrp).loc_addr [];
    Hashtbl.replace result consumer_loc.loc_addr
      (const_load_directives dst value consumer_loc);
    match AdrpMap.find_opt kill_reg outs with
    | Some _ -> AdrpMap.remove kill_reg outs
    | None -> outs

  let emit_pair (adrp : instr) (full : int) (consumer_loc : loc)
      (new_consumer : instr) (kill_reg : string) (outs : abs_state) : abs_state =
    Hashtbl.replace result (get_loc adrp).loc_addr
      [ rewrite_adrp_operand adrp full ];
    Hashtbl.replace result consumer_loc.loc_addr [ new_consumer ];
    match AdrpMap.find_opt kill_reg outs with
    | Some _ -> AdrpMap.remove kill_reg outs
    | None -> outs

  let flow_through (i : instr) (ins : abs_state) : abs_state =
    let module TU = Tags_utils in
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
        let adrp_instr =
          TripleInstr
            ( Arm_OP (Arm_CommonOP (Arm_Arithm ADRP), condsuff, widthsuff),
              Const (Immediate base_addr),
              Reg exp_reg,
              loc,
              pre,
              tags )
        in
        let adrp_add_instrs : instr list =
          [
            adrp_instr;
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
        AdrpMap.add (p_reg exp_reg) { base = base_addr; adrp = adrp_instr } outs
      end
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Arithm ADRP), _, _),
          (Symbol (JumpDes base_addr) | Const (Immediate base_addr)),
          Reg exp_reg,
          _,
          _,
          _ ) as adrp_i ->
        AdrpMap.add (p_reg exp_reg) { base = base_addr; adrp = adrp_i } outs
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
          Ptr (UnOP exp_offset_reg),
          Reg (Arm_Reg arm_exp_reg as ldr_dest_exp),
          loc,
          pre,
          tags )
      when AdrpMap.mem (p_reg exp_offset_reg) outs -> begin
        match AdrpMap.find_opt (p_reg exp_offset_reg) outs with
        | Some { base; adrp } -> begin
            let full = base in
            let new_ldr =
              TripleInstr
                ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                  Ptr (BinOP_PLUS (exp_offset_reg, full)),
                  Reg ldr_dest_exp,
                  loc,
                  pre,
                  TU.set_tag_by_kv ":lo12:" (Str "") tags )
            in
            let kill_dest = promote_reg_for_load arm_exp_reg in
            emit_pair adrp full loc new_ldr (p_reg kill_dest) outs
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
      when AdrpMap.mem (p_reg exp_offset_reg) outs -> begin
        match AdrpMap.find_opt (p_reg exp_offset_reg) outs with
        | Some { base; adrp } -> begin
            let full = base + offset in
            let new_ldr =
              TripleInstr
                ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), condsuff, widthsuff),
                  Ptr (BinOP_PLUS (exp_offset_reg, full)),
                  Reg ldr_dest_exp,
                  loc,
                  pre,
                  TU.set_tag_by_kv ":lo12:" (Str "") tags )
            in
            let kill_dest = promote_reg_for_load arm_exp_reg in
            emit_pair adrp full loc new_ldr (p_reg kill_dest) outs
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
           && AdrpMap.mem (p_reg exp_offset_reg) outs -> begin
        match AdrpMap.find_opt (p_reg exp_offset_reg) outs with
        | Some { base; adrp } -> begin
            let full = base + offset in
            let new_load =
              TripleInstr
                ( Arm_OP (Arm_CommonOP (Arm_Assign load_op), condsuff, widthsuff),
                  Ptr (BinOP_PLUS (exp_offset_reg, full)),
                  Reg ldr_dest_exp,
                  loc,
                  pre,
                  TU.set_tag_by_kv ":lo12:" (Str "") tags )
            in
            let kill_dest = promote_reg_for_load arm_exp_reg in
            emit_pair adrp full loc new_load (p_reg kill_dest) outs
          end
        | None -> outs
      end
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Assign STR), condsuff, widthsuff),
          Ptr (BinOP_PLUS (exp_offset_reg, offset)),
          Reg (Arm_Reg arm_exp_reg as src_exp),
          loc,
          pre,
          tags )
      when AdrpMap.mem (p_reg exp_offset_reg) outs -> begin
        match AdrpMap.find_opt (p_reg exp_offset_reg) outs with
        | Some { base; adrp } -> begin
            let full = base + offset in
            let new_str =
              TripleInstr
                ( Arm_OP (Arm_CommonOP (Arm_Assign STR), condsuff, widthsuff),
                  Ptr (BinOP_PLUS (exp_offset_reg, full)),
                  Reg src_exp,
                  loc,
                  pre,
                  TU.set_tag_by_kv ":lo12:" (Str "") tags )
            in
            let kill_src = promote_reg_for_load arm_exp_reg in
            emit_pair adrp full loc new_str (p_reg kill_src) outs
          end
        | None -> outs
      end
    | TripleInstr
        ( Arm_OP (Arm_CommonOP (Arm_Assign store_op), condsuff, widthsuff),
          Ptr (BinOP_PLUS (exp_offset_reg, offset)),
          Reg (Arm_Reg arm_exp_reg as src_exp),
          loc,
          pre,
          tags )
      when (store_op = STRB || store_op = STRH)
           && AdrpMap.mem (p_reg exp_offset_reg) outs -> begin
        match AdrpMap.find_opt (p_reg exp_offset_reg) outs with
        | Some { base; adrp } -> begin
            let full = base + offset in
            let new_store =
              TripleInstr
                ( Arm_OP
                    (Arm_CommonOP (Arm_Assign store_op), condsuff, widthsuff),
                  Ptr (BinOP_PLUS (exp_offset_reg, full)),
                  Reg src_exp,
                  loc,
                  pre,
                  TU.set_tag_by_kv ":lo12:" (Str "") tags )
            in
            let kill_src = promote_reg_for_load arm_exp_reg in
            emit_pair adrp full loc new_store (p_reg kill_src) outs
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
      when AdrpMap.mem (p_reg exp_reg1) outs -> begin
        match AdrpMap.find_opt (p_reg exp_reg1) outs with
        | Some { base; adrp } -> begin
            let full = base + offset in
            let kill_dest = promote_reg_for_load arm_exp_reg in
            if base = 0 then
              emit_const adrp (Reg add_dest_exp) full loc (p_reg kill_dest) outs
            else
              let new_add =
                FourInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), condsuff, widthsuff),
                    Const (Immediate full),
                    Reg exp_reg1,
                    Reg add_dest_exp,
                    loc,
                    pre,
                    TU.set_tag_by_kv ":lo12:" (Str "") tags )
              in
              emit_pair adrp full loc new_add (p_reg kill_dest) outs
          end
        | None -> outs
      end
    | DoubleInstr (Arm_OP (Arm_ControlOP (BL | BLX), _, _), _, _, _, _)
    | DoubleInstr (Arm_OP (Arm_CommonOP (Arm_Assign BLR), _, _), _, _, _, _) ->
        (* A call clobbers the caller-saved registers, so drop every tracked reg
           except the callee-saved ones *)
        AdrpMap.filter (fun reg _ -> List.mem reg callee_saved_regs) outs
    | TripleInstr (p, _, Reg dest_exp, _, _, _) when not (is_read_last_reg p) ->
      begin
        match AdrpMap.find_opt (p_reg dest_exp) outs with
        | Some _ -> AdrpMap.remove (p_reg dest_exp) outs
        | None -> outs
      end
    | FourInstr (p, _, _, Reg dest_exp, _, _, _) when not (is_read_last_reg p)
      -> begin
        match AdrpMap.find_opt (p_reg dest_exp) outs with
        | Some _ -> AdrpMap.remove (p_reg dest_exp) outs
        | None -> outs
      end
    | FifInstr (p, _, _, _, Reg dest_exp, _, _, _) when not (is_read_last_reg p)
      -> begin
        match AdrpMap.find_opt (p_reg dest_exp) outs with
        | Some _ -> AdrpMap.remove (p_reg dest_exp) outs
        | None -> outs
      end
    | _ -> outs
end
