open Ail_utils
open Pp_print
open Type


let init_got () : int =
  let split_by_space s = Str.split (Str.regexp " ") s in
  let filelines : string list = read_lines "pic_secs.info" in
  let info = List.nth filelines 0 in
  let info_str = split_by_space info in
  let got_addr_str = "0x" ^ List.nth info_str 1 in
  int_of_string got_addr_str

let got_rewrite_instr
    (got_reg : string)
    (got_addr : int)
    (result : (int, instr) Hashtbl.t)
    (i : instr)
  : unit =
  (*let _ = print_endline (pp_print_instr' i) in*)
  let module U = UIO in
  match i with
  | DoubleInstr
      ( Intel_OP (Intel_ControlOP (Intel_Jump JMP)),
        Symbol (StarDes (Ptr (FourOP_MINUS (reg1, reg2, const1, const2)))),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg1)) ->
      (* reg1 contains GOT pointer,
       * reg2 contain index,
       * const1 contains multipler,
       * const2 contains offset
       * EX: 808a4d6: jmp    *-0x2514(%ebp,%eax,4)  ---> jmp *S_0x80F7508(,%eax,0x4)
       *)
      let got_plus_offset = got_addr - const2 in
      Hashtbl.replace
        result
        loc.loc_addr
        ( DoubleInstr
          ( Intel_OP (Intel_ControlOP (Intel_Jump JMP)),
            Symbol (StarDes (Ptr (JmpTable_PLUS (got_plus_offset, reg2, const1)))),
            loc,
            prefix,
            tag,
            tags ) )
  | DoubleInstr
      ( Intel_OP (Intel_ControlOP CALL),
        Symbol (StarDes (Ptr (BinOP_PLUS (reg, const)))),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( DoubleInstr
          ( Intel_OP (Intel_ControlOP CALL),
            Symbol (StarDes (Const (Point got_plus_offset))),
            loc,
            prefix,
            tag,
            tags ) )
  | DoubleInstr
      ( Intel_OP (Intel_ControlOP CALL),
        Symbol (StarDes (Ptr (BinOP_MINUS (reg, const)))),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr - const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( DoubleInstr
          ( Intel_OP (Intel_ControlOP CALL),
            Symbol (StarDes (Const (Point got_plus_offset))),
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
        dest_ptr,
        Ptr (BinOP_MINUS (reg, const)),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr - const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
        dest_ptr,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
        dest_ptr,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags ) )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Arithm ADD)),
          Reg reg1,
          Reg reg2,
          loc,
          prefix,
          tags )
      when got_reg = (p_exp (Reg reg2)) ->
        Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Arithm ADD)),
            Reg reg1,
            Const (Normal got_addr),
            loc,
            prefix,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
        dest_ptr,
        Ptr (FourOP_PLUS (reg1, reg2, const1, const2)),
        loc,
        prefix,
        tags )
    when got_reg = (p_exp (Reg reg1)) ->
      let got_plus_offset = got_addr + const2 in
      Hashtbl.replace
      result
      loc.loc_addr
      ( TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          dest_ptr,
          Ptr (JmpTable_PLUS (got_plus_offset, reg2, const1)),
          loc,
          prefix,
          tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
        dest_ptr,
        Ptr (FourOP_MINUS (reg1, reg2, const1, const2)),
        loc,
        prefix,
        tags )
    (* reg1 is base address, reg2 is offset *)
    when got_reg = (p_exp (Reg reg1)) ->
      let got_plus_offset = got_addr - const2 in
      Hashtbl.replace
      result
      loc.loc_addr
      ( TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          dest_ptr,
          Ptr (JmpTable_PLUS (got_plus_offset, reg2, const1)),
          loc,
          prefix,
          tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
        Ptr (BinOP_PLUS (reg, const)),
        dest_ptr,
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
            Const (Point got_plus_offset),
            dest_ptr,
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
        dest_ptr,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
        Ptr (BinOP_PLUS (reg, const)),
        dest_ptr,
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            Const (Point got_plus_offset),
            dest_ptr,
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
        dest_ptr,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
        Ptr (BinOP_PLUS (reg, const)),
        dest_ptr,
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
            Const (Point got_plus_offset),
            dest_ptr,
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
        Ptr (BinOP_PLUS (reg, const)),
        cmp_to,
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
            Const (Point got_plus_offset),
            cmp_to,
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
        cmp_to,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
            cmp_to,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
        Ptr (BinOP_MINUS (reg, const)),
        cmp_to,
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
            Const (Point got_plus_offset),
            cmp_to,
            loc,
            prefix,
            tag,
            tags ) )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
        cmp_to,
        Ptr (BinOP_MINUS (reg, const)),
        loc,
        prefix,
        tag,
        tags )
    when got_reg = (p_exp (Reg reg)) ->
      let got_plus_offset = got_addr + const in
      Hashtbl.replace
        result
        loc.loc_addr
        ( TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
            cmp_to,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags ) )
  | _ -> ()

(** assuming that initial register containing GOT, got_reg, is not overwritten
  * throughout the function, perform GOT rewriting without considering control
  * or data flow *)
let got_rewrite
    (got_reg : string)
    (il : instr list)
    (result : (int, instr) Hashtbl.t)
  : unit =
  List.iter (
    fun instr ->
      got_rewrite_instr got_reg (init_got ()) result instr
  ) il

(** identify whether reg is overwritten in il *)
let is_reassign (reg : string) (il : instr list) : bool =
  let reassigned = List.find_opt (
    fun instr ->
      match instr with
      | TripleInstr (p, e1, e2, _, _, _, _) -> (
          match (p, e1, e2) with
        | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Label l, Reg r
        | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Reg r, Label l ->
            false
         | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), e1, e2 ->
              if reg = (p_exp e1) then
                (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Arithm SUB)), e1, e2 ->
              if reg = (p_exp e1) then
                (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Assign MOV)), e1, e2 ->
              (* e1: dest, e2: src *)
              if reg = (p_exp e1) then
                (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Assign MOVZBL)), e1, e2 ->
              (* e1: dest, e2: src *)
              if reg = (p_exp e1) then (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Logic XOR)), e1, e2 ->
              (* e1: dest, e2: src *)
              if reg = (p_exp e1) then (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Logic OR)), e1, e2 ->
              (* e1: dest, e2: src *)
              if reg = (p_exp e1) then (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Rol SHR)), e1, e2 ->
              (* e1: dest, e2: src *)
              if reg = (p_exp e1) then (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Rol SHL)), e1, e2 ->
              (* e1: dest, e2: src *)
              if reg = (p_exp e1) then (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Logic AND)), e1, e2 ->
              (* e1: dest, e2: src *)
              if reg = (p_exp e1) then (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Logic NOT)), e1, e2 ->
              (* e1: dest, e2: src *)
              if reg = (p_exp e1) then (* kill *)
                true
              else false
          | Intel_OP (Intel_CommonOP (Intel_Assign LEA)), e1, e2 ->
              if reg = (p_exp e1) then (* kill *)
                true
              else false
          | _ -> false)
      | _ -> false
  ) il in
  match reassigned with
  | Some _ -> true
  | None -> false
