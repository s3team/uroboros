module ExpOrd = struct
  open Type
  open Pp_print

  type t = exp

  let compare i1 i2 = String.compare (p_exp i1) (p_exp i2)
end

module ExpSet = Set.Make (ExpOrd)

module type DfaAbs = sig
  open Type

  val initial : ExpSet.t
  val equal : ExpSet.t -> ExpSet.t -> bool
  val merge : instr option list -> (instr, ExpSet.t) Hashtbl.t -> ExpSet.t
  val flow_through : instr -> ExpSet.t -> ExpSet.t
  val process_instr : instr -> ExpSet.t -> unit
  val result : (int, instr) Hashtbl.t (* term rewriting based on DFA results *)
end

module GotAbs : DfaAbs = struct
  open Type
  open Ail_utils
  open Pp_print

  let initial = ExpSet.empty
  let equal = ExpSet.equal
  let union = ExpSet.union
  let result = Hashtbl.create 100

  let init_got () : int =
    let split_by_space s = Str.split (Str.regexp " ") s in
    let filelines : string list = read_lines "pic_secs.info" in
    let info = List.nth filelines 0 in
    let info_str = split_by_space info in
    let got_addr_str = "0x" ^ List.nth info_str 1 in
    int_of_string got_addr_str

  let merge
      (preds : instr option list)
      (input_map : (instr, ExpSet.t) Hashtbl.t)
    : ExpSet.t =
    let union_preds =
     fun acc (p_op : instr option) ->
      match p_op with
      | Some p ->
          let out = Hashtbl.find input_map p in
          union acc out
      | None -> acc
    in
    List.fold_left union_preds ExpSet.empty preds

  let got_rewrite_instr (i : instr) (ins : ExpSet.t) (got_addr : int) : instr =
    match i with
    | DoubleInstr
        ( Intel_OP (Intel_ControlOP (Intel_Jump JMP)),
          Symbol (StarDes (Ptr (FourOP_MINUS (reg1, reg2, const1, const2)))),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg1) ins ->
        let got_plus_offset = got_addr - const2 in
        (*let _ = print_endline ("orig0: " ^ (pp_print_instr' i)) in*)
         DoubleInstr
         ( Intel_OP (Intel_ControlOP (Intel_Jump JMP)),
           Symbol (StarDes (Ptr (JmpTable_PLUS (got_plus_offset, reg2, const1)))),
           loc,
           prefix,
           tag,
           tags )
    | DoubleInstr
        ( Intel_OP (Intel_ControlOP CALL),
          Symbol (StarDes (Ptr (BinOP_PLUS (reg, const)))),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig1: " ^ (pp_print_instr' i)) in*)
        DoubleInstr
          ( Intel_OP (Intel_ControlOP CALL),
            Symbol (StarDes (Const (Point got_plus_offset))),
            loc,
            prefix,
            tag,
            tags )
    | DoubleInstr
        ( Intel_OP (Intel_ControlOP CALL),
          Symbol (StarDes (Ptr (BinOP_MINUS (reg, const)))),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr - const in
        (*let _ = print_endline ("orig2: " ^ (pp_print_instr' i)) in*)
        DoubleInstr
          ( Intel_OP (Intel_ControlOP CALL),
            Symbol (StarDes (Const (Point got_plus_offset))),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Arithm ADD)),
          Reg reg1,
          Reg reg2,
          loc,
          prefix,
          tags )
      when ExpSet.mem (Reg reg2) ins ->
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Arithm ADD)),
            Reg reg1,
            Const (Normal got_addr),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Arithm ADD)),
          Reg reg1,
          Reg reg2,
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg2) ins ->
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Arithm ADD)),
            Reg reg1,
            Const (Normal got_addr),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
          dest_ptr,
          Ptr (BinOP_MINUS (reg, const)),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr - const in
        (*let _ = print_endline ("orig3: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
          dest_ptr,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig4: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
          dest_ptr,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig5: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
          Ptr (BinOP_PLUS (reg, const)),
          dest_ptr,
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig6: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
            Const (Point got_plus_offset),
            dest_ptr,
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          dest_ptr,
          Ptr (FourOP_PLUS (reg1, reg2, const1, const2)),
          loc,
          prefix,
          tags )
      when ExpSet.mem (Reg reg1) ins ->
        let got_plus_offset = got_addr + const2 in
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            dest_ptr,
            Ptr (JmpTable_PLUS (got_plus_offset, reg2, const1)),
            loc,
            prefix,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          dest_ptr,
          Ptr (FourOP_MINUS (reg1, reg2, const1, const2)),
          loc,
          prefix,
          tags )
      (* reg1 is base address, reg2 is offset *)
      when ExpSet.mem (Reg reg1) ins ->
        let got_plus_offset = got_addr - const2 in
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            dest_ptr,
            Ptr (JmpTable_PLUS (got_plus_offset, reg2, const1)),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          dest_ptr,
          Ptr (FourOP_PLUS (reg1, reg2, const1, const2)),
          loc,
          prefix,
          tags )
      when ExpSet.mem (Reg reg1) ins ->
        let got_plus_offset = got_addr + const2 in
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            dest_ptr,
            Ptr (JmpTable_PLUS (got_plus_offset, reg2, const1)),
            loc,
            prefix,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          dest_ptr,
          Ptr (FourOP_MINUS (reg1, reg2, const1, const2)),
          loc,
          prefix,
          tags )
      (* reg1 is base address, reg2 is offset *)
      when ExpSet.mem (Reg reg1) ins ->
        let got_plus_offset = got_addr - const2 in
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            dest_ptr,
            Ptr (JmpTable_PLUS (got_plus_offset, reg2, const1)),
            loc,
            prefix,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          dest_ptr,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          Ptr (BinOP_PLUS (reg, const)),
          dest_ptr,
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            Const (Point got_plus_offset),
            dest_ptr,
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
          dest_ptr,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
            dest_ptr,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
          Ptr (BinOP_PLUS (reg, const)),
          dest_ptr,
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
            Const (Point got_plus_offset),
            dest_ptr,
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          Ptr (BinOP_PLUS (reg, const)),
          cmp_to,
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
            Const (Point got_plus_offset),
            cmp_to,
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          cmp_to,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
            cmp_to,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          Ptr (BinOP_MINUS (reg, const)),
          cmp_to,
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
            Const (Point got_plus_offset),
            cmp_to,
            loc,
            prefix,
            tag,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          cmp_to,
          Ptr (BinOP_MINUS (reg, const)),
          loc,
          prefix,
          tag,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
        TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
            cmp_to,
            Const (Point got_plus_offset),
            loc,
            prefix,
            tag,
            tags )
    | _ -> i

  let process_instr (i : instr) (ins : ExpSet.t) : unit =
    let got_addr = init_got () in
    let newi = got_rewrite_instr i ins got_addr in
    if pp_print_instr newi <> pp_print_instr i then
      Hashtbl.replace result (get_loc i).loc_addr newi
    else ()

  (** identify GOT pointer and propagate facts *)
  let flow_through (i : instr) (ins : ExpSet.t) : ExpSet.t =
    let outs = ins in
    match i with
    | i when contains_existing_fact (get_tags i) "use" outs ->
        let exp_def = Hashtbl.find (get_tags i) "def" in
        ( match exp_def with
          | Exp e -> ExpSet.add e outs
          | _ -> outs )
    | TripleInstr (p, e1, e2, _, _, _) -> (
        match (p, e1, e2) with
        | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Label l, Reg r
        | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Reg r, Label l ->
            if contains ~str:l ~sub:"$_GLOBAL_OFFSET_TABLE_" then
              (* gen *)
              (*print_endline ("gen: " ^ (p_exp (Reg r)) ^ ", " ^ (pp_print_instr' i));*)
              let got_reg_tags = Hashtbl.create 0 in
              let _ = Hashtbl.replace got_reg_tags "got_reg" (Str (p_exp (Reg r))) in
              Hashtbl.replace
                result
                0
                ( SingleInstr (Intel_OP (Intel_CommonOP (Intel_Other NOP)),
                  stub_loc,
                  None,
                  None,
                  got_reg_tags) );
              ExpSet.add (Reg r) outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), e1, e2 ->
            if ExpSet.mem e1 outs then
              (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Arithm SUB)), e1, e2 ->
            if ExpSet.mem e1 outs then
              (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Assign MOV)), e1, e2 ->
            (* e1: dest, e2: src *)
            if ExpSet.mem e2 outs then
              (* gen *)
              match e1 with
              | Reg _
              | Ptr (UnOP _)
              | Ptr (BinOP_MINUS (Intel_Reg (Intel_StackReg _), _))
              | Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg _), _)) ->
                  (*print_endline ("gen: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
                  ExpSet.add e1 outs
              | _ -> outs
            else if ExpSet.mem e1 outs then (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Assign MOVZBL)), e1, e2 ->
            (* e1: dest, e2: src *)
            if ExpSet.mem e2 outs then
              (* gen *)
              match e1 with
              | Reg _
              | Ptr (UnOP _)
              | Ptr (BinOP_MINUS (Intel_Reg (Intel_StackReg _), _))
              | Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg _), _)) ->
                  (*print_endline ("gen: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
                  ExpSet.add e1 outs
              | _ -> outs
            else if ExpSet.mem e1 outs then (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Logic XOR)), e1, e2 ->
            (* e1: dest, e2: src *)
            if ExpSet.mem e1 outs then (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Logic OR)), e1, e2 ->
            (* e1: dest, e2: src *)
            if ExpSet.mem e1 outs then (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Rol SHR)), e1, e2 ->
            (* e1: dest, e2: src *)
            if ExpSet.mem e1 outs then (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Rol SHL)), e1, e2 ->
            (* e1: dest, e2: src *)
            if ExpSet.mem e1 outs then (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Logic AND)), e1, e2 ->
            (* e1: dest, e2: src *)
            if ExpSet.mem e1 outs then (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Logic NOT)), e1, e2 ->
            (* e1: dest, e2: src *)
            if ExpSet.mem e1 outs then (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | Intel_OP (Intel_CommonOP (Intel_Assign LEA)), e1, e2 ->
            if ExpSet.mem e2 outs then
              (* gen *)
              match e1 with
              | Reg _
              | Ptr (UnOP _)
              | Ptr (BinOP_MINUS (Intel_Reg (Intel_StackReg _), _))
              | Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg _), _)) ->
                  (*print_endline ("gen: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
                  ExpSet.add e1 outs
              | _ -> outs
            else if ExpSet.mem e1 outs then (* kill *)
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            else outs
        | _ -> outs)
    | _ -> outs
end

module DFA (A : DfaAbs) = struct
  open Pp_print
  open Ail_utils
  open Type

  let in_map = Hashtbl.create 10
  let out_map = Hashtbl.create 10

  let flow_analysis (f : string) (cfg : cfgi) : unit =
    (* init each CFG node *)
    let { preds; succs; il } = cfg in
    let _ =
      List.iter
        (fun i ->
          Hashtbl.add in_map i A.initial;
          Hashtbl.add out_map i A.initial)
        il
    in
    (* init worklist *)
    let worklist = Queue.create () in
    let _ = List.iter (fun i -> Queue.push i worklist) il in
    (* iterate until fixpoint *)
    while not (Queue.is_empty worklist) do
      let i : instr = Queue.pop worklist in
      let preds : instr option list =
        try Hashtbl.find preds (Some i) with Not_found -> []
      in
      (* With [preds], get all outs from [out_map] and merge them *)
      let ins = A.merge preds out_map in
      let _ = Hashtbl.replace in_map i ins in

      A.process_instr i ins;

      let old_outs = Hashtbl.find out_map i in
      let outs = A.flow_through i ins in

      (* debug log *)
      (*if f = "S_0x8058165" then
        let _ = print_endline ("> " ^ (pp_print_instr' i)) in
        let _ = print_endline (" outs: ") in
        let _ = ExpSet.iter (fun e -> print_endline ("    " ^ (p_exp e))) outs in
        let _ = print_endline (" ins: ") in
        ExpSet.iter (fun e -> print_endline ("    " ^ (p_exp e))) ins;*)

      if not (A.equal old_outs outs) then (
        Hashtbl.replace out_map i outs;
        let succs =
          match Hashtbl.find_opt succs (Some i) with Some v -> v | None -> []
        in
        List.iter
          (fun i_op ->
            match i_op with Some i -> Queue.push i worklist | None -> ())
          succs)
    done
end
