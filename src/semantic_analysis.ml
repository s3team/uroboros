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
  val result : (int, instr) Hashtbl.t (* for rewriting based on DFA results *)
end

module GotAbs : DfaAbs = struct
  open Type
  open Ail_utils
  open Pp_print

  let initial = ExpSet.empty
  let equal = ExpSet.equal
  let union = ExpSet.union
  let result = Hashtbl.create 100

  let init_got () =
    let split_by_space s = Str.split (Str.regexp " ") s in
    let filelines : string list = read_lines "pic_secs.info" in
    let info = List.nth filelines 0 in
    let info_str = split_by_space info in
    let got_addr_str = "0x" ^ List.nth info_str 1 in
    int_of_string got_addr_str

  let merge preds input_map =
    let union_preds =
     fun acc (p_op : instr option) ->
      match p_op with
      | Some p ->
          let out = Hashtbl.find input_map p in
          union acc out
      | None -> acc
    in
    List.fold_left union_preds ExpSet.empty preds

  let process_inner i ins got_addr =
    match i with
    | DoubleInstr
        ( Intel_OP (Intel_ControlOP CALL),
          Symbol (StarDes (Ptr (BinOP_PLUS (reg, const)))),
          loc,
          prefix,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr + const in
        (*let _ = print_endline ("orig1: " ^ (pp_print_instr' i)) in*)
        DoubleInstr
          ( Intel_OP (Intel_ControlOP CALL),
            Symbol (StarDes (Const (Point got_plus_offset))),
            loc,
            prefix,
            tags )
    | DoubleInstr
        ( Intel_OP (Intel_ControlOP CALL),
          Symbol (StarDes (Ptr (BinOP_MINUS (reg, const)))),
          loc,
          prefix,
          tags )
      when ExpSet.mem (Reg reg) ins ->
        let got_plus_offset = got_addr - const in
        (*let _ = print_endline ("orig2: " ^ (pp_print_instr' i)) in*)
        DoubleInstr
          ( Intel_OP (Intel_ControlOP CALL),
            Symbol (StarDes (Const (Point got_plus_offset))),
            loc,
            prefix,
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
          dest_ptr,
          Ptr (BinOP_MINUS (reg, const)),
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
          dest_ptr,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
          dest_ptr,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
          Ptr (BinOP_PLUS (reg, const)),
          dest_ptr,
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          dest_ptr,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          Ptr (BinOP_PLUS (reg, const)),
          dest_ptr,
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
          dest_ptr,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
          Ptr (BinOP_PLUS (reg, const)),
          dest_ptr,
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          Ptr (BinOP_PLUS (reg, const)),
          cmp_to,
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          cmp_to,
          Ptr (BinOP_PLUS (reg, const)),
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          Ptr (BinOP_MINUS (reg, const)),
          cmp_to,
          loc,
          prefix,
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
            tags )
    | TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          cmp_to,
          Ptr (BinOP_MINUS (reg, const)),
          loc,
          prefix,
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
            tags )
    | _ -> i

  let process_instr i ins =
    let got_addr = init_got () in
    let newi = process_inner i ins got_addr in
    if pp_print_instr newi <> pp_print_instr i then
      Hashtbl.replace result (get_loc i).loc_addr newi
    else ()

  let flow_through i ins =
    let outs = ins in
    match i with
    | TripleInstr (p, e1, e2, _, _, _) -> (
        match (p, e1, e2) with
        | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Label l, Reg r
        | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Reg r, Label l ->
            if contains ~str:l ~sub:"$_GLOBAL_OFFSET_TABLE_" then
              (* gen *)
              (*print_endline ("gen: " ^ (p_exp (Reg r)) ^ ", " ^ (pp_print_instr' i));*)
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

  let flow_analysis cfg =
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
      let ins = A.merge preds out_map in
      let _ = Hashtbl.replace in_map i ins in

      A.process_instr i ins;

      let old_outs = Hashtbl.find out_map i in
      let outs = A.flow_through i ins in

      (* debug log *)
      (*let _ = print_endline ("> " ^ (pp_print_instr' i)) in
      let _ = print_endline (" outs: ") in
      let _ = ExpSet.iter (fun e -> print_endline ("    " ^ (p_exp e))) outs in
      let _ = print_endline (" ins: ") in
      let _ = ExpSet.iter (fun e -> print_endline ("    " ^ (p_exp e))) ins in*)
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