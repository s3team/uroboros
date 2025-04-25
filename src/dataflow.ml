open Pp_print
open Ail_utils
open Type

module ExpOrd = struct
  type t = exp

  let compare i1 i2 = String.compare (p_exp i1) (p_exp i2)
end

module ExpSet = Set.Make (ExpOrd)

let init_got () =
  let split_by_space s = Str.split (Str.regexp " ") s in
  let filelines : string list = read_lines "pic_secs.info" in
  let info = List.nth filelines 0 in
  let info_str = split_by_space info in
  let got_addr_str = "0x" ^ List.nth info_str 1 in
  int_of_string got_addr_str

let process_instr i ins got_addr =
  match i with
  | DoubleInstr
      ( Intel_OP (Intel_ControlOP CALL),
        Symbol (StarDes (Ptr (BinOP_PLUS (reg, const)))),
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig1: " ^ (pp_print_instr' i)) in*)
      DoubleInstr
        ( Intel_OP (Intel_ControlOP CALL),
          Symbol (StarDes (Const (Point got_plus_offset))),
          loc,
          prefix )
  | DoubleInstr
      ( Intel_OP (Intel_ControlOP CALL),
        Symbol (StarDes (Ptr (BinOP_MINUS (reg, const)))),
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr - const in
      (*let _ = print_endline ("orig2: " ^ (pp_print_instr' i)) in*)
      DoubleInstr
        ( Intel_OP (Intel_ControlOP CALL),
          Symbol (StarDes (Const (Point got_plus_offset))),
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
        dest_ptr,
        Ptr (BinOP_MINUS (reg, const)),
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr - const in
      (*let _ = print_endline ("orig3: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
          dest_ptr,
          Const (Point got_plus_offset),
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
        dest_ptr,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig4: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
          dest_ptr,
          Const (Point got_plus_offset),
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
        dest_ptr,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig5: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
          dest_ptr,
          Const (Point got_plus_offset),
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
        Ptr (BinOP_PLUS (reg, const)),
        dest_ptr,
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig6: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVB)),
          Const (Point got_plus_offset),
          dest_ptr,
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
        dest_ptr,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          dest_ptr,
          Const (Point got_plus_offset),
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
        Ptr (BinOP_PLUS (reg, const)),
        dest_ptr,
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
          Const (Point got_plus_offset),
          dest_ptr,
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
        dest_ptr,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
          dest_ptr,
          Const (Point got_plus_offset),
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
        Ptr (BinOP_PLUS (reg, const)),
        dest_ptr,
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
          Const (Point got_plus_offset),
          dest_ptr,
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
        Ptr (BinOP_PLUS (reg, const)),
        cmp_to,
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          Const (Point got_plus_offset),
          cmp_to,
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
        cmp_to,
        Ptr (BinOP_PLUS (reg, const)),
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          cmp_to,
          Const (Point got_plus_offset),
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
        Ptr (BinOP_MINUS (reg, const)),
        cmp_to,
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          Const (Point got_plus_offset),
          cmp_to,
          loc,
          prefix )
  | TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
        cmp_to,
        Ptr (BinOP_MINUS (reg, const)),
        loc,
        prefix )
    when ExpSet.mem (Reg reg) ins ->
      let got_plus_offset = got_addr + const in
      (*let _ = print_endline ("orig7: " ^ (pp_print_instr' i)) in*)
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Compare CMP)),
          cmp_to,
          Const (Point got_plus_offset),
          loc,
          prefix )
  | _ -> i

let apply_flow_function i ins =
  let outs = ins in
  match i with
  | TripleInstr (p, e1, e2, _, _) -> (
      match (p, e1, e2) with
      | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Label l, Reg r
      | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Reg r, Label l ->
          if contains l "$_GLOBAL_OFFSET_TABLE_" then (
            (* gen *)
            (*print_endline ("gen: " ^ (p_exp (Reg r)) ^ ", " ^ (pp_print_instr' i));*)
            ExpSet.add (Reg r) outs)
          else outs
      | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), e1, e2 ->
        if ExpSet.mem e1 outs then (
          (* kill *)
          (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
          ExpSet.remove e1 outs)
        else outs
      | Intel_OP (Intel_CommonOP (Intel_Arithm SUB)), e1, e2 ->
        if ExpSet.mem e1 outs then (
          (* kill *)
          (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
          ExpSet.remove e1 outs)
        else outs
      | Intel_OP (Intel_CommonOP (Intel_Assign MOV)), e1, e2 ->
          (* e1: dest, e2: src *)
          if ExpSet.mem e2 outs then (
            (* gen *)
            match e1 with
            | Reg _ | Ptr (UnOP _) | Ptr (BinOP_MINUS (Intel_Reg (Intel_StackReg _), _)) | Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg _), _)) ->
              begin
                (*print_endline ("gen: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
                ExpSet.add e1 outs
              end
            | _ -> outs
          )
          else if ExpSet.mem e1 outs then
            (* kill *)
            begin
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            end
          else outs
      | Intel_OP (Intel_CommonOP (Intel_Assign MOVZBL)), e1, e2 ->
          (* e1: dest, e2: src *)
          if ExpSet.mem e2 outs then (
            (* gen *)
            match e1 with
            | Reg _ | Ptr (UnOP _) | Ptr (BinOP_MINUS (Intel_Reg (Intel_StackReg _), _)) | Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg _), _)) ->
              begin
                (*print_endline ("gen: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
                ExpSet.add e1 outs
              end
            | _ -> outs
          )
          else if ExpSet.mem e1 outs then
            (* kill *)
            begin
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            end
          else outs
      | Intel_OP (Intel_CommonOP (Intel_Logic XOR)), e1, e2 ->
          (* e1: dest, e2: src *)
          if ExpSet.mem e1 outs then
            (* kill *)
            begin
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            end
          else outs
      | Intel_OP (Intel_CommonOP (Intel_Logic OR)), e1, e2 ->
          (* e1: dest, e2: src *)
          if ExpSet.mem e1 outs then
            (* kill *)
            begin
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            end
          else outs
      | Intel_OP (Intel_CommonOP (Intel_Rol SHR)), e1, e2 ->
          (* e1: dest, e2: src *)
          if ExpSet.mem e1 outs then
            (* kill *)
            begin
            (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
            ExpSet.remove e1 outs
            end
          else outs
      | Intel_OP (Intel_CommonOP (Intel_Rol SHL)), e1, e2 ->
          (* e1: dest, e2: src *)
          if ExpSet.mem e1 outs then
            (* kill *)
            begin
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            end
          else outs
      | Intel_OP (Intel_CommonOP (Intel_Logic AND)), e1, e2 ->
          (* e1: dest, e2: src *)
          if ExpSet.mem e1 outs then
            (* kill *)
            begin
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            end
          else outs
      | Intel_OP (Intel_CommonOP (Intel_Logic NOT)), e1, e2 ->
          (* e1: dest, e2: src *)
          if ExpSet.mem e1 outs then
            (* kill *)
            begin
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            end
          else outs
      | Intel_OP (Intel_CommonOP (Intel_Assign LEA)), e1, e2 ->
          if ExpSet.mem e2 outs then (
            (* gen *)
            match e1 with
            | Reg _ | Ptr (UnOP _) | Ptr (BinOP_MINUS (Intel_Reg (Intel_StackReg _), _)) | Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg _), _)) ->
              begin
                (*print_endline ("gen: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
                ExpSet.add e1 outs
              end
            | _ -> outs
          )
          else if ExpSet.mem e1 outs then
            (* kill *)
            begin
              (*print_endline ("kill: " ^ (p_exp e1) ^ ", " ^ (pp_print_instr' i));*)
              ExpSet.remove e1 outs
            end
          else outs
      | _ -> outs)
  | _ -> outs

let got_flow (pred_cfg : (instr option, instr option list) Hashtbl.t)
    (succ_cfg : (instr option, instr option list) Hashtbl.t) (il : instr list)
    (addr2newi : (int, instr) Hashtbl.t) =
  let in_map = Hashtbl.create 10 in
  let out_map : (instr, ExpSet.t) Hashtbl.t = Hashtbl.create 10 in
  let _ =
    List.iter
      (fun i ->
        Hashtbl.add in_map i ExpSet.empty;
        Hashtbl.add out_map i ExpSet.empty)
      il
  in
  let worklist = Queue.create () in
  let _ = List.iter (fun i -> Queue.push i worklist) il in
  while not (Queue.is_empty worklist) do
    let i : instr = Queue.pop worklist in
    let preds : instr option list =
      try Hashtbl.find pred_cfg (Some i) with Not_found -> []
    in
    let ins =
      List.fold_left
        (fun acc (p_op : instr option) ->
          match p_op with
          | Some p ->
              let out = Hashtbl.find out_map p in
              ExpSet.union acc out
          | None -> acc)
        ExpSet.empty preds
    in
    let _ = Hashtbl.replace in_map i ins in
    let got_addr = init_got () in 
    let newi = process_instr i ins got_addr in
    if pp_print_instr newi <> pp_print_instr i then
      Hashtbl.replace addr2newi (get_loc i).loc_addr newi;
    let old_outs = Hashtbl.find out_map i in
    let outs = apply_flow_function i ins in

    (* debug log *)
    (*let _ = print_endline ("> " ^ (pp_print_instr' i)) in
    let _ = print_endline (" outs: ") in
    let _ = ExpSet.iter (fun e -> print_endline ("    " ^ (p_exp e))) outs in
    let _ = print_endline (" ins: ") in
    let _ = ExpSet.iter (fun e -> print_endline ("    " ^ (p_exp e))) ins in*)

    if not (ExpSet.equal old_outs outs) then (
      Hashtbl.replace out_map i outs;
      (*let succs = Hashtbl.find succ_cfg (Some i) in*)
      let succs = 
        match Hashtbl.find_opt succ_cfg (Some i) with
        | Some v -> v
        | None -> []
      in
      List.iter
        (fun i_op ->
          match i_op with Some i -> Queue.push i worklist | None -> ())
        succs)
  done
