(*
   This instrumentation plugin is for 32-bit binary.
   It records the instruction addresses of an execution trace.
   The purpose is to record the execution trace and de-bloat
   the code for just that execution.
 *)

(* this instrumentation plugin inserts counting instructions at the beginning
   of a basic block. Only works for 32-bit binary.
 *)

module Instrumentation_Plugin = struct

    open Ail_utils

    let build_stub b =
      let open Type in
      let n = b.bblock_name in
      n ^ "_stub"


   let template_trace  acc b il bmap =
     let open Type in
     let open Batteries in
     let module BU = BB_utils in
      let bil = Hashtbl.find bmap (b.bblock_name) in
      let bn = b.bblock_name in
      let i = List.nth bil 0 in
      let iloc = get_loc i in
      let iloc' = {iloc with loc_label = ""} in
      let addr' = iloc.loc_addr in
      let sn = build_stub b in
      let i1 = DoubleInstr (StackOP PUSH, Reg (CommonReg ECX), iloc, None) in
      let i2 = TripleInstr (CommonOP (Assign MOVL), Reg (CommonReg ECX), Label "index", iloc', None) in
      let i3 = DoubleInstr (ControlOP (Loop LOOP), Label sn, iloc', None) in
      (* let i4 = TripleInstr (CommonOP (Assign MOVL), Reg (CommonReg ECX),
                            Const (Normal 0x400000), iloc', None) in *)
      let i5 = TripleInstr (CommonOP (Assign MOVL), Ptr (JmpTable_PLUS_S ("buf", CommonReg ECX, 4)),
                            Const (Normal addr'), {iloc' with loc_label = sn ^ ":"}, None) in
      let i6 = TripleInstr (CommonOP (Assign MOVL), Label "index", Reg (CommonReg ECX), iloc', None) in
      let i7 = DoubleInstr (StackOP POP, Reg (CommonReg ECX), iloc', None) in
      let i' = set_loc i iloc' in
      let open Instr_utils in
     set_update_fold i7 iloc acc
     |> set_update_fold i6 iloc
     |> set_update_fold i5 iloc
     (* |> set_update_fold i4 iloc *)
     |> set_update_fold i3 iloc
     |> set_update_fold i2 iloc
     |> set_update_fold i1 iloc
     |> sub_update_fold i' iloc i


    let gen_trace acc b il bmap =
      template_trace acc b il bmap


   let instrument_bb bmap bl il acc =
      List.fold_left (fun acc b ->
                      gen_trace acc b il bmap) acc bl


   let instrument il fb_bbl bbl =
     let open Batteries in 
     let module EU = ELF_utils in
     let module DU = Dataset_utils in
     let module BU = BB_utils in
     let module IU = Instr_utils in
     let aux f bl acc =
       bl :: acc
     in
     let l = Hashtbl.fold aux fb_bbl [] in

     let bbl_sort = BU.bbl_sort bbl in
     let bmap = BU.bb_map bbl_sort il in

     let help acc bl =
       (* print_endline "done"; *)
       instrument_bb bmap bl il acc
     in
     try
     if EU.elf_32 () then
      begin
        DU.insert_data "index" "0xff" "0x4" true "before";
        DU.insert_data "buf" "0x00" "0x1000000" false "after";
        List.flatten @@ List.fold_left help [] l
        |> IU.insert_instrument_instrs il
      end
      else 
        assert false
     with _ ->
      begin
        print_string "Plugin Failed: This plugin is for 32-bit binary, not for 64-bit.\n";
        il
      end

end
