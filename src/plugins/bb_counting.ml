(*
   This instrumentation plugin is for 32-bit binary.
   It inserts counting instructions
   at the beginning of each basic block. 
 *)

module Instrumentation_Plugin = struct

    open Ail_utils

    (* il update list  *)
    let il_update = ref []


    let set_update i =
      let open Instr_utils in
      let l = get_loc i in
      il_update := (i, l, INSERT, "") :: !il_update

   let set_update_fold i l acc =
     let open Instr_utils in
     [(i, l, INSERT, "")] :: acc

   let sub_update_fold i l i' acc =
      let open Pp_print in
      let open Instr_utils in
      let i_s = pp_print_instr i' in
      [(i, l, SUB, i_s)] :: acc

   (*

basic block counting:
counts basic blocks number by inplementing this :


      SingleInstr (CommonOP (Assign LAHF), iloc', None) ;
      DoubleInstr (CommonOP (Arithm INC), Label ("(counter)"), iloc', None)
      SingleInstr (CommonOP (Assign SAHF), iloc', None)

 *)


   let template_BB_counting acc b il bmap =
     let open Type in
     let open Batteries in
     let module BU = BB_utils in
      let bil = Hashtbl.find bmap (b.bblock_name) in
      let bn = b.bblock_name in
      let i = List.nth bil 0 in
      let iloc = get_loc i in
      let iloc' = {iloc with loc_label = ""} in
      let addr' = iloc.loc_addr in
      let i0 = SingleInstr (StackOP (PUSHF), iloc, None) in
      let i3 = DoubleInstr (CommonOP (Arithm INCL), Label ("(counter)"), iloc', None) in
      let i5 = SingleInstr (StackOP (POPF), iloc', None) in
      let i' = set_loc i iloc' in
     set_update_fold i5 iloc acc
     |> set_update_fold i3 iloc
     |> set_update_fold i3 iloc
     |> set_update_fold i3 iloc
     |> set_update_fold i3 iloc
     |> set_update_fold i3 iloc
     |> set_update_fold i3 iloc
     |> set_update_fold i3 iloc
     |> set_update_fold i3 iloc
     |> set_update_fold i3 iloc
     |> set_update_fold i3 iloc
     |> set_update_fold i0 iloc
     |> sub_update_fold i' iloc i


   let template_BB_counting_opt acc b il bmap =
     let build_stub b =
       let open Type in
       let n = b.bblock_name in
       n ^ "_stub" in
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
                            Const (Normal 0xffffffff), iloc', None) in *)
      let i5 = TripleInstr (CommonOP (Assign MOVL), Label "index", Reg (CommonReg ECX),
                            {iloc' with loc_label = sn ^ ":"}, None) in
      let i6 = DoubleInstr (StackOP POP, Reg (CommonReg ECX), iloc', None) in
      let i' = set_loc i iloc' in
      let open Instr_utils in
     set_update_fold i6 iloc acc
     |> set_update_fold i5 iloc
     (* |> set_update_fold i4 iloc *)
     |> set_update_fold i3 iloc
     |> set_update_fold i2 iloc
     |> set_update_fold i1 iloc
     |> sub_update_fold i' iloc i



    let gen_BB_counting acc b il bmap =
      template_BB_counting_opt acc b il bmap


   let insert_instrument_instrs il =
      let module IU = Instr_utils in
      !il_update
      |> IU.sort_il_update
      |> IU.update_instrs_infront il


   let instrument_bb bmap bl il acc =
      List.fold_left (fun acc b ->
                      gen_BB_counting acc b il bmap) acc bl


   let instrument il fb_bbl bbl =
     let open Batteries in
     let module EU = ELF_utils in
     let module DU = Dataset_utils in
     let module BU = BB_utils in
     let aux f bl acc =
       bl :: acc
     in
     (* print_endline "bb counting : ";
     print_int (Hashtbl.length fb_bbl);
     print_string "\n"; *)
     let l = Hashtbl.fold aux fb_bbl [] in

     let bbl_sort = BU.bbl_sort bbl in
     let bmap = BU.bb_map bbl_sort il in

     let help acc bl =
       (* print_endline "done1"; *)
       instrument_bb bmap bl il acc
     in
     try
        if EU.elf_32 () then
        begin
            DU.insert_data "index" "0xff" "0x4" true "before";
            il_update := List.flatten @@ List.fold_left help [] l;
            insert_instrument_instrs il
        end
        else
          assert false
     with  _ ->
      begin
        print_string "Plugin Failed: This plugin is for 32-bit binary, not for 64-bit.\n";
        il
      end

end
