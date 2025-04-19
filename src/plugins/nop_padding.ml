(*
   This instrumentation plugin inserts two NOP instructions
   at the beginning of each basic block.
   It works for both 32-bit and 64-bit binary.
 *)

 module Instrumentation_Plugin = struct 

    open Ail_utils

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

    let insert_nop acc b il bmap =
      let open Type in
      let open Batteries in
      let module BU = BB_utils in
       let bil = Hashtbl.find bmap (b.bblock_name) in
       let bn = b.bblock_name in
       let i = List.nth bil 0 in
       let iloc = get_loc i in
       let iloc' = {iloc with loc_label = ""} in
       let addr' = iloc.loc_addr in
       let i1 = SingleInstr (CommonOP (Other NOP), iloc, None) in
       let i' = set_loc i iloc' in
       let open Instr_utils in
      set_update_fold i1 iloc acc
      |> set_update_fold i1 iloc
      |> sub_update_fold i' iloc i
 
 
     let gen_NOP_padding acc b il bmap =
       insert_nop acc b il bmap
 
 
    let insert_instrument_instrs il =
       let module IU = Instr_utils in
       !il_update
       |> IU.sort_il_update
       |> IU.update_instrs_infront il
 
 
    let instrument_bb bmap bl il acc =
       List.fold_left (fun acc b ->
                       gen_NOP_padding acc b il bmap) acc bl

    let instrument il fb_bbl bbl =
      let open Batteries in
      let module BU = BB_utils in
      let aux f bl acc =
        bl :: acc
      in
      let l = Hashtbl.fold aux fb_bbl [] in
 
      let bbl_sort = BU.bbl_sort bbl in
      let bmap = BU.bb_map bbl_sort il in
 
      let help acc bl = instrument_bb bmap bl il acc
      in
 
      il_update := List.flatten @@ List.fold_left help [] l;
 
      insert_instrument_instrs il
end
