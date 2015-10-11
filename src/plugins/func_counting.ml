(* this instrumentation plugin inserts counting instructions at the beginning
   of a function.
 *)

module Func_counting = struct

    open Ail_utils



    let template_func_counting acc b il bmap =
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
     let open Instr_utils in
     set_update_fold i5 iloc acc
     |> set_update_fold i3 iloc
     |> set_update_fold i0 iloc
     |> sub_update_fold i' iloc i


    let gen_func_counting acc b il bmap =
      template_func_counting acc b il bmap


   let instrument_func bl il acc bmap =
      List.nth bl 0
      |> (fun b -> gen_func_counting acc b il bmap)


   let instrument il fb_bbl bbl =
     let open Batteries in
     let module IU = Instr_utils in
     let module BU = BB_utils in
     let aux _ bl acc =
       bl :: acc
     in
     print_endline "bb counting : ";
     print_int (Hashtbl.length fb_bbl);
     print_string "\n";
     let l = Hashtbl.fold aux fb_bbl [] in

     let bbl_sort = BU.bbl_sort bbl in
     let bmap = BU.bb_map bbl_sort il in

     let help acc bl =
       print_endline "done";
       instrument_func bl il acc bmap
     in

     List.flatten @@ List.fold_left help [] l
     |> IU.insert_instrument_instrs il

	(*
     let module P = Parallel in
     il_update :=  List.flatten @@
     P.pfold ~ncores: 12 ~concat: (@)  help l [];

      *)


end
