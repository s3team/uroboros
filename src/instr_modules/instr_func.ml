(**
  Instrumentation Plugin: Function Entry and Exit
  Filename: instr_func.ml
  Usage: assign target function labels in target_func_labels

  This instrumentation inserts two nops at:
    - the entry of target functions
    - the exit of target functions
*)
open Instrumentation

module PLUGIN = struct
  open Ail_utils
  open Type
  open Batteries
  open Instr_utils

  let insert_nop (inst : instr) :
      (instr * loc * instr_update * string) list list =
    let inst_loc = get_loc inst in
    let inst_loc_without_label = { inst_loc with loc_label = "" } in
    let nop = SingleInstr (CommonOP (Other NOP), inst_loc, None) in
    let nop_without_label =
      SingleInstr (CommonOP (Other NOP), inst_loc_without_label, None)
    in
    let inst' = set_loc inst inst_loc_without_label in

    set_update_fold nop_without_label inst_loc []
    |> set_update_fold nop inst_loc
    |> sub_update_fold inst' inst_loc inst

  let instrument_func_entry (bl : bblock list) (il : 'a)
      (bmap : (string, instr list) Hashtbl.t)
      (instrs_update : (instr * loc * instr_update * string) list list) :
      (instr * loc * instr_update * string) list list =
    let entry_block = List.nth bl 0 in
    let entry_block_insts = Hashtbl.find bmap entry_block.bblock_name in
    let entry_inst = List.nth entry_block_insts 0 in

    insert_nop entry_inst @ instrs_update

  let instrument_func_exit (bl : bblock list) (il : instr list)
      (bmap : (string, instr list) Hashtbl.t)
      (instrs_update : (instr * loc * instr_update * string) list list) :
      (instr * loc * instr_update * string) list list =
    (* Assumption: the last instruction within the function scope is the exit *)
    (* let module BU = BB_utils in
    let exit_block = List.nth bl (List.length bl - 1) in
    let exit_block_insts = Hashtbl.find bmap exit_block.bblock_name in
    let exit_inst =
      List.nth exit_block_insts (List.length exit_block_insts - 1)
    in
    let exit_loc = get_loc exit_inst in
    let exit_loc' = { exit_loc with loc_label = "" } in
    (* let addr' = exit_loc.loc_addr in *)
    let nop = SingleInstr (CommonOP (Other NOP), exit_loc, None) in
    let exit_inst' = set_loc exit_inst exit_loc' in

    let open Instr_utils in
    set_update_fold nop exit_loc instrs_update
    |> set_update_fold nop exit_loc
    |> sub_update_fold exit_inst' exit_loc exit_inst *)

    (* Assumption: 1. all rets are exits 2. call exit is exit *)
    let is_exit (inst : instr) : bool =
      match inst with
      (* ret *)
      | SingleInstr (p, _, _) when is_ret p -> true
      (* call exit *)
      | DoubleInstr (p, Symbol (CallDes func), _, _)
        when is_call p && func.func_name = "exit" ->
          true
      | _ -> false
    in

    let rec instrument_exit (inst_list : instr list)
        (instrs_update : (instr * loc * instr_update * string) list list) :
        (instr * loc * instr_update * string) list list =
      match inst_list with
      | [] -> instrs_update
      | inst :: t when is_exit inst ->
          insert_nop inst @ instrument_exit t instrs_update
      | _ :: t -> instrument_exit t instrs_update
    in

    List.fold_left
      (fun instrs_update block ->
        let instrs_update =
          Hashtbl.find bmap block.bblock_name |> fun block_inst_list ->
          instrument_exit block_inst_list instrs_update
        in
        instrs_update)
      instrs_update bl

  let instrument_func (il : instr list)
      (instrs_update : (instr * loc * instr_update * string) list list)
      (bmap : (string, instr list) Hashtbl.t) (bl : bblock list) :
      (instr * loc * instr_update * string) list list =
    instrument_func_entry bl il bmap instrs_update
    |> instrument_func_exit bl il bmap

  let instrument (il : instr list) (fb_bbl : ('a, bblock list) Hashtbl.t)
      (bbl : bblock list) : instr list =
    print_endline "Enabling NOP padding at function entry and exit";
    let module IV = Instr_visitor in
    let module IU = Instr_utils in
    let module EU = ELF_utils in
    let module DU = Dataset_utils in
    let module BU = BB_utils in
    let target_func_labels = [] in
    let l =
      try
        if List.is_empty target_func_labels then
          Hashtbl.fold (fun _ bl acc -> bl :: acc) fb_bbl []
        else
          List.fold_left
            (fun target_bbls target_func_label ->
              Hashtbl.find fb_bbl target_func_label :: target_bbls)
            [] target_func_labels
      with
      | Not_found ->
          Printf.printf
            "Function label not found in the function block table:\t";
          (* Print all function labels *)
          Hashtbl.iter
            (fun func_label _ -> Printf.printf "%s\t" func_label)
            fb_bbl;
          print_newline ();
          []
      | _ ->
          print_string "Querying Functions Failed.\n";
          []
    in

    let bbl_sort = BU.bbl_sort bbl in
    let bmap = BU.bb_map bbl_sort il in

    try
      List.fold_left
        (fun instrs_update bl -> instrument_func il instrs_update bmap bl)
        [] l
      |> List.flatten
      |> IU.insert_instrument_instrs il
    with
    | Failure msg ->
        Printf.printf "Caught an exception: %s\n" msg;
        il
    | _ ->
        print_string "Plugin Failed.\n";
        il
end

let () =
  plugin := Some (module PLUGIN)