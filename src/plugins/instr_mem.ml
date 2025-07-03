(**
  Instrumentation Plugin: Memory Read/Write
  Filename: instr_mem.ml

  It inserts nop before
    - memory reads
    - memory writes
*)

open Instrumentation

module PLUGIN = struct

  open Ail_utils

  let il_update = ref []

  let insert_nop i acc =
    begin
      let open Type in
      let open Batteries in
      let module BU = BB_utils in
      let iloc = get_loc i in
      let iloc' = {iloc with loc_label = ""} in
      let addr' = iloc.loc_addr in
      let i1 = SingleInstr (CommonOP (Other NOP), iloc, None) in
      let i' = set_loc i iloc' in
      let open Instr_utils in
      set_update_fold i1 iloc acc
    end

  let instrument_mem i t =
    begin
      let open Type in
      let module IT = Instr_template in
      let module IU = Instr_utils in
      il_update := (insert_nop i []) @ !il_update;
      IU.eliminate_label i
    end

  let instrument il fb_bbl bbl =
    print_endline "Enabling NOP padding before memory read and write";
    let open Batteries in
    let module IV = Instr_visitor in
    let module IU = Instr_utils in
    let module EU = ELF_utils in
    let module DU = Dataset_utils in
    let module BU = BB_utils in

    try
      begin
        IV.map_instr IU.is_mem_instr instrument_mem il
        |> IU.insert_instr_list BEFORE (List.flatten !il_update)
      end
    with _ ->
      begin
        print_string "Plugin Failed.\n";
        il
      end
end

let () =
  plugin := Some (module PLUGIN)