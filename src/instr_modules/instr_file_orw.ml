(**
  Instrumentation Plugin: File I/O Read/Write
  Filename: instr_file_orw.ml

  This instrumentation plugin inserts two nops before call to
    - fopen
    - fread
    - fwrite
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
      |> set_update_fold i1 iloc
      |> sub_update_fold i' iloc i
    end


  let instrument_f_call i t =
    begin
      let open Type in
      let module IT = Instr_template in
      let module IU = Instr_utils in

      il_update := (insert_nop i []) @ !il_update;
      IU.eliminate_label i
    end

  let is_f_call instr =
    let open Type in
    let open Pp_print in

    match (pp_print_instr instr) with
    | "call fread" | "call fwrite" | "call fopen"
    | "callq fread" | "callq fwrite" | "callq fopen" -> Some DIRECT_CALL
    | _ -> None

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
        IV.map_instr is_f_call instrument_f_call il
        |> IU.insert_instr_list BEFORE (List.flatten !il_update)
      end
    with _ ->
      begin
        print_string "Plugin Failed: This plugin is for 32-bit binary, not for 64-bit.\n";
        il
      end
end

let () =
  plugin := Some (module PLUGIN)