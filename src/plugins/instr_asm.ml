(**
  Instrumentation Plugin: Custom Assembly
  Filename: instr_asm.ml

  It inserts user-defined assembly at user-specified locations (addresses or labels).

  User-specified locations must be defined in the "instrument_locs.ins" file and 
  placed in working directory (where uroboros.py is located), where each location, 
  symbol or address, is in its own line. User-specified assembly must be defined 
  in the "generic_instr_asm.asm" file and placed in working directory as well. *)

open Instrumentation

module PLUGIN = struct

  open Ail_utils
  open Ail_parser
  open Pp_print
  open Type

  let il_update = ref []
  let il_usr = ref []

  let insert_asm instr acc =
    begin
      let open Type in
      let open Batteries in
      let module BU = BB_utils in

      let iloc = get_loc instr in
      let open Instr_utils in
      List.fold_left (fun acc i -> set_update_fold i iloc acc) acc !il_usr;
    end

  let instrument_asm i =
    begin
      let open Type in
      let module IT = Instr_template in
      let module IU = Instr_utils in

      il_update := (insert_asm i []) @ !il_update;
      IU.eliminate_label i
    end

  let write_pass () =
    (* specify this pass is ran for reassembly
     * since the object file needs to be passed *)
    let oc = open_out "instrument_pass.info" in
    output_string oc "pass_56_custom_asm\n";
    close_out oc

  let instrument
      (il : instr list)
      (fb_bbl : (string, bblock list) Hashtbl.t)
      (bbl : bblock list)
    : instr list =
    let open Batteries in
    let module IV = Instr_visitor in
    let module IU = Instr_utils in
    let module EU = ELF_utils in
    let module DU = Dataset_utils in
    let module BU = BB_utils in

    try
      begin
        write_pass ();
        let read_lines filename =
          File.with_file_in filename (fun input ->
            List.of_enum (IO.lines_of input)
          )
        in

        (* convert raw assembly to our internal representation *)
        let usr_asm = read_lines "generic_instr_asm.asm" in
        let ail_parser = new ailParser in
        let _ = ail_parser#process_asms usr_asm "intel" in
        il_usr := ail_parser#get_instrs;

        (* handle instrumentation *)
        let filelines = read_lines "instrument_locs.ins" in
        IV.map_instr' IU.is_addr_or_label instrument_asm il filelines
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