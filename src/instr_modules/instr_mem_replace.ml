(**
  Instrumentation Plugin: Memcpy Replacement
  Filename: instr_mem_replace.ml

  It replaces memcpy with user-defined memcpy_user. 

  To use, place the user-defined memcpy_user function
  in the file generic_instr_fun.c, which should be placed
  in the AIL/src (root) folder. *)

open Instrumentation

module PLUGIN = struct

  open Ail_utils

  type mem_operation_type =
    | MEMCPY

  let il_update = ref []

  let replace_call i acc =
    begin
      let open Type in
      let open Batteries in
      let module BU = BB_utils in
      let module EU = ELF_utils in
      let iloc = get_loc i in
      let new_instr =
        DoubleInstr (
          Intel_OP (Intel_ControlOP (CALL)),
          Symbol (
            CallDes {
              func_name="memcpy_user";
              func_begin_addr=0;
              func_end_addr=0;
              is_lib=false;
            }
          ),
          iloc,
          None)
      in
      let open Instr_utils in
      set_update_fold new_instr iloc [];
    end

  let instrument_call i i_type =
    begin
      let open Type in
      let module IT = Instr_template in
      let module IU = Instr_utils in
      il_update := (replace_call i []) @ !il_update;
      IU.eliminate_label i
    end
  
  let is_memcpy inst =
    begin
      let open Pp_print in
      match pp_print_instr inst with
      | "call memcpy" | "callq memcpy" -> Some MEMCPY
      | x -> None
    end

  let write_pass () =  
    (* specify this pass is ran for reassembly
     * since the object file needs to be passed *)
    let oc = open_out "instrument_pass.info" in 
    output_string oc "pass_memcpy_replace\n";
    close_out oc

  let instrument il fb_bbl bbl =
    let open Batteries in
    let module IV = Instr_visitor in
    let module IU = Instr_utils in
    let module EU = ELF_utils in
    let module DU = Dataset_utils in
    let module BU = BB_utils in

    write_pass ();
    try
      begin
        IV.map_instr is_memcpy instrument_call il
        |> IU.insert_instr_list INPLACE !il_update
      end
    with _ ->
      begin
        print_string "Plugin Failed.\n";
        il
      end
end

let () =
  plugin := Some (module M:PLUGIN)