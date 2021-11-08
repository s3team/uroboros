(*
   This instrumentation plugin is for 32-bit binary.
   It traces memory write operations. 
 *)

module Instrumentation_Plugin = struct

    open Ail_utils

    let il_update = ref []


    let process i t =
      let open Type in
      let module IT = Instr_template in
      let module IU = Instr_utils in
      match t with
      | SINGLE_WRITE
      | DOUBLE_WRITE
      | TRIPLE_WRITE ->
         il_update :=
           (IU.get_loc i
            |> IT.gen_logging_instrs i) @ !il_update;
         IU.eliminate_label i
      | _ -> i


    let instrument il fb_bbl bbl =
      let open Type in
      let module EU = ELF_utils in
      let module DU = Dataset_utils in
      let module IV = Instr_visitor in
      let module IU = Instr_utils in
      let visit i t =
        process i t
      in
      try
      if EU.elf_32 () then
        begin
          DU.insert_data "index" "0xff" "0x4" true "before";
          DU.insert_data "buf" "0x00" "0x1000000" false "after";
          IV.map_instr IU.is_mem_write_instr visit il
          |> IU.insert_instr_list BEFORE !il_update
        end
      else
        assert false
      with _ ->
        begin
          print_string "Plugin Failed: This plugin is for 32-bit binary, not for 64-bit.\n";
          il
        end

      


end
