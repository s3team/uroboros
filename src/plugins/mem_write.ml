(*
   this instrumentation plugin trace memory write operations
*)

module Mem_write = struct

    open Ail_utils

    let il_update = ref []


    let instrument i t =
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


    let process il =
      let open Type in
      let module IV = Instr_visitor in
      let module IU = Instr_utils in
      let visit i t =
        instrument i t
      in
      IV.map_instr IU.is_mem_write_instr visit il
      |> IU.insert_instr_list BEFORE !il_update


end
