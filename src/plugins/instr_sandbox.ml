(*
   This instrumentation plugin inserts sandboxing instruction right before
   indirect control flow transfers, and as a result, control transfers are
   restricted to predefined ranges (sandboxing).
   Similar methods are used for SFI (Software Fault Isolation).

   This implementation is to demo the instrumentation facility.
   The inserted sandboxing does not actually restrict control transfers
   as performing AND 0xFFFFFFFF on an address results in the same address,
   but it might affect the flags and cause unexpected problems.

   This plugin is for 32-bit binary.
   To sandbox 64-bit binary, 0xFFFFFFFFFFFFFFFF should be used instead
   of 0xFFFFFFFF.
 *)

module Instrumentation_Plugin = struct

    open Ail_utils

    (* il update list  *)
    let il_update = ref []


    let set_update i =
      let open Instr_utils in
      let l = get_loc i in
      il_update := (i, l, INSERT, "") :: !il_update


    (* remember we have to remove the label on the jmp operation, and move it
    to the sandboxing instruction *)
    let eliminate_label i =
      update_label i ""


    let sandboxing_template_indirect loc e =
      let open Type in
      let e =
      match e with
      | Symbol (StarDes e') -> e'
      | _ -> e
      in
      TripleInstr (CommonOP (Logic ANDL), e, Const (Normal 0xffffffff), loc, None)
      |> set_update


    let sandboxing_template_ret loc =
      let open Type in
      TripleInstr (CommonOP (Logic ANDL), Ptr (UnOP (StackReg ESP)), Const (Normal 0xffffffff), loc, None)
      |> set_update


    let gen_sandboxing_ret i =
      get_loc i |> sandboxing_template_ret;
      eliminate_label i


    let gen_sandboxing_indirect i =
      let l = get_loc i in
      let e = get_exp_1 i in
      sandboxing_template_indirect l e;
      eliminate_label i


   let insert_sandboxing_instrs il =
      let module IU = Instr_utils in
      !il_update
      |> IU.sort_il_update
      |> IU.update_instrs_infront il


    let instrument il fb_bbl bbl =
      let open Type in
      let module EU = ELF_utils in
      let module IV = Instr_visitor in
      let module IU = Instr_utils in
      try
      if EU.elf_32 () then
        begin
          let visit i t =
            match t with
            | RET_TYPE ->
              gen_sandboxing_ret i
            | INDIRECT ->
              gen_sandboxing_indirect i
            | _ -> i
          in
          IV.map_instr IU.is_jmp_instr visit il
          |> insert_sandboxing_instrs
        end
      else
        assert false
      with _ ->
      begin
        print_string "Plugin Failed: This plugin is for 32-bit binary, not for 64-bit.\n";
        il
       end


end
