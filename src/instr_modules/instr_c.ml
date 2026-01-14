(**
  Instrumentation Plugin: Custom C
  Filename: instr_c.ml

  It inserts user-defined C code at user-specified locations (addresses or labels).

  User-specified locations must be defined in the "instrument_locs.ins" file and
  placed in working directory (where uroboros.py is located), where each location,
  symbol or address, is in its own line. User-specified C code must be in the
  "generic_instr_fun()" function, which is defined in the "generic_instr_fun.c" file
  and placed in working directory. *)
open Instrumentation

module PLUGIN = struct

  open Ail_utils
  open Type

  let il_update = ref []

  let insert_call i acc =
    begin
      let open Type in
      let open Batteries in
      let module BU = BB_utils in
      let module EU = ELF_utils in

      let iloc = get_loc i in
      let call_seq = if EU.elf_32 () then
        (* surround call to handle caller-saved registers 32-bit *)
        [
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg EAX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg EDX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg ECX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_ControlOP (CALL)),
            Symbol (
              CallDes {
                func_name="generic_instr_fun";
                func_begin_addr=0;
                func_end_addr=0;
                is_lib=false;
              }
            ),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_CommonReg ECX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_CommonReg EDX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_CommonReg EAX)),
            iloc,
            None,
            Hashtbl.create 0
          );
        ]
      else
        [
        (* surround call to handle caller-saved registers for 64-bit *)
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg RAX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg RDX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg RCX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_SpecialReg R8)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_SpecialReg R9)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_SpecialReg R10)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_SpecialReg R11)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_ControlOP (CALL)),
            Symbol (
              CallDes {
                func_name="generic_instr_fun";
                func_begin_addr=0;
                func_end_addr=0;
                is_lib=false;
              }
            ),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_SpecialReg R11)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_SpecialReg R10)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_SpecialReg R9)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_SpecialReg R8)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_CommonReg RCX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_CommonReg RDX)),
            iloc,
            None,
            Hashtbl.create 0
          );
          DoubleInstr (
            Intel_OP (Intel_StackOP (POP)),
            Reg (Intel_Reg (Intel_CommonReg RAX)),
            iloc,
            None,
            Hashtbl.create 0
          );
        ]
      in
      let open Instr_utils in
      List.fold_left (fun acc i -> set_update_fold i iloc acc) acc (List.rev call_seq);
    end


  let instrument_call i =
    begin
      let open Type in
      let module IT = Instr_template in
      let module IU = Instr_utils in

      il_update := (insert_call i []) @ !il_update;
      IU.eliminate_label i
    end

  let write_pass () =
    (* specify this pass is ran for reassembly
     * since the object file needs to be passed *)
    let oc = open_out "instrument_pass.info" in
    output_string oc "pass_7_custom_c\n";
    close_out oc

  let instrument il fb_bbl bbl =
    let open Batteries in
    let module IV = Instr_visitor in
    let module IU = Instr_utils in
    let module EU = ELF_utils in
    let module DU = Dataset_utils in
    let module BU = BB_utils in

    if EU.elf_32 () then
      ignore (Sys.command ("gcc -no-pie -c generic_instr_fun.c -m32"))
    else
      ignore (Sys.command ("gcc -no-pie -c generic_instr_fun.c"));

    try
      begin
        write_pass ();
        let read_lines filename =
          File.with_file_in filename (fun input ->
            List.of_enum (IO.lines_of input)
          )
        in
        let filelines = read_lines "instrument_locs.ins" in
        IV.map_instr' IU.is_addr_or_label instrument_call il filelines
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
