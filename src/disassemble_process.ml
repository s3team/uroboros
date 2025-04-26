(* this module utilizes disassemble validator to disassemble the input binary *)

exception Pass_Validator

module Disam = struct

    open Semantic_analysis
    open Reassemble_symbol_get
    open Disassemble_validator
    open Ail_parser
    open Ail_utils
    open Type

  let disasm_skip f ba ea =
    let ba = string_of_int ba in
    let ea = string_of_int ea in

    ignore(Sys.command
      ("objdump -Dr -j .text " ^ f ^ " --start-address=" ^ ba
     ^ " --stop-address=" ^ ea ^ " > " ^ f ^ ".temp"));
    ignore(Sys.command ("python3 useless_func_del.py " ^ f));
    ignore(Sys.command
      ("cat " ^ f ^ ".disassemble | grep \"^ \" | cut -f1,3 > instrs.info"));

    ignore(Sys.command "python3 filter_nop.py");

    ignore(Sys.command "cut -f 1 instrs.info > text_mem.info");
    ()

  let get_userfuncs fl = List.filter (fun f -> f.is_lib = false) fl

  let disassemble f funcs secs arch =
    let module TR = Time_Record in
    let re = new reassemble in
    let dis_valid = new dis_validator in
    let ail_parser = new ailParser in
    let il = ref [] in

    let total = ref 0.0 in
    let cond = ref false in
    (* less than 10 minutes *)

    while !cond = false && !total < 600.0 do
      let once = TR.get_utime () in
      let module FnU = Func_utils in
      let module EU = ELF_utils in
      let module GA = GotAbs in
      let module D = DFA(GA) in
      ail_parser#set_funcs funcs;
      ail_parser#set_secs secs;

      let instr_list = read_file "instrs.info" in
      ail_parser#process_instrs instr_list arch;

      let fl = ail_parser#get_funcs in
      print_endline "2: disassembly validates --> ";

      (*let _ = List.iter (
        fun f ->
          let name = f.func_name in
          let addr = f.func_begin_addr in
          let ea = f.func_end_addr in
          let ba = dec_hex addr in
          let ea = dec_hex ea in
          print_endline (name ^ ", " ^ ba ^ ", " ^ ea);
      ) fl in*)

      if EU.elf_32 () && arch <> "arm" then
        let func2cfg_table = FnU.func2cfg ail_parser#get_instrs fl in
        let _ = Hashtbl.iter (
          fun f cfg ->
            let _ = D.flow_analysis cfg in
            ()
        ) func2cfg_table in ()
      else ();
      

      il :=
        if EU.elf_32 () && arch <> "arm" then
          begin
            FnU.replace_got_ref GA.result @@ ail_parser#get_instrs
            |> re#visit_heuristic_analysis
          end
        else
          begin
            re#visit_heuristic_analysis @@ ail_parser#get_instrs
          end
        |> re#adjust_loclabel |> re#adjust_jmpref
        |> re#add_func_label @@ get_userfuncs fl
        |> dis_valid#visit;

      let adjust_list = dis_valid#trim_results in
      if List.length adjust_list != 0 then (
        print_endline "\tdisassembly error found!";
        (* objdump cannot handle multiple "gaps" *)
        let ba, ea = List.nth adjust_list 0 in
        disasm_skip f ba ea;
        total := !total +. TR.elapsed once)
      else cond := true
    done;
    print_endline "\tno disassembly error detects";
    let funcs = ail_parser#get_funcs in
    (!il, funcs, re)
end
