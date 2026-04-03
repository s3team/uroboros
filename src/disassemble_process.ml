(* this module utilizes disassemble validator to disassemble the input binary *)

exception Pass_Validator

module Disam = struct
  open Arm_got_instr_analysis
  open Intel_got_instr_analysis
  open Adrp_instr_analysis
  open Semantic_analysis
  open Common_reassemble_symbol_get
  open Arm_reassemble_symbol_get
  open Arm_postprocess
  open Flow_insensitive_analysis
  open Reassemble_symbol_get
  open Visit
  open Disassemble_validator
  open Ail_parser
  open Utils
  open Ail_utils
  open Tags_utils
  open Type
  open Pp_print

  let disasm_skip f ba ea =
    let ba = string_of_int ba in
    let ea = string_of_int ea in

    ignore
      (Sys.command
         ("objdump -Dr -j .text " ^ f ^ " --start-address=" ^ ba
        ^ " --stop-address=" ^ ea ^ " > " ^ f ^ ".temp"));
    ignore (Sys.command ("python3 useless_func_del.py " ^ f));
    ignore
      (Sys.command
         ("cat " ^ f ^ ".disassemble | grep \"^ \" | cut -f1,3 > instrs.info"));

    ignore (Sys.command "python3 filter_nop.py");

    ignore (Sys.command "cut -f 1 instrs.info > text_mem.info");
    ()

  let get_userfuncs fl = List.filter (fun f -> f.is_lib = false) fl

  let create_re (arch : string) =
    match arch with
    | "intel" -> (new reassemble :> common_reassemble)
    | "thumb" | "arm" -> begin
        let re = (new arm_reassemble :> common_reassemble) in
        let _ = re#set_arch arch in
        re
      end
    | _ -> failwith "unsupported architecture for reassemble"

  let disassemble f funcs secs arch =
    let module TR = Time_Record in
    let re = create_re arch in
    let dis_valid = new dis_validator in
    let ail_parser = new ailParser in
    let il = ref [] in

    let total = ref 0.0 in
    let cond = ref false in
    (* less than 10 minutes *)

    while !cond = false && !total < 600.0 do
      let once = TR.get_utime () in
      let module FnU = Func_utils in
      let module EU = Elf_utils in
      let module IL = IL_utils in
      let module GA = GotAbs in
      let module D = DFA (GA) in
      let module AGA = ArmGotAbsWithValues in
      let module AD = DFA (AGA) in
      let module ADRPA = AdrpAbs in
      let module ADRPD = DFA (ADRPA) in
      let module AP = ArmPostprocess in
      ail_parser#set_funcs funcs;
      ail_parser#set_secs secs;
      ail_parser#set_arch arch;

      let instr_list = read_file "instrs.info" in
      (* remove_literal_pools is here *)
      timed "disassemble:process_instrs" (fun () -> ail_parser#process_instrs instr_list arch);
      timed "disassemble:instr_addrs_collect" (fun () -> re#instr_addrs_collect ail_parser#get_instrs);

      (* func_slicing *)
      let fl = timed "disassemble:get_funcs" (fun () -> ail_parser#get_funcs) in
      print_endline "2: disassembly validates";

      let il_init =
        if EU.elf_32 () && arch = "intel" then
          IL.adjust_esp ail_parser#get_instrs
        else ail_parser#get_instrs
      in

      let fl =
        if EU.elf_64 () && EU.elf_arm () then
          FnU.fix_fl_underapprox il_init fl
        else fl
      in

      (*
      List.iter
        (fun f ->
          Printf.printf "func: %s begin: 0x%x end: 0x%x\n"
            f.func_name f.func_begin_addr f.func_end_addr)
        fl;
      *)

      let rewrite (il : instr list) =
        if EU.elf_32 () && arch = "intel" then
          let func2cfg = FnU.func2cfg il fl in
          let _ =
            Hashtbl.iter
              (fun f cfg ->
                (* track GOT ptr in comment of result[0] NOP instr *)
                let _ = Hashtbl.remove GA.result 0 in
                let _ = D.flow_analysis f cfg in
                if Hashtbl.mem GA.result 0 then
                  let got_ptr = List.nth (Hashtbl.find GA.result 0) 0 in
                  let tags = get_tags got_ptr in
                  let got_reg = Hashtbl.find tags "got_reg" in
                  match got_reg with
                  | Str gr ->
                      if not (is_reassign gr cfg.il) then
                        (* perform flow-insensitive term rewriting *)
                        got_rewrite gr cfg.il GA.result
                  | _ -> ())
              func2cfg
          in
          GA.result
        else if EU.elf_32 () && (arch = "thumb" || arch = "arm") then
          let func2cfg_table = FnU.func2cfg il fl in
          let _ =
            Hashtbl.iter
              (fun f cfg ->
                let _ = AD.flow_analysis f cfg in
                ())
              func2cfg_table
          in
          AGA.result
        else if EU.elf_64 () && arch = "arm" then
          let func2cfg_table = FnU.func2cfg il fl in
          let _ =
            Hashtbl.iter
              (fun f cfg ->
                let _ = ADRPD.flow_analysis f cfg in
                ())
              func2cfg_table
          in
          ADRPA.result
        else Hashtbl.create 1
      in

      let arm_postprocess (ilist : instr list) : instr list =
        if arch = "thumb" then AP.adjust_thumb_function_pointer ilist else ilist
      in

      let rewriting_result = timed "disassemble:dfa" (fun () -> rewrite il_init) in

      il :=
        (if EU.elf_32 () && arch = "intel" then
           IL.perform_dfa_rewrites rewriting_result @@ il_init
           |> re#jmp_table_rewrite
           (* second call to got_rewrite account switch dests in CFGs *)
           (*|> (fun il -> FnU.perform_dfa_rewrites (got_rewrite il) @@ il )*)
         else if EU.elf_64 () && arch = "intel" then
           re#jmp_table_rewrite64 @@ il_init
         else begin IL.perform_dfa_rewrites rewriting_result @@ il_init
           (* The postprocess should be executed after symbolization completed.
            * Note that the symbolization-related functions such as vinst2 and v_exp2
            * are called by visit_type_infer_analysis and visit_heuristic_analysis.
            * If something wrong happens, AP functions might need to be moved to [ail.ml#post_process].
            *)
         end)
        |> re#visit_heuristic_analysis
        |> process_tags (* should be located after visit_heuristic_analysis *)
        |> re#adjust_loclabel |> re#adjust_jmpref
        |> re#add_func_label @@ get_userfuncs fl
        |> dis_valid#visit |> arm_postprocess;

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
