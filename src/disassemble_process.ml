(* this module utilizes disassemble validator to disassemble the input binary *)


exception Pass_Validator;;

module Disam = struct

    open Reassemble_symbol_get
    open Disassemble_validator
    open Ail_parser
    open Ail_utils
    open Type


  let disasm_skip f ba ea =
    let ba = string_of_int ba in
    let ea = string_of_int ea in

    Sys.command("objdump -Dr -j .text "^f^" --start-address="^ba^" --stop-address="^ea^" > "^f^".temp");
    Sys.command("python useless_func_del.py "^f);
    Sys.command("cat "^f^".disassemble | grep \"^ \" | cut -f1,3 > instrs.info");

    Sys.command("python filter_nop.py");

	Sys.command("cut -f 1 instrs.info > text_mem.info");
    ()


  let get_userfuncs fl =
    List.filter (fun f -> f.is_lib=false) fl


  let disassemble f funcs secs =
    let module TR = Time_Record in
    let re = new reassemble in
    let dis_valid = new dis_validator in
    let ail_parser = new ailParser in
    let il = ref [] in

    let total = ref 0.0 in
    let cond = ref false in
    (* less than 10 minutes *)

    while (!cond = false) && (!total < 600.0) do

      let once = TR.get_utime() in
      ail_parser#set_funcs(funcs);
	  ail_parser#set_secs(secs);
      ail_parser#processInstrs @@ read_file "instrs.info";

      let fl = ail_parser#get_funcs in
	  print_endline "2: disassembly validates --> ";


      il :=
        re#visit_heuristic_analysis @@ ail_parser#get_instrs
        |> re#adjust_loclabel
        |> re#adjust_jmpref
        |> re#add_func_label @@ get_userfuncs fl
        |> dis_valid#visit;

	  let adjust_list = dis_valid#trim_results in
	  if List.length adjust_list != 0 then
        begin
	      print_endline "disassembly error found!";
          (* objdjump cannot handle multiple "gaps" *)
          let (ba,ea) = List.nth adjust_list 0 in
          disasm_skip f ba ea;
          total := !total +. TR.elapsed once
        end
	  else
        cond := true;
    done;

	print_endline "no disassembly error detects";
    let funcs = ail_parser#get_funcs in
    (!il, funcs, re)

end
