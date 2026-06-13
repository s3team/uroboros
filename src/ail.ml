open Type
open Ail_parser
open Pp_print
open Reassemble_symbol_get
open Cfg
open Cg
open Ail_utils
open Utils

class ail =
  object (self)
    val mutable funcs : func list = []
    val mutable secs : section list = []

    method sections : unit =
      let filelines = read_file "sections.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x" ^ List.nth items 1)
        and size = int_of_string ("0x" ^ List.nth items 3)
        and secname = List.nth items 0 in
        secs <-
          { sec_name = secname; sec_begin_addr = addr; sec_size = size } :: secs
      in
      List.iter help filelines

    method externfuncs : unit =
      let filelines = read_file "externfuncs.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x" ^ List.nth items 0)
        and func = List.nth items 1 in
        funcs <-
          {
            func_name = func;
            func_begin_addr = addr;
            func_end_addr = 0;
            is_lib = true;
          }
          :: funcs
      in
      List.iter help filelines

    method userfuncs : unit =
      let filelines = read_file "userfuncs.info"
      and help l =
        if contains ~str:l ~sub:"-0x" || contains ~str:l ~sub:"+0x" then ()
        else begin
          let items = Str.split (Str.regexp " +") l in
          let addr = int_of_string ("0x" ^ List.nth items 0)
          and funname = List.nth items 1 in
          let len = String.length funname in
          let funname' = String.sub funname 1 (len - 3) in
          if contains ~str:funname' ~sub:"@@" then
            let fn = List.nth (Str.split (Str.regexp_string "@@") funname') 0 in
            funcs <-
              {
                func_name = fn;
                func_begin_addr = addr;
                func_end_addr = 0;
                is_lib = false;
              }
              :: funcs
          else
            funcs <-
              {
                func_name = funname';
                func_begin_addr = addr;
                func_end_addr = 0;
                is_lib = false;
              }
              :: funcs
        end
      in
      List.iter help filelines

    method ehframe_dump : unit =
      ignore (Sys.command "cat eh_frame.data >> final.s")

    method excpt_tbl_dump : unit =
      ignore (Sys.command "cat gcc_exception_table.data >> final.s")

    method post_process (f : string) (arch : string) : unit =
      ignore (Sys.command ("python3 post_process.py " ^ arch));
      ignore (Sys.command "python3 post_process_lib.py")
    (* self#ehframe_dump;
         self#excpt_tbl_dump; *)

    method pre_process (f : string) (arch : string) : unit =
      ignore (Sys.command "python3 pre_process.py");
      ignore (Sys.command ("python3 main_discover.py " ^ " " ^ f ^ " " ^ arch))

    method instr_process (f : string) (arch : string) : unit =
      if Elf_utils.elf_reloc () then self#etrel_process f arch
      else self#normal_instr_process f arch

    (* reuse the parser to build the typed instr list, then symbolize+emit
       from the relocation tables. No DFA, jump-table, validator passes. *)
    method etrel_process (f : string) (arch : string) : unit =
      let funcs =
        List.filter_map
          (fun l ->
            match Str.split (Str.regexp " +") l with
            | v :: _sz :: "FUNC" :: _bind :: _ndx :: nm :: _ ->
                let a = int_of_string ("0x" ^ v) in
                Some
                  {
                    func_name = nm;
                    func_begin_addr = a;
                    func_end_addr = a;
                    is_lib = false;
                  }
            | _ -> None)
          (read_file "symtab.info")
      in
      let ail_parser = new ailParser in
      ail_parser#set_funcs funcs;
      ail_parser#set_secs secs;
      ail_parser#set_arch arch;
      ail_parser#process_instrs (read_file "instrs.info") arch;
      Reassemble_symbol_get.etrel_emit ail_parser#get_instrs f

    method normal_instr_process (f : string) (arch : string) : unit =
      let open Disassemble_process in
      let open Analysis_process in
      let module D = Disam in
      let module A = Analysis in
      let module S = Symbol_table_get in
      let module I = Instrumentation in
      let module EU = Elf_utils in
      let module FnU = Func_utils in
      let module TR = Time_Record in
      let _ = timed "preprocess" (fun () -> self#pre_process f arch) in
      (* .text section is in instrs.info *)
      let il, fl, re =
        timed "disassemble" (fun () -> D.disassemble f funcs secs arch)
      in

      print_endline "3: symbolization and instrumentation";

      let fbl, bbl, cfg_t, cg, il', re, ufl =
        timed "analyze_one" (fun () -> A.analyze_one il fl re arch)
      in

      let il', ufl', fch = timed "symbolize" (fun () -> S.apply il' ufl f) in

      (* refine function starts and ends for instrumentation *)
      let ufl' =
        ufl'
        |> (if EU.elf_intel () && EU.elf_32 () then
              flip FnU.refine_func_begin il'
            else Fun.id)
        |> flip FnU.refine_func_end il'
      in

      let instrumented_il : instr list =
        timed "instrument" (fun () ->
            I.apply ~instrs:il' ~fbl ~bbl ~funcs:ufl' ~fname_callsites:fch)
      in

      print_endline "4: post processing and reassembly";
      timed "postanalyze" (fun () -> A.post_analyze instrumented_il re arch);

      timed "postprocess" (fun () -> self#post_process f arch)
  end
