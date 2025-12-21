open Batteries
open Type
open Ail_parser
open Pp_print
open Reassemble_symbol_get
open Cfg
open Cg

open Ail_utils

let merge_func (funcs : func list) (il : instr list) : func list =
  let module OU = Opcode_utils in
  let rec update funcs il acc =
    match (funcs, il) with
    | [], il' -> List.rev acc
    | funcs', [] -> List.rev acc
    | hf :: tf, hi :: ti ->
      let f_ea = hf.func_end_addr in
      let f_ba = hf.func_begin_addr in
      let i_loc = get_loc hi in
      let i_addr = i_loc.loc_addr in
      begin
        if i_addr = f_ba then
          match hi with
          | TripleInstr (p, e1, e2, _, _, _) -> (
              match (p, e1, e2) with
              | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Label l, Reg r
              | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Reg r, Label l ->
                  if contains ~str:l ~sub:"$_GLOBAL_OFFSET_TABLE_" then
                    let prev_func = List.hd acc in
                    let acc' = List.tl acc in
                    let prev_func' = { prev_func with func_end_addr = hf.func_end_addr } in
                    update funcs ti (prev_func' :: acc')
                  else
                    update funcs ti acc
              | _ -> update funcs ti acc )
          | _ -> update funcs ti acc
        else if i_addr >= f_ea then
          update tf il (hf :: acc)
        else if i_addr < f_ba then
          update funcs ti acc
        else
          update funcs ti acc
      end
    in
    update funcs il []

let update_func_end (funcs : func list) (il : instr list) : func list =
  let module OU = Opcode_utils in
  let rec update funcs il acc =
    match (funcs, il) with
    | [], il' -> acc
    | funcs', [] -> acc
    | hf :: tf, hi :: ti ->
      let f_ea = hf.func_end_addr in
      let f_ba = hf.func_begin_addr in
      let i_loc = get_loc hi in
      let i_addr = i_loc.loc_addr in
      begin
        if i_addr >= f_ea then
          update tf il (hf :: acc)
        else if i_addr < f_ba then
          update funcs ti acc
        else
          if OU.is_ret_instr hi then
            update ({ hf with func_end_addr = i_addr } :: tf) ti acc
          else
            update funcs ti acc
      end
  in
  update funcs il []

let merge_func (funcs : func list) (il : instr list) : func list =
  let module OU = Opcode_utils in
  let rec update funcs il acc =
    match (funcs, il) with
    | [], il' -> List.rev acc
    | funcs', [] -> List.rev acc
    | hf :: tf, hi :: ti ->
      let f_ea = hf.func_end_addr in
      let f_ba = hf.func_begin_addr in
      let i_loc = get_loc hi in
      let i_addr = i_loc.loc_addr in
      begin
        if i_addr = f_ba then
          match hi with
          | TripleInstr (p, e1, e2, _, _, _) -> (
              match (p, e1, e2) with
              | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Label l, Reg r
              | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Reg r, Label l ->
                  if contains ~str:l ~sub:"$_GLOBAL_OFFSET_TABLE_" then
                    let prev_func = List.hd acc in
                    let acc' = List.tl acc in
                    let prev_func' = { prev_func with func_end_addr = hf.func_end_addr } in
                    update funcs ti (prev_func' :: acc')
                  else
                    update funcs ti acc
              | _ -> update funcs ti acc )
          | _ -> update funcs ti acc
        else if i_addr >= f_ea then
          update tf il (hf :: acc)
        else if i_addr < f_ba then
          update funcs ti acc
        else
          update funcs ti acc
      end
    in
    update funcs il []

let update_func_end (funcs : func list) (il : instr list) : func list =
  let module OU = Opcode_utils in
  let rec update funcs il acc =
    match (funcs, il) with
    | [], il' -> acc
    | funcs', [] -> acc
    | hf :: tf, hi :: ti ->
      let f_ea = hf.func_end_addr in
      let f_ba = hf.func_begin_addr in
      let i_loc = get_loc hi in
      let i_addr = i_loc.loc_addr in
      begin
        if i_addr >= f_ea then
          update tf il (hf :: acc)
        else if i_addr < f_ba then
          update funcs ti acc
        else
          if OU.is_ret_instr hi then
            update ({ hf with func_end_addr = i_addr } :: tf) ti acc
          else
            update funcs ti acc
      end
  in
  update funcs il []

class ail =
object (self)
  val mutable funcs : func list = []
  val mutable secs: section list = []
  val mutable intrs: string list = []
  val mutable instrs_list: instr list = []
  val mutable datas: string list = []
  val mutable g_bss: (string * string * string) list = []

  method sections : unit =
    let filelines = File.lines_of "sections.info"
    and help l =
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x"^(List.nth items 1))
      and size = int_of_string ("0x"^(List.nth items 3))
      and secname = List.nth items 0 in
      secs <- {
        sec_name = secname;
        sec_begin_addr = addr;
        sec_size = size
      } :: secs
    in
    Enum.iter help filelines

  method externfuncs : unit =
    let filelines = File.lines_of "externfuncs.info"
    and help l =
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x"^(List.nth items 0))
      and func = List.nth items 1 in
      funcs <- {
        func_name = func;
        func_begin_addr = addr;
        func_end_addr = 0;
        is_lib = true
      } :: funcs
    in
    Enum.iter help filelines

  (** in stripped binary, any user functions' information has been stripped
   *  so slicing function class really does the job *)
  method userfuncs : unit =
    let filelines = File.lines_of "userfuncs.info"
    and help l =
      if String.exists l "-0x" || String.exists l "+0x" then
        ()
      else
        begin
          let items = Str.split (Str.regexp " +") l in
          let addr = int_of_string ("0x"^(List.nth items 0))
          and funname = List.nth items 1 in
          let len = String.length funname in
          let funname' = String.sub funname 1 (len-3) in
          if String.exists funname' "@@" then
            let fn = List.nth (Str.split (Str.regexp_string "@@") funname') 0 in
            funcs <- {
              func_name = fn;
              func_begin_addr = addr;
              func_end_addr = 0;
              is_lib = false
            } :: funcs
          else
          funcs <- {
            func_name = funname';
            func_begin_addr = addr;
            func_end_addr = 0;
            is_lib = false
          } :: funcs
        end
    in
    Enum.iter help filelines

  method get_userfuncs : func list =
    List.filter (fun f -> f.is_lib=false) funcs

  method externdatas : unit =
    let filelines = File.lines_of "externdatas.info"
    and help l =
      let data = String.trim l in
      datas <- data::datas
    in
    Enum.iter help filelines

  method global_bss : unit =
    let filelines = File.lines_of "globalbss.info"
    and help l =
      let items = Str.split (Str.regexp " +") l in
      let t = List.nth items 0 in
      let addr = String.sub t 1 ((String.length t)-1) in
      let addr' = String.uppercase_ascii addr
      and rtype = List.nth items 1
      and n = String.trim (List.nth items 2) in
      g_bss <- (addr', rtype, n) :: g_bss
    in
    Enum.iter help filelines

  method ail_dump : unit =
    (* currently we just dump the extern function info *)
    let check_sym_func f =
      try
        let s = Char.escaped(f.func_name.[0])^Char.escaped(f.func_name.[1]) in
        s <> "__"
      with
      | _ -> false in
    let oc = open_out_gen [Open_append; Open_creat] 0o666 "final.s" in
    (List.filter (fun f -> f.is_lib) funcs
     |> List.filter check_sym_func
     |> List.iter (fun l -> Printf.fprintf oc "extern %s\n" l.func_name));
    close_out oc

  method ehframe_dump : unit =
    ignore ( Sys.command ("cat eh_frame.data >> final.s") )

  method excpt_tbl_dump : unit =
    ignore ( Sys.command ("cat gcc_exception_table.data >> final.s") )

  method post_process (f : string) (arch : string) : unit =
    ignore ( Sys.command ("python3 main_discover.py " ^ " " ^ f ^ " " ^ arch) );
    ignore ( Sys.command ("python3 post_process.py "^arch) );
    ignore ( Sys.command ("python3 post_process_lib.py") )
    (*
    self#ehframe_dump;
    self#excpt_tbl_dump;
    *)

  method pre_process (f : string) (arch : string) : unit =
    ignore ( Sys.command ("python3 pre_process.py") );
    ignore ( Sys.command ("python3 main_discover.py " ^ " " ^ f ^ " " ^ arch) )

  method instr_process (f : string) (arch : string) : unit =
    let open Disassemble_process in
    let open Analysis_process in
    let module D = Disam in
    let module A = Analysis in
    let module S = Symbol_table_get in
    let module I = Instrumentation in
    let module EU = ELF_utils in
    let _ = self#pre_process f arch in
    (* .text section is in instrs.info *)
    let il, fl, re = D.disassemble f funcs secs arch in

    print_endline "3: analysis";

    let fbl, bbl, cfg_t, cg, il', re, ufl = A.analyze_one il fl re arch in

    let il', ufl', fch = S.apply il' ufl f in

    let ufl' =
      if EU.elf_32 () then
        merge_func ufl' il'
      else
        ufl'
    in
    let ufl' = update_func_end ufl' il' in

    let instrumented_il : instr list =
      I.apply ~instrs:il' ~fbl ~bbl ~funcs:ufl' ~fname_callsites:fch
    in

    print_endline "4: post processing";
    A.post_analyze instrumented_il re arch;

    self#post_process f arch
end
