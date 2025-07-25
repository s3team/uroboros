(** Perform instrumentation if file points.ins exists *)

open Ail_utils
open Ail_parser
open Type

type act = INSERT | INSERTCALL | DELETE | REPLACE | PRINTARGS
type dir = BEFORE | AFTER
type location = ADDRESS of string | SYMBOL of string
type locm = SELF | FUNENTRY | FUNEXIT | CALLSITE
type lang = ASM | C | OCaml
(* instrument point:
 *  format 1: address action direction stack cmd language code code-entry-point
 * or
 *  format 2: user module *)
type point_f1 =
  int * act option * dir option * (string * string) list
  * string * lang option * string * string
type point_f2 = string * string

module type PLUGIN = sig
  val instrument:
    instr list ->
    (string, bblock list) Hashtbl.t ->
    bblock list ->
    instr list
end

let plugin = ref None

let get_plugin () : (module PLUGIN)  =
  match !plugin with
  | Some s -> s
  | None ->
    failwith ("Need to pass plugin path, failure at " ^ __LOC__)

let load_plugin f =
  let module_name = Filename.basename f in
  let module_path = "_build/default/plugins/" ^ module_name in
  if Sys.file_exists module_path then
    try
      Dynlink.loadfile_private module_path
    with
    | _ -> failwith ("Error loading plugin, failure at " ^ __LOC__)
  else failwith ("Need to pass plugin path, failure at " ^ __LOC__)

let do_instrumentation : bool ref = ref false
let points_f1 : point_f1 list ref = ref []
let points_f2 : point_f2 list ref = ref []

let stub_loc : loc = {
  loc_label = "";
  loc_addr = 0;
  loc_visible = true
}

let file_append (filename : string) (content : string) : unit =
  let oc = open_out_gen [Open_append; Open_creat; Open_text] 0o644 filename in
  output_string oc content;
  close_out oc

let remove_comment (ch : char) (s : string) : string =
  match String.index_opt s ch with
  | Some i -> String.sub s 0 i
  | None -> s

let caller_saved_before : instr list =
  let module EU = ELF_utils in
  if EU.elf_32 () then
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg EAX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg EBX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg EDX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          stub_loc,
          None )
    ]
  else
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RSI)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RAX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RBX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RDX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RCX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RDI)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R8)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R9)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R10)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R11)),
          stub_loc,
          None );
    ]

let caller_saved_after : instr list =
  let module EU = ELF_utils in
  if EU.elf_32 () then
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg EDX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg EBX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg EAX)),
          stub_loc,
          None )
    ]
  else
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R11)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R10)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R9)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R8)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RDI)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RCX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RDX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RBX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RAX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RSI)),
          stub_loc,
          None )
    ]

let read_points ~(f : string) : string list =
  let ic = open_in f in
  let content = really_input_string ic (in_channel_length ic) in
  let _ = close_in ic in
  let content_nocomments = List.map (
    fun s -> remove_comment '#' s
  ) (String.split_on_char '\n' content) in
  let content' = String.concat "\n" content_nocomments in
  String.split_on_char ';' content'

let count_char ~(s : string) ~(c : char) : int =
  String.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0 s

let strip_bracket (s : string) : string =
  let len = String.length s in
  String.sub s 1 (len - 2)

let get_action : string -> act option = function
  | "INSERT" | "insert" -> Some INSERT
  | "INSERTCALL" | "insertcall" -> Some INSERTCALL
  | "DELETE" | "delete" -> Some DELETE
  | "REPLACE" | "replace" -> Some REPLACE
  | "PRINTARGS" | "printargs" -> Some PRINTARGS
  | _ -> None

let get_dir : string -> dir option = function
  | "BEFORE" | "before" -> Some BEFORE
  | "AFTER" | "after" -> Some AFTER
  | _ -> None

let get_loc_modifier : string -> locm option = function
  | "SELF" | "self" -> Some SELF
  | "FUNENTRY" | "funentry" -> Some FUNENTRY
  | "FUNEXIT" | "funexit" -> Some FUNEXIT
  | "CALLSITE" | "callsite" -> Some CALLSITE
  | _ -> None

let get_lang : string -> lang option = function
  | "ASM" | "asm" -> Some ASM
  | "C" | "c" -> Some C
  | "OCaml" | "ocaml" -> Some OCaml
  | _ -> None

let get_location (l : string) : location =
  if String.starts_with ~prefix:"0x" l then ADDRESS l
  else SYMBOL l

let create_range ~(_start : int) ~(_end : int) : location list =
  let rec cr s i acc =
    if i >= 0 then cr (s+1) (i-1) (ADDRESS (dec_hex s) :: acc)
    else acc
  in
  cr _start (_end-_start) []

let extract_location_range (loc : string) : location list =
  let locs = String.split_on_char '-' loc in
  match locs with
  | [_start ; _end]  ->
    let _start' = int_of_string _start in
    let _end' = int_of_string _end in
    create_range ~_start:_start' ~_end:_end'
  | _ -> failwith ("invalid loc range at " ^ __LOC__)

let create_points_f1
    (action : act option)
    (dir : dir option)
    (locm : locm option)
    (stack : (string * string) list)
    (cmd : string)
    (lang : lang option)
    (code_ep : string)
    (code : string)
    (ufl : func list)
    (fname2css : (string, int list) Hashtbl.t)
    (loc : string) 
  : unit =
  let locations : location list =
    if contains ~str:loc ~sub:"-" then
      extract_location_range loc
    else
      (get_location loc) :: []
  in
  let locations_len = List.length locations in
  let visit_location (i : int) (location : location) : unit =
    match location with
    | ADDRESS a ->
      begin match action with
      | Some REPLACE ->
        begin
          if i = 0 then
            (* latest address *)
            let p' =
              ( int_of_string a,
                Some REPLACE,
                dir,
                stack,
                cmd,
                lang,
                code_ep,
                code )
            in
            points_f1 := p' :: !points_f1
          else
            let p' =
              ( int_of_string a,
                Some DELETE,
                None,
                [],
                "",
                None,
                "",
                "" )
            in
            points_f1 := p' :: !points_f1
        end
      | _ ->
        let p' =
          ( int_of_string a,
            action,
            dir,
            stack,
            cmd,
            lang,
            code_ep,
            code )
        in
        points_f1 := p' :: !points_f1
      end
    | SYMBOL s ->
      match locm with
      | Some FUNENTRY ->
        begin match List.find_opt (fun f -> f.func_name = s) ufl with
        | Some s_func ->
          let p' =
            ( s_func.func_begin_addr,
              action,
              dir,
              stack,
              cmd,
              lang,
              code_ep,
              code )
          in
          points_f1 := p' :: !points_f1
        | None -> ()
        end
      | Some FUNEXIT ->
        begin match List.find_opt (fun f -> f.func_name = s) ufl with
        | Some s_func ->
          let p' =
            (* s_func.func_end_addr is the starting address of the
             * following function so -1 is the address of ret,
             * which is 1 byte. Therefore, we assume that the last
             * instruction in a function is always ret *)
            ( s_func.func_end_addr - 1,
              action,
              dir,
              stack,
              cmd,
              lang,
              code_ep,
              code )
          in
          points_f1 := p' :: !points_f1
        | None -> ()
        end
      | Some CALLSITE ->
        begin match Hashtbl.find_opt fname2css s with
        | Some css ->
          List.iter ( fun addr ->
            let p' =
              ( addr,
                action,
                dir,
                stack,
                cmd,
                lang,
                code_ep,
                code )
            in
            points_f1 := p' :: !points_f1
          ) css
        | None -> ()
        end
      | None -> failwith ("invalid loc-modifier at " ^ __LOC__)
  in
  List.iteri visit_location locations

let process_instrument_point
    (ufl : func list)
    (fname2css : (string, int list) Hashtbl.t)
    (instrs : instr list)
    (fbl : (string, bblock list) Hashtbl.t)
    (bbl : bblock list)
    (line : string)
  : unit =
  if not (String.starts_with ~prefix:"user" line) then
    (* create a point for format 1 *)
    let ls' =
      let ls = String.trim line |> String.split_on_char '\"' in
      let ls0 = List.nth ls 0 |> String.trim |> String.split_on_char ' ' in
      let ls1 = List.nth ls 1 in
      let ls2 = List.nth ls 2 |> String.trim |> String.split_on_char ' ' in
      let quotes = count_char ~s:line ~c:'\"' in
      if quotes = 2 then
        ls0 @ [ls1] @ ls2
      else
        let ls3 = List.nth ls 3 |> String.trim in
        ls0 @ [ls1] @ ls2 @ [ls3]
    in
    match ls' with
    | [a; d; l; lm; stack; cmd; lang; code_ep; code] ->
      let code' = String.trim code in
      let action = get_action a in
      let dir = get_dir d in
      let locm = get_loc_modifier lm in
      let lang = get_lang lang in
      let locations = l |> strip_bracket |> String.split_on_char ',' in
      let stack' =
        stack
        |> strip_bracket
        |> String.split_on_char ','
        |> List.filter (fun s -> not (String.trim s = ""))
      in
      let stack'' =
        List.map (
          fun s ->
            let s' = String.split_on_char ':' s in
            (List.nth s' 0, List.nth s' 1)
        ) stack'
      in
      List.iter (
        create_points_f1
          action dir locm stack'' cmd lang code_ep code' ufl fname2css
      ) locations
    | _ -> failwith ("invalid instrument format at " ^ __LOC__)
  else
    (* create a point for format 2 *)
    let ls' = String.trim line |> String.split_on_char ' ' in
    match ls' with
    | [user; module_path] -> begin
      let module_name = module_path
        |> Filename.basename
        |> Filename.remove_extension
      in
      let module_path' =
        (Filename.dirname module_path)
        ^ "/"
        ^ module_name
        ^ ".cmxs"
      in
      let dune_add =
        "(subdir plugins\n"
        ^ "  (library\n"
        ^ "    (name " ^ module_name ^ ")\n"
        ^ "    (modules " ^ module_name ^ ")\n"
        ^ "    (libraries ail_utils ail_parser instrumentation)\n"
        ^ "    (wrapped false)\n"
        ^ "    (flags (:standard -w -7-8-26-27-10-11-32-33-39))))\n"
      in
      file_append "dune" dune_add;
      (* add to points_f2 *)
      points_f2 := (user, module_path') :: !points_f2
    end
    | _ -> failwith ("invalid instrument format at " ^ __LOC__)

let parse_instrumentation
    ~(funcs : func list)
    ~(fname_callsites : (string, int list) Hashtbl.t)
    ~(instrs : instr list)
    ~(fbl : (string, bblock list) Hashtbl.t)
    ~(bbl : bblock list)
  : unit =
  let filelines =
    read_points ~f:"points.ins"
    |> List.filter (fun s -> not (String.trim s = ""))
    |> List.map (fun s -> String.trim s)
  in
  List.iter (
    process_instrument_point funcs fname_callsites instrs fbl bbl
  ) filelines;
  points_f1 := List.sort (
    fun (addr1, _, _, _, _, _, _, _) (addr2, _, _, _, _, _, _, _) ->
      compare addr1 addr2
  ) !points_f1

let arg_order_64 i =
  match i with
  | 0 -> Intel_CommonReg RDI
  | 1 -> Intel_CommonReg RSI
  | 2 -> Intel_CommonReg RDX
  | 3 -> Intel_CommonReg RCX
  | 4 -> Intel_SpecialReg R8
  | 5 -> Intel_SpecialReg R9
  | x -> failwith (
          (string_of_int x)
           ^ " is too many arguments. Need to insert with assembly instead."
           ^ "Failure at " ^ __LOC__)

let set_arg i arg =
  let module EU = ELF_utils in
  if EU.elf_32 () then
    DoubleInstr
      ( Intel_OP (Intel_StackOP PUSH),
        arg,
        stub_loc,
        None )
  else
    let dest_arg_reg = arg_order_64 i in
    TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
        Reg (Intel_Reg dest_arg_reg),
        arg,
        stub_loc,
        None )

(** update stack since for 32-bit arguments are passed on the stack *)
let clean_arg_32 arg =
  TripleInstr
    ( Intel_OP (Intel_CommonOP (Intel_Arithm ADD)),
      Reg (Intel_Reg (Intel_StackReg ESP)),
      Const (Normal 4),
      stub_loc,
      None )

(** insert call with arguments *)
let add_call_seq_arg
    ~(ins_loc : loc)
    ~(code_ep : string)
    ~(args : (string * string) list)
  : instr list =
  let module EU = ELF_utils in
  let parse_arg arg =
    let _type, _value = arg in
    match _type with
    | "int" -> 
      ( try Const (Normal (int_of_string _value))
      with _ -> Label _value )
    | "char*" | "char *" | "int*" | "int *" ->
      Label _value
    | "print-arg" ->
      if EU.elf_32 () then
        failwith ("for PRINTARGS 32-bits, should not reach here. Failure at "
                  ^ __LOC__)
      else
        Reg (Intel_Reg (arg_order_64 (int_of_string _value)))
  in
  let args' = List.map (
    parse_arg
  ) args
  in
  if EU.elf_32 () then
    List.rev
      ( caller_saved_before
      @ List.mapi (
        set_arg
      ) args'
      @ [ DoubleInstr
          ( Intel_OP (Intel_ControlOP (CALL)),
            Symbol (
              CallDes {
                func_name=code_ep;
                func_begin_addr=0;
                func_end_addr=0;
                is_lib=false;
              }
            ),
            ins_loc,
            None ) ]
    @ List.map ( clean_arg_32 ) args  (* arguments on stack for 32-bits only *)
    @ caller_saved_after )
  else
    List.rev
      ( caller_saved_before
      @ List.mapi (
        set_arg
      ) args'
      @ [ DoubleInstr
          ( Intel_OP (Intel_ControlOP (CALL)),
            Symbol (
              CallDes {
                func_name=code_ep;
                func_begin_addr=0;
                func_end_addr=0;
                is_lib=false;
              }
            ),
            ins_loc,
            None ) ]
    @ caller_saved_after )

(** insert call without arguments *)
let add_call_seq
    ~(ins_loc : loc)
    ~(code_ep : string)
    ~(use_existing_arg : bool)
  : instr list =
  let module EU = ELF_utils in
  (** 32-bit x86 calling convention pushes arguments on the stack,
    * so if we save caller-saved registers on the stack before the call
    * the argument stack is messed up *)
  if EU.elf_32 () && use_existing_arg then
    List.rev
    ( [ DoubleInstr
        ( Intel_OP (Intel_ControlOP (CALL)),
          Symbol (
            CallDes {
              func_name=code_ep;
              func_begin_addr=0;
              func_end_addr=0;
              is_lib=false;
            }
          ),
          ins_loc,
          None ) ] )
  else
    List.rev
      ( caller_saved_before
      @ [ DoubleInstr
          ( Intel_OP (Intel_ControlOP (CALL)),
            Symbol (
              CallDes {
                func_name=code_ep;
                func_begin_addr=0;
                func_end_addr=0;
                is_lib=false;
              }
            ),
            ins_loc,
            None ) ]
      @ caller_saved_after )

let print_args (args : (string * string) list) : instr list =
  let module EU = ELF_utils in
  let rec call_with_arg
      (args : (string * string) list)
      (i : int)
      (acc : instr list)
    : instr list =
    match args with
    | [] -> List.rev acc
    | (t, v) :: rest ->
      if EU.elf_32 () then
        let code_ep =
          match t with
          | "int" -> "print_int_arg"
          | "char*" | "char *" -> "print_char_star_arg"
          | "int*" | "int *" -> "print_int_star_arg"
        in
        let esp_arg_i = 4 * (i + 1) in
        let call_seq = [ DoubleInstr
          ( Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg EAX)),
            stub_loc,
            None );
          TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            Reg (Intel_Reg (Intel_CommonReg EAX)),
            Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg ESP), esp_arg_i)),
            stub_loc,
            None );
          DoubleInstr
          ( Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg EAX)),
            stub_loc,
            None );
          DoubleInstr
          ( Intel_OP (Intel_ControlOP (CALL)),
            Symbol (
              CallDes {
                func_name=code_ep;
                func_begin_addr=0;
                func_end_addr=0;
                is_lib=false;
              }
            ),
            stub_loc,
            None );
          DoubleInstr
            ( Intel_OP (Intel_StackOP (POP)),
              Reg (Intel_Reg (Intel_CommonReg EAX)),
              stub_loc,
              None );
          DoubleInstr
            ( Intel_OP (Intel_StackOP (POP)),
              Reg (Intel_Reg (Intel_CommonReg EAX)),
              stub_loc,
              None ) ]
        in
        call_with_arg rest (i+1) (call_seq @ acc)
      else
        let call_seq =
          let code_ep =
            match t with
            | "int" -> "print_int_arg"
            | "char*" | "char *" -> "print_char_star_arg"
            | "int*" | "int *" -> "print_int_star_arg"
          in
          let arg_i = string_of_int i in
          List.rev
            ( add_call_seq_arg
                ~ins_loc:stub_loc ~code_ep ~args:[("print-arg", arg_i)] )
        in
        call_with_arg rest (i+1) (call_seq @ acc)
  in
  call_with_arg args 0 []

let parse_instr_from_code
    ~(cmd : string)
    ~(ins_loc : loc)
    ~(code_ep : string)
    ~(code : string)
    ~(stack : (string * string) list)
    ~(use_existing_arg : bool)
  : instr list =
  let module EU = ELF_utils in
  if cmd = "" && code_ep = "x" && code = "x" then begin
    (* action PRINTARGS *)
    if EU.elf_32 () then
      ignore (Sys.command ("gcc -no-pie -c ./plugins/c/lib.c -m32"))
    else
      ignore (Sys.command ("gcc -no-pie -c ./plugins/c/lib.c"));
    match stack with
    | [] ->
      failwith
        ("number of arguments and their types need to be specified in stack. "
         ^ "Failure at " ^ __LOC__)
    | args ->
      (* print args as specified *)
      print_args args
  end else if String.ends_with ~suffix:".c" code then begin
    ignore (Sys.command (cmd));
    match stack with
    | [] -> add_call_seq ~ins_loc ~code_ep ~use_existing_arg
    | _ -> add_call_seq_arg ~ins_loc ~code_ep ~args:stack
  end else if String.ends_with ~suffix:".asm" code then begin
    let ail_parser = new ailParser in
    let asm = read_lines code in
    let _ = ail_parser#process_asms asm "intel" in
    ail_parser#get_instrs
  end else begin
    let ail_parser = new ailParser in
    let asm = String.split_on_char '\n' code in
    let _ = ail_parser#process_asms asm "intel" in
    ail_parser#get_instrs
  end

let apply
    ~(instrs : instr list)
    ~(fbl : (string, bblock list) Hashtbl.t)
    ~(bbl : bblock list)
    ~(funcs : func list)
    ~(fname_callsites : (string, int list) Hashtbl.t)
  : instr list =
  match !do_instrumentation with
  | true ->
    begin
      (* populate points_f1 and points_f2 *)
      parse_instrumentation ~funcs ~fname_callsites ~instrs ~fbl ~bbl;
      let rec add_instrumentation_f1
          (instrs : instr list)
          (points_f1 : point_f1 list)
          (acc : instr list)
        : instr list =
        match instrs, points_f1 with
        | ([], _) -> List.rev acc
        | (ih :: it, []) -> add_instrumentation_f1 it [] (ih :: acc)
        | (ih :: it, ph :: pt) ->
          begin
            let ( p_addr, p_action, p_dir, p_stack,
                  p_cmd, p_lang, p_code_ep, p_code ) = ph in
            let ih_loc : loc = get_loc ih in
            let ih_addr : int = ih_loc.loc_addr in
            if ih_addr = p_addr then begin
              match p_action with
              | Some INSERT ->
                let add_seq =
                  parse_instr_from_code
                    ~cmd:p_cmd
                    ~ins_loc:ih_loc
                    ~code_ep:p_code_ep
                    ~code:p_code
                    ~stack:[]
                    ~use_existing_arg: false
                in
                let acc' : instr list =
                  match p_dir with
                  | Some BEFORE | None -> ih :: add_seq
                  | Some AFTER -> add_seq @ [ih]
                in
                add_instrumentation_f1 it pt (acc' @ acc)
              | Some INSERTCALL ->
                let add_seq =
                  parse_instr_from_code
                    ~cmd:p_cmd
                    ~ins_loc:ih_loc
                    ~code_ep:p_code_ep
                    ~code:p_code
                    ~stack:p_stack
                    ~use_existing_arg: true
                in
                let acc' : instr list =
                  match p_dir with
                  | Some BEFORE | None -> ih :: add_seq
                  | Some AFTER -> add_seq @ [ih]
                in
                add_instrumentation_f1 it pt (acc' @ acc)
              | Some DELETE ->
                add_instrumentation_f1 it pt acc
              | Some REPLACE ->
                let add_seq =
                  parse_instr_from_code
                    ~cmd:p_cmd
                    ~ins_loc:ih_loc
                    ~code_ep:p_code_ep
                    ~code:p_code
                    ~stack:[]
                    ~use_existing_arg:false
                in
               add_instrumentation_f1 it pt (add_seq @ acc)
              | Some PRINTARGS ->
                let add_seq =
                  parse_instr_from_code
                    ~cmd:""
                    ~ins_loc:ih_loc
                    ~code_ep:p_code_ep
                    ~code:p_code
                    ~stack:p_stack
                    ~use_existing_arg:false
                in
                let acc' : instr list =
                  match p_dir with
                  | Some BEFORE | None -> ih :: add_seq
                  | Some AFTER ->
                    failwith
                      ("for PRINTARGS, can only insert before a call, not after."
                       ^ " Failure at " ^ __LOC__)
                in
                add_instrumentation_f1 it pt (acc' @ acc)
              | _ ->
                add_instrumentation_f1 it pt (ih :: acc)
            end else if ih_addr > p_addr then
              add_instrumentation_f1 instrs pt acc
            else
              add_instrumentation_f1 it points_f1 (ih :: acc)
          end
      in
      let rec add_instrumentation_f2
          (instrs : instr list)
          (points_f2 : point_f2 list)
        : instr list =
        match points_f2 with
        | [] -> instrs
        | ph :: [] -> begin
          let (user, module_path) = ph in
          ignore (Sys.command ("dune build " ^ module_path));
          load_plugin module_path;
          let module M = (val get_plugin () : PLUGIN) in
          let il = M.instrument instrs fbl bbl in
          il
        end
        | ph :: pt -> begin
          let (user, module_path) = ph in
          ignore (Sys.command ("dune build " ^ module_path));
          load_plugin module_path;
          let module M = (val get_plugin () : PLUGIN) in
          let il = M.instrument instrs fbl bbl in
          add_instrumentation_f2 il pt
        end
      in
      let il = add_instrumentation_f1 instrs !points_f1 [] in
      let il = add_instrumentation_f2 il !points_f2 in
      (* remove modifications made to dune just for the instrumentation *)
      let _ = ignore (Sys.command ("git checkout dune")) in
      il
    end
  | false -> instrs

let () =
  match Sys.file_exists "points.ins" with
  | true -> do_instrumentation := true
  | false -> do_instrumentation := false