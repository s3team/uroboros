(** Perform instrumentation if file points.ins exists *)

open Ail_utils
open Ail_parser
open Type

type act = INSERT | INSERTCALL | DELETE | REPLACE | PRINTARGS
type dir = BEFORE | AFTER
type location = ADDRESS of string | SYMBOL of string
type locm = SELF | FUNENTRY | FUNEXIT | CALLSITE
type lang = ASM | C | OCaml
(* INPUT:
 * address action direction location loc-modifier
 * stack cmd language code code-entry-point *)
(* point: address action direction stack cmd language code code-entry-point *)
type point =
  int * act option * dir option * (string * string) list
  * string * lang option * string * string

let do_instrumentation : bool ref = ref false
let points : point list ref = ref []

let stub_loc : loc = {
  loc_label = "";
  loc_addr = 0;
  loc_visible = true
}

let remove_from_str ch s =
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
          Reg (Intel_Reg (Intel_CommonReg RAX)),
          stub_loc,
          None );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RSI)),
          stub_loc,
          None )
    ]

let read_lines_by_sep ~(f : string) ~(sep : char) : string list =
  let ic = open_in f in
  let content = really_input_string ic (in_channel_length ic) in
  let _ = close_in ic in
  let content_nocomments = List.map (
    fun s -> remove_from_str '#' s
  ) (String.split_on_char '\n' content) in
  let content' = String.concat "\n" content_nocomments in
  String.split_on_char sep content'

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
  | _ -> failwith "invalid loc range"

let create_points
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
            points := p' :: !points
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
            points := p' :: !points
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
        points := p' :: !points
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
          points := p' :: !points
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
          points := p' :: !points
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
            points := p' :: !points
          ) css
        | None -> ()
        end
      | None -> failwith "invalid loc-modifier"
  in
  List.iteri visit_location locations

let process_instrument_point
    (ufl : func list)
    (fname2css : (string, int list) Hashtbl.t)
    (line : string)
  : unit =
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
      create_points action dir locm stack'' cmd lang code_ep code' ufl fname2css
    ) locations
  | _ ->
    (*
    let _ = print_endline "^^^^^ ls' ^^^^^" in
    let _ = print_endline line in
    let _ = List.iter (
      fun s -> print_endline ("! " ^ s ^ " @")
    ) ls' in
    let _ = print_endline "~~~~~ ls' ~~~~~" in
    *)
    failwith "invalid instrument format"

let parse_instrumentation
    ~(funcs : func list)
    ~(fname_callsites : (string, int list) Hashtbl.t)
  : unit =
  let filelines = read_lines_by_sep ~f:"points.ins" ~sep:';' in
  let filelines' =
    List.filter (fun s -> not (String.trim s = "")) filelines
  in
  List.iter (
    process_instrument_point funcs fname_callsites
  ) filelines';
  points := List.sort (
    fun (addr1, _, _, _, _, _, _, _) (addr2, _, _, _, _, _, _, _) ->
      compare addr1 addr2
  ) !points

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
           ^ " is too many arguments. Need to insert with assembly instead" )

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
        failwith "for PRINTARGS 32-bits, should not reach here"
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

let print_args (args : (string * string) list)
  : instr list =
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
      ignore (Sys.command ("gcc -no-pie -c ./plugins/instr_c/lib.c -m32"))
    else
      ignore (Sys.command ("gcc -no-pie -c ./plugins/instr_c/lib.c"));
    match stack with
    | [] ->
      (* MAYBE: bruteforce print each argument as int, char*, and int* *)
      failwith
        "number of arguments and their types need to be specified in stack"
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
    ~(funcs : func list)
    ~(fname_callsites : (string, int list) Hashtbl.t)
  : instr list =
  match !do_instrumentation with
  | true ->
    begin
      (* populate points *)
      parse_instrumentation ~funcs ~fname_callsites;
      let rec add_instrumentation (instrs : instr list)
          (points : point list) (acc : instr list) : instr list =
        match instrs, points with
        | ([], _) -> List.rev acc
        | (ih :: it, []) -> add_instrumentation it [] (ih :: acc)
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
                add_instrumentation it pt (acc' @ acc)
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
                add_instrumentation it pt (acc' @ acc)
              | Some DELETE ->
                add_instrumentation it pt acc
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
               add_instrumentation it pt (add_seq @ acc)
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
                      "for PRINTARGS, can only insert before a call, not after"
                in
                add_instrumentation it pt (acc' @ acc)
              | _ ->
                add_instrumentation it pt (ih :: acc)
            end else if ih_addr > p_addr then
              add_instrumentation instrs pt acc
            else
              add_instrumentation it points (ih :: acc)
          end
      in
      add_instrumentation instrs !points []
    end
  | false -> instrs

let () =
  match Sys.file_exists "points.ins" with
  | true -> do_instrumentation := true
  | false -> do_instrumentation := false