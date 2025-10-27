(** Perform instrumentation if file points.ins exists *)

open Ail_utils
open Ail_parser
open Type
open Pp_print

type act = INSERT | INSERTCALL | DELETE | REPLACE | PRINTARGS | INCLUDE
type dir = BEFORE | AFTER | AT
type location = ADDRESS of string | SYMBOL of string
type locm = SELF | FUNENTRY | FUNEXIT | CALLSITE
type lang = ASM | C | OCaml
(* instrument point:
 *  format 1:
      address action direction stack cmd language code code-entry-point lidx pidx fp
 * or
 *  format 2: user module *)
type point_f1 =
  int * act option * dir option * (string * string) list
  * string * lang option * string * string * string * int * string
type point_f2 = string * string

type c_ret_type = VOID | NOT_VOID

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

let load_plugin
    (f : string)
  : unit =
  let module_name = Filename.basename f in
  let module_path = "_build/default/instr_modules/" ^ module_name in
  if Sys.file_exists module_path then
    try
      Dynlink.loadfile_private module_path
    with
    | _ -> failwith ("Error loading plugin, failure at " ^ __LOC__)
  else failwith ("Need to pass plugin path, failure at " ^ __LOC__)

let do_instrumentation : bool ref = ref false
let points_f1 : point_f1 list ref = ref []
(* to group points that target the same address into the same list *)
let points_f1' : point_f1 list list ref = ref []
let points_f2 : point_f2 list ref = ref []

let stub_loc : loc = {
  loc_label = "";
  loc_addr = 0;
  loc_visible = true
}

let file_append
    (filename : string)
    (content : string)
  : unit =
  let oc = open_out_gen [Open_append; Open_creat; Open_text] 0o644 filename in
  output_string oc content;
  close_out oc

let get_linenum_point
    (s : string)
  : string * string =
  match String.index_opt s '^' with
  | Some i ->
    begin
      let linenum = String.sub s 0 i in
      let linenum_len = String.length linenum in
      let point = String.sub s (linenum_len+1) (String.length s - (linenum_len+1)) in
      (linenum, point)
    end
  | None ->
    let s = String.lowercase_ascii s in
    if String.starts_with ~prefix:"user" s then
      (* for calling OCaml modules *)
      ("0", s)
    else failwith ("invalid point format at " ^ __LOC__)

let is_valid_action
    (s : string)
  : bool =
  let s = String.lowercase_ascii s in
  if String.starts_with ~prefix:"insert" s ||
     String.starts_with ~prefix:"insertcall" s ||
     String.starts_with ~prefix:"delete" s ||
     String.starts_with ~prefix:"replace" s ||
     String.starts_with ~prefix:"printargs" s ||
     String.starts_with ~prefix:"include" s
  then true
  else false

let remove_comment_from_point
    (ch : char)
    (s : string)
    (idx : int)
  : string =
  match String.index_opt s ch with
  | Some i ->
    if is_valid_action s then (string_of_int idx) ^ "^" ^ (String.sub s 0 i)
    else String.sub s 0 i
  | None ->
    if is_valid_action s then (string_of_int idx) ^ "^" ^ s
    else s

let create_comment
    (comment : string)
  : (string, tags_val) Hashtbl.t =
  let tags = Hashtbl.create 1 in
  let _ = Hashtbl.add tags "comment" (Str comment) in
  tags

let add_comment
    (instr : instr)
    (comment : string)
  : instr =
  match instr with
  | SingleInstr (op, loc, prefix, tag, tags) ->
    SingleInstr (op, loc, prefix, tag, create_comment comment)
  | DoubleInstr (op, exp, loc, prefix, tag, tags) ->
    DoubleInstr (op, exp, loc, prefix, tag, create_comment comment)
  | TripleInstr (op, exp1, exp2, loc, prefix, tag, tags) ->
    TripleInstr (op, exp1, exp2, loc, prefix, tag, create_comment comment)
  | FourInstr (op, exp1, exp2, exp3, loc, prefix, tag, tags) ->
    FourInstr (op, exp1, exp2, exp3, loc, prefix, tag, create_comment comment)
  | FifInstr (op, exp1, exp2, exp3, exp4, loc, prefix, tag, tags) ->
    FifInstr (op, exp1, exp2, exp3, exp4, loc, prefix, tag, create_comment comment)

let p_dir_str
    (dir : dir option)
  : string =
  match dir with
  | Some BEFORE -> "before"
  | Some AFTER -> "after"
  | Some AT -> "at"
  | None -> ""

(** preserve return value of inserted callee *)
let caller_saved_before_ret
    (instr_type : string)
  : instr list =
  let module EU = ELF_utils in
  if EU.elf_arm () then
    [
      DoubleInstr
        ( Arm_OP (Arm_StackOP (PUSH), None, None),
          Label "{r1, r2, r3, r12, lr}",
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]
  else if EU.elf_32 () then
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg EBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg EBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg EDX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]
  else
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RSI)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RDX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RCX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RDI)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R8)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R9)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R10)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R11)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
    ]

let caller_saved_before
    (instr_type : string)
  : instr list =
  let module EU = ELF_utils in
  if EU.elf_arm () then
    [
      DoubleInstr
        ( Arm_OP (Arm_StackOP (PUSH), None, None),
          Label "{r0, r1, r2, r3, r12, lr}",
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]
  else if EU.elf_32 () then
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg EAX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg EBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg EDX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]
  else
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RSI)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RAX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RDX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RCX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_CommonReg RDI)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R8)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R9)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R10)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (PUSH)),
          Reg (Intel_Reg (Intel_SpecialReg R11)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
    ]

let caller_saved_after_ret
    (instr_type : string)
  : instr list =
  let module EU = ELF_utils in
  if EU.elf_arm () then
    [
      DoubleInstr
        ( Arm_OP (Arm_StackOP (POP), None, None),
          Label "{r1, r2, r3, r12, lr}",
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]
  else if EU.elf_32 () then
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg EDX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg EBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg EBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]
  else
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R11)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R10)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R9)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R8)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RDI)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RCX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RDX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RSI)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]

let caller_saved_after
    (instr_type : string)
  : instr list =
  let module EU = ELF_utils in
  if EU.elf_arm () then
    [
      DoubleInstr
        ( Arm_OP (Arm_StackOP (POP), None, None),
          Label "{r0, r1, r2, r3, r12, lr}",
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]
  else if EU.elf_32 () then
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg EDX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg EBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg EAX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]
  else
    [
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R11)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R10)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R9)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_SpecialReg R8)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RDI)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RCX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RDX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RBX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RAX)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) );
      DoubleInstr
        ( Intel_OP (Intel_StackOP (POP)),
          Reg (Intel_Reg (Intel_CommonReg RSI)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) )
    ]

let caller_saved_before_regs
    (instr_type : string)
  : c_ret_type -> instr list = function
  | VOID -> caller_saved_before instr_type
  | NOT_VOID -> caller_saved_before_ret instr_type

let caller_saved_after_regs
    (instr_type : string)
  : c_ret_type -> instr list = function
  | VOID -> caller_saved_after instr_type
  | NOT_VOID -> caller_saved_after_ret instr_type

let read_file
    (f : string)
  : string list =
  let ic = open_in f in
  let content = really_input_string ic (in_channel_length ic) in
  let _ = close_in ic in
  let content = List.filter (
    fun i -> String.trim i <> ""
  ) (String.split_on_char '\n' content) in
  content

let read_points
    ~(f : string)
  : string list =
  try
    let content = read_file f in
    let content_nocomments = List.mapi (
      fun i s -> remove_comment_from_point '#' s (i + 1)
    ) content in
    let content' = String.concat "\n" content_nocomments in
    let points = String.split_on_char ';' content' in
    let points = List.filter (
      fun i -> String.trim i <> ""
    ) points in
    points
  with Sys_error _ -> []

let count_char
    ~(s : string)
    ~(c : char)
  : int =
  String.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0 s

let strip_bracket
    (s : string)
  : string =
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
  | "AT" | "at" -> Some AT
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

let get_location
    (l : string)
  : location =
  if String.starts_with ~prefix:"0x" l then ADDRESS l
  else SYMBOL l

let create_range
    ~(_start : int)
    ~(_end : int)
  : location list =
  let rec cr (s : int) (i : int) (acc : location list) =
    if i >= 0 then cr (s+1) (i-1) (ADDRESS (dec_hex s) :: acc)
    else acc
  in
  cr _start (_end-_start) []

let extract_location_range
    (loc : string)
  : location list =
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
    (idx : int)
    (loc : string)
  : unit =
  let locations : location list =
    if contains ~str:loc ~sub:"-" then
      extract_location_range loc
    else
      (get_location loc) :: []
  in
  let locations_len = List.length locations in
  let visit_location
      (i : int)
      (location : location)
    : unit =
    match location with
    | ADDRESS a ->
      begin match action with
      | Some REPLACE ->
        begin
          if i = locations_len - 1 then
            let p' =
              ( int_of_string a,
                Some REPLACE,
                dir,
                stack,
                cmd,
                lang,
                code_ep,
                code,
                linenum,
                pidx + 1,
                filepath )
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
                "",
                linenum,
                pidx + 1,
                filepath )
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
            code,
            linenum,
            pidx + 1,
            filepath )
        in
        points_f1 := p' :: !points_f1
      end
    | SYMBOL s ->
      let s' =
        if s = "main" then main_symbol
        else s
      in
      match locm with
      | Some FUNENTRY ->
        begin match List.find_opt (fun f -> f.func_name = s') ufl with
        | Some s_func ->
          let p' =
            ( s_func.func_begin_addr,
              action,
              dir,
              stack,
              cmd,
              lang,
              code_ep,
              code,
              linenum,
              pidx + 1,
              filepath )
          in
          points_f1 := p' :: !points_f1
        | None -> ()
        end
      | Some FUNEXIT ->
        begin match List.find_opt (fun f -> f.func_name = s') ufl with
        | Some s_func ->
          let p' =
            ( s_func.func_end_addr,
              action,
              dir,
              stack,
              cmd,
              lang,
              code_ep,
              code,
              linenum,
              pidx + 1,
              filepath )
          in
          points_f1 := p' :: !points_f1
        | None -> ()
        end
      | Some CALLSITE ->
        begin match Hashtbl.find_opt fname2css s' with
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
                code,
                linenum,
                pidx + 1,
                filepath )
            in
            points_f1 := p' :: !points_f1
          ) css
        | None -> ()
        end
      | None -> failwith ("invalid loc-modifier at " ^ __LOC__)
  in
  let locations_len = List.length locations in
  List.iteri visit_location locations

let process_instrument_point
    (ufl : func list)
    (fname2css : (string, int list) Hashtbl.t)
    (instrs : instr list)
    (fbl : (string, bblock list) Hashtbl.t)
    (bbl : bblock list)
    (line : string)
    (pidx : int)
    (filepath : string)
    (main_symbol : string)
  : unit =
  let (linenum, point) = get_linenum_point line in
  if not (String.starts_with ~prefix:"user" point) &&
     not (String.starts_with ~prefix:"INCLUDE" point) then
    (* create a point for format 1 *)
    let ls' =
      let ls = String.trim point |> String.split_on_char '\"' in
      let ls0 = List.nth ls 0 |> String.trim |> String.split_on_char ' ' in
      let ls1 = List.nth ls 1 in
      let ls2 = List.nth ls 2 |> String.trim |> String.split_on_char ' ' in
      let quotes = count_char ~s:point ~c:'\"' in
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
          action
          dir
          locm
          stack''
          cmd
          lang
          code_ep
          code'
          ufl
          fname2css
          linenum
          pidx
          filepath
          main_symbol
      ) locations;
    | _ -> failwith ("invalid instrument format at " ^ __LOC__)
  else if String.starts_with ~prefix:"INCLUDE" point then
    let ls = String.trim point |> String.split_on_char '\"' in
    let cmd = List.nth ls 1 in
    ignore (Sys.command (cmd))
  else
    (* create a point for format 2 *)
    let ls' = String.trim point |> String.split_on_char ' ' in
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
        "(subdir instr_modules\n"
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

let parse_instrument_file
    ~(funcs : func list)
    ~(fname_callsites : (string, int list) Hashtbl.t)
    ~(instrs : instr list)
    ~(fbl : (string, bblock list) Hashtbl.t)
    ~(bbl : bblock list)
    ~(filepath : string)
    ~(main_symbol : string)
  : unit =
  let filelines =
    read_points ~f:filepath
    |> List.filter (fun s -> not (String.trim s = ""))
    |> List.map (fun s -> String.trim s)
  in
  List.iteri (
    fun i l ->
      process_instrument_point
        funcs
        fname_callsites
        instrs
        fbl
        bbl
        l
        i
        filepath
        main_symbol
  ) filelines;
  let sorted = List.sort (
    fun (addr1, _, _, _, _, _, _, _, _, _, _)
        (addr2, _, _, _, _, _, _, _, _, _, _)
    -> compare addr1 addr2
  ) !points_f1 in
  (* group consecutive elements with the same address *)
  let rec group_by_addr acc current_group = function
    | [] ->
        (match current_group with
         | [] -> List.rev acc
         | _ -> List.rev (List.rev current_group :: acc))
    | ((addr, _, _, _, _, _, _, _, _, _, _) as hd) :: tl ->
        (match current_group with
         | [] ->
             group_by_addr acc [hd] tl
         | (addr_prev, _, _, _, _, _, _, _, _, _, _) :: _ ->
             if addr = addr_prev then
               group_by_addr acc (hd :: current_group) tl
             else
               group_by_addr (List.rev current_group :: acc) [hd] tl)
  in
  points_f1' := group_by_addr [] [] sorted

let parse_instrument
    ~(funcs : func list)
    ~(fname_callsites : (string, int list) Hashtbl.t)
    ~(instrs : instr list)
    ~(fbl : (string, bblock list) Hashtbl.t)
    ~(bbl : bblock list)
  : unit =
  let main_symbol = List.hd (read_file "main.info") in
  (* points folder contains the instrumentation files *)
  let files = Sys.readdir "points"
              |> Array.to_list
              |> List.filter (fun f -> f <> ".gitignore") (* so git track the points folder *)
              |> Array.of_list
  in
  Array.iter (fun f ->
    let filepath = Filename.concat "points" f in
    parse_instrument_file
      ~funcs
      ~fname_callsites
      ~instrs
      ~fbl
      ~bbl
      ~filepath
      ~main_symbol
  ) files

let arg_order_arm
    (i : int)
  : arm_reg =
  match i with
  | 0 -> Arm_CommonReg R0
  | 1 -> Arm_CommonReg R1
  | 2 -> Arm_CommonReg R2
  | 3 -> Arm_CommonReg R3
  | x -> failwith (
          (string_of_int x)
           ^ " is too many arguments"
           ^ "Failure at " ^ __LOC__)

let arg_order_64
    (i : int)
  : intel_reg =
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

let set_arg
    (instr_type : string)
    (i : int)
    (arg : exp)
  : instr list =
  let module EU = ELF_utils in
  let _type, _value = arg in
  let exp_type =
    match _type with
    | "int" ->
      ( try Const (Normal (int_of_string _value))
      with _ -> Label _value )
    | "char*" | "char *" | "int*" | "int *" | "void*" | "void *" ->
      Label _value
    | "print-arg" ->
      if EU.elf_32 () then
        failwith ("for PRINTARGS 32-bits, should not reach here. Failure at "
                  ^ __LOC__)
      else
        Reg (Intel_Reg (arg_order_64 (int_of_string _value)))
  in
  let opcode =
    if EU.elf_32 () then
      Intel_OP (Intel_StackOP PUSH)
    else
      match _type with
      | "void*" | "void *" ->
        Intel_OP (Intel_CommonOP (Intel_Assign LEA))
      | _ ->
        Intel_OP (Intel_CommonOP (Intel_Assign MOV))
  in
    if EU.elf_arm () then
    match arg with
    | Label _ ->
      let dest_arg_reg = arg_order_arm i in
      [
        TripleInstr
          ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), None, None),
            arg,
            Reg (Arm_Reg dest_arg_reg),
            stub_loc,
            None,
            None,
            (create_comment instr_type) );
        TripleInstr
          ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), None, None),
            Ptr (UnOP (Arm_Reg dest_arg_reg)),
            Reg (Arm_Reg dest_arg_reg),
            stub_loc,
            None,
            None,
            (create_comment instr_type) )
      ]
    | Reg _ ->
      let dest_arg_reg = arg_order_arm i in
      [
        TripleInstr
          ( Arm_OP (Arm_CommonOP (Arm_Assign MOV), None, None),
            arg,
            Reg (Arm_Reg dest_arg_reg),
            stub_loc,
            None,
            None,
            (create_comment instr_type) )
      ]
  else if EU.elf_32 () then
    [ DoubleInstr
      ( opcode,
        exp_type,
        stub_loc,
        None,
        None,
        (create_comment instr_type) ) ]
  else
    let dest_arg_reg = arg_order_64 i in
    [ TripleInstr
      ( opcode,
        Reg (Intel_Reg dest_arg_reg),
        exp_type,
        stub_loc,
        None,
        None,
        (create_comment instr_type) ) ]

(** update stack since for 32-bit arguments are passed on the stack *)
(*
let clean_arg_32 (instr_type : string) (arg : exp) : instr =
  TripleInstr
    ( Intel_OP (Intel_CommonOP (Intel_Arithm ADD)),
      Reg (Intel_Reg (Intel_StackReg ESP)),
      Const (Normal 4),
      stub_loc,
      None,
      (create_comment instr_type) )
*)

(** insert call with arguments *)
let add_call_seq_arg
    ~(instr_type : string)
    ~(code_ep : string)
    ~(args : (string * string) list)
    ~(ret_type : c_ret_type)
  : instr list =
  let module EU = ELF_utils in
  let parse_arg arg =
    let _type, _value = arg in
    match _type with
    | "int" ->
      if EU.elf_arm () then
          Label ("="^_value)
      else
        ( try Const (Normal (int_of_string _value))
          with _ -> Label _value)
    | "char*" | "char *" | "int*" | "int *" ->
      if EU.elf_arm () then
        Label ("="^_value)
      else
        Label _value
    | "print-arg" ->
      if EU.elf_arm () then
        Reg (Arm_Reg (arg_order_arm (int_of_string _value)))
      else if EU.elf_32 () then
        failwith ("for PRINTARGS 32-bits, should not reach here. Failure at "
                  ^ __LOC__)
      else
        Reg (Intel_Reg (arg_order_64 (int_of_string _value)))
  in
  let args' = List.map (
    parse_arg
  ) args
  in
  if EU.elf_arm () then
    (* first four arguments are on the stack *)
    List.rev
      ( (caller_saved_before_regs instr_type ret_type)
      @ List.flatten ( List.mapi (
        set_arg instr_type
      ) args )
      @ [ DoubleInstr
          ( Arm_OP (Arm_ControlOP (BL), None, None),
            Symbol (
              CallDes {
                func_name=code_ep;
                func_begin_addr=0;
                func_end_addr=0;
                is_lib=false;
              }
            ),
            stub_loc,
            None,
            None,
            (create_comment instr_type) ) ]
    @ (caller_saved_after_regs instr_type ret_type) )
  else if EU.elf_32 () then
    List.rev
      ( (caller_saved_before_regs instr_type ret_type)
      @ List.flatten ( List.mapi (
        set_arg instr_type
      ) args' )
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
            stub_loc,
            None,
            None,
            (create_comment instr_type) ) ]
    @ [ TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Arithm ADD)),
          Reg (Intel_Reg (Intel_StackReg ESP)),
          Const (Normal (4 * List.length args)),
          stub_loc,
          None,
          None,
          (create_comment instr_type) ) ] (* arguments on stack for 32-bits *)
    @ (caller_saved_after_regs instr_type ret_type) )
  else
    List.rev
      ( (caller_saved_before_regs instr_type ret_type)
      @ List.flatten ( List.mapi (
        set_arg instr_type
      ) args
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
            stub_loc,
            None,
            None,
            (create_comment instr_type) ) ]
    @ (caller_saved_after_regs instr_type ret_type) )

(** insert call without arguments *)
let add_call_seq
    ~(instr_type : string)
    ~(code_ep : string)
    ~(use_existing_arg : bool)
    ~(ret_type : c_ret_type)
  : instr list =
  let module EU = ELF_utils in
  (** 32-bit x86 calling convention pushes arguments on the stack,
    * so if we save caller-saved registers on the stack before the call
    * the argument stack is messed up *)
if EU.elf_arm () then
    List.rev
    ( (caller_saved_before_regs instr_type ret_type)
    @ [ DoubleInstr
        ( Arm_OP (Arm_ControlOP (BL), None, None),
          Symbol (
            CallDes {
              func_name=code_ep;
              func_begin_addr=0;
              func_end_addr=0;
              is_lib=false;
            }
          ),
          stub_loc,
          None,
          None,
          (create_comment instr_type) ) ]
    @ (caller_saved_after_regs instr_type ret_type) )
else if EU.elf_32 () && use_existing_arg then
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
          stub_loc,
          None,
          None,
          (create_comment instr_type) ) ] )
  else
    List.rev
    ( (caller_saved_before_regs instr_type ret_type)
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
          stub_loc,
          None,
          None,
          (create_comment instr_type) ) ]
    @ (caller_saved_after_regs instr_type ret_type) )

let print_args
    ~(instr_type : string)
    ~(args : (string * string) list)
  : instr list =
  let module EU = ELF_utils in
  let rec call_with_arg
      (instr_type : string)
      (args : (string * string) list)
      (i : int)
      (acc : instr list)
    : instr list =
    match args with
    | [] -> List.rev acc
    | (t, v) :: rest ->
      if EU.elf_arm () then
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
                ~instr_type
                ~code_ep
                ~args:[("print-arg", arg_i)]
                ~ret_type:VOID )
        in
        call_with_arg instr_type rest (i+1) (call_seq @ acc)
      else if EU.elf_32 () then
        let code_ep =
          match t with
          (* function to call to print the arg, located in lib.c *)
          | "int" -> "print_int_arg"
          | "char*" | "char *" -> "print_char_star_arg"
          | "int*" | "int *" -> "print_int_star_arg"
        in
        let esp_arg_i = 4 * (i + 1) in
        let call_seq = [ DoubleInstr
          ( Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg EAX)),
            stub_loc,
            None,
            None,
            (create_comment instr_type) );
          TripleInstr
          ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
            Reg (Intel_Reg (Intel_CommonReg EAX)),
            Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg ESP), esp_arg_i)),
            stub_loc,
            None,
            None,
            (create_comment instr_type) );
          DoubleInstr
          ( Intel_OP (Intel_StackOP (PUSH)),
            Reg (Intel_Reg (Intel_CommonReg EAX)),
            stub_loc,
            None,
            None,
            (create_comment instr_type) );
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
            None,
            None,
            (create_comment instr_type) );
          DoubleInstr
            ( Intel_OP (Intel_StackOP (POP)),
              Reg (Intel_Reg (Intel_CommonReg EAX)),
              stub_loc,
              None,
              None,
              (create_comment instr_type) );
          DoubleInstr
            ( Intel_OP (Intel_StackOP (POP)),
              Reg (Intel_Reg (Intel_CommonReg EAX)),
              stub_loc,
              None,
              None,
              (create_comment instr_type) ) ]
        in
        call_with_arg instr_type rest (i+1) (call_seq @ acc)
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
                ~instr_type
                ~code_ep
                ~args:[("print-arg", arg_i)]
                ~ret_type:VOID )
        in
        call_with_arg instr_type rest (i+1) (call_seq @ acc)
  in
  call_with_arg instr_type args 0 []

let get_ret_type
    (code : string)
    (code_ep : string)
  : c_ret_type =
  let lines = read_lines code in
  let rec find_code_ep
      (code_ep : string)
    : string list -> c_ret_type = function
    | [] -> failwith ("code entry point not found. Failure at "
                      ^ __LOC__)
    | line :: rest -> begin
      let line_trimmed = String.trim line in
      if contains ~str:line_trimmed ~sub:(" " ^ code_ep ^ " ") then
        match String.split_on_char ' ' line_trimmed with
        | ret_type :: line_rest ->
          begin match ret_type with
          | "void" -> VOID
          | _ -> NOT_VOID
          end
      else find_code_ep code_ep rest
    end
  in
  find_code_ep code_ep lines

let process_point
    ~(instr_type : string)
    ~(cmd : string)
    ~(code_ep : string)
    ~(code : string)
    ~(stack : (string * string) list)
    ~(use_existing_arg : bool)
  : instr list =
  let module EU = ELF_utils in
  if cmd = "" && code_ep = "x" && code = "x" then begin
    (* action PRINTARGS *)
    if EU.elf_arm () then
      ignore (Sys.command ("arm-linux-gnueabihf-gcc -no-pie -c ./instr_modules/c/lib.c"))
    else if EU.elf_32 () then
      ignore (Sys.command ("gcc -no-pie -c ./instr_modules/c/lib.c -m32"))
    else
      ignore (Sys.command ("gcc -no-pie -c ./instr_modules/c/lib.c"));
    match stack with
    | [] ->
      failwith
        ("number of arguments and their types need to be specified in stack. "
         ^ "Failure at " ^ __LOC__)
    | args ->
      print_args ~instr_type ~args
  end else if String.ends_with ~suffix:".c" code then begin
    let ret_type : c_ret_type = get_ret_type code code_ep in
    match stack with
    | [] -> add_call_seq
              ~instr_type
              ~code_ep
              ~use_existing_arg
              ~ret_type
    | _ -> add_call_seq_arg
            ~instr_type
            ~code_ep
            ~args:stack
            ~ret_type
  end else if String.ends_with ~suffix:".asm" code then begin
    let ail_parser = new ailParser in
    let asm = read_lines code in
    let _ = ail_parser#process_asms asm "intel" in
    let asm = ail_parser#get_instrs in
    List.map (
      fun i ->
        add_comment i instr_type
    ) asm
  end else begin
    (* asm provided in instrumentation point *)
    let ail_parser = new ailParser in
    let asm = String.split_on_char '\n' code in
    let _ = ail_parser#process_asms asm "intel" in
    let asm = ail_parser#get_instrs in
    List.map (
      fun i ->
        add_comment i instr_type
    ) asm
  end

let instrument_at_instr
    (instr :instr)
    (p_list: point_f1 list)
  : (instr list) =
  let include_instr : bool ref = ref true in
  let rec add
      (p_list : point_f1 list)
      (acc_before : instr list)
      (acc_after : instr list)
    : instr list =
    match p_list with
    | [] ->
      if !include_instr then
        acc_after @ [instr] @ acc_before
      else
        (* for actions DELETE and REPLACE *)
        acc_after @ acc_before
    | ph :: pt -> begin
        let ( p_addr, p_action, p_dir, p_stack, p_cmd,
              p_lang, p_code_ep, p_code, p_linenum, p_idx, p_fp ) = ph in
        let comment : string = "    filename: " ^ p_fp
                               ^ ", line: " ^ p_linenum
                               ^ ", instrumentation point: "
                               ^ string_of_int p_idx in
        if p_cmd <> "" then
          ( ignore (Sys.command (p_cmd));
            print_endline ("^ executing command: " ^ p_cmd) );
        match p_action with
        | Some INSERT ->
          let add_seq : instr list =
            process_point
              ~instr_type:comment
              ~cmd:p_cmd
              ~code_ep:p_code_ep
              ~code:p_code
              ~stack:[]
              ~use_existing_arg: false
          in
          let acc_before' : instr list =
            match p_dir with
            | Some BEFORE | None -> add_seq @ acc_before
            | Some AFTER -> acc_before
            | Some AT ->
              let ih_lab = get_label instr in
              let add_seq_len = List.length add_seq in
              let ih' = update_label instr "" in
              let add_seq' = List.mapi (
                fun i asi ->
                  if i = add_seq_len - 1 then update_label asi ih_lab
                  else asi
              ) add_seq in
              add_seq' @ acc_before
          in
          let acc_after' : instr list =
            match p_dir with
            | Some BEFORE | None -> acc_after
            | Some AFTER -> add_seq @ acc_after
            | Some AT -> acc_after
          in
          add pt acc_before' acc_after'
        | Some INSERTCALL ->
          let add_seq : instr list =
            process_point
              ~instr_type:comment
              ~cmd:p_cmd
              ~code_ep:p_code_ep
              ~code:p_code
              ~stack:p_stack
              ~use_existing_arg: true
          in
          let acc_before' : instr list =
            match p_dir with
            | Some BEFORE | None -> add_seq @ acc_before
            | Some AFTER -> acc_before
            | Some AT ->
              let ih_lab = get_label instr in
              let add_seq_len = List.length add_seq in
              let ih' = update_label instr "" in
              let add_seq' = List.mapi (
                fun i asi ->
                  if i = add_seq_len - 1 then update_label asi ih_lab
                  else asi
              ) add_seq in
              add_seq' @ acc_before
          in
          let acc_after' : instr list =
            match p_dir with
            | Some BEFORE | None -> acc_after
            | Some AFTER -> add_seq @ acc_after
            | Some AT -> acc_after
          in
          add pt acc_before' acc_after'
        | Some DELETE -> begin
            include_instr := false;
            add pt acc_before acc_after
          end
        | Some REPLACE -> begin
            let add_seq : instr list =
              process_point
                ~instr_type:comment
                ~cmd:p_cmd
                ~code_ep:p_code_ep
                ~code:p_code
                ~stack:[]
                ~use_existing_arg:false
            in
            let ih_lab = get_label instr in
            let add_seq_len = List.length add_seq in
            let add_seq' = List.mapi (
              fun i asi ->
                if i = add_seq_len - 1 then update_label asi ih_lab
                else asi
            ) add_seq in
            include_instr := false;
            add pt (acc_before @ add_seq') acc_after
          end
        | Some PRINTARGS ->
          let add_seq : instr list =
            process_point
              ~instr_type:comment
              ~cmd:""
              ~code_ep:p_code_ep
              ~code:p_code
              ~stack:p_stack
              ~use_existing_arg:false
          in
          let acc_before' : instr list =
            match p_dir with
            | Some BEFORE | None -> add_seq @ acc_before
            | Some AT ->
              let ih_lab = get_label instr in
              let add_seq_len = List.length add_seq in
              let ih' = update_label instr "" in
              let add_seq' = List.mapi (
                fun i asi ->
                  if i = add_seq_len - 1 then update_label asi ih_lab
                  else asi
              ) add_seq in
              add_seq' @ acc_before
            | Some AFTER ->
              failwith
                ("for PRINTARGS, can only insert before a call, not after."
                  ^ " Failure at " ^ __LOC__)
          in
          add pt acc_before' acc_after
        | _ ->
          add pt acc_before acc_after
      end
    in
    add p_list [] []

let apply
    ~(instrs : instr list)
    ~(fbl : (string, bblock list) Hashtbl.t)
    ~(bbl : bblock list)
    ~(funcs : func list)
    ~(fname_callsites : (string, int list) Hashtbl.t)
  : instr list =
  (* populate points_f1 and points_f2 *)
  parse_instrument ~funcs ~fname_callsites ~instrs ~fbl ~bbl;
  let rec add_instrumentation_f1
      (instrs : instr list)
      (points_f1' : point_f1 list list)
      (acc : instr list)
    : instr list =
    match instrs, points_f1' with
    | ([], _) -> List.rev acc
    | (ih :: it, []) -> add_instrumentation_f1 it [] (ih :: acc)
    | (ih :: it, ph_list :: pt) ->
      let ph = List.hd ph_list in
      let ih_loc : loc = get_loc ih in
      let ih_addr : int = ih_loc.loc_addr in
      let ( p_addr, _, _, _, _, _, _, _, _, _, _ ) = ph in
      if ih_addr > p_addr then
        add_instrumentation_f1 instrs pt acc
      else if ih_addr < p_addr then
        add_instrumentation_f1 it points_f1' (ih :: acc)
      else
        let acc_ih = instrument_at_instr ih ph_list in
        add_instrumentation_f1 it pt (acc_ih @ acc)
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
  let il = add_instrumentation_f1 instrs !points_f1' [] in
  let il = add_instrumentation_f2 il !points_f2 in
  (* remove modifications made to dune just for the instrumentation *)
  let _ = ignore (Sys.command ("git checkout dune")) in
  il
