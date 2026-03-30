open Pp_print
open Type
open Utils

let compare_loc (l1 : loc) (l2 : loc) : bool =
  l1.loc_addr = l2.loc_addr && l1.loc_label = l2.loc_label

let print_loclist (ll : loc list) : unit =
  List.iter
    (fun l ->
      print_string l.loc_label;
      print_string "\n";
      print_string (dec_hex l.loc_addr ^ "\n"))
    ll

(* sort loc lost in ascending order *)
let sort_loc (ll : loc list) : loc list =
  List.sort (fun l1 l2 -> l1.loc_addr - l2.loc_addr) ll

(* suppose locs are sorted in ascending odder, and no same element exists
   we assume no instructions share same location! *)
let get_instr_byloc (instrs : instr list) (locs : loc list) : instr list =
  let rec aux il ll acc =
    match (il, ll) with
    | ih :: it, lh :: lt -> begin
        let iloc = get_loc ih in
        if iloc.loc_addr < lh.loc_addr then aux it ll acc
        else if iloc.loc_addr = lh.loc_addr then aux it lt (ih :: acc)
        else assert false
      end
    | _, [] -> List.rev acc
    | _ -> assert false
  in
  aux instrs locs []

let recover_addr_from_label (s : string) : int =
  try
    assert (contains ~str:s ~sub:"S_0x");
    (* S_0x80509E4 *)
    let s' = String.sub s 2 9 in
    int_of_string s'
  with _ -> -1

let stub_loc : loc = { loc_label = ""; loc_addr = 0; loc_visible = true }

(* get byte value, '.byte 0x50' returns '50' *)
let get_v s = String.sub s 8 2

(* read data bytes files, the _split.info files *)
let collect name =
  if Sys.file_exists name then begin
    let lines = read_file name in
    List.map String.trim lines
  end
  else []

let get_next_bb sn =
  assert (contains ~str:sn ~sub:"BB_");
  (* S_0x80509E4 *)
  (*
  let split = Str.split (Str.regexp_string "_") in
  let items = split sn in
  let index = List.nth items 1 in
   *)
  let index = String.sub sn 3 (String.length sn - 3) in
  let index' = int_of_string index + 1 in
  "BB_" ^ string_of_int index'

(* some functions manipulating basic blocks *)
let get_bbl cfg = (unify_str_list << List.map fst) cfg

(*get all the instructions within one function as a list*)
let f_instrs f instrs =
  let baddr = f.func_begin_addr and eaddr = f.func_end_addr in
  let rec help il acc =
    match il with
    | h :: t -> begin
        let loc = get_loc h in
        if loc.loc_addr >= baddr && loc.loc_addr < eaddr then help t (h :: acc)
        else help t acc
      end
    | [] -> List.rev acc
  in
  help instrs []

let bb_successors cfg b =
  let aux acc edge =
    match edge with
    | _, (_, None) -> acc
    | _, (_, Some s) when s = "T" -> acc
    | _, (_, Some s) when s = "INTER" -> acc
    | _, (_, Some s) when s = "RET" -> acc
    | s, (_, Some d) when s = b -> d :: acc
    | _ -> acc
  in
  let dl = List.fold_left aux [] cfg in
  unify_str_list dl

let bb_predecessors cfg b =
  let aux acc edge =
    match edge with
    | _, (_, None) -> acc
    | _, (_, Some s) when s = "T" -> acc
    | _, (_, Some s) when s = "INTER" -> acc
    | _, (_, Some s) when s = "RET" -> acc
    | s, (_, Some d) when d = b -> s :: acc
    | _ -> acc
  in
  let sl = List.fold_left aux [] cfg in
  unify_str_list sl

(*get all the instructions within one basic block as a list*)
let bb_instrs b instrs =
  let bloc = b.bblock_begin_loc and eloc = b.bblock_end_loc in
  let rec help il acc =
    match il with
    | h :: t -> begin
        let loc = get_loc h in
        if loc.loc_addr >= bloc.loc_addr && loc.loc_addr <= eloc.loc_addr then
          help t (h :: acc)
        else help t acc
      end
    | [] -> List.rev acc
  in
  help instrs []

let bbn_byloc e ls =
  (* implement a binary search, the original way seems too slow *)
  let rec bs min max =
    if min > max then false
    else
      let mid = (min + max) / 2 in
      let mid_num = Array.get ls mid in
      if e = mid_num then true
      else if e < mid_num then bs min (mid - 1)
      else bs (mid + 1) max
  in
  bs 0 (Array.length ls - 1)

module Opcode_utils = struct
  let is_ct (op : op) : bool =
    match op with
    | Intel_OP io -> (
        match io with
        | Intel_ControlOP ico -> (
            match ico with Intel_Jump _ -> true | _ -> false)
        | _ -> false)
    | Arm_OP (ao, _, _) -> (
        match ao with
        (* TODO: Add more ARM control operations. *)
        | Arm_ControlOP B -> true
        | Arm_ControlOP CBNZ -> true
        | Arm_ControlOP CBZ -> true
        | Arm_Condition TBZ -> true
        | Arm_CommonOP (Arm_Assign TBNZ) -> true
        | _ -> false)
    | _ -> false

  let is_cond_jmp (op : op) : bool =
    match op with
    | Intel_OP io ->
        begin match io with
        | Intel_ControlOP ico ->
            begin match ico with
            | Intel_Jump JMP -> false
            | Intel_Jump JMPQ -> false
            | Intel_Jump _ -> true
            | _ -> false
            end
        | _ -> false
        end
    | Arm_OP (ao, cond, _) ->
        begin match ao with
        | Arm_ControlOP aco ->
            begin match aco with B when cond <> None -> true | _ -> false
            end
        | _ -> false
        end

  (* this is a conservative judge *)
  let is_assign : op -> bool = function
    | Intel_OP io ->
        begin match io with
        | Intel_CommonOP (Intel_Assign _) -> true
        | _ -> false
        end
    | Arm_OP _ -> failwith "unsupported arm opcode"

  let is_push (op : op) : bool =
    match op with
    | Intel_OP (Intel_StackOP PUSH) -> true
    | Arm_OP (Arm_StackOP PUSH, _, _) -> true
    | _ -> false

  let is_stack_op : op -> bool = function
    | Intel_OP io -> begin
        match io with
        | Intel_StackOP PUSH
        | Intel_StackOP PUSHL
        | Intel_StackOP PUSHF
        | Intel_StackOP POP
        | Intel_StackOP POPL
        | Intel_StackOP POPF
        | Intel_ControlOP LEAVE ->
            true
        | _ -> false
        end
    | Arm_OP (ao, _, _) ->
        begin match ao with Arm_StackOP _ -> true | _ -> false
        end
    | _ -> false

  let is_mov (op : intel_op) : bool =
    match op with
    | Intel_CommonOP (Intel_Assign MOV) | Intel_CommonOP (Intel_Assign MOVL) ->
        true
    | _ -> false

  let is_call (op : op) : bool =
    match op with
    | Intel_OP iop ->
        begin match iop with
        | Intel_ControlOP c ->
            begin match c with CALL -> true | CALLQ -> true | _ -> false
            end
        | _ -> false
        end
    | Arm_OP (ao, _, _) ->
        begin match ao with
        | Arm_ControlOP aco ->
            begin match aco with BL | BLX -> true | _ -> false
            end
        | _ -> false
        end
    | _ -> false

  let is_cmp_op (op : intel_op) : bool =
    match op with Intel_CommonOP (Intel_Compare _) -> true | _ -> false
end

module Exp_utils = struct
  let is_reg (e : exp) : bool = match e with Reg _ -> true | _ -> false
  let is_const (e : exp) : bool = match e with Const _ -> true | _ -> false
  let is_mem (e : exp) : bool = match e with Ptr _ -> true | _ -> false

  let is_func (e : exp) : bool =
    match e with Symbol (CallDes _) -> true | _ -> false

  let is_indirect : exp -> bool = function
    | Symbol s -> begin
        match s with StarDes _ -> true | _ -> false
        (* this is triky:  repz ret could be parsed into
         *        ((op repz) (exp ret))
         *  and the post_process.py script will translate into
         *      label :  repz
         *                ret
         *  which is a typical indirect control flow transfer
         *  Let's ad-hoc identify it in this way
         *)
        (* | Label s -> s = "ret" || s = "retq" *)
        end
    | _ -> false

  let rec is_mem_exp (e : exp) : bool =
    match e with
    | Ptr _ -> true
    | Symbol (StarDes e) -> is_mem_exp e
    | _ -> false

  let print_exp_type (e : exp) : unit =
    match e with
    | Const _ -> print_string "const"
    | Symbol _ -> print_string "symbol"
    | Reg _ -> print_string "reg"
    | Assist _ -> print_string "assist"
    | Ptr _ -> print_string "ptr"
    | Label _ -> print_string "label"
end

module Instr_utils = struct
  let get_prefix (i : instr) : prefix option =
    match i with
    | SingleInstr (_, _, pre, _) -> pre
    | DoubleInstr (_, _, _, pre, _) -> pre
    | TripleInstr (_, _, _, _, pre, _) -> pre
    | FourInstr (_, _, _, _, _, pre, _) -> pre
    | FifInstr (_, _, _, _, _, _, pre, _) -> pre
    | SixInstr (_, _, _, _, _, _, _, pre, _) -> pre
    | SevenInstr (_, _, _, _, _, _, _, _, pre, _) -> pre

  let get_loc (i : instr) : loc =
    match i with
    | SingleInstr (_, l, _, _) -> l
    | DoubleInstr (_, _, l, _, _) -> l
    | TripleInstr (_, _, _, l, _, _) -> l
    | FourInstr (_, _, _, _, l, _, _) -> l
    | FifInstr (_, _, _, _, _, l, _, _) -> l
    | SixInstr (_, _, _, _, _, _, l, _, _) -> l
    | SevenInstr (_, _, _, _, _, _, _, l, _, _) -> l

  let set_loc (i : instr) (l : loc) : instr =
    match i with
    | SingleInstr (p, _, pre, tags) -> SingleInstr (p, l, pre, tags)
    | DoubleInstr (p, e, _, pre, tags) -> DoubleInstr (p, e, l, pre, tags)
    | TripleInstr (p, e1, e2, _, pre, tags) ->
        TripleInstr (p, e1, e2, l, pre, tags)
    | FourInstr (p, e1, e2, e3, _, pre, tags) ->
        FourInstr (p, e1, e2, e3, l, pre, tags)
    | FifInstr (p, e1, e2, e3, e4, _, pre, tags) ->
        FifInstr (p, e1, e2, e3, e4, l, pre, tags)
    | SixInstr (p, e1, e2, e3, e4, e5, _, pre, tags) ->
        SixInstr (p, e1, e2, e3, e4, e5, l, pre, tags)
    | SevenInstr (p, e1, e2, e3, e4, e5, e6, _, pre, tags) ->
        SevenInstr (p, e1, e2, e3, e4, e5, e6, l, pre, tags)

  let update_label (i : instr) (l : string) : instr =
    let loc = get_loc i in
    set_loc i { loc with loc_label = l }

  let get_addr (i : instr) : int =
    let l = get_loc i in
    l.loc_addr

  let get_label (i : instr) : string =
    let l = get_loc i in
    l.loc_label

  let get_op (i : instr) : op =
    match i with
    | SingleInstr (p, _, _, _) -> p
    | DoubleInstr (p, _, _, _, _) -> p
    | TripleInstr (p, _, _, _, _, _) -> p
    | FourInstr (p, _, _, _, _, _, _) -> p
    | FifInstr (p, _, _, _, _, _, _, _) -> p
    | SixInstr (p, _, _, _, _, _, _, _, _) -> p
    | SevenInstr (p, _, _, _, _, _, _, _, _, _) -> p

  let change_op (i : instr) (new_op : op) : instr =
    match i with
    | SingleInstr (_, l, pre, tags) -> SingleInstr (new_op, l, pre, tags)
    | DoubleInstr (_, e, l, pre, tags) -> DoubleInstr (new_op, e, l, pre, tags)
    | TripleInstr (_, e1, e2, l, pre, tags) ->
        TripleInstr (new_op, e1, e2, l, pre, tags)
    | FourInstr (_, e1, e2, e3, l, pre, tags) ->
        FourInstr (new_op, e1, e2, e3, l, pre, tags)
    | FifInstr (_, e1, e2, e3, e4, l, pre, tags) ->
        FifInstr (new_op, e1, e2, e3, e4, l, pre, tags)
    | SixInstr (_, e1, e2, e3, e4, e5, l, pre, tags) ->
        SixInstr (new_op, e1, e2, e3, e4, e5, l, pre, tags)
    | SevenInstr (_, e1, e2, e3, e4, e5, e6, l, pre, tags) ->
        SevenInstr (new_op, e1, e2, e3, e4, e5, e6, l, pre, tags)

  let change_condsuff (i : instr) (new_condsuff : arm_condsuff option) : instr =
    let op = get_op i in
    match op with
    | Arm_OP (aop, _, width) ->
        let new_op = Arm_OP (aop, new_condsuff, width) in
        change_op i new_op
    | _ -> failwith "unsupported opcode"

  let get_cf_des (i : instr) : exp option =
    match i with DoubleInstr (_, e, _, _, _) -> Some e | _ -> None

  (** Get the first exp. e.g.: bx r1 -> r1 mov r0, r1 -> r1 add r1, r2, r3 -> r3
  *)
  let get_exp_1 (i : instr) : exp =
    match i with
    | SingleInstr (_, _, _, _) -> failwith "undefined expression"
    | DoubleInstr (_, e, _, _, _) -> e
    | TripleInstr (_, e, _, _, _, _) -> e
    | FourInstr (_, e, _, _, _, _, _) -> e
    | FifInstr (_, e, _, _, _, _, _, _) -> e
    | SixInstr (_, e, _, _, _, _, _, _, _) -> e
    | SevenInstr (_, e, _, _, _, _, _, _, _, _) -> e

  (** Get the second exp. e.g.: mov r0, r1 -> r0 add r1, r2, r3 -> r2 *)
  let get_exp_2 (i : instr) : exp =
    match i with
    | SingleInstr (_, _, _, _) -> failwith "undefined expression"
    | DoubleInstr (_, _, _, _, _) -> failwith "undefined expression"
    | TripleInstr (_, _, e, _, _, _) -> e
    | FourInstr (_, _, e, _, _, _, _) -> e
    | FifInstr (_, _, e, _, _, _, _, _) -> e
    | SixInstr (_, _, e, _, _, _, _, _, _) -> e
    | SevenInstr (_, _, e, _, _, _, _, _, _, _) -> e

  let has_lr_reg (i : instr) : bool =
    let exp = get_exp_1 i in
    match exp with
    | Label l -> if contains ~str:l ~sub:"lr" then true else false
    | Reg (Arm_Reg (Arm_LinkReg LR)) -> true
    | _ -> false

  let has_pc_reg (i : instr) : bool =
    let exp = get_exp_1 i in
    match exp with
    | Label l -> if contains ~str:l ~sub:"pc" then true else false
    | _ -> false

  let get_ct_des (i : instr) : int option =
    let module OU = Opcode_utils in
    match i with
    | DoubleInstr (p, e, l, _, _) when OU.is_ct p -> (
        match e with
        | Symbol (JumpDes d)
        | Const (Point d)
        | Const (Normal d)
        | Const (Immediate d) ->
            Some d
        | _ -> None)
    | TripleInstr (p, e1, e2, l, _, _) when OU.is_ct p -> (
        (* ARM: cbz r3,0x10428 *)
        match e1 with
        | Symbol (JumpDes d)
        | Const (Point d)
        | Const (Normal d)
        | Const (Immediate d) ->
            Some d
        | _ -> None)
    | FourInstr (p, e1, e2, e3, l, _, _) when OU.is_ct p -> (
        match e1 with
        | Symbol (JumpDes d)
        | Const (Point d)
        | Const (Normal d)
        | Const (Immediate d) ->
            Some d
        | _ -> None)
    | _ -> None

  let is_control_des (i : instr) : bool =
    let aux s = contains ~str:s ~sub:":" in
    get_label i |> aux

  let is_jmp (i : instr) : bool =
    match get_op i with
    | Intel_OP iop ->
        begin match iop with
        | Intel_ControlOP ico ->
            begin match ico with Intel_Jump JMP -> true | _ -> false
            end
        | _ -> false
        end
    | Arm_OP (ao, _, _) ->
        begin match ao with
        | Arm_ControlOP aco ->
            begin match aco with
            (* TODO: ARM: *)
            (* should be distinguished from "BX LR", which is a return. *)
            | B | BXJ -> true
            | BX when not (has_lr_reg i) -> true
            | _ -> false
            end
        | _ -> false
        end
    | _ -> false

  let is_ret (i : instr) : bool =
    match get_op i with
    | Intel_OP iop ->
        begin match iop with
        | Intel_ControlOP c ->
            begin match c with RET | RETN | RETQ -> true | _ -> false
            end
        | _ -> false
        end
    | Arm_OP (aop, _, _) ->
        begin match aop with
        | Arm_ControlOP aco ->
            begin match aco with
            (* TODO: ARM: This is naive because we need to check if BX uses LR register *)
            | BX when has_lr_reg i -> true
            | RET -> true
            | _ -> false
            end
        | Arm_StackOP aso ->
            begin match aso with
            (* TODO: ARM: This is naive because we need to check if POP uses PC register *)
            | POP when has_pc_reg i -> true
            | _ -> false
            end
        | _ -> false
        end
    | _ -> false

  let is_control_transfer_op (i : instr) : bool =
    let module OU = Opcode_utils in
    let op = get_op i in
    OU.is_call op || is_jmp i || OU.is_cond_jmp op || is_ret i

  let is_arm_ret (i : instr) : bool =
    match i with
    (* TODO: ARM: implement the case below: *)
    (* pop {r7, pc} *)
    | DoubleInstr (op, e, _, _, _) -> (
        match op with
        | Arm_OP (Arm_ControlOP BX, _, _) -> e = Reg (Arm_Reg (Arm_LinkReg LR))
        | Arm_OP (Arm_ControlOP RET, _, _) -> true
        | Arm_OP (Arm_StackOP POP, _, _) when has_pc_reg i -> true
        | _ -> false)
    | _ -> false

  let print_instr_type : instr -> unit = function
    | SingleInstr _ -> print_string "single instr"
    | DoubleInstr _ -> print_string "double instr"
    | TripleInstr _ -> print_string "triple instr"
    | FourInstr _ -> print_string "four instr"
    | FifInstr _ -> print_string "five instr"
    | SixInstr _ -> print_string "six instr"
    | SevenInstr _ -> print_string "seven instr"
end

module IL_utils = struct
  open Instr_utils

  let track_stack_var_fact (il : instr list) : instr list =
    (* stack in 32-bit intel binaries can change over the course of
       the function. This function find the adjusted stack variable
       offset for accurate fact propagation in the data-flow analysis *)
    (* this is specific to 32-bit intel binaries *)
    let rec traverse : instr list -> instr list = function
      | i1 :: i2 :: i3 :: rest -> begin
          match (i2, i3) with
          | ( TripleInstr
                ( Intel_OP (Intel_CommonOP (Intel_Arithm SUB)),
                  Reg (Intel_Reg (Intel_StackReg ESP)),
                  Const (Normal lab_i2),
                  loc_i2,
                  prefix_i2,
                  tags_i2 ),
              TripleInstr
                ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
                  Reg dest_reg_i3,
                  Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg ESP), const_i3)),
                  loc_i3,
                  prefix_i3,
                  tags_i3 ) )
            when p_op (get_op i1) <> "push" && p_op (get_op i1) <> "add" ->
            begin
              let new_tags = Hashtbl.create 0 in
              let _ =
                Hashtbl.replace new_tags "use"
                  (Exp
                     (Ptr
                        (BinOP_PLUS
                           (Intel_Reg (Intel_StackReg ESP), const_i3 - lab_i2))))
              in
              let _ = Hashtbl.replace new_tags "def" (Exp (Reg dest_reg_i3)) in
              let i3_new =
                TripleInstr
                  ( Intel_OP (Intel_CommonOP (Intel_Assign MOV)),
                    Reg dest_reg_i3,
                    Ptr (BinOP_PLUS (Intel_Reg (Intel_StackReg ESP), const_i3)),
                    loc_i3,
                    prefix_i3,
                    new_tags )
              in
              i1 :: traverse (i2 :: i3_new :: rest)
            end
          | _ -> i1 :: traverse (i2 :: i3 :: rest)
          end
      | rest -> rest
    in
    traverse il

  let perform_dfa_rewrites (addr2newi : (int, instr list) Hashtbl.t) il =
    List.concat_map
      (fun i ->
        let loc = get_loc i in
        Hashtbl.find_opt addr2newi loc.loc_addr |> Option.value ~default:[ i ])
      il
end

module Func_utils = struct
  open Instr_utils

  let get_userfuncs funcs = List.filter (fun f -> f.is_lib = false) funcs

  let unify_funclist_by_name (l : func list) : func list =
    let t = Hashtbl.create (List.length l * 2) in
    List.iter (fun f -> Hashtbl.replace t f.func_name f) l;
    Hashtbl.fold (fun k v s -> v :: s) t []

  (* this function must be called after the sort by addr*)
  let unify_funclist_by_addr (l : func list) : func list =
    let rec help l acc =
      match l with
      | [] -> []
      | h :: [] -> List.rev_append acc [ h ]
      | h1 :: h2 :: t ->
          begin if h1.func_begin_addr == h2.func_begin_addr then
            begin if contains ~str:h1.func_name ~sub:"S_0x" then
              (* print_string ("remove : "^(h1.func_name)^"\n"); *)
              help (h2 :: t) acc
            else if contains ~str:h2.func_name ~sub:"S_0x" then
              (* print_string ("remove : "^(h2.func_name)^"\n"); *)
              help (h1 :: t) acc
            else help (h2 :: t) (h1 :: acc)
            end
          else help (h2 :: t) (h1 :: acc)
          end
    in
    help l []

  let func_map fl instrs =
    let m = Hashtbl.create 50 in
    let rec aux bl il acc =
      match (bl, il) with
      | bh :: bt, ih :: it ->
          let ba = bh.func_begin_addr in
          let ea = bh.func_end_addr in
          let ia = get_addr ih in
          (* the end of a instr list *)
          if ia = ea then begin
            let n = bh.func_name in
            Hashtbl.replace m n (ih :: acc);
            aux bt it []
          end
          else if ia >= ba && ia < ea then
            (* the start of a new list *)
            aux bl it (ih :: acc)
          else aux bl it []
          (* failwith "undefined behavior in func map construction 1" *)
      | [], [] -> m
      | _, [] -> failwith "undefined behavior in bb map construction 2"
      | [], _ -> m
      (* failwith "undefined behavior in func map construction 3" *)
    in
    aux fl instrs []

  (* sort func list from smallest to largest;
     Note that we need to first filter out lib function *)
  let func_sort fl =
    fl
    |> List.filter (fun f -> f.is_lib = false)
    |> List.sort (fun b1 b2 -> b1.func_begin_addr - b2.func_begin_addr)

  let get_range f =
    let a1 = f.func_begin_addr in
    let a2 = f.func_end_addr in
    a1 - a2

  (* the first instruction is a target of control
     flow transfer  *)
  let start_with_fallthrough f fmap =
    let il = Hashtbl.find fmap f.func_name in
    let ifirst = List.nth il 0 in
    get_label ifirst <> ""

  (* the last instruction is a target of control
     flow transfer  *)
  let end_with_fallthrough f fmap =
    let il = Hashtbl.find fmap f.func_name in
    let ilast = List.nth il (List.length il - 1) in
    get_label ilast <> ""

  (* collect all the callers *)
  let caller_collect fn instrs =
    let module OU = Opcode_utils in
    let help acc i =
      match i with
      | DoubleInstr (p, e, _, _, _) when OU.is_call p ->
          let open Pp_print in
          let es = p_exp e in
          if contains ~str:es ~sub:fn then i :: acc else acc
      | _ -> acc
    in
    List.fold_left help [] instrs

  let refine_func_begin (funcs : func list) (il : instr list) : func list =
    (* in the earlier function slicing, we consider each initial got-assigning instruction
     as the function start, to help with the DFA. That step messes up the function
     starts and ends, which negatively affect the instrumentation. This function
     restore the original function starts and ends *)
    (* assigning initial got-assigning instruction as the function starts only
     happen to 32-bit ELF intel binaries *)
    (* this is specifically for instrumentation when the loc-modifier is funentry *)
    let module OU = Opcode_utils in
    let module IU = Instr_utils in
    let rec update funcs il acc =
      match (funcs, il) with
      | [], il' -> List.rev acc
      | funcs', [] -> List.rev acc
      | hf :: tf, hi :: ti ->
          let f_ea = hf.func_end_addr in
          let f_ba = hf.func_begin_addr in
          let i_loc = IU.get_loc hi in
          let i_addr = i_loc.loc_addr in
          begin if i_addr = f_ba then
            match hi with
            | TripleInstr (p, e1, e2, _, _, _) -> (
                match (p, e1, e2) with
                | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Label l, Reg r
                | Intel_OP (Intel_CommonOP (Intel_Arithm ADD)), Reg r, Label l
                  ->
                    if contains ~str:l ~sub:"$_GLOBAL_OFFSET_TABLE_" then
                      let prev_func = List.hd acc in
                      let acc' = List.tl acc in
                      let prev_func' =
                        { prev_func with func_end_addr = hf.func_end_addr }
                      in
                      update funcs ti (prev_func' :: acc')
                    else update funcs ti acc
                | _ -> update funcs ti acc)
            | _ -> update funcs ti acc
          else if i_addr >= f_ea then update tf il (hf :: acc)
          else if i_addr < f_ba then update funcs ti acc
          else update funcs ti acc
          end
    in
    update funcs il []

  let refine_func_end (funcs : func list) (il : instr list) : func list =
    (* the earlier function slicing over-approximates function boundaries.
     This function refine the function boundaries with the last final
     function-ending instruction before the start of the next function *)
    (* this is specifically for instrumentation when the loc-modifier is funexit *)
    let module IU = Instr_utils in
    let rec update funcs il acc =
      match (funcs, il) with
      | [], il' -> acc
      | funcs', [] -> acc
      | hf :: tf, hi :: ti ->
          let f_ea = hf.func_end_addr in
          let f_ba = hf.func_begin_addr in
          let i_loc = get_loc hi in
          let i_addr = i_loc.loc_addr in
          begin if i_addr >= f_ea then update tf il (hf :: acc)
          else if i_addr < f_ba then update funcs ti acc
          else if IU.is_ret hi then
            update ({ hf with func_end_addr = i_addr } :: tf) ti acc
          else update funcs ti acc
          end
    in
    update funcs il []

  let refine_func_boundary_by_cf (il : instr list) (fl : func list) : func list
      =
    let module IU = Instr_utils in
    let text_start, text_end =
      let line = List.hd (read_file "text_sec.info") in
      let parts = String.split_on_char ' ' line in
      let start = int_of_string ("0x" ^ List.nth parts 1) in
      let size = int_of_string ("0x" ^ List.nth parts 3) in
      (start, start + size)
    in
    let rec update fl il acc =
      match (fl, il) with
      | [], _ -> List.rev acc
      | fl', [] -> List.rev_append acc fl'
      | hf :: tf, hi :: ti ->
          let f_ea = hf.func_end_addr in
          let f_ba = hf.func_begin_addr in
          let i_addr = (get_loc hi).loc_addr in
          if i_addr >= f_ea then update tf il (hf :: acc)
          else if i_addr < f_ba then update fl ti acc
          else
            begin match IU.get_ct_des hi with
            | Some des when des >= f_ea && des >= text_start && des < text_end
              -> begin
                (* intra jump target is after function ends.
                   Merge with the next (or next-next) function if target falls in it *)
                (* each entry: (function to merge hf with, survivors after dropping up through it) *)
                let merge_targets =
                  match tf with
                  | f1 :: (f2 :: rest2 as rest1) -> [ (f1, rest1); (f2, rest2) ]
                  | [ f ] -> [ (f, []) ]
                  | [] -> []
                in
                let in_range f =
                  des >= f.func_begin_addr && des < f.func_end_addr
                in
                match
                  List.find_opt (fun (f, _) -> in_range f) merge_targets
                with
                | Some (f, rest) ->
                    let merged = { hf with func_end_addr = f.func_end_addr } in
                    update (merged :: rest) il acc
                | None -> update fl ti acc
              end
            | _ -> update fl ti acc
            end
    in
    update fl il []

  let pp_func2cfg func2cfg_table =
    let aux f (pred_cfg, succ_cfg, _) =
      Hashtbl.iter
        (fun i_from i_tos ->
          match (i_from, i_tos) with
          | Some i_from, i_tos -> begin
              print_endline (pp_print_instr' i_from ^ " p->");
              List.iter
                (fun i_to ->
                  match i_to with
                  | Some i_to -> print_endline (">>> " ^ pp_print_instr' i_to)
                  | None -> print_endline ">>> None")
                i_tos
            end
          | None, i_tos -> print_endline "")
        pred_cfg;
      Hashtbl.iter
        (fun i_from i_tos ->
          match (i_from, i_tos) with
          | Some i_from, i_tos -> begin
              print_endline (pp_print_instr' i_from ^ " s->");
              List.iter
                (fun i_to ->
                  match i_to with
                  | Some i_to -> print_endline (">>> " ^ pp_print_instr' i_to)
                  | None -> print_endline ">>> None")
                i_tos
            end
          | None, i_tos -> print_endline "")
        succ_cfg
    in
    (*aux "S_0x80541C5" (Hashtbl.find func2cfg_table "S_0x80541C5")*)
    Hashtbl.iter aux func2cfg_table

  let func2cfg (il : instr list) (funcs : func list) : (string, cfgi) Hashtbl.t
      =
    let module IU = Instr_utils in
    let is_commented_out i =
      let tags =
        match i with
        | SingleInstr (_, _, _, t) -> t
        | DoubleInstr (_, _, _, _, t) -> t
        | TripleInstr (_, _, _, _, _, t) -> t
        | FourInstr (_, _, _, _, _, _, t) -> t
        | FifInstr (_, _, _, _, _, _, _, t) -> t
        | SixInstr (_, _, _, _, _, _, _, _, t) -> t
        | SevenInstr (_, _, _, _, _, _, _, _, _, t) -> t
      in
      Hashtbl.mem tags "commented_out"
    in
    (* Exclude commented-out instructions (e.g. literal pool data marked by
       remove_literal_pools) from CFG construction and data-flow analysis.
       They are kept in ail_parser#get_instrs for pp_print output only. *)
    let il = List.filter (fun i -> not (is_commented_out i)) il in
    let il =
      List.sort
        (fun i1 i2 -> compare (get_loc i1).loc_addr (get_loc i2).loc_addr)
        il
    in
    let funcs =
      List.sort
        (fun f1 f2 -> compare f1.func_begin_addr f2.func_begin_addr)
        funcs
    in
    let fb2fn funcs d =
      (* find the function name with the function begin address *)
      let* f = List.find_opt (fun f -> f.func_begin_addr = d) funcs in
      Some f.func_name
    in
    let func2il il =
      let func2il_table = Hashtbl.create 40 in
      let worklist = Queue.create () in
      List.iter
        (fun i ->
          let i_addr = (get_loc i).loc_addr in
          match
            List.find_opt
              (fun f -> i_addr >= f.func_begin_addr && i_addr < f.func_end_addr)
              funcs
          with
          | Some f ->
              let il =
                Hashtbl.find_opt func2il_table f.func_name
                |> Option.value ~default:[]
              in
              Hashtbl.replace func2il_table f.func_name (i :: il);
              let _ =
                let* d = IU.get_ct_des i in
                if d >= f.func_end_addr then begin
                  let* fn = fb2fn funcs d in
                  Queue.push (f.func_name, fn) worklist;
                  Some ()
                end
                else Some ()
              in
              ()
          | None -> ())
        il;
      let func2il' = func2il_table in
      (* debug *)
      (* let _ = *)
      (*   Hashtbl.iter *)
      (*     (fun k v -> *)
      (*       Printf.printf "func: %s\n%!" k; *)
      (*       List.iter *)
      (*         (fun i -> Printf.printf "\t%s\n%!" (Pp_print.pp_print_instr' i)) *)
      (*         v) *)
      (*     func2il' *)
      (* in *)
      (* Instructions are reserve-ordered and will be adjusted in [create_cfg] *)
      (* end debug *)
      while not (Queue.is_empty worklist) do
        let fn, fn2 = Queue.pop worklist in
        ignore
          (let* f_il = Hashtbl.find_opt func2il' fn
           and* f_il2 = Hashtbl.find_opt func2il' fn2 in
           Hashtbl.replace func2il' fn (f_il @ f_il2);
           Hashtbl.remove func2il' fn2;
           Some ())
      done;
      func2il'
    in
    let add_edge curr_cfg (i_from : instr option) (i_to : instr option) :
        (instr option, instr option list) Hashtbl.t =
      let edges =
        Hashtbl.find_opt curr_cfg i_from |> Option.value ~default:[]
      in
      Hashtbl.replace curr_cfg i_from (i_to :: edges);
      curr_cfg
    in
    let func2cfg_table = Hashtbl.create 40 in
    let func2il_table = func2il il in

    let rec create_cfg f (f_il : instr list) pred_cfg succ_cfg =
      match f_il with
      | [] ->
          let ordered_il = List.rev (Hashtbl.find func2il_table f.func_name) in
          Hashtbl.replace func2cfg_table f.func_name
            { preds = pred_cfg; succs = succ_cfg; il = ordered_il }
      | i :: [] ->
          let succ_cfg = add_edge succ_cfg (Some i) None in
          let ordered_il = List.rev (Hashtbl.find func2il_table f.func_name) in
          Hashtbl.replace func2cfg_table f.func_name
            { preds = pred_cfg; succs = succ_cfg; il = ordered_il }
      | i :: i' :: il' ->
          let i_op = get_op i in
          let _ =
            match i_op with
            | Intel_OP (Intel_ControlOP (Intel_Jump JMP)) -> ()
            | Intel_OP (Intel_ControlOP RET)
            | Intel_OP (Intel_ControlOP RETN)
            | Intel_OP (Intel_ControlOP RETQ)
            | Intel_OP (Intel_ControlOP LEAVE)
            | Intel_OP (Intel_ControlOP LEAVEQ) ->
                ()
            | Arm_OP (Arm_ControlOP RET, _, _) -> ()
            | Arm_OP (Arm_StackOP POP, _, _) -> ()
            | Arm_OP (Arm_ControlOP B, None, _) -> ()
            | _ ->
                let pred_cfg = add_edge pred_cfg (Some i') (Some i) in
                let succ_cfg = add_edge succ_cfg (Some i) (Some i') in
                ()
          in
          let pred_cfg =
            match IU.get_ct_des i with
            | Some d -> begin
                let ils_full : instr list =
                  Hashtbl.find func2il_table f.func_name
                in
                let des_is : instr list =
                  List.filter (fun l -> (get_loc l).loc_addr = d) ils_full
                in
                match des_is with
                | [] -> pred_cfg
                | des_i :: [] -> add_edge pred_cfg (Some des_i) (Some i)
                | _ -> pred_cfg
              end
            | None -> pred_cfg
          in
          let succ_cfg =
            match IU.get_ct_des i with
            | Some d -> begin
                let ils_full : instr list =
                  Hashtbl.find func2il_table f.func_name
                in
                let des_is : instr list =
                  List.filter (fun l -> (get_loc l).loc_addr = d) ils_full
                in
                match des_is with
                | [] -> succ_cfg
                | des_i :: [] -> add_edge succ_cfg (Some i) (Some des_i)
                | _ -> succ_cfg
              end
            | None -> succ_cfg
          in
          create_cfg f (i' :: il') pred_cfg succ_cfg
    in
    (* function construction main starting here *)
    List.iter
      (fun f ->
        match Hashtbl.find_opt func2il_table f.func_name with
        | Some f_il ->
            create_cfg f (List.rev f_il) (Hashtbl.create 40) (Hashtbl.create 40)
        | None -> ())
      funcs;
    func2cfg_table
end

module Instrumentation_utils = struct
  open Opcode_utils
  open Instr_utils
  open Exp_utils

  let sort_il_update instrs_update =
    List.sort
      (fun (_, l1, _, _) (_, l2, _, _) -> l1.loc_addr - l2.loc_addr)
      instrs_update

  type instr_update = SUB | INSERT

  let set_update_fold i l acc = [ (i, l, INSERT, "") ] :: acc

  let sub_update_fold i l i' acc =
    let open Pp_print in
    let i_s = pp_print_instr i' in
    [ (i, l, SUB, i_s) ] :: acc

  let eliminate_label i = update_label i ""

  let tem i =
    let _, _, _, s = i in
    s

  (* update inserted instruction at the front of target instruction *)
  let update_instrs_infront instrs instrs_update =
    let same loc1 loc2 = loc1.loc_addr = loc2.loc_addr in
    let rec help l_u l acc =
      match (l_u, l) with
      | h_u :: t_u, h :: t -> begin
          let i, loc1, ty, i_s = h_u and loc2 = get_loc h in
          if same loc1 loc2 then
            begin match ty with
            | SUB -> begin
                let open Pp_print in
                let h_s = pp_print_instr h in
                if i_s = h_s then help t_u (i :: t) acc
                else help l_u t (h :: acc)
              end
            | INSERT -> help t_u (h :: t) (i :: acc)
            end
          else help l_u t (h :: acc)
        end
      | [], l' -> List.rev_append acc l'
      | h :: t, [] -> begin failwith "error in update_instrs" end
    in
    help instrs_update instrs []

  let insert_instrument_instrs il il_update =
    il_update |> sort_il_update |> update_instrs_infront il

  let sub_single_instr i' il_update il =
    let loc = get_loc i' in
    let acc =
      List.fold_left (fun acc i -> sub_update_fold i loc i' acc) il_update []
    in
    let acc' = List.flatten acc in
    insert_instrument_instrs il acc'

  let instrument_update il_update il = insert_instrument_instrs il il_update

  let instrument_instrs_func il il_update update_func =
    il_update |> sort_il_update |> update_func il

  let insert_instr_list d il_update il =
    match d with
    | BEFORE -> instrument_instrs_func il il_update update_instrs_infront
    | AFTER -> instrument_instrs_func il il_update update_instrs_infront

  let insert_single_instr d loc il_update il =
    let acc =
      List.fold_left (fun acc i -> set_update_fold i loc acc) [] il_update
    in
    let acc' = List.flatten acc in
    insert_instr_list d acc' il

  (* get basic block information from the last instruction of a basic
    block. This task has a quicker algorithm comparing with the more general
    task. Also, it is quick useful.
    *)
  let get_bb_by_last_instr i bbl =
    let addr = get_addr i in
    (*
      print_string @@ dec_hex addr;
      print_string "\n";
      *)
    List.find (fun b -> b.bblock_end_loc.loc_addr = addr) bbl

  (* generate one byte no-op padding instruction *)
  let gen_nop loc =
    SingleInstr
      (Intel_OP (Intel_CommonOP (Intel_Other NOP)), loc, None, Hashtbl.create 0)

  (* generate four bytes no-op padding instruction *)
  let gen_4_lea loc =
    TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
        Reg (Intel_Reg (Intel_CommonReg ESI)),
        Ptr (BinOP_PLUS (Intel_Reg (Intel_CommonReg ESI), 0)),
        loc,
        None,
        Hashtbl.create 0 )

  (* generate six bytes no-op padding instruction *)
  let gen_6_lea loc =
    TripleInstr
      ( Intel_OP (Intel_CommonOP (Intel_Assign LEA)),
        Reg (Intel_Reg (Intel_CommonReg EDI)),
        Ptr (BinOP_PLUS (Intel_Reg (Intel_CommonReg EDI), 0)),
        loc,
        None,
        Hashtbl.create 0 )

  let is_mem_write_instr i =
    let module OU = Opcode_utils in
    match i with
    | DoubleInstr (p, _, _, _, _) when OU.is_push p -> Some DOUBLE_WRITE
    (* memory assignment operation *)
    | TripleInstr (op, e1, _, _, _, _) when OU.is_assign op && is_mem_exp e1 ->
        Some TRIPLE_WRITE
    | _ -> None

  let is_mem_read_instr instr =
    match instr with
    | SingleInstr (op, _, _, _) when is_ret instr ->
        (* ret *)
        Some SINGLE_READ
    | DoubleInstr (_, _, _, _, _) when is_arm_ret instr -> Some DOUBLE_READ
    | DoubleInstr (op, _, _, _, _) when is_stack_op op ->
        (* push & pop *)
        Some DOUBLE_READ
    | TripleInstr (op, _, e2, _, _, _) when is_mem_exp e2 ->
        begin match op with
        | Intel_OP io ->
            begin match io with
            | Intel_CommonOP (Intel_Assign io)
              when io = LEA || io = LEAL || io = LEAQ ->
                None
            | Intel_CommonOP (Intel_Assign _) -> Some TRIPLE_READ
            | _ -> None
            end
        | Arm_OP (ao, _, _) ->
            begin match ao with
            | Arm_CommonOP (Arm_Assign _) -> Some TRIPLE_READ
            | _ -> None
            end
        end
    | _ -> None

  let is_mem_instr instr =
    match is_mem_write_instr instr with
    | Some t -> Some (MEM_WRITE_TYPE t)
    | None ->
        let* t = is_mem_read_instr instr in
        Some (MEM_READ_TYPE t)

  let is_addr_or_label instr aols =
    let iloc = get_loc instr in
    let help aol =
      if String.get aol 0 = '0' && String.get aol 1 = 'x' then
        (* process address *)
        let addr = int_of_string aol in
        if iloc.loc_addr = addr then true else false
      else
        (* process label *)
        let contains s1 s2 =
          (* check if s2 is a substring of s1*)
          let re = Str.regexp_string s2 in
          try
            ignore (Str.search_forward re s1 0);
            true
          with Not_found -> false
        in
        (* for non-bb label (e.g., S_0x...), it follows the BB block *)
        (* therefore, has to check for substring *)
        if contains iloc.loc_label aol then true else false
    in
    List.exists help aols

  let is_jmp_instr i =
    let module OU = Opcode_utils in
    match i with
    | SingleInstr (p, _, _, _) when is_ret i -> Some RET_TYPE
    | DoubleInstr (_, _, _, _, _) when is_arm_ret i -> Some RET_TYPE
    | DoubleInstr (_, e, _, _, _) when is_indirect e -> Some INDIRECT
    | DoubleInstr (p, _, _, _, _) when OU.is_call p -> Some DIRECT_CALL
    | DoubleInstr (p, e, _, _, _) when is_jmp i && is_func e = true ->
        Some DIRECT_JMP_INTER
    | DoubleInstr (p, e, _, _, _) when is_jmp i && is_func e = false ->
        Some DIRECT_JMP_INTRA
    | DoubleInstr (p, e, _, _, _) when OU.is_cond_jmp p && is_func e = false ->
        Some COND_JMP_INTRA
    | DoubleInstr (p, e, _, _, _) when OU.is_cond_jmp p && is_func e = true ->
        Some COND_JMP_INTER
    | _ -> None
end

module Instrumentation_visitor = struct
  open Instr_utils

  let map_instr judge visitor il =
    let aux i = match judge i with Some t -> visitor i t | None -> i in
    il |> List.rev_map aux |> List.rev

  let map_instr' judge visitor il labels =
    let rec aux il visited acc =
      match il with
      | [] -> List.rev acc
      | h :: t -> (
          let h_loc = get_loc h in
          let h_addr = h_loc.loc_addr in
          if List.mem h_addr visited then aux t visited (h :: acc)
          else
            match judge h labels with
            | true ->
                let i' = visitor h in
                aux t (h_addr :: visited) (i' :: acc)
            | false -> aux t (h_addr :: visited) (h :: acc))
    in
    aux il [] []
end

module Instrumentation_template = struct
  let gen_logging_instrs i iloc =
    let open Type in
    let iloc' = { iloc with loc_label = "" } in
    let addr' = iloc.loc_addr in
    let ads = "sub_" ^ dec_hex addr' in
    let i1 =
      DoubleInstr
        ( Intel_OP (Intel_StackOP PUSH),
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          iloc,
          None,
          Hashtbl.create 0 )
    in
    let i2 =
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          Label "index",
          iloc',
          None,
          Hashtbl.create 0 )
    in
    let i3 =
      DoubleInstr
        ( Intel_OP (Intel_ControlOP (Intel_Loop LOOP)),
          Label ads,
          iloc',
          None,
          Hashtbl.create 0 )
    in
    (* let i4 = TripleInstr (CommonOP (Assign MOVL), Reg (CommonReg ECX),
                            Const (Normal 0x400000), iloc', None) in *)
    let i5 =
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
          Ptr (JmpTable_PLUS_S ("buf", Intel_Reg (Intel_CommonReg ECX), 4)),
          Const (Normal addr'),
          { iloc' with loc_label = ads ^ ":" },
          None,
          Hashtbl.create 0 )
    in
    let i6 =
      TripleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Assign MOVL)),
          Label "index",
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          iloc',
          None,
          Hashtbl.create 0 )
    in
    let i7 =
      DoubleInstr
        ( Intel_OP (Intel_StackOP POP),
          Reg (Intel_Reg (Intel_CommonReg ECX)),
          iloc',
          None,
          Hashtbl.create 0 )
    in
    let open Instrumentation_utils in
    set_update_fold i7 iloc [] |> set_update_fold i6 iloc
    |> set_update_fold i5 iloc
    (* |> set_update_fold i4 iloc *)
    |> set_update_fold i3 iloc
    |> set_update_fold i2 iloc |> set_update_fold i1 iloc |> List.flatten
end

module BB_utils = struct
  open Instr_utils
  (* OK, this is an optimization function.
       bb_instrs are frequently used in many functions,
       but this is too slow.
       Let's construct a map, with bb name as the key and
       its corresponding instruction list as the value.
     *)

  let bb_map bbl instrs =
    let m = Hashtbl.create 50 in
    let rec aux bl il acc =
      match (bl, il) with
      | bh :: bt, ih :: it ->
          let bloc = bh.bblock_begin_loc and eloc = bh.bblock_end_loc in
          let ba = bloc.loc_addr in
          let ea = eloc.loc_addr in
          let ia = get_addr ih in
          (* the end of a instr list *)
          if ia = ea then begin
            let n = bh.bblock_name in
            ih :: acc |> List.rev |> Hashtbl.replace m n;
            aux bt it []
          end
          else if ia >= ba && ia < ea then
            (* the start of a new list *)
            aux bl it (ih :: acc)
          else aux bl it []
          (* failwith "undefined behavior in bb map construction 1" *)
      | [], [] -> m
      | _, [] -> failwith "undefined behavior in bb map construction 2"
      | [], _ -> m
      (* failwith "undefined behavior in bb map construction 3" *)
    in
    aux bbl instrs []

  (*get all the instructions within one basic block as a list*)
  let bb_instrs b instrs =
    let bloc = b.bblock_begin_loc and eloc = b.bblock_end_loc in
    let rec help il acc =
      match il with
      | h :: t -> begin
          let loc = get_loc h in
          if loc.loc_addr >= bloc.loc_addr && loc.loc_addr <= eloc.loc_addr then
            help t (h :: acc)
          else help t acc
        end
      | [] -> List.rev acc
    in
    help instrs []

  (* sort basic block list from smallest to largest  *)
  let bbl_sort bbl =
    List.sort
      (fun b1 b2 -> b1.bblock_begin_loc.loc_addr - b2.bblock_end_loc.loc_addr)
      bbl
end

module Bblock_visitor = struct
  let map_bblock judge visitor bbl =
    let aux i = match judge i with Some t -> visitor i t | None -> i in
    bbl |> List.rev_map aux |> List.rev
end

module Function_visitor = struct
  let map_function judge visitor fl =
    let aux i = match judge i with Some t -> visitor i t | None -> i in
    fl |> List.rev_map aux |> List.rev
end

module Dataset_utils = struct
  let insert_data label value size first_time label_pos =
    let first_time = if first_time then "1" else "0" in
    Sys.command
      ("python3 data_instrumentation.py " ^ label ^ " " ^ value ^ " " ^ size
     ^ " " ^ first_time ^ " " ^ label_pos)
end

module Cfg_utils = struct
  open Instrumentation_utils
  open Instr_utils
  open Exp_utils

  (*
      Finding all mergeable basic block pairs.
      Basic blocks in mergeable pair should have one single
      predecesor and one single seccessor.
      This method is very useful when optimizing and diversifying.
     *)
  let mergeable_bb cfg =
    List.fold_left
      (fun acc1 (f, v) ->
        let ic =
          List.exists
            (fun t ->
              match t with
              | _, (_, None) -> false
              | _, (_, Some s) when s = "T" -> true
              | _, (_, Some s) when s = "INTER" -> true
              | _, (_, Some s) when s = "RET" -> false
              | s, (_, Some d) -> false)
            v
        in
        if ic = true then acc1
        else begin
          let t =
            List.fold_left
              (fun acc t ->
                match t with
                | _, (_, None) -> acc
                | _, (_, Some s) when s = "T" -> acc
                | _, (_, Some s) when s = "INTER" -> acc
                | _, (_, Some s) when s = "RET" -> acc
                | s, (_, Some d) -> (s, d) :: acc)
              [] v
          in
          let sl = List.map fst t in
          let dl = List.map snd t in
          let sl' = remove_over_once sl in
          let dl' = remove_over_once dl in
          List.fold_left
            (fun acc s ->
              match List.assoc s v with
              | _, Some d when List.mem d dl' -> begin
                  assert (contains ~str:s ~sub:"BB_");
                  assert (contains ~str:d ~sub:"BB_");
                  (f, s, d) :: acc
                end
              | _ -> acc)
            acc1 sl'
        end)
      [] cfg

  (* a visitor for control flow transfers; original jmp are rewritten into
    new format *)
  let map_jmp (visitor : instr -> jmp_type -> instr) instrs : instr list =
    let module OU = Opcode_utils in
    let aux i =
      match i with
      | SingleInstr (p, _, _, _) when is_ret i -> visitor i RET_TYPE
      | DoubleInstr (_, _, _, _, _) when is_arm_ret i -> visitor i RET_TYPE
      | DoubleInstr (_, e, _, _, _) when is_indirect e -> visitor i INDIRECT
      | DoubleInstr (p, _, _, _, _) when OU.is_call p -> visitor i DIRECT_CALL
      | DoubleInstr (p, e, _, _, _) when is_jmp i && is_func e = true ->
          visitor i DIRECT_JMP_INTER
      | DoubleInstr (p, e, _, _, _) when is_jmp i && is_func e = false ->
          visitor i DIRECT_JMP_INTRA
      | DoubleInstr (p, e, _, _, _) when OU.is_cond_jmp p && is_func e = false
        ->
          visitor i COND_JMP_INTRA
      | DoubleInstr (p, e, _, _, _) when OU.is_cond_jmp p && is_func e = true ->
          visitor i COND_JMP_INTER
      | _ -> i
    in
    instrs |> List.rev_map aux |> List.rev

  (* a visitor for memory rewrite operation *)
  let map_mem_write (visitor : instr -> mem_write_type -> instr) instrs :
      instr list =
    let module OU = Opcode_utils in
    let aux i =
      match i with
      | DoubleInstr (p, _, _, _, _) when OU.is_push p -> visitor i DOUBLE_WRITE
      (* memory assignment operation *)
      | TripleInstr (op, e1, _, _, _, _) when OU.is_assign op && is_mem_exp e1
        ->
          visitor i TRIPLE_WRITE
      | _ -> i
    in
    instrs |> List.rev_map aux |> List.rev
end

module Cg_utils = struct
  let show_call_graph cg =
    let help key value =
      print_endline @@ dec_hex key;
      List.iter (fun f -> print_endline ("    " ^ f.func_name)) value
    in
    Hashtbl.iter help cg

  (* this cfi_cg can support CFI transformation. *)
  let show_cfi_sepcified_graph cfi_cg =
    let help key value =
      print_endline key;
      List.iter (fun l -> print_endline ("    " ^ dec_hex l)) value
    in
    Hashtbl.iter help cfi_cg

  (* visitor function for CFI transformation *)
  let map_cfi_des cfi_cg fn visit = Hashtbl.find cfi_cg fn |> List.map visit

  (* visitor function for CFI transformation *)
  let map_cfi_des_cond cfi_cg fn visit judge =
    Hashtbl.find cfi_cg fn |> List.filter judge |> List.map visit
end
