open Hashtbl
open Batteries

open Type

module IntOrder : Set.OrderedType = struct
  type t = int
  let compare = Pervasives.compare
end

module StringOrder : Set.OrderedType = struct
  type t = string
  let compare = Pervasives.compare
end

module IntSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = int
  end )

module StringSet = Set.Make(String)

let unify_int_list (l: int list) : int list =
  let s =
    List.fold_left (fun s ele -> IntSet.add ele s) IntSet.empty l in
  (* elements should be returned in increasing order *)
  IntSet.elements s

let unify_str_list (l: string list) : string list =
  let s =
    List.fold_left (fun s ele -> StringSet.add ele s) StringSet.empty l in
  (* elements should be returned in increasing order *)
  StringSet.elements s


let unify_funclist_by_name (l : func list) : func list =
  let t = Hashtbl.create (List.length l * 2) in
  List.iter (fun f -> Hashtbl.replace t (f.func_name) f) l;
  Hashtbl.fold (fun k v s -> v::s) t []


(* this function must be called after the sort by addr*)
let unify_funclist_by_addr (l : func list) : func list =
  let rec help l acc =
    match l with
    | [] -> []
    | h::[] -> List.rev_append acc [h]
    | h1::h2::t ->
      begin
        if (h1.func_begin_addr) == (h2.func_begin_addr) then
          begin
            if String.exists (h1.func_name) "S_0x" then
              (
                (* print_string ("remove : "^(h1.func_name)^"\n"); *)
                help (h2::t) acc
              )
            else if String.exists (h2.func_name) "S_0x" then
              (
                (* print_string ("remove : "^(h2.func_name)^"\n"); *)
                help (h1::t) acc
              )
            else help (h2::t) (h1::acc)
          end
        else help (h2::t) (h1::acc)
      end
  in
  help l []


let unify_hash_list h =
  let l = Hashtbl.length h in
  let h' = Hashtbl.create (l + 1)
  and se = Hashtbl.fold (fun k v s -> StringSet.add k s) h StringSet.empty in
  let help n =
    let f = Hashtbl.find h n in
    Hashtbl.add h' n f in
  List.iter help (StringSet.elements se);
  h'

(* on 32bit system, OCaml's maxinum integer is 0x3fffffff *)
let string_to_int32 s =
  try
    Some (int_of_string s)
  with
  | _ -> None


let compare_loc l1 l2 =
  l1.loc_addr = l2.loc_addr && (l1.loc_label = l2.loc_label)

let get_loc i =
  match i with
  | SingleInstr (_, l, _) -> l
  | DoubleInstr (_, _, l, _) -> l
  | TripleInstr (_, _, _, l, _) -> l
  | FourInstr (_, _, _, _, l, _) -> l
  | FifInstr (_, _, _, _, _, l, _) -> l

let set_loc i l =
  match i with
  | SingleInstr (p, _, pre) -> SingleInstr (p, l, pre)
  | DoubleInstr (p, e, _, pre) -> DoubleInstr (p, e, l, pre)
  | TripleInstr (p, e1, e2, _, pre) -> TripleInstr (p, e1, e2, l, pre)
  | FourInstr (p, e1, e2, e3, _, pre) -> FourInstr (p, e1, e2, e3, l, pre)
  | FifInstr (p, e1, e2, e3, e4, _, pre) -> FifInstr (p, e1, e2, e3, e4, l, pre)

let get_addr i =
  let l = get_loc i in
  l.loc_addr

let get_label i =
  let l = get_loc i in
  l.loc_label

let update_label i l =
  let loc = get_loc i in
  set_loc i {loc with loc_label = l}

let get_op i =
  match i with
  | SingleInstr (p, _, _) -> p
  | DoubleInstr (p, _, _, _) -> p
  | TripleInstr (p, _, _, _, _) -> p
  | FourInstr (p, _, _, _, _, _) -> p
  | FifInstr (p, _, _, _, _, _, _) -> p

let get_cf_des i =
  match i with
  | DoubleInstr (_, e, _, _) -> Some e
  | _ -> None


let get_exp_1 i =
  match i with
  | SingleInstr (_, _, _) -> failwith "undefined expression"
  | DoubleInstr (_, e, _, _) -> e
  | TripleInstr (_, e, _, _, _) -> e
  | FourInstr (_, e, _, _, _, _) -> e
  | FifInstr (_, e, _, _, _, _, _) -> e


let read_file (filename : string) : string list =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines;;


let dec_hex (s:int) : string =
  (Printf.sprintf "0x%X" s)


let print_loclist (ll : loc list) : unit =
  List.iter (fun l ->
      print_string l.loc_label;
      print_string "\n";
      print_string ((dec_hex l.loc_addr)^"\n")
    ) ll

let print_addrlist (ll : int list) : unit =
  List.iter (fun l ->
      print_string ((dec_hex l)^"\n")
    ) ll

let zip lst1 lst2 =
  let rec aux lst1 lst2 acc =
    match lst1,lst2 with
    | [],_ -> List.rev acc
    | _, []-> List.rev acc
    | (x::xs),(y::ys) -> aux xs ys ((x,y)::acc)
  in
  aux lst1 lst2 []

let rec cat_from (l: string list) (f : int) (s: string) : string =
  (* For the reason of high efficiency, we don't write it tail-recursively*)
  match l with
  | h::t ->
    if f == 0 then
      h^s^(cat_from t 0 s)
    else
      cat_from t (f-1) s
  | [] -> ""

let split_by_list (s : string) (pl : int list) =
  let rec help s last_p pl' acc =
    match pl' with
    | hp::tp ->
      let s' = String.sub s (last_p+1) (hp-last_p-1) in
      help s hp tp (s'::acc)
    | [] ->
      let ep = String.length s - 1 in
      List.rev_append acc [String.sub s (last_p+1) (ep-last_p-1)]
  in
  help s (-1) pl []

let int_of_string_opt (s : string) : int option =
  try
    Some(int_of_string s)
  with
  | _ -> None

let print_exp_type (e : exp) : unit =
  match e with
  | Const _ -> print_string "const"
  | Symbol _ -> print_string "symbol"
  | Reg _ -> print_string "reg"
  | Assist _ -> print_string "assist"
  | Ptr _ -> print_string "ptr"
  | Label _ -> print_string "label"

let print_instr_type (i : instr) : unit =
  match i with
  | SingleInstr _ -> print_string "single instr"
  | DoubleInstr _ -> print_string "double instr"
  | TripleInstr _ -> print_string "triple instr"
  | FourInstr _ -> print_string "four instr"
  | FifInstr _ -> print_string "five instr"

(* sort loc lost in ascending order *)
let sort_loc (ll : loc list) : loc list =
  List.sort (fun l1 l2 -> l1.loc_addr - l2.loc_addr) ll

(* suppose locs are sorted in ascending odder, and no same element exists
 * we assume no instructions share same location!
*)
let get_instr_byloc (instrs : instr list) (locs : loc list) : instr list =
  let rec aux il ll acc =
    match (il, ll) with
    | (ih::it, lh::lt) ->
      begin
        let iloc = get_loc ih in
        if iloc.loc_addr < lh.loc_addr then
          aux it ll acc
        else if iloc.loc_addr = lh.loc_addr then
          aux it lt (ih::acc)
        else assert(false)
      end
    | (_, []) -> List.rev acc
    | _ -> assert(false) in
  aux instrs locs []

let recover_addr_from_label s =
  try
    assert(String.exists s "S_0x");
    (* S_0x80509E4 *)
    let s' = String.sub s 2 9 in
    int_of_string s'
  with _ -> -1


let get_next_bb sn =
  assert(String.exists sn "BB_");
  (* S_0x80509E4 *)
  (*
  let split = Str.split (Str.regexp_string "_") in
  let items = split sn in
  let index = List.nth items 1 in
   *)
  let index = String.sub sn 3 (String.length sn - 3) in
  let index' = (int_of_string index) + 1 in
  "BB_"^(string_of_int index')


(* we implement several functions to support section info querying *)
(* let's implement them leveraging memo function *)

let memo_rec f =
  let m = ref [] in
  let rec g x =
    try
      List.assoc x !m
    with
      Not_found ->
      let y = f g x in
      m := (x, y) :: !m ;
      y
  in
  g

let memo f =
  let m = ref [] in
  fun x ->
    try
      List.assoc x !m
    with
      Not_found ->
      let y = f x in
      m := (x, y) :: !m ;
      y

let get_end_addr_sec sec =
  Sys.command("rm sec_all.info");
  Sys.command("cat text_sec.info >> sec_all.info");
  Sys.command("cat sections.info >> sec_all.info");
  let secl = read_file "sec_all.info" in
  List.fold_left (
    fun acc l ->
      let split = Str.split (Str.regexp " +") in
      let items = split l in
      if List.nth items 0 = sec then
        let baddrs = List.nth items 1
        and sizes = List.nth items 3 in
        let baddr = int_of_string ("0x"^baddrs) in
        let size = int_of_string ("0x"^sizes) in
        baddr+size
      else
        acc
  ) 0 secl

let sec_end_memo = memo get_end_addr_sec



(* some functions manipulating basic blocks *)
let get_bbl cfg =
  let bbl = List.map (fun (b, _) -> b) cfg in
  unify_str_list bbl


(*get all the instructions within one function as a list*)
let f_instrs f instrs =
  let baddr = f.func_begin_addr
  and eaddr = f.func_end_addr in
  let rec help il acc =
    match il with
    | h::t ->
      begin
        let loc = get_loc h in
        if loc.loc_addr >= baddr &&
           loc.loc_addr < eaddr then
          help t (h::acc)
        else help t acc
      end
    | [] -> List.rev acc in
  help instrs []

let bb_successors cfg b =
  let aux acc edge =
    match edge with
    | (_, (_, None)) -> acc
    | (_, (_, Some(s))) when s = "T" -> acc
    | (_, (_, Some(s))) when s = "INTER" -> acc
    | (_, (_, Some(s))) when s = "RET" -> acc
    | (s, (_, Some(d))) when s = b -> d::acc
    | _ -> acc in
  let dl = List.fold_left aux [] cfg in
  unify_str_list dl

let bb_predecessors cfg b =
  let aux acc edge =
    match edge with
    | (_, (_, None)) -> acc
    | (_, (_, Some(s))) when s = "T" -> acc
    | (_, (_, Some(s))) when s = "INTER" -> acc
    | (_, (_, Some(s))) when s = "RET" -> acc
    | (s, (_, Some(d))) when d = b -> s::acc
    | _ -> acc in
  let sl = List.fold_left aux [] cfg in
  unify_str_list sl


(*get all the instructions within one basic block as a list*)
let bb_instrs b instrs =
  let bloc = b.bblock_begin_loc
  and eloc = b.bblock_end_loc in
  let rec help il acc =
    match il with
    | h::t ->
      begin
        let loc = get_loc h in
        if loc.loc_addr >= bloc.loc_addr &&
           loc.loc_addr <= eloc.loc_addr then
          help t (h::acc)
        else help t acc
      end
    | [] -> List.rev acc in
  help instrs []



(* this function remove element from list if this element appears more than once*)
(*d = [x for x in d if d.count(x) == 1]*)
let remove_over_once l =
  let len = List.length l in
  let m = Hashtbl.create (len+1) in
  List.iter (
    fun ele ->
      if Hashtbl.mem m ele then
        let c = Hashtbl.find m ele in
        Hashtbl.replace m ele (c+1)
      else Hashtbl.add m ele 1
  ) l;
  List.filter (
    fun ele ->
      if (Hashtbl.find m ele) > 1 then
        false
      else true
  ) l

let bbn_byloc e ls =
  (* implement a binary search, the original way seems too slow *)
  let rec bs min max =
    if min > max then false
    else
      let mid = (min + max) / 2 in
      let mid_num = Array.get ls mid in
      if e = mid_num then
  	    true
      else
      if e < mid_num
      then bs min (mid-1)
      else bs (mid + 1) max
  in
  bs 0 (Array.length ls - 1)



let (@@) f x = f x


module Algo = struct

    let sort_uniq cmp l =
      let rec aux acc l =
        match l with
        | (h1::h2::t) ->
           if cmp h1 h2 = 0 then
             aux acc (h2::t)
           else
             aux (h1::acc) (h2::t)
        | (h::[]) -> h::acc
        | [] -> acc
      in
      l |> List.sort cmp |> aux [] |> List.rev

    let b_search e ls cmp =
      (* a binary search with configurable cmp *)
      let rec bs min max =
        if min > max then false
        else
          let mid = (min + max) / 2 in
          let mid_num = Array.get ls mid in
          let r = cmp e mid_num in
          if r = 0 then
      	    true
          else
          if r < 0
          then bs min (mid-1)
          else bs (mid + 1) max
      in
      bs 0 (Array.length ls - 1)


end


module ELF_utils = struct


    let elf_32 () =
      read_file "elf.info"
      |> (fun l -> List.nth l 0)
      |> (fun l -> String.exists l "ELF 32-bit" )


    let elf_64 () =
      read_file "elf.info"
      |> (fun l -> List.nth l 0)
      |> (fun l -> String.exists l "ELF 64-bit" )


    let elf_dynamic () =
      read_file "elf.info"
      |> (fun l -> List.nth l 0)
      |> (fun l -> String.exists l "dynamically linked")


    let elf_static () =
      read_file "elf.info"
      |> (fun l -> List.nth l 0)
      |> (fun l -> String.exists l "static linked")


    let elf_unstrip () =
      read_file "elf.info"
      |> (fun l -> List.nth l 0)
      |> (fun l -> String.exists l "not stripped")


    let elf_strip () =
      read_file "elf.info"
      |> (fun l -> List.nth l 0)
      |> (fun l -> String.exists l ", stripped")


    let elf_exe () =
      read_file "elf.info"
      |> (fun l -> List.nth l 0)
      |> (fun l -> String.exists l "LSB executable")


    let elf_lib () =
      read_file "elf.info"
      |> (fun l -> List.nth l 0)
      |> (fun l -> String.exists l "LSB shared object")

end



module Addr_utils = struct


    let compare_loc l1 l2 =
      l1.loc_addr = l2.loc_addr && (l1.loc_label = l2.loc_label)

    let get_loc i =
      match i with
      | SingleInstr (_, l, _) -> l
      | DoubleInstr (_, _, l, _) -> l
      | TripleInstr (_, _, _, l, _) -> l
      | FourInstr (_, _, _, _, l, _) -> l
      | FifInstr (_, _, _, _, _, l, _) -> l

    let set_loc i l =
      match i with
      | SingleInstr (p, _, pre) -> SingleInstr (p, l, pre)
      | DoubleInstr (p, e, _, pre) -> DoubleInstr (p, e, l, pre)
      | TripleInstr (p, e1, e2, _, pre) -> TripleInstr (p, e1, e2, l, pre)
      | FourInstr (p, e1, e2, e3, _, pre) -> FourInstr (p, e1, e2, e3, l, pre)
      | FifInstr (p, e1, e2, e3, e4, _, pre) -> FifInstr(p, e1, e2, e3, e4, l, pre)

    let get_addr i =
      let l = get_loc i in
      l.loc_addr

    let get_label i =
      let l = get_loc i in
      l.loc_label

    let update_label i l =
      let loc = get_loc i in
      set_loc i {loc with loc_label = l}

end


module Opcode_utils = struct

    let is_control_des i =
      let aux s =
        String.exists s ":"
      in
      get_label i |> aux


    let is_func e =
        match e with
          | Symbol (CallDes _) -> true
          | _ -> false

      let is_jmp = function
        | ControlOP op ->
           begin
             match op with
             | Jump JMP -> true
             | _ -> false
           end
        | _ -> false


      let is_cond_jmp = function
        | ControlOP op ->
           begin
             match op with
             | Jump JMP -> false
             | Jump _ -> true
             | _ -> false
           end
        | _ -> false

      (* this is a conservative judge *)
      let is_assign = function
        | CommonOP (Assign _) -> true
        | _ -> false


      let rec is_mem_exp = function
          | Ptr _ -> true
          | Symbol (StarDes e) -> is_mem_exp e
          | _ -> false


      let is_push = function
          | StackOP PUSH -> true
          | _ -> false


      let is_stack_op = function
          | StackOP PUSH
          | StackOP PUSHL
          | StackOP PUSHF
          | StackOP POP
          | StackOP POPL
          | StackOP POPF
          | ControlOP LEAVE -> true
          | _ -> false


      let is_mov = function
          | CommonOP (Assign MOV)
          | CommonOP (Assign MOVL) -> true
          | _ -> false


      let is_call op =
        match op with
        | ControlOP c ->
           begin
             match c with
             | CALL -> true
             | CALLQ -> true
             | _ -> false
           end
        | _ -> false

      let is_ret op =
        match op with
        | ControlOP c ->
           begin
             match c with
             | RET | RETN -> true
             | _ -> false
           end
        | _ -> false

      let is_indirect = function
        | Symbol s ->
           begin
             match s with
             |StarDes _ -> true
             | _ -> false
           end
        (* this is triky:  repz ret could be parsed into
         *        ((op repz) (exp ret))
         *  and the post_process.py script will translate into
         *      label :  repz
         *                ret
         *  which is a typical indirect control flow transfer
         *  Let's ad-hoc identify it in this way
         *)
        (* | Label s -> s = "ret" || s = "retq" *)
        | _ -> false


      let is_control_transfer_op op =
        is_call op || is_jmp op || is_cond_jmp op || is_ret op


      let is_cmp_op = function
          | CommonOP (Compare _) -> true
          | _         -> false



end



module Exp_utils = struct

    let is_reg e =
      match e with
        | Reg _ -> true
        | _ -> false


    let is_const e =
      match e with
        | Const _ -> true
        | _ -> false


    let is_mem e =
      match e with
        | Ptr _ -> true
        | _ -> false


end

module Instr_utils = struct

    include Opcode_utils
    include Addr_utils

    let sort_il_update instrs_update =
        List.sort (
            fun (_,l1,_,_) (_,l2,_,_) ->
            l1.loc_addr - l2.loc_addr
          ) instrs_update


   type instr_update = SUB | INSERT


   let set_update_fold i l acc =
     [(i, l, INSERT, "")] :: acc

   let sub_update_fold i l i' acc =
      let open Pp_print in
      let i_s = pp_print_instr i' in
      [(i, l, SUB, i_s)] :: acc


   let eliminate_label i =
     update_label i ""

   let tem i =
     let (_, _ , _, s) = i in
     s
    (* update inserted instruction at the front of target instruction *)
    let update_instrs_infront instrs instrs_update =
      let same loc1 loc2 =
        loc1.loc_addr = loc2.loc_addr in
      let rec help l_u l acc =
        match (l_u, l) with
        | (h_u::t_u, h::t) ->
          begin
            let (i, loc1, ty, i_s) = h_u
            and loc2 = get_loc h in
              if same loc1 loc2 then
                begin
                  match ty with
                  | SUB ->
                     begin
                       let open Pp_print in
                       let h_s = pp_print_instr h in
                       if i_s = h_s then
                         (help t_u (i::t) acc)
                       else
                         help l_u t (h::acc)
                     end
                  | INSERT ->
                      help t_u (h::t) (i::acc)
                end
              else
                help l_u t (h::acc)
          end
        | ([], l') -> List.rev_append acc l'
        | (h::t, []) ->
          begin
            failwith "error in update_instrs"
          end
      in
      help instrs_update instrs []




   let insert_instrument_instrs il il_update =
      il_update
      |> sort_il_update
      |> update_instrs_infront il


   let sub_single_instr i' il_update il =
     let loc = get_loc i' in
     let acc = List.fold_left
                 (fun acc i ->
                 sub_update_fold i loc i' acc)
                                 il_update [] in
     let acc' = List.flatten acc in
     insert_instrument_instrs il acc'


   let instrument_update il_update il =
     insert_instrument_instrs il il_update


   let instrument_instrs_func il il_update update_func =
      il_update
      |> sort_il_update
      |> update_func il


   let insert_instr_list d il_update il =
     match d with
       | BEFORE ->
          instrument_instrs_func il il_update update_instrs_infront
       | AFTER ->
          instrument_instrs_func il il_update update_instrs_infront


   let insert_single_instr d loc il_update il =
     let acc = List.fold_left (
                   fun acc i ->
                   set_update_fold i loc acc)
                              [] il_update
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
      List.find (fun b ->
                 b.bblock_end_loc.loc_addr = addr
                )
                bbl


    (* generate one byte no-op padding instruction *)
    let gen_nop loc =
      SingleInstr (CommonOP (Other NOP), loc, None)

    (* generate four bytes no-op padding instruction *)
    let gen_4_lea loc =
      TripleInstr (CommonOP (Assign LEA), Reg (CommonReg ESI),
                   Ptr (BinOP_PLUS (CommonReg ESI, 0)), loc, None)


    (* generate six bytes no-op padding instruction *)
    let gen_6_lea loc =
      TripleInstr (CommonOP (Assign LEA), Reg (CommonReg EDI),
                   Ptr (BinOP_PLUS (CommonReg EDI, 0)), loc, None)



    let is_mem_write_instr i =
        match i with
        | DoubleInstr (p, _, _, _) when (is_push p) ->
           Some DOUBLE_WRITE
        (* memory assignment operation *)
        | TripleInstr (op, e1, _, _, _) when (is_assign op) && (is_mem_exp e1)->
           Some TRIPLE_WRITE
        | _ -> None


    let is_jmp_instr i =
        match i with
        | SingleInstr (p, _, _) when (is_ret p) ->
           Some RET_TYPE
        | DoubleInstr (_, e, _, _) when (is_indirect e) ->
           Some INDIRECT
        | DoubleInstr (p, _, _, _) when (is_call p) ->
           Some DIRECT_CALL
        | DoubleInstr (p, e, _, _) when (is_jmp p) && (is_func e = true) ->
           Some DIRECT_JMP_INTER
        | DoubleInstr (p, e, _, _) when (is_jmp p) && (is_func e = false) ->
           Some DIRECT_JMP_INTRA
        | DoubleInstr (p, e, _, _) when (is_cond_jmp p) && (is_func e = false)->
           Some COND_JMP_INTRA
        | DoubleInstr (p, e, _, _) when (is_cond_jmp p) && (is_func e = true)->
           Some COND_JMP_INTER
        | _ -> None


end



module Instr_visitor = struct

     let map_instr judge visitor il =
       let aux i =
         match judge i with
         | Some t -> visitor i t
         | None -> i
       in
       il |> List.rev_map aux |> List.rev


end

module Instr_template = struct


    let gen_logging_instrs i iloc =
      let open Type in
      let iloc' = {iloc with loc_label = ""} in
      let addr' = iloc.loc_addr in
      let ads = "sub_" ^ (dec_hex addr')  in
      let i1 = DoubleInstr (StackOP PUSH, Reg (CommonReg ECX), iloc, None) in
      let i2 = TripleInstr (CommonOP (Assign MOVL), Reg (CommonReg ECX), Label "index", iloc', None) in
      let i3 = DoubleInstr (ControlOP (Loop LOOP), Label ads, iloc', None) in
      (* let i4 = TripleInstr (CommonOP (Assign MOVL), Reg (CommonReg ECX),
                            Const (Normal 0x400000), iloc', None) in *)
      let i5 = TripleInstr (CommonOP (Assign MOVL), Ptr (JmpTable_PLUS_S ("buf", CommonReg ECX, 4)),
                            Const (Normal addr'), {iloc' with loc_label = ads ^ ":"}, None) in
      let i6 = TripleInstr (CommonOP (Assign MOVL), Label "index", Reg (CommonReg ECX), iloc', None) in
      let i7 = DoubleInstr (StackOP POP, Reg (CommonReg ECX), iloc', None) in
      let i' = set_loc i iloc' in
      let open Instr_utils in
      set_update_fold i7 iloc []
      |> set_update_fold i6 iloc
      |> set_update_fold i5 iloc
      (* |> set_update_fold i4 iloc *)
      |> set_update_fold i3 iloc
      |> set_update_fold i2 iloc
      |> set_update_fold i1 iloc
      |> List.flatten



end


module BB_utils = struct


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
        | (bh::bt, ih::it) ->
           let bloc = bh.bblock_begin_loc
           and eloc = bh.bblock_end_loc in
           let ba = bloc.loc_addr in
           let ea = eloc.loc_addr in
           let ia = get_addr ih in
           (* the end of a instr list *)
           if ia = ea then
             begin
               let n = bh.bblock_name in
               (ih::acc) |> List.rev |>
               Hashtbl.replace m n;
               aux bt it []
             end
           (* the start of a new list *)
           else if ia >= ba && ia < ea then
             aux bl it (ih::acc)
           else
             aux bl it []
             (* failwith "undefined behavior in bb map construction 1" *)
        | ([], []) -> m
        | (_, []) -> failwith "undefined behavior in bb map construction 2"
        | ([], _) ->
           m
           (* failwith "undefined behavior in bb map construction 3" *)
      in
      aux bbl instrs []



    (*get all the instructions within one basic block as a list*)
    let bb_instrs b instrs =
      let bloc = b.bblock_begin_loc
      and eloc = b.bblock_end_loc in
      let rec help il acc =
        match il with
        | h::t ->
           begin
             let loc = get_loc h in
             if loc.loc_addr >= bloc.loc_addr &&
                  loc.loc_addr <= eloc.loc_addr then
               help t (h::acc)
             else help t acc
           end
        | [] -> List.rev acc in
      help instrs []


    (* sort basic block list from smallest to largest  *)
    let bbl_sort bbl =
      List.sort (fun b1 b2 ->
                 b1.bblock_begin_loc.loc_addr - b2.bblock_end_loc.loc_addr
                ) bbl


    let get_range b =
     let a1 = b.bblock_begin_loc.loc_addr in
     let a2 = b.bblock_end_loc.loc_addr in
     a1 - a2


    (* the first instruction is a target of fall-through transfer  *)
    let start_with_fallthrough b bmap =
      let il = Hashtbl.find bmap b.bblock_name in
      let ifirst = List.nth il 0 in
      (get_label ifirst) = ""


    (* the first instruction is a target of control
    flow transfer  *)
    let start_with_fallthrough b bmap =
      let il = Hashtbl.find bmap b.bblock_name in
      let ifirst = List.nth il 0 in
      (get_label ifirst) <> ""

    (* the last instruction is a target of fall-through transfer  *)
    let end_with_fallthrough b bmap =
      let il = Hashtbl.find bmap b.bblock_name in
      let ilast = List.nth il (List.length il - 1) in
      (get_label ilast) = ""


    (* the last instruction is a target of control
    flow transfer  *)
    let end_with_fallthrough b bmap =
      let il = Hashtbl.find bmap b.bblock_name in
      let ilast = List.nth il (List.length il - 1) in
      (get_label ilast) <> ""


    let start_with_jmp b bmap =
      let module IU = Instr_utils in
      let il = Hashtbl.find bmap b.bblock_name in
      let ifirst = List.nth il 0 in
      IU.is_jmp_instr ifirst


    let start_with_mem_write b bmap =
      let module IU = Instr_utils in
      let il = Hashtbl.find bmap b.bblock_name in
      let ifirst = List.nth il 0 in
      IU.is_mem_write_instr ifirst


    let end_with_jmp b bmap =
      let module IU = Instr_utils in
      let il = Hashtbl.find bmap b.bblock_name in
      let ilast = List.nth il (List.length il - 1) in
      IU.is_jmp_instr ilast


    let end_with_mem_write b bmap =
      let module IU = Instr_utils in
      let il = Hashtbl.find bmap b.bblock_name in
      let ilast = List.nth il (List.length il - 1) in
      IU.is_mem_write_instr ilast


end


module Bblock_visitor = struct

     let map_bblock judge visitor bbl =
       let aux i =
         match judge i with
         | Some t -> visitor i t
         | None -> i
       in
       bbl |> List.rev_map aux |> List.rev


end



module Func_utils = struct


    let func_map fl instrs =
      let m = Hashtbl.create 50 in
      let rec aux bl il acc =
      match (bl, il) with
        | (bh::bt, ih::it) ->
           let ba = bh.func_begin_addr in
           let ea = bh.func_end_addr in
           let ia = get_addr ih in
           (* the end of a instr list *)
           if ia = ea then
             begin
               let n = bh.func_name in
               Hashtbl.replace m n (ih::acc);
               aux bt it []
             end
           (* the start of a new list *)
           else if ia >= ba && ia < ea then
             aux bl it (ih::acc)
           else
             aux bl it []
             (* failwith "undefined behavior in func map construction 1" *)
        | ([], []) -> m
        | (_, []) -> failwith "undefined behavior in bb map construction 2"
        | ([], _) ->
           m
           (* failwith "undefined behavior in func map construction 3" *)
      in
      aux fl instrs []



    (*
       sort func list from smallest to largest;
       Note that we need to first filter out lib function
     *)
    let func_sort fl =
      fl |> List.filter (fun f -> f.is_lib = false) |>
      List.sort (fun b1 b2 ->
                 b1.func_begin_addr - b2.func_begin_addr
                )


    let get_range f =
     let a1 = f.func_begin_addr in
     let a2 = f.func_end_addr in
     a1 - a2


    (* the first instruction is a target of fall-through transfer  *)
    let start_with_fallthrough f fmap =
      let il = Hashtbl.find fmap f.func_name in
      let ifirst = List.nth il 0 in
      (get_label ifirst) = ""


    (* the first instruction is a target of control
    flow transfer  *)
    let start_with_fallthrough f fmap =
      let il = Hashtbl.find fmap f.func_name in
      let ifirst = List.nth il 0 in
      (get_label ifirst) <> ""

    (* the last instruction is a target of fall-through transfer  *)
    let end_with_fallthrough f fmap =
      let il = Hashtbl.find fmap f.func_name in
      let ilast = List.nth il (List.length il - 1) in
      (get_label ilast) = ""


    (* the last instruction is a target of control
    flow transfer  *)
    let end_with_fallthrough f fmap =
      let il = Hashtbl.find fmap f.func_name in
      let ilast = List.nth il (List.length il - 1) in
      (get_label ilast) <> ""


    let start_with_jmp f fmap =
      let module IU = Instr_utils in
      let il = Hashtbl.find fmap f.name in
      let ifirst = List.nth il 0 in
      IU.is_jmp_instr ifirst


    let start_with_mem_write f fmap =
      let module IU = Instr_utils in
      let il = Hashtbl.find fmap f.func_name in
      let ifirst = List.nth il 0 in
      IU.is_mem_write_instr ifirst


    let end_with_jmp f fmap =
      let module IU = Instr_utils in
      let il = Hashtbl.find fmap f.func_name in
      let ilast = List.nth il (List.length il - 1) in
      IU.is_jmp_instr ilast


    let end_with_mem_write f fmap =
      let module IU = Instr_utils in
      let il = Hashtbl.find fmap f.func_name in
      let ilast = List.nth il (List.length il - 1) in
      IU.is_mem_write_instr ilast

    (* collect all the callers *)
    let caller_collect fn instrs =
      let is_call op =
        match op with
        | ControlOP c ->
           begin
             match c with
             | CALL -> true
             | _ -> false
           end
        | _ -> false in
      let help acc i =
        match i with
        | DoubleInstr (p, e, _, _) when (is_call p) ->
           let open Pp_print in
           let es = p_exp e in
           if String.exists es fn then
             i::acc
           else acc
        | _ -> acc in
      List.fold_left help [] instrs


end


module Function_visitor = struct

     let map_function judge visitor fl =
       let aux i =
         match judge i with
         | Some t -> visitor i t
         | None -> i
       in
       fl |> List.rev_map aux |> List.rev


end


module Dataset_utils = struct

    let insert_data label value size first_time label_pos =
      let first_time = if first_time then "1" else "0" in
      Sys.command("python data_instrumentation.py " ^ label ^ " " ^ value ^ " " ^ size ^ " " ^ first_time ^ " " ^ label_pos)

end


module Cfg_utils = struct

    open Instr_utils

    (*
      Finding all mergeable basic block pairs.
      Basic blocks in mergeable pair should have one single
      predecesor and one single seccessor.
      This method is very useful when optimizing and diversifying.
     *)
    let mergeable_bb cfg =
      List.fold_left (
          fun acc1 (f, v) ->
          let ic = List.exists (fun t ->
                               match t with
                               | (_, (_, None)) -> false
                               | (_, (_, Some(s))) when s = "T" -> true
                               | (_, (_, Some(s))) when s = "INTER" -> true
                               | (_, (_, Some(s))) when s = "RET" -> false
                               | (s, (_, Some(d))) -> false
                    ) v in
          if ic = true then acc1
          else
          begin
          let t = List.fold_left (
                      fun acc t ->
                      match t with
                      | (_, (_, None)) -> acc
                      | (_, (_, Some(s))) when s = "T" -> acc
                      | (_, (_, Some(s))) when s = "INTER" -> acc
                      | (_, (_, Some(s))) when s = "RET" -> acc
                      | (s, (_, Some(d))) -> (s,d)::acc
                    ) [] v in
          let sl = List.map (fun (s,_) -> s) t in
          let dl = List.map (fun (_,d) -> d) t in
          let sl' = remove_over_once sl in
          let dl' = remove_over_once dl in
          List.fold_left (
              fun acc s ->
              match List.assoc s v with
              |  (_, Some(d)) when List.mem d dl' ->
                 begin
                   assert(String.exists s "BB_");
                   assert(String.exists d "BB_");
                   (f,s,d)::acc
                 end
              | _ -> acc
            ) acc1 sl';
        end
        ) [] cfg



    (* a visitor for control flow transfers; original jmp are rewritten into
    new format *)
    let map_jmp (visitor : (instr -> jmp_type -> instr)) instrs : instr list =
      let aux i =
        match i with
        | SingleInstr (p, _, _) when (is_ret p) ->
           visitor i RET_TYPE
        | DoubleInstr (_, e, _, _) when (is_indirect e) ->
           visitor i INDIRECT
        | DoubleInstr (p, _, _, _) when (is_call p) ->
           visitor i DIRECT_CALL
        | DoubleInstr (p, e, _, _) when (is_jmp p) && (is_func e = true) ->
           visitor i DIRECT_JMP_INTER
        | DoubleInstr (p, e, _, _) when (is_jmp p) && (is_func e = false) ->
           visitor i DIRECT_JMP_INTRA
        | DoubleInstr (p, e, _, _) when (is_cond_jmp p) && (is_func e = false)->
           visitor i COND_JMP_INTRA
        | DoubleInstr (p, e, _, _) when (is_cond_jmp p) && (is_func e = true)->
           visitor i COND_JMP_INTER
        | _ -> i in
      instrs |> List.rev_map aux |> List.rev


    (* a visitor for memory rewrite operation *)
    let map_mem_write (visitor : (instr -> mem_write_type -> instr)) instrs : instr list =
      let aux i =
        match i with
        | DoubleInstr (p, _, _, _) when (is_push p) ->
           visitor i DOUBLE_WRITE
        (* memory assignment operation *)
        | TripleInstr (op, e1, _, _, _) when (is_assign op) && (is_mem_exp e1)->
           visitor i TRIPLE_WRITE
        | _ -> i in
      instrs |> List.rev_map aux |> List.rev

end


module Cg_utils = struct


    let show_call_graph cg =
      let rec help key value =
        print_endline @@ dec_hex key;
        List.iter (fun f ->
                   print_endline ("    "^f.func_name)
                  ) value
      in
      Hashtbl.iter help cg


    (* this cfi_cg can support CFI transformation. *)
    let show_cfi_sepcified_graph cfi_cg =
      let rec help key value =
        print_endline key;
        List.iter (fun l ->
                   print_endline ("    "^dec_hex(l))
                  ) value
      in
      Hashtbl.iter help cfi_cg


    (* visitor function for CFI transformation *)
    let map_cfi_des cfi_cg fn visit =
      Hashtbl.find cfi_cg fn
      |> List.map visit


    (* visitor function for CFI transformation *)
    let map_cfi_des_cond cfi_cg fn visit judge =
      Hashtbl.find cfi_cg fn
      |> List.filter judge
      |> List.map visit



end


module Parallel = struct
    (* this module provides multi-core support for OCaml code. *)
    (* In particular, many OCaml operations, say, map, fold, mapfold, iter
     can be boosted by multi-thread, as operations would not affect each other
     *)
    (*
    However, no official module are provided by OCaml.
    So in this module, we provide a simple interface to wrap OCaml's
    multiple-core library.
    *)

    (* Right now, we leverage Parmap
       https://github.com/rdicosmo/parmap
https://github.com/rdicosmo/parmap/blob/642dc2bdd081cfdd3c3687c08851f3eafa09725c/parmap.mli
     *)

    open Parmap

    (* The parallel functions for lists *)
    let pmap ?ncores f il =
      let nc = match ncores with
          (* well, I set the default value to 1 *)
          | None -> 1
          | Some x -> x
      in
      Parmap.parmap ~ncores: nc f (Parmap.L il)


    let piter ?ncores f il =
      let nc = match ncores with
          (* well, I set the default value to 1 *)
          | None -> 1
          | Some x -> x
      in
      Parmap.pariter ~ncores: nc f (Parmap.L il)


    let pfold ?ncores ?concat f il acc =
      let nc = match ncores with
          (* I set the default value to 1 *)
          | None -> 1
          | Some x -> x
      in
      let cc = match concat with
          (* I set the default value to [] *)
          | None -> (@)
          | Some x -> x
      in
      Parmap.parfold ~ncores: nc f (Parmap.L il) acc cc


    (* The parallel functions for array *)
    let pmap_arr ?ncores f il =
      let nc = match ncores with
          (* well, I set the default value to 1 *)
          | None -> 1
          | Some x -> x
      in
      Parmap.parmap ~ncores: nc f (Parmap.A il)


    let piter_arr ?ncores f il =
      let nc = match ncores with
          (* well, I set the default value to 1 *)
          | None -> 1
          | Some x -> x
      in
      Parmap.pariter ~ncores: nc f (Parmap.A il)


    let pfold_arr ?ncores ?concat f il acc =
      let nc = match ncores with
          (* I set the default value to 1 *)
          | None -> 1
          | Some x -> x
      in
      let cc = match concat with
          (* I set the default value to [] *)
          | None -> (@)
          | Some x -> x
      in
      Parmap.parfold ~ncores: nc f (Parmap.A il) acc cc


end


module Time_Record = struct
    (* this module wrapper the UNIX time expansion facilities of  OCaml
     *)

    let stamp () =
      Printf.printf "stamp : %f sec\n" (Unix.gettimeofday ());
      ()

    let get_utime () =
      Unix.gettimeofday ()

    let elapsed t =
      let t1 = Unix.gettimeofday () in
      Printf.printf "execution elapsed time: %f sec\n" (t1 -. t);
      t1


end


module UIO = struct

    let lines_from_file (filename : string) : string list =
      (* this method is slow.. *)
      let lines = ref [] in
      let chan = open_in filename in
      try
        while true; do
          lines := input_line chan :: !lines
        done; []
      with End_of_file ->
        close_in chan;
        List.rev !lines;;


    let write_file ll fn =
      let oc = open_out_gen [Open_append; Open_creat] 0o666 fn in
      List.iter (fun l -> output_string oc l; output_char oc '\n') ll;
      close_out oc

end
