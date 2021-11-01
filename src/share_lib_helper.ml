open Batteries

open Type
open Pp_print

open Ail_utils

(* 
 * this code is developed for handling 32-bit shared library code. One
       distinguish feature comparing 32-bit shared lib with 64-bit is that
       32-bit lib relies on this pattern to calculate register value and do
       position independent location.
       *
71 000004ac <mean>:

 ........
 ........
 76  4b3:   e8 ef ff ff ff          call   4a7 <__i686.get_pc_thunk.bx>
 77  4b8:   81 c3 3c 1b 00 00       add    $0x1b3c,%ebx
 ........
 ........
 86  4d6:   8d 83 84 e5 ff ff       lea    -0x1a7c(%ebx),%eax
 ........
 ........
 93  4f8:   8d 83 8a e5 ff ff       lea    -0x1a76(%ebx),%eax
 94  4fe:   89 04 24                mov    %eax,(%esp)
 95  501:   e8 aa fe ff ff          call   3b0 <printf@plt>
 96  506:   dd 45 f0                fldl   -0x10(%ebp)
 97  509:   dc 45 e8                faddl  -0x18(%ebp)
 98  50c:   dd 83 94 e5 ff ff       fldl   -0x1a6c(%ebx)
100  514:   83 c4 24                add    $0x24,%esp
 ........
 ........

Let's translate code above into this:

 30         call    __i686.get_pc_thunk.bx
 31         Addl    $_GLOBAL_OFFSET_TABLE_, %ebx
 32         movl    8(%ebp), %eax
 33         movl    %eax, -16(%ebp)
 34         movl    12(%ebp), %eax
 35         movl    %eax, -12(%ebp)
 36         movl    16(%ebp), %eax
 37         movl    %eax, -24(%ebp)
 38         movl    20(%ebp), %eax
 39         movl    %eax, -20(%ebp)
 40         leal    S_0x578@GOTOFF(%ebx), %eax
 .......
 .......
 *)

(*
       we undertake a simple dataflow analysis, tracing the usage of
       identified register.

       No register propagation, intra-procedure or even inter-procedure analysis are considered.
 *)

class lib32_helper (instr_list : instr list) =
  (* list of registers and its corresponding index
   *  the extensible version of registers should be used in PIC location
   *
   *  eax -> 0
   *  ebx -> 1
   *  ecx -> 2
   *  edx -> 3
   *  edi -> 4
   *  esi -> 5
   *)

  let r = ref 6 in
  let rv = ref 0 in

  let pic_addr = ref "" in

  let reg_to_index = function
    | CommonReg r' ->
       begin
         match r' with
         | EAX -> 0
         | EBX -> 1
         | ECX -> 2
         | EDX -> 3
         | EDI -> 4
         | ESI -> 5
         | _   -> -1
       end
    | _   -> -1 in

  let index_of_reg = function
    | Reg r ->
       begin
         match r with
         | CommonReg r' ->
            begin
              match r' with
              | EAX -> 0
              | EBX -> 1
              | ECX -> 2
              | EDX -> 3
              | EDI -> 4
              | ESI -> 5
              | _   -> failwith "unsupport register in index_of_reg"
            end
         | _   -> failwith "unsupport register in index_of_reg"
       end
    | _ -> failwith "unsupport register in index_of_reg" in
  let value_of_reg e l =
    let v1 = match e with
      | Const c ->
         (
           match c with
           | Normal n -> n
           | _ -> failwith "failed in value of reg"
         )
      | _ -> failwith "failed in value of reg" in
    let v2 = l.loc_addr in
    v1 + v2 in
  let is_call_op = function
    | ControlOP CALL -> true
    | _ -> false in
  let is_get_pc_thunk  = function
    | Symbol e' ->
       begin
         match e' with
         | CallDes func' -> String.exists (func'.func_name) !pic_addr
         | _ -> false
       end
    | Label s -> String.exists s !pic_addr
    | _ -> false in
  let match_add = function
    | CommonOP op ->
       begin
         match op with
         | Arithm ADD -> true
         | _ -> false
       end
    | _ -> false in
  let is_const = function
    | Const c ->
       (
         match c with
         | Normal _ -> true
         | _ -> false
       )
    | _ -> false in
  let is_reg = function
    | Reg r ->
       begin
         match r with
         | CommonReg r' -> true
         | _ -> false
       end
    | _ -> false in
  let match_get_pc_thunk op e =
    (is_call_op op)&&(is_get_pc_thunk e) in
  (* it matches the instruction below and get the used register
   *
              77  4b8:   81 c3 3c 1b 00 00     add    $0x1b3c,%ebx
   *)
  let match_global_tbl op e =
    let es = p_exp e in
    (match_add op) && (String.exists es "_GLOBAL_OFFSET_TABLE_") in
  let update_track i =
    match i with
    | TripleInstr (p, e1, e2, l, pre) ->
       begin
         r := index_of_reg e1;
         rv := value_of_reg e2 l;
         TripleInstr(p, e1, Label "$_GLOBAL_OFFSET_TABLE_", l, pre)
       end
    | _ ->
       failwith "unsupport pic register pattern in update track" in


  (* pic locating usage helpers*)

  object (self)
    val mutable label : int list = []
    val mutable instrs : instr list = []
    val mutable sec : section list = []

    method check_elf =
      let ls = read_file "elf.info" in
      let l = List.nth ls 0 in
      if String.exists l "ELF 32-bit" && String.exists l "shared object" then
        true
      else false


    method pic_locating i =
      let v_exp e =
        match e with
        | Ptr s ->
           begin
             match s with
             | BinOP_PLUS (r', addr) ->
                if (reg_to_index r') = !r then
                  begin
                    let des = addr + !rv in
                    match self#check_sec des with
                    | Some s ->
                       begin
                         let rs = p_reg r' in
                         let s_label = Printf.sprintf
                                         "S_%s@GOTOFF(%s)" (dec_hex des) rs in
                         (* label <- (s.sec_name, des)::label; *)
                         label <- des::label;
                         Label s_label
                       end
                    | None ->
                       e
                  end
                else
                  e
             | BinOP_MINUS (r', addr) ->
                if (reg_to_index r') = !r then
                  begin
                    let des = !rv - addr in
                    match self#check_sec des with
                    | Some s ->
                       begin
                         let rs = p_reg r' in
                         let s_label = Printf.sprintf
                                         "S_%s@GOTOFF(%s)" (dec_hex des) rs in
                         (* label <- (s.sec_name, des)::label; *)
                         label <- des::label;
                         Label s_label
                       end
                    | None ->
                       e
                  end
                else e
             | _ -> e
           end
        | _ -> e in
      match i with
      | SingleInstr (p, l, pre) -> i
      | DoubleInstr (p, e, l, pre) -> DoubleInstr (p, v_exp e, l, pre)
      | TripleInstr (p, e1, e2, l, pre) -> TripleInstr (p, v_exp e1, v_exp e2, l, pre)
      | FourInstr (p, e1, e2, e3, l, pre) -> FourInstr (p, e1, v_exp e2, e3, l, pre)
      | FifInstr (p, e1, e2, e3, e4, l, pre) -> FifInstr (p, e1, v_exp e2, e3, e4, l, pre)


    method traverse =
      let rec aux l =
        match l with
        | [] -> []
        | (h1::[]) -> h1::[]
        | (h1::h2::t) ->
           begin
             match h1 with
             | DoubleInstr (p, e, l, pre) ->
                (* if (match_global_tbl p e2) then *)
                if (match_get_pc_thunk p e) then
                  begin
                    let h2' = update_track h2 in
                    h1::h2'::(aux t)
                  end
                else
                  begin
                    let i' = self#pic_locating h1 in
                    i'::(aux (h2::t))
                  end
             | _ ->
                begin
                  let i' = self#pic_locating h1 in
                  i'::(aux (h2::t))
                end
           end in
      if self#check_elf then
        begin
          instrs <- aux instr_list;
          unify_int_list label
        end
      else
        begin
          instrs <- instr_list;
          []
        end

    method get_instrs =
      instrs

    method section_collect =
      let filelines = File.lines_of "sections.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x"^(List.nth items 1))
        and size = int_of_string ("0x"^(List.nth items 3))
        and secname = List.nth items 0 in
        sec <- {sec_name=secname; sec_begin_addr=addr;
                sec_size=size}::sec
      in
      Enum.iter help filelines

    method check_sec addr =
      let rec help l addr =
        match l with
        | h::t -> (
          let b = h.sec_begin_addr in
          let e = b + h.sec_size in
          if (addr>=b && addr < e) then
            Some (h)
          else  help t addr )
        | [] -> None in
      help sec addr

    method init =
      let ls = read_file "pic_thunk.info" in
      let l' = List.nth ls 0 in
      let l1 = String.uppercase (String.trim l') in
      pic_addr := "0x"^l1;

    initializer
      self#section_collect;
      self#init;

  end
