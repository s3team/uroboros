open Visit
open Type
open Pp_print

open Ail_utils

class func_slicer instrs funcs =
  (* A pattern recognization based func slicing implementation.

  *)




  let dec_hex (s:int) : string =
    "0x"^(Printf.sprintf "%X" s) in

  let func_set = Hashtbl.create 40 in

  object(self)

    val mutable baddr : int = -1
    val mutable eaddr : int = -1
    val mutable label : string = ""
    val mutable funcs' = []
    val mutable func_begins = []
    val mutable addr_set : int list = []
    val mutable text_b_addr : int = 0
    val mutable text_e_addr : int = 0


    val mutable last_nop : bool = false
    val mutable last_ret : bool = false
    val mutable last_xchg : bool = false
    val mutable last_special : bool = false
    val mutable last_jmp : bool = false
    (*this is what we alreadly have, use it as a filter*)

    method update =
      (*           if List.mem baddr addr_set  then
            (
            print_string ((dec_hex baddr)^"\n");
            ()
            )
          else *)
      (* begin *)
      (* used to be a bug here, function name in the parser is
       * S_0xdeadbeef while here create function name as 0xdeadbeef *)
      let func = "S_"^(dec_hex baddr) in
      if Hashtbl.mem func_set func then
        let f' = Hashtbl.find func_set func in
        Hashtbl.replace func_set func {f' with func_begin_addr = baddr;
                                               func_end_addr = eaddr; is_lib = false}
      else
        let f' = {func_name = func; func_begin_addr = baddr;
                  func_end_addr = eaddr; is_lib = false} in
        Hashtbl.replace func_set func f'
    (* end *)

    method filter_addr_by_secs (bl : int list) : int list =
      Sys.command("python init_sec_adjust.py");
	  let il = read_file "init_sec.info" in
	  let l = List.nth il 0 in
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x"^(List.nth items 1))
      and size = int_of_string ("0x"^(List.nth items 3)) in
	  List.filter (
	  	  fun n ->
	  	  n < addr || n >= (addr+size)
	  	) bl

	(* we manually add the begin address of text section as the first address of a function,
	 * otherwise it would crash whening basic blocks try to add its function info *)
  	method update_text_info =
  	  let il = read_file "text_sec.info" in
  	  let l = List.nth il 0 in
      let items = Str.split (Str.regexp " +") l in
      text_b_addr <- int_of_string ("0x"^(List.nth items 1));
      text_e_addr <- text_b_addr + int_of_string ("0x"^(List.nth items 3))

    method build_func_info =
      func_begins <- unify_int_list func_begins;
      func_begins <- self#filter_addr_by_secs func_begins;
	  (*    func_begins <- text_b_addr::func_begins; *)
      let rec help fbs =
        match fbs with
        | h1::h2::t ->
           begin
             baddr <- h1;
             eaddr <- h2;
             self#update;
             help (h2::t)
           end
        | h::[] ->
           begin
             (* bug fixed:  instrs are in reverse orders *)
             let ei = List.nth instrs 0 in
             let le = get_loc ei in
             baddr <- h;
             eaddr <- le.loc_addr;
             self#update
           end
        | [] -> () in
     (* ) help (List.rev func_begins) *)
      help func_begins

    method process =
      let is_begin op e =
        let is_stack = match op with
          | StackOP PUSH -> true
          | _ -> false in
        let is_ebp = match e with
          | Reg (StackReg EBP) -> true
          | Reg (StackReg EBP) -> true
          | _ -> false in
        is_stack && is_ebp in
      let rec help l =
        match l with
        | h::[] ->
           begin
             let loc' = get_loc h in
             if eaddr = (-1) then
               failwith "no function recognized at all"
             else
               eaddr <- (loc'.loc_addr);
             self#update
           end
        | h::t ->
           begin
             match h with
             | DoubleInstr (p, e, l, _) when (is_begin p e) ->
                if eaddr = (-1) then (* first function *)
                  (
                    baddr <- l.loc_addr;
                    eaddr <- l.loc_addr;
                    help t
                  )
                else
                  (
                    eaddr <- l.loc_addr;
                    self#update;
                    baddr <- l.loc_addr;
                    help t
                  )
             | _ -> ()
           end
        | _ -> failwith "process " in
      help (List.rev instrs)

    method check_text e =
      let check c =
        let n = c.func_name in
        try
          let n' = String.sub n 2 (String.length n - 2) in
          let n1 = int_of_string n' in
          n1 >=text_b_addr && n1 < text_e_addr
        with
        (* it might be function name symbols, with uninitized function begin addr *)
        | _ -> false in
      match e with
      | Symbol s ->
         begin
           match s with
           | CallDes c ->
              begin
                if c.is_lib = false then
                  check c
                else false
              end
           | _ -> false
         end
      | _ -> false

    method process2 =
      let is_s_begin op e =
        let is_stack = match op with
          | StackOP PUSH -> true
          | _ -> false in
        let is_ebp = match e with
          | Reg (StackReg EBP) -> true
          | Reg (StackReg EBP) -> true
          | _ -> false in
        is_stack && is_ebp in
      let is_s_begin2 op e1 e2 i =
        begin
              let is_sub = match op with
                | CommonOP (Arithm SUB) -> true
                | _ -> false in
              let is_const = match e1 with
                | Const (Normal _) -> true
                | _ -> false in
              let is_sp = match e2 with
                | Reg (StackReg ESP) -> true
                | Reg (StackReg RSP) -> true
                | _ -> false in
              is_sub && is_const && is_sp
        end in
      (* match mov    0x4(%esp),%_ *)
      (* match mov    0x8(%esp),%_ *)
      (* match mov    0x10(%esp),%_ *)
      let is_begin3 op e1 e2 =
        begin
          let c1 =
            match op with
            | CommonOP (Assign MOV) -> true
            | CommonOP (Assign MOVZBL) -> true
            | _ -> false in
          let c2 =
            match e1 with
            | Ptr (BinOP_PLUS(StackReg ESP,4)) -> true
            | Ptr (BinOP_PLUS(StackReg ESP,8)) -> true
            | Ptr (BinOP_PLUS(StackReg ESP,0x10)) -> true
            | Ptr (BinOP_PLUS(StackReg ESP,0xc)) -> true
            | _ -> false in
          let c3 =
            match e2 with
            | Reg (CommonReg _) -> true
            | _ -> false in
          c1 && c2 && c3
        end in
      (*
          match for lea 0x0(%esi,%eiz, 1)
          lea 0x0(%edi,...)
          lea 0x0(%rsi,...)
          lea 0x0(%rdi,...)
       *)
      let is_nop1 op e =
        let is_lea = match op with
          | CommonOP (Assign LEA) -> true
          | _ -> false in
        let is_fourp = match e with
          | Ptr (FourOP_PLUS (r1,r2,off1,off2)) ->
             begin
               let aux1 = match r1 with
                 | CommonReg ESI -> true
                 | CommonReg EDI -> true
                 | CommonReg RSI -> true
                 | CommonReg RDI -> true
                 | _ -> false in
               let aux2 = match r2 with
                 | OtherReg EIZ -> true
                 | _ -> false in
               aux1 && aux2 && (off2 = 0) && (off1 = 1)
             end
          | _ -> false
        in
        is_lea && is_fourp in
      let is_nop2 op e1 e2 =
        let is_lea = match op with
          | CommonOP (Assign LEA) -> true
          | _ -> false in
        let is_onep = match e1 with
          | Ptr (BinOP_PLUS (r,off)) ->
             begin
               let aux1 = match r with
                 | CommonReg EDI -> true
                 | CommonReg ESI -> true
                 | CommonReg RSI -> true
                 | CommonReg RDI -> true
                 | _ -> false in
               aux1 && (off = 0)
             end
          | _ -> false in
        let is_reg = match e2 with
          | Reg (CommonReg EDI) -> true
          | Reg (CommonReg ESI) -> true
          | Reg (CommonReg RSI) -> true
          | Reg (CommonReg RDI) -> true
          | _ -> false in
        is_lea && is_reg && is_onep in
      let is_c_begin op e =
        let is_call = match op with
          | ControlOP c ->
             begin
               match c with
               | CALL -> true
               | _ -> false
             end
          | _ -> false
        and is_local = match e with
          | Symbol s ->
             begin
               match s with
               | CallDes c ->
                  begin
                    if c.is_lib = false then true
                    else false
                  end
               | _ -> false
             end
          | _ -> false in
        is_call && is_local && (self#check_text e)
      and call_des e = match e with
        | Symbol s ->
           begin
             match s with
             | CallDes c -> if c.func_begin_addr <> 0 then c.func_begin_addr
                            else let n = c.func_name in
                                 let n' = String.sub n 2 (String.length n - 2) in
                                 begin
    	                           try
                                     int_of_string n'
                                   with
                                   (* it might be function name symbols, with uninitized function begin addr *)
                                   | _ -> failwith ("uninitized function begin addr "^n)
                                 end
             | _ -> failwith "didn't support in call_des"
           end
        | _ -> failwith "didn't support in call_des"
      in
      let rec help h =
        match h with
        (* multiple lea ... can be used to align together *)
        | TripleInstr(p, e1, e2, l, _) when (is_nop1 p e2) ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- true
           end
        (* multiple lea    0x0(%edi),%edi can be used to align together *)
        | TripleInstr(p, e1, e2, l, _) when (is_nop2 p e2 e1) ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- true
           end
        (* here is our trategy, whenever detect the instruction
             lea    0x0(%esi,%eiz,1),%esi
           consider the next instruction as function beginning.
         *)
        | SingleInstr (CommonOP (Other NOP),_,_) ->
           begin
             last_nop <- true;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false
           end
        | TripleInstr (CommonOP (Assign XCHG), Reg (CommonReg AX),
                       Reg (CommonReg AX), _, _)
          ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- true;
             last_jmp <- false;
             last_special <- false;
           end
        | SingleInstr (ControlOP RET, _, _) ->
           begin
             last_nop <- false;
             last_special <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_ret <- true
           end
        | DoubleInstr(ControlOP (Jump JMP), _, l, _) when last_nop ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_ret <- false;
             last_special <- false;
             last_jmp <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | DoubleInstr (ControlOP (Jump JMP), _,_,_) ->
           begin
             last_nop <- false;
             last_special <- false;
             last_xchg <- false;
             last_jmp <- true;
             last_ret <- false
           end
       (* | DoubleInstr (CommonOP (Assign REPZ), Label "ret", l, _) when *)
        (* last_nop || last_special || last_ret -> *)
        | DoubleInstr (CommonOP (Assign REPZ), Label "ret", l, _) when last_nop || last_special ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_ret <- false;
             last_special <- false;
             last_jmp <- false;
             func_begins <- l.loc_addr::func_begins
           end
        (* xor %eax,%eax *)
        | TripleInstr (CommonOP (Logic XOR), Reg (CommonReg EAX),
                       Reg (CommonReg EAX), l, _) when last_special ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_ret <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | h when last_special = true ->
           begin
             let c = match h with
               | DoubleInstr (p, _,_,_) ->
                  begin
                    match p with
                    | ControlOP (_) -> false
                    (*  | CommonOP (Assign _) -> true *)
                    | CommonOP _ -> false
                    | SystemOP _ -> false
                    | StackOP _ -> true
                    | ErrorOP _ -> false
                  end
               | TripleInstr (CommonOP (Assign MOV), e1,e2,_,_) ->
                  is_begin3 (CommonOP (Assign MOV)) e2 e1
               | TripleInstr (CommonOP (Assign MOVZBL), e1,e2,_,_) ->
                  is_begin3 (CommonOP (Assign MOVZBL)) e2 e1
               | TripleInstr (CommonOP (Arithm SUB), e1,e2,_,_) ->
                  is_s_begin2 (CommonOP (Arithm SUB)) e2 e1 h
               | _ -> false in
             if c = false then
               begin
                 last_special <- false;
             	 last_jmp <- false;
                 last_xchg <- false;
             	 last_ret <- false;
                 last_nop <- false
               end
             else
               begin
                 last_special <- false;
                 last_xchg <- false;
             	 last_jmp <- false;
                 last_ret <- false;
                 last_nop <- false;
                 let l = get_loc h in
                 func_begins <- l.loc_addr::func_begins
               end
           end
        | TripleInstr (p, e1, e2, l,_) when ( last_nop || last_jmp) && is_begin3 p e2 e1 ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_ret <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | SingleInstr (CommonOP (Assign (FLDZ)), l,_) when last_special ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_ret <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | (DoubleInstr (StackOP PUSH, Reg (CommonReg ESI), l, _)
          ) when last_nop || last_xchg || last_special || last_ret  || last_jmp ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_ret <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | (DoubleInstr (StackOP PUSH, Reg (CommonReg EDI), l, _)
          ) when last_nop || last_ret || last_xchg || last_special ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_ret <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | (DoubleInstr (StackOP PUSH, Reg (CommonReg EBX), l, _)) when last_ret || last_ret || last_nop || last_special ->
           begin
             last_nop <- false;
             last_xchg <- false;
             last_ret <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | TripleInstr (p, e1, e2, l, _) when (is_s_begin2 p e2 e1 h) && (last_ret || last_nop || last_special || last_xchg || last_jmp) ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | TripleInstr(CommonOP (Assign MOV), Reg (CommonReg EAX), Const (Normal 1), l,_) when last_special ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        | DoubleInstr (p, e, l, _) when (is_s_begin p e) ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false;
             func_begins <- l.loc_addr::func_begins
           end
        (* we collect all the function calls inside current modular *)
        | DoubleInstr (p, e, l, _) when (is_c_begin p e) ->
           begin
             last_nop <- false;
             last_ret <- false;
             last_xchg <- false;
             last_jmp <- false;
             last_special <- false;
             let c_des = call_des e in
             func_begins <- c_des::func_begins
           end
        | _ ->
           begin
             last_special <- false;
             last_xchg <- false;
             last_ret <- false;
             last_jmp <- false;
             last_nop <- false
           end
      in
      List.iter help (List.rev instrs)

    method update_func =
      let help l =
        Hashtbl.replace func_set l.func_name l in
      List.iter help funcs;
    (*           addr_set <- List.map (fun f ->
                                    print_string ((dec_hex f.func_begin_addr)^"\n");
                                    f.func_begin_addr) funcs ;
          print_string "-------------\n";
            () *)

    method print_f (fl : func list) =
      List.iter (
          fun f ->
          print_string f.func_name;
          print_string " ";
          print_string (dec_hex f.func_begin_addr);
          print_string " ";
          print_string (dec_hex f.func_end_addr);
          print_string "\n" ) fl;
      fl


    method get_func_list =
      let help fn f fl =
        f::fl in
      Hashtbl.fold help func_set []

    method funcaddr_from_file =
      let t = read_file "faddr.txt" in
      func_begins <- List.map int_of_string t;

    method dump_funclist =
      let dec_hex (s:int) : string =
        "0x0"^(Printf.sprintf "%x" s) in
      let bs = unify_int_list func_begins in
      let oc = open_out_gen [Open_append; Open_creat] 0o666 "fadd1.txt" in
      List.iter (fun l -> Printf.fprintf oc "%s\n" (dec_hex l)) bs;
      close_out oc


    method get_funcs =
      (*self#process2; *)
      (* self#dump_funclist; *)
      self#funcaddr_from_file;
      self#build_func_info;
      let fl = self#get_func_list in
      print_string "func number ";
      print_int (List.length fl);
      print_string "\n";
      (* self#print_f fl; *)
      fl

  end
