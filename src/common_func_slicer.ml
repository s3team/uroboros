open Type
open Pp_print
open Ail_utils

class virtual common_func_slicer instrs funcs =
  (* A pattern recognization based func slicing implementation. *)
  let dec_hex (s : int) : string = "0x" ^ Printf.sprintf "%X" s in

  let func_set = Hashtbl.create 40 in

  object (self)
    val mutable baddr : int = -1
    val mutable eaddr : int = -1
    val mutable label : string = ""
    val mutable funcs' = []
    val mutable func_begins = []
    val mutable addr_set : int list = []
    val mutable text_b_addr : int = 0
    val mutable text_e_addr : int = 0

    (*this is what we alreadly have, use it as a filter*)
    val mutable last_nop : bool = false
    val mutable last_ret : bool = false
    val mutable last_xchg : bool = false
    val mutable last_special : bool = false
    val mutable last_jmp : bool = false
    val mutable last_pop : bool = false
    val mutable data_field : bool = false
    method virtual process2 : unit

    method check_text e =
      let check c =
        let n = c.func_name in
        try
          let n' = String.sub n 2 (String.length n - 2) in
          let n1 = int_of_string n' in
          n1 >= text_b_addr && n1 < text_e_addr
        with
        (* it might be function name symbols, with uninitized function begin addr *)
        | _ ->
          false
      in
      match e with
      | Symbol s -> (
          match s with
          | CallDes c -> if c.is_lib = false then check c else false
          | _ -> false)
      | _ -> false

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
      let func = "S_" ^ dec_hex baddr in
      if Hashtbl.mem func_set func then
        let f' = Hashtbl.find func_set func in
        Hashtbl.replace func_set func
          {
            f' with
            func_begin_addr = baddr;
            func_end_addr = eaddr;
            is_lib = false;
          }
      else
        let f' =
          {
            func_name = func;
            func_begin_addr = baddr;
            func_end_addr = eaddr;
            is_lib = false;
          }
        in
        Hashtbl.replace func_set func f'
    (* end *)

    method filter_addr_by_secs (bl : int list) : int list =
      ignore (Sys.command "python3 init_sec_adjust.py");
      let il = read_file "init_sec.info" in
      let l = List.nth il 0 in
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x" ^ List.nth items 1)
      and size = int_of_string ("0x" ^ List.nth items 3) in
      List.filter (fun n -> n < addr || n >= addr + size) bl

    method update_text_info =
      (* we manually add the begin address of text section as the first address of a function,
       * otherwise it would crash whening basic blocks try to add its function info *)
      let il = read_file "text_sec.info" in
      let l = List.nth il 0 in
      let items = Str.split (Str.regexp " +") l in
      text_b_addr <- int_of_string ("0x" ^ List.nth items 1);
      text_e_addr <- text_b_addr + int_of_string ("0x" ^ List.nth items 3)

    method build_func_info =
      func_begins <- unify_int_list func_begins;
      func_begins <- self#filter_addr_by_secs func_begins;
      (*    func_begins <- text_b_addr::func_begins; *)
      let rec help fbs =
        match fbs with
        | h1 :: h2 :: t ->
            baddr <- h1;
            eaddr <- h2;
            self#update;
            help (h2 :: t)
        | h :: [] ->
            (* bug fixed:  instrs are in reverse orders *)
            let ei = List.nth instrs 0 in
            let le = get_loc ei in
            baddr <- h;
            eaddr <- le.loc_addr;
            self#update
        | [] -> ()
      in
      (* ) help (List.rev func_begins) *)
      help func_begins

    method update_func =
      let help l = Hashtbl.replace func_set l.func_name l in
      List.iter help funcs
    (*           addr_set <- List.map (fun f ->
                                  print_string ((dec_hex f.func_begin_addr)^"\n");
                                  f.func_begin_addr) funcs ;
        print_string "-------------\n";
          () *)

    method print_f (fl : func list) =
      List.iter
        (fun f ->
          print_string f.func_name;
          print_string " ";
          print_string (dec_hex f.func_begin_addr);
          print_string " ";
          print_string (dec_hex f.func_end_addr);
          print_string "\n")
        fl;
      fl

    method get_func_list =
      let help fn f fl = f :: fl in
      Hashtbl.fold help func_set []

    method funcaddr_from_file =
      let t = read_file "faddr.txt" in
      let t1 = read_file "faddr_call.txt" in
      let func_begins2 = List.map int_of_string t in
      let func_begins3 = List.map int_of_string t1 in
      func_begins <- func_begins @ func_begins2 @ func_begins3;

    method dump_funclist =
      let dec_hex (s : int) : string = "0x0" ^ Printf.sprintf "%x" s in
      let bs = unify_int_list func_begins in
      let oc = open_out_gen [ Open_append; Open_creat ] 0o666 "fadd1.txt" in
      List.iter (fun l -> Printf.fprintf oc "%s\n" (dec_hex l)) bs;
      close_out oc

    method get_funcs =
      self#process2;
      (* self#dump_funclist; *)
      self#funcaddr_from_file;
      self#build_func_info;
      let fl = self#get_func_list in
      print_string "\tfunc number: ";
      print_int (List.length fl);
      print_string "\n";
      fl
  end
