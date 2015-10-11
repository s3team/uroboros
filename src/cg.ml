open Visit
open Type

open Ail_utils

class cg =
  (* A Call Graph construction impelmentation.
   *
   * In general, it's not possible to build CG precisely from assembly code.
   * Think about indirect calls / branches, for example.
   *
   *  currently, in my design, CG should process before reassemble plguin.
   *  see cfg_exp's pattern match for details
   *
   *   1. construct simple cg based on direct control flow transfer (call/jump)
   *   2. do value-set analysis towards indirect control flow transfer (but be caucious as VSA is limited in some cases as well )
   *   3. update CG for several rounds (theoretically, until no change at all)
   *
   *)

  (*
   * the key of this table is the address of control flow transfer,
   * the value is a list of all the possible control flow transfer
   * destinations (for the direct transfer case, then we just have one item in the list)
   * *)
  let dec_hex (s:int) : string =
    "0x"^(Printf.sprintf "%X" s) in
  let cg_tbl = Hashtbl.create 500
  and cfi_tbl = Hashtbl.create 500 in

  object(self)
    inherit ailVisitor

    (* currently, cfg_exp can only identify all the **direct** CF destination
     *  *)

    method update_cgtbl l func =
      if Hashtbl.mem cg_tbl l.loc_addr then
        let ll = Hashtbl.find cg_tbl l.loc_addr in
        Hashtbl.replace cg_tbl l.loc_addr (func::ll)
      else Hashtbl.replace cg_tbl l.loc_addr [func]

    method update_cfitbl func l =
      (* print_string (func.func_name); *)
      (* print_string "\n"; *)
      if Hashtbl.mem cfi_tbl func.func_name then
        let ll = Hashtbl.find cfi_tbl func.func_name in
        Hashtbl.replace cfi_tbl func.func_name (l::ll)
      else Hashtbl.add cfi_tbl func.func_name [l]

    method print_func =
      let help f =
        (* if f.func_name = "S_0x80484CB" then *)
        begin
          print_string f.func_name;
          print_string "\n";
          print_string ("    "^(dec_hex f.func_begin_addr));
          print_string "\n";
          print_string ("    "^(dec_hex f.func_end_addr));
          print_string "\n";
        end
      in List.iter help funcs

    method func_info l =
      let rec help funcs =
        match funcs with
        | h::[] ->
           begin
             if l.loc_addr >= h.func_begin_addr && l.loc_addr < h.func_end_addr then
               h
             else
               (
                 failwith ((dec_hex l.loc_addr)^"can't find coresponding functions")
               )
           end
        | h::t ->
           begin
             if l.loc_addr >= h.func_begin_addr && l.loc_addr < h.func_end_addr then
               h
             else help t
           end
        | _ -> failwith "can't find coresponding functions"
      in
      help funcs

    method cg_process e l =
      match e with
      | Symbol s ->
         begin
           match s with
           | JumpDes d ->
              begin
                let f = self#func_info l in
                if d >= f.func_begin_addr && d < f.func_end_addr then
                  (
                    (* self#update_cgtbl l f *)
                    (* print_string (dec_hex l.loc_addr); *)
                    (* print_string f.func_name; *)
                    (* print_string "\n"; *)
                    ()
                  )
                else
                  (
                    (* print_string f.func_name; *)
                    (* print_string "\n"; *)
                    self#update_cgtbl l f
                  )
              end
           | CallDes f when (f.is_lib = false) ->
              self#update_cgtbl l f
           | _ -> ()
         end
      | _ -> ()

    method vinst_tail instrs =
      let rec aux il acc = 
      (* check whether is control flow transfer opcodes *)
      let is_dct op e =
        let is_ct =
          match op with
          | ControlOP c ->
             begin
               match c with
               | Jump _ -> true (* jump ops *)
               | CALL -> true (* call *)
               | _ -> false
             end
          | _ -> false in
        if is_ct = false then false
        else
          match e with
          | Symbol s ->
             begin
               match s with
               | JumpDes _ -> true
               | CallDes _ -> true
               | _ -> false
             end
          | _ -> false in
      match il with
      | ((DoubleInstr (p, e, l, _) as h)::t) when (is_dct p e) ->
         self#cg_process e l;
         aux t (h::acc)
      | h::t ->
         aux t (h::acc)
      | [] -> List.rev acc in
	aux instrs []

     method visit (instrs: instr list) =
        self#vinst_tail instrs

    (* in CFI protection of `ret`, we have to identify all the possible caller
     * of each function( each ret)
     * create cfi sepcfied table;
     * function <--> [call_addr1; call_addr2; ...] *)
    method cfi_specified_tbl =
      let help key value =
        List.iter (fun f -> self#update_cfitbl f key) value in
      Hashtbl.iter help cg_tbl


    method print_cg_graph =
      let rec help key value =
        print_string (dec_hex key);
        List.iter (fun f -> print_string ("    "^f.func_name^"\n")) value in
      Hashtbl.iter help cg_tbl

    method print_cfi_sepcified_graph =
      let dec_hex (s:int) : string =
        "0x"^(Printf.sprintf "%X\n" s) in
      let rec help key value =
        print_string (key^"\n");
        List.iter (fun l -> print_string ("    "^dec_hex(l))) value in
      self#cfi_specified_tbl;
      Hashtbl.iter help cfi_tbl

    method get_cg_table =
      (* self#print_cg_graph; *)
      cg_tbl

    (* method print_hashtb cfi_tbl = *)
    (* Hashtbl.iter (fun k v -> print_string k; print_string "\n"; ()) cfi_tbl *)

    method get_cfi_tbl =
      self#cfi_specified_tbl;
      (* self#print_hashtb cfi_tbl; *)
      cfi_tbl

  end
