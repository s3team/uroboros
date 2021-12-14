open Batteries

open Visit
open Type
open Pp_print

open Ail_utils

type t = {bn : string; baddr : int; eaddr : int}

class cfg =
  (* A CFG construction impelmentation.
   *
   *  CFG construction is the fundation of diversify transformation as well as
   *  fine-grained CFI 
   *
   * *)

  (* simple cfg table, when:
   *     0xdeadbeef:   call S_0xabcdefab;
   * then creating entry like :
   *     0xdeadbeef ; 0xabcdefab
   * *)

  let cfg_table = Hashtbl.create 40
  and cfg_bdiv_table = Hashtbl.create 40
  and counter = ref 0
  and dec_hex (s:int) : string =
    "0x"^(Printf.sprintf "%X" s) in

  object(self)
    (* keep track of whether we have found the entry point *)
    val mutable found_entry = false
    (* it is for sure that the next instruction of next to last bb is the
     * the entry point of current bb*)
    val mutable skip_entry = false
    val mutable entry_loc : loc = {loc_label = ""; loc_addr = 0;loc_visible = true;}
    val mutable last_loc : loc = {loc_label = ""; loc_addr = 0; loc_visible = true;}
    (* last loc for the whole instruction sequence *)
    val mutable end_loc : loc = {loc_label = ""; loc_addr = 0; loc_visible = true;}
    val mutable entry_instr : instr =
      SingleInstr (CommonOP (Other NOP), {loc_label=""; loc_addr = 0; loc_visible=true;}, None)
    val mutable bb_list = []
    val mutable bl = []
    (* opt: opt the performance *)
    val mutable bl_sort = Array.create 1 {bn =""; baddr = 0; eaddr = 0;}

    inherit ailVisitor


    (* currently, cfg_exp can only identify all the **direct** CF destination
     *  *)
    method cfg_exp e l =
      let a = l.loc_addr in
      match e with
      | Symbol e' ->
         begin
           match e' with
           | JumpDes d ->
              Hashtbl.replace cfg_table a d
           | CallDes f ->
              if f.is_lib = false then
                Hashtbl.replace cfg_table a f.func_begin_addr
              else ()
           | _ -> ()
         end
      | _ -> ()

    method get_loc i =
      match i with
      | SingleInstr (_, l, _) -> l
      | DoubleInstr (_, _, l, _) -> l
      | TripleInstr (_, _, _, l, _) -> l
      | FourInstr (_, _, _, _, l, _) -> l
      | FifInstr (_, _, _, _, _, l, _) -> l

    method vinst' (i: instr) =
      (* check whether is control flow transfer opcodes *)
      let is_ct op =
        match op with
        | ControlOP c ->
           begin
             match c with
             | Jump _ -> true (* jump ops *)
             | CALL -> true (* call *)
             | _ -> false
           end
        | _ -> false in
      match i with
      | DoubleInstr (p, e, l, _) when (is_ct p) ->
         self#cfg_exp e l;
         i
      | _ -> i

    method vinst (i: instr) =
      (* locating the exit point of basic block
       * generally, it should be a control transfer op
       *
       * If I was right, it is not a madantory to identify all
       * the possible control flow transfer op in the context
       * of basic block reordering
       * *)
      let is_last_instr l =
	(*
        let llast = sec_end_memo ".text" in
        (* the last instrction serves as the exit point of the last bb *)
        if l.loc_addr = (llast - 1) then true
	*)
	if l.loc_addr = end_loc.loc_addr then true
        else false in
      let bb_exit op l =
        match op with
        | ControlOP c ->
           begin
             match c with
             | Jump _ -> true (* jump ops *)
             (*| CALL -> true (* call *)*)
             | RET ->  true
             | RETN -> true
             | _ -> false
           end
        | _ -> false
      and bb_entry i =
        let l = self#get_loc i in
        let s = l.loc_label in
        (* this is the thing, we only consider S_0xXXXXX : as entry point *)
        (* if String.exists s "S_" && String.exists s ":" then *)
        if String.exists s ":" then true
        else false
      and help_entry i =
        if found_entry = true then
          (*
           * if we have found the entry, then this is the exit
           * of current basic block, and a new bb is found
           * *)
          begin
            let bn = "BB_"^(string_of_int !counter) in
            counter := !counter + 1;
            let b = {bf_name = ""; bblock_name = bn;
                     bblock_begin_loc = entry_loc; bblock_end_loc = last_loc;
                    bblock_head_instr = entry_instr } in

            (*               print_string "\n";
              print_string b.bblock_name;
              print_string " ";
              print_string (dec_hex b.bblock_begin_loc.loc_addr);
              print_string " ";
              print_string (dec_hex b.bblock_end_loc.loc_addr);
              print_string " ";
              print_string "\n";
             *)
            bb_list <- b::bb_list;

            last_loc <- self#get_loc i;
            entry_loc <- self#get_loc i;
            entry_instr <- i;
            found_entry <- true;
            i
          end
        else
          (* if we haven't found the entry, then this is the entry*)
          begin
            found_entry <- true;
            entry_loc <- self#get_loc i;
            entry_instr <- i;
            last_loc <- self#get_loc i;
            i
          end
      and help_exit i =
        if found_entry = true then
          (*
           * if we have found the entry, then this is the exit
           * of current basic block, and a new bb is found
           * *)
          begin
            last_loc <- self#get_loc i;
            let bn = "BB_"^(string_of_int !counter) in
            counter := !counter + 1;
            let b = {bf_name = ""; bblock_name = bn;
                     bblock_begin_loc = entry_loc; bblock_end_loc = last_loc;
                    bblock_head_instr = entry_instr } in
	(*

              print_string "\n";
              print_string b.bblock_name;
              print_string " ";
              print_string (dec_hex b.bblock_begin_loc.loc_addr);
              print_string " ";
              print_string (dec_hex b.bblock_end_loc.loc_addr);
              print_string " ";
              print_string "\n";
	*)

            bb_list <- b::bb_list;
            found_entry <- false;
            skip_entry <- true;
            i
          end
        else if (is_last_instr (get_loc i)) = true then
	(* this is the beginning and end! *)
	    begin
		let tloc = self#get_loc i in
            	let bn = "BB_"^(string_of_int !counter) in
            	counter := !counter + 1;
            	let b = {bf_name = ""; bblock_name = bn;
                	bblock_begin_loc = tloc; bblock_end_loc = tloc;
                        bblock_head_instr = i} in
            bb_list <- b::bb_list;
            i
	    end
	else
          (* I haven't found label, so skip it*)
          begin
            last_loc <- self#get_loc i;
            i
          end
      in
      match i with
      | _ when skip_entry = true && (is_last_instr (get_loc i))->
	(* this is the beginning and end! *)
	    begin
		let tloc = self#get_loc i in
            	let bn = "BB_"^(string_of_int !counter) in
            	counter := !counter + 1;
            	let b = {bf_name = ""; bblock_name = bn;
                	bblock_begin_loc = tloc; bblock_end_loc = tloc;
                        bblock_head_instr = i} in
            bb_list <- b::bb_list;
            i
	    end
      | _ when skip_entry = true ->
         begin
           entry_loc <- self#get_loc i;
           entry_instr <- i;
           found_entry <- true;
           skip_entry <- false;
           last_loc <- self#get_loc i;
           let module OP = Opcode_utils in
           if OP.is_control_transfer_op @@ get_op i then
             (* if it is a control transfer instruction, and it is the beginning of a bb,
            then it is also the end of a bb. *)
             help_exit i
           else
             i
         end
      | _ when is_last_instr (get_loc i) = true ->
         help_exit i
      | _ when bb_entry i ->
         help_entry i;
         let module OP = Opcode_utils in
         if OP.is_control_transfer_op @@ get_op i then
           (* if it is a control transfer instruction, and it is the beginning of a bb,
            then it is also the end of a bb. *)
           help_exit i
         else
           i
      | DoubleInstr (p, e, l, _) when (bb_exit p l) ->
           help_exit i
      | SingleInstr (p, l, _) when (bb_exit p l) ->
         help_exit i
      | _ ->
         begin
           last_loc <- self#get_loc i;
           i
         end


    method visit il =
      let last_i = List.nth il (List.length il - 1) in
      end_loc <- get_loc last_i;
      let il' = List.map self#vinst il in
      bl <- self#update_bl;
      self#fb_list bl;
      let bl' = List.sort (fun b1 b2 ->
                            b1.bblock_begin_loc.loc_addr - b2.bblock_begin_loc.loc_addr
                ) bl in
      let bl1 = List.map (
                     fun b ->
                     {
                       bn = b.bblock_name;
                       baddr = b.bblock_begin_loc.loc_addr;
                       eaddr = b.bblock_end_loc.loc_addr;
                     }
                   ) bl' in
      bl_sort <- Array.of_list bl1;
      il'

    method get_fbl =
      let help f bl =
        let bl' = List.rev bl in
        Hashtbl.replace cfg_bdiv_table f bl' in
      Hashtbl.iter help cfg_bdiv_table;
      (*self#print_fbl;*)
      cfg_bdiv_table

    method get_bbl =
      bl

    method print_fbl =
      let help fn bl =
        print_string fn;
        print_string "\n";
        let h' b =
          print_string "    ";
          print_string b.bf_name;
          print_string " ";
          print_string b.bblock_name;
          print_string " ";
          print_string (dec_hex b.bblock_begin_loc.loc_addr);
          print_string " ";
          print_string (dec_hex b.bblock_end_loc.loc_addr);
          print_string " ";
          print_string "\n" in
        List.iter h' bl;
        print_string "\n" in
      Hashtbl.iter help cfg_bdiv_table





    (*
     *
     *  the standard
     *
     * this function might only serve the
     * bb diversify
     *
     *  each function has its own bb list
     * *)
    method fb_list bl =
      let help b =
        let fn = b.bf_name in
        if Hashtbl.mem cfg_bdiv_table fn then
          (
            let bl = b::(Hashtbl.find cfg_bdiv_table fn) in
            Hashtbl.replace cfg_bdiv_table fn bl
          )
        else
          Hashtbl.add cfg_bdiv_table fn [b]
      in
      List.iter help bl

    method update_bl =
      let funcs' = List.sort (
                       fun f1 f2 ->
                       f1.func_begin_addr - f2.func_begin_addr
                     ) funcs
      and bls' = List.sort (
                     fun b1 b2 ->
                     b1.bblock_begin_loc.loc_addr - b2.bblock_begin_loc.loc_addr) bb_list
      and addr_comp addr1 addr2 =
        addr1 - addr2 in
      let rec assign fl bl acc =
        match (fl,bl) with
        | (hf::tf, hb::tb) ->
           begin
             let b_bl = hb.bblock_begin_loc
             and b_el = hb.bblock_end_loc
             and f_ba = hf.func_begin_addr
             and f_ea = hf.func_end_addr in
             if (addr_comp b_bl.loc_addr f_ba) >=0
                && (addr_comp b_el.loc_addr f_ea) <= 0 then
               let hb' = {hb with bf_name = hf.func_name} in
               (
                 assign fl tb (hb'::acc)
               )
             else assign tf bl acc
           end
        | (fl', []) -> List.rev acc
        | ([], hb::tb) ->
		   (
             failwith ((dec_hex hb.bblock_begin_loc.loc_addr)^" undentified basic block")
		   ) in
      assign funcs' bls' []


    (* CFG for each function
     *  for each function f:
     *  Some (
     *  (BBk_1; BB_2)
     *  (BB_3; BB_4)
     *  (BB_5; BB_6)
     *  (BB_7; BB_8)
     *  )
     *   or
     *   None;
     *
     *  Some([]) ==> T
     *  Some("INTER_PROCEDURAL") ==> call xxx
     *
     *)
    method recover_cfg =
      let is_jmp = function
        | ControlOP op ->
           begin
             match op with
             | Jump JMP -> true
             | _ -> false
           end
        | _ -> false in
      let is_cond_jmp = function
        | ControlOP op ->
           begin
             match op with
             | Jump JMP -> false
             | Jump JMPQ -> false
             | Jump _ -> true
             | _ -> false
           end
        | _ -> false in
      let is_call op =
        match op with
        | ControlOP c ->
           begin
             match c with
             | CALL -> true
             | _ -> false
           end
        | _ -> false in
      let is_ret op =
        match op with
        | ControlOP c ->
           begin
             match c with
             | RET | RETN -> true
             | _ -> false
           end
        | _ -> false in
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
        | Label s -> s = "ret" || s = "retq"
        | _ -> false in
      let next_bb (bnl : string list) bn =
        let bn' = get_next_bb bn in
        if List.mem bn' bnl then bn'
        else "INTER" in
      (* find basic block name by location *)
      let bbn_byloc addr =
        let cmp addr b =
          if b.baddr <= addr && b.eaddr >= addr
          then 0
          else if b.baddr > addr then -1
          else 1 in
        (* implement a binary search, the original way seems too slow *)
        let rec bs min max =
          if min > max then
	begin
	    print_string (dec_hex addr);
	    print_string "\n";
		let blen = Array.length bl_sort in
		let blast = Array.get bl_sort (blen-1) in
		let bfist = Array.get bl_sort 0 in
		print_string (dec_hex bfist.baddr);
		print_string "\n";
		print_string (dec_hex bfist.eaddr);
		print_string "\n";
            assert(false)
	end
          else
            let mid = (min + max) / 2 in
	let delta =
	try
            cmp addr (Array.get bl_sort mid)
	with  _ ->
	begin
	print_string "failed ! ";
	print_int mid;
	print_string "\n";
	print_int (Array.length bl_sort);
	print_string "\n";
	0
	end
	in
            if delta = 0 then
              (Array.get bl_sort mid).bn
            else
              if delta < 0
              then bs min (mid-1)
              else bs (mid + 1) max
        in
        bs 0 (Array.length bl_sort - 1)
      in
      (* it is true that ret is a kind of indirect control transfer,
       * but assign "T" for each ret instruction could negatively affect
       * the accuracy of our static anaylsis, so let's distinguish ret with
       * regular indirect control flow transfer *)
      let indirect_ret i =
        let l = get_loc i in
        let bn = bbn_byloc l.loc_addr in
        if bn = "" then assert(false)
        else (bn, (J, Some("RET"))) in
      let indirect i =
        let l = get_loc i in
        let bn = bbn_byloc l.loc_addr in
        if bn = "" then assert(false)
        else (bn, (J, Some("T"))) in
      (* abandon *)
      let dir_sin_c i =
        let l = get_loc i in
        let bn = bbn_byloc l.loc_addr in
        if bn = "" then assert(false)
        else (bn, (J, Some("INTER"))) in
      let dir_c bnl i =
        let l = get_loc i in
        let bn = bbn_byloc l.loc_addr in
        if bn = "" then assert(false)
        else
          begin
            let bn' = next_bb bnl bn in
            let b1 = (bn, (J, Some("INTER"))) in
            let b2 = (bn, (J, Some(bn'))) in
            (b1, b2)
          end in
      let dir_sin_j i e =
        let l = get_loc i in
        let sn = bbn_byloc l.loc_addr in
        if sn = "" then assert(false)
        else
          begin
            let es = p_exp e in
            let en = recover_addr_from_label es in
	    if en = -1 then (* instr replace replace call into jmp; and one jmp(near GLOBAL_OFF_GOT) can't find des*)
             (sn, (J, Some("T")))
	    else
	    begin
              let dn = bbn_byloc en in
              if dn = "" then
                begin
                  print_string "\n";
                  print_string es;
                  assert(false)
                end
              else
                (sn, (J, Some(dn)))
	    end
          end in
      let dir_dou_j bnl i e =
        let (sn, _) as d1 = dir_sin_j i e in
        let sn' = next_bb bnl sn in
        (d1, (sn, (F, Some(sn'))))  in
      let dir_fall bnl i =
        let l = get_loc i in
        let sn = bbn_byloc l.loc_addr in
        if sn = "" then assert(false)
        else
          let dn = next_bb bnl sn in
          (sn, (F, Some(dn))) in
      let is_func e =
        match e with
          | Symbol (CallDes _) -> true
          | _ -> false in
      let aux (bnl : string list) acc i =
        match i with
        | SingleInstr (p, _, _) when (is_ret p) ->
           (indirect_ret i)::acc
        (* 2 seconds *)
        | DoubleInstr (_, e, _, _) when (is_indirect e) ->
           (indirect i)::acc
        | DoubleInstr (p, _, _, _) when (is_call p) ->
           let (d1, d2) = (dir_c bnl i) in
           d1::d2::acc
        (* is it possible that jmp strcpy *)
        (* 2 seconds *)
        | DoubleInstr (p, e, _, _) when (is_jmp p) && (is_func e = true) ->
           (dir_sin_c i)::acc
        | DoubleInstr (p, e, _, _) when (is_jmp p) && (is_func e = false) ->
           (dir_sin_j i e)::acc
        (* 10 second *)
        | DoubleInstr (p, e, _, _) when (is_cond_jmp p) && (is_func e = false)->
           let (d1, d2) = (dir_dou_j bnl i e) in
           d1::d2::acc
        | DoubleInstr (p, e, l, _) when (is_cond_jmp p) && (is_func e = true)->
           print_endline (p_exp e);
           print_endline @@ dec_hex @@ l.loc_addr;
           assert(false)
        (* 5 seconds *)
        | _ -> (dir_fall bnl i)::acc
        | _ -> acc in
      Hashtbl.fold (
          fun f bl cfg_f ->
          (
            let bnl : string list = List.map (fun b -> b.bblock_name) bl in
            let cfg_l =
              ( List.map (fun b -> b.bblock_end_loc) bl
                |> sort_loc
                |> get_instr_byloc instrs
                |> List.fold_left (aux bnl) []
              ) in
            (f, cfg_l)::cfg_f
          )
        ) cfg_bdiv_table []

    method print_cfg_graph (cfg_t : (string * (string * (control * string option)) list
                                    ) list) =
      let aux c =
        match c with
        | F -> "Fall through"
        | J -> "Jump" in
      List.iter (
          fun (f, v) ->
          let sl = List.map (
                       fun t ->
                       match t with
                       | (s, (c, Some("T"))) -> s^" --"^(aux c)^"--> T\n"
                       | (s, (c, Some(d))) -> s^" --"^(aux c)^"--> "^d^"\n"
                       | (s, (c, None)) -> s^" --"^(aux c)^"--> Undefined\n"
                     ) v in
          let s = List.fold_left (^) "" sl in
          print_string f;
          print_string (" :\n"^s^"\n")
        ) cfg_t;

    method get_cfg_table instr_list =
      instrs <- instr_list;
      let cfg_t = self#recover_cfg in
      (* self#print_cfg_graph cfg_t; *)
      cfg_t

  end
