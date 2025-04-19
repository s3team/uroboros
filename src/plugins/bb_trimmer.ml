(*
   facilities for searching on trace
   debloating based on execution traces.

   This is not a plugin, but some ongoing work based on trace profiling.
 *)

module Trimmer_Pre = struct

    open Type

    let obtain_trace () =
      let open Ail_utils in
      read_file "bb_trace.txt"

    let obtain_sorted_uniq_trace () =
      let open Ail_utils in
      read_file "bb_sort_uniq_trace.txt"

    let addr_trace t =
      t |> List.rev_map int_of_string |> List.rev


    let obtain_sorted_uniq_join_trace () =
      let open Ail_utils in
      read_file "bb_join_sort_uniq_trace.txt"


    let addr_trace_joined t =
      let split = Str.split (Str.regexp " +") in
      t |> List.rev_map
             (fun s ->
              let [x;y]= split s in
              (int_of_string x, int_of_string y)
             ) |> List.rev


    let addr_trace_sort_uniq t =
      let open Ail_utils in
      let  module AL = Algo in
      t |> List.rev_map int_of_string |> List.rev
      |> AL.sort_uniq (fun x y -> x - y)

    let addr_trace_sort t =
      t |> List.rev_map int_of_string |> List.rev
      |> List.sort (fun x y -> x - y)

    (* we construct control transfer pairs from the trace;
       make sure the trace is sorted from smallest one
       to the largest one
     *)
    let build_tranfers_from_trace t =
      let open Ail_utils in
      let  module AL = Algo in
      let t = List.rev @@ List.rev_map int_of_string t in
      let t1 = List.rev @@ List.tl @@ List.rev t in
      let t2 = List.tl t in
      zip t1 t2  |> AL.sort_uniq (fun a b ->
           match (a,b) with
            | ((a1, a2), (b1, b2)) when a1 = b1 && a2 = b2 -> 0
            | ((a1, a2), (b1, b2)) when a1 = b1 && a2 > b2 -> 1
            | ((a1, a2), (b1, b2)) when a1 = b1 && a2 < b2 -> -1
            | ((a1, a2), (b1, b2)) when a1 < b1 -> -1
            | ((a1, a2), (b1, b2)) when a1 > b1 -> 1
            | _ -> failwith "undefined"
        )



    (* obtain a marked list of basic blocks ; the trace contains a sequence of
    bb address *)
    let bb_mark_by_addr bbl t =
      let rec aux bl acc tl =
        match (bl, tl) with
        | ((bh::bt, th::tt)) ->
           if bh.bblock_begin_loc.loc_addr = th then
             aux bt (bh::acc) tt
           else
             aux bt acc tl
        | (_, []) -> List.rev acc
        | ([], _) -> failwith "undefined trace"
      in
      aux bbl [] t


    type bb_addr_typ = BEGIN | END

    let marked_addr_range bbl =
      bbl |> List.rev_map (fun b ->
                [(b.bblock_begin_loc.loc_addr, BEGIN) ;
                (b.bblock_end_loc.loc_addr, END)]
               )
      |> List.rev |> List.flatten

            (* )
      |> (fun a -> List.nth a 1 |> fst |> print_int ; a)
             *)

      (*
      list.fold_left (fun b acc ->
                 [(b.bblock_begin_loc.loc_addr, BEGIN)
                 ; (b.bblock_end_loc.loc_addr, END)] @ acc
                )
                bbl []
       *)



end


(* mark instructions which are not in the trace *)
module BB_marker = struct

    open Trimmer_Pre


    let instr_mark bbl instrs t =
      let open Ail_utils in
      let mark i =
        let l = get_loc i in
        set_loc i {l with loc_visible = false}
      in
      let rec aux il flag acc al =
        match (il, al, flag) with
        | (_, [], false) -> List.rev acc
        | (ih::it, ah::at, _)  when get_addr ih = fst ah ->
           begin
             match (ah, flag) with
             | ((_,BEGIN), false) ->
                let (ah1::ah2::at') = al in
                if fst ah1 = fst ah2 then
                  (* this block has only one instruction *)
                  aux it false (ih::acc) at'
                else
                  aux it true (ih::acc) at
             | ((_,END), true) ->
                aux it false (ih::acc) at
             | _ -> failwith "unexpected situation in instr_mark 1"
           end
        | (ih::it, ah::at, false) ->
           aux it false ((mark ih)::acc) al
        | (ih::it, ah::at, true) ->
           aux it true (ih::acc) al
        | ([], ah::at, _ ) ->
           failwith "undefined behavior in instr_mark 3"
        | _ -> failwith "unexpected situation in instr_mark 2"
      in
      bb_mark_by_addr bbl t |> marked_addr_range |>
      aux instrs false []

end

(*   *)
module CF_changer = struct

    open Type

    let get_addr_in_exp e =
      let open Pp_print in
      let open Ail_utils in
      p_exp e |> recover_addr_from_label

    let check_call_des e =
      let open Pp_print in
      let module B = Batteries in
      p_exp e |> (fun es -> B.String.exists es "S_0x")

    let mark i =
      let open Ail_utils in
      let l = get_loc i in
      set_loc i {l with loc_visible = false}


    let check_label i =
        let open Ail_utils in
	let l = get_label i in
	l = "" |> not

    (* judge one
       mark the control transfers as long as the destination block is not in
       the trace
     *)
    let des_not_in_trace t jmp_t i e =
      let open Ail_utils in
      match jmp_t with
         (*  do nothing *)
      | DIRECT_CALL when check_call_des e = false -> false
         (* this is a inter-module function call for stripped binaries *)
      | DIRECT_JMP_INTER | COND_JMP_INTER -> false
         (* this is a inter-module jmp for stripped binaries *)
      | DIRECT_CALL | _ ->
         begin
           let at = Array.of_list t in
           get_addr_in_exp e
           (* this might be slow *)
           |> (fun e -> bbn_byloc e at)
           |> (fun r -> r = false)
         end


    (* judge two
      mark the control transfers when this transfer is not in the trace
     *)
    let transfer_not_in_trace bbl t jmp_t i e =
      let open Ail_utils in
      let l = get_loc i in
      if l.loc_visible = false then
        false
      else
        begin
          match jmp_t with
          (*  do nothing *)
          | DIRECT_CALL when check_call_des e = false -> false
             (* this is a intra-module function call for stripped binaries *)
          | DIRECT_JMP_INTER | COND_JMP_INTER -> false
         (* this is a inter-module jmp for stripped binaries *)
          | DIRECT_CALL | _ ->
             begin
              let open Ail_utils in
              let module IU = Instr_utils in
              let at = Array.of_list t in
              let a' = get_addr_in_exp e in
              let b = IU.get_bb_by_last_instr i bbl in
              let p = (b.bblock_begin_loc.loc_addr, a') in
		(*
	      print_endline "start checking a pair";
              print_string @@ dec_hex @@ fst p;
              print_string "\n";
              print_string @@ dec_hex @@ snd p;
              print_string "\n";
		*)
              let cmp p1 p2 =
                match (p1, p2) with
                | ((b1, e1), (b2, e2)) when b1 = b2 && e1 = e2 -> 0
                | ((b1, e1), (b2, e2)) when b1 = b2 && e1 > e2 -> 1
                | ((b1, e1), (b2, e2)) when b1 = b2 && e1 < e2 -> -1
                | ((b1, e1), (b2, e2)) when b1 < b2 -> -1
                | ((b1, e1), (b2, e2)) when b1 > b2 -> 1
                | _ -> failwith "undefined behavior in transfer_not_in_trace"
              in
              let module AL = Algo in
              (*
              let rr = AL.b_search p at cmp = false in
              if rr then
                  print_endline "true!"
              else
                  print_endline "false!";
              rr
               *)
              AL.b_search p at cmp |> not
             end
        end


    let change_cf_on_judge instrs judge =
      let open Ail_utils in
      let open Cfg_utils in
      let visitor i t =
        match t with
        | DIRECT_JMP_INTRA
        | DIRECT_JMP_INTER
        | COND_JMP_INTRA
        | COND_JMP_INTER
        | DIRECT_CALL ->
           begin
             match i with
             | DoubleInstr (p, e, _, _) when judge t i e ->
                mark i
             | DoubleInstr (p, e, _, _) -> i
             | _ -> failwith "undefined jmp instruction"
           end
        | _ -> i
      in
      map_jmp visitor instrs


    let change_cf_judge_one instrs t  : instr list =
      let module TP = Trimmer_Pre in
      change_cf_on_judge instrs (des_not_in_trace t)

    let change_cf_judge_two bbl t instrs =
      change_cf_on_judge instrs (transfer_not_in_trace bbl t)

end

module Trimmer = struct

  (* this plugin trim CFG to get a simpler CFG *)
  (* INPUT:
        the original binary
        trace contains basic block index (or name; address and so on..)
     OUTPUT:
        leaned CFG
   *)

  (*
   steps:
     1. mark bb in the trace;
     2. change unmarked bb with an "invisible" flag
        a. note that pp_printer would not print out instructions with an
   invisible flag
     3. change control transfers on unmarked basic block to a fixed position
        a. For the intra-procedure, I think it should be fine to just change all (conditional) jmp as
   long as its operand refers to a basic block.
        b. As for inter-procedure, XXX
        c. system call, XXX
     4. change control transfers between marked basic blocks to a fixed
   position as long as this connection does not appear in the trace.
        a. For the intra-procedure, I think it should be fine to just change all (conditional) jmp as
   long as its operand refers to a basic block.
        b. As for inter-procedure, XXX
        c. system call, XXX

     5. Get rid of all the control transfers to a fixed position (marked it as dummy)

     6. Can we do more? Is it a straight-line code already?
   *)


  let procedure_one bbl t instrs =
    let module BM = BB_marker in
    print_endline "procedure one\n";
    BM.instr_mark bbl instrs t

  let procedure_two t instrs =
    let module CC = CF_changer in
    print_endline "procedure two\n";
    CC.change_cf_judge_one instrs t

  let procedure_three t instrs =
    let module CC = CF_changer in
    print_endline "procedure three\n";
    CC.change_cf_judge_two instrs t

  (* this method is slow for three reasons, reading
  large file, sorting, and procedure three *)
  let trim_old bbl instrs =
    let open Ail_utils in
    let module TP = Trimmer_Pre in
    let module TR = Time_Record in
    let t1 = TR.get_utime () in
    print_endline "start obtain trace";
    let t = TP.obtain_trace() in
    let t2 = TR.elapsed t1 in
    print_endline "start sort";
    let ts = TP.addr_trace_sort_uniq t in
    print_endline "finish sort";
    let t3 = TR.elapsed t2 in
    let r = procedure_one bbl ts instrs in
    let t4 = TR.elapsed t3 in
    let r = procedure_two ts r in
    let t5 = TR.elapsed t4 in
    let r = procedure_three (TP.build_tranfers_from_trace t) bbl r in
    let _ = TR.elapsed t5 in
        r

  let trim bbl instrs =
    let open Ail_utils in
    let module TP = Trimmer_Pre in
    let module TR = Time_Record in
    let t1 = TR.get_utime () in
    print_endline "start obtain trace opt";
    let t = TP.obtain_sorted_uniq_trace() |> TP.addr_trace in
    let t1 = TR.elapsed t1 in
    let t_join = TP.obtain_sorted_uniq_join_trace() |> TP.addr_trace_joined in
    let t1 = TR.elapsed t1 in
    let r = procedure_one bbl t instrs in
    let t1 = TR.elapsed t1 in
    let r = procedure_two t r in
    let t1 = TR.elapsed t1 in
    let r = procedure_three t_join bbl r in
    let _ = TR.elapsed t1 in

    r


end
