open Batteries

open Visit
open Type
open Ail_utils


class stack_of_loc =
    object (self)
      val mutable the_list : loc list = []
      method push x =
        the_list <- x :: the_list
      method pop =
        let result = List.hd the_list in
        the_list <- List.tl the_list;
        result
      method peek =
        List.hd the_list
      method size =
        List.length the_list
end


class dis_validator =
(*
 *  Disassembly validator:
 *
 *  three situations would be considered as disassembly errors:
 *        Invalid opcode
 *        Direct control transfers outside the current module
 *        Direct control transfer to the middle of an instruction
 *
 *  Recover solution:
 *    we have to identify the data gap:
 *      1. beginning addr:   to the closest preceding unconditional control-flow transfer
 *      1. ending addr:   to the closest indirect control flow transfer target
 **)

    let icf_stack = new stack_of_loc in

    object(self)

    val mutable looking_for_cfd : bool = false

    val mutable text_secs: (int*int) list = []
    val mutable text_mem_addrs: int list = []
    val mutable text_mem_arr = Array.create 1 0
    val mutable locs: int list = []
    val mutable up_bound : loc = {loc_label=""; loc_addr = 0; loc_visible=true;}
    val mutable low_bound : loc = {loc_label=""; loc_addr = 0; loc_visible=true;}

    val mutable trim_tbl = Hashtbl.create 8


    method text_sec_collect =
      let filelines = File.lines_of "text_sec.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x"^(List.nth items 1))
        and size = int_of_string ("0x"^(List.nth items 3)) in
          text_secs <- (addr, size)::text_secs
        in
        Enum.iter help filelines;

        let module EU = ELF_utils in

        if EU.elf_32 () then
          begin
            text_mem_addrs <- List.map (fun a -> let a1 = String.trim a in
                                             int_of_string ("0x"^(String.sub a1 0 7))) (read_file "text_mem.info");
            text_mem_arr <- Array.of_list text_mem_addrs;
          end
        else
          begin
            text_mem_addrs <- List.map (fun a ->
                                        let
                                          a1 = String.trim a
                                        in
                                        int_of_string ("0x"^(String.sub a1 0 6))) (read_file "text_mem.info");
            text_mem_arr <- Array.of_list text_mem_addrs;
          end

(*
    let filelines = File.lines_of "init_sec.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x"^(List.nth items 1))
        and size = int_of_string ("0x"^(List.nth items 3)) in
          text_secs <- (addr, size)::text_secs
        in
        Enum.iter help filelines
*)

    method invalid_opcode (i : instr) : bool =
      match i with
      | SingleInstr (p, _, _) ->
        begin
          match p with
          | ErrorOP _ -> true
          | _ -> false
        end
      | _ -> false


    method invalid_transfer (i : instr) : bool =
      let is_cp p =
        match p with
        | ControlOP p' ->
            begin
              match p' with
              | Jump _-> true
              | CALL -> true
              | _ -> false
            end
        | _ -> false
      and is_des e =
        match e with
         | Symbol s ->
            begin
            match s with
             | JumpDes d -> Some d
             | CallDes f ->
              (
                if f.is_lib == false then
                  (* S_0x8040240 *)
                  try
                    let f' = String.sub f.func_name 2 9 in
                      Some (int_of_string f')
                  with
                  | _ -> None
                else None
              )
             | _ -> None
            end
          | _ -> None
      and is_outside (d : int) : bool =
        let help d (b,s) =
          if d < b || b >= (b+s) then
            true
          else false in
        List.exists (help d) text_secs in
      let is_inside addr =
	      bbn_byloc addr text_mem_arr in
      match i with
      | DoubleInstr (p, e1, _, _) ->
        if is_cp p == true then
          match is_des e1 with
          | Some d -> is_outside d || is_inside d
          | None -> false
        else false
      | _ -> false


      method visit (il : instr list) : instr list =
        let help acc i =
          if self#invalid_opcode i || self#invalid_transfer i then
            let lo = get_loc i in
              (lo.loc_addr)::acc
          else acc in
        locs <- List.fold_left help [] il;
        if List.length locs != 0 then
          (
            (*print_addrlist locs; *)
             self#validate il;
             (*self#trim_results; *)
             il
          )
        else
          il


      method trim_results : (int*int) list =
        let ll = Hashtbl.fold (fun b e acc -> (b,e)::acc) trim_tbl [] in
        ll;


      method validate (il : instr list) =
        let instr_array = Array.of_list il in
        let update_trimtbl b e =
          if Hashtbl.mem trim_tbl b then
            (
            let e' = Hashtbl.find trim_tbl b in
              if e > e' then
                Hashtbl.replace trim_tbl b e
              else ()
            )
          else
            Hashtbl.replace trim_tbl b e in
        let is_icf p e =
          let r = match e with
          | Some _ -> true
          | None -> false in
          if r == false then false
          else
            begin
              let e' = match e with
              | Some s -> s
              | _ -> failwith "error in is_icf" in
              let is_cp p =
                match p with
                | ControlOP p' ->
                    begin
                      match p' with
                        | Jump _-> true
                        | CALL -> true
                        | _ -> false
                    end
                | _ -> false
              and is_des e =
                match e with
                 | Symbol s ->
                    begin
                      match s with
                       | StarDes _ -> true
                       | _ -> false
                    end
                  | _ -> false in
                (is_cp p)&&(is_des e')
            end
        and is_call p =
          match p with
            | ControlOP p' ->
                begin
                  match p' with
                    | CALL -> true
                    | _ -> false
                end
            | _ -> false
        and is_icd i =
          let l = get_loc i in
            if String.length (l.loc_label) > 1 then true
            else false
        and is_err i =
          let il = get_loc i in
            List.mem (il.loc_addr) locs in
        (* this algorithm leverage a stack to store the unconditional control flow transfer
         * and use two "pointer" to keep track of the control flow transfer destination *)
        let five_q = Queue.create () in
        let update_cft_track i =
          Queue.add i five_q;
          if Queue.length five_q == 6 then
            ignore(Queue.take five_q)  in
        let invalid_instr i =
          let il = get_loc i in
            List.mem (il.loc_addr) locs in
        let update_cft_stack (i : instr) : unit =
          let five_l = Queue.fold (fun acc q -> q::acc) [] five_q in
            if List.exists invalid_instr five_l == false then
              icf_stack#push (get_loc i)
            else
              () in
        let update_cfd i =
          if looking_for_cfd == true then
            let tl : instr list = [instr_array.(i); instr_array.(i+1); instr_array.(i+2); instr_array.(i+3); instr_array.(i+4)] in
              if List.exists invalid_instr tl == false then
                (
                  looking_for_cfd <- false;
                  let il = instr_array.(i) in
                    let ll = get_loc il in
                    update_trimtbl up_bound.loc_addr ll.loc_addr
                )
          else
            () in
        let help index i =
          if is_err i then (* error decode locating *)
            (
              up_bound <- icf_stack#pop;
              looking_for_cfd <- true;
              ()
            )
          else
            begin
                if is_icd i then  (* found indirect control flow destination, match last error*)
                  (
                    update_cfd index;
                    update_cft_track i;
                  )
                else
                  begin
                    let p = get_op i
                    and e = get_cf_des i in
                    if is_call p then
                      (*this is wired, I don't believe
        situations like this could happen*)
                    (
                      print_endline "detect call instruction in disassembly validator";
                      update_cfd (index + 1);
                      if is_icf p e then
                      (
                          update_cft_stack i;
                          update_cft_track i;

                      )
                      else
                       update_cft_track i
                    )
                    else if is_icf p e then
                    (
                      update_cft_stack i;
                      update_cft_track i
                    )
                    else
                      update_cft_track i
                  end
            end in
          Array.iteri help instr_array

    end
