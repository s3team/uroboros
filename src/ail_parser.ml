open Batteries

open Ail_utils

open Type
open Common_parser
open Parser
open Arm_parser
open Pp_print
open Arm_func_slicer
open Common_func_slicer
open Tag_utils

class ailParser =
object (self)
  val mutable instrs : instr list = []
  val mutable funcs : func list = []
  val mutable secs: section list = []
  val mutable arch : string = ""

  method set_funcs (l : func list) =
    funcs <- l

  method set_arch (a : string) =
    arch <- a

  method update_func_info (l : func list) : func list =
    let rec help l acc =
      match l with
      | [] -> acc
      | h::[] -> List.rev_append acc [h]
      | h1::h2::t ->
         begin
           if (h1.func_begin_addr) == (h2.func_begin_addr) then
             begin
               if String.exists (h1.func_name) "S_0x" then
                 (
			       let h2' = {h2 with func_end_addr = h1.func_end_addr} in
                   help (h2'::t) (h1::acc)
                 )
               else if String.exists (h2.func_name) "S_0x" then
                 (
			       let h1' = {h1 with func_end_addr = h2.func_end_addr} in
                   help (h2::t) (h1'::acc)
                 )
		       else
                 help (h2::t) (h1::acc)
             end
           else help (h2::t) (h1::acc)
         end
    in
    help l []

  method print_f (fl : func list) =
	List.iter (
		fun f ->
		print_string f.func_name;
		print_string " ";
		print_string (dec_hex f.func_begin_addr);
		print_string " ";
		print_string (dec_hex f.func_end_addr);
        print_string " ";
        print_bool (f.is_lib);
        print_string "\n";
      ) fl;
	fl

  method get_funcs =
    ( self#func_slicing
      |> unify_funclist_by_name
	  |> List.sort (fun f1 f2 -> f1.func_begin_addr - f2.func_begin_addr)
	  |> self#update_func_info
      |> self#filter_func
      (* |> self#print_f *)
      |> unify_funclist_by_addr
    )

  method filter_func (fl : func list) : func list =
	(self#filter_func_by_name fl
	 |> self#filter_func_by_secs)


  method filter_func_by_name (fl : func list) : func list =
	List.filter ( fun f -> (String.exists f.func_name ".text") == false) fl

  (* some of the functions are allocated in .init section, in which case we don't have to record *)
  method filter_func_by_secs (fl : func list) : func list =
	let il = read_file "text_sec.info" in
	let l = List.nth il 0 in
    let items = Str.split (Str.regexp " +") l in
    let addr = int_of_string ("0x"^(List.nth items 1))
    and size = int_of_string ("0x"^(List.nth items 3)) in
    List.filter (
		fun f ->
		let fl = String.length f.func_name in
        if fl < 3 then
          true
        else
          begin
			match int_of_string_opt (String.sub f.func_name 2 (fl-2)) with
			| None -> true
  			| Some n ->
			   n >= addr && n < (addr+size)
          end
	  ) fl


  method print_funcs funcs =
    let dec_hex (s:int) : string =
      "0x"^(Printf.sprintf "%X" s) in
    let help f =
      if f.is_lib = false then
        begin
          print_string f.func_name;
          print_string "\n";
          print_string (dec_hex f.func_begin_addr);
          print_string "\n";
          print_string (dec_hex f.func_end_addr);
          print_string "\n";
          print_bool (f.is_lib);
          print_string "\n"
        end
    in
    List.iter help funcs

  method create_func_slicer instrs funcs arch = match arch with
    | "intel" -> (new Func_slicer.func_slicer instrs funcs :> common_func_slicer)
    | "thumb" | "arm" -> (new Arm_func_slicer.arm_func_slicer instrs funcs :> common_func_slicer)
    | _ -> failwith "unsupported architecture"

  method func_slicing =
    (* self#print_f funcs; *)
    let fs = self#create_func_slicer instrs funcs arch in
    fs#update_text_info;
    fs#update_func;
    fs#get_funcs;
  (*self#print_funcs funcs *)

  method set_secs (l : section list) =
  (* let rec print_element = function
    | s::t -> print_string(s.sec_name ^ " " ^ (Printf.sprintf "%X" s.sec_begin_addr) ^ " " ^ (Printf.sprintf "%X" s.sec_size) ^ "\n"); print_element t
    | [] -> ()
  in
    print_element l; *)
    secs <- l

  method create_parser arch = match arch with
    | "intel" -> (new Parser.parse :> common_parser)
    | "thumb" | "arm" -> (new Arm_parser.arm_parse :> common_parser)
    | _ -> failwith "unsupported architecture"

  method remove_literal_pools (instr_list : instr list) =
    (* Assume that literal pools are always located in
     * between a pop and a push instruction.
     * However, we keep the instructions after the last pop instruction,
     * because they might not be literal pools.
     *)
    let ordered_il = List.rev instr_list in
    let collect_branch_targets (instrs : instr list) =
      List.fold_left (
        fun acc i ->
          match get_op i with
          | Arm_OP (Arm_ControlOP aco, _, _) -> begin
            let exp = get_exp_1 i in
            match exp with
            | Symbol (JumpDes j) ->
              j :: acc
            | _ -> acc
          end
          | _ -> acc
      ) [] instrs
    in
    let is_pop arm_stackop = match arm_stackop with
      | POP | VPOP | LDMIA -> true
      | _ -> false
    in
    let is_push arm_stackop = match arm_stackop with
      | PUSH | VPUSH | STMDB -> true
      | _ -> false
    in
    let has_r2_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "r2" then true else false
        | _ -> false
    in
    let has_r3_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "r3" then true else false
        | _ -> false
    in
    let has_r7_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "r7" then true else false
        | _ -> false
    in
    let has_lr_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "lr" then true else false
        | _ -> false
    in
    let has_pc_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "pc" then true else false
        | _ -> false
    in
    let has_fp_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "fp" then true else false
        | _ -> false
    in
    let is_exp_lr_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Reg (Arm_Reg (Arm_LinkReg LR)) -> true
        | _ -> false
    in
    let is_func_start (i : instr) (idx : int) =
      match get_op i with
      | Arm_OP (Arm_StackOP PUSH, _, _) when has_r2_reg i && has_r3_reg i -> true
      | Arm_OP (Arm_StackOP PUSH, _, _) when has_r7_reg i -> true
      | Arm_OP (Arm_StackOP PUSH, _, _) -> begin
          let next_instr = List.nth ordered_il (idx + 1) in
          let next_op = get_op next_instr in
          match next_op with
          | Arm_OP (Arm_StackOP PUSH, _, _) when has_lr_reg next_instr -> true
          | _ -> false
        end
      | Arm_OP (Arm_StackOP op, _, _) when is_push op && (has_lr_reg i || has_fp_reg i) -> true
      | _ -> false
    in
    let is_func_end (i : instr) =
      match get_op i with
      | Arm_OP (Arm_StackOP op, _, _) when is_pop op && (has_pc_reg i || has_fp_reg i) -> true
      | Arm_OP (Arm_ControlOP BX, _, _) -> begin
          let exp = get_exp_1 i in
          match exp with
          | Reg (Arm_Reg (Arm_LinkReg LR)) -> true
          | _ -> false
      end
      | _ -> false
    in
    let branch_target_addrs = ref [] in
    let newnew = ref [] in
    let instrs_after_pop = ref [] in
    let instrs_to_keep = ref [] in
    let is_going_through_literal_pool = ref false in
    let pop_detected = ref false in
    let collect_branch_target_addr (i : instr) =
      match get_op i with
      | Arm_OP (Arm_ControlOP aco, _, _) -> begin
        let exp = get_exp_1 i in
        match exp with
        | Symbol (JumpDes j) ->
          branch_target_addrs := j :: !branch_target_addrs;
        | _ -> ()
      end
      | _ -> ()
    in
    let check_if_target_in_branch_target_addrs (addr_list : int list) (i : instr) : bool =
      let loc = get_loc i in
      List.exists (fun addr -> addr = loc.loc_addr) addr_list
    in
    let process_instr (instr : instr) (index : int) : bool =
      collect_branch_target_addr instr;
      (* function start *)
      if is_func_start instr index then begin
        is_going_through_literal_pool := false;
        pop_detected := false;
        (* check if at least one of addresses in branch_target_addrs is in addr of instr in instrs_after_pop *)
        let c2c_symbol_detected = List.exists (fun lp_instr ->
          let result = check_if_target_in_branch_target_addrs !branch_target_addrs lp_instr in
          result
        ) !instrs_after_pop
        in
        if c2c_symbol_detected then begin
            newnew := !instrs_after_pop @ !newnew;
            instrs_after_pop := [];
            true
          end
        else begin
          (* check the length to find the following pattern and keep the instruction:
            e.g., make-prime-list
              107e4:	e8bd 0ff0 	ldmia.w	sp!, {r4, r5, r6, r7, r8, r9, sl, fp}
              107e8:	4770      	bx	lr ; want to keep
              107ea:	e92d 43b0 	stmdb	sp!, {r4, r5, r7, r8, r9, lr}
           *)
          if List.length !instrs_after_pop = 1 then begin
            newnew := !instrs_after_pop @ !newnew;
            instrs_after_pop := [];
          end
          else begin
          instrs_after_pop := [];
          end;
          false
        end
      end
      (* function end *)
      else if is_func_end instr then begin
        pop_detected := true;
        false
      end
    else false
    in
    let rec aux acc index = function
      | [] -> begin
          let result = !instrs_after_pop @ !newnew in
          result
        end
      | instr :: t ->
        begin
          if !pop_detected = true then
            is_going_through_literal_pool := true;
          let should_keep = process_instr instr index in

          if !is_going_through_literal_pool = true then begin
              instrs_after_pop := instr :: !instrs_after_pop;
              aux acc (index + 1) t
            end
          else begin
              newnew := instr :: !newnew;
              aux (instr :: acc) (index + 1) t
            end
        end
    in
    aux [] 0 ordered_il

  method remove_literal_pools_v2 (instr_list : instr list) : instr list =
    let ordered_il = List.rev instr_list in
    let collect_branch_targets (instrs : instr list) =
      List.fold_left (
        fun acc i ->
          match get_op i with
          | Arm_OP (Arm_ControlOP aco, _, _) -> begin
            let exp = get_exp_1 i in
            match exp with
            | Symbol (JumpDes j) ->
              j :: acc
            | _ -> acc
          end
          | _ -> acc
      ) [] instrs
    in
    let is_pop arm_stackop = match arm_stackop with
      | POP | VPOP | LDMIA -> true
      | _ -> false
    in
    let is_push arm_stackop = match arm_stackop with
      | PUSH | VPUSH | STMDB -> true
      | _ -> false
    in
    let has_r2_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "r2" then true else false
        | _ -> false
    in
    let has_r3_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "r3" then true else false
        | _ -> false
    in
    let has_r7_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "r7" then true else false
        | _ -> false
    in
    let has_lr_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "lr" then true else false
        | _ -> false
    in
    let has_pc_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "pc" then true else false
        | _ -> false
    in
    let has_fp_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Label l -> if contains l "fp" then true else false
        | _ -> false
    in
    let is_exp_lr_reg (i : instr) =
      let exp = get_exp_1 i in
      match exp with
        | Reg (Arm_Reg (Arm_LinkReg LR)) -> true
        | _ -> false
    in
    let is_func_start (i : instr) (idx : int) =
      match get_op i with
      | Arm_OP (Arm_StackOP PUSH, _, _) when has_r2_reg i && has_r3_reg i -> true
      | Arm_OP (Arm_StackOP PUSH, _, _) when has_r7_reg i -> true
      | Arm_OP (Arm_StackOP PUSH, _, _) -> begin
          let next_instr = List.nth ordered_il (idx + 1) in
          let next_op = get_op next_instr in
          match next_op with
          | Arm_OP (Arm_StackOP PUSH, _, _) when has_lr_reg next_instr -> true
          | _ -> false
        end
      | Arm_OP (Arm_StackOP op, _, _) when is_push op && (has_lr_reg i || has_fp_reg i) -> true
      | _ -> false
    in
    let is_func_end (i : instr) =
      match get_op i with
      | Arm_OP (Arm_StackOP op, _, _) when is_pop op && (has_pc_reg i || has_fp_reg i) -> true
      | Arm_OP (Arm_ControlOP BX, _, _) -> begin
          let exp = get_exp_1 i in
          match exp with
          | Reg (Arm_Reg (Arm_LinkReg LR)) -> true
          | _ -> false
      end
      | _ -> false
    in
    let check_if_common_reg (e : exp) : bool =
      match e with
      | Reg (Arm_Reg (Arm_CommonReg _)) -> true
      | _ -> false
    in
    let is_movs (op : op) : bool = match op with
      | Arm_OP (Arm_CommonOP Arm_Assign MOVS, _, _) -> true
      | _ -> false
    in
    let is_nop (op : op) : bool = match op with
      | Arm_OP (Arm_CommonOP Arm_Other NOP, _, _) -> true
      | _ -> false
    in
    let branch_target_addrs = ref [] in
    let rec aux acc is_literal_pool nop_detected idx = function
      | [] -> acc
      | instr :: t when is_func_end instr -> begin
          let n_instr = List.nth ordered_il (idx + 1) in
          let nop_detected = is_nop (get_op n_instr) in
          let nn_offset = if nop_detected then 3 else 2 in
          let nn_instr = List.nth ordered_il (idx + nn_offset) in
          match get_op nn_instr with
          | Arm_OP (Arm_CommonOP Arm_Assign MOVS, _, _)
            when (check_if_common_reg (get_exp_1 nn_instr)
                  && check_if_common_reg (get_exp_2 nn_instr)) -> begin
              (* keep this instruction *)
              aux (instr :: acc) true nop_detected (idx + 1) t
            end
          | _ -> begin
              (* keep this instruction *)
              aux (instr :: acc) false false (idx + 1) t
            end
        end
      | instr :: t when is_func_start instr idx -> begin
          (* keep this instruction *)
          aux (instr :: acc) false false (idx + 1) t
        end
      | instr :: t when is_literal_pool -> begin
          (* skip this instruction *)
          aux acc true false (idx + 1) t
        end
      | instr :: t when not is_literal_pool -> begin
          (* keep this instruction *)
          aux (instr :: acc) false false (idx + 1) t
        end
      | _ -> failwith "should not reach here"
    in
    aux [] false false 0 ordered_il

  (** For branch instructions that have width specifier,
      adjust it depending on the distance to target address. *)
  method adjust_width_specifier (instrs : instr list) : instr list =
    let change_width_specifier (i : instr) (new_widthsuff : arm_widthsuff option) : instr =
      match i with
      | SingleInstr (Arm_OP (op, condsuff, widthsuff), loc, pre, tag) ->
        SingleInstr (Arm_OP (op, condsuff, new_widthsuff), loc, pre, tag)
      | DoubleInstr (Arm_OP (op, condsuff, widthsuff), exp, loc, pre, tag) ->
        DoubleInstr (Arm_OP (op, condsuff, new_widthsuff), exp, loc, pre, tag)
      | TripleInstr (Arm_OP (op, condsuff, widthsuff), exp, exp2, loc, pre, tag) ->
        TripleInstr (Arm_OP (op, condsuff, new_widthsuff), exp, exp2, loc, pre, tag)
      | FourInstr (Arm_OP (op, condsuff, widthsuff), exp, exp2, exp3, loc, pre, tag) ->
        FourInstr (Arm_OP (op, condsuff, new_widthsuff), exp, exp2, exp3, loc, pre, tag)
      | FifInstr (Arm_OP (op, condsuff, widthsuff), exp, exp2, exp3, exp4, loc, pre, tag) ->
        FifInstr (Arm_OP (op, condsuff, new_widthsuff), exp, exp2, exp3, exp4, loc, pre, tag)
      | _ -> failwith "Unsupported instruction type"
    in
    let distance_limit = 128 in
    List.map (
      fun i ->
        match get_op i with
        (* | Arm_OP (Arm_ControlOP aco, Some condsuff, Some (Arm_Opqualifier N)) ->
          let target_addr = match get_exp_1 i with
            | Symbol (JumpDes j) -> j
            | _ -> 0
          in
          let instr_addr = (get_loc i).loc_addr in
          if target_addr < instr_addr then begin
            let addr_diff = instr_addr - target_addr in
            if addr_diff > distance_limit then
              change_width_specifier i (Some (Arm_Opqualifier W))
            else
              i
          end else i *)
        | Arm_OP (Arm_ControlOP B, Some condsuff, Some (Arm_Opqualifier N)) ->
          change_width_specifier i None
        | _ -> i
    ) instrs

  method remove_literal_pools_after_blx (instr_list : instr list) : instr list =
    (* Find blx and movs opcodes patterns like below and remove the literal pool instructions
     * that follow the blx instruction:
      blx abort
      cbnz r4,S_0x114C2
      movs r1,r0
      lsls r0,r3,#0x5
      movs r0,r0

      Note that there could be a nop instruction after the the blx instruction.
    *)
    let ordered_il = List.rev instr_list in
    let check_if_common_reg (e : exp) : bool =
      match e with
      | Reg (Arm_Reg (Arm_CommonReg _)) -> true
      | _ -> false
    in
    let is_movs (op : op) : bool = match op with
      | Arm_OP (Arm_CommonOP Arm_Assign MOVS, _, _) -> true
      | _ -> false
    in
    let is_nop (op : op) : bool = match op with
      | Arm_OP (Arm_CommonOP Arm_Other NOP, _, _) -> true
      | _ -> false
    in
    let rec aux acc is_literal_pool is_nop_after_blx idx = function
    | [] -> acc
    | instr :: t -> begin
        match get_op instr with
        | Arm_OP (Arm_ControlOP BLX, _, _)
            when not is_literal_pool && (idx + 3) < List.length ordered_il -> begin
          (* check if the next instruction is nop or not *)
          let n_instr = List.nth ordered_il (idx + 1) in
          let nop_detected = is_nop (get_op n_instr) in
          let nn_offset = if nop_detected then 3 else 2 in
          let nn_instr = List.nth ordered_il (idx + nn_offset) in
          match get_op nn_instr with
          | Arm_OP (Arm_CommonOP Arm_Assign MOVS, _, _)
            when (check_if_common_reg (get_exp_1 nn_instr)
                  && check_if_common_reg (get_exp_2 nn_instr)) -> begin
              (* keep the blx instruction *)
              aux (instr :: acc) true nop_detected (idx + 1) t
            end
            (* keep this instruction *)
          | _ -> aux (instr :: acc) false false (idx + 1) t
        end
        | Arm_OP (Arm_CommonOP Arm_Other NOP, _, _) when is_nop_after_blx -> begin
          (* skip nop instruction located right after blx *)
          aux acc true false (idx + 1) t
        end
        | _ -> begin
          if is_literal_pool then begin
            let n_instr = List.nth ordered_il (idx + 1) in
            let cur_op = get_op instr in
            let next_op = get_op n_instr in
            if is_movs cur_op || is_movs next_op then begin
              (* skip this instruction *)
              aux acc true false (idx + 1) t
            end
            else begin
              (* stop skipping instructions *)
              aux (instr :: acc) false false (idx + 1) t
            end
          end
          else begin
            aux (instr :: acc) false false (idx + 1) t
          end
        end
      end
    in
    aux [] false false 0 ordered_il

  method remove_literal_pools_after_nop (instr_list : instr list) : instr list =
    (* 12f0c:	f3af 8000 	nop.w
       ...
       12f18:	0001      	movs	r1, r0
       12f1a:	0000      	movs	r0, r0
       12f1c:	0000      	movs	r0, r0
       12f1e:	0000      	movs	r0, r0
       12f20:	7698      	strb	r0, [r3, #26]
       12f22:	0001      	movs	r1, r0
      *)
    let ordered_il = List.rev instr_list in
    let check_if_common_reg (e : exp) : bool =
      match e with
      | Reg (Arm_Reg (Arm_CommonReg _)) -> true
      | _ -> false
    in
    let is_movs (op : op) : bool = match op with
      | Arm_OP (Arm_CommonOP Arm_Assign MOVS, _, _) -> true
      | _ -> false
    in
    let rec aux acc is_literal_pool idx = function
    | [] -> acc
    | instr :: t -> begin
      match get_op instr with
      | Arm_OP (Arm_CommonOP (Arm_Other NOP), _, _) when (idx + 2) < List.length ordered_il -> begin
        let nn_instr = List.nth ordered_il (idx + 2) in
        match get_op nn_instr with
        | Arm_OP (Arm_CommonOP Arm_Assign MOVS, _, _)
          when (check_if_common_reg (get_exp_1 nn_instr)
                && check_if_common_reg (get_exp_2 nn_instr)) -> begin
            (* skip this instruction *)
            aux acc true (idx + 1) t
          end
        | _ -> aux (instr :: acc) false (idx + 1) t
        end
      | _ -> begin
        if is_literal_pool then begin
          let n_instr = List.nth ordered_il (idx + 1) in
          let cur_op = get_op instr in
          let next_op = get_op n_instr in
          if is_movs cur_op || is_movs next_op then begin
            aux acc true (idx + 1) t
          end
          else begin
            aux (instr :: acc) false (idx + 1) t
          end
        end
        else begin
          aux (instr :: acc) false (idx + 1) t
        end
      end
    end
    in
    aux [] false 0 ordered_il

  method remove_illegal_instructions (instr_list : instr list) (arch : string) : instr list =
    let ordered_il = List.rev instr_list in
    match arch with
    | "thumb" -> begin
      let check_illegal (i : instr) : bool =
        match get_op i with
        | Arm_OP (Arm_SystemOP PLDW, _, _) -> true
        | _ -> false
      in
      let rec help acc = function
        | [] -> acc
        | h :: t -> begin
            if check_illegal h then
              help acc t
            else
              help (h :: acc) t
          end
      in
      help [] ordered_il
    end
    | _ -> instr_list


  method process_instrs (l : string list) (arch : string) =
    let cat_tail s =
      match s with
      | [] -> "" (* this will never happen*)
      | h::t -> String.trim (String.concat ":" t) in
    (* make a branch here to split Intel and ARM parsers using inheritance *)
    let p = self#create_parser arch in
    let _ = p#set_funclist(funcs)
    and _ = p#set_seclist(secs)
    and split = Str.split (Str.regexp_string ":") in
    let l' = List.filter (
                 fun i ->
                 let items = split i in
                 let len = List.length items in
                 len > 1 ) l in
    let is_illegal_instr (instr : string) =
      (* If there is an "illegal" instruction in the Thumb objdump result,
       * the instruction is a literal pool in most cases.
       * Therefore, we can skip the instruction.
       * See [text_as_data] in [arm_reassemble_symbol_get.ml#v_exp2] *)
      let illegal_instrs = [
        "illegal"; "??";"cdp"; "cdp2"; "mrc"; "mrc2"; "ldc2l"; "stc"; "stc2l";
        "ltc2l"; "vst1"; "ldc"; "ldcl"; "mrrc"; "mcr2"; "mcrr"; "mcr";
        "vld4"; "<und>"]
      in
      let rec has_illegal_instr instr' = function
        | [] -> false
        | illegal_instr :: t ->
          if contains instr' illegal_instr then true
          else has_illegal_instr instr' t
      in
      has_illegal_instr instr illegal_instrs
    in
    let help i =
      let items = split i in
      let loc = List.nth items 0 in
      let instr = cat_tail items in
      if is_illegal_instr instr then
        ()
      else
        instrs <- (p#parse_instr instr loc arch)::instrs;
    in
    List.iter help l';
    if arch = "arm" || arch = "thumb" then begin
      instrs <- self#remove_literal_pools instrs;
      (* instrs <- self#remove_literal_pools_v2 instrs; *)
      instrs <- self#remove_literal_pools_after_blx instrs;
      instrs <- self#remove_literal_pools_after_nop instrs;
      instrs <- self#remove_illegal_instructions instrs arch;
      instrs <- self#adjust_width_specifier instrs;

    end;

    (* giyeol: for debugging *)
    (* List.iter (fun i -> Printf.printf "giyeol: reversed?: %s\n" (pp_print_instr' i)) (List.rev instrs); *)
    (* let is_branch_instr (i : instr) =
      match get_op i with
      | Arm_OP (Arm_ControlOP B, _, _)
      | Arm_OP (Arm_ControlOP BL, _, _)
      | Arm_OP (Arm_ControlOP BX, _, _)
      | Arm_OP (Arm_ControlOP BLX, _, _)
        -> true
      | _ -> false
    in
    List.iter (fun i -> match get_op i with
      | Arm_OP (Arm_ControlOP aco, Some condsuff, Some (Arm_Opqualifier N))
        when is_branch_instr i ->
          let target_addr = match get_exp_1 i with
            | Symbol (JumpDes j) -> j
            | _ -> 0
          in
          let instr_addr = (get_loc i).loc_addr in
          if target_addr < instr_addr then begin
              let addr_diff = instr_addr - target_addr in
              (* Printf.printf "branch instr with condsuff: %s\n" (pp_print_instr' i); *)
              Printf.printf "\tdiff: %d(0x%x)\n" addr_diff addr_diff;
            end
      | _ -> ()
    ) (List.rev instrs); *)
    (* giyeol: for debugging end *)
    funcs <- p#get_funclist;

  method p_instrs =
    List.iter (fun i -> let is = pp_print_instr' i in print_endline is) instrs

  method get_instrs =
    List.rev instrs

  method get_instrs_len =
    List.length instrs

end
