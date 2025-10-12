open Batteries

open Ail_utils
open Arm_utils

open Type
open Common_parser
open Parser
open Arm_parser
open Pp_print
open Arm_func_slicer
open Common_func_slicer
open Tag_utils

class ailParser =
let addr_hexcode_map : (int, string) Hashtbl.t = Hashtbl.create 200 in

let get_hexcode (instr : instr) : string=
  let loc = get_loc instr in
  try
    Hashtbl.find addr_hexcode_map loc.loc_addr
  with Not_found -> ""
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
  | Arm_OP (Arm_CommonOP (Arm_Other NOP), _, _) -> true
  | _ -> false
in

let is_control_op (op : op) : bool = match op with
  | Arm_OP (Arm_ControlOP _, _, _) -> true
  | _ -> false
in

let is_movs_r0_r0 (i : instr) : bool = match i with
  | TripleInstr (op, exp1, exp2, _, _, _)
    when is_movs op
         && exp1 = Reg (Arm_Reg (Arm_CommonReg R0))
         && exp2 = Reg (Arm_Reg (Arm_CommonReg R0)) -> true
  | _ -> false
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

let is_movs (op : op) : bool = match op with
  | Arm_OP (Arm_CommonOP Arm_Assign MOVS, _, _) -> true
  | _ -> false
in

let is_simple_branch (op : op) : bool = match op with
  | Arm_OP (Arm_ControlOP B, _, _) -> true
  | _ -> false
in

let check_if_common_reg (e : exp) : bool =
  match e with
  | Reg (Arm_Reg (Arm_CommonReg _)) -> true
  | _ -> false
in

let is_func_start (instrs : instr list) (i : instr) (idx : int) =
  let not_func_start () =
    let next_instr = List.nth instrs (idx + 1) in
    let next_op = get_op next_instr in
    match get_op i with
    | Arm_OP (Arm_StackOP PUSH, _, _)
      when idx + 1 < List.length instrs
           && is_movs next_op
           && check_if_common_reg (get_exp_1 next_instr)
           && check_if_common_reg (get_exp_2 next_instr) -> begin
        (* Not to classify the following pattern as func start:
         * In Coreutils ginstall,
         * 0x145D6: pop {r4,r5,r7,pc} / pop: true
         * 0x145D8: push {r1,r3,r5,r6,r7} / pop: false
         * 0x145DA: movs r2,r0 / pop: false
         * 0x145DC: push {r2,r5,r6,r7} / pop: false
         * 0x145DE: movs r2,r0 / pop: false
         *)
        true
      end
    | _ -> false
  in
  if idx + 1 < List.length instrs && not_func_start () then false
  else begin
    match i with
    | TripleInstr
        (Arm_OP (Arm_CommonOP (Arm_Arithm SUB), _, _), _, exp2, _, _, _)
      when exp2 = Reg (Arm_Reg (Arm_StackReg SP)) -> begin
        (* In Coreutils stat,
         * 14004:	b082      	sub	sp, #8 ;; func start
         * 14006:	b490      	push	{r4, r7}
         *)
        let next_instr = List.nth instrs (idx + 1) in
        let next_op = get_op next_instr in
        match next_op with
        | Arm_OP (Arm_StackOP op, _, _) when is_push op -> true
        | _ -> false
      end
    | _ -> begin
        match get_op i with
        | Arm_OP (Arm_StackOP PUSH, _, _) when has_r2_reg i && has_r3_reg i ->
            true
        | Arm_OP (Arm_StackOP PUSH, _, _) when has_r7_reg i -> true
        | Arm_OP (Arm_StackOP PUSH, _, _) -> begin
            let next_instr = List.nth instrs (idx + 1) in
            let next_op = get_op next_instr in
            match next_op with
            | Arm_OP (Arm_StackOP PUSH, _, _) when has_lr_reg next_instr -> true
            | _ -> false
          end
        | Arm_OP (Arm_StackOP op, _, _)
          when is_push op && (has_lr_reg i || has_fp_reg i) ->
            true
        | _ -> false
      end
  end
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

  method remove_literal_pools_v1 (instr_list : instr list) =
    (* Assume that literal pools are always located in
     * between a pop and a push instruction.
     * However, we keep the instructions after the last pop instruction,
     * because they might not be literal pools.
     *)
    let ordered_il = List.rev instr_list in
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
      (* function start *)
      if is_func_start ordered_il instr index then begin
        is_going_through_literal_pool := false;
        pop_detected := false;
        (* check if at least one of addresses in branch_target_addrs is in addr of instr in instrs_after_pop *)
        let c2c_symbol_detected = List.exists (fun lp_instr ->
          let result = check_if_target_in_branch_target_addrs !branch_target_addrs lp_instr in
          result
        ) !instrs_after_pop
        in
        if c2c_symbol_detected then begin
            (* keep instructions *)
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
            let ii =  List.hd !instrs_after_pop in
            match get_op ii with
            | Arm_OP (Arm_ControlOP BX, _, _) ->
              (* keep the branch instruction *)
              newnew := !instrs_after_pop @ !newnew;
              instrs_after_pop := [];
            | _ -> instrs_after_pop := [];
          end
          else begin
            (* remove instructions *)
            instrs_after_pop := [];
          end;
          false
        end
      end
      else if is_func_end instr then begin
        (* function end *)
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
              collect_branch_target_addr instr;
              newnew := instr :: !newnew;
              aux (instr :: acc) (index + 1) t
            end
        end
    in
    aux [] 0 ordered_il

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
    (* let distance_limit = 128 in *)
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
        | Arm_OP (Arm_ControlOP B, _, Some (Arm_Opqualifier N)) ->
          change_width_specifier i None
        | _ -> i
    ) instrs

  method remove_literal_pools_with_movs (instr_list : instr list) : instr list =
    (* Find nop/blx and movs opcodes patterns like below and remove the literal pool instructions
      * that follow the blx instruction:
        blx abort
        cbnz r4,S_0x114C2
        movs r1,r0
        lsls r0,r3,#0x5
        movs r0,r0

        Note that there could be a nop instruction after the the blx instruction.

        12f0c:	f3af 8000 	nop.w
        ...
        12f18:	0001      	movs	r1, r0
        12f1a:	0000      	movs	r0, r0
        12f1c:	0000      	movs	r0, r0
        12f1e:	0000      	movs	r0, r0
        12f20:	7698      	strb	r0, [r3, #26]
        12f22:	0001      	movs	r1, r0

        There could be undefined instructions in literal pools:
        e.g., In Coreutils mv,
        1a332:	e36e      	b.n	1aa12 <putc_unlocked@plt+0x882e>
        1a334:	49a6      	ldr	r1, [pc, #664]	; (1a5d0 <putc_unlocked@plt+0x83ec>)
        1a336:	0001      	movs	r1, r0
        1a338:	48c8      	ldr	r0, [pc, #800]	; (1a65c <putc_unlocked@plt+0x8478>)
        1a33a:	0001      	movs	r1, r0
        1a33c:	44b4      	add	ip, r6
        1a33e:	0001      	movs	r1, r0
        1a340:	47f2      			; <UNDEFINED> instruction: 0x47f2
        1a342:	0001      	movs	r1, r0
        1a344:	476c      	bxns	sp
        1a346:	0001      	movs	r1, r0
        See Also: arm_parser.ml#push_stack
      *)
    let ordered_il = List.rev instr_list in
    let rec aux acc is_literal_pool is_nop_after_blx is_second_movs idx = function
      | [] -> acc
      | instr :: t -> begin
          match get_op instr with
          (* | Arm_OP (Arm_CommonOP (Arm_Other NOP), _, _)
            when idx + 2 < List.length ordered_il -> begin
              let nn_instr = List.nth ordered_il (idx + 2) in
              match get_op nn_instr with
              | Arm_OP (Arm_CommonOP (Arm_Assign MOVS), _, _)
                when check_if_common_reg (get_exp_1 nn_instr)
                    && check_if_common_reg (get_exp_2 nn_instr) -> begin
                  (* skip this instruction *)
                  aux acc true false false (idx + 1) t
                end
              | _ -> aux (instr :: acc) false false false (idx + 1) t
            end *)
          | Arm_OP (Arm_ControlOP B, _, Some (Arm_Opqualifier W))
          | Arm_OP (Arm_ControlOP B, _, Some (Arm_Opqualifier N))
            when (not is_literal_pool) && idx + 3 < List.length ordered_il ->
            begin
              (* check if the next instruction is nop or not *)
              let n_instr = List.nth ordered_il (idx + 1) in
              let nop_detected = is_nop (get_op n_instr) in
              let nn_offset = if nop_detected then 3 else 2 in
              let nn_instr = List.nth ordered_il (idx + nn_offset) in
              match get_op nn_instr with
              | Arm_OP (Arm_CommonOP (Arm_Assign MOVS), _, _)
                when check_if_common_reg (get_exp_1 nn_instr)
                    && check_if_common_reg (get_exp_2 nn_instr) -> begin
                  (* keep the blx instruction *)
                  aux (instr :: acc) true nop_detected false (idx + 1) t
                end
              | _ ->
                  (* keep this instruction *)
                  aux (instr :: acc) false false false (idx + 1) t
            end
          | Arm_OP (Arm_CommonOP (Arm_Other NOP), _, _) when is_nop_after_blx ->
            begin
              (* keep nop instruction located right after blx
              * to prevent the "undefined reference" issue *)
              aux (instr :: acc) true false false (idx + 1) t
            end
          | _ -> begin
              let tag = get_tag instr in
              if is_literal_pool && tag = Some Pad then begin
                (* keep this padding instruction and continue *)
                aux (instr :: acc) true false false (idx + 1) t
              end
              else
                let hexcode = get_hexcode instr in
                if is_literal_pool && String.length hexcode = 9 then begin
                  let upper_half = String.sub hexcode 5 4 in
                  if
                    upper_half = "0000" || upper_half = "0001"
                    || upper_half = "0002"
                  then begin
                    (* skip this instruction *)
                    aux acc true false false (idx + 1) t
                  end
                  else begin
                    (* stop skipping instructions *)
                    aux (instr :: acc) false false false (idx + 1) t
                  end
                end
                else if is_literal_pool && idx + 1 < List.length ordered_il then begin
                  let n_instr = List.nth ordered_il (idx + 1) in
                  let cur_op = get_op instr in
                  let next_op = get_op n_instr in
                  if (not is_second_movs) && is_movs next_op then begin
                    (* skip this instruction *)
                    aux acc true false true (idx + 1) t
                  end
                  else if is_second_movs && is_movs cur_op then begin
                    (* skip this instruction *)
                    aux acc true false false (idx + 1) t
                  end
                  else begin
                    (* stop skipping instructions *)
                    aux (instr :: acc) false false false (idx + 1) t
                  end
                end
                else begin
                  (* last instruction *)
                  aux (instr :: acc) false false false (idx + 1) t
                end
            end
        end
    in
    aux [] false false false 0 ordered_il


  (** Remove instructions after branch opcodes unless they are pointed by other instructions *)
  method remove_literal_pools_after_branch (instr_list : instr list) : instr list =
    let ordered_il = List.rev instr_list in
    let branch_target_addrs = ref [] in
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
    let _ = List.iter (fun i -> collect_branch_target_addr i) instrs in
    (** [instr_buffer] is for temporarily keeping literal pools until another function begins *)
    let rec aux acc instr_buffer is_literal_pool idx = function
      | [] -> acc
      | [instr] -> instr :: instr_buffer @ acc
      | instr :: n_instr :: t' -> begin
        let t = n_instr :: t' in
        (* collect_branch_target_addr instr; *)
        if is_literal_pool then begin
            let pointed = check_if_target_in_branch_target_addrs !branch_target_addrs instr in
            (* The check for whether an instruction is "pointed" to is not perfect:
             * later instructions can point back to earlier ones (e.g., backward branches). *)
            if pointed then
              aux (instr :: acc) [] false (idx + 1) t
            else
              (* remove the instruction *)
              aux acc (instr :: instr_buffer) true (idx + 1) t
          end
        else begin
          match get_op instr with
          | Arm_OP (Arm_ControlOP B, None, _) when is_nop (get_op n_instr) ->
              (* keep the nop instruction to prevent the "undefined reference" issue *)
              aux (n_instr :: instr :: acc) [] true (idx + 2) t
          | Arm_OP (Arm_ControlOP B, None, _) ->
              aux (instr :: acc) [] true (idx + 1) t
          | _ -> aux (instr :: acc) [] false (idx + 1) t
        end
      end
    in
    aux [] [] false 0 ordered_il

  (** Remove padding patterns *)
  method remove_inline_paddings (instr_list : instr list) : instr list =
    (* e.g., In Coreutils printf,
     * 1ceb0:	0000      	movs	r0, r0
     * 1ceb2:	0000      	movs	r0, r0
     * 1ceb4:	0000      	movs	r0, r0
     * 1ceb6:	3fb0      	subs	r7, #176	; 0xb0

     * The last opcode can be lsrs, rors, subs and so on
     *)
    let ordered_il = List.rev instr_list in
    let rec aux acc idx = function
      | [] -> acc
      | instr :: n_instr :: nn_instr :: nnn_instr :: t
        when is_movs_r0_r0 instr && is_movs_r0_r0 n_instr && is_movs_r0_r0 nn_instr ->
        begin
          (* skip four instructions *)
          aux acc (idx + 4) t
        end
      | instr :: t -> aux (instr :: acc) (idx + 1) t
    in
    aux [] 0 ordered_il

  (** Remove instructions pointed to by PC-relative loads *)
  method remove_literal_pools_by_pc_relative_load (instr_list : instr list) : instr list =
    let ordered_il = List.rev instr_list in
    let pc_relative_addrs = ref [] in
    (** [instr_buffer] is for temporarily keeping literal pools until another function begins *)
    let rec aux acc instr_buffer = function
      | [] -> acc
      | instr :: [] -> instr :: instr_buffer @ acc
      | instr :: t ->
        begin
          if List.exists (fun addr -> addr = (get_loc instr).loc_addr) !pc_relative_addrs then
            (* skip this instruction *)
            (* let _ = Printf.printf "Removing instruction: %s\n" (pp_print_instr' instr) in *)
            aux acc (instr :: instr_buffer) t
          else
            begin
              match instr with
              | TripleInstr (Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _), Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg r), offset)), Reg (Arm_Reg (Arm_CommonReg _)), loc, _, _)
                when offset mod 2 = 0 ->
                begin
                  let module AU = ArmUtils in
                  let pc_addr = AU.get_pc_relative_addr "thumb" instr in
                  pc_relative_addrs := pc_addr :: !pc_relative_addrs;
                  aux (instr :: acc) [] t
                end
              | _ ->
                begin
                  aux (instr :: acc) [] t
                end
            end
        end
    in
    aux [] [] ordered_il

  (** Tag inline paddings to preserve them *)
  method tag_inline_paddings (instr_list : instr list) : instr list =
    let ordered_il = List.rev instr_list in
    let rec aux acc idx = function
      | [] -> acc
      | instr :: n_instr :: nn_instr :: nnn_instr :: t
        when is_movs_r0_r0 instr && is_movs_r0_r0 n_instr && is_movs_r0_r0 nn_instr ->
        begin
          let tagged_instr = TagUtils.replace_tag instr (Some Pad) in
          let tagged_n_instr = TagUtils.replace_tag n_instr (Some Pad) in
          let tagged_nn_instr = TagUtils.replace_tag nn_instr (Some Pad) in
          let tagged_nnn_instr = TagUtils.replace_tag nnn_instr (Some Pad) in
          aux (tagged_nnn_instr :: tagged_nn_instr :: tagged_n_instr :: tagged_instr :: acc) (idx + 4) t
        end
      | instr :: t -> aux (instr :: acc) (idx + 1) t
    in
    aux [] 0 ordered_il

  (** Tag branch islands to preserve them *)
  method tag_branch_islands (instr_list : instr list) : instr list =
    let ordered_il = List.rev instr_list in
    let rec aux acc idx = function
      | [] -> acc
      | instr :: n_instr :: nn_instr :: nnn_instr :: t
        when is_movs_r0_r0 instr && is_movs_r0_r0 n_instr && is_movs_r0_r0 nn_instr ->
        begin
          let tagged_instr = TagUtils.replace_tag instr (Some Pad) in
          let tagged_n_instr = TagUtils.replace_tag n_instr (Some Pad) in
          let tagged_nn_instr = TagUtils.replace_tag nn_instr (Some Pad) in
          let tagged_nnn_instr = TagUtils.replace_tag nnn_instr (Some Pad) in
          aux (tagged_nnn_instr :: tagged_nn_instr :: tagged_n_instr :: tagged_instr :: acc) (idx + 4) t
        end
      | instr :: t -> aux (instr :: acc) (idx + 1) t
    in
    aux [] 0 ordered_il

  method remove_illegal_instructions (instr_list : instr list) (arch : string) : instr list =
    let ordered_il = List.rev instr_list in
    match arch with
    | "thumb" -> begin
      let check_illegal (i : instr) : bool =
        match get_op i with
        | Arm_OP (Arm_SystemOP PLDW, _, _) -> true
        | Arm_OP (Arm_CommonOP (Arm_Arithm VHADD), _, _) -> true
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

  method remove_undefined_instructions (instr_list : instr list) : instr list =
    List.filter (fun i ->
      match get_op i with
      | Undefined_OP -> false
      | _ -> true
    ) instr_list

  method remove_literal_pools (instr_list : instr list) =
    (* Function call order matters *)
    (* instrs <- self#remove_inline_paddings instrs; *)
    instrs <- self#tag_inline_paddings instrs;
    instrs <- self#tag_branch_islands instrs;
    instrs <- self#remove_literal_pools_v1 instrs;
    instrs <- self#remove_literal_pools_with_movs instrs;
    instrs <- self#remove_literal_pools_after_branch instrs;
    instrs <- self#remove_literal_pools_by_pc_relative_load instrs;
    instrs <- self#remove_illegal_instructions instrs arch;
    instrs <- self#remove_undefined_instructions instrs;

  method convert_instructions (instr_list : instr list) : instr list =
    let ordered_il = List.rev instr_list in
    let rec aux acc idx = function
      | [] -> acc
      | instr :: t -> begin
          match instr with
          | DoubleInstr (Arm_OP (Arm_StackOP PUSH, Some _, width), _, _, _, _)
            ->
              let new_instr =
                change_op instr (Arm_OP (Arm_StackOP PUSH, None, width))
              in
              aux (new_instr :: acc) (idx + 1) t
          | DoubleInstr (Arm_OP (Arm_StackOP STMDB, Some LE, width), _, _, _, _)
            ->
              let new_instr =
                change_op instr (Arm_OP (Arm_StackOP STMDB, None, width))
              in
              aux (new_instr :: acc) (idx + 1) t
          | DoubleInstr
              (Arm_OP (Arm_StackOP PUSH, Some LT, width), Label label, _, _, _)
            when contains label "lr" ->
              let new_instr =
                change_op instr (Arm_OP (Arm_StackOP PUSH, None, width))
              in
              aux (new_instr :: acc) (idx + 1) t
          | DoubleInstr (Arm_OP (Arm_ControlOP BL, Some PL, None), _, _, _, _)
            ->
              let new_instr =
                change_op instr (Arm_OP (Arm_ControlOP BL, None, None))
              in
              aux (new_instr :: acc) (idx + 1) t
          | TripleInstr
              (Arm_OP (Arm_StackOP STMDB, Some LE, width), _, _, _, _, _) ->
              let new_instr =
                change_op instr (Arm_OP (Arm_StackOP STMDB, None, width))
              in
              aux (new_instr :: acc) (idx + 1) t
          | TripleInstr
              ( Arm_OP (Arm_CommonOP (Arm_Arithm SUB), Some _, width),
                _,
                exp2,
                _,
                _,
                _ )
            when exp2 = Reg (Arm_Reg (Arm_StackReg SP)) ->
              let new_instr =
                change_op instr
                  (Arm_OP (Arm_CommonOP (Arm_Arithm SUB), None, width))
              in
              aux (new_instr :: acc) (idx + 1) t
          | _ -> (
              match get_op instr with
              | Arm_OP (Arm_CommonOP (Arm_Assign MOV), Some UND, _) ->
                  let new_instr =
                    change_op instr
                      (Arm_OP (Arm_CommonOP (Arm_Assign MOVS), None, None))
                  in
                  aux (new_instr :: acc) (idx + 1) t
              | Arm_OP (Arm_CommonOP (Arm_Arithm SUB), Some UND, _) ->
                  let new_instr =
                    change_op instr
                      (Arm_OP (Arm_CommonOP (Arm_Arithm SUB), None, None))
                  in
                  aux (new_instr :: acc) (idx + 1) t
              | Arm_OP
                  ( Arm_CommonOP (Arm_Assign MOVS),
                    None,
                    Some (Arm_Opqualifier W) ) ->
                  let new_instr =
                    change_op instr
                      (Arm_OP (Arm_CommonOP (Arm_Assign MOVS), None, None))
                  in
                  aux (new_instr :: acc) (idx + 1) t
              | _ -> aux (instr :: acc) (idx + 1) t)
        end
    in
    aux [] 0 ordered_il

  method remove_it_sequence (instr_list : instr list) : instr list =
    let ordered_il = List.rev instr_list in
    let rec aux acc skip_count idx = function
      | [] -> acc
      | instr :: t -> begin
          match get_op instr with
          | Arm_OP (Arm_Condition it_op, _, _) ->
              let it_cond_length = match it_op with
                | IT -> 1
                | ITE -> 2
                | ITT -> 2
                | ITTT -> 3
                | ITTE -> 3
                | ITEE -> 3
                | ITET -> 3
                | ITTTT -> 4
                | ITTTE -> 4
                | ITTET -> 4
                | ITTEE -> 4
                | ITETT -> 4
                | ITETE -> 4
                | ITEET -> 4
                | ITEEE -> 4
                | _ -> failwith "unsupported IT opcode"
              in
              (* Check for the instructions within the length.
               * If there is at least one instruction that does not have condition suffix,
               * then it is likely literal pools.
               * Thus, we remove the IT instruction and the following instructions within the length.
               *)
              let has_condsuff (instr : instr) : bool =
                match get_op instr with
                | Arm_OP (_, Some _, _) -> true
                | _ -> false
              in
              let rec count_condsuff count n =
                match n with
                | 0 -> count
                | _ -> begin
                  let target_instr = List.nth ordered_il (idx + n) in
                  if has_condsuff target_instr then
                    count_condsuff (count + 1) (n - 1)
                  else
                    count_condsuff count (n - 1)
                end
              in
              let cond_count = count_condsuff 0 it_cond_length in
              if cond_count < it_cond_length then
                aux acc cond_count (idx + 1) t
              else
                aux (instr :: acc) 0 (idx + 1) t
          | _ ->
            if skip_count > 0 then
              aux acc (skip_count - 1) (idx + 1) t
            else
             aux (instr :: acc) 0 (idx + 1) t
        end
    in
    aux [] 0 0 ordered_il

  (** Currently, disabled. Check IT condition suffixes and correct them if necessary. *)
  method convert_it_condsuff (instr_list : instr list) : instr list =
    let ordered_il = List.rev instr_list in
    let rec aux acc replacement_instrs idx = function
      | [] -> acc
      | instr :: t -> begin
          match replacement_instrs with
          | r_instr :: r_t -> aux (r_instr :: acc) r_t (idx + 1) t
          | [] -> begin
              match get_op instr with
              | Arm_OP (Arm_Condition it_op, Some cond, width) as op
                when idx + 4 < List.length ordered_il ->
                  let get_it_cond_arr (it_instr : instr) : bool list =
                    match get_op it_instr with
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = IT ->
                        [ true ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITE ->
                        [ false; true ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITT ->
                        [ true; true ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITTT ->
                        [ true; true; true ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITTE ->
                        [ true; true; false ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITEE ->
                        [ true; false; false ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITET ->
                        [ true; false; true ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITTTT ->
                        [ true; true; true; true ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITTTE ->
                        [ true; true; true; false ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITTET ->
                        [ true; true; false; true ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITTEE ->
                        [ true; true; false; false ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITETT ->
                        [ true; false; true; true ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITETE ->
                        [ true; false; true; false ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITEET ->
                        [ true; false; false; true ]
                    | Arm_OP (Arm_Condition it_op, _, _) when it_op = ITEEE ->
                        [ true; false; false; false ]
                    | _ -> []
                  in
                  let it_instr = instr in
                  let cond_arr = get_it_cond_arr instr in
                  let check_if_label (e : exp) : bool =
                    match e with Label l -> true | _ -> false
                  in
                  if check_if_label (get_exp_1 it_instr) = false then
                    failwith
                      (Printf.sprintf
                         "Error: The operand of IT instruction must be a \
                          label: %s\n"
                         (pp_print_instr' it_instr));

                  let new_instrs = ref [] in
                  List.iteri
                    (fun i it_cond ->
                      let offset = i + 1 in
                      let target_instr = List.nth ordered_il (idx + offset) in
                      let target_op = get_op target_instr in
                      let (Label it_label) = get_exp_1 it_instr in
                      let new_target_condsuff =
                        if it_cond then Arm_parser.condsuff_symb it_label
                        else if it_label = "eq" then NE
                        else if it_label = "ne" then EQ
                        else if it_label = "hs" then LO
                        else if it_label = "lo" then HS
                        else if it_label = "cs" then CC
                        else if it_label = "cc" then CS
                        else if it_label = "mi" then PL
                        else if it_label = "pl" then MI
                        else if it_label = "vs" then VC
                        else if it_label = "vc" then VS
                        else if it_label = "hi" then LS
                        else if it_label = "ls" then HI
                        else if it_label = "ge" then LT
                        else if it_label = "lt" then GE
                        else if it_label = "gt" then LE
                        else if it_label = "le" then GT
                        else
                          failwith
                            (Printf.sprintf
                               "Error: Unexpected condition suffix: %s\n"
                               it_label)
                      in
                      let new_instr =
                        change_condsuff target_instr (Some new_target_condsuff)
                      in
                      new_instrs := !new_instrs @ [ new_instr ])
                    cond_arr;
                  aux (instr :: acc) (List.rev !new_instrs) (idx + 1) t
              | _ -> aux (instr :: acc) [] (idx + 1) t
          end
        end
    in
    aux [] [] 0 ordered_il

  method build_address_hex_map () =
    let lines = read_file "hexcode.info" in
    List.iter (fun line ->
      let trimmed_line = String.trim line in
      match Str.split (Str.regexp ":") trimmed_line with
      | addr :: hex :: _ ->
        let addr' = "0x"^addr in
        let trimmed_hex = String.trim hex in
        (* Printf.printf "addr: %d, hex: %s\n" (int_of_string addr') trimmed_hex; *)
        Hashtbl.replace addr_hexcode_map (int_of_string addr') trimmed_hex
      | _ -> ()
    ) lines;

  method process_instrs (l : string list) (arch : string) =
    let cat_tail s =
      match s with
      | [] -> "" (* this will never happen*)
      | h::t -> String.trim (String.concat ":" t)
    in
    let is_illegal_instr (instr : string) : bool =
      (* If there is an "illegal" instruction in the Thumb objdump result,
       * the instruction is a literal pool in most cases.
       * Therefore, we can skip the instruction.
       * See [text_as_data] in [arm_reassemble_symbol_get.ml#v_exp2] *)
      let illegal_instrs = [
        "illegal"; "??";"cdp"; "cdp2"; "mrc"; "mrc2"; "ldc2l"; "stc"; "stc2l";
        "ltc2l"; "vst1"; "ldc"; "ldcl"; "mrrc"; "mcr2"; "mcrr"; "mcr";
        "vld4"; "vld1.8"; "bfcsel";]
      in
      let illegal_opcodes = [
        "bfl"; "vrhadd";
      ]
      in
      let rec has_illegal_instr instr' = function
        | [] -> false
        | illegal_instr :: t ->
          if contains instr' illegal_instr then true
          else has_illegal_instr instr' t
      in
      let rec has_illegal_opcode instr' = function
      | [] -> false
      | illegal_opcode :: t ->
        (* split using \t or " " as delimeters *)
        let items = Str.split (Str.regexp "[\t ]+") instr' in
        let opcode = List.nth items 0 in
        if contains opcode illegal_opcode then true
        else has_illegal_opcode instr' t
      in
      has_illegal_instr instr illegal_instrs || has_illegal_opcode instr illegal_opcodes
    in
    (* make a branch here to split Intel and ARM parsers using inheritance *)
    let p = self#create_parser arch in
    let _ = p#set_funclist(funcs)
    and _ = p#set_seclist(secs)
    and split = Str.split (Str.regexp_string ":") in

    (* change empty instruction to "undefined" string *)
    let l' =
      List.map
        (fun i ->
          let items = split i in
          let len = List.length items in
          if len > 1 then i
          else
            let loc = List.nth items 0 in
            loc ^ ":\tundefined") l
    in
    let help i =
      let items = split i in
      let loc = List.nth items 0 in
      let instr = cat_tail items in
      if arch = "thumb" && is_illegal_instr instr then
        ()
      else
        instrs <- (p#parse_instr instr loc arch)::instrs;
    in
    List.iter help l';
    if arch = "arm" || arch = "thumb" then begin
      self#build_address_hex_map ();
      self#remove_literal_pools instrs;
      instrs <- self#adjust_width_specifier instrs;
    end;
    if arch = "thumb" then begin
      instrs <- self#remove_it_sequence instrs;
      instrs <- self#convert_instructions instrs;
    end;
    funcs <- p#get_funclist;

  method p_instrs =
    List.iter (fun i -> let is = pp_print_instr' i in print_endline is) instrs

  method get_instrs =
    List.rev instrs

  method get_instrs_len =
    List.length instrs

end
