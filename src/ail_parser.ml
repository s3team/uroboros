open Ail_utils
open Utils
open Arm_utils
open Type
open Common_parser
open Parser
open Arm_parser
open Pp_print
open Arm_func_slicer
open Common_func_slicer
open Tags_utils

class ailParser =
  let addr_hexcode_map : (int, string) Hashtbl.t = Hashtbl.create 200 in

  let get_hexcode (instr : instr) : string =
    let loc = get_loc instr in
    Hashtbl.find_opt addr_hexcode_map loc.loc_addr |> Option.value ~default:""
  in

  let is_movs (op : op) : bool =
    match op with
    | Arm_OP (Arm_CommonOP (Arm_Assign MOVS), _, _) -> true
    | _ -> false
  in

  let is_nop (op : op) : bool =
    match op with
    | Arm_OP (Arm_CommonOP (Arm_Other NOP), _, _) -> true
    | _ -> false
  in

  let is_movs_r0_r0 (i : instr) : bool =
    match i with
    | TripleInstr (op, exp1, exp2, _, _, _)
      when is_movs op
           && exp1 = Reg (Arm_Reg (Arm_CommonReg R0))
           && exp2 = Reg (Arm_Reg (Arm_CommonReg R0)) ->
        true
    | _ -> false
  in

  let is_pop = function POP | VPOP | LDMIA -> true | _ -> false in

  let is_push = function PUSH | VPUSH | STMDB -> true | _ -> false in

  let has_r2_reg (i : instr) =
    let module IU = Instr_utils in
    let exp = IU.get_exp_1 i in
    match exp with
    | Label l -> if contains ~str:l ~sub:"r2" then true else false
    | _ -> false
  in

  let has_r3_reg (i : instr) =
    let module IU = Instr_utils in
    let exp = IU.get_exp_1 i in
    match exp with
    | Label l -> if contains ~str:l ~sub:"r3" then true else false
    | _ -> false
  in

  let has_r7_reg (i : instr) =
    let module IU = Instr_utils in
    let exp = IU.get_exp_1 i in
    match exp with
    | Label l -> if contains ~str:l ~sub:"r7" then true else false
    | _ -> false
  in

  let has_fp_reg (i : instr) =
    let module IU = Instr_utils in
    let exp = IU.get_exp_1 i in
    match exp with
    | Label l -> if contains ~str:l ~sub:"fp" then true else false
    | _ -> false
  in

  let is_exp_lr_reg (i : instr) =
    let module IU = Instr_utils in
    let exp = IU.get_exp_1 i in
    match exp with Reg (Arm_Reg (Arm_LinkReg LR)) -> true | _ -> false
  in

  let is_movs (op : op) : bool =
    match op with
    | Arm_OP (Arm_CommonOP (Arm_Assign MOVS), None, _) -> true
    | _ -> false
  in

  let is_mov (op : op) : bool =
    match op with
    | Arm_OP (Arm_CommonOP (Arm_Assign MOV), None, _) -> true
    | _ -> false
  in

  let check_if_common_reg (e : exp) : bool =
    match e with
    | Reg (Arm_Reg (Arm_CommonReg R0)) -> true
    | Reg (Arm_Reg (Arm_CommonReg R1)) -> true
    | Reg (Arm_Reg (Arm_CommonReg R2)) -> true
    | _ -> false
  in

  let is_func_start (instrs : instr list) (i : instr) (idx : int) =
    let module IU = Instr_utils in
    let not_func_start () =
      let next_instr = List.nth instrs (idx + 1) in
      let next_op = IU.get_op next_instr in
      match IU.get_op i with
      | Arm_OP (Arm_StackOP PUSH, _, _)
        when idx + 1 < List.length instrs
             && is_movs next_op
             && check_if_common_reg (IU.get_exp_1 next_instr)
             && check_if_common_reg (IU.get_exp_2 next_instr) -> begin
          (* Not to classify the following pattern as func start:
             In Coreutils ginstall,
             0x145D6: pop {r4,r5,r7,pc} / pop: true
             0x145D8: push {r1,r3,r5,r6,r7} / pop: false
             0x145DA: movs r2,r0 / pop: false
             0x145DC: push {r2,r5,r6,r7} / pop: false
             0x145DE: movs r2,r0 / pop: false *)
          true
        end
      | _ -> false
    in
    if idx + 1 < List.length instrs && not_func_start () then false
    else
      begin match i with
      | TripleInstr
          (Arm_OP (Arm_CommonOP (Arm_Arithm SUB), _, _), _, exp2, _, _, _)
        when exp2 = Reg (Arm_Reg (Arm_StackReg SP)) -> begin
          (* In Coreutils stat,
             14004:	b082      	sub	sp, #8 ;; func start
             14006:	b490      	push	{r4, r7} *)
          let next_instr = List.nth instrs (idx + 1) in
          let next_op = IU.get_op next_instr in
          match next_op with
          | Arm_OP (Arm_StackOP op, _, _) when is_push op -> true
          | _ -> false
        end
      | _ ->
          begin match IU.get_op i with
          | Arm_OP (Arm_StackOP PUSH, _, _) when has_r2_reg i && has_r3_reg i ->
              true
          | Arm_OP (Arm_StackOP PUSH, _, _) when has_r7_reg i -> true
          | Arm_OP (Arm_StackOP PUSH, _, _) -> begin
              let next_instr = List.nth instrs (idx + 1) in
              let next_op = IU.get_op next_instr in
              match next_op with
              | Arm_OP (Arm_StackOP PUSH, _, _) when IU.has_lr_reg next_instr ->
                  true
              | _ -> false
            end
          | Arm_OP (Arm_StackOP op, _, _)
            when is_push op && (IU.has_lr_reg i || has_fp_reg i) ->
              true
          | _ -> false
          end
      end
  in

  let is_func_end (i : instr) =
    let module IU = Instr_utils in
    match IU.get_op i with
    | Arm_OP (Arm_StackOP op, _, _)
      when is_pop op && (IU.has_pc_reg i || has_fp_reg i) ->
        true
    | Arm_OP (Arm_ControlOP BX, _, _) -> begin
        let exp = IU.get_exp_1 i in
        match exp with Reg (Arm_Reg (Arm_LinkReg LR)) -> true | _ -> false
      end
    | _ -> false
  in

  let collect_branch_targets (instrs : instr list) =
    let module IU = Instr_utils in
    List.fold_left
      (fun acc i ->
        match IU.get_op i with
        | Arm_OP (Arm_ControlOP aco, _, _) -> begin
            let exp = IU.get_exp_1 i in
            match exp with
            | Symbol (JumpDes j) -> j :: acc
            | Const (Immediate j) -> j :: acc
            | _ -> acc
          end
        | _ -> acc)
      [] instrs
  in

  object (self)
    val mutable instrs : instr list = []
    val mutable funcs : func list = []
    val mutable secs : section list = []
    val mutable arch : string = ""
    method set_funcs (l : func list) = funcs <- l
    method set_arch (a : string) = arch <- a

    method update_func_info (l : func list) : func list =
      let rec help l acc =
        match l with
        | [] -> acc
        | h :: [] -> List.rev_append acc [ h ]
        | h1 :: h2 :: t ->
            begin if h1.func_begin_addr == h2.func_begin_addr then
              begin if contains ~str:h1.func_name ~sub:"S_0x" then
                let h2' = { h2 with func_end_addr = h1.func_end_addr } in
                help (h2' :: t) (h1 :: acc)
              else if contains ~str:h2.func_name ~sub:"S_0x" then
                let h1' = { h1 with func_end_addr = h2.func_end_addr } in
                help (h2 :: t) (h1' :: acc)
              else help (h2 :: t) (h1 :: acc)
              end
            else help (h2 :: t) (h1 :: acc)
            end
      in
      help l []

    method print_f (fl : func list) =
      List.iter
        (fun f ->
          print_string f.func_name;
          print_string " ";
          print_string (dec_hex f.func_begin_addr);
          print_string " ";
          print_string (dec_hex f.func_end_addr);
          print_string " ";
          print_string @@ string_of_bool f.is_lib;
          print_string "\n")
        fl;
      fl

    method get_funcs =
      let module FnU = Func_utils in
      self#func_slicing |> FnU.unify_funclist_by_name
      |> List.sort (fun f1 f2 -> f1.func_begin_addr - f2.func_begin_addr)
      |> self#update_func_info |> self#filter_func
      (* |> self#print_f *)
      |> FnU.unify_funclist_by_addr

    method filter_func (fl : func list) : func list =
      self#filter_func_by_name fl |> self#filter_func_by_secs

    method filter_func_by_name (fl : func list) : func list =
      List.filter (fun f -> contains ~str:f.func_name ~sub:".text" == false) fl

    (* some of the functions are allocated in .init section,
       in which case we don't have to record *)
    method filter_func_by_secs (fl : func list) : func list =
      let module EU = Elf_utils in
      let text_info = read_file "text_sec.info" in
      let text_info = List.nth text_info 0 in
      let text_items = Str.split (Str.regexp " +") text_info in
      let text_addr = int_of_string ("0x" ^ List.nth text_items 1)
      and text_size = int_of_string ("0x" ^ List.nth text_items 3) in
      let plt_range =
        if EU.elf_arm () && EU.elf_static () then
          let plt_info = read_file "plt_sec.info" in
          if plt_info = [] then
            let _ =
              Printf.printf
                "Warning: No plt section info found. Skip filtering functions \
                 by plt section.\n"
            in
            None
          else begin
            let plt_info = List.nth plt_info 0 in
            let plt_items = Str.split (Str.regexp " +") plt_info in
            let plt_addr = int_of_string ("0x" ^ List.nth plt_items 1)
            and plt_size = int_of_string ("0x" ^ List.nth plt_items 3) in
            Some (plt_addr, plt_size)
          end
        else None
      in
      List.filter
        (fun f ->
          let fl = String.length f.func_name in
          if fl < 3 then true
          else
            begin match
              int_of_string_opt (String.sub f.func_name 2 (fl - 2))
            with
            | None -> true
            | Some n ->
                begin match plt_range with
                | Some (plt_addr, plt_size) ->
                    (n >= text_addr && n < text_addr + text_size)
                    || (n >= plt_addr && n < plt_addr + plt_size)
                | None -> n >= text_addr && n < text_addr + text_size
                end
            end)
        fl

    method print_funcs funcs =
      let help f =
        if f.is_lib = false then begin
          print_string f.func_name;
          print_string "\n";
          print_string (dec_hex f.func_begin_addr);
          print_string "\n";
          print_string (dec_hex f.func_end_addr);
          print_string "\n";
          print_string @@ string_of_bool f.is_lib;
          print_string "\n"
        end
      in
      List.iter help funcs

    method create_func_slicer instrs funcs arch =
      match arch with
      | "intel" ->
          (new Func_slicer.func_slicer instrs funcs :> common_func_slicer)
      | "thumb" | "arm" ->
          (new Arm_func_slicer.arm_func_slicer instrs funcs
            :> common_func_slicer)
      | _ -> failwith "unsupported architecture"

    method func_slicing =
      (* self#print_f funcs; *)
      let fs = self#create_func_slicer instrs funcs arch in
      fs#update_text_info;
      fs#update_func;
      fs#get_funcs arch

    method set_secs (l : section list) = secs <- l

    method create_parser arch =
      match arch with
      | "intel" -> (new Parser.parse :> common_parser)
      | "thumb" | "arm" -> (new Arm_parser.arm_parse :> common_parser)
      | _ -> failwith "unsupported architecture"

    method remove_literal_pools_v1 (instr_list : instr list) =
      (* Assume that literal pools are always located in
         between a pop and a push instruction.
         However, we keep the instructions after the last pop instruction,
         because they might not be literal pools. *)
      let module IU = Instr_utils in
      (* In mixed ARM32/Thumb binaries, ARM32 instructions must not be treated
         as function-end/start boundaries for literal pool detection.
         Load the address→mode map written by arm_arm32_disassembler.py. *)
      let arm32_addr_set : (int, unit) Hashtbl.t = Hashtbl.create 16 in
      (if arch = "arm" then
         let map_file = "instruction_addr_mode_map.txt" in
         try
           let lines = Utils.read_file map_file in
           List.iter
             (fun line ->
               match String.split_on_char ':' line with
               | [ addr_s; mode_s ] -> (
                   if String.trim mode_s = "arm32" then
                     match int_of_string_opt (String.trim addr_s) with
                     | Some addr -> Hashtbl.replace arm32_addr_set addr ()
                     | None -> ())
               | _ -> ())
             lines
         with Sys_error _ -> ());
      let is_arm32_instr (i : instr) =
        Hashtbl.mem arm32_addr_set (get_loc i).loc_addr
      in
      let ordered_il = List.rev instr_list in
      let branch_target_addrs = ref [] in
      let instrs_after_pop = ref [] in
      let instrs_to_keep = ref [] in
      let is_going_through_literal_pool = ref false in
      let pop_detected = ref false in
      let collect_branch_target_addr (i : instr) =
        (* Use get_ct_des to handle all branch target encodings:
           Symbol(JumpDes), Const(Immediate), Const(Point), etc. *)
        match IU.get_ct_des i with
        | Some j -> branch_target_addrs := j :: !branch_target_addrs
        | None -> ()
      in
      let check_if_target_in_branch_target_addrs (addr_list : int list)
          (i : instr) : bool =
        let loc = get_loc i in
        List.exists (fun addr -> addr = loc.loc_addr) addr_list
      in
      let process_instr (instr : instr) (index : int) : bool =
        (* function start *)
        if is_func_start ordered_il instr index then begin
          is_going_through_literal_pool := false;
          pop_detected := false;
          (* check if at least one of addresses in branch_target_addrs
             is in addr of instr in instrs_after_pop *)
          let c2c_symbol_detected =
            List.exists
              (fun lp_instr ->
                let result =
                  check_if_target_in_branch_target_addrs !branch_target_addrs
                    lp_instr
                in
                result)
              !instrs_after_pop
          in
          if c2c_symbol_detected then begin
            (* keep instructions *)
            instrs_to_keep := !instrs_after_pop @ !instrs_to_keep;
            instrs_after_pop := [];
            true
          end
          else begin
            (* check the length to find the following pattern and keep the instruction:
               e.g., make-prime-list
                 107e4:	e8bd 0ff0 	ldmia.w	sp!, {r4, r5, r6, r7, r8, r9, sl, fp}
                 107e8:	4770      	bx	lr ; want to keep
                 107ea:	e92d 43b0 	stmdb	sp!, {r4, r5, r7, r8, r9, lr} *)
            if List.length !instrs_after_pop = 1 then begin
              let ii = List.hd !instrs_after_pop in
              match IU.get_op ii with
              | Arm_OP (Arm_ControlOP BX, _, _) ->
                  (* keep the branch instruction *)
                  instrs_to_keep := !instrs_after_pop @ !instrs_to_keep;
                  instrs_after_pop := []
              | _ -> instrs_after_pop := []
            end
            else begin
              (* Remove (comment out) instructions *)
              List.iter
                (fun i ->
                  let commented_out_instr = mark_commented_out i in
                  instrs_to_keep := commented_out_instr :: !instrs_to_keep)
                !instrs_after_pop
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
            let result = !instrs_after_pop @ !instrs_to_keep in
            result
          end
        | instr :: t -> begin
            if !pop_detected = true then is_going_through_literal_pool := true;
            let should_keep = process_instr instr index in

            if !is_going_through_literal_pool = true then begin
              instrs_after_pop := instr :: !instrs_after_pop;
              aux acc (index + 1) t
            end
            else begin
              collect_branch_target_addr instr;
              instrs_to_keep := instr :: !instrs_to_keep;
              aux (instr :: acc) (index + 1) t
            end
          end
      in
      aux [] 0 ordered_il

    method adjust_width_specifier (instrs : instr list) : instr list =
      let module IU = Instr_utils in
      let change_width_specifier (i : instr)
          (new_widthsuff : arm_widthsuff option) : instr =
        match i with
        | SingleInstr (Arm_OP (op, condsuff, widthsuff), loc, pre, tags) ->
            SingleInstr (Arm_OP (op, condsuff, new_widthsuff), loc, pre, tags)
        | DoubleInstr (Arm_OP (op, condsuff, widthsuff), exp, loc, pre, tags) ->
            DoubleInstr
              (Arm_OP (op, condsuff, new_widthsuff), exp, loc, pre, tags)
        | TripleInstr
            (Arm_OP (op, condsuff, widthsuff), exp, exp2, loc, pre, tags) ->
            TripleInstr
              (Arm_OP (op, condsuff, new_widthsuff), exp, exp2, loc, pre, tags)
        | FourInstr
            (Arm_OP (op, condsuff, widthsuff), exp, exp2, exp3, loc, pre, tags)
          ->
            FourInstr
              ( Arm_OP (op, condsuff, new_widthsuff),
                exp,
                exp2,
                exp3,
                loc,
                pre,
                tags )
        | FifInstr
            ( Arm_OP (op, condsuff, widthsuff),
              exp,
              exp2,
              exp3,
              exp4,
              loc,
              pre,
              tags ) ->
            FifInstr
              ( Arm_OP (op, condsuff, new_widthsuff),
                exp,
                exp2,
                exp3,
                exp4,
                loc,
                pre,
                tags )
        | _ -> failwith "Unsupported instruction type"
      in
      (* let distance_limit = 128 in *)
      List.map
        (fun i ->
          match IU.get_op i with
          | Arm_OP (Arm_ControlOP B, _, Some (Arm_Opqualifier N)) ->
              change_width_specifier i None
          | _ -> i)
        instrs
    (* For branch instructions that have width specifier, adjust it depending
     * on the distance to target address.
     *)

    method remove_literal_pools_with_movs (instr_list : instr list) : instr list
        =
      (* Find nop/blx and movs opcodes patterns like below and remove the literal pool instructions
         that follow the blx instruction:
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
         See Also: arm_parser.ml#push_stack *)
      let module IU = Instr_utils in
      let ordered_il = List.rev instr_list in
      let rec aux acc is_literal_pool is_nop_after_blx is_second_movs idx =
        function
        | [] -> acc
        | instr :: t ->
            begin match IU.get_op instr with
            | Arm_OP (Arm_ControlOP B, _, Some (Arm_Opqualifier W))
            | Arm_OP (Arm_ControlOP B, _, Some (Arm_Opqualifier N))
              when (not is_literal_pool) && idx + 3 < List.length ordered_il ->
              begin
                (* check if the next instruction is nop or not *)
                let n_instr = List.nth ordered_il (idx + 1) in
                let nop_detected = is_nop (IU.get_op n_instr) in
                let nn_offset = if nop_detected then 3 else 2 in
                let nn_instr = List.nth ordered_il (idx + nn_offset) in
                match IU.get_op nn_instr with
                | Arm_OP (Arm_CommonOP (Arm_Assign MOVS), _, _)
                  when check_if_common_reg (IU.get_exp_1 nn_instr)
                       && check_if_common_reg (IU.get_exp_2 nn_instr) -> begin
                    (* keep the blx instruction *)
                    aux (instr :: acc) true nop_detected false (idx + 1) t
                  end
                | _ ->
                    (* keep this instruction *)
                    aux (instr :: acc) false false false (idx + 1) t
              end
            | Arm_OP (Arm_CommonOP (Arm_Other NOP), _, _) when is_nop_after_blx
              -> begin
                (* keep nop instruction located right after blx
                 * to prevent the "undefined reference" issue *)
                aux (instr :: acc) true false false (idx + 1) t
              end
            | _ -> begin
                let instr_tag = get_instr_tag instr in
                if is_literal_pool && instr_tag = Some Pad then begin
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
                      (* Skip (comment out) this instruction *)
                      let commented_out_instr = mark_commented_out instr in
                      aux
                        (commented_out_instr :: acc)
                        true false false (idx + 1) t
                    end
                    else begin
                      (* stop skipping instructions *)
                      aux (instr :: acc) false false false (idx + 1) t
                    end
                  end
                  else if is_literal_pool && idx + 1 < List.length ordered_il
                  then begin
                    let n_instr = List.nth ordered_il (idx + 1) in
                    let cur_op = IU.get_op instr in
                    let next_op = IU.get_op n_instr in
                    if (not is_second_movs) && is_movs next_op then begin
                      (* Skip (comment out) this instruction *)
                      let commented_out_instr = mark_commented_out instr in
                      aux
                        (commented_out_instr :: acc)
                        true false true (idx + 1) t
                    end
                    else if is_second_movs && is_movs cur_op then begin
                      (* Skip (comment out) this instruction *)
                      let commented_out_instr = mark_commented_out instr in
                      aux
                        (commented_out_instr :: acc)
                        true false false (idx + 1) t
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

    method remove_literal_pools_after_blx_exit (instr_list : instr list) :
        instr list =
      let module IU = Instr_utils in
      let ordered_il = List.rev instr_list in
      let rec aux acc is_literal_pool is_nop_after_blx is_second_movs idx =
        function
        | [] -> acc
        | instr :: t ->
            begin match IU.get_op instr with
            | Arm_OP (Arm_ControlOP BLX, _, _) ->
                begin match IU.get_exp_1 instr with
                | Symbol (CallDes func)
                  when func.func_name = "exit" || func.func_name = "abort" ->
                  begin
                    (* check if the next instruction is nop or not *)
                    let n_instr = List.nth ordered_il (idx + 1) in
                    let nop_detected = is_nop (IU.get_op n_instr) in
                    let nn_offset = if nop_detected then 3 else 2 in
                    let nn_instr = List.nth ordered_il (idx + nn_offset) in
                    match IU.get_op nn_instr with
                    | Arm_OP (Arm_CommonOP (Arm_Assign MOV), _, _)
                    | Arm_OP (Arm_CommonOP (Arm_Assign MOVS), _, _)
                      when check_if_common_reg (IU.get_exp_1 nn_instr)
                           && check_if_common_reg (IU.get_exp_2 nn_instr) ->
                      begin
                        (* keep the blx instruction *)
                        aux (instr :: acc) true nop_detected false (idx + 1) t
                      end
                    | _ -> begin
                        (* keep this instruction *)
                        aux (instr :: acc) false false false (idx + 1) t
                      end
                  end
                | _ -> aux (instr :: acc) false false false (idx + 1) t
                end
            | Arm_OP (Arm_CommonOP (Arm_Other NOP), _, _) when is_nop_after_blx
              -> begin
                (* keep nop instruction located right after blx
                 * to prevent the "undefined reference" issue.
                 *)
                aux (instr :: acc) true false false (idx + 1) t
              end
            | _ ->
                begin if is_literal_pool && idx + 1 < List.length ordered_il
                then begin
                  let n_instr = List.nth ordered_il (idx + 1) in
                  let cur_op = IU.get_op instr in
                  let next_op = IU.get_op n_instr in
                  if (not is_second_movs) && (is_movs next_op || is_mov next_op)
                  then begin
                    (* Skip (comment out) this instruction *)
                    let commented_out_instr = mark_commented_out instr in
                    aux (commented_out_instr :: acc) true false true (idx + 1) t
                  end
                  else if is_second_movs && (is_movs cur_op || is_mov cur_op)
                  then begin
                    (* Skip (comment out) this instruction *)
                    let commented_out_instr = mark_commented_out instr in
                    aux
                      (commented_out_instr :: acc)
                      true false false (idx + 1) t
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

    method remove_literal_pools_after_branch (instr_list : instr list) :
        instr list =
      (* Remove instructions after branch opcodes unless they are pointed by
         other instructions *)
      let module IU = Instr_utils in
      let ordered_il = List.rev instr_list in
      let branch_target_addrs = ref [] in
      let collect_branch_target_addr (i : instr) =
        match IU.get_op i with
        | Arm_OP (Arm_ControlOP aco, _, _) -> begin
            let exp = IU.get_exp_1 i in
            match exp with
            | Symbol (JumpDes j) ->
                branch_target_addrs := j :: !branch_target_addrs
            | Const (Immediate j) ->
                branch_target_addrs := j :: !branch_target_addrs
            | _ -> ()
          end
        | _ -> ()
      in
      let check_if_target_in_branch_target_addrs (addr_list : int list)
          (i : instr) : bool =
        let loc = get_loc i in
        List.exists (fun addr -> addr = loc.loc_addr) addr_list
      in
      let _ = List.iter (fun i -> collect_branch_target_addr i) instrs in
      (* [instr_buffer] is for temporarily keeping literal pools until another function begins *)
      let rec aux acc instr_buffer is_literal_pool idx = function
        | [] -> acc
        | [ instr ] -> (instr :: instr_buffer) @ acc
        | instr :: n_instr :: t' -> begin
            let t = n_instr :: t' in
            (* collect_branch_target_addr instr; *)
            if is_literal_pool then begin
              let pointed =
                check_if_target_in_branch_target_addrs !branch_target_addrs
                  instr
              in
              (* The check for whether an instruction is "pointed" to is not perfect:
               * later instructions can point back to earlier ones (e.g., backward branches). *)
              if pointed then aux (instr :: acc) [] false (idx + 1) t
              else
                (* Remove (comment out) the instruction *)
                let commented_out_instr = mark_commented_out instr in
                aux (commented_out_instr :: acc) [] true (idx + 1) t
            end
            else
              begin match IU.get_op instr with
              | Arm_OP (Arm_ControlOP B, None, _)
                when is_nop (IU.get_op n_instr) ->
                  (* keep the nop instruction to prevent the "undefined reference" issue *)
                  aux (n_instr :: instr :: acc) [] true (idx + 2) t
              | Arm_OP (Arm_ControlOP B, None, _) ->
                  aux (instr :: acc) [] true (idx + 1) t
              | _ -> aux (instr :: acc) [] false (idx + 1) t
              end
          end
      in
      aux [] [] false 0 ordered_il

    method remove_inline_paddings (instr_list : instr list) : instr list =
      (* Remove padding patterns.
       e.g., In Coreutils printf,
       1ceb0:	0000      	movs	r0, r0
       1ceb2:	0000      	movs	r0, r0
       1ceb4:	0000      	movs	r0, r0
       1ceb6:	3fb0      	subs	r7, #176	; 0xb0

       The last opcode can be lsrs, rors, subs and so on *)
      let ordered_il = List.rev instr_list in
      let rec aux acc idx = function
        | [] -> acc
        | instr :: n_instr :: nn_instr :: nnn_instr :: t
          when is_movs_r0_r0 instr && is_movs_r0_r0 n_instr
               && is_movs_r0_r0 nn_instr -> begin
            (* Skip (comment out) four instructions *)
            let commented_out_instr = mark_commented_out instr in
            let commented_out_n_instr = mark_commented_out n_instr in
            let commented_out_nn_instr = mark_commented_out nn_instr in
            let commented_out_nnn_instr = mark_commented_out nnn_instr in
            aux
              (commented_out_nnn_instr :: commented_out_nn_instr
             :: commented_out_n_instr :: commented_out_instr :: acc)
              (idx + 4) t
          end
        | instr :: t -> aux (instr :: acc) (idx + 1) t
      in
      aux [] 0 ordered_il

    method remove_literal_pools_by_pc_relative_load (instr_list : instr list) :
        instr list =
      (* Remove instructions pointed to by PC-relative loads *)
      let ordered_il = List.rev instr_list in
      let pc_relative_addrs = ref [] in
      (* Build a set of all known instruction addresses so we never comment out
         a real instruction: a literal pool data entry that happens to decode as
         an LDR with a target inside actual code would otherwise incorrectly
         mark that code instruction as a literal pool. *)
      let instr_addr_set : (int, unit) Hashtbl.t = Hashtbl.create 2048 in
      List.iter
        (fun i -> Hashtbl.replace instr_addr_set (get_loc i).loc_addr ())
        ordered_il;
      (* [instr_buffer] is for temporarily keeping literal pools until another function begins *)
      let rec aux acc instr_buffer = function
        | [] -> acc
        | instr :: [] -> (instr :: instr_buffer) @ acc
        | instr :: t ->
            begin if
              List.exists
                (fun addr -> addr = (get_loc instr).loc_addr)
                !pc_relative_addrs
            then begin
              (* Skip (comment out) this instruction *)
              let commented_out_instr = mark_commented_out instr in
              aux (commented_out_instr :: acc) (instr :: instr_buffer) t
            end
            else begin
              let is_commented_out i =
                Hashtbl.mem (get_tags i) "commented_out"
              in
              match instr with
              | TripleInstr
                  ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
                    Ptr (BinOP_PLUS (Arm_Reg (Arm_PCReg r), offset)),
                    Reg (Arm_Reg (Arm_CommonReg _)),
                    loc,
                    _,
                    _ )
                when offset mod 2 = 0 && not (is_commented_out instr) -> begin
                  let module AU = ArmUtils in
                  let pc_addr = AU.get_pc_relative_addr "thumb" instr in
                  (* Only treat the target as a literal pool if it is not
                     already a known instruction address. If it is, the LDR
                     itself is likely a literal pool data entry that was
                     decoded as an LDR instruction but actually contains a
                     pointer into executable code. *)
                  if not (Hashtbl.mem instr_addr_set pc_addr) then
                    pc_relative_addrs := pc_addr :: !pc_relative_addrs;
                  aux (instr :: acc) [] t
                end
              | _ -> begin aux (instr :: acc) [] t end
            end
            end
      in
      aux [] [] ordered_il

    method tag_inline_paddings (instr_list : instr list) : instr list =
      (* Tag inline paddings to preserve them *)
      let ordered_il = List.rev instr_list in
      let rec aux acc idx = function
        | [] -> acc
        | instr :: n_instr :: nn_instr :: nnn_instr :: t
          when is_movs_r0_r0 instr && is_movs_r0_r0 n_instr
               && is_movs_r0_r0 nn_instr -> begin
            let tagged_instr = replace_instr_tag instr (Some Pad) in
            let tagged_n_instr = replace_instr_tag n_instr (Some Pad) in
            let tagged_nn_instr = replace_instr_tag nn_instr (Some Pad) in
            let tagged_nnn_instr = replace_instr_tag nnn_instr (Some Pad) in
            aux
              (tagged_nnn_instr :: tagged_nn_instr :: tagged_n_instr
             :: tagged_instr :: acc)
              (idx + 4) t
          end
        | instr :: t -> aux (instr :: acc) (idx + 1) t
      in
      aux [] 0 ordered_il

    method tag_branch_islands (instr_list : instr list) : instr list =
      (* Tag branch islands to preserve them *)
      let ordered_il = List.rev instr_list in
      let rec aux acc idx = function
        | [] -> acc
        | instr :: n_instr :: nn_instr :: nnn_instr :: t
          when is_movs_r0_r0 instr && is_movs_r0_r0 n_instr
               && is_movs_r0_r0 nn_instr -> begin
            let tagged_instr = replace_instr_tag instr (Some Pad) in
            let tagged_n_instr = replace_instr_tag n_instr (Some Pad) in
            let tagged_nn_instr = replace_instr_tag nn_instr (Some Pad) in
            let tagged_nnn_instr = replace_instr_tag nnn_instr (Some Pad) in
            aux
              (tagged_nnn_instr :: tagged_nn_instr :: tagged_n_instr
             :: tagged_instr :: acc)
              (idx + 4) t
          end
        | instr :: t -> aux (instr :: acc) (idx + 1) t
      in
      aux [] 0 ordered_il

    method remove_illegal_instructions (instr_list : instr list) (arch : string)
        : instr list =
      let module IU = Instr_utils in
      let ordered_il = List.rev instr_list in
      match arch with
      | "thumb" -> begin
          let has_pc e =
            match e with Reg (Arm_Reg (Arm_PCReg _)) -> true | _ -> false
          in
          let reg_list_has sub e =
            match e with Label s -> contains ~str:s ~sub | _ -> false
          in
          let check_illegal (i : instr) : bool =
            match IU.get_op i with
            | Arm_OP (Arm_SystemOP PLDW, _, _) -> true
            | Arm_OP (Arm_CommonOP (Arm_Arithm VHADD), _, _) -> true
            | Arm_OP (Arm_CommonOP (Arm_Arithm VADDL), _, _) ->
                (* vaddl decodes from literal pool data in Thumb binaries:
              it appears after bl/b instructions before .ltorg, always
              followed by the lsls r0,r1,#1 / movs r0,r0 marker pattern *)
                true
            | Arm_OP (Arm_CommonOP (Arm_Compare CMP), _, _) ->
                (* The pc register is not allowed with CMP *)
                begin match i with
                | TripleInstr (_, e1, e2, _, _, _) -> has_pc e1 || has_pc e2
                | _ -> false
                end
            (* ARMv8-M instructions not present in ARMv7 Thumb binaries.
               Might need to remove this when ARMv8-M support is added *)
            | Arm_OP (Arm_SystemOP SETPAN, _, _) -> true
            | Arm_OP (Arm_ControlOP BXNS, _, _) -> true
            | Arm_OP (Arm_ControlOP BLXNS, _, _) -> true
            | Arm_OP (Arm_CommonOP (Arm_Assign STR), _, _) ->
                (* STR with PC as the source register is UNPREDICTABLE *)
                begin match i with
                | TripleInstr (_, _, e2, _, _, _) -> has_pc e2
                | _ -> false
                end
            | Arm_OP (Arm_CommonOP (Arm_Arithm SDIV), _, _) ->
                (* SDIV with PC as any operand is UNPREDICTABLE *)
                begin match i with
                | FourInstr (_, e1, e2, e3, _, _, _) ->
                    has_pc e1 || has_pc e2 || has_pc e3
                | _ -> false
                end
            | Arm_OP (Arm_CommonOP (Arm_Assign LDRD), _, _) ->
                begin match i with
                | FourInstr (_, _, _, e3, _, _, _) -> has_pc e3
                | _ -> false
                end
            (* Multiply-long where rdhi == rdlo is UNPREDICTABLE *)
            | Arm_OP (Arm_CommonOP (Arm_Arithm UMULL), _, _)
            | Arm_OP (Arm_CommonOP (Arm_Arithm UMLAL), _, _)
            | Arm_OP (Arm_CommonOP (Arm_Arithm SMULL), _, _)
            | Arm_OP (Arm_CommonOP (Arm_Arithm SMLAL), _, _)
            | Arm_OP (Arm_CommonOP (Arm_Arithm SMLALBB), _, _)
            | Arm_OP (Arm_CommonOP (Arm_Arithm SMLALBT), _, _)
            | Arm_OP (Arm_CommonOP (Arm_Arithm SMLALTB), _, _)
            | Arm_OP (Arm_CommonOP (Arm_Arithm SMLALTT), _, _) ->
                begin match i with
                | FifInstr (_, _, _, e3, e4, _, _, _) ->
                    begin match (e3, e4) with
                    | Reg (Arm_Reg r1), Reg (Arm_Reg r2) -> r1 = r2
                    | _ -> false
                    end
                | _ -> false
                end
            | Arm_OP (Arm_StackOP LDMIA, _, _)
            | Arm_OP (Arm_CommonOP (Arm_Assign LDMIA), _, _) ->
                (* LDMIA with both LR+PC, SP in the load list, or empty list, is UNPREDICTABLE *)
                begin match i with
                | TripleInstr (_, e1, e2, _, _, _) -> begin
                    (reg_list_has "lr" e1 && reg_list_has "pc" e1)
                    || reg_list_has "sp" e1
                    || (match e1 with Label s -> s = "{}" | _ -> false)
                    ||
                    match e2 with
                    | Ptr (WB (Arm_CommonReg r)) ->
                        reg_list_has (show_arm_reg (Arm_CommonReg r)) e1
                    | _ -> false
                  end
                | _ -> false
                end
            | Arm_OP (Arm_CommonOP (Arm_Assign STMIA), _, _) ->
                (* STMIA with PC in the register list, or empty list, is UNPREDICTABLE *)
                begin match i with
                | TripleInstr (_, e1, e2, _, _, _) -> (
                    reg_list_has "pc" e1
                    || (match e1 with Label s -> s = "{}" | _ -> false)
                    ||
                    match e2 with
                    | Ptr (WB (Arm_CommonReg r)) ->
                        reg_list_has (show_arm_reg (Arm_CommonReg r)) e1
                    | _ -> false)
                | _ -> false
                end
            | Arm_OP (Arm_CommonOP (Arm_Assign STMDB), _, _) ->
                begin match i with
                | TripleInstr (_, e1, e2, _, _, _) -> (
                    reg_list_has "pc" e1
                    ||
                    match e2 with
                    | Ptr (WB (Arm_CommonReg r)) ->
                        (* Detect a pattern where only base registers are used *)
                        reg_list_has (show_arm_reg (Arm_CommonReg r)) e1
                    | _ -> false)
                | _ -> false
                end
            | Arm_OP (Arm_StackOP PUSH, _, _) | Arm_OP (Arm_StackOP POP, _, _)
              ->
                (* Empty push or pop *)
                begin match i with
                | DoubleInstr (_, e1, _, _, _) ->
                    begin match e1 with Label s -> s = "{}" | _ -> false
                    end
                | _ -> false
                end
            | _ -> false
          in
          let rec help acc = function
            | [] -> acc
            | h :: t ->
                begin if check_illegal h then help acc t else help (h :: acc) t
                end
          in
          help [] ordered_il
        end
      | _ -> instr_list

    method remove_undefined_instructions (instr_list : instr list) : instr list
        =
      let module IU = Instr_utils in
      List.filter
        (fun i -> match IU.get_op i with Undefined_OP -> false | _ -> true)
        instr_list

    method remove_literal_pools (instr_list : instr list) =
      (* Function call order matters *)
      (* instrs <- self#remove_inline_paddings instrs; *)
      instrs <- self#tag_inline_paddings instrs;
      instrs <- self#tag_branch_islands instrs;
      (* instrs <- self#remove_literal_pools_v1 instrs; *)
      (* instrs <- self#remove_literal_pools_with_movs instrs; *)
      (* instrs <- self#remove_literal_pools_after_blx_exit instrs; *)
      (* instrs <- self#remove_literal_pools_after_branch instrs; *)
      instrs <- self#remove_literal_pools_by_pc_relative_load instrs;
      instrs <- self#remove_illegal_instructions instrs arch;
      instrs <- self#remove_undefined_instructions instrs

    method convert_instructions (instr_list : instr list) : instr list =
      let module IU = Instr_utils in
      let ordered_il = List.rev instr_list in
      let rec aux acc idx = function
        | [] -> acc
        | instr :: t ->
            begin match instr with
            | DoubleInstr (Arm_OP (Arm_StackOP PUSH, Some _, width), _, _, _, _)
              ->
                let new_instr =
                  IU.change_op instr (Arm_OP (Arm_StackOP PUSH, None, width))
                in
                aux (new_instr :: acc) (idx + 1) t
            | DoubleInstr
                (Arm_OP (Arm_StackOP STMDB, Some LE, width), _, _, _, _) ->
                let new_instr =
                  IU.change_op instr (Arm_OP (Arm_StackOP STMDB, None, width))
                in
                aux (new_instr :: acc) (idx + 1) t
            | DoubleInstr
                (Arm_OP (Arm_StackOP PUSH, Some LT, width), Label label, _, _, _)
              when contains ~str:label ~sub:"lr" ->
                let new_instr =
                  IU.change_op instr (Arm_OP (Arm_StackOP PUSH, None, width))
                in
                aux (new_instr :: acc) (idx + 1) t
            | DoubleInstr (Arm_OP (Arm_ControlOP BL, Some PL, None), _, _, _, _)
              ->
                let new_instr =
                  IU.change_op instr (Arm_OP (Arm_ControlOP BL, None, None))
                in
                aux (new_instr :: acc) (idx + 1) t
            | TripleInstr
                (Arm_OP (Arm_StackOP STMDB, Some LE, width), _, _, _, _, _) ->
                let new_instr =
                  IU.change_op instr (Arm_OP (Arm_StackOP STMDB, None, width))
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
                  IU.change_op instr
                    (Arm_OP (Arm_CommonOP (Arm_Arithm SUB), None, width))
                in
                aux (new_instr :: acc) (idx + 1) t
            | _ -> (
                match IU.get_op instr with
                | Arm_OP (Arm_CommonOP (Arm_Assign MOV), Some UND, _) ->
                    let new_instr =
                      IU.change_op instr
                        (Arm_OP (Arm_CommonOP (Arm_Assign MOVS), None, None))
                    in
                    aux (new_instr :: acc) (idx + 1) t
                | Arm_OP (Arm_CommonOP (Arm_Arithm SUB), Some UND, _) ->
                    let new_instr =
                      IU.change_op instr
                        (Arm_OP (Arm_CommonOP (Arm_Arithm SUB), None, None))
                    in
                    aux (new_instr :: acc) (idx + 1) t
                | Arm_OP
                    ( Arm_CommonOP (Arm_Assign MOVS),
                      None,
                      Some (Arm_Opqualifier W) ) ->
                    let new_instr =
                      IU.change_op instr
                        (Arm_OP (Arm_CommonOP (Arm_Assign MOVS), None, None))
                    in
                    aux (new_instr :: acc) (idx + 1) t
                | _ -> aux (instr :: acc) (idx + 1) t)
            end
      in
      aux [] 0 ordered_il

    method remove_it_sequence (instr_list : instr list) : instr list =
      let module IU = Instr_utils in
      let ordered_il = List.rev instr_list in
      let rec aux acc skip_count idx = function
        | [] -> acc
        | instr :: t ->
            begin match IU.get_op instr with
            | Arm_OP (Arm_Condition it_op, _, _) ->
                let it_cond_length =
                  match it_op with
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
                  match IU.get_op instr with
                  | Arm_OP (_, Some _, _) -> true
                  | _ -> false
                in
                let has_und_condsuff (instr : instr) : bool =
                  match IU.get_op instr with
                  | Arm_OP (_, Some UND, _) -> true
                  | _ -> false
                in
                let rec count_condsuff count n =
                  match n with
                  | 0 -> count
                  | _ -> begin
                      let target_instr = List.nth ordered_il (idx + n) in
                      if has_condsuff target_instr then
                        count_condsuff (count + 1) (n - 1)
                      else count_condsuff count (n - 1)
                    end
                in
                let rec has_und_in_sequence n =
                  match n with
                  | 0 -> false
                  | _ ->
                      let target_instr = List.nth ordered_il (idx + n) in
                      has_und_condsuff target_instr
                      || has_und_in_sequence (n - 1)
                in
                let cond_count = count_condsuff 0 it_cond_length in
                let und_in_block = has_und_in_sequence it_cond_length in
                if und_in_block then
                  (* UND condition means the else-branch of AL — literal pool data.
                   Remove IT and all instructions in the block. *)
                  aux acc it_cond_length (idx + 1) t
                else if cond_count < it_cond_length then
                  (* Remove only IT instructions, and keep the sequences *)
                  aux acc 0 (idx + 1) t
                else aux (instr :: acc) 0 (idx + 1) t
            | _ ->
                (* For now, since the skip count is always 0. *)
                if skip_count > 0 then aux acc (skip_count - 1) (idx + 1) t
                else aux (instr :: acc) 0 (idx + 1) t
            end
      in
      aux [] 0 0 ordered_il

    method remove_orphan_condsuff (instr_list : instr list) : instr list =
      let module IU = Instr_utils in
      let ordered_il = List.rev instr_list in
      (* Load ARM32 address ranges from arm32_replaced.info so that ARM32
         conditional instructions (which don't need IT blocks) are not
         incorrectly commented out. *)
      let arm32_ranges : (int * int) list =
        if arch <> "thumb" then []
        else
          try
            let lines = Utils.read_file "arm32_replaced.info" in
            List.filter_map
              (fun line ->
                match Str.split (Str.regexp ":") line with
                | [ s; e ] -> (
                    match (int_of_string_opt s, int_of_string_opt e) with
                    | Some start_addr, Some end_addr ->
                        Some (start_addr, end_addr)
                    | _ -> None)
                | _ -> None)
              lines
          with Sys_error _ -> []
      in
      let is_in_arm32_range (instr : instr) : bool =
        let addr = (get_loc instr).loc_addr in
        List.exists (fun (s, e) -> addr >= s && addr < e) arm32_ranges
      in
      let it_cond_length it_op =
        match it_op with
        | IT -> 1
        | ITE | ITT -> 2
        | ITTT | ITTE | ITEE | ITET -> 3
        | ITTTT | ITTTE | ITTET | ITTEE | ITETT | ITETE | ITEET | ITEEE -> 4
        | _ -> failwith "unsupported IT opcode"
      in
      let rec aux acc it_remaining = function
        | [] -> acc
        | instr :: t ->
            begin match IU.get_op instr with
            | Arm_OP (Arm_Condition it_op, _, _) ->
                let length = it_cond_length it_op in
                aux (instr :: acc) length t
            | Arm_OP (_, Some _, _) when it_remaining > 0 ->
                aux (instr :: acc) (it_remaining - 1) t
            | Arm_OP (Arm_ControlOP _, Some _, _) ->
                (* Branch with condition suffix but no IT block: keep as-is. *)
                aux (instr :: acc) 0 t
            | Arm_OP (_, Some _, _) when is_in_arm32_range instr ->
                (* ARM32 conditional instruction: all instructions can be
                   conditional without an IT block in ARM32 mode. *)
                aux (instr :: acc) 0 t
            | Arm_OP
                ( (Arm_SystemOP (MRC | MRC2 | MCR | MCR2) as cop_op),
                  Some _,
                  width ) ->
                (* Coprocessor instruction with orphan condition suffix: the
                   condition is spurious, coming from literal-pool data decoded
                   as IT blocks. Strip it and keep the instruction. *)
                aux
                  (IU.change_op instr (Arm_OP (cop_op, None, width)) :: acc)
                  0 t
            | Arm_OP (_, Some _, _) ->
                (* Non-branch with condition suffix but no IT block: comment out. *)
                aux (mark_commented_out instr :: acc) 0 t
            | _ -> aux (instr :: acc) (max 0 (it_remaining - 1)) t
            end
      in
      aux [] 0 ordered_il

    method convert_it_condsuff (instr_list : instr list) : instr list =
      (* Currently, disabled. Check IT condition suffixes and correct them if
         necessary. *)
      let module IU = Instr_utils in
      let ordered_il = List.rev instr_list in
      let rec aux acc replacement_instrs idx = function
        | [] -> acc
        | instr :: t ->
            begin match replacement_instrs with
            | r_instr :: r_t -> aux (r_instr :: acc) r_t (idx + 1) t
            | [] ->
                begin match IU.get_op instr with
                | Arm_OP (Arm_Condition it_op, Some cond, width) as op
                  when idx + 4 < List.length ordered_il ->
                    let get_it_cond_arr (it_instr : instr) : bool list =
                      match IU.get_op it_instr with
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
                    if check_if_label (IU.get_exp_1 it_instr) = false then
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
                        let target_op = IU.get_op target_instr in
                        let (Label it_label) = IU.get_exp_1 it_instr in
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
                          IU.change_condsuff target_instr
                            (Some new_target_condsuff)
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
      List.iter
        (fun line ->
          let trimmed_line = String.trim line in
          match Str.split (Str.regexp ":") trimmed_line with
          | addr :: hex :: _ ->
              let addr' = "0x" ^ addr in
              let trimmed_hex = String.trim hex in
              (* Printf.printf "addr: %d, hex: %s\n" (int_of_string addr') trimmed_hex; *)
              Hashtbl.replace addr_hexcode_map (int_of_string addr') trimmed_hex
          | _ -> ())
        lines

    method process_instrs (l : string list) (arch : string) =
      let module EU = Elf_utils in
      let cat_tail = function
        | [] -> "" (* this will never happen*)
        | h :: t -> String.trim (String.concat ":" t)
      in
      let is_illegal_instr (instr : string) : bool =
        (* If there is an "illegal" instruction in the Thumb objdump result,
         * the instruction is a literal pool in most cases.
         * Therefore, we can skip the instruction.
         * See [text_as_data] in [arm_reassemble_symbol_get.ml#v_exp2] *)
        let illegal_instrs =
          [
            "illegal";
            "??";
            "cdp";
            "cdp2";
            "ldc2l";
            "stc";
            "stc2l";
            "ltc2l";
            "vst1";
            "ldc";
            "ldcl";
            "mrrc";
            "mcrr";
            "vld4";
            "vld1.8";
            "bfcsel";
          ]
        in
        let illegal_opcodes =
          [
            "bfl";
            "vrhadd";
            (* original: stcl p1, c10, [ip], 8
             * in objdump: stfp f2, [ip], #8 ;; wrong
             *)
            "stfp";
            (* original: ldcl p1, c10, [ip], 8
             * in objdump: ldfp f2, [ip], #8 ;; wrong
             *)
            "ldfp";
            "stfe";
            (* original: ldcl p1, c0, [r0], 8
             * in objdump: ldfe    f0, [r0], #8 ;; wrong
             *)
            "ldfe";
          ]
        in
        let rec has_illegal_instr instr' = function
          | [] -> false
          | illegal_instr :: t ->
              if contains ~str:instr' ~sub:illegal_instr then true
              else has_illegal_instr instr' t
        in
        let rec has_illegal_opcode instr' = function
          | [] -> false
          | illegal_opcode :: t ->
              (* split using \t or " " as delimeters *)
              let items = Str.split (Str.regexp "[\t ]+") instr' in
              let opcode = List.nth items 0 in
              if contains ~str:opcode ~sub:illegal_opcode then true
              else has_illegal_opcode instr' t
        in
        has_illegal_instr instr illegal_instrs
        || has_illegal_opcode instr illegal_opcodes
      in
      (* make a branch here to split Intel and ARM parsers using inheritance *)
      let p = self#create_parser arch in
      let _ = p#set_funclist funcs
      and _ = p#set_seclist secs
      and split = Str.split (Str.regexp_string ":") in
      (* change empty instruction to "undefined" string *)
      let l' =
        List.map
          (fun i ->
            let items = split i in
            let len = List.length items in
            if len > 1 then
              begin if String.trim (cat_tail items) = "" then begin
                (* 102fc: b6c2             ; <UNDEFINED> instruction: 0xb6c2 *)
                let loc = List.nth items 0 in
                loc ^ ":\tundefined"
              end
              else i
              end
            else
              let loc = List.nth items 0 in
              loc ^ ":\tundefined")
          l
      in
      let help i =
        let items = split i in
        let loc = List.nth items 0 in
        let instr = cat_tail items in
        if arch = "thumb" && is_illegal_instr instr then ()
        else if arch = "intel" && contains ~str:instr ~sub:"undefined" then ()
        else instrs <- p#parse_instr instr loc arch :: instrs
      in
      List.iter help l';
      if (arch = "arm" && EU.elf_32 ()) || arch = "thumb" then begin
        self#build_address_hex_map ();
        if EU.elf_32 () then self#remove_literal_pools instrs;
        instrs <- self#adjust_width_specifier instrs
      end;
      if arch = "thumb" then begin
        instrs <- self#convert_instructions instrs;
        instrs <- self#remove_it_sequence instrs;
        instrs <- self#remove_orphan_condsuff instrs
      end;
      funcs <- p#get_funclist

    method process_asms (l : string list) (arch : string) =
      let p = self#create_parser arch in
      let help instr = instrs <- p#parse_instr instr "0" arch :: instrs in
      List.iter help l;
      instrs <- List.rev instrs

    method p_instrs =
      List.iter
        (fun i ->
          let is = pp_print_instr' i in
          print_endline is)
        instrs

    method get_instrs = List.rev instrs
    method get_instrs_len = List.length instrs
  end
