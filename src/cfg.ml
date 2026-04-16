open Visit
open Type
open Pp_print
open Ail_utils
open Utils

type t = { bn : string; baddr : int; eaddr : int }

class cfg =
  let cfg_table = Hashtbl.create 40
  and cfg_bdiv_table = Hashtbl.create 40
  and counter = ref 0 in

  object (self)
    val mutable found_entry = false
    val mutable skip_entry = false

    val mutable entry_loc : loc =
      { loc_label = ""; loc_addr = 0; loc_visible = true }

    val mutable last_loc : loc =
      { loc_label = ""; loc_addr = 0; loc_visible = true }

    val mutable end_loc : loc =
      { loc_label = ""; loc_addr = 0; loc_visible = true }

    val mutable entry_instr : instr =
      SingleInstr
        ( Intel_OP (Intel_CommonOP (Intel_Other NOP)),
          { loc_label = ""; loc_addr = 0; loc_visible = true },
          None,
          Hashtbl.create 0 )

    val mutable bb_list = []
    val mutable bl = []
    val mutable bl_sort = Array.make 1 { bn = ""; baddr = 0; eaddr = 0 }
    inherit ailVisitor

    method cfg_exp e l =
      let a = l.loc_addr in
      match e with
      | Symbol e' -> begin
          match e' with
          | JumpDes d -> Hashtbl.replace cfg_table a d
          | CallDes f ->
              if f.is_lib = false then
                Hashtbl.replace cfg_table a f.func_begin_addr
              else ()
          | _ -> ()
        end
      | _ -> ()

    method vinst' (i : instr) =
      let is_ct = function
        | Intel_OP io -> begin
            match io with
            | Intel_ControlOP ico -> begin
                match ico with
                | Intel_Jump _ -> true (* jump ops *)
                | CALL -> true (* call *)
                | _ -> false
              end
            | _ -> false
          end
        | Arm_OP (ao, _, _) -> begin
            match ao with Arm_ControlOP _ -> true | _ -> false
          end
        | _ -> false
      in
      match i with
      | DoubleInstr (p, e, l, _, _) when is_ct p ->
          self#cfg_exp e l;
          i
      | _ -> i

    method vinst (i : instr) =
      let module IU = Instr_utils in
      let has_pc_reg (i : instr) =
        let exp = IU.get_exp_1 i in
        match exp with
        | Label l -> if contains ~str:l ~sub:"pc" then true else false
        | _ -> false
      in
      let is_last_instr l =
        if l.loc_addr = end_loc.loc_addr then true else false
      in
      let bb_exit (i : instr) l =
        match IU.get_op i with
        | Intel_OP io -> begin
            match io with
            | Intel_ControlOP ico -> begin
                match ico with
                | Intel_Jump _ -> true (* jump ops *)
                (*| CALL -> true (* call *)*)
                | RET -> true
                | RETN -> true
                | _ -> false
              end
            | _ -> false
          end
        | Arm_OP (ao, _, _) -> begin
            match ao with
            | Arm_ControlOP _ -> true
            | Arm_StackOP POP when has_pc_reg i -> true
            | _ -> false
          end
        | _ -> false
      and bb_entry i =
        let l = IU.get_loc i in
        let s = l.loc_label in
        if contains ~str:s ~sub:":" then true else false
      and help_entry i =
        if found_entry = true then begin
          let bn = "BB_" ^ string_of_int !counter in
          counter := !counter + 1;
          let b =
            {
              bf_name = "";
              bblock_name = bn;
              bblock_begin_loc = entry_loc;
              bblock_end_loc = last_loc;
              bblock_head_instr = entry_instr;
            }
          in

          bb_list <- b :: bb_list;

          last_loc <- IU.get_loc i;
          entry_loc <- IU.get_loc i;
          entry_instr <- i;
          found_entry <- true
        end
        else begin
          found_entry <- true;
          entry_loc <- IU.get_loc i;
          entry_instr <- i;
          last_loc <- IU.get_loc i
        end
      and help_exit i =
        if found_entry = true then begin
          last_loc <- IU.get_loc i;
          let bn = "BB_" ^ string_of_int !counter in
          counter := !counter + 1;
          let b =
            {
              bf_name = "";
              bblock_name = bn;
              bblock_begin_loc = entry_loc;
              bblock_end_loc = last_loc;
              bblock_head_instr = entry_instr;
            }
          in
          bb_list <- b :: bb_list;
          found_entry <- false;
          skip_entry <- true;
          i
        end
        else if is_last_instr (IU.get_loc i) = true then begin
          let tloc = IU.get_loc i in
          let bn = "BB_" ^ string_of_int !counter in
          counter := !counter + 1;
          let b =
            {
              bf_name = "";
              bblock_name = bn;
              bblock_begin_loc = tloc;
              bblock_end_loc = tloc;
              bblock_head_instr = i;
            }
          in
          bb_list <- b :: bb_list;
          i
        end
        else begin
          last_loc <- IU.get_loc i;
          i
        end
      in
      match i with
      | _ when skip_entry = true && is_last_instr (IU.get_loc i) -> begin
          let tloc = IU.get_loc i in
          let bn = "BB_" ^ string_of_int !counter in
          counter := !counter + 1;
          let b =
            {
              bf_name = "";
              bblock_name = bn;
              bblock_begin_loc = tloc;
              bblock_end_loc = tloc;
              bblock_head_instr = i;
            }
          in
          bb_list <- b :: bb_list;
          i
        end
      | _ when skip_entry = true -> begin
          entry_loc <- IU.get_loc i;
          entry_instr <- i;
          found_entry <- true;
          skip_entry <- false;
          last_loc <- IU.get_loc i;
          let module IU = Instr_utils in
          if IU.is_control_transfer_op i then help_exit i else i
        end
      | _ when is_last_instr (IU.get_loc i) = true -> help_exit i
      | _ when bb_entry i ->
          help_entry i;
          let module IU = Instr_utils in
          if IU.is_control_transfer_op i then help_exit i else i
      | DoubleInstr (p, e, l, _, _) when bb_exit i l -> help_exit i
      | SingleInstr (p, l, _, _) when bb_exit i l -> help_exit i
      | _ -> begin
          last_loc <- IU.get_loc i;
          i
        end

    method visit il =
      let module IU = Instr_utils in
      let last_i = List.nth il (List.length il - 1) in
      end_loc <- IU.get_loc last_i;
      let il' = List.map self#vinst' il in
      bl <- self#update_bl;
      self#fb_list bl;
      let bl' =
        List.sort
          (fun b1 b2 ->
            b1.bblock_begin_loc.loc_addr - b2.bblock_begin_loc.loc_addr)
          bl
      in
      let bl1 =
        List.map
          (fun b ->
            {
              bn = b.bblock_name;
              baddr = b.bblock_begin_loc.loc_addr;
              eaddr = b.bblock_end_loc.loc_addr;
            })
          bl'
      in
      bl_sort <- Array.of_list bl1;
      il'

    method get_fbl =
      let help f bl =
        let bl' = List.rev bl in
        Hashtbl.replace cfg_bdiv_table f bl'
      in
      Hashtbl.iter help cfg_bdiv_table;
      cfg_bdiv_table

    method get_bbl = bl

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
          print_string "\n"
        in
        List.iter h' bl;
        print_string "\n"
      in
      Hashtbl.iter help cfg_bdiv_table

    method fb_list bl =
      let help b =
        let fn = b.bf_name in
        let bl =
          Hashtbl.find_opt cfg_bdiv_table fn |> Option.value ~default:[]
        in
        Hashtbl.replace cfg_bdiv_table fn (b :: bl)
      in
      List.iter help bl

    method update_bl =
      let funcs' =
        List.sort (fun f1 f2 -> f1.func_begin_addr - f2.func_begin_addr) funcs
      and bls' =
        List.sort
          (fun b1 b2 ->
            b1.bblock_begin_loc.loc_addr - b2.bblock_begin_loc.loc_addr)
          bb_list
      and addr_comp addr1 addr2 = addr1 - addr2 in
      let rec assign fl bl acc =
        match (fl, bl) with
        | hf :: tf, hb :: tb -> begin
            let b_bl = hb.bblock_begin_loc
            and b_el = hb.bblock_end_loc
            and f_ba = hf.func_begin_addr
            and f_ea = hf.func_end_addr in
            if
              addr_comp b_bl.loc_addr f_ba >= 0
              && addr_comp b_el.loc_addr f_ea <= 0
            then
              let hb' = { hb with bf_name = hf.func_name } in
              assign fl tb (hb' :: acc)
            else assign tf bl acc
          end
        | fl', [] -> List.rev acc
        | [], hb :: tb -> assign [] tb (hb :: acc)
      in
      assign funcs' bls' []

    method recover_cfg =
      let module IU = Instr_utils in
      let is_jmp = Instr_utils.is_jmp in
      let is_cond_jmp = Opcode_utils.is_cond_jmp in
      let is_call = Opcode_utils.is_call in
      let is_ret = Instr_utils.is_ret in
      let is_arm_ret = Instr_utils.is_arm_ret in
      let is_indirect = Exp_utils.is_indirect in
      let is_func = Exp_utils.is_func in
      let next_bb (bnl : string list) bn =
        let bn' = get_next_bb bn in
        if List.mem bn' bnl then bn' else "INTER"
      in
      let bbn_byloc addr =
        let cmp addr b =
          if b.baddr <= addr && b.eaddr >= addr then 0
          else if b.baddr > addr then -1
          else 1
        in
        let rec bs min max =
          if min > max then begin
            print_string (dec_hex addr);
            print_string "\n";
            let blen = Array.length bl_sort in
            let blast = Array.get bl_sort (blen - 1) in
            let bfist = Array.get bl_sort 0 in
            print_string (dec_hex bfist.baddr);
            print_string "\n";
            print_string (dec_hex bfist.eaddr);
            print_string "\n";
            assert false
          end
          else
            let mid = (min + max) / 2 in
            let delta =
              try cmp addr (Array.get bl_sort mid)
              with _ ->
                begin
                  print_string "failed ! ";
                  print_int mid;
                  print_string "\n";
                  print_int (Array.length bl_sort);
                  print_string "\n";
                  0
                end
            in
            if delta = 0 then (Array.get bl_sort mid).bn
            else if delta < 0 then bs min (mid - 1)
            else bs (mid + 1) max
        in
        bs 0 (Array.length bl_sort - 1)
      in
      let indirect_ret i =
        let l = IU.get_loc i in
        let bn = bbn_byloc l.loc_addr in
        if bn = "" then assert false else (bn, (J, Some "RET"))
      in
      let indirect i =
        let l = IU.get_loc i in
        let bn = bbn_byloc l.loc_addr in
        if bn = "" then assert false else (bn, (J, Some "T"))
      in
      (* abandon *)
      let dir_sin_c i =
        let l = IU.get_loc i in
        let bn = bbn_byloc l.loc_addr in
        if bn = "" then assert false else (bn, (J, Some "INTER"))
      in
      let dir_c bnl i =
        let l = IU.get_loc i in
        let bn = bbn_byloc l.loc_addr in
        if bn = "" then assert false
        else begin
          let bn' = next_bb bnl bn in
          let b1 = (bn, (J, Some "INTER")) in
          let b2 = (bn, (J, Some bn')) in
          (b1, b2)
        end
      in
      let dir_sin_j i e =
        let l = IU.get_loc i in
        let sn = bbn_byloc l.loc_addr in
        if sn = "" then assert false
        else begin
          let es = p_exp e in
          let en = recover_addr_from_label es in
          if en = -1 then (sn, (J, Some "T"))
          else begin
            let dn = bbn_byloc en in
            if dn = "" then begin
              print_string "\n";
              print_string es;
              assert false
            end
            else (sn, (J, Some dn))
          end
        end
      in
      let dir_dou_j bnl i e =
        let ((sn, _) as d1) = dir_sin_j i e in
        let sn' = next_bb bnl sn in
        (d1, (sn, (F, Some sn')))
      in
      let dir_fall bnl i =
        let l = IU.get_loc i in
        let sn = bbn_byloc l.loc_addr in
        if sn = "" then assert false
        else
          let dn = next_bb bnl sn in
          (sn, (F, Some dn))
      in
      let aux (bnl : string list) acc i =
        match i with
        | SingleInstr (p, _, _, _) when is_ret i -> indirect_ret i :: acc
        | DoubleInstr (_, _, _, _, _) when is_arm_ret i -> indirect_ret i :: acc
        | DoubleInstr (_, e, _, _, _) when is_indirect e -> indirect i :: acc
        | DoubleInstr (p, _, _, _, _) when is_call p ->
            let d1, d2 = dir_c bnl i in
            d1 :: d2 :: acc
        | DoubleInstr (p, e, _, _, _) when is_jmp i && is_func e = true ->
            dir_sin_c i :: acc
        | DoubleInstr (p, e, _, _, _) when is_jmp i && is_func e = false ->
            dir_sin_j i e :: acc
        | DoubleInstr (p, e, _, _, _) when is_cond_jmp p && is_func e = false ->
            let d1, d2 = dir_dou_j bnl i e in
            d1 :: d2 :: acc
        | DoubleInstr (p, e, l, _, _) when is_cond_jmp p && is_func e = true ->
            print_endline (p_exp e);
            print_endline @@ dec_hex @@ l.loc_addr;
            assert false
        | _ -> dir_fall bnl i :: acc
        | _ -> acc
      in
      Hashtbl.fold
        (fun f bl cfg_f ->
          let bnl : string list = List.map (fun b -> b.bblock_name) bl in
          let cfg_l =
            List.map (fun b -> b.bblock_end_loc) bl
            |> sort_loc |> get_instr_byloc instrs
            |> List.fold_left (aux bnl) []
          in
          (f, cfg_l) :: cfg_f)
        cfg_bdiv_table []

    method print_cfg_graph
        (cfg_t : (string * (string * (control * string option)) list) list) =
      let aux c = match c with F -> "Fall through" | J -> "Jump" in
      List.iter
        (fun (f, v) ->
          let sl =
            List.map
              (fun t ->
                match t with
                | s, (c, Some "T") -> s ^ " --" ^ aux c ^ "--> T\n"
                | s, (c, Some d) -> s ^ " --" ^ aux c ^ "--> " ^ d ^ "\n"
                | s, (c, None) -> s ^ " --" ^ aux c ^ "--> Undefined\n")
              v
          in
          let s = List.fold_left ( ^ ) "" sl in
          print_string f;
          print_string (" :\n" ^ s ^ "\n"))
        cfg_t

    method get_cfg_table instr_list =
      instrs <- instr_list;
      let cfg_t = self#recover_cfg in
      cfg_t
  end
