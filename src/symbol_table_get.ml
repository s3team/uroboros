open Ail_utils
open Type
open Pp_print

let plt_map = Hashtbl.create 100
let rela_plt_map = Hashtbl.create 100
let func_sym_map = Hashtbl.create 100
let load_ranges = Hashtbl.create 100  (* ranges of LOAD from readelf -l *)
let file_bytes = ref (Bytes.create 0)

let strip_symbol_prefix (s : string) : string =
  let len = String.length s in
  assert (len > 2);
  String.sub s 2 (len - 2)

let strip_leading_star (s : string) : string =
  if String.starts_with ~prefix:"*" s then
    String.sub s 1 (String.length s - 1)
  else s

let strip_trailing_colon (s : string) : string =
  if String.ends_with ~suffix:":" s then
    String.sub s 0 (String.length s - 1)
  else s

let replace_at (c : char) : char =
  (* assembler rejects label containing @ *)
  match c with
  | '@' -> '_'
  | _ -> c

let read_file_bytes (filename : string) : bytes =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  buf

let read_addr_bytes buf offset =
  let b0 = Bytes.get buf (offset)     |> Char.code in
  let b1 = Bytes.get buf (offset + 1) |> Char.code in
  let b2 = Bytes.get buf (offset + 2) |> Char.code in
  let b3 = Bytes.get buf (offset + 3) |> Char.code in
  let res = Int32.logor
    (Int32.of_int b0)
    (Int32.logor
        (Int32.shift_left (Int32.of_int b1) 8)
        (Int32.logor
          (Int32.shift_left (Int32.of_int b2) 16)
          (Int32.shift_left (Int32.of_int b3) 24)))
  in
  Int32.to_int res

let plt_mapping () =
  let module EU = ELF_utils in
  let filelines = read_lines "plt_whole.info" in
  if EU.elf_64 () then
    let lines_key = List.filter (fun l -> contains ~str:l ~sub:"endbr") filelines
    and lines_value =
      List.filter
        (fun l -> contains ~str:l ~sub:"_GLOBAL_OFFSET_TABLE_") filelines
    in
    List.iter2 (
      fun kl vl ->
        let k_lst = Str.split (Str.regexp "[ \t]+") kl in
        let k = List.nth k_lst 0 |> strip_trailing_colon in
        let v_lst = Str.split (Str.regexp "[ \t]+") vl in
        let v_idx = (List.length v_lst) - 2 in
        let v = List.nth v_lst v_idx in
        Hashtbl.add plt_map (int_of_string ("0x"^k)) (int_of_string ("0x"^v))
    ) lines_key lines_value
  else
    let lines = List.filter (fun l -> contains ~str:l ~sub:"jmp") filelines in
    List.iter (
    fun l ->
      let items = Str.split (Str.regexp "[ \t]+") l in
      let k = List.nth items 0 |> strip_trailing_colon in
      let v_idx = (List.length items) - 1 in  (* last element *)
      let v = List.nth items v_idx |> strip_leading_star in
      Hashtbl.add plt_map (int_of_string ("0x"^k)) (int_of_string v)
    ) lines

let rela_plt_mapping () =
  let filelines = read_lines "rela_plt.info" in
  let filelines' = List.filter (
    fun l -> contains ~str:l ~sub:"IRELATIV"
  ) filelines in
  List.iter (
    fun l ->
      let items = Str.split (Str.regexp "[ \t]+") l in
      let k = List.nth items 0 in
      let v = List.nth items 3 in
      Hashtbl.add rela_plt_map (int_of_string ("0x"^k)) (int_of_string ("0x"^v))
  ) filelines'

let addr_file_bytes_mapping () =
  let filelines = read_lines "headers.info" in
  let filelines' =
    List.filter (fun l -> contains ~str:l ~sub:"LOAD") filelines
  in
  List.iter (
    fun l ->
      let items = Str.split (Str.regexp "[ \t]+") l in
      let f_offset = int_of_string (List.nth items 1) in
      let v_addr = int_of_string (List.nth items 2) in
      let v_addr_sz = int_of_string (List.nth items 5) in
      Hashtbl.add load_ranges (v_addr, v_addr+v_addr_sz) f_offset
  ) filelines'

let func_sym_mapping () =
  let filelines = read_lines "nm.info" in
  let filelines' =
    List.filter (fun l -> contains ~str:l ~sub:" i ") filelines
  in
  List.iter (
    fun l ->
      let items = Str.split (Str.regexp "[ \t]+") l in
      let k = List.nth items 0 in
      let v = List.nth items 2 in
      Hashtbl.add func_sym_map (int_of_string ("0x"^k)) (v)
  ) filelines'

let parse_nm () : (int, string * string) Hashtbl.t =
  let sym_addr2label = Hashtbl.create 100 in
  let seen = Hashtbl.create 100 in
  let filelines = read_lines "nm.info" in
  List.iter (
    fun l ->
      if not (contains ~str:l ~sub:" U ")
        && not (contains ~str:l ~sub:" i ")
        && not (contains ~str:l ~sub:" w ")
        && not (contains ~str:l ~sub:" v ")
        && not (contains ~str:l ~sub:" V ")
      then
        let items = Str.split (Str.regexp " +") l in
        let addr = List.nth items 0 in
        let symbol_type = List.nth items 1 in
        let symbol_name = List.nth items 2 in
        let symbol_name' =
          symbol_name |> String.to_seq
                      |> Seq.map replace_at
                      |> String.of_seq
        in
        let addr' = int_of_string ("0x"^addr) in
        if symbol_name' <> "main" then
          if not (Hashtbl.mem seen symbol_name') then
            (Hashtbl.add sym_addr2label addr' (symbol_name', symbol_type);
            Hashtbl.add seen symbol_name' "")
  ) filelines;
  sym_addr2label

let is_call (i : instr) : bool =
  let op = get_op i in
  match op with
  | Intel_OP io ->
    begin
      match io with
      | Intel_ControlOP ico ->
        begin
          match ico with
          | CALL -> true
          | _ -> false
        end
      | _ -> false
    end
  | Arm_OP (ao, _) ->
    begin
      match ao with
      | Arm_ControlOP _ -> true
      | _ -> false
    end
  | _ -> false

let update_loc (i : instr) (new_loc : loc) : instr =
  match i with
  | SingleInstr (op, loc, prefix_op, tags) ->
    SingleInstr (op, new_loc, prefix_op, tags)
  | DoubleInstr (op, exp, loc, prefix_op, tags) ->
    DoubleInstr (op, exp, new_loc, prefix_op, tags)
  | TripleInstr (op, exp1, exp2, loc, prefix_op, tags) ->
    TripleInstr (op, exp1, exp2, new_loc, prefix_op, tags)
  | FourInstr (op, exp1, exp2, exp3, loc, prefix_op, tags) ->
    FourInstr (op, exp1, exp2, exp3, new_loc, prefix_op, tags)
  | FifInstr (op, exp1, exp2, exp3, exp4, loc, prefix_op, tags) ->
    FifInstr (op, exp1, exp2, exp3, exp4, new_loc, prefix_op, tags)

let update_fname2css
    (fname2css : (string, int list) Hashtbl.t)
    (fname : string)
    (addr : int)
  : unit =
  match Hashtbl.find_opt fname2css fname with
  | Some css ->
    if not (List.mem addr css) then
      Hashtbl.replace fname2css fname (addr::css)
  | None ->
    Hashtbl.add fname2css fname [addr]

let apply
    (il : instr list)
    (ufunc : func list)
    (f : string)
  : instr list * func list * (string, int list) Hashtbl.t =
  let module EU = ELF_utils in
  if EU.elf_static () && EU.elf_unstrip () then
    if EU.elf_64 () then
      ( plt_mapping ();
        rela_plt_mapping ();
        func_sym_mapping () )
    else
      ( plt_mapping ();
        file_bytes := read_file_bytes f;
        addr_file_bytes_mapping ();
        func_sym_mapping () );
  let fname2css = Hashtbl.create 100 in  (* function name to callsites list *)
  let sym_addr2label = parse_nm () in
  let rec add_existing_symbols il acc =
    match il with
    | [] -> List.rev acc
    | i::rest ->
      let i' =
        let i_loc = get_loc i in
        let i_addr = i_loc.loc_addr in
        let i_label = i_loc.loc_label in
        match Hashtbl.find_opt sym_addr2label i_addr with
        | Some (sym_name, sym_type) ->
          update_loc i {i_loc with loc_label = sym_name^":\n"^i_label}
        | None -> i
      in
      match i' with
      | DoubleInstr 
          (Intel_OP (Intel_ControlOP CALL), Label func_name, loc, prefix_op, tags) ->
        begin
          let func_name' = strip_symbol_prefix func_name |> int_of_string in
          match Hashtbl.find_opt sym_addr2label func_name' with
          | Some (sym_name, sym_type) ->
            let call' =
              DoubleInstr (Intel_OP (Intel_ControlOP CALL), Label sym_name, loc, prefix_op, tags)
            in
            let new_acc = call'::acc in
            ( update_fname2css fname2css sym_name loc.loc_addr;
              add_existing_symbols rest new_acc )
          | None -> begin
            update_fname2css fname2css func_name loc.loc_addr;
            add_existing_symbols rest (i' :: acc)
          end
        end
      | DoubleInstr
          ( Intel_OP (Intel_ControlOP CALL),
            Symbol (CallDes {func_name; func_begin_addr; func_end_addr; is_lib}),
            loc,
            prefix_op,
            tags ) ->
            ( update_fname2css fname2css func_name loc.loc_addr;
              add_existing_symbols rest (i' :: acc) )
      | DoubleInstr
          ( Intel_OP (Intel_ControlOP CALL),
            Const (Point call_addr),
            loc,
            prefix_op,
            tags ) ->
          begin
            if EU.elf_64 () then
              ( match Hashtbl.find_opt plt_map call_addr with
                | Some resolve_addr -> begin
                    match Hashtbl.find_opt rela_plt_map resolve_addr with
                    | Some func_addr -> begin
                        match Hashtbl.find_opt func_sym_map func_addr with
                        | Some func_name ->
                          let call' = DoubleInstr
                            ( Intel_OP (Intel_ControlOP CALL),
                              Label (func_name^"@PLT"),
                              loc,
                              prefix_op,
                              tags )
                          in
                          let new_acc = call' :: acc in
                          ( update_fname2css fname2css func_name loc.loc_addr;
                            add_existing_symbols rest new_acc )
                        | None -> add_existing_symbols rest (i' :: acc)
                      end
                    | None -> add_existing_symbols rest (i' :: acc)
                  end
                | None -> add_existing_symbols rest (i' :: acc) )
            else
              (
                match Hashtbl.find_opt plt_map call_addr with
                | Some resolve_addr ->
                  begin
                    let f_offset =
                      Hashtbl.fold (
                        fun (l_start_addr, l_end_addr) l_f_offset acc ->
                          match acc with
                          | Some _ -> acc
                          | None ->
                            if resolve_addr >= l_start_addr && resolve_addr < l_end_addr then
                              let f_addr = l_f_offset + resolve_addr - l_start_addr in
                              Some f_addr
                            else None
                      ) load_ranges None
                    in
                    let f_addr = Option.value f_offset ~default:0 in
                    let func_addr = read_addr_bytes !file_bytes f_addr in
                    ( match Hashtbl.find_opt func_sym_map func_addr with
                    | Some func_name ->
                      let call' = DoubleInstr
                        ( Intel_OP (Intel_ControlOP CALL),
                          Label (func_name^"@PLT"),
                          loc,
                          prefix_op,
                          tags )
                      in
                      let new_acc = call' :: acc in
                      ( update_fname2css fname2css func_name loc.loc_addr;
                        add_existing_symbols rest new_acc )
                    | None -> add_existing_symbols rest (i' :: acc) )
                  end
                | None -> begin
                  add_existing_symbols rest (i' :: acc)
                end
              )
          end
      | DoubleInstr
          ( Intel_OP (Intel_ControlOP CALL),
            _,
            loc,
            prefix_op,
            tags ) -> begin
              add_existing_symbols rest (i' :: acc)
            end
      | _ -> add_existing_symbols rest (i' :: acc)
  in
  let il' = add_existing_symbols il [] in
  (* update function list with function symbols *)
  let ufunc = 
    List.filter (fun f -> String.starts_with ~prefix:"S_" f.func_name) ufunc in
  let ufunc' = List.map (
    fun f ->
      let f_name = f.func_name |> strip_symbol_prefix |> int_of_string in
      match Hashtbl.find_opt sym_addr2label f_name with
      | Some (sym_name, sym_type) ->
        {f with func_name = sym_name}
      | None -> f
  ) ufunc in
  il', ufunc', fname2css