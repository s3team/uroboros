open Batteries

open Ail_utils

open Type
open Parser
open Pp_print
open Func_slicer

class ailParser =
object (self)
  val mutable instrs : instr list = []
  val mutable funcs : func list = []
  val mutable secs: section list = []

  method set_funcs (l : func list) =
    funcs <- l

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

  method func_slicing =
    (* self#print_f funcs; *)
    let fs = new func_slicer instrs funcs in
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

  method processInstrs (l : string list) =
    let cat_tail s =
      match s with
      | [] -> "" (* this will never happen*)
      | h::t -> String.trim (String.concat ":" t) in
    let p = new parse in
    let _ = p#set_funclist(funcs)
    and _ = p#set_seclist(secs)
    and split = Str.split (Str.regexp_string ":") in
    let l' = List.filter (
                 fun i ->
                 let items = split i in
                 let len = List.length items in
                 len > 1 ) l in
    let help i =
      let items = split i in
      let loc = List.nth items 0 in
      let instr = cat_tail items
      in
      instrs <- (p#parse_instr instr loc)::instrs
    in
    List.iter help l';
    funcs <- p#get_funclist

  method p_instrs =
    List.iter (fun i -> let is = pp_print_instr i in print_string is) instrs

  method get_instrs =
    List.rev instrs

  method get_instrs_len =
    List.length instrs

end
