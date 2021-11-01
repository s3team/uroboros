(* thise module contains the analysis operations, note that we have transform *)
(* before instrumentation and also after instrumentation. *)

module Analysis = struct


    open Cg
    open Cfg
    open Ail_utils
    open Pp_print
    open Type
    open Reassemble_symbol_get

    let global_bss () =
      let filelines = read_file "globalbss.info"
      and help acc l =
        let items = Str.split (Str.regexp " +") l in
        let t = List.nth items 0 in
        let addr = String.sub t 1 ((String.length t)-1) in
        let addr' = String.uppercase addr
        and value = String.trim (List.nth items 2)  in
        let rtype = String.trim (List.nth items 1) in 
        (addr', value, rtype)::acc
      in
      List.fold_left help [] filelines

    let get_userfuncs funcs =
      List.filter (fun f -> f.is_lib=false) funcs

    let analyze_one il fl re =
      let cfg = new cfg in
      let cg = new cg in

      cg#set_funcs fl;
      cfg#set_funcs fl;

      let u_fl = get_userfuncs fl in
      print_endline "user defined func number ";
      print_int (List.length u_fl);
      print_string "\n";
       let il' =
         cg#visit il
         |> re#visit_type_infer_analysis []
         |> re#share_lib_processing
         |> re#adjust_loclabel
       in

      re#reassemble_dump u_fl;

      let il' =
        re#adjust_jmpref il'
        (* |> re#add_func_label u_fl *)
        |> cfg#visit
      in

      let fbl = cfg#get_fbl in
      let bbl = cfg#get_bbl in
      let cfg_t = cfg#get_cfg_table il' in
      let cg = cg#get_cg_table in
      let il = re#add_bblock_label bbl il' in
      (fbl, bbl, cfg_t, cg, il, re)


    let post_analyze il re =
     ( re#unify_loc il
       |> pp_print_list
       |> re#adjust_globallabel @@ global_bss ()
       |> pp_print_file);

  end
