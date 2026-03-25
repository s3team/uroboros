(* thise module contains the analysis operations, note that we have transform *)
(* before instrumentation and also after instrumentation. *)

module Analysis = struct
  open Cg
  open Cfg
  open Ail_utils
  open Utils
  open Pp_print
  open Type
  open Reassemble_symbol_get
  open Elf_utils

  let analyze_one il fl re (arch : string) =
    let module FnU = Func_utils in
    let cfg = new cfg in
    let cg = new cg in

    cg#set_funcs fl;
    cfg#set_funcs fl;

    let u_fl = FnU.get_userfuncs fl in

    let il' =
      cg#visit il
      |> re#visit_type_infer_analysis []
      |> re#share_lib_processing |> re#adjust_loclabel
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
    (fbl, bbl, cfg_t, cg, il, re, u_fl)

  let post_analyze il re (arch : string) =
    let module EU = Elf_utils in
    let is_static = EU.elf_static () in
    let pp_print_file_arch = pp_print_file arch in
    let pp_print_list_arch = pp_print_list arch is_static in
    re#unify_loc il |> pp_print_list_arch
    |> re#adjust_globallabel @@ global_bss ()
    |> pp_print_file_arch
end
