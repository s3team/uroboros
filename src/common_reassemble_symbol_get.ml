open Visit
open Type

exception Reassemble_Error of string;;

type ft = {fn : string; fbaddr : int; feaddr : int}

class virtual common_reassemble =
  object (self)
    inherit ailVisitor

    method virtual visit_heuristic_analysis : instr list -> instr list
    method virtual adjust_loclabel : instr list -> instr list
    method virtual adjust_jmpref : instr list -> instr list
    method virtual add_func_label : func list -> instr list -> instr list
    method virtual share_lib_processing : instr list -> instr list
    method virtual add_bblock_label : bblock list -> instr list -> instr list
    method virtual reassemble_dump : func list -> unit
    method virtual unify_loc : instr list -> instr list
    method virtual visit_type_infer_analysis : bblock list -> instr list -> instr list
    method virtual adjust_globallabel : (string * string * string) list -> string list -> string list
  end
