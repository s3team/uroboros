open Type

exception ParseError

class virtual common_parser = object
  val mutable func_list : func list = []
  val mutable sec_list : section list = []

  method virtual pp_print : string list -> unit
  method virtual print_f : func list -> func list
  method virtual set_funclist : func list -> unit
  method virtual get_funclist : func list
  method virtual set_seclist : section list -> unit
  method virtual get_func : string -> bool -> func
  method virtual get_sec : string -> section
  method virtual init_process : unit
  method virtual parse_instr : string -> string -> string -> instr
end
