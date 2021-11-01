open Ail_utils

open Pp_print
open Type
open Parser


class ailVisitor =
    object (self)
        val mutable instrs : instr list = []
        val mutable funcs: func list = []
        val mutable secs: section list = []

        method vinst (i: instr) =
            match i with
            | SingleInstr _ ->
                (* print_string "single instruction\n";  *)
                i
            | DoubleInstr _ ->
                (* print_string "double instruction\n"; *)
                i
            | TripleInstr _ ->
                (* print_string "triple instruction\n"; *)
                i
            | FourInstr _ ->
                (* print_string "four instruction\n"; *)
                i
            | FifInstr _ ->
                i

        method v_exp (e: exp) =
            match e with
            | _ -> e

        method set_funcs (l : func list) =
            funcs <- l

        method set_secs (l : section list) =
            secs <- l

        method visit (instrs: instr list) =
            List.map self#vinst instrs

end
