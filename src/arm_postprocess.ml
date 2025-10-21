(* This module provides postprocessing utilities for ARM binaries.
 * Although there already exists a Python implementation (arm_postprocess.py),
 * this OCaml module was created to handle instruction adjustments directly.

 * This module contains post-symbolization processing functions.
 *)

open Ail_utils
open Type
open Pp_print

module ArmPostprocess = struct
  let adjust_thumb_function_pointer (il : instr list) : instr list =
    (* Insert an ADD instruction after LDR instructions that load function pointers.
     * e.g.,
     * ldr r3,=0x11338
     * add r3, #1
     *
     * e.g.,
     * ldr r3,=S_0x11346
     * add r3, #1
     *)

    (* read "text_sec.info" and parse the below as numbers *)
    (* .text 000110e8 0010e8 004f24 *)
    let text_secs = read_file "text_sec.info" in
    let is_in_text_sec addr =
      List.exists
        (fun line ->
          let items = Str.split (Str.regexp " +") line in
          let sec_start = int_of_string ("0x" ^ List.nth items 1) in
          let sec_size = int_of_string ("0x" ^ List.nth items 3) in
          let sec_end = sec_start + sec_size in
          addr >= sec_start && addr < sec_end)
        text_secs
    in
    let rec aux acc = function
      | [] -> List.rev acc
      | i :: t -> begin
          match i with
          | TripleInstr
              ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
                Const (Point addr),
                Reg (Arm_Reg (Arm_CommonReg reg)),
                loc,
                _,
                _ ) -> begin
              if is_in_text_sec addr then
                let add_instr =
                  TripleInstr
                    ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), None, None),
                      Const (Immediate 1),
                      Reg (Arm_Reg (Arm_CommonReg reg)),
                      loc,
                      None,
                      None )
                in
                aux (add_instr :: i :: acc) t
              else aux (i :: acc) t
            end
          | TripleInstr
              ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
                Label s_label,
                Reg (Arm_Reg (Arm_CommonReg reg)),
                loc,
                _,
                _ ) -> begin
              (* ldr r3,=S_0x11346 *)
              let addr =
                int_of_string (String.sub s_label 3 (String.length s_label - 3))
              in
              if is_in_text_sec addr then
                let add_instr =
                  TripleInstr
                    ( Arm_OP (Arm_CommonOP (Arm_Arithm ADD), None, None),
                      Const (Immediate 1),
                      Reg (Arm_Reg (Arm_CommonReg reg)),
                      loc,
                      None,
                      None )
                in
                aux (add_instr :: i :: acc) t
              else aux (i :: acc) t
            end
          | _ -> aux (i :: acc) t
        end
    in
    aux [] il
end
