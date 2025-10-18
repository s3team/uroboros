(* This module provides postprocessing utilities for ARM binaries.
 * Although there already exists a Python implementation (arm_postprocess.py),
 * this OCaml module was created to migrate as much functionality as possible
 * into the OCaml codebase for better integration, performance, and maintainability.

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
     *)
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
              (* read "text_sec.info" and parse the below as numbers *)
              (* .text 000110e8 0010e8 004f24 *)
              let text_secs = read_file "text_sec.info" in
              let is_in_text_sec =
                List.exists
                  (fun line ->
                    let items = Str.split (Str.regexp " +") line in
                    let sec_start = int_of_string ("0x" ^ List.nth items 1) in
                    let sec_size = int_of_string ("0x" ^ List.nth items 3) in
                    let sec_end = sec_start + sec_size in
                    addr >= sec_start && addr < sec_end)
                  text_secs
              in
              if is_in_text_sec then
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
