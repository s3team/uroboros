open Ail_utils

let reg_op0 = lsl 1 0 in
let reg_op1 = lsl 1 1 in
let reg_op2 = lsl 1 2 in
let reg_op3 = lsl 1 3 in
let reg_op4 = lsl 1 4 in
let reg_op5 = lsl 1 5 in

let reg_op_base = lsl 1 6 in
let reg_op_index = lsl 1 7 in

let def_op_all = reg_op0 || reg_op1 || reg_op2 || reg_op3 ||
                   reg_op4 || reg_op5

let use_op_all = reg_op0 || reg_op1 || reg_op2 || reg_op3 ||
                   reg_op4 || reg_op5 || reg_op_base || reg_op_index

type defEntry = {
    opcode : int;
    op_mask : int;
    reg_mask : bitString;
    reg_mask8 : bitString;
    reg_mask16 : bitString;
    reg_mask32 : bitString;
    reg_mask64 : bitString;
}

(*
 * should possibly using the same struct for both.
 * duplicating just in case if there is a difference in usage
 *)
type defEntry = {
    opcode : int;
    op_mask : int;
    reg_mask : bitString;
    reg_mask8 : bitString;
    reg_mask16 : bitString;
    reg_mask32 : bitString;
    reg_mask64 : bitString;
}

class regProps =

object(self)

  val mutable reg_ : reg_entry = 0
  val mutable num_ : int = 0
  val mutable mask_ : bitString = 0
  val mutable sub_regs_ : bitString = 0
  val mutable parent_regs_ : bitString = 0

  method reg = reg_
  method mask = mask_
  method sub_regs = sub_regs_
  method parent_regs = parent_regs_
  method name = reg_.reg_name
  method num = num_

  method addSubReg bstr = sub_regs_ <- sub_regs_ || bstr

  method addParentReg bstr = parent_regs_ <- parent_regs_ || bstr

end
