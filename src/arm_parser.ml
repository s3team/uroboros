open Batteries

open Ail_utils

open Common_parser
open Type
open Lex_new
open Printf
open Pp_print

type stack_type = Op of op
                | Exp of exp
                | Loc of loc

let condsuff_symb = function
  | "eq" -> EQ | "ne" -> NE | "cs" -> CS | "cc" -> CC | "mi" -> MI | "pl" -> PL
  | "vs" -> VS | "vc" -> VC | "lo" -> LO | "hi" -> HI | "ls" -> LS | "ge" -> GE
  | "lt" -> LT | "gt" -> GT | "le" -> LE | "al" -> AL | "hs" -> HS
  | "<und>" -> UND
  | _ -> raise ParseError

class arm_parse =
  let call_des = ref false in

  let opqualifier_symb = function
    | "w" -> W | "n" -> N | "f32" -> F32 | "f64" -> F64 | "u8" -> U8 | "u16" -> U16
    | "u32" -> U32 | "s8" -> S8 | "s16" -> S16 | "s32" -> S32 | "i16" -> I16| "i8" -> I8
    | "s64" -> S64 | "u64" -> U64
    | "8" -> SIZE 8 | "16" -> SIZE 16 | "32" -> SIZE 32 | "64" -> SIZE 64
    | _ -> raise ParseError

  and commonreg_symb = function
    | "r0" -> R0 | "r1" -> R1 | "r2" -> R2 | "r3" -> R3
    | "r4" -> R4 | "r5" -> R5 | "r6" -> R6 | "r7" -> R7
    | "r8" -> R8 | "r9" -> R9 | "r10" -> R10 | "r11" -> R11
    | "r12" -> R12
    | "w0" -> W0 | "w1" -> W1 | "w2" -> W2 | "w3" -> W3 | "w4" -> W4
    | "w5" -> W5 | "w6" -> W6 | "w7" -> W7 | "w8" -> W8 | "w9" -> W9
    | "w10" -> W10 | "w11" -> W11 | "w12" -> W12 | "w13" -> W13 | "w14" -> W14
    | "w15" -> W15 | "w16" -> W16 | "w17" -> W17 | "w18" -> W18 | "w19" -> W19
    | "w20" -> W20 | "w21" -> W21 | "w22" -> W22 | "w23" -> W23 | "w24" -> W24
    | "w25" -> W25 | "w26" -> W26 | "w27" -> W27 | "w28" -> W28 | "w29" -> W29
    | "w30" -> W30
    | "x0" -> X0 | "x1" -> X1 | "x2" -> X2 | "x3" -> X3 | "x4" -> X4
    | "x5" -> X5 | "x6" -> X6 | "x7" -> X7 | "x8" -> X8 | "x9" -> X9
    | "x10" -> X10 | "x11" -> X11 | "x12" -> X12 | "x13" -> X13 | "x14" -> X14
    | "x15" -> X15 | "x16" -> X16 | "x17" -> X17 | "x18" -> X18 | "x19" -> X19
    | "x20" -> X20 | "x21" -> X21 | "x22" -> X22 | "x23" -> X23 | "x24" -> X24
    | "x25" -> X25 | "x26" -> X26 | "x27" -> X27 | "x28" -> X28
    | _ -> raise ParseError

  and specialreg_symb = function
    | "d0" -> D0 | "d1" -> D1 | "d2" -> D2 | "d3" -> D3
    | "d4" -> D4 | "d5" -> D5 | "d6" -> D6 | "d7" -> D7
    | "d8" -> D8 | "d9" -> D9 | "d10" -> D10 | "d11" -> D11
    | "d12" -> D12 | "d13" -> D13 | "d14" -> D14 | "d15" -> D15
    | "d16" -> D16 | "d17" -> D17 | "d18" -> D18 | "d19" -> D19
    | "d20" -> D20 | "d21" -> D21 | "d22" -> D22 | "d23" -> D23
    | "d24" -> D24 | "d25" -> D25 | "d26" -> D26 | "d27" -> D27
    | "d28" -> D28 | "d29" -> D29 | "d30" -> D30 | "d31" -> D31
    | "s0" -> S0 | "s1" -> S1 | "s2" -> S2 | "s3" -> S3
    | "s4" -> S4 | "s5" -> S5 | "s6" -> S6 | "s7" -> S7
    | "s8" -> S8 | "s9" -> S9 | "s10" -> S10 | "s11" -> S11
    | "s12" -> S12 | "s13" -> S13 | "s14" -> S14 | "s15" -> S15
    | "s16" -> S16 | "s17" -> S17 | "s18" -> S18 | "s19" -> S19
    | "s20" -> S20 | "s21" -> S21 | "s22" -> S22 | "s23" -> S23
    | "s24" -> S24 | "s25" -> S25 | "s26" -> S26 | "s27" -> S27
    | "s28" -> S28 | "s29" -> S29 | "s30" -> S30 | "s31" -> S31
    | "q0" -> Q0 | "q1" -> Q1 | "q2" -> Q2 | "q3" -> Q3
    | "q4" -> Q4 | "q5" -> Q5 | "q6" -> Q6 | "q7" -> Q7
    | "q8" -> Q8 | "q9" -> Q9 | "q10" -> Q10 | "q11" -> Q11
    | "q12" -> Q12 | "q13" -> Q13 | "q14" -> Q14 | "q15" -> Q15
    | "c0" -> C0 | "c1" -> C1 | "c2" -> C2 | "c3" -> C3
    | "c4" -> C4 | "c5" -> C5 | "c6" -> C6 | "c7" -> C7
    | "c8" -> C8 | "c9" -> C9 | "c10" -> C10 | "c11" -> C11
    | "c12" -> C12 | "c13" -> C13 | "c14" -> C14 | "c15" -> C15
    | _ -> raise ParseError

  and stackreg_symb = function
    | "r13" -> R13 | "sp" -> SP | "fp" -> FP | "sb" -> SB | "sl" -> SL
    | "x29" -> X29 | "x31" -> X31
    | _ -> raise ParseError
  and arm_linkreg = function
    | "r14" -> R14 | "lr" -> LR | "ip" -> IP | "x30" -> X30
    | _ -> raise ParseError
  and pcreg_symb = function
    | "r15" -> R15 | "pc" -> PC
    | _ -> raise ParseError in

  let reg_symb r =
    let r' = String.sub r 0 ((String.length r)) in
    try Arm_CommonReg (commonreg_symb r')
    with _ ->
      try Arm_SpecialReg (specialreg_symb r')
      with _ ->
        try Arm_StackReg (stackreg_symb r')
        with _ ->
          try Arm_LinkReg (arm_linkreg r')
          with _ ->
            try Arm_PCReg (pcreg_symb r')
            with _ -> raise ParseError

  and widthsuff_symb s =
    try Arm_Opqualifier (opqualifier_symb s)
    with _ ->
      let s_list = Str.split (Str.regexp_string ".") s in
      let s1 = List.nth s_list 0 in
      let s2 = List.nth s_list 1 in
      try Arm_Double_Opqualifier (opqualifier_symb s1, opqualifier_symb s2)
      with _ -> raise ParseError

  and loc_symb s =
    {loc_label=""; loc_addr=(int_of_string s); loc_visible = true;}

  and const_symb s =
    if s.[0] = '#' then
      let s' = String.sub s 1 ((String.length s)-1) in
        Immediate (int_of_string s')
    else
      failwith "const_symb" in

  let binptr_p_symb s =
    if not (String.contains s ',') then raise ParseError
    else if not (String.contains s '#') then raise ParseError
    else
      let bracket_removed_s =
        (* Check exclamation mark for write-back syntax: *)
        (* LDR r0, [sp,#4]! *)
        if s.[(String.length s)-1] = '!' then
          raise ParseError
        else
          String.sub s 1 ((String.length s)-2)
      in
      let split = Str.split (Str.regexp_string ",") in
      let items = split bracket_removed_s in
      if List.length items <> 2 then raise ParseError;
      let reg = List.nth items 0 in
      let offset_with_hash = List.nth items 1 in
      let offset = String.sub offset_with_hash 1 ((String.length offset_with_hash)-1) in
      (* Handle minus offset in binptr_m_symb function *)
      if offset.[0] = '-' then raise ParseError
      else
        (reg_symb reg, int_of_string offset) in

  let binptr_p_r_symb s =
    if not (String.contains s ',') then raise ParseError
    else
      let bracket_removed_s =
        (* Check exclamation mark for write-back syntax: *)
        (* LDR r0, [sp,#4]! *)
        if s.[(String.length s)-1] = '!' then
          raise ParseError
        else
          String.sub s 1 ((String.length s)-2)
      in
      let split = Str.split (Str.regexp_string ",") in
      let items = split bracket_removed_s in
      if List.length items <> 2 then raise ParseError;
      let first_reg = List.nth items 0 in
      let second_reg = List.nth items 1 in
      (reg_symb first_reg, reg_symb second_reg) in

  let binptr_p_wb_symb s =
    if not (String.contains s ',') then raise ParseError
    else
      let bracket_removed_s =
        (* Exclamation mark should exist: *)
        (* LDR r0, [sp,#4]! *)
        if s.[(String.length s)-1] <> '!' then
          raise ParseError
        else
          String.sub s 1 ((String.length s)-3)
      in
      let split = Str.split (Str.regexp_string ",") in
      let items = split bracket_removed_s in
      if List.length items <> 2 then raise ParseError;
      let reg = List.nth items 0 in
      let offset_with_hash = List.nth items 1 in
      let offset = String.sub offset_with_hash 1 ((String.length offset_with_hash)-1) in
      (* Handle minus offset in binptr_m_symb function *)
      if offset.[0] = '-' then raise ParseError
      else
        (reg_symb reg, int_of_string offset) in

  let binptr_m_symb s =
    if not (String.contains s ',') then raise ParseError
    else
      let bracket_removed_s =
        (* Check exclamation mark for write-back syntax: *)
        (* LDR r0, [sp,#-32]! *)
        if s.[(String.length s)-1] = '!' then
          raise ParseError
        else
          String.sub s 1 ((String.length s)-2)
      in
      let split = Str.split (Str.regexp_string ",") in
      let items = split bracket_removed_s in
      if List.length items <> 2 then raise ParseError;
      let reg = List.nth items 0 in
      let offset_with_hash = List.nth items 1 in
      let offset = String.sub offset_with_hash 1 ((String.length offset_with_hash)-1) in
      if offset.[0] <> '-' then raise ParseError
      else
        let unsigned_offset = String.sub offset 1 ((String.length offset)-1) in
        (reg_symb reg, int_of_string unsigned_offset) in

  let binptr_m_wb_symb s =
    if not (String.contains s ',') then raise ParseError
    else
      let bracket_removed_s =
        (* Exclmation mark should exist: *)
        (* LDR r0, [sp,#-32]! *)
        if s.[(String.length s)-1] <> '!' then
          raise ParseError
        else
          String.sub s 1 ((String.length s)-3)
      in
      let split = Str.split (Str.regexp_string ",") in
      let items = split bracket_removed_s in
      if List.length items <> 2 then raise ParseError;
      let reg = List.nth items 0 in
      let offset_with_hash = List.nth items 1 in
      let offset = String.sub offset_with_hash 1 ((String.length offset_with_hash)-1) in
      if offset.[0] <> '-' then raise ParseError
      else
        let unsigned_offset = String.sub offset 1 ((String.length offset)-1) in
        (reg_symb reg, int_of_string unsigned_offset) in

  let threeptr_s_symb s =
    let bracket_removed_s =
      if s.[(String.length s)-1] = ']' then
        String.sub s 1 ((String.length s)-2)
      else
        raise ParseError in
    let split = Str.split (Str.regexp_string ",") in
    let items = split bracket_removed_s in
    let r1 = reg_symb (List.nth items 0)
    and r2 = reg_symb (List.nth items 1)
    and str = List.nth items 2 in
    (Arm_Reg r1, Arm_Reg r2, str)
  in

  let arithm_symb = function
    | "adc" -> ADC | "adcs" -> ADCS | "add" -> ADD | "adds" -> ADDS | "addw" -> ADDW | "adr" -> ADR | "and" -> AND | "ands" -> ANDS
    | "clz" -> CLZ | "mla" -> MLA | "mls" -> MLS
    | "mul" -> MUL | "muls" -> MULS
    | "neg" -> NEG | "qadd" -> QADD | "qadd16" -> QADD16 | "qadd8" -> QADD8
    | "qasx" -> QASX | "qdadd" -> QDADD | "qdsub" -> QDSUB | "qsax" -> QSAX | "qsub" -> QSUB | "qsub16" -> QSUB16 | "qsub8" -> QSUB8
    | "rsb" -> RSB | "rsbs" -> RSBS
    | "rsc" -> RSC | "rscs" -> RSCS
    | "sadd16" -> SADD16 | "sadd8" -> SADD8 | "sasx" -> SASX | "sbc" -> SBC | "sbcs" -> SBCS
    | "sdiv" -> SDIV | "shadd16" -> SHADD16 | "shadd8" -> SHADD8 | "shasx" -> SHASX | "shsax" -> SHSAX | "shsub16" -> SHSUB16
    | "shsub8" -> SHSUB8 | "smlabb" -> SMLABB | "smlabt" -> SMLABT | "smlatb" -> SMLATB | "smlatt" -> SMLATT
    | "smladx" -> SMLADX | "smlal" -> SMLAL | "smlalbb" -> SMLALBB | "smlalbt" -> SMLALBT | "smlaltb" -> SMLALTB | "smlaltt" -> SMLALTT
    | "smlad" -> SMLAD | "smlawb" -> SMLAWB | "smlawt" -> SMLAWT | "smlsd" -> SMLSD | "smlsdx" -> SMLSDX
    | "smlsld" -> SMLSLD | "smlsldx" -> SMLSLDX | "smmla" -> SMMLA | "smmlar" -> SMMLAR | "smmls" -> SMMLS | "smmlsr" -> SMMLSR
    | "smmul" -> SMMUL | "smmulr" -> SMMULR | "smuad" -> SMUAD | "smuadx" -> SMUADX | "smulbb" -> SMULBB | "smulbt" -> SMULBT
    | "smultb" -> SMULTB | "smultt" -> SMULTT | "smull" -> SMULL | "smulwb" -> SMULWB | "smulwt" -> SMULWT | "smusd" -> SMUSD
    | "smusdx" -> SMUSDX | "ssat" -> SSAT | "ssat16" -> SSAT16 | "ssax" -> SSAX | "ssub16" -> SSUB16 | "ssub8" -> SSUB8
    | "sub" -> SUB | "subs" -> SUBS | "subw" -> SUBW
    | "sxtab" -> SXTAB | "sxtab16" -> SXTAB16
    | "sxth" -> SXTH
    | "sxtah" -> SXTAH
    | "sxtb" -> SXTB
    | "mar" -> MAR | "mra" -> MRA
    | "negs" -> NEGS
    | "sxtb16" -> SXTB16 | "uadd16" -> UADD16 | "uadd8" -> UADD8 | "uasx" -> UASX | "udiv" -> UDIV | "uhadd16" -> UHADD16
    | "uhadd8" -> UHADD8 | "uhasx" -> UHASX | "uhsax" -> UHSAX | "uhsub16" -> UHSUB16 | "uhsub8" -> UHSUB8 | "umaal" -> UMAAL
    | "umlal" -> UMLAL | "umull" -> UMULL | "uqadd16" -> UQADD16 | "uqadd8" -> UQADD8 | "uqasx" -> UQASX | "uqsax" -> UQSAX
    | "uqsub16" -> UQSUB16 | "uqsub8" -> UQSUB8 | "usad8" -> USAD8 | "usada8" -> USADA8 | "usat" -> USAT | "usat16" -> USAT16
    | "usax" -> USAX | "usub16" -> USUB16 | "usub8" -> USUB8 | "uxtab" -> UXTAB | "uxtab16" -> UXTAB16 | "uxtah" -> UXTAH
    | "uxtb" -> UXTB | "uxtb16" -> UXTB16 | "uxth" -> UXTH
    | "vhadd" -> VHADD | "vhsub" -> VHSUB
    | "vaddhn" -> VADDHN
    | "vmul" -> VMUL | "vnmul" -> VNMUL | "vmla" -> VMLA | "vmls" -> VMLS
    | "vnmls" -> VNMLS | "vnmla" -> VNMLA | "vadd" -> VADD | "vsub" -> VSUB | "vdiv" -> VDIV | "vabs" -> VABS | "vneg" -> VNEG
    | "vsqrt" -> VSQRT | "vrhadd" -> VRHADD | "vaddl" -> VADDL | "vraddhn" -> VRADDHN | "vmax" -> VMAX
    | "abs" -> ABS | "addg" -> ADDG | "addpt" -> ADDPT | "adrp" -> ADRP
    | _ -> raise ParseError
  and logicop_symb = function
    | "bic" -> BIC | "bics" -> BICS | "eor" -> EOR | "eors" -> EORS | "orn" -> ORN | "orns" -> ORNS | "orr" -> ORR | "orrs" -> ORRS
    | "pkhbt" -> PKHBT | "pkhtb" -> PKHTB | "rbit" -> RBIT | "rev" -> REV | "rev16" -> REV16 | "revsh" -> REVSH | "sbfx" -> SBFX
    | "ubfx" -> UBFX
    | _ -> raise ParseError
  and rolop_symb = function
    | "asr" -> ASR | "asrs" -> ASRS | "lsl" -> LSL | "lsls" -> LSLS | "lsr" -> LSR | "lsrs" -> LSRS | "ror" -> ROR
    | "rors" -> RORS | "rrx" -> RRX | "rrxs" -> RRXS
    | "vqrshl" -> VQRSHL
    | "vshl" -> VSHL | "vshr" -> VSHR
    | _ -> raise ParseError
  and assignop_symb = function
    | "bfc" -> BFC | "bfi" -> BFI | "cpy" -> CPY | "ldm" -> LDM
    | "stm" -> STM | "stmda" -> STMDA | "stmed" -> STMED
    | "ldmdb" -> LDMDB | "ldmea" -> LDMEA | "ldmia" -> LDMIA | "ldmfd" -> LDMFD
    | "ldr" -> LDR | "ldrb" -> LDRB | "ldrbt" -> LDRBT | "ldrd" -> LDRD | "ldrex" -> LDREX | "ldrexb" -> LDREXB | "ldrexd" -> LDREXD
    | "ldrexh" -> LDREXH | "ldrh" -> LDRH | "ldrht" -> LDRHT | "ldrsb" -> LDRSB | "ldrsbt" -> LDRSBT | "ldrsh" -> LDRSH | "ldrsht" -> LDRSHT
    | "ldrt" -> LDRT | "mov" -> MOV | "movs" -> MOVS | "movw" -> MOVW | "movt" -> MOVT | "mrs" -> MRS | "msr" -> MSR
    | "mvn" -> MVN | "mvns" -> MVNS
    | "stmib" -> STMIB
    | "ldmib" -> LDMIB
    | "sel" -> SEL | "stmdb" -> STMDB | "stmfd" -> STMFD | "stmia" -> STMIA | "stmea" -> STMEA | "str" -> STR | "strb" -> STRB | "strbt" -> STRBT
    | "strd" -> STRD | "strex" -> STREX | "strexb" -> STREXB | "strexd" -> STREXD | "strexh" -> STREXH | "strh" -> STRH | "strht" -> STRHT
    | "strt" -> STRT | "vcvt" -> VCVT | "vcvtt" -> VCVTT | "vcvtr" -> VCVTR | "vcvtb" -> VCVTB | "vmov" -> VMOV | "vmsr" -> VMSR
    | "vstr" -> VSTR | "vst4" -> VST4
    | "vstm" -> VSTM | "vstmdb" -> VSTMDB | "vpush" -> VPUSH | "vldr" -> VLDR | "vldm" -> VLDM | "vldmdb" -> VLDMDB
    | "vld4" -> VLD4 | "vstmia" -> VSTMIA | "vldmia" -> VLDMIA | "vmrs" -> VMRS
    | "stp" -> STP | "ldp" -> LDP
    | "vext" -> VEXT
    | "vrev16" -> VREV16 | "vrev32" -> VREV32|  "vrev64" -> VREV64
    | "vswp" -> VSWP
    | _ -> raise ParseError
  and compareop_symb = function
    | "cmn" -> CMN | "cmp" -> CMP
    | "teq" -> TEQ
    | "tst" -> TST | "vcmpe" -> VCMPE | "vcmp" -> VCMP
    | "cmeq" -> CMEQ
    | _ -> raise ParseError
  and conditionop_symb = function
    | "it" -> IT | "ite" -> ITE | "itt" -> ITT
    | "ittt" -> ITTT | "itte" -> ITTE | "itee" -> ITEE | "itet" -> ITET
    | "itttt" -> ITTTT | "ittte" -> ITTTE | "ittet" -> ITTET | "ittee" -> ITTEE
    | "iteet" -> ITEET | "iteee" -> ITEEE
    | _ -> raise ParseError
  and otherop_symb = function
    | "nop" -> NOP | "hlt" -> HLT | "nopw" -> NOPW | "nopl" -> NOPL | "ud2" -> UD2
    | "setend" -> SETEND | "vsli" -> VSLI
    | _ -> raise ParseError
  and stackop_symb = function
    | "push" -> PUSH | "pop" -> POP | "vpush" -> VPUSH | "vpop" -> VPOP
    | "stmdb" -> STMDB | "ldmia" -> LDMIA
    |  _ -> raise ParseError
  and control_symb = function
    | "b" -> B | "bl" -> BL | "blx" -> BLX | "bx" -> BX | "bxj" -> BXJ | "cbnz" -> CBNZ | "cbz" -> CBZ | "tbb" -> TBB | "tbh" -> TBH
    | "br" -> BR
    | "bxns" -> BXNS | "blxns" -> BLXNS
    | "ret" -> RET | "retaa" -> RETAA | "retab" -> RETAB | "retaasppc" -> RETAASPPC
    | "retabsppc" -> RETABSPPC | "retaasppcr" -> RETAASPPCR | "retabsppcr" -> RETABSPPCR
    | _ -> raise ParseError in

  let controlop_symb = function s ->
    try (control_symb s) with  _ ->
      raise ParseError

  and systemop_symb = function
    | "bkpt" -> BKPT | "clrex" -> CLREX | "cps" -> CPS | "cpsie" -> CPSIE | "cpsid" -> CPSID | "dbg" -> DBG | "dmb" -> DMB
    | "dsb" -> DSB | "isb" -> ISB
    | "pld" -> PLD | "pldw" -> PLDW
    | "pli" -> PLI | "rfe" -> RFE | "sev" -> SEV | "smc" -> SMC | "srs" -> SRS
    | "svc" -> SVC | "wfe" -> WFE | "wfi" -> WFI | "yield" -> YIELD | "udf" -> UDF
    | "mcr" -> MCR | "mcr2" -> MCR2 | "mcrr" -> MCRR | "mcrr2" -> MCRR2
    | "mrc" -> MRC | "mrc2" -> MRC2
    | "setpan" -> SETPAN
    | _ -> raise ParseError in

  let errorop_symb = function
    | "(bad)" -> BAD
    | _ -> failwith "unsupported type in errorop_symb" in

  let commonop_symb (s : string) (is_special : bool) =
    if is_special then
      raise ParseError
    else
      try Arm_Arithm (arithm_symb s)
      with _ ->
        try Arm_Logic (logicop_symb s)
        with _ ->
          try Arm_Rol (rolop_symb s)
          with _ ->
            try Arm_Assign (assignop_symb s)
            with _ ->
              try Arm_Compare (compareop_symb s)
              with _ ->
                try Arm_Other (otherop_symb s)
                with _ -> raise ParseError in

  let op_symb (s : string) (is_special : bool) =
    try Arm_ErrorOP (errorop_symb s)
    with _ ->
      try Arm_CommonOP (commonop_symb s is_special)
      with _ ->
        try Arm_StackOP (stackop_symb s)
        with _ ->
          try Arm_ControlOP (controlop_symb s)
          with _ ->
            try Arm_Condition (conditionop_symb s)
            with _ ->
              try Arm_SystemOP (systemop_symb s)
              with _ -> raise ParseError in

  let op_cond_symb (s : string) (is_special : bool) =
    try (op_symb s is_special, None)
    with _ ->
      try
        (* get last two characters *)
        let len = String.length s in
        if len < 2 then raise ParseError
        else
          if contains s "<und>" then
            let cond_str = String.sub s (len-5) 5 in
            let op_str = String.sub s 0 (len-5) in
            let op = op_symb op_str is_special in
            let cond = condsuff_symb cond_str in
            (op, Some cond)
          else
            let cond_str = String.sub s (len-2) 2 in
            let op_str = String.sub s 0 (len-2) in
            let op = op_symb op_str is_special in
            let cond = condsuff_symb cond_str in
            (op, Some cond)
      with _ -> raise ParseError in

  let unptr_symb s =
    let l = String.length s in
    if s.[l-1] = '!' then raise ParseError
    else
      let s' = String.sub s 1 (l-2) in
      reg_symb s' in

  let unptr_wb_symb s =
    let len = String.length s in
    if s.[len-1] <> '!' then raise ParseError
    else
      let s' = String.sub s 1 (len-3) in
      reg_symb s' in

  let simple_wb_symb s =
    let len = String.length s in
    if s.[len-1] <> '!' then raise ParseError
    else
      let s' = String.sub s 0 (len-1) in
      reg_symb s' in

  object (self) inherit common_parser

  method pp_print l =
    let rec help l =
      match l with
      | h::t -> (printf "item: %s") h; help t
      | [] -> print_string "end\n" in
    help l

  method conptr_symb s =
    let split = Str.split (Str.regexp_string ":") in
    let items = split s in
    let addr = List.nth items 1 in
    let sec' = self#get_sec addr in
    (sec', int_of_string addr)

  method ptraddr_symb s =
    try UnOP (Arm_Reg (unptr_symb s))
    with _ ->
      (* ARM-specific write-back syntax *)
      try UnOP_WB (unptr_wb_symb s)
      with _ ->
        try
          let r, i = binptr_p_symb s in
          BinOP_PLUS (Arm_Reg r, i)
        with _ ->
          try
            let r1, r2 = binptr_p_r_symb s in
            BinOP_PLUS_R (r1, r2)
          with _ ->
            try
              let r, i = binptr_p_wb_symb s in
              (* ARM-specific write-back syntax *)
              BinOP_PLUS_WB (r, i)
            with _ ->
              try
                let r, i = binptr_m_symb s in
                BinOP_MINUS (Arm_Reg r, i)
              with _ ->
                try
                  let r, i = binptr_m_wb_symb s in
                  BinOP_MINUS_WB (r, i)
                with _ ->
                  try ThreeOP_S (threeptr_s_symb s)
                  with _ ->
                    raise ParseError

  method ptr_symb s =
    let has s1 s2 =
      let re = Str.regexp_string s2 in
      try ignore (Str.search_forward re s1 0); true
      with Not_found -> false in
    if ((has s "[")&&(has s "]")) then  (* then it should be ptr *)
      self#ptraddr_symb s
    else if ((String.length s) > 2) && (s.[(String.length s)-1] = '!') then
      (* ldmia r5!, {r0, r1, r2, r3}
       * treat is as a pointer until other cases are found *)
      WB (simple_wb_symb s)
    else raise ParseError

  (* this might be another approach, currently abandoned*)
  method jumpdes_symb_bak s =
    let split = Str.split (Str.regexp " +") in
    let s1 = List.nth (split s) 1 in
    let s' = String.trim s1
    and split = Str.split (Str.regexp_string "+") in
    let items = split s' in
    let offset = List.nth items 1
    and func = List.nth items 0 in
    let func' = String.sub func 1 ((String.length func)-1)
    and offset' = String.sub offset 0 ((String.length offset)-1) in
    let f = self#get_func func' false in
    (f, int_of_string offset')

  method jumpdes_symb s =
    (* giyeol: *)
    (* let _ = Printf.printf "jumpdes_symb: %s\n" s in *)
    if String.contains s '+' then
      let split = Str.split (Str.regexp " +") in
      let s1 = List.nth (split s) 0 in
      JumpDes (int_of_string ("0x"^s1))
    else if String.contains s '-' then
      let split = Str.split (Str.regexp " +") in
      let s1 = List.nth (split s) 0 in
      JumpDes (int_of_string ("0x"^s1))
    else  (* jmp deadbeef <func@plt> *)
      CallDes (self#calldes_symb s)

  method calldes_symb (s: string) : func =
    let split = Str.split (Str.regexp " +") in
    let s1 = List.nth (split s) 1 in
    let s' = String.trim s1 in
    (* call   80484cb <__libc_start_main@plt+0x1ab> *)
    if String.contains s' '+' then
      let func = List.nth (split s) 0 in
      let func' = "S_0x"^(String.uppercase_ascii func) in
      let f = self#get_func func' false in
      f
    else if String.contains s' '-' then
      let func = List.nth (split s) 0 in
      let func' = "S_0x"^(String.uppercase_ascii func) in
      let f = self#get_func func' false in
      f
    else if String.contains s' '@' then
      let split = Str.split (Str.regexp_string "@") in
      let items = split s' in
      let item = List.nth items 0 in
      let func = String.sub item 1 ((String.length item)-1) in
      let f = self#get_func func true in
      f
    else
      let func = String.sub s' 1 ((String.length s')-2) in
      let f = self#get_func func true in
      f

  method star_jmptable_symb s =
    if String.contains s '*' then
      let split = Str.split (Str.regexp_string ",") in
      let tokens = split s in
      let addr = List.nth tokens 0
      and r = List.nth tokens 1
      and off = List.nth tokens 2 in
      let addr' = String.sub addr 1 ((String.length addr)-2) in
      let off' = String.sub off 0 ((String.length off)-1) in
      (int_of_string addr', reg_symb r, int_of_string off')
    else raise ParseError

  method jmptable_m_symb (s: string) =
    if String.exists s "(," then
      let split = Str.split (Str.regexp_string ",") in
      let tokens = split s in
      let addr = List.nth tokens 0 in
      if addr.[0] <>'-' then raise ParseError
      else
        let r = List.nth tokens 1
        and off = List.nth tokens 2 in
        let addr' = String.sub addr 1 ((String.length addr)-2) in
        let off' = String.sub off 0 ((String.length off)-1) in
        (int_of_string addr', reg_symb r, int_of_string off')
    else raise ParseError

  method symbol_symb s =
    let s' = String.trim s in
    if String.contains s '#' then
      raise ParseError
    else if !call_des = true then
      (
        CallDes (self#calldes_symb s')
      )
      (* else if String.contains s '+' then *)
    else
      (self#jumpdes_symb s')

  method is_inline_shift (s : string) =
    let s' = String.trim s in
    let s_list = Str.split (Str.regexp "\\^") s' in
    let opcode = List.hd s_list in
    try
      let _ = rolop_symb opcode in
      true
    with _ ->
      false

  method exp_symb s =
    (* To prevent the following instruction to be classified as a Symbol,
     * check if an instruction contains an inline shift:
     * andeq r0,r0,ip,lsl r0 *)
    if self#is_inline_shift s then
      Label (s)
    else
      try Ptr (self#ptr_symb s)
      with _ ->
        try Reg (Arm_Reg (reg_symb s))
        with _ ->
          try Const (const_symb s)
          with _ ->
            try Symbol (self#symbol_symb s)
            with _ ->
              try Label (s)   (* we just consider these as labels *)
              with _ ->
                raise ParseError

  (** [is_special] is used to make stmdb and ldmia opcodes parsed to stackop *)
  method push_stack lex (is_special : bool) =
    match lex with
    | Lop s ->
      let str_list = Str.split (Str.regexp_string ".") in
      if contains s "undefined" then
        Op (Undefined_OP)
      else
        if (List.length (str_list s)) = 2 then
        let op_str = List.nth (str_list s) 0 in
        let widthsuff_str = List.nth (str_list s) 1 in
        let widthsuff = widthsuff_symb widthsuff_str in
        let op, cond = op_cond_symb op_str is_special in
        Op (Arm_OP (op, cond, Some (widthsuff)))
      else if (List.length (str_list s)) = 3 then
        let op_str = List.nth (str_list s) 0 in
        let qualifier_str =
          (List.nth (str_list s) 1) ^ "." ^ (List.nth (str_list s) 2)
        in
        let op, cond = op_cond_symb op_str is_special in
        let suffix = widthsuff_symb qualifier_str in
        Op (Arm_OP (op, cond, Some (suffix)))
      else
        let op, cond = op_cond_symb s is_special in
        Op (Arm_OP (op, cond, None))
    | Lexp s -> Exp (self#exp_symb s)
    | Lloc s -> Loc (loc_symb s)
    | _ -> raise ParseError

  method reduce_stack stack pre (tag : tag option) =
    match stack with
    | (Loc l)::(Op p)::[] -> SingleInstr (p, l, pre, tag, Hashtbl.create 0)
    | (Loc l)::(Exp exp1)::(Op p)::[] -> DoubleInstr(p, exp1, l, pre, tag, Hashtbl.create 0)
    | (Loc l)::(Exp exp1)::(Exp exp2)::(Op p)::[] -> TripleInstr(p, exp1, exp2, l, pre, tag, Hashtbl.create 0)
    | (Loc l)::(Exp exp1)::(Exp exp2)::(Exp exp3)::(Op p)::[] -> FourInstr(p, exp1, exp2, exp3, l, pre, tag, Hashtbl.create 0)
    | (Loc l)::(Exp exp1)::(Exp exp2)::(Exp exp3)::(Exp exp4)::(Op p)::[] -> FifInstr(p, exp1, exp2, exp3, exp4, l, pre, tag, Hashtbl.create 0)
    | _ -> raise ParseError

  method print_f (fl : func list) =
    List.iter (
      fun f ->
        print_string f.func_name;
        print_string " ";
        print_string (dec_hex f.func_begin_addr);
        print_string " ";
        print_string (dec_hex f.func_end_addr);
        print_string "\n" ) fl;
    fl

  method set_funclist l =
    func_list <- l

  method get_funclist =
    func_list

  method set_seclist l =
    sec_list <- l

  method get_func name lib =
    let rec help l name =
      match l with
      | h::t -> if h.func_name = name then h else help t name
      | [] ->
        let func' : func =
          {func_name=name; func_begin_addr=0; func_end_addr = 0; is_lib=lib;} in
        func_list <- func'::func_list;
        func' in
    help func_list name

  method get_sec addr =
    let rec help l addr =
      match l with
      | h::t -> (
          let b = h.sec_begin_addr in
          let e = b + h.sec_size in
          if (addr>=b && addr < e)  then
            h
          else  help t addr)
      | [] -> print_string "error in get_sec"; raise ParseError in
    help sec_list (int_of_string addr)

  method init_process =
    call_des := false

  method parse_instr instr loc (arch : string) =
    (* giyeol: *)
    (* let _ = Printf.printf "parse_instr: %s\n" instr in *)
    self#init_process;
    let compact (instr : string) =
      (* See [arm_postprocess.py#remove_caret] *)
      let instr' = Str.global_replace (Str.regexp ", ") "," instr in
      if contains instr' " " then
        let instr' = Str.global_replace (Str.regexp "lsl ") "lsl^" instr' in
        instr'
      else instr'
    in
    let compact_instr = compact instr in
    let pre = prefix_identify compact_instr in
    (* let (compact_instr, tag) = tag_identify compact_instr in *)
    let compact_instr' = prefix_sub compact_instr in
    let is_special_stackop (i : string) =
      if contains i "stmdb" && contains i "sp!" then
        true
      else if contains i "ldmia" && contains i "sp!" then
        true
      else false
    in
    let is_special = is_special_stackop compact_instr' in
    let lexem_list = Array.to_list (lexer compact_instr' loc arch) in
    let s : stack_type list = [] in
    let parse_one s l =
      match l with
      | Lend -> s
      | _ -> (self#push_stack l is_special)::s
    in
  let stack = List.fold_left parse_one s lexem_list in
  let parsed_instr = self#reduce_stack stack pre None in
  parsed_instr

end
