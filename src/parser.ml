open Batteries

open Ail_utils

open Lex_new
open Type
open Printf

exception ParseError

type stack_type = Op of op
                | Exp of exp
                | Loc of loc

class parse =

  let call_des = ref false in

  let commonreg_symb = function
    | "rax" -> RAX | "rbx" -> RBX | "rcx" -> RCX
    | "rdx" -> RDX | "rdi" -> RDI | "rsi" -> RSI
    | "eax" -> EAX | "ebx" -> EBX | "ecx" -> ECX
    | "edx" -> EDX | "edi" -> EDI | "esi" -> ESI
    | "ax" -> AX | "bx" -> BX | "cx" -> CX
    | "dx" -> DX | "al" -> AL | "bl" -> BL
    | "cl" -> CL | "dl" -> DL | "ah" -> AH
    | "bh" -> BH | "ch" -> CH | "dh" -> DH
    | _ -> raise ParseError

  and specialreg_symb = function
    | "r8" -> R8 | "r9" -> R9 | "r10" -> R10
    | "r11" -> R11 | "r12" -> R12 | "r13" -> R13
    | "r14" -> R14 | "r15" -> R15 | "r8d" -> R8D
    | "r9d" -> R9D | "r10d" -> R10D | "r11d" -> R11D
    | "r12d" -> R12D | "r13d" -> R13D | "r14d" -> R14D
    | "r15d" -> R15D | "r8w" -> R8W | "r9w" -> R9W
    | "r10w" -> R10W | "r11w" -> R11W | "r12w" -> R12W
    | "r13w" -> R13W | "r14w" -> R14W | "r15w" -> R15W
    | "r8b" -> R8B | "r9b" -> R9B | "r10b" -> R10B
    | "r11b" -> R11B | "r12b" -> R12B | "r13b" -> R13B
    | "r14b" -> R14B | "r15b" -> R15B
    | "xmm0" -> XMM0 | "xmm1" -> XMM1 | "xmm2" -> XMM2
    | "xmm3" -> XMM3 | "xmm4" -> XMM4 | "xmm5" -> XMM5
    | "xmm6" -> XMM6 | "xmm7" -> XMM7
    | "ymm0" -> YMM0 | "ymm1" -> YMM1 | "ymm2" -> YMM2
    | "ymm3" -> YMM3 | "ymm4" -> YMM4 | "ymm5" -> YMM5
    | "ymm6" -> YMM6 | "ymm7" -> YMM7
    | _ -> raise ParseError

  and stackreg_symb = function
    | "rsp" -> RSP | "rbp" -> RBP
    | "esp" -> ESP | "ebp" -> EBP
    | _ -> raise ParseError
  and otherreg_symb = function
    | "eiz" -> EIZ
    | "eiz*1" -> EIZ
    | _ -> raise ParseError
  and pcreg_symb = function
    | "eip" -> EIP
    | "rip" -> RIP
    | _ -> raise ParseError in

  let reg_symb r =
    let r' = String.sub r 1 ((String.length r)-1) in
    try CommonReg (commonreg_symb r')
    with _ ->
      try SpecialReg (specialreg_symb r')
      with _ ->
        try StackReg (stackreg_symb r')
        with _ ->
          try PCReg (pcreg_symb r')
          with _ ->
            try OtherReg(otherreg_symb r')
            with _ -> raise ParseError

  and loc_symb s =
    {loc_label=""; loc_addr=(int_of_string s); loc_visible = true;}

  and const_symb s =
    if s.[0] = '$' then
      let s' = String.sub s 1 ((String.length s)-1) in
      Normal (int_of_string s')
    else
      Point (int_of_string s) in

  (*
and ptrtyp_symb = function
    | "QWORD" -> QWORD | "DWORD" -> DWORD
    | "WORD" -> WORD | "BYTE" -> BYTE | "TBYTE" -> BYTE
    | _ -> raise ParseError


and mathsymb_op = function
    | "+" -> MATHADD | "-" -> MATHSUB
    | _ -> raise ParseError in
   *)

  let binptr_p_symb s =
    if String.contains s ',' then raise ParseError
    else let split = Str.split (Str.regexp_string "(") in
      let items = split s in
      let offset = List.nth items 0 in
      let s' = List.nth items 1 in
      (*let len = String.length s' in *)
      let r = String.sub s' 0 ((String.length s')-1) in
      (reg_symb r, int_of_string offset) in

  let binptr_m_symb s =
    if String.contains s ',' then raise ParseError
    else let split = Str.split (Str.regexp_string "(") in
      let items = split s in
      let offset = List.nth items 0 in
      if offset.[0] <>'-' then raise ParseError
      else
        let len = String.length offset in
        let offset' = String.sub offset 1 (len-1)
        and s' = List.nth items 1 in
        let r = String.sub s' 0 ((String.length s')-1) in
        (reg_symb r, int_of_string offset')

  (*
and triptr_symb_1 = function s ->
  let op = if String.contains s '+' then "+" else "-"
  and len = String.length s in
  let s' = String.sub s 1 (len-2) in
  let split = Str.split (Str.regexp_string op)
  and split' = Str.split (Str.regexp_string "*") in
    let items = split s' in
      let item = List.nth items 0
      and addr = List.nth items 1 in
        let items' = split' item in
          let reg = List.nth items' 0
          and size = List.nth items' 1 in
       (reg_symb reg, int_of_string size, int_of_string addr, mathsymb_op op)

(* mov eax,DWORD PTR [ebx+eax*4] *)
and triptr_symb_2 = function s ->
  let op = if String.contains s '+' then "+" else "-"
  and len = String.length s in
  let s' = String.sub s 1 (len-2) in
  let split = Str.split (Str.regexp_string op)
  and split' = Str.split (Str.regexp_string "*") in
    let items = split s' in
      let reg1 = List.nth items 0
      and item = List.nth items 1 in
        let items' = split' item in
          let reg2 = List.nth items' 0
          and size = List.nth items' 1 in
       (reg_symb reg1, reg_symb reg2, int_of_string size, mathsymb_op op)
   *)

  and threeptr_symb s =
    let bc = String.get s 0 in
    let l = String.length s in
    let ec = String.get s (l-1) in
    if bc = '(' && ec = ')' then
      begin
        let s' = String.sub s 1 (l-2) in
        let split = Str.split (Str.regexp_string ",") in
        let items = split s' in
        if List.length items <> 3 then
          raise ParseError
        else
          begin
            let r1 = List.nth items 0
            and r2 = List.nth items 1
            and off = List.nth items 2 in
            (reg_symb r1, reg_symb r2, int_of_string off)
          end
      end
    else
      raise ParseError


  and fourptr_p_symb s =
    let split = Str.split (Str.regexp_string "(")
    and split' = Str.split (Str.regexp_string ",") in
    let items = split s in
    let offset = List.nth items 0
    and s' = List.nth items 1 in
    let l = String.length s' in
    let ss = String.sub s' 0 (l-1) in
    let items' = split' ss in
    if List.length items' <> 3 then raise ParseError
    else
      let r1 = List.nth items' 0
      and r2 = List.nth items' 1
      and off = List.nth items' 2 in
      (reg_symb r1, reg_symb r2, int_of_string off, int_of_string offset)

  and fourptr_m_symb s =
    let split = Str.split (Str.regexp_string "(")
    and split' = Str.split (Str.regexp_string ",") in
    let items = split s in
    let offset = List.nth items 0 in
    if offset.[0] <>'-' then raise ParseError
    else
      let len = String.length offset in
      let offset' = String.sub offset 1 (len-1) in
      let s' = List.nth items 1 in
      let l = String.length s' in
      let ss = String.sub s' 0 (l-1) in
      let items' = split' ss in
      if List.length items' <> 3 then raise ParseError
      else
        let r1 = List.nth items' 0
        and r2 = List.nth items' 1
        and off = List.nth items' 2 in
        (reg_symb r1, reg_symb r2, int_of_string off, int_of_string offset')

  and remove_dollar_sign s' =
    let s = String.trim s' in
    if s.[0] = '$' then
      let l = String.length s in
      String.sub s 1 (l-1)
    else raise ParseError

  (*
and remove_bracket s' =
  let s = String.trim s' in
    let l = String.length s in
      if s.[0] = '(' && s.[l-1] = ')' then
        String.sub s 1 (l-2)
      else raise ParseError
   *)

  and seg_symb = function
    | "fs" -> FS | "gs" -> GS | "cs" -> CS
    | "ss" -> SS | "ds" -> DS | "es" -> ES
    | _ -> raise ParseError  in

  let segref_symb s =
    let seg_list = ["%fs"; "%gs" ; "%cs" ; "%ss" ; "%ds" ; "%es"] in
    let list_has item =
      try ignore (List.find (fun i-> i = item) seg_list); true
      with Not_found -> false in
    if (String.contains s ':') then
      let split = Str.split (Str.regexp_string ":") in
      let items = split s in
      let se = List.nth items 0 in
      let has_seg = list_has se in
      if ((List.length items) = 2) && (has_seg = true) then
        let se' = remove_dollar_sign se in
        (seg_symb se', reg_symb (List.nth items 1))
      else
        raise ParseError
    else
      raise ParseError


  and arithm_symb = function
    | "adc" -> ADC | "add" -> ADD | "xadd" -> XADD | "addl" -> ADDL | "addq" ->ADDQ
    | "addsd" -> ADDSD | "addss" -> ADDSS | "subss" -> SUBSS
    | "sub" -> SUB | "mul" -> MUL | "imul" -> IMUL | "mulsd" -> MULSD
    | "mulb" -> MULB | "div" -> DIV | "divq" -> DIVQ | "mulpd" -> MULPD
    | "divpd" -> DIVPD | "divps" -> DIVPS
    | "mulps" -> MULPS
    | "mulss" -> MULSS | "pmuludq" -> PMULUDQ
    | "idiv" -> IDIV | "divss" -> DIVSS | "idivl" -> IDIVL | "divsd" -> DIVSD
    | "inc" -> INC
    | "incl" -> INCL | "incw" -> INCW | "incb" -> INCB
    | "incq" -> INCQ
    | "dec" -> DEC | "neg" -> NEG | "negl" -> NEGL
    | "decl" -> DECL | "fdiv" -> FDIV | "decb" -> DECB
    | "decd" -> DECD | "decw" -> DECW
    | "sbb" -> SBB | "fadd" -> FADD | "fxch" -> FXCH | "subl" -> SUBL | "subsd" -> SUBSD
    | "subq" -> SUBQ
    | "fucomip" -> FUCOMIP | "fucomi" -> FUCOMI | "bsr" -> BSR | "divl" -> DIVL
    | "ucomisd" -> UCOMISD | "ucomiss" -> UCOMISS
    | "adcl" -> ADCL | "mull" -> MULL | "addw" -> ADDW | "fmul" -> FMUL | "fmull" -> FMULL
    | "fmulp" -> FMULP | "fmuls" -> FMULS | "fadds" -> FADDS | "faddp" -> FADDP
    | "subw" -> SUBW | "imull" -> IMULL | "faddl" -> FADDL | "bswap" -> BSWAP
    | "fdivl" -> FDIVL | "addb" -> ADDB | "subb" -> SUBB | "sbbl" -> SBBL
    | "fdivr" -> FDIVR | "fabs" -> FABS | "fsqrt" -> FSQRT | "fdivrs" -> FDIVRS | "sqrtss" -> SQRTSS
    | "frndint" -> FRNDINT | "fdivrl" -> FDIVRL | "fprem" -> FPREM | "cvtsi2sd" -> CVTSI2SD
    | "cvtsi2sdl" -> CVTSI2SDL | "cvtsi2ssl" -> CVTSI2SSL | "cvtss2sd" -> CVTSS2SD | "cvttsd2si" -> CVTTSD2SI
    | "cvtsi2ss" -> CVTSI2SS | "cbtw" -> CBTW | "fcompp" -> FCOMPP | "fcompl" -> FCOMPL | "fcomip" -> FCOMIP
    | "cvttss2si" -> CVTTSS2SI | "cvtsi2sdq" -> CVTSI2SDQ | "cvtps2pd" -> CVTPS2PD
    | "cvtdq2ps" -> CVTDQ2PS | "cvttps2dq" -> CVTTPS2DQ
    | "maxsd" -> MAXSD | "negq" -> NEGQ | "idivq" -> IDIVQ
    | "unpcklps" -> UNPCKLPS | "unpcklpd" -> UNPCKLPD | "unpckhpd" -> UNPCKHPD
    | "cvtpd2ps" -> CVTPD2PS
    | "cvtsd2ss" -> CVTSD2SS | "maxss" -> MAXSS
    | "minsd" -> MINSD | "sqrtsd" -> SQRTSD | "minss" -> MINSS
    | "decq" -> DECQ
    | "subpd" -> SUBPD | "addpd" -> ADDPD | "addps" -> ADDPS
    | "paddq" -> PADDQ | "paddd" -> PADDD | "paddb" -> PADDB
    | "imulq" -> IMULQ | "mulq" -> MULQ
    | "psubb" -> PSUBB | "psubw" -> PSUBW | "psubd" -> PSUBD
    | "vpsubb" -> VPSUBB | "vpsubw" -> VPSUBW | "vpsubd" -> VPSUBD
    | "psubusw" -> PSUBUSW
    | "cqto" -> CQTO
    | "pclmullqlqdq" -> PCLMULLQLQDQ | "vpclmullqlqdq" -> VPCLMULLQLQDQ
    | "pclmulhqlqdq" -> PCLMULHQLQDQ | "vpclmulhqlqdq" -> VPCLMULHQLQDQ
    | "pclmullqhqdq" -> PCLMULLQHQDQ | "vpclmullqhqdq" -> VPCLMULLQHQDQ
    | "pclmulhqhqdq" -> PCLMULHQHQDQ | "vpclmulhqhqdq" -> VPCLMULHQHQDQ
    | _ -> raise ParseError
  and logicop_symb = function
    | "and" -> AND | "andb" -> ANDB | "or" -> OR | "xor" -> XOR | "pxor" -> PXOR | "not" -> NOT
    | "notl" -> NOTL | "andl" -> ANDL | "orw" -> ORW | "notb" -> NOTB | "notw" -> NOTW
    | "xorb" -> XORB | "xorl" -> XORL | "sahf" -> SAHF | "andw" -> ANDW
    | "xorpd" -> XORPD | "xorps" -> XORPS | "andq" -> ANDQ
    | "xorq" -> XORQ | "andps" -> ANDPS | "andnps" -> ANDNPS | "orps" -> ORPS | "andpd" -> ANDPD
    | "notq" -> NOTQ | "andnpd" -> ANDNPD | "orpd" -> ORPD | "pand" -> PAND
    | "por" -> POR | "pandn" -> PANDN | "vpxor" -> VPXOR | "vpxord" -> VPXORD | "vpxorq" -> VPXORQ
    | _ -> raise ParseError
  and rolop_symb = function
    | "rol" -> ROL | "shl" -> SHL | "shr" -> SHR | "shrl" -> SHRL
    | "shld" -> SHLD | "shrd" -> SHRD | "roll" -> ROLL | "shrb" -> SHRB
    | "sal" -> SAL | "sar" -> SAR | "shll" -> SHLL | "shlb" -> SHLB
    | "sarl" -> SARL | "ror" -> ROR  | "rol" -> ROL | "rorl" -> RORL
    | "rolw" -> ROLW | "shlw" -> SHLW | "sarw" -> SARW | "shrw" -> SHRW
    | "shlq" -> SHLQ | "shrq" -> SHRQ 
    | "psllw" -> PSLLW | "pslld" -> PSLLD | "psllq" -> PSLLQ
    | "psraw" -> PSRAW | "psrad" -> PSRAD | "pslldq" -> PSLLDQ
    | "psrldq" -> PSRLDQ | "psrld" -> PSRLD
    | "shufps" -> SHUFPS | "shufpd" -> SHUFPD 
    | "pshuflw" -> PSHUFLW | "vpshuflw" -> VPSHUFLW
    | "pshufd" -> PSHUFD | "vpshufd" -> VPSHUFD
    | "pshufb" -> PSHUFB | "vpshufb" -> VPSHUFB
    | "pshufhw" -> PSHUFHW | "vpshufhw" -> VPSHUFHW
    | "pshufw" -> PSHUFW | "vpshufw" -> VPSHUFW
    | _ -> raise ParseError
  and assignop_symb = function
    | "mov" -> MOV | "movaps" -> MOVAPS | "vmovaps" -> VMOVAPS | "xchg" -> XCHG | "lea" -> LEA | "leal" -> LEAL | "leaq" -> LEAQ
    | "movsx" -> MOVSX | "movapd" -> MOVAPD | "movslq" -> MOVSLQ | "movq" -> MOVQ | "movabs" -> MOVABS
    | "movsd" -> MOVSD | "movsw" -> MOVSW | "movsb" -> MOVSB | "movss" -> MOVSS | "fstpl" -> FSTPL
    | "movzx" -> MOVZX | "fld" -> FLD | "fstp" -> FSTP | "movl" -> MOVL | "fldl" -> FLDL
    | "cmovae" -> CMOVAE | "cmove" -> CMOVE | "cmovne" -> CMOVNE | "cmovbe" -> CMOVBE
    | "cmovb" -> CMOVB | "cmovs" -> CMOVS | "cmova" -> CMOVA | "cmovns" -> CMOVNS
    | "movb" -> MOVB | "movsbl" -> MOVSBL | "movsbw" -> MOVSBW | "fldt" -> FLDT
    | "fstpt" -> FSTPT | "fstps" -> FSTPS | "fsts" -> FSTS
    | "orl" -> ORL | "orb" -> ORB | "fnstcw" -> FNSTCW | "fldcw" -> FLDCW
    | "fnstsw" -> FNSTSW | "fldz" -> FLDZ | "fld1" -> FLD1 | "fdivp" -> FDIVP
    | "repe" -> REPE | "repz" -> REPZ | "movzbl" -> MOVZBL | "movw" -> MOVW 
    | "movswq" -> MOVSWQ | "movzbw" -> MOVZBW | "movsbq" -> MOVSBQ
    | "movzwl" -> MOVZWL | "movswl" -> MOVSWL | "repnz" -> REPNZ | "fildll" -> FILDLL
    | "rep" -> REP | "cmovle" -> CMOVLE | "cmovg" -> CMOVG | "cmovl" -> CMOVL
    | "flds" -> FLDS | "filds" -> FILDS | "fildl" -> FILDL | "fstl" -> FSTL | "fistpl" -> FISTPL
    | "fsub" -> FSUB | "fdivs" -> FDIVS | "fistpll" -> FISTPLL | "fdivrp" -> FDIVRP
    | "cmovge" -> CMOVGE | "fcmovbe" -> FCMOVBE | "fsubp" -> FSUBP | "fild" -> FILD
    | "fistl" -> FISTL | "fsubrp" -> FSUBRP | "fsubrl" -> FSUBRL | "cwtl" -> CWTL
    | "fsubrs" -> FSUBRS | "fsubs" -> FSUBS | "fsubr" -> FSUBR | "fsubl" -> FSUBL
    | "fcmovnbe" -> FCMOVNBE | "fcmove" -> FCMOVE | "fcmovne" -> FCMOVNE
    | "fcmovb" -> FCMOVB | "fistp" -> FISTP | "fcmovnb" -> FCMOVNB
    | "cmovnp" -> CMOVNP | "stos" -> STOS | "stosb" -> STOSB
    | "stosw" -> STOSW | "stosd" -> STOSD | "fist" -> FIST | "fistps" -> FISTPS | "ffree" -> FFREE
    | "orq" -> ORQ | "movdqu" -> MOVDQU | "vmovdqu" -> VMOVDQU | "movdqa" -> MOVDQA | "vmovdqa" -> VMOVDQA | "vmovdqa32" -> VMOVDQA32 | "vmovdqa64" -> VMOVDQA64
    | "movups" -> MOVUPS | "vmovups" -> VMOVUPS | "movd" -> MOVD | "vmovd" -> VMOVD | "vmovq" -> VMOVQ | "movhlps" -> MOVHLPS
    | "movhpd" -> MOVHPD | "movhps" -> MOVHPS | "movlpd" -> MOVLPD | "movupd" -> MOVUPD
    | "vmovhpd" -> VMOVHPD | "vmovhps" -> VMOVHPS
    | "punpckhqdq" -> PUNPCKHQDQ 
    | "punpckldq" -> PUNPCKLDQ | "vpunpckldq" -> VPUNPCKLDQ
    | "punpcklbw" -> PUNPCKLBW
    | "punpcklqdq" -> PUNPCKLQDQ | "vpunpcklqdq" -> VPUNPCKLQDQ
    | "punpcklwd" -> PUNPCKLWD | "vpunpcklwd" -> VPUNPCKLWD
    | "pinsrw" -> PINSRW | "pinsrb" -> PINSRB | "pinsrd" -> PINSRD | "pinsrq" -> PINSRQ
    | "vpinsrw" -> VPINSRW | "vpinsrb" -> VPINSRB | "vpinsrd" -> VPINSRD | "vpinsrq" -> VPINSRQ
    | "pextrw" -> PEXTRW | "vpextrw" -> VPEXTRW
    | "movlhps" -> MOVLHPS
    | "vinserti128" -> VINSERTI128 | "vextracti128" -> VEXTRACTI128
    | "psadbw" -> PSADBW | "vpsadbw" -> VPSADBW
    | "bsf" -> BSF
    | _ -> raise ParseError
  and compareop_symb = function
    | "cmp" -> CMP | "cmpq" -> CMPQ | "test" -> TEST | "cmpl" -> CMPL
    | "cmpb" -> CMPB | "cmpw" -> CMPW | "testb" -> TESTB
    | "testl" -> TESTL | "cmpsb" -> CMPSB | "bt" -> BT
    | "testw" -> TESTW | "cmpnless" -> CMPNLESS | "cmpltss" -> CMPLTSS
    | "cmpltsd" -> CMPLTSD | "cmpnlesd" -> CMPNLESD
    | "cmpnltss" -> CMPNLTSS | "testq" -> TESTQ
    | "cmpnltsd" -> CMPNLTSD
    | "pcmpgtd" -> PCMPGTD
    | "pcmpgtb" -> PCMPGTB
    | "pcmpeqb" -> PCMPEQB | "vpcmpeqb" -> VPCMPEQB
    | "pcmpeqd" -> PCMPEQD | "vpcmpeqd" -> VPCMPEQD
    | "pcmpeqw" -> PCMPEQW
    | "cmpeqss" -> CMPEQSS
    | "fcomi" -> FCOMI
    | "comiss" -> COMISS | "comisd" -> COMISD
    | _ -> raise ParseError
  and setop_symb = function
    | "seta" ->  SETA | "setae" -> SETAE | "setb" -> SETB | "setc" -> SETC
    | "setnbe" ->  SETNBE | "setnc" -> SETNC | "setng" -> SETNG
    | "setne" -> SETNE  | "sete" -> SETE | "setnp" -> SETNP
    | "setge" -> SETGE | "setg" -> SETG | "setbe" -> SETBE
    | "setle" -> SETLE | "setl" -> SETL | "setp" -> SETP | "setns" -> SETNS
    | "sets" -> SETS
    | _ -> raise ParseError
  and otherop_symb = function
    | "nop" -> NOP | "hlt" -> HLT | "nopw" -> NOPW | "nopl" -> NOPL | "ud2" -> UD2 | "endbr32" -> ENDBR32 | "endbr64" -> ENDBR64 | "cpuid" -> CPUID
    | _ -> raise ParseError

  and stackop_symb = function
    | "push" -> PUSH | "pop" -> POP 
    | "pushl" -> PUSHL | "popl" -> POPL
    | "pushq" -> PUSHQ | "popq" -> POPQ
    | "pushf" -> PUSHF | "popf" -> POPF
    |  _ -> raise ParseError

  and jumpop_symb = function
    | "jmp" -> JMP | "jne" -> JNE | "je" -> JE
    | "jb" -> JB | "jnae" -> JNAE | "jc" -> JC
    | "jnb" -> JNB | "jae" -> JAE | "jnc" -> JNC
    | "jbe" -> JBE | "jna" -> JNA | "ja" -> JA
    | "jnbe" -> JNBE | "jl" -> JL | "jnge" -> JNGE
    | "jge" -> JGE | "jnl" -> JNL | "jle" -> JLE
    | "jng" -> JNG | "jg" -> JG | "jnle" -> JNLE
    | "js" -> JS | "jns" -> JNS | "jp" -> JP
    | "jnp" -> JNP | "jmpq" -> JMPQ | "jno" -> JNO
    | _ -> raise ParseError
  and loopop_symb = function
    | "loop" -> LOOP | "loope" -> LOOPE | "loopne" -> LOOPNE
    | _ -> raise ParseError
  and flagop_symb = function
    | "cld" -> CLD | "cltd" -> CLTD | "cltq" -> CLTQ
    | _ -> raise ParseError
  and control_symb = function
    | "call" -> (call_des := true; CALL)
    | "callq" -> (call_des := true; CALLQ)
    | "leave" -> LEAVE
    | "leaveq" -> LEAVEQ
    | "ret" -> RET
    | "retn" -> RETN
    | "retq" -> RETQ
    | "fxam" -> FXAM | "fchs" -> FCHS
    | _ -> raise ParseError in

  let controlop_symb = function s ->
    try Jump (jumpop_symb s) with _ ->
      try Loop (loopop_symb s) with _ ->
        try Flag (flagop_symb s) with _ ->
          try (control_symb s) with  _ ->
            raise ParseError

  and systemp_symb = function
    | "int" -> INT | "in" -> IN
    | "out" -> OUT | _ -> raise ParseError in

  let errorop_symb = function
    | "(bad)" -> BAD
    | _ -> failwith "unsupported type in errorop_symb" in

  let commonop_symb = function s ->
    try Arithm (arithm_symb s)
    with _ ->
      try Logic (logicop_symb s)
      with _ ->
        try Rol (rolop_symb s)
        with _ ->
          try Assign (assignop_symb s)
          with _ ->
            try Compare (compareop_symb s)
            with _ ->
              try Set (setop_symb s)
              with _ ->
                try Other (otherop_symb s)
                with _ -> raise ParseError in

  let op_symb = function s ->
    try ErrorOP (errorop_symb s)
    with _ ->
      try CommonOP (commonop_symb s)
      with _ ->
        try StackOP (stackop_symb s)
        with _ ->
          try ControlOP (controlop_symb s)
          with _ ->
            try SystemOP (systemp_symb s)
            with _ ->
              print_string (s ^ "\n");
              raise ParseError in

  let unptr_symb s =
    let l = String.length s in
    let s' = String.sub s 1 (l-2) in
    reg_symb s' in

  (*
let eiz_symb = function s ->
  let s' = String.trim s in
  let l = String.length s' in
    let s1 = String.sub s' 1 (l-2) in
  let split = Str.split (Str.regexp_string "+") in
    let items = split s1 in
      let offset = List.nth items 2
      and reg1 = List.nth items 0
      and reg2 = List.nth items 1 in
      if reg2 <> "eiz*1" then raise ParseError
      else
          (reg_symb reg1, reg_symb reg2, int_of_string offset) in
   *)

  let assist_symb = function
    | "scas" -> SCAS
    (*
| "stos" -> STOS
     *)
    | "movsl" -> MOVSL
    | "movsq" -> MOVSQ
    | "cmpsw" -> CMPSW
    | "cmpsb" -> CMPSB
    | "pop" -> POP
    | _ -> raise ParseError
  in

  object (self)

    val mutable func_list : func list = []
    val mutable sec_list : section list = []

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
      try UnOP (unptr_symb s)
      with _ ->
        try
          BinOP_MINUS (binptr_m_symb s)
        with _ ->
          try
            BinOP_PLUS (binptr_p_symb s)
          with _ ->
            try
              ThreeOP (threeptr_symb s)
            with _ ->
              try
                FourOP_MINUS (fourptr_m_symb s)
              with _ ->
                try
                  FourOP_PLUS (fourptr_p_symb s)
                with _ ->
                  try
                    JmpTable_MINUS (self#jmptable_m_symb s)
                  with _ ->
                    try
                      JmpTable_PLUS (self#jmptable_p_symb s)
                    with _ ->
                      try SegRef (segref_symb s)
                      with _ -> raise ParseError

    method ptr_symb s =
      let has s1 s2 =
        let re = Str.regexp_string s2 in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false in
      if ((has s "(")&&(has s ")")) then  (* then it should be ptr *)
        self#ptraddr_symb s
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
        let func' = "S_0x"^(String.uppercase func) in
        let f = self#get_func func' false in
        f
      else if String.contains s' '-' then
        let func = List.nth (split s) 0 in
        let func' = "S_0x"^(String.uppercase func) in
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

    method jmptable_m_symb s =
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

    method jmptable_p_symb s =
      if String.exists s "(," then
        let split = Str.split (Str.regexp_string ",") in
        let tokens = split s in
        let addr = List.nth tokens 0
        and r = List.nth tokens 1
        and off = List.nth tokens 2 in
        let addr' = String.sub addr 0 ((String.length addr)-1) in
        let off' = String.sub off 0 ((String.length off)-1) in
        (int_of_string addr', reg_symb r, int_of_string off')
      else raise ParseError

    (* call *0x80808080(, %eax, 4) *)
    method calljmp_symb s =
      if String.exists s "(," then
        let split = Str.split (Str.regexp_string ",") in
        let tokens = split s in
        let addr = List.nth tokens 0
        and r = List.nth tokens 1
        and off = List.nth tokens 2 in
        let addr' = String.sub addr 1 ((String.length addr)-2) in
        let off' = String.sub off 0 ((String.length off)-1) in
        (int_of_string addr', reg_symb r, int_of_string off')
      else raise ParseError

    method leades_symb s =
      if String.exists s "(," then
        let split = Str.split (Str.regexp_string ",") in
        let tokens = split s in
        let addr = List.nth tokens 0
        and r = List.nth tokens 1
        and off = List.nth tokens 2 in
        let addr' = String.sub addr 0 ((String.length addr)-1) in
        let off' = String.sub off 0 ((String.length off)-1) in
        (int_of_string addr', reg_symb r, int_of_string off')
      else raise ParseError

    method callstar_symb s =
      if s.[0] <> '*' then raise ParseError
      else
        (
          let s' = String.sub s 1 (String.length s - 1) in
          (int_of_string s')
        )

    method stardes_symb s =
      let s' = String.sub s 1 (String.length s - 1) in
      self#exp_symb s'

    method symbol_symb s =
      let s' = String.trim s in
      if s'.[0] = '*' then
        StarDes (self#stardes_symb s')
      else if !call_des = true then
        (
          CallDes (self#calldes_symb s')
        )
        (* else if String.contains s '+' then *)
      else
        (self#jumpdes_symb s')

    method exp_symb s =
      try Ptr (self#ptr_symb s)
      with _ ->
        try Reg (reg_symb s)
        with _ ->
          try Assist (assist_symb s)
          with _ ->
            try Const (const_symb s)
            with _ ->
              try Symbol (self#symbol_symb s)
              with _ ->
                try Label (s)   (* we jut consider these as labels *)
                with _ ->
                  raise ParseError

    method push_stack lex =
      match lex with
      | Lop s ->  Op (op_symb s)
      | Lexp s -> Exp (self#exp_symb s)
      | Lloc s -> Loc (loc_symb s)
      | _ -> raise ParseError

    method reduce_stack stack pre =
      let pre' = match pre with
        | true -> Some LOCK
        | false -> None in
      match stack with
      | (Loc l)::(Op p)::[] -> SingleInstr (p, l, pre')
      | (Loc l)::(Exp exp1)::(Op p)::[] -> DoubleInstr(p, exp1, l, pre')
      | (Loc l)::(Exp exp1)::(Exp exp2)::(Op p)::[] -> TripleInstr(p, exp1, exp2, l, pre')
      | (Loc l)::(Exp exp1)::(Exp exp2)::(Exp exp3)::(Op p)::[] -> FourInstr(p, exp1, exp2, exp3, l, pre')
      | (Loc l)::(Exp exp1)::(Exp exp2)::(Exp exp3)::(Exp exp4)::(Op p)::[] -> FifInstr(p, exp1, exp2, exp3, exp4, l, pre')
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

    method parse_instr instr loc =
      self#init_process;
      let has_pre = prefix_identify instr
      and instr' = prefix_sub instr in
      let lexem_list = Array.to_list (lexer instr' loc) in
      let s : stack_type list = [] in
      let parse_one s l =
        match l with
        | Lend -> s
        | _ -> (self#push_stack l)::s
      in
      let stack = List.fold_left parse_one s lexem_list in
      self#reduce_stack stack has_pre
  end
