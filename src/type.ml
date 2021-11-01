type file =
            {
               name: string;
               striped: bool; (*whether this file has been stripped*)
               format: string; (*ELF, PE, MachOS or even COFF*)
               is32bit: bool; (*whether this file is compiled 32 bit or not*)
            }
and section =
    {
        sec_name: string;
        sec_begin_addr : int;
        sec_size: int;
    }
and func =
    {
        func_name: string;
        func_begin_addr : int;
        func_end_addr : int;
        is_lib: bool;
    }
(* basic block defination
 * In order to do bb level diversify,
 * one critical thing is that
 *    whether the preceding block is **fall-through** to current block
 *    like this:
 *
 *  BB1
 *  {
 *     instr1;
 *     instr2;
 *     instr3;
 *  }
 *  BB2
 *  {
 *     L:
 *      instr1'
 *      instr2'
 *      instr3'
 *      instr4'
 *  }
 *
 *    or it can not be reached directly by preceding block like this:
 *
 *  BB1
 *  {
 *     instr1;
 *     instr2;
 *     call/jmp L';
 *  }
 *  BB2
 *  {
 *      instr1'
 *      instr2'
 *      instr3'
 *      instr4'
 *  }
 *
 * The key observation is whether the last instruction of preceding block is
 *  ControlOP or not
 *
 *  Even though it might have no difference comparing these two situations
 *  in the context of diversity, but it should be necessary to record this info
 *
 *  we will record the preceding block with current block
 *  as well as current block with succeeding block
 *
 **)
(* two typical ways for control flow transfer, fall through and jump *)
and control = J | F
and loc =
    {
        loc_label : string;
        loc_addr : int;
        loc_visible : bool;
    }
type op =
  | CommonOP of commonop
  | StackOP of stackop
  | ControlOP of controlop
  | SystemOP of systemop
  | ErrorOP of errorop
and errorop = BAD
and commonop =
  | Arithm of arithmop
  | Logic of logicop
  | Rol of rolop
  | Assign of assignop
  | Compare of compareop
  | Set of setop
  | Other of otherop
and controlop =
  | Jump of jumpop
  | Loop of loopop
  | Flag of flagop
  | CALL | CALLQ | LEAVE | LEAVEQ
  | RET | RETN | RETQ
  | FXAM | FCHS
and stackop = PUSH | POP | PUSHL | POPL | PUSHF | POPF | PUSHQ | POPQ
and systemop = INT | IN | OUT
and arithmop = ADC | ADD | XADD | SUB | ADDL | ADDQ | SUBL | SUBQ
               | MUL | IMUL | MULB | MULSD | DIV | IDIV | DIVL | ADCL | IDIVL | DIVSD |DIVSS
               | MULSS | DIVQ | IDIVQ | PMULUDQ
               | INC | INCQ | INCL | INCW | DEC | NEG | SBB | FADD | NEGL | FMUL
               | FXCH | FUCOMIP | FUCOMI | FCOMPP | FCOMPL | FCOMIP | BSR | MULL | FMULL
               | UCOMISD | UCOMISS | SUBSS
               | ADDW | ADDSD | ADDSS | FMULP | FMULS | FADDS | FADDP | FADDL
               | SUBW | SUBSD | IMULL | BSWAP | DECL | DECB | DECD | DECW
               | FDIV | FDIVL
               | ADDB | SUBB | SBBL | FDIVR | FABS | FSQRT | FDIVRS | CBTW
               | FRNDINT | FDIVRL | FPREM | CVTSI2SD | CVTSI2SDL | CVTSI2SSL
               | CVTSS2SD | CVTDQ2PS
               | CVTSI2SS | CVTTSD2SI | CVTTSS2SI | CVTSI2SDQ | CVTPS2PD
               | MAXSD | NEGQ | UNPCKLPS | UNPCKLPD | CVTPD2PS | CVTSD2SS
               | SQRTSS | MAXSS | MINSD | SQRTSD | MINSS | CVTTPS2DQ
               | DECQ | SUBPD | ADDPD | PADDQ | IMULQ | PADDD | PADDB
               | PSUBD | PSUBW | PSUBB | MULPD | UNPCKHPD | ADDPS | MULPS
               | VPSUBD | VPSUBW | VPSUBB | MULQ
               | DIVPD | DIVPS | CQTO | INCB | PSUBUSW
               | PCLMULLQLQDQ | VPCLMULLQLQDQ
               | PCLMULHQLQDQ | VPCLMULHQLQDQ
               | PCLMULLQHQDQ | VPCLMULLQHQDQ
               | PCLMULHQHQDQ | VPCLMULHQHQDQ
and logicop = AND | ANDB | OR | XOR | PXOR | NOT | ANDL | NOTL | ORW | XORB | XORL
               | SAHF | ANDW | NOTB | NOTW | XORPD | XORPS | ANDQ
               | XORQ | ANDPS | ANDNPS | ORPS | ANDPD | NOTQ | ANDNPD
               | ORPD | PAND | POR | PANDN | VPXOR | VPXORD | VPXORQ
and rolop = ROL | SHL | SHR | SHLD |SHRD | SHRL | ROR | RORL
            | SAL | SAR | SHLL | ROLL | SHRB | SHLB | SARL | ROLW | SHLW
            | SARW | SHRW | SHLQ | SHRQ | SHUFPS | SHUFPD
            | PSLLW | PSLLD | PSLLQ | PSRAW | PSRAD | PSLLDQ | PSRLDQ| PSRLD 
            | PSHUFLW | PSHUFB | PSHUFW | PSHUFHW | PSHUFD
            | VPSHUFLW | VPSHUFB | VPSHUFW | VPSHUFHW | VPSHUFD
and assignop = MOV | XCHG | LEA | LEAL | LEAQ | MOVSX | MOVSD | MOVL | FLDL | MOVZBL | MOVZBW
               | MOVSW | MOVAPD | MOVSLQ | MOVQ | MOVABS | MOVSBQ
               | MOVW | MOVZX | MOVAPS | VMOVAPS | FLD | FSTP | CMOVAE | CMOVE | CMOVNE | MOVSS
               | CMOVBE | CMOVB | CMOVS | CMOVA | CMOVNS | MOVB
               | MOVZWL | MOVSWL | MOVSBL | MOVSBW | FLDT | FSTPT | ORL | ORB | MOVSB
               | FNSTCW | FLDCW | FLDZ | REPZ | REPE | FSTPL | REPNZ
               | REP | FNSTSW | CMOVLE | CMOVG | CMOVL | FILDLL
               | FLDS | FILDS | FISTPS | FILDL | FLD1 | FDIVP | FSTL | FISTPL | FILD
               | FSUB | FDIVS | FISTPLL | FDIVRP | CMOVGE | FCMOVBE
               | FSUBP | FISTL | FSUBRP | FSUBRL | CWTL | FSUBRS | FSTPS
               | FSUBS | FSUBR | FSTS | FSUBL | FCMOVNBE | FCMOVE | FCMOVNE
               | FCMOVB | FISTP | FCMOVNB | CMOVNP | STOS | STOSB | STOSW | STOSD
               | FIST | FFREE | MOVSWQ | ORQ | MOVDQU | VMOVDQU | MOVDQA | VMOVDQA | VMOVDQA32 | VMOVDQA64
               | MOVUPS | VMOVUPS | MOVD | VMOVD | VMOVQ | MOVHLPS | MOVLHPS | MOVUPD
               | PUNPCKHQDQ | PUNPCKLDQ | PUNPCKLBW | PEXTRW 
               | VPUNPCKLDQ | VPEXTRW
               | PINSRB | VPINSRB
               | PINSRD | VPINSRD
               | PINSRQ | VPINSRQ
               | PINSRW | VPINSRW
               | PUNPCKLQDQ | PUNPCKLWD | VPUNPCKLWD | MOVHPD | MOVHPS | MOVLPD
               | VPUNPCKLQDQ | VMOVHPD | VMOVHPS
               | LAHF | SAHF
               | VINSERTI128 | VEXTRACTI128
               | PSADBW | VPSADBW
               | BSF
and compareop = CMP | CMPQ | TEST | CMPL | CMPB | CMPW | TESTB | TESTL | CMPSB
                | BT | TESTW | CMPNLESS | CMPNLESD | CMPLTSS | CMPNLTSS | TESTQ
                | CMPNLTSD | PCMPGTD | PCMPGTB | PCMPEQD | VPCMPEQD | PCMPEQB| VPCMPEQB | CMPLTSD | PCMPEQW
                | CMPEQSS | FCOMI | COMISS | COMISD
and setop = SETA | SETAE | SETB | SETBE | SETC
            | SETNBE | SETNC | SETNG | SETNE
            | SETE | SETNP | SETGE | SETG | SETLE
            | SETL | SETP | SETNS | SETS
and otherop = NOP | HLT | NOPW | NOPL | UD2 | ENDBR32 | ENDBR64 | CPUID
and jumpop = JMP | JNE | JE | JB | JNAE | JNP | JNO
              | JC | JNB | JAE | JNC | JBE | JNA
              | JA | JNBE | JL | JNGE | JGE | JNL | JLE
              | JNG | JG | JNLE | JS | JNS | JP | JMPQ
and loopop = LOOP | LOOPE |LOOPNE
and flagop = CLD | CLTD | CLTQ
(* and assistop = SCAS | CMPSB | STOS | MOVSL | MOVSB | CMPSW *)
and assistop = SCAS | MOVSL | MOVSB | CMPSW | CMPSB | MOVSQ | POP
and reg =
  | CommonReg of commonreg
  | SpecialReg of specialreg (* update for speical registers *)
  | StackReg of stackreg
  | PCReg of pcreg (*Program counter, probabaly we don't need this*)
  | OtherReg of otherreg
and commonreg =
  | RAX | RBX | RCX | RDX | RDI | RSI
  | EAX | EBX | ECX | EDX | EDI | ESI
  | AX | BX | CX | DX | AL | BL | CL | DL
  | AH | BH | CH | DH
and specialreg =
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D
  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W
  | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7
  | YMM0 | YMM1 | YMM2 | YMM3 | YMM4 | YMM5 | YMM6 | YMM7

and stackreg =
  | RBP | RSP
  | ESP | EBP
and pcreg = EIP | RIP
and otherreg = EIZ
and ptrtyp = QWORD | DWORD | WORD | TBYTE | BYTE
and mathop = MATHADD | MATHSUB
and seg = FS | GS | CS | SS | DS | ES
     deriving (Show)
type symbol =
  (* | JumpDes of (func * int) jle    8048427 <main+0x43> *)
  | JumpDes of int (*jle    8048427 <main+0x43>,   we only take 8048427 *)
  | CallDes of func (*call   8048300 <printf@plt>*)
  | StarDes of exp (*call/jmp   *exp  typically exp could be const; jmptable; ptr*)
 (*jmp *0x80509e4(,%eax,4)*)
 (* | StarDes_S of (string * reg * int) (*jmp *S_0x80509E4(,%eax,4)*) *)
 (* | LeaDes of (int * reg * int) *)
 (*lea    0x0(,%ebp,8),%eax  so basically this is a quite rare situation. *)
 and const =
   | Point of int (* 0x8048440*) | Normal of int (* $0x8048440*)
 and exp =
   | Const of const
   | Symbol of symbol
   | Reg of reg
   | Assist of assistop (*this is rare:   repz ret*)
   | Ptr of ptraddr
   | Label of string
 and ptraddr =
   | UnOP of reg
   (* mov (%eax) ,%eax *)
   | BinOP_PLUS of (reg * int)
   | BinOP_PLUS_S of (reg * string)
   (* mov 0x3c(%esp),%eax *)
   | BinOP_MINUS of (reg * int)
   (* mov -0x3c(%esp),%eax *)
   | BinOP_MINUS_S of (reg * string)
   (* mov -0x3c(%esp),%eax *)
   | ThreeOP of (reg * reg * int)
   (* lea (%edi, %esi, 8), %edi *)
   | FourOP_PLUS of (reg * reg * int * int)
   (* mov 0x18(%esp,%eax,4),%edx *)
   | FourOP_MINUS of (reg * reg * int * int)
   (* mov -0x18(%esp,%eax,4),%edx *)
   | FourOP_PLUS_S of (reg * reg * int * string)
   (* mov S_0x8048050(%esp,%eax,4),%edx *)
   | FourOP_MINUS_S of (reg * reg * int * string)
   (* mov -S_0x8048050(%esp,%eax,4),%edx *)
   | JmpTable_PLUS of (int * reg * int) (* mov 0x805e17c(,%ebx,4),%eax *)
   | JmpTable_MINUS of (int * reg * int) (* lea -4(,%ebx,4),%eax *)
   | JmpTable_PLUS_S of (string * reg * int) (* mov S_0x805e17c(,%ebx,4),%eax *)
   | JmpTable_MINUS_S of (string * reg * int) (* lea -S_0x805e17c(,%ebx,4),%eax *)
   | SegRef of (seg * reg)
 (* rep stos %al,%es:(%edi)*)
 and prefix = LOCK
 and instr =
   | SingleInstr of op * loc * prefix option
   | DoubleInstr of op * exp * loc * prefix option
   | TripleInstr of op * exp * exp * loc * prefix option
   | FourInstr of op * exp * exp * exp * loc * prefix option
   | FifInstr of op * exp * exp * exp * exp * loc * prefix option
 (* as far as I know, imul $0xe10,%ebp,%eax *)

and bblock =
    {
        bf_name: string; (* which function belongs to*)
        bblock_name : string;
        bblock_begin_loc : loc;
        bblock_end_loc : loc;
        bblock_head_instr : instr;
        (* preceding_direct_reach : bool; *)
        (* succeeding_direct_reach : bool; *)
    }


 and jmp_type = RET_TYPE | INDIRECT | DIRECT_CALL | DIRECT_JMP_INTER |
                DIRECT_JMP_INTRA | COND_JMP_INTER | COND_JMP_INTRA

 and instrument_direct = BEFORE | AFTER

 and mem_write_type =
   SINGLE_WRITE
   (* push XXX *)
   | DOUBLE_WRITE
   (* mov XXX, 4(%eax) *)
   | TRIPLE_WRITE
