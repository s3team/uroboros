(*****************)
(**** Common *****)
(*****************)

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
and control = J | F
and loc =
    {
        loc_label : string;
        loc_addr : int;
        loc_visible : bool;
    }
and label = string
and jumpdes = int
and calldes = func

(*****************)
(**** All Arch ***)
(*****************)

and op =
  | Intel_OP of intel_op
  | Arm_OP of (arm_op * arm_suffix option)
and intel_arm_reg =
| Intel_Reg of intel_reg
| Arm_Reg of arm_reg
and ptraddr =
  | UnOP of intel_arm_reg
  (* mov (%eax) ,%eax *)
  | UnOP_WB of arm_reg
  | BinOP_PLUS of (intel_arm_reg * int)
  | BinOP_PLUS_S of (intel_arm_reg * string)
  (* mov 0x3c(%esp),%eax *)
  | BinOP_PLUS_WB of (arm_reg * int)
  (* ldr x19,[sp,#16]! *)
  | BinOP_MINUS of (intel_arm_reg * int)
  (* mov -0x3c(%esp),%eax *)
  | BinOP_MINUS_S of (intel_arm_reg * string)
  (* mov -0x3c(%esp),%eax *)
  | BinOP_MINUS_WB of (arm_reg * int)
  (* stp x29,x30,[sp,#-32]! *)
  | ThreeOP of (intel_arm_reg * intel_arm_reg * int)
  (* lea (%edi, %esi, 8), %edi *)
  | FourOP_PLUS of (intel_arm_reg * intel_arm_reg * int * int)
  (* mov 0x18(%esp,%eax,4),%edx *)
  | FourOP_MINUS of (intel_arm_reg * intel_arm_reg * int * int)
  (* mov -0x18(%esp,%eax,4),%edx *)
  | FourOP_PLUS_S of (intel_arm_reg * intel_arm_reg * int * string)
  (* mov S_0x8048050(%esp,%eax,4),%edx *)
  | FourOP_MINUS_S of (intel_arm_reg * intel_arm_reg * int * string)
  (* mov -S_0x8048050(%esp,%eax,4),%edx *)
  | JmpTable_PLUS of (int * intel_arm_reg * int) (* mov 0x805e17c(,%ebx,4),%eax *)
  | JmpTable_MINUS of (int * intel_arm_reg * int) (* lea -4(,%ebx,4),%eax *)
  | JmpTable_PLUS_S of (string * intel_arm_reg * int) (* mov S_0x805e17c(,%ebx,4),%eax *)
  | JmpTable_MINUS_S of (string * intel_arm_reg * int) (* lea -S_0x805e17c(,%ebx,4),%eax *)
  | SegRef of (intel_seg * intel_arm_reg)
(* rep stos %al,%es:(%edi)*)
and symbol =
  (* | JumpDes of (func * int) jle    8048427 <main+0x43> *)
  | JumpDes of jumpdes (*jle    8048427 <main+0x43>,   we only take 8048427 *)
  | CallDes of calldes (*call   8048300 <printf@plt>*)
  | StarDes of exp (*call/jmp   *exp  typically exp could be const; jmptable; ptr*)
 (*jmp *0x80509e4(,%eax,4)*)
 (* | StarDes_S of (string * reg * int) (*jmp *S_0x80509E4(,%eax,4)*) *)
 (* | LeaDes of (int * reg * int) *)
 (*lea    0x0(,%ebp,8),%eax  so basically this is a quite rare situation. *)
and const =
  | Point of int (* 0x8048440*) | Normal of int (* $0x8048440*)
  | Immediate of int (* #0 and #0x0 *)
and exp =
  | Const of const
  | Symbol of symbol
  | Reg of intel_arm_reg
  | Assist of assistop (*this is rare:   repz ret*)
  | Ptr of ptraddr
  | Label of label
and prefix = LOCK | ADDR32 | BND
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

and mem_operation_type =
  | MEM_WRITE_TYPE of mem_write_type
  | MEM_READ_TYPE of mem_read_type
and mem_write_type =
  SINGLE_WRITE
  (* push XXX *)
  | DOUBLE_WRITE
  (* mov XXX, 4(%eax) *)
  | TRIPLE_WRITE
and mem_read_type =
  SINGLE_READ
  | DOUBLE_READ
  | TRIPLE_READ

(*****************)
(***** Intel *****)
(*****************)

and intel_op =
  | Intel_CommonOP of intel_commonop
  | Assist of assistop (*this is rare:   repz ret*)
  | Intel_StackOP of intel_stackop
  | Intel_ControlOP of intel_controlop
  | Intel_SystemOP of intel_systemop
  | Intel_ErrorOP of intel_errorop
and intel_errorop = BAD
and intel_commonop =
  | Intel_Arithm of intel_arithmop
  | Intel_Logic of intel_logicop
  | Intel_Rol of intel_rolop
  | Intel_Assign of intel_assignop
  | Intel_Compare of intel_compareop
  | Intel_Set of intel_setop
  | Intel_Other of intel_otherop
and intel_controlop =
  | Intel_Jump of intel_jumpop
  | Intel_Loop of intel_loopop
  | Intel_Flag of intel_flagop
  | CALL | CALLQ | LEAVE | LEAVEQ
  | RET | RETN | RETQ
  | FXAM | FCHS
and intel_stackop = PUSH | POP | PUSHL | POPL | PUSHF | POPF | PUSHQ | POPQ
and intel_systemop = INT | IN | OUT | SYSCALL | XABORT |PREFETCHNTA | PREFETCHT0 | PREFETCHT1 | SFENCE | LFENCE | FWAIT
and intel_arithmop = ADC | ADD | XADD | SUB | ADDL | ADDQ | SUBL | SUBQ | PSUBQ | ADCQ
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
               | BZHI | VPADDB
and intel_logicop = AND | ANDB | OR | XOR | PXOR | NOT | ANDL | NOTL | ORW | XORB | XORL
               | SAHF | ANDW | NOTB | NOTW | XORPD | XORPS | ANDQ
               | XORQ | ANDPS | ANDNPS | ORPS | ANDPD | NOTQ | ANDNPD
               | ORPD | PAND | POR | PANDN | VPXOR | VPXORD | VPXORQ | VPORQ
               | VPAND | SHLX
               | VPTERNLOGD | VPTESTMB | VPTESTNMB
               | KORTESTD | KORD | ANDN | KORTESTQ | BLSR | VPTESTNMD | KXNORQ
and intel_rolop = ROL | SHL | SHR | SHLD |SHRD | SHRL | ROR | RORL
            | SAL | SAR | SHLL | ROLL | SHRB | SHLB | SARL | ROLW | SHLW | SHRX
            | SARW | SHRW | SHLQ | SHRQ | SHUFPS | SHUFPD
            | PSLLW | PSLLD | PSLLQ | PSRAW | PSRAD | PSLLDQ | PSRLDQ| PSRLD
            | PSHUFLW | PSHUFB | PSHUFW | PSHUFHW | PSHUFD
            | VPSHUFLW | VPSHUFB | VPSHUFW | VPSHUFHW | VPSHUFD
            | VPSRLDQ | VPSLLDQ
            | SARQ | SARX
and intel_assignop = MOV | XCHG | LEA | LEAL | LEAQ | MOVSX | MOVSD | MOVL | FLDL | MOVZBL | MOVZBW
               | MOVSW | MOVAPD | MOVSLQ | MOVQ | MOVABS | MOVSBQ | MOVBE | MOVNTDQ | LDDQU
               | MOVW | MOVZX | MOVAPS | VMOVAPS | FLD | FSTP | CMOVAE | CMOVE | CMOVNE | MOVSS
               | CMOVBE | CMOVB | CMOVS | CMOVA | CMOVNS | MOVB | MOVNTPS
               | MOVZWL | MOVSWL | MOVSBL | MOVSBW | FLDT | FSTPT | ORL | ORB | MOVSB
               | FNSTCW | FLDCW | FLDZ | REPZ | REPE | FSTPL | REPNZ
               | REP | FNSTSW | CMOVLE | CMOVG | CMOVL | FILDLL
               | FLDS | FILDS | FISTPS | FILDL | FLD1 | FDIVP | FSTL | FISTPL | FILD
               | FSUB | FDIVS | FISTPLL | FDIVRP | CMOVGE | FCMOVBE
               | FSUBP | FISTL | FSUBRP | FSUBRL | CWTL | FSUBRS | FSTPS
               | FSUBS | FSUBR | FSTS | FSUBL | FCMOVNBE | FCMOVE | FCMOVNE
               | FCMOVB | FISTP | FCMOVNB | CMOVNP | STOS | STOSB | STOSW | STOSD
               | FIST | FFREE | MOVSWQ | ORQ | MOVDQU | VMOVDQU | MOVDQA | VMOVDQA
               | VMOVDQA32 | VMOVDQA64 | VMOVNTDQ
               | MOVUPS | VMOVUPS | MOVD | VMOVD | VMOVQ | MOVHLPS | MOVLHPS | MOVUPD
               | PUNPCKHQDQ | PUNPCKLDQ | PUNPCKLBW | PEXTRW
               | VPUNPCKLDQ | VPEXTRW | PUNPCKHDQ
               | KUNPCKDQ | KUNPCKBW
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
               | XGETBV | PMOVMSKB | VPMOVMSKB | PMINUB | PMINUD | VPMINUB | VPMINUD | PALIGNR | VPALIGNR | TZCNT | VZEROUPPER | PCMPISTRI
               | PMAXUB | VPMAXUB | VPBROADCASTB | VPBROADCASTD | VPBROADCASTQ | VBROADCASTSS | VPOR | VPAND | VPANDN | VPTEST
               | VPCMPEQB | VPCMPEQD | VPCMPEQQ | VPCMPGTB | VPCMPISTRI
               | VPSHUFB | VPSLLDQ | VPSRLDQ | VPSUBB | BTS | STD | CLD | RDSSPD | RDSSPQ | MOVMSKPS | MOVMSKPD | BNDMOV
               | FXSAVE | FXAM | FXRSTOR | XRSTOR | XSAVE | XSAVEC | INCSSPQ | BTR | FLDENV | FNSTENV | STMXCSR | INCSSPD | LDMXCSR
               | VINSERTI64X4 | VINSERTI32X4 | VINSERTI16X4 | VINSERTI8X4
               | VMOVDQU64 | VMOVDQU32 | VMOVDQU16 | VMOVDQU8
               | KMOVQ | KMOVD | KMOVW | KMOVB | VPCMPLTUB
               | POPCNT | RDTSC | BLSMSK | RSTORSSP | SAVEPREVSSP
and intel_compareop = CMP | CMPQ | TEST | CMPL | CMPB | CMPW | TESTB | TESTL | CMPSB | PTEST
                | BT | BTC | BTCQ | TESTW | CMPNLESS | CMPNLESD | CMPLTSS | CMPNLTSS | TESTQ
                | CMPNLTSD | PCMPGTD | PCMPGTB | PCMPEQD | VPCMPEQD | PCMPEQB| VPCMPEQB | CMPLTSD | PCMPEQW
                | CMPEQSS | FCOMI | COMISS | COMISD | CMPXCHG
                | XTEST
                | VPCMPNEQUB | VPCMPNEQB | VPCMPNEQD
and intel_setop = SETA | SETAE | SETB | SETBE | SETC | SETO
            | SETNBE | SETNC | SETNG | SETNE
            | SETE | SETNP | SETGE | SETG | SETLE
            | SETL | SETP | SETNS | SETS
and intel_otherop = NOP | HLT | NOPW | NOPL | UD2 | ENDBR32 | ENDBR64 | CPUID | PAUSE | XEND | VZEROALL
and intel_jumpop = JMP | JNE | JE | JB | JNAE | JNP | JNO
              | JC | JNB | JAE | JNC | JBE | JNA
              | JA | JNBE | JL | JNGE | JGE | JNL | JLE
              | JNG | JG | JNLE | JS | JNS | JP | JMPQ
              | JZ | JNZ | JPE | JO | JRCXZ | JECXZ
              | XBEGIN
and intel_loopop = LOOP | LOOPE |LOOPNE
and intel_flagop = CLD | CLTD | CLTQ
(* and assistop = SCAS | CMPSB | STOS | MOVSL | MOVSB | CMPSW *)
and assistop = SCAS | MOVSL | MOVSB | CMPSW | CMPSB | MOVSQ | POP
and intel_reg =
  | Intel_CommonReg of intel_commonreg
  | Intel_SpecialReg of intel_specialreg (* update for speical registers *)
  | Intel_StackReg of intel_stackreg
  | Intel_PCReg of intel_pcreg (*Program counter, probabaly we don't need this*)
  | Intel_OtherReg of intel_otherreg
and intel_commonreg =
  | RAX | RBX | RCX | RDX | RDI | RSI
  | EAX | EBX | ECX | EDX | EDI | ESI
  | AX | BX | CX | DX | AL | BL | CL | DL
  | AH | BH | CH | DH
and intel_specialreg =
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D
  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W
  | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7
  | YMM0 | YMM1 | YMM2 | YMM3 | YMM4 | YMM5 | YMM6 | YMM7
and intel_stackreg =
  | RBP | RSP
  | ESP | EBP
and intel_pcreg = EIP | RIP
and intel_otherreg = EIZ
and intel_ptrtyp = QWORD | DWORD | WORD | TBYTE | BYTE
and intel_mathop = MATHADD | MATHSUB
and intel_seg = FS | GS | CS | SS | DS | ES

(*****************)
(****** ARM ******)
(*****************)

and arm_op =
  | Arm_CommonOP of arm_commonop
  | Arm_StackOP of arm_stackop
  | Arm_ControlOP of arm_controlop
  | Arm_SystemOP of arm_systemop
  | Arm_ErrorOP of arm_errorop
and arm_errorop = BAD
and arm_commonop =
  | Arm_Arithm of arm_arithmop
  | Arm_Logic of arm_logicop
  | Arm_Rol of arm_rolop
  | Arm_Assign of arm_assignop
  | Arm_Compare of arm_compareop
  | Arm_Other of arm_otherop
and arm_controlop =
  (* Return instructions in ARM32 be like:
   * 1. BX LR
   * 2. MOV PC, LR
   *)
  | B | BL | BX | BLX | BXJ | TBB | TBH
  | BCC | BCS | BGE | BGT | BHI | BLE | BLS | BLT| BMI | BNE | BPL | BVC | BVS
  | CBNZ | CBZ
  | BEQ | BR | BXEQ | BLEQ
  (* AArch64 *)
  | RET | RETAA | RETAB | RETAASPPC | RETABSPPC | RETAASPPCR | RETABSPPCR
and arm_stackop = PUSH | POP | VPUSH | VPOP
and arm_systemop =
  | BKPT | CLREX | CPS | CPSIE | CPSID | DBG | DMB
  | DSB | ISB | PLD | PLI | RFE | SEV | SMC | SRS
  | SVC | WFE | WFI | YIELD | UDF
and arm_arithmop =
  | ADC | ADCS | ADD | ADDS | ADDW | ADR | AND | ANDS
  | ANDEQ
  | CLZ | MLA | MLS | MUL | NEG | QADD | QADD16 | QADD8
  | QASX | QDADD | QDSUB | QSAX | QSUB | QSUB16 | QSUB8
  | RSB | RSBS | SADD16 | SADD8 | SASX | SBC | SBCS
  | SDIV | SHADD16 | SHADD8 | SHASX | SHSAX | SHSUB16
  | SHSUB8 | SMLABB | SMLABT | SMLATB | SMLATT | SMLAD
  | SMLADX | SMLAL | SMLALBB | SMLALBT | SMLALTB | SMLALTT
  | SMLALD | SMLALDX | SMLAWB | SMLAWT | SMLSD | SMLSDX
  | SMLSLD | SMLSLDX | SMMLA | SMMLAR | SMMLS | SMMLSR
  | SMMUL | SMMULR | SMUAD | SMUADX | SMULBB | SMULBT
  | SMULTB | SMULTT | SMULL | SMULWB | SMULWT | SMUSD
  | SMUSDX | SSAT | SSAT16 | SSAX | SSUB16 | SSUB8
  | SUB | SUBS | SUBW | SXTAB | SXTAB16 | SXTAH | SXTB
  | SXTB16 | SXTH | UADD16 | UADD8 | UASX | UDIV | UHADD16
  | UHADD8 | UHASX | UHSAX | UHSUB16 | UHSUB8 | UMAAL
  | UMLAL | UMULL | UQADD16 | UQADD8 | UQASX | UQSAX
  | UQSUB16 | UQSUB8 | USAD8 | USADA8 | USAT | USAT16
  | USAX | USUB16 | USUB8 | UXTAB | UXTAB16 | UXTAH
  | UXTB | UXTB16 | UXTH | VMUL | VNMUL | VMLA | VMLS
  | VNMLS | VNMLA | VADD | VSUB | VDIV | VABS | VNEG
  | VSQRT | VRHADD | VADDL | VRADDHN | VMAX
  (* AArch64 *)
  | ABS | ADDG | ADDPT | ADRP
and arm_logicop =
  | BIC | BICS | EOR | EORS | ORN | ORNS | ORR | ORRS
  | PKHBT | PKHTB | RBIT | REV | REV16 | REVSH | SBFX
  | UBFX
and arm_rolop =
  | ASR | ASRS | LSL | LSLS | LSR | LSRS | ROR | RORS | RRX | RRXS
and arm_assignop =
  | BFC | BFI | CPY | LDM | STM | LDMDB | LDMEA | LDMIA | LDMFD
  | LDR | LDRB | LDRBT | LDRD | LDREX | LDREXB | LDREXD
  | LDREXH | LDRH | LDRHT | LDRSB | LDRSBT | LDRSH | LDRSHT
  | LDRT | MOV | MOVS | MOVW | MOVT | MRS | MSR | MVN | MVNS
  | SEL | STMDB | STMFD | STMIA | STMEA | STR | STRB | STRBT
  | STRD | STREX | STREXB | STREXD | STREXH | STRH | STRHT
  | STRT | VCVT | VCVTT | VCVTR | VCVTB | VMOV | VMSR
  | VSTR | VSTM | VSTMDB | VPUSH | VLDR | VLDM | VLDMDB
  | VLD4 | VSTMIA | VLDMIA | VMRS
  (* AArch64 *)
  | STP | LDP
and arm_compareop =
  | CMN | CMP | IT | TEQ | TST | VCMPE | VCMP | ITE | ITT
  | ITTT | ITTE | ITEE | ITET | ITTTT | ITTTE | ITTET | ITTEE
  | ITETT | ITETE | ITEET | ITEEE
and arm_otherop = NOP | HLT | NOPW | NOPL | UD2
and arm_condsuff =
  | EQ | NE | CS | CC | MI | PL | VS | VC | LO
  | HI | LS | GE | LT | GT | LE | AL | HS
and arm_opqualifier =
  | W | N | F32 | F64 | U8 | U16 | U32 | S8 | S16 | S32 | I16
  | I8 (* not sure if I8 or 8 *)
and arm_suffix =
  | Arm_Condsuff of arm_condsuff
  | Arm_Opqualifier of arm_opqualifier
and arm_reg =
  | Arm_CommonReg of arm_commonreg
  | Arm_SpecialReg of arm_specialreg
  | Arm_StackReg of arm_stackreg
  | Arm_PCReg of arm_pcreg
  | Arm_LinkReg of arm_linkreg
and arm_commonreg =
  | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12
  | W0 | W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W10 | W11 | W12
  | W13 | W14 | W15 | W16 | W17 | W18 | W19 | W20 | W21 | W22 | W23 | W24
  | W25 | W26 | W27 | W28 | W29 | W30
  | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 | X10 | X11 | X12
  | X13 | X14 | X15 | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23 | X24
  | X25 | X26 | X27 | X28
and arm_specialreg =
  | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
  | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15
  | D16 | D17 | D18 | D19 | D20 | D21 | D22 | D23
  | D24 | D25 | D26 | D27 | D28 | D29 | D30 | D31
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
  | S8 | S9 | S10 | S11 | S12 | S13 | S14 | S15
  | S16 | S17 | S18 | S19 | S20 | S21 | S22 | S23
  | S24 | S25 | S26 | S27 | S28 | S29 | S30 | S31
  | Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7
  | Q8 | Q9 | Q10 | Q11 | Q12 | Q13 | Q14 | Q15
  | C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7
  | C8 | C9 | C10 | C11 | C12 | C13 | C14 | C15
and arm_stackreg = R13 | SP | FP | SB | SL | X29 (* FP *) | X31 (* SP *)
and arm_linkreg = R14 | LR | IP | X30 (* LR *)
and arm_pcreg = R15 | PC
  [@@deriving show { with_path = false }]

(* customize deriving show_* *)
let show_intel_op = function
  | Intel_CommonOP icommon ->
    begin
      match icommon with
      | Intel_Arithm icommon_iar ->
        begin
          match icommon_iar with
            | ADC -> "adc" | ADD -> "add" | XADD -> "xadd" | SUB -> "sub" | ADDL -> "addl" | ADDQ -> "addq" | ADCQ -> "adcq"
            | SUBL -> "subl" | SUBQ -> "subq" | PSUBQ -> "psubq"
            | MUL -> "mul" | IMUL -> "imul" | MULB -> "mulb" | MULSD -> "mulsd" | DIV -> "div" | IDIV -> "idiv" | DIVL -> "divl" | ADCL -> "adcl" | IDIVL -> "idivl"
            | DIVSD -> "divsd" |DIVSS -> "divss" | MULSS -> "mulss" | DIVQ -> "divq" | IDIVQ -> "idivq" | PMULUDQ -> "pmuludq"
            | INC -> "inc" | INCQ -> "incq" | INCL -> "incl" | INCW -> "incw" | DEC -> "dec" | NEG -> "neg" | SBB -> "sbb" | FADD -> "fadd" | NEGL -> "negl" | FMUL -> "fmul"
            | FXCH -> "fxch" | FUCOMIP -> "fucomip" | FUCOMI -> "fucomi" | FCOMPP -> "fcompp" | FCOMPL -> "fcompl" | FCOMIP -> "fcomip" | BSR -> "bsr" | MULL -> "mull"
            | FMULL -> "fmul" | UCOMISD -> "ucomisd" | UCOMISS -> "ucomiss" | SUBSS -> "subss"
            | ADDW -> "addw" | ADDSD -> "addsd" | ADDSS -> "addss" | FMULP -> "fmulp" | FMULS -> "fmuls" | FADDS -> "fadds" | FADDP -> "faddp" | FADDL -> "faddl"
            | SUBW -> "subw" | SUBSD -> "subsd" | IMULL -> "imull" | BSWAP -> "bswap" | DECL -> "decl" | DECB -> "decb" | DECD -> "decd" | DECW -> "decw"
            | FDIV -> "fdiv" | FDIVL -> "fdivl"
            | ADDB -> "addb" | SUBB -> "subb" | SBBL -> "sbbl" | FDIVR -> "fdivr" | FABS -> "fabs" | FSQRT -> "fsqrt" | FDIVRS -> "fdivrs" | CBTW -> "cbtw"
            | FRNDINT -> "frndint" | FDIVRL -> "fdivrl" | FPREM -> "fprem" | CVTSI2SD -> "cvtsi2sd" | CVTSI2SDL -> "cvtsi2sdl" | CVTSI2SSL -> "cvtsi2ssl"
            | CVTSS2SD -> "cvtss2sd" | CVTDQ2PS -> "cvtdq2ps"
            | CVTSI2SS -> "cvtsi2ss" | CVTTSD2SI -> "cvttsd2si" | CVTTSS2SI -> "cvttss2si" | CVTSI2SDQ -> "cvtsi2sdq" | CVTPS2PD -> "cvtps2pd"
            | MAXSD -> "maxsd" | NEGQ -> "negq" | UNPCKLPS -> "unpcklps" | UNPCKLPD -> "unpcklpd" | CVTPD2PS -> "cvtpd2ps" | CVTSD2SS -> "cvtsd2ss"
            | SQRTSS -> "sqrtss" | MAXSS -> "maxss" | MINSD -> "minsd" | SQRTSD -> "sqrtsd" | MINSS -> "minss" | CVTTPS2DQ -> "cvttps2dq"
            | DECQ -> "decq" | SUBPD -> "subpd" | ADDPD -> "addpd" | PADDQ -> "paddq" | IMULQ -> "imulq" | PADDD -> "paddd" | PADDB -> "paddb"
            | PSUBD -> "psubd" | PSUBW -> "psubw" | PSUBB -> "psubb" | MULPD -> "mulpd" | UNPCKHPD -> "unpckhpd" | ADDPS -> "addps" | MULPS -> "mulps"
            | VPSUBD -> "vpsubd" | VPSUBW -> "vpsubw" | VPSUBB -> "vpsubb" | MULQ -> "mulq"
            | DIVPD -> "divpd" | DIVPS -> "divps" | CQTO -> "cqto" | INCB -> "incb" | PSUBUSW -> "psubusw"
            | PCLMULLQLQDQ -> "pclmullqlqdq" | VPCLMULLQLQDQ -> "vpclmullqlqdq"
            | PCLMULHQLQDQ -> "pclmulhqlqdq" | VPCLMULHQLQDQ -> "vpclmulhqlqdq"
            | PCLMULLQHQDQ -> "pclmullqhqdq" | VPCLMULLQHQDQ -> "vpclmullqhqdq"
            | PCLMULHQHQDQ -> "pclmulhqhqdq" | VPCLMULHQHQDQ -> "vpclmulhqhqdq"
            | BZHI -> "bzhi" | VPADDB -> "VPADDB"
        end
      | Intel_Logic icommon_il ->
        begin
          match icommon_il with
            | AND -> "and" | ANDB -> "andb" | OR -> "or" | XOR -> "xor" | PXOR -> "pxor" | NOT -> "not" | ANDL -> "andl" | NOTL -> "notl" | ORW -> "orw"
            | XORB -> "xorb" | XORL -> "xor" | SAHF -> "sahf" | ANDW -> "andw" | NOTB -> "notb" | NOTW -> "notw" | XORPD -> "xorpd" | XORPS -> "xorps" | ANDQ -> "andq"
            | XORQ -> "xorq" | ANDPS -> "andps" | ANDNPS -> "andnps" | ORPS -> "orps" | ANDPD -> "andpd" | NOTQ -> "notq" | ANDNPD -> "andnpd"
            | ORPD -> "orpd" | PAND -> "pand" | POR -> "por" | PANDN -> "pandn" | VPXOR -> "vpxor" | VPXORD -> "vpxord" | VPXORQ -> "vpxorq" | VPORQ -> "vporq" 
            | VPAND -> "vpand" | VPTERNLOGD -> "vpternlogd"  | VPTESTMB -> "vptestmb" | VPTESTNMB -> "vptestnmb" | SHLX -> "shlx"
            | KORTESTD -> "kortestd"  | KORD -> "kord" | ANDN -> "andn" | KORTESTQ -> "kortestq" | BLSR -> "blsr" | VPTESTNMD -> "vptestnmd" | KXNORQ -> "kxnorq"
        end
      | Intel_Rol icommon_ir ->
        begin
          match icommon_ir with
            | ROL -> "rol" | SHL -> "shl" | SHR -> "shr" | SHLD -> "shld" |SHRD -> "shrd" | SHRL -> "shrl" | ROR -> "ror" | RORL -> "rorl"
            | SAL -> "sal" | SAR -> "sar" | SHLL -> "shll" | ROLL -> "roll" | SHRB -> "shrb" | SHLB -> "shlb" | SARL -> "sarl" | ROLW -> "rolw" | SHLW -> "shlw"
            | SARW -> "sarw" | SHRW -> "shrw" | SHLQ -> "shlq" | SHRQ -> "shrq" | SHUFPS -> "shufps" | SHUFPD -> "shufpd" | SHRX -> "shrx"
            | PSLLW -> "psllw" | PSLLD -> "pslld" | PSLLQ -> "psllq" | PSRAW -> "psraw" | PSRAD -> "psrad" | PSLLDQ -> "pslldq" | PSRLDQ -> "psrld"| PSRLD -> "psrld"
            | PSHUFLW -> "pshuflw" | PSHUFB -> "pshufb" | PSHUFW -> "pshufw" | PSHUFHW -> "pshufhw" | PSHUFD -> "pshufd"
            | VPSHUFLW -> "vpshuflw" | VPSHUFB -> "vpshufb" | VPSHUFW -> "vpshufw" | VPSHUFHW -> "vpshufhw" | VPSHUFD -> "vpshufd"
            | VPSRLDQ -> "vpsrldq" | VPSLLDQ -> "vpslldq"
            | SARQ -> "sarq" | SARX ->"sarx" 
        end
      | Intel_Assign icommon_ias ->
        begin
          match icommon_ias with
            | MOV -> "mov" | XCHG -> "xchg" | LEA -> "lea" | LEAL -> "leal" | LEAQ -> "leaq" | MOVSX -> "movsx" | MOVSD -> "movsd" | MOVL -> "movl" | FLDL -> "fldl" | MOVZBL -> "movzbl" | MOVZBW -> "movzbw"
            | MOVSW -> "movsw" | MOVAPD -> "movapd" | MOVSLQ -> "movslq" | MOVQ -> "movq" | MOVABS -> "movabs" | MOVSBQ -> "movsbq" | MOVBE -> "movbe" | MOVNTDQ -> "movntdq" | LDDQU -> "lddqu"
            | MOVW -> "movw" | MOVZX -> "movzx" | MOVAPS -> "movaps" | VMOVAPS -> "vmovaps" | FLD -> "fld" | FSTP -> "fstp" | CMOVAE -> "cmovae" | CMOVE -> "cmove" | CMOVNE -> "cmovne" | MOVSS -> "movss"
            | CMOVBE -> "cmovbe" | CMOVB -> "cmovb" | CMOVS -> "cmovs" | CMOVA -> "cmova" | CMOVNS -> "cmovns" | MOVB -> "movb"
            | MOVZWL -> "movzwl" | MOVSWL -> "movswl" | MOVSBL -> "movsbl" | MOVSBW -> "movsbw" | FLDT -> "fldt" | FSTPT -> "fstpt" | ORL -> "orl" | ORB -> "orb" | MOVSB -> "movsb"
            | FNSTCW -> "fnstcw" | FLDCW -> "fldcw" | FLDZ -> "fldz" | REPZ -> "repz" | REPE -> "repe" | FSTPL -> "fstpl" | REPNZ -> "repnz"
            | REP -> "rep" | FNSTSW -> "fnstsw" | CMOVLE -> "cmovle" | CMOVG -> "cmovg" | CMOVL -> "cmovl" | FILDLL -> "fildll"
            | FLDS -> "flds" | FILDS -> "filds" | FISTPS -> "fistps" | FILDL -> "fildl" | FLD1 -> "fld1" | FDIVP -> "fdivp" | FSTL -> "fstl" | FISTPL -> "fistpl" | FILD -> "fild"
            | FSUB -> "fsub" | FDIVS -> "fdivs" | FISTPLL -> "fistpll" | FDIVRP -> "fdivrp" | CMOVGE -> "cmovge" | FCMOVBE -> "fcmovbe" | MOVNTPS -> "movntps"
            | FSUBP -> "fsubp" | FISTL -> "fistl" | FSUBRP -> "fsubrp" | FSUBRL -> "fsubrl" | CWTL -> "cwtl" | FSUBRS -> "fsubrs" | FSTPS -> "fstps"
            | FSUBS -> "fsubs" | FSUBR -> "fsubr" | FSTS -> "fsts" | FSUBL -> "fsubl" | FCMOVNBE -> "fcmovnbe" | FCMOVE -> "fcmove" | FCMOVNE -> "fcmovne"
            | FCMOVB -> "fcmovb" | FISTP -> "fistp" | FCMOVNB -> "fcmovnb" | CMOVNP -> "cmovnp" | STOS -> "stos" | STOSB -> "stosb" | STOSW -> "stosw" | STOSD -> "stosd"
            | FIST -> "fist" | FFREE -> "ffree" | MOVSWQ -> "movswq" | ORQ -> "orq" | MOVDQU -> "movdqu" | VMOVDQU -> "vmovdqu" | MOVDQA -> "movdqa" | VMOVDQA -> "vmovdqa"
            | VMOVDQA32 -> "vmovdqa32" | VMOVDQA64 -> "vmovdqa64" | VMOVNTDQ -> "vmovntdq"
            | MOVUPS -> "movups" | VMOVUPS -> "vmovups" | MOVD -> "movd" | VMOVD -> "vmovd" | VMOVQ -> "vmovq" | MOVHLPS -> "movhlps" | MOVLHPS -> "movlhps" | MOVUPD -> "movupd"
            | PUNPCKHQDQ -> "punpckhqdq" | PUNPCKLDQ -> "punpckldq" | PUNPCKLBW -> "punpcklbw" | PEXTRW -> "pextrw"
            | VPUNPCKLDQ -> "vpunpckldq" | VPEXTRW -> "vpextrw"
            | PINSRB -> "pinsrb" | VPINSRB -> "vpinsrb"
            | PINSRD -> "pinsrd" | VPINSRD -> "vpinsrd"
            | PINSRQ -> "pinsrq" | VPINSRQ -> "vpinsrq"
            | PINSRW -> "pinsrw" | VPINSRW -> "vpinsrw"
            | PUNPCKLQDQ -> "punpcklqdq" | PUNPCKLWD -> "punpcklwd" | VPUNPCKLWD -> "vpunpcklwd" | MOVHPD -> "movhpd" | MOVHPS -> "movhps" | MOVLPD -> "movlpd"
            | VPUNPCKLQDQ -> "vpunpcklqdq" | VMOVHPD -> "vmovhpd" | VMOVHPS -> "vmovhps" | PUNPCKHDQ -> "punpckhdq"
            | KUNPCKDQ -> "kunpckdq" | KUNPCKBW -> "kunpckbw" 
            | LAHF -> "lahf" | SAHF -> "sahf"
            | VINSERTI128 -> "vinserti128" | VEXTRACTI128 -> "vextracti128"
            | PSADBW -> "psadbw" | VPSADBW -> "vpsadbw"
            | BSF -> "bsf"
            | XGETBV -> "xgetbv" | PMOVMSKB -> "pmovmskb" | VPMOVMSKB -> "vpmovmskb" | PMINUB -> "pminub" | PMINUD -> "pminud" | VPMINUB -> "vpminub" | VPMINUD -> "vpminud" | PALIGNR -> "palignr" | VPALIGNR -> "vpalignr" | TZCNT -> "tzcnt" | VZEROUPPER -> "vzeroupper" | PCMPISTRI -> "pcmpistri"
            | PMAXUB -> "pmaxub" | VPMAXUB -> "vpmaxub" | VPBROADCASTB -> "vpbroadcastb" | VPBROADCASTD -> "vpbroadcastd" | VPBROADCASTQ -> "vpbroadcastq" | VBROADCASTSS -> "vbroadcastss" | VPOR -> "vpor" | VPAND -> "vpand" | VPANDN -> "vpandn" | VPTEST -> "vptest"
            | VPCMPEQB -> "vpcmpeqb" | VPCMPEQD -> "vpcmpeqd" | VPCMPEQQ -> "vpcmpeqq" | VPCMPGTB -> "vpcmpgtb" | VPCMPISTRI -> "vpcmpistri"
            | VPSHUFB -> "vpshufb" | VPSLLDQ -> "vpslldq" | VPSRLDQ -> "vpsrldq" | VPSUBB -> "vpsubb" | BTS -> "bts" | STD -> "std" | CLD -> "cld" | RDSSPD -> "rdsspd" | RDSSPQ -> "rdsspq" | MOVMSKPS -> "movmskps" | MOVMSKPD -> "movmskpd" | BNDMOV -> "bndmov"
            | FXSAVE -> "fxsave" | FXAM -> "fxam" | FXRSTOR -> "fxrstor" | XRSTOR -> "xrstor" | XSAVE -> "xsave" | XSAVEC -> "xsavec" | INCSSPQ -> "incsspq" | BTR -> "btr" | FLDENV -> "fldenv" | FNSTENV -> "fnstenv" | STMXCSR -> "stmxcsr" | INCSSPD -> "incsspd" | LDMXCSR -> "ldmxcsr"
            | VINSERTI64X4 -> "vinserti64x4" | VINSERTI32X4 -> "vinserti32x4" | VINSERTI16X4 -> "vinserti16x4" | VINSERTI8X4 -> "vinserti8x4"
            | VMOVDQU64 -> "vmovdqu64" | VMOVDQU32 -> "vmovdqu32" | VMOVDQU16 -> "vmovdqu16" | VMOVDQU8 -> "vmovdqu8"
            | KMOVQ -> "kmovq" | KMOVD -> "kmovd" | KMOVW -> "kmovw" | KMOVB -> "kmovb" | VPCMPLTUB -> "vpcmpltub"
            | POPCNT -> "popcnt" | RDTSC -> "rdtsc" | BLSMSK -> "blsmsk" | RSTORSSP -> "rstorssp" | SAVEPREVSSP -> "saveprevssp"
        end
      | Intel_Compare icommon_ic ->
        begin
        match icommon_ic with
          | CMP -> "cmp" | CMPQ -> "cmpq" | TEST -> "test" | CMPL -> "cmpl" | CMPB -> "cmpb" | CMPW -> "cmpw" | TESTB -> "testb" | TESTL -> "testl" | CMPSB -> "cmpsb" | PTEST -> "ptest"
          | BT -> "bt" | BTC -> "btc" | BTCQ -> "btcq"
          | TESTW -> "testw" | CMPNLESS -> "cmpnless" | CMPNLESD -> "cmpnlesd" | CMPLTSS -> "cmpltss" | CMPNLTSS -> "cmpnltss" | TESTQ -> "testq"
          | CMPNLTSD -> "cmpnltsd" | PCMPGTD -> "pcmpgtd" | PCMPGTB -> "pcmpgtb" | PCMPEQD -> "pcmpeqd" | VPCMPEQD -> "vpcmpeqd" | PCMPEQB -> "pcmpeqb" | VPCMPEQB | CMPLTSD -> "cmpltsd" | PCMPEQW -> "pcmpeqw"
          | CMPEQSS -> "cmpeqss" | FCOMI -> "fcomi" | COMISS -> "comiss" | COMISD -> "comisd" | CMPXCHG -> "cmpxchg"
          | XTEST -> "xtest"
          | VPCMPNEQUB -> "vpcmpnequb" | VPCMPNEQB -> "vpcmpneqb" | VPCMPNEQD -> "vpcmpneqd" 
        end
      | Intel_Set icommon_is ->
        begin
        match icommon_is with
          | SETA -> "seta" | SETAE -> "setae" | SETB -> "setb" | SETBE -> "setbe" | SETC -> "setc" | SETO -> "seto"
          | SETNBE -> "setnbe" | SETNC -> "setnc" | SETNG -> "setng" | SETNE -> "setne"
          | SETE -> "sete" | SETNP -> "setnp" | SETGE -> "setge" | SETG -> "setg" | SETLE -> "setle"
          | SETL -> "setl" | SETP -> "setp" | SETNS -> "setns" | SETS -> "sets"
        end
      | Intel_Other icommon_io ->
        begin
        match icommon_io with
          | NOP -> "nop" | HLT -> "hlt" | NOPW -> "nopw" | NOPL -> "nopl" | UD2 -> "ud2" | ENDBR32 -> "endbr32" | ENDBR64 -> "endbr64" | CPUID -> "cpuid" | PAUSE ->"pause" | XEND -> "xend"
          | VZEROALL -> "vzeroall"
        end
    end
  | Assist a ->
    begin
    match a with
      | SCAS -> "scas" | MOVSL -> "movsl" | MOVSB -> "movsb" | CMPSW -> "cmpsw" | CMPSB -> "cmpsb" | MOVSQ -> "movsq" | POP -> "pop"
    end
  | Intel_StackOP is ->
    begin
    match is with
      | PUSH -> "push" | POP -> "pop" | PUSHL -> "pushl" | POPL -> "popl" | PUSHF -> "pushf" | POPF -> "popf" | PUSHQ -> "pushq" | POPQ -> "popq"
    end
  | Intel_ControlOP icontrol ->
    begin
    match icontrol with
      | CALL -> "call" | CALLQ -> "callq" | LEAVE -> "leave" | LEAVEQ -> "leaveq"
      | RET -> "ret" | RETN -> "retn" | RETQ -> "retq"
      | FXAM -> "fxam" | FCHS -> "fchs"
      | Intel_Jump icontrol_ij ->
        begin
        match icontrol_ij with
          | JMP -> "jmp" | JNE -> "jne" | JE -> "je" | JB -> "jb" | JNAE -> "jnae" | JNP -> "jnp" | JNO -> "jno"
          | JC -> "jc" | JNB -> "jnb" | JAE -> "jae" | JNC -> "jnc" | JBE -> "jbe" | JNA -> "jna"
          | JA -> "ja" | JNBE -> "jnbe" | JL -> "jl" | JNGE -> "jnge" | JGE -> "jge" | JNL -> "jnl" | JLE -> "jle"
          | JNG -> "jng" | JG -> "jg" | JNLE -> "jnle" | JS -> "js" | JNS -> "jns" | JP -> "jp" | JMPQ -> "jmpq"
          | JZ -> "jz" | JNZ -> "jnz" | JPE -> "jpe" | JO -> "jo" | JRCXZ -> "jrcxz" | JECXZ -> "jecxz" | XBEGIN -> "xbegin"
        end
      | Intel_Loop icontrol_il ->
        begin
        match icontrol_il with
          | LOOP -> "loop" | LOOPE -> "loope" |LOOPNE -> "loopne"
        end
      | Intel_Flag icontrol_if ->
        begin
        match icontrol_if with
          | CLD -> "cld" | CLTD -> "cltd" | CLTQ -> "cltq"
        end
    end
  | Intel_SystemOP isystem ->
    begin
    match isystem with
      | INT -> "int" | IN -> "in" | OUT -> "out" | SYSCALL -> "syscall" | XABORT -> "xabort" | PREFETCHNTA -> "prefetchnta" | PREFETCHT0 -> "prefetcht0" | PREFETCHT1 -> "prefetcht1" | SFENCE -> "sfence" | LFENCE -> "lfence" | FWAIT -> "fwait"
    end
  | Intel_ErrorOP ierror ->
    begin
    match ierror with
    | BAD -> "BAD"
    end

let show_intel_reg = function
  | Intel_CommonReg i_cr ->
    begin
      match i_cr with
        | RAX -> "rax" | RBX -> "rbx" | RCX -> "rcx" | RDX -> "rdx" | RDI -> "rdi" | RSI -> "rsi"
        | EAX -> "eax" | EBX -> "ebx" | ECX -> "ecx" | EDX -> "edx" | EDI -> "edi" | ESI -> "esi"
        | AX -> "ax" | BX -> "bx" | CX -> "cx" | DX -> "dx" | AL -> "al" | BL -> "bl" | CL -> "cl" | DL -> "dl"
        | AH -> "ah" | BH -> "bh" | CH -> "ch" | DH -> "dh"
    end
  | Intel_SpecialReg i_spr ->
    begin
      match i_spr with
        | R8 -> "r8" | R9 -> "r9" | R10 -> "r10" | R11 -> "r11" | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15"
        | R8D -> "r8d" | R9D -> "r9d" | R10D -> "r10d" | R11D -> "r11d" | R12D -> "r12d" | R13D -> "r13d" | R14D -> "r14d" | R15D -> "r15d"
        | R8W -> "r8w" | R9W -> "r9w" | R10W -> "r10w" | R11W -> "r11w" | R12W -> "r12w" | R13W -> "r13w" | R14W -> "r14w" | R15W -> "r15w"
        | R8B -> "r8b" | R9B -> "r9b" | R10B -> "r10b" | R11B -> "r11b" | R12B -> "r12b" | R13B -> "r13b" | R14B -> "r14b" | R15B -> "r15b"
        | XMM0 -> "xmm0" | XMM1 -> "xmm1" | XMM2 -> "xmm2" | XMM3 -> "xmm3" | XMM4 -> "xmm4" | XMM5 -> "xmm5" | XMM6 -> "xmm6" | XMM7 -> "xmm7"
        | YMM0 -> "ymm0" | YMM1 -> "ymm1" | YMM2 -> "ymm2" | YMM3 -> "ymm3" | YMM4 -> "ymm4" | YMM5 -> "ymm5" | YMM6 -> "ymm6" | YMM7 -> "ymm7"
    end
  | Intel_StackReg i_str ->
    begin
      match i_str with
        | RBP -> "rbp" | RSP -> "rsp"
        | ESP -> "esp" | EBP -> "ebp"
    end
  | Intel_PCReg i_pr ->
    begin
      match i_pr with
        | EIP -> "eip" | RIP -> "rip"
    end
  | Intel_OtherReg i_or ->
    begin
      match i_or with
      | EIZ -> "eiz"
    end

  let show_intel_seg = function
    | FS -> "fs" | GS -> "gs" | CS -> "cs" | SS -> "ss" | DS -> "ds" | ES -> "es"

  let show_intel_ptrtyp = function
    | QWORD -> "qword" | DWORD -> "dword" | WORD -> "word" | TBYTE -> "tbyte" | BYTE -> "byte"

  let show_assistop = function
    | SCAS -> "scas" | MOVSL -> "movsl" | MOVSB -> "movsb" | CMPSW -> "cmpsw" | CMPSB -> "cmpsb" | MOVSQ -> "movsq" | POP -> "pop"

  let show_intel_mathop = function
    | MATHADD -> "mathadd" | MATHSUB -> "mathsub"

  let show_arm_op = function
    | Arm_CommonOP acommon ->
      begin
        match acommon with
        | Arm_Arithm acommon_arith ->
          begin
            match acommon_arith with
            | ADC -> "adc" | ADCS -> "adcs" | ADD -> "add" | ADDS -> "adds" | ADDW -> "addw" | ADR -> "adr" | AND -> "and" | ANDS -> "ands"
            | ANDEQ -> "andeq"
            | CLZ -> "clz" | MLA -> "mla" | MLS -> "mls" | MUL -> "mul" | NEG -> "neg" | QADD -> "qadd" | QADD16 -> "qadd16" | QADD8 -> "qadd8"
            | QASX -> "qasx" | QDADD -> "qdadd" | QDSUB -> "qdsub" | QSAX -> "qsax" | QSUB -> "qsub" | QSUB16 -> "qsub16" | QSUB8 -> "qsub8"
            | RSB -> "rsb" | RSBS -> "rsbs" | SADD16 -> "sadd16" | SADD8 -> "sadd8" | SASX -> "sasx" | SBC -> "sbc" | SBCS -> "sbcs"
            | SDIV -> "sdiv" | SHADD16 -> "shadd16" | SHADD8 -> "shadd8" | SHASX -> "shasx" | SHSAX -> "shsax" | SHSUB16 -> "shsub16"
            | SHSUB8 -> "shsub8" | SMLABB -> "smlabb" | SMLABT -> "smlabt" | SMLATB -> "smlatb" | SMLATT -> "smlatt" | SMLAD -> "smlad"
            | SMLADX -> "smladx" | SMLAL -> "smlal" | SMLALBB -> "smlalbb" | SMLALBT -> "smlalbt" | SMLALTB -> "smlaltb" | SMLALTT -> "smlaltt"
            | SMLALD -> "smlald" | SMLALDX -> "smlaldx" | SMLAWB -> "smlawb" | SMLAWT -> "smlawt" | SMLSD -> "smlsd" | SMLSDX -> "smlsdx"
            | SMLSLD -> "smlsld" | SMLSLDX -> "smlsldx" | SMMLA -> "smmla" | SMMLAR -> "smmlar" | SMMLS -> "smmls" | SMMLSR -> "smmlsr"
            | SMMUL -> "smmul" | SMMULR -> "smmulr" | SMUAD -> "smuad" | SMUADX -> "smuadx" | SMULBB -> "smulbb" | SMULBT -> "smulbt"
            | SMULTB -> "smultb" | SMULTT -> "smultt" | SMULL -> "smull" | SMULWB -> "smulwb" | SMULWT -> "smulwt" | SMUSD -> "smusd"
            | SMUSDX -> "smusdx" | SSAT -> "ssat" | SSAT16 -> "ssat16" | SSAX -> "ssax" | SSUB16 -> "ssub16" | SSUB8 -> "ssub8"
            | SUB -> "sub" | SUBS -> "subs" | SUBW -> "subw" | SXTAB -> "sxtab" | SXTAB16 -> "sxtab16" | SXTAH -> "sxtah" | SXTB -> "sxtb"
            | SXTB16 -> "sxtb16" | SXTH -> "sxth" | UADD16 -> "uadd16" | UADD8 -> "uadd8" | UASX -> "uasx" | UDIV -> "udiv" | UHADD16 -> "uhadd16"
            | UHADD8 -> "uhadd8" | UHASX -> "uhasx" | UHSAX -> "uhsax" | UHSUB16 -> "uhsub16" | UHSUB8 -> "uhsub8" | UMAAL -> "umaal"
            | UMLAL -> "umlal" | UMULL -> "umull" | UQADD16 -> "uqadd16" | UQADD8 -> "uqadd8" | UQASX -> "uqasx" | UQSAX -> "uqsax"
            | UQSUB16 -> "uqsub16" | UQSUB8 -> "uqsub8" | USAD8 -> "usad8" | USADA8 -> "usada8" | USAT -> "usat" | USAT16 -> "usat16"
            | USAX -> "usax" | USUB16 -> "usub16" | USUB8 -> "usub8" | UXTAB -> "uxtab" | UXTAB16 -> "uxtab16" | UXTAH -> "uxtah"
            | UXTB -> "uxtb" | UXTB16 -> "uxtb16" | UXTH -> "uxth" | VMUL -> "vmul" | VNMUL -> "vnmul" | VMLA -> "vmla" | VMLS -> "vmls"
            | VNMLS -> "vnmls" | VNMLA -> "vnmla" | VADD -> "vadd" | VSUB -> "vsub" | VDIV -> "vdiv" | VABS -> "vabs" | VNEG -> "vneg"
            | VSQRT -> "vsqrt" | VRHADD -> "vrhadd" | VADDL -> "vaddl" | VRADDHN -> "vraddhn" | VMAX -> "vmax"
            | ABS -> "abs" | ADDG -> "addg" | ADDPT -> "addpt" | ADRP -> "adrp"
          end
        | Arm_Logic acommon_logic ->
          begin
            match acommon_logic with
            | BIC -> "bic" | BICS -> "bics" | EOR -> "eor" | EORS -> "eors" | ORN -> "orn" | ORNS -> "orns" | ORR -> "orr" | ORRS -> "orrs"
            | PKHBT -> "pkhbt" | PKHTB -> "pkhtb" | RBIT -> "rbit" | REV -> "rev" | REV16 -> "rev16" | REVSH -> "revsh" | SBFX -> "sbfx"
            | UBFX -> "ubfx"
          end
        | Arm_Rol acommon_rol->
          begin
            match acommon_rol with
            | ASR -> "asr" | ASRS -> "asrs" | LSL -> "lsl" | LSLS -> "lsls" | LSR -> "lsr" | LSRS -> "lsrs" | ROR -> "ror" | RORS -> "rors" | RRX -> "rrx" | RRXS -> "rrxs"
          end
        | Arm_Assign acommon_assign ->
          begin
            match acommon_assign with
            | BFC -> "bfc" | BFI -> "bfi" | CPY -> "cpy" | LDM -> "ldm" | STM -> "stm" | LDMDB -> "ldmdb" | LDMEA -> "ldmea" | LDMIA -> "ldmia" | LDMFD -> "ldmfd"
            | LDR -> "ldr" | LDRB -> "ldrb" | LDRBT -> "ldrbt" | LDRD -> "ldrd" | LDREX -> "ldrex" | LDREXB -> "ldrexb" | LDREXD -> "ldrexd"
            | LDREXH -> "ldrexh" | LDRH -> "ldrh" | LDRHT -> "ldrht" | LDRSB -> "ldrsb" | LDRSBT -> "ldrsbt" | LDRSH -> "ldrsh" | LDRSHT -> "ldrsht"
            | LDRT -> "ldrt" | MOV -> "mov" | MOVS -> "movs" | MOVW -> "movw" | MOVT -> "movt" | MRS -> "mrs" | MSR -> "msr" | MVN -> "mvn" | MVNS -> "mvns"
            | SEL -> "sel" | STMDB -> "stmdb" | STMFD -> "stmfd" | STMIA -> "stmia" | STMEA -> "stmea" | STR -> "str" | STRB -> "strb" | STRBT -> "strbt"
            | STRD -> "strd" | STREX -> "strex" | STREXB -> "strexb" | STREXD -> "strexd" | STREXH -> "strexh" | STRH -> "strh" | STRHT -> "strht"
            | STRT -> "strt" | VCVT -> "vcvt" | VCVTT -> "vcvtt" | VCVTR -> "vcvtr" | VCVTB -> "vcvtb" | VMOV -> "vmov" | VMSR -> "vmsr"
            | VSTR -> "vstr" | VSTM -> "vstm" | VSTMDB -> "vstmdb" | VPUSH -> "vpush" | VLDR -> "vldr" | VLDM -> "vldm" | VLDMDB -> "vldmdb"
            | VLD4 -> "vld4" | VSTMIA -> "vstmia" | VLDMIA -> "vldmia" | VMRS -> "vmrs"
            | STP -> "stp" | LDP -> "ldp"
          end
        | Arm_Compare acommon_compare ->
          begin
            match acommon_compare with
            | CMN -> "cmn" | CMP -> "cmp" | IT -> "it" | TEQ -> "teq" | TST -> "tst" | VCMPE -> "vcmpe" | VCMP -> "vcmp" | ITE -> "ite" | ITT -> "itt"
            | ITTT -> "ittt" | ITTE -> "itte" | ITEE -> "itee" | ITET -> "itet" | ITTTT -> "itttt" | ITTTE -> "ittte" | ITTET -> "ittet" | ITTEE -> "ittee"
            | ITETT -> "itett" | ITETE -> "itete" | ITEET -> "iteet" | ITEEE -> "iteee"
          end
        | Arm_Other acommon_other ->
          begin
            match acommon_other with
            | NOP -> "nop" | HLT -> "hlt" | NOPW -> "nopw" | NOPL -> "nopl" | UD2 -> "ud2"
          end
      end
    | Arm_StackOP astack ->
      begin
        match astack with
        | PUSH -> "push" | POP -> "pop" | VPUSH -> "vpush" | VPOP -> "vpop"
      end
    | Arm_ControlOP acontrol ->
      begin
        match acontrol with
        | B -> "b" | BL -> "bl" | BX -> "bx" | BLX -> "blx" | BXJ -> "bxj" | TBB -> "tbb" | TBH -> "tbh"
        | CBNZ -> "cbnz" | CBZ -> "cbz" | BEQ -> "beq" | BR -> "br" | BXEQ -> "bxeq" | BLEQ -> "bleq"
        | BCC -> "bcc" | BCS -> "bcs" | BGE -> "bge" | BGT -> "bgt" | BHI -> "bhi" | BLE -> "ble" | BLS -> "bls" | BLT -> "blt"
        | BMI -> "bmi" | BNE -> "bne" | BPL -> "bpl" | BVC -> "bvc" | BVS -> "bvs"
        | RET -> "ret" | RETAA -> "retaa" | RETAB -> "retab" | RETAASPPC -> "retaasppc" | RETABSPPC -> "retabsppc"
        | RETAASPPCR -> "retaasppcr" | RETABSPPCR -> "retabsppcr"
      end
    | Arm_SystemOP asystem ->
      begin
        match asystem with
        | BKPT -> "bkpt" | CLREX -> "clrex" | CPS -> "cps" | CPSIE -> "cpsie" | CPSID -> "cpsid" | DBG -> "dbg" | DMB -> "dmb"
        | DSB -> "dsb" | ISB -> "isb" | PLD -> "pld" | PLI -> "pli" | RFE -> "rfe" | SEV -> "sev" | SMC -> "smc" | SRS -> "srs"
        | SVC -> "svc" | WFE -> "wfe" | WFI -> "wfi" | YIELD -> "yield" | UDF -> "udf"
      end
    | Arm_ErrorOP aerror ->
      begin
        match aerror with
        | BAD -> "BAD"
      end

  let show_arm_opqualifier = function
    | W -> "w" | N -> "n" | F32 -> "f32" | F64 -> "f64" | U8 -> "u8" | U16 -> "u16" | U32 -> "u32" | S8 -> "s8" | S16 -> "s16"
    | S32 -> "s32" | I16 -> "i16" | I8 -> "i8"

  let show_arm_condsuff = function
    | EQ -> "eq" | NE -> "ne" | CS -> "cs" | CC -> "cc" | MI -> "mi" | PL -> "pl" | VS -> "vs" | VC -> "vc"
    | LO -> "lo" | HI -> "hi" | LS -> "ls" | GE -> "ge" | LT -> "lt" | GT -> "gt" | LE -> "le" | AL -> "al" | HS -> "hs"
    | _ -> failwith "Unknown ARM condsuff"

  let show_arm_reg = function
    | Arm_CommonReg acommonr ->
      begin
        match acommonr with
        | R0 -> "r0" | R1 -> "r1" | R2 -> "r2" | R3 -> "r3" | R4 -> "r4" | R5 -> "r5" | R6 -> "r6" | R7 -> "r7" | R8 -> "r8" | R9 -> "r9" | R10 -> "r10" | R11 -> "r11" | R12 -> "r12"
        | W0 -> "w0" | W1 -> "w1" | W2 -> "w2" | W3 -> "w3" | W4 -> "w4" | W5 -> "w5" | W6 -> "w6" | W7 -> "w7"
        | W8 -> "w8" | W9 -> "w9" | W10 -> "w10" | W11 -> "w11" | W12 -> "w12" | W13 -> "w13" | W14 -> "w14" | W15 -> "w15"
        | W16 -> "w16" | W17 -> "w17" | W18 -> "w18" | W19 -> "w19" | W20 -> "w20" | W21 -> "w21" | W22 -> "w22" | W23 -> "w23"
        | W24 -> "w24" | W25 -> "w25" | W26 -> "w26" | W27 -> "w27" | W28 -> "w28" | W29 -> "w29" | W30 -> "w30"
        | X0 -> "x0" | X1 -> "x1" | X2 -> "x2" | X3 -> "x3" | X4 -> "x4" | X5 -> "x5" | X6 -> "x6" | X7 -> "x7"
        | X8 -> "x8" | X9 -> "x9" | X10 -> "x10" | X11 -> "x11" | X12 -> "x12" | X13 -> "x13" | X14 -> "x14" | X15 -> "x15"
        | X16 -> "x16" | X17 -> "x17" | X18 -> "x18" | X19 -> "x19" | X20 -> "x20" | X21 -> "x21" | X22 -> "x22" | X23 -> "x23"
        | X24 -> "x24" | X25 -> "x25" | X26 -> "x26" | X27 -> "x27" | X28 -> "x28"
        | _ -> failwith "Unknown ARM common reg"
      end
    | Arm_SpecialReg aspecialr ->
      begin
        match aspecialr with
        | D0 -> "d0" | D1 -> "d1" | D2 -> "d2" | D3 -> "d3" | D4 -> "d4" | D5 -> "d5" | D6 -> "d6" | D7 -> "d7"
        | D8 -> "d8" | D9 -> "d9" | D10 -> "d10" | D11 -> "d11" | D12 -> "d12" | D13 -> "d13" | D14 -> "d14" | D15 -> "d15"
        | D16 -> "d16" | D17 -> "d17" | D18 -> "d18" | D19 -> "d19" | D20 -> "d20" | D21 -> "d21" | D22 -> "d22" | D23 -> "d23"
        | D24 -> "d24" | D25 -> "d25" | D26 -> "d26" | D27 -> "d27" | D28 -> "d28" | D29 -> "d29" | D30 -> "d30" | D31 -> "d31"
        | S0 -> "s0" | S1 -> "s1" | S2 -> "s2" | S3 -> "s3" | S4 -> "s4" | S5 -> "s5" | S6 -> "s6" | S7 -> "s7"
        | S8 -> "s8" | S9 -> "s9" | S10 -> "s10" | S11 -> "s11" | S12 -> "s12" | S13 -> "s13" | S14 -> "s14" | S15 -> "s15"
        | S16 -> "s16" | S17 -> "s17" | S18 -> "s18" | S19 -> "s19" | S20 -> "s20" | S21 -> "s21" | S22 -> "s22" | S23 -> "s23"
        | S24 -> "s24" | S25 -> "s25" | S26 -> "s26" | S27 -> "s27" | S28 -> "s28" | S29 -> "s29" | S30 -> "s30" | S31 -> "s31"
        | Q0 -> "q0" | Q1 -> "q1" | Q2 -> "q2" | Q3 -> "q3" | Q4 -> "q4" | Q5 -> "q5" | Q6 -> "q6" | Q7 -> "q7"
        | Q8 -> "q8" | Q9 -> "q9" | Q10 -> "q10" | Q11 -> "q11" | Q12 -> "q12" | Q13 -> "q13" | Q14 -> "q14" | Q15 -> "q15"
        | C0 -> "c0" | C1 -> "c1" | C2 -> "c2" | C3 -> "c3" | C4 -> "c4" | C5 -> "c5" | C6 -> "c6" | C7 -> "c7"
        | C8 -> "c8" | C9 -> "c9" | C10 -> "c10" | C11 -> "c11" | C12 -> "c12" | C13 -> "c13" | C14 -> "c14" | C15 -> "c15"
      end
    | Arm_StackReg astackr ->
      begin
        match astackr with
        | R13 -> "r13" | SP -> "sp" | FP -> "fp" | SB -> "sb" | SL -> "sl" | X29 -> "x29" | X31 -> "x31"
        | _ -> failwith "Unknown ARM stack reg"
      end
    | Arm_PCReg apcr ->
      begin
        match apcr with
        | R15 -> "r15" | PC -> "pc"
      end
    | Arm_LinkReg alinkr ->
      begin
        match alinkr with
        | R14 -> "r14" | LR -> "lr" | IP -> "ip" | X30 -> "x30"
      end
