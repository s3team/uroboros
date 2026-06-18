 OUTPUT_FORMAT("elf64-littleaarch64", "elf64-bigaarch64", "elf64-littleaarch64")
 OUTPUT_ARCH(aarch64)
ENTRY(_start)
SECTIONS
{

  . = SEGMENT_START("text-segment", (DEFINED(__wrs_rtp_base) ? __wrs_rtp_base : 0));
  PROVIDE (__executable_start = .);
  . += SIZEOF_HEADERS;

  .interp : { *(.interp) }
  .note.gnu.build-id : { *(.note.gnu.build-id) }
  .hash : { *(.hash) }
  .gnu.hash : { *(.gnu.hash) }
  .dynsym : { *(.dynsym) }
  .dynstr : { *(.dynstr) }
  .gnu.version : { *(.gnu.version) }
  .gnu.version_d : { *(.gnu.version_d) }
  .gnu.version_r : { *(.gnu.version_r) }
  .rel.dyn :
    {
      *(.rel.init)
      *(.rel.text .rel.text.* .rel.gnu.linkonce.t.*)
      *(.rel.fini)
      *(.rel.rodata .rel.rodata.* .rel.gnu.linkonce.r.*)
      *(.rel.data.rel.ro .rel.data.rel.ro.* .rel.gnu.linkonce.d.rel.ro.*)
      *(.rel.data .rel.data.* .rel.gnu.linkonce.d.*)
      *(.rel.tdata .rel.tdata.* .rel.gnu.linkonce.td.*)
      *(.rel.tbss .rel.tbss.* .rel.gnu.linkonce.tb.*)
      *(.rel.ctors)
      *(.rel.dtors)
      *(.rel.got)
      *(.rel.bss .rel.bss.* .rel.gnu.linkonce.b.*)
      PROVIDE_HIDDEN (__rel_iplt_start = .);
      *(.rel.iplt)
      PROVIDE_HIDDEN (__rel_iplt_end = .);
      PROVIDE_HIDDEN (__rela_iplt_start = .);
      PROVIDE_HIDDEN (__rela_iplt_end = .);
    }
  .rela.dyn :
    {
      *(.rela.opd)
      *(.rela.init)
      *(.rela.text .rela.text.* .rela.gnu.linkonce.t.*)
      *(.rela.fini)
      *(.rela.rodata .rela.rodata.* .rela.gnu.linkonce.r.*)
      *(.rela.data.rela.ro* .rela.gnu.linkonce.d.rela.ro.*)
      *(.rela.data .rela.data.* .rela.gnu.linkonce.d.*)
      *(.rela.tdata .rela.tdata.* .rela.gnu.linkonce.td.*)
      *(.rela.tbss .rela.tbss.* .rela.gnu.linkonce.tb.*)
      *(.rela.ctors)
      *(.rela.dtors)
      *(.rela.got)
      *(.rela.got1)
      *(.rela.got2)
      *(.rela.toc)
      *(.rela.toc1)
      *(.rela.tocbss)
      *(.rela.branch_lt)
      *(.rela.sdata .rela.sdata.* .rela.gnu.linkonce.s.*)
      *(.rela.sbss .rela.sbss.* .rela.gnu.linkonce.sb.*)
      *(.rela.sdata2 .rela.sdata2.* .rela.gnu.linkonce.s2.*)
      *(.rela.sbss2 .rela.sbss2.* .rela.gnu.linkonce.sb2.*)
      *(.rela.bss .rela.bss.* .rela.gnu.linkonce.b.*)
      PROVIDE_HIDDEN (__rel_iplt_start = .);
      PROVIDE_HIDDEN (__rel_iplt_end = .);
      *(.rela.ldata .rela.ldata.* .rela.gnu.linkonce.l.*)
      *(.rela.lbss .rela.lbss.* .rela.gnu.linkonce.lb.*)
      *(.rela.lrodata .rela.lrodata.* .rela.gnu.linkonce.lr.*)
      *(.rela.ifunc)
      PROVIDE_HIDDEN (__rela_iplt_start = .);
      *(.rela.iplt)
      PROVIDE_HIDDEN (__rela_iplt_end = .);
    }
  .rel.plt :
    {
      *(.rel.plt)
    }
  .rela.plt :
    {
      *(.rela.plt)
      PROVIDE_HIDDEN (__rel_iplt_start = .);
      *(.rela.iplt)
      PROVIDE_HIDDEN (__rel_iplt_end = .);
    }
  .init :
  {
    _init = .;
    KEEP (*(.init))
  }

  .plt : { *(.plt) }
  .iplt : { *(.iplt) }

  .text :
  {
    HIDDEN(__wrs_text_start = .);
    *(.text.unlikely .text.*_unlikely .text.unlikely.*)
    *(.text.exit .text.exit.*)
    *(.text.startup .text.startup.*)
    *(.text.hot .text.hot.*)
    *(.text .stub .text.* .gnu.linkonce.t.*)

    *(.gnu.warning)

    *(.glue_7t) *(.glue_7) *(.vfp11_veneer) *(.v4_bx)





    HIDDEN (__wrs_text_end = .);
  }
  .fini :
  {
    _fini = .;
    KEEP (*(.fini))
  }
  PROVIDE (__etext_unrelocated = .);
  PROVIDE (_etext_unrelocated = .);
  PROVIDE (etext_unrelocated = .);
  PROVIDE (__etext = .);
  PROVIDE (_etext = .);
  PROVIDE (etext = .);
  .rodata : { *(.rodata .rodata.* .gnu.linkonce.r.*) }
  .rodata1 : { *(.rodata1) }
  .eh_frame_hdr : {
    HIDDEN(__wrs_eh_frame_hdr_start = .);
    *(.eh_frame_hdr) *(.eh_frame_entry .eh_frame_entry.*)
    HIDDEN(__wrs_eh_frame_hdr_end = .);
  }
  .eh_frame :
  {



    __EH_FRAME_BEGIN__ = .;




    HIDDEN(__wrs_eh_frame_start = .);
    KEEP (*(.eh_frame)) *(.eh_frame.*)
    HIDDEN(__wrs_eh_frame_end = .);




  }
  .gcc_except_table : ONLY_IF_RO { *(.gcc_except_table .gcc_except_table.*) }
  .gnu_extab : ONLY_IF_RO { *(.gnu_extab*) }

  .exception_ranges : ONLY_IF_RO { *(.exception_ranges .exception_ranges*) }




  . = ALIGN (CONSTANT (MAXPAGESIZE));



  .eh_frame : ONLY_IF_RW { KEEP (*(.eh_frame)) *(.eh_frame.*) }
  .gnu_extab : ONLY_IF_RW { *(.gnu_extab) }
  .exception_ranges : ONLY_IF_RW { *(.exception_ranges .exception_ranges*) }
  .gcc_except_table : ONLY_IF_RW { *(.gcc_except_table .gcc_except_table.*) }





  .tdata : {
    __wrs_rtp_tls_data_start = .;
    ___wrs_rtp_tls_data_start = __wrs_rtp_tls_data_start;
    *(.tdata .tdata.* .gnu.linkonce.td.*)
  }
  __wrs_rtp_tls_data_filesize = SIZEOF(.tdata);
  ___wrs_rtp_tls_data_filesize = __wrs_rtp_tls_data_filesize;
  .tbss : {
    *(.tbss .tbss.* .gnu.linkonce.tb.*) *(.tcommon)
    __wrs_rtp_tls_bss_end = .;
  }
  __wrs_rtp_tls_data_memsize = ABSOLUTE(__wrs_rtp_tls_bss_end - __wrs_rtp_tls_data_start);
  __wrs_rtp_tls_data_align = ALIGNOF(.tdata);
  ___wrs_rtp_tls_data_align = ALIGNOF(.tdata);
  .preinit_array :
  {
    PROVIDE_HIDDEN (__preinit_array_start = .);
    KEEP (*(.preinit_array))
    PROVIDE_HIDDEN (__preinit_array_end = .);
  }
  .init_array :
  {
    PROVIDE_HIDDEN (__init_array_start = .);
    KEEP (*(SORT_BY_INIT_PRIORITY(.init_array.*) SORT_BY_INIT_PRIORITY(.ctors.*)))
    KEEP (*(.init_array EXCLUDE_FILE (*crtbegin.o *crtbegin?.o *crtend.o *crtend?.o ) .ctors))
    PROVIDE_HIDDEN (__init_array_end = .);
  }
  .fini_array :
  {
    PROVIDE_HIDDEN (__fini_array_start = .);
    KEEP (*(SORT_BY_INIT_PRIORITY(.fini_array.*) SORT_BY_INIT_PRIORITY(.dtors.*)))
    KEEP (*(.fini_array EXCLUDE_FILE (*crtbegin.o *crtbegin?.o *crtend.o *crtend?.o ) .dtors))
    PROVIDE_HIDDEN (__fini_array_end = .);
  }
  .ctors :
  {
    KEEP (*crtbegin.o(.ctors))
    KEEP (*crtbegin?.o(.ctors))
    KEEP (*ctordtor.sho(.ctors))
    KEEP (*ctordtor.o(.ctors))




    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o ) .ctors))
    KEEP (*(SORT(.ctors.*)))
    KEEP (*(.ctors))
  }
  .dtors :
  {
    KEEP (*crtbegin.o(.dtors))
    KEEP (*crtbegin?.o(.dtors))
    KEEP (*ctordtor.o(.dtors))
    KEEP (*ctordtor.sho(.dtors))
    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o ) .dtors))
    KEEP (*(SORT(.dtors.*)))
    KEEP (*(.dtors))
  }
  .jcr : { KEEP (*(.jcr)) }
  .data.rel.ro : { *(.data.rel.ro.local* .gnu.linkonce.d.rel.ro.local.*) *(.data.rel.ro .data.rel.ro.* .gnu.linkonce.d.rel.ro.*) }




  .dynamic : { *(.dynamic) }

  .got : { *(.got.plt) *(.igot.plt) *(.got) *(.igot) }
  .data :
  {

    __data_start = . ;
    *crt1.o(.data .data.* .gnu.linkonce.d.*)






    *(.data .data.* .gnu.linkonce.d.*)
    SORT(CONSTRUCTORS)
  }
  .data1 : { *(.data1) }
  .edata : { PROVIDE (_edata = .); }
  . = .;
  __bss_start = ABSOLUTE(.);
  __bss_start__ = ABSOLUTE(.);
  .bss :
  {
   *(.dynbss)
   *(.bss .bss.* .gnu.linkonce.b.*)
   *(COMMON)





   . = ALIGN(. != 0 ? 4 : 1);
  }
  _bss_end__ = . ; __bss_end__ = . ;

  . = ALIGN( 4 );
  . = SEGMENT_START("ldata-segment", .);
  . = ALIGN( 4 );

  PROVIDE (_ehdr = (DEFINED (__wrs_rtp_base) ? __wrs_rtp_base : 0));
  _end = ABSOLUTE(.); PROVIDE (end = .);


  .stab 0 : { *(.stab) }
  .stabstr 0 : { *(.stabstr) }
  .stab.excl 0 : { *(.stab.excl) }
  .stab.exclstr 0 : { *(.stab.exclstr) }
  .stab.index 0 : { *(.stab.index) }
  .stab.indexstr 0 : { *(.stab.indexstr) }
  .comment 0 : { *(.comment) }




  .debug 0 : { *(.debug) }
  .line 0 : { *(.line) }

  .debug_srcinfo 0 : { *(.debug_srcinfo) }
  .debug_sfnames 0 : { *(.debug_sfnames) }

  .debug_aranges 0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }

  .debug_info 0 : { *(.debug_info .gnu.linkonce.wi.*) }
  .debug_abbrev 0 : { *(.debug_abbrev) }
  .debug_line 0 : { *(.debug_line .debug_line.* .debug_line_end ) }
  .debug_frame 0 : { *(.debug_frame) }
  .debug_str 0 : { *(.debug_str) }
  .debug_loc 0 : { *(.debug_loc) }
  .debug_macinfo 0 : { *(.debug_macinfo) }

  .debug_weaknames 0 : { *(.debug_weaknames) }
  .debug_funcnames 0 : { *(.debug_funcnames) }
  .debug_typenames 0 : { *(.debug_typenames) }
  .debug_varnames 0 : { *(.debug_varnames) }

  .debug_pubtypes 0 : { *(.debug_pubtypes) }
  .debug_ranges 0 : { *(.debug_ranges) }

  .debug_macro 0 : { *(.debug_macro) }

    .stack 0x80000 :
  {
    _stack = .;
    *(.stack)
  }
  .ARM.attributes 0 : { KEEP (*(.ARM.attributes)) KEEP (*(.gnu.attributes)) }
  .note.gnu.arm.ident 0 : { KEEP (*(.note.gnu.arm.ident)) }

  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
  .debug_addr 0 : { *(.debug_addr) }
  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
  /DISCARD/ : { *(.fixup) }
  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
}
