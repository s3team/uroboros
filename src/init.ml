(* init : use GNU utils objdump to disassemble
 * initialize file info, section info, generate disassemble file*)

open Batteries

open Ail

open Ail_utils

let is_32 = ref false

class ailInit =

object (self)
  val mutable arch : string = ""
  val mutable bit_mode : string = ""
  val mutable objdump_command : string = ""

  method init (arch_ : string) (bit_mode_ : string) : unit =
    self#set_arch arch_;
    self#set_bit_mode bit_mode_;
    self#set_objdump_command

  method set_arch (arch_ : string) : unit =
    arch <- arch_

  method set_bit_mode (bit_mode_ : string) : unit =
    bit_mode <- bit_mode_

  method set_objdump_command : unit =
    if arch = "intel" then
      objdump_command <- "objdump"
    else if arch = "arm" then
      if bit_mode = "32" then
          objdump_command <- "arm-linux-gnueabihf-objdump"
      else if bit_mode = "64" then
          objdump_command <- "aarch64-linux-gnu-objdump"
    else
      failwith "unsupported architecture"

  method disassemble (f : string) (arch : string) : unit =
    let disassemble_arm_thumb_binary (f : string) =
      let ret = ref 0 in
      ret := Sys.command("python3 arm_preprocess.py " ^ f);
    in

    let ret = ref 0 in
	  print_endline "1: linearly disassemble";

    if arch = "arm" && bit_mode = "32" then
      disassemble_arm_thumb_binary f
    else
      ret := Sys.command(objdump_command ^ " -Dr -j .text "^f^" > "^f^".temp");

    self#checkret ret (f^".temp");

    ignore (Sys.command("python3 pic_process.py "^f^" "^(string_of_bool !is_32)));
    ignore (Sys.command("python3 extern_symbol_process64.py "^f));
    ignore (Sys.command("python3 pic_process64.py "^f^" "
                       ^(string_of_bool !is_32)^" "^arch));

    ret := Sys.command(objdump_command ^ " -s -j \
                        .text "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > text.info");
    ret := Sys.command(objdump_command ^ " -s -j \
                        .rodata "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 \
                        > rodata.info");
    self#checkret ret "rodata.info";
    ret := Sys.command(objdump_command ^ " -s -j \
                        .data "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 \
                        > data.info");
    self#checkret ret "data.info";
    ret := Sys.command(objdump_command ^ " -s -j \
                        .data.rel.ro "^f^" | grep \"^ \" | \
                        cut -d \" \" -f3,4,5,6 > data_rel_ro.info");
    self#checkret ret "data_rel_ro.info";
    ret := Sys.command(objdump_command ^ " -s -j \
                        rodata.cst32 "^f^" | grep \"^ \" | \
                        cut -d \" \" -f3,4,5,6 > rodata_cst32.info");
    self#checkret ret "rodata_cst32.info";
    ret := Sys.command(objdump_command ^ " -s -j \
                        .eh_frame "^f^" | grep \"^ \" | \
                        cut -d \" \" -f3,4,5,6 > eh_frame.info");
    ret := Sys.command(objdump_command ^ " -s -j \
                        .eh_frame_hdr "^f^" | grep \"^ \" | \
                        cut -d \" \" -f3,4,5,6 > eh_frame_hdr.info");
    ret := Sys.command(objdump_command ^  " -s -j \
                        .got "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 \
                        > got.info");
    let module EU = ELF_utils in
    if EU.elf_static () then begin
      ret := Sys.command(objdump_command ^  " -s -j \
                          .got.plt "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 \
                          > got_plt.info");
      ret := Sys.command(objdump_command ^ " -d -j \
                          .plt "^f^" | grep jmp | wc -l > plt_entries.info");
      ret := Sys.command(objdump_command ^ " -s -j \
                          __libc_IO_vtables "^f^" | grep \"^ \" | \
                          cut -d \" \" -f3,4,5,6 > __libc_IO_vtables.info");
      self#checkret ret "__libc_IO_vtables.info";
      ret := Sys.command(objdump_command ^ " -s -j \
                          __libc_freeres_ptrs "^f^" | grep \"^ \" | \
                          cut -d \" \" -f3,4,5,6 > __libc_freeres_ptrs.info");
      self#checkret ret "__libc_freeres_ptrs.info"
    end

  method process (f : string) (arch : string) (bit_mode : string) : unit =
    self#text_process f arch bit_mode;
    if !is_32 = true then
      ( self#section_process_32(f) )
    else
      ( self#section_process_64(f) );
    self#bss_handler(f);
    let module EU = ELF_utils in
    if EU.elf_static () then
      self#__libc_freeres_ptrs_handler();
    self#extern_data_process(f);
    self#export_tbl_dump(f);
    self#plt_process(f);
    self#user_func_process(f)

  method __libc_freeres_ptrs_handler () : unit =
    ignore (Sys.command("python3 sec_empty_creator.py __libc_freeres_ptrs"))

  method bss_handler (f : string) : unit =
    ignore (Sys.command("python3 sec_empty_creator.py .bss"));
    ignore (Sys.command(objdump_command ^ " -R "^f^" | \
                       awk \'/GLOB_DAT/ {print $1, $2, $3}\' > globalbss.info"));
    ignore (Sys.command(objdump_command ^ " -R "^f^" | \
                       awk \'/COPY/ {print $1, $2, $3}\' >> globalbss.info"))

  method checkret (r : int ref) (file : string) : unit =
    if !r <> 0 then ignore (Sys.command("rm "^file))

  method text_process (f : string) (arch : string) (bit_mode : string) : unit =
    ignore (Sys.command("python3 useless_func_del.py "^f));
    let fields =
      if arch = "intel" then "1,3"
      else if arch = "arm" then "1,3,4"
      else failwith "unsupported architecture for fields" in
    ignore (Sys.command("cat "^f^".disassemble | grep \"^ \" | cut -f"^fields^" \
                        > instrs.info"));
    let module EU = ELF_utils in
    if EU.elf_static () then
      ( ignore (Sys.command("cat plt_whole.info | grep \"^ \" | cut -f1,3 \
                          > plt_whole2.info"));
      ignore (Sys.command("cat plt_whole2.info >> instrs.info")) );
    ignore (Sys.command("python3 filter_nop.py"));
    ignore (Sys.command("cut -f 1 instrs.info > text_mem.info"));

  method user_func_process (f : string) : unit =
    ignore (Sys.command("cat "^f^".disassemble | grep \"<\" | grep \">:\" \
                        > userfuncs.info"))

  method section_process_32 (f : string) : unit =
    let module EU = ELF_utils in
    if EU.elf_static () then
        (ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | \
                              awk \'/data|bss|got|__libc_IO_vtables|\
                              __libc_freeres_ptrs/ {print $3,$5,$6,$7} \' | \
                              awk \' {print $1,$2,$3,$4}\' \
                              > sections.info"));
        ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | \
                            awk \'/data|bss|got|__libc_IO_vtables|\
                            __libc_freeres_ptrs/ {print $2,$4,$5,$6} \' | \
                            awk \' {print $1,$2,$3,$4}\' \
                            >> sections.info")))
    else
        (ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | \
                             awk \'/data|bss|got/ {print $3,$5,$6,$7} \' | \
                            awk \' $1 != \".got.plt\" {print $1,$2,$3,$4}\' \
                            > sections.info"));
        ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | \
                            awk \'/data|bss|got/ {print $2,$4,$5,$6} \' | \
                            awk \' $1 != \".got.plt\" {print $1,$2,$3,$4}\' \
                            >> sections.info")));
    ignore (Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | \
                        awk \'/text/ {print $3,$5,$6,$7} \' > text_sec.info"));
    ignore (Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | \
                        awk \'/text/ {print $2,$4,$5,$6} \' >> text_sec.info"));
    ignore (Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | \
                        awk \'/init/ {print $3,$5,$6,$7} \' | \
                        awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' \
                        > init_sec.info"));
    ignore (Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | \
                        awk \'/init/ {print $2,$4,$5,$6} \' | \
                        awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' \
                        >> init_sec.info"));
    ignore (Sys.command("rm init_array.info"));
    ignore (Sys.command(objdump_command ^ " -s -j .init_array "^f^" \
                       >>init_array.info 2>&1"));
    ignore (Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | \
                        awk '$3==\".plt\" {print $3,$5,$6,$7}' > plt_sec.info"));
    ignore (Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | \
                        awk '$2==\".plt\" {print $2,$4,$5,$6}' >> plt_sec.info"))

  method section_process_64 (f : string) : unit =
    let module EU = ELF_utils in
    if EU.elf_static () then
        (ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | \
                             awk \'/data|bss|got|__libc_IO_vtables|\
                             __libc_freeres_ptrs/ {print $3,$5,$6,$7} \' \
                             > sections.info"));
        ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | \
                            awk \'/data|bss|got|\
                            __libc_IO_vtables|__libc_freeres_ptrs/ \
                            {print $2,$4,$5,$6} \'  >> sections.info")))
    else
        (ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | \
                             awk \'/data|bss|got/ {print $3,$5,$6,$7} \' \
                             > sections.info"));
        ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | \
                            awk \'/data|bss|got/ {print $2,$4,$5,$6} \' \
                            >> sections.info")));
    ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | \
                        awk \'/text/ {print $3,$5,$6,$7} \' > text_sec.info"));
    ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | \
                        awk \'/text/ {print $2,$4,$5,$6} \' >> text_sec.info"));
    ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | \
                        awk \'/init/ {print $3,$5,$6,$7} \' | \
                        awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' \
                        > init_sec.info"));
    ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | \
                        awk \'/init/ {print $2,$4,$5,$6} \' | \
                        awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' \
                        >> init_sec.info"));
    ignore (Sys.command("rm init_array.info"));
    ignore (Sys.command(objdump_command ^ " -s -j .init_array " ^ f ^ " >> \
                                           init_array.info 2>&1"));
    ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | \
                        awk '$3==\".plt\" {print $3,$5,$6,$7}' > plt_sec.info"));
    ignore (Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | \
                        awk '$2==\".plt\" {print $2,$4,$5,$6}' >> plt_sec.info"))

  method externFuncProcess (f : string) : unit =
    ignore (Sys.command("readelf -r "^f^" | awk \'/JUMP_SLOT/ {print $1,$5} \' > \
                        externfuncs.info"))

  method check_disassemble (f : string) : unit =
    let ret = Sys.command("grep \"(bad)\" "^f^".temp > /dev/null") in
    if ret == 0 then
      failwith "detect disassembly error"

  method export_tbl_dump (f : string) : unit =
    ignore (Sys.command("readelf -s "^f^" | grep GLOBAL > export_tbl.info"))

  method plt_process (f : string) : unit =
    ignore (Sys.command(objdump_command ^  " -j .plt -Dr "^f^" | grep \">:\" > \
                                            plts.info"))

  method extern_data_process (f : string) : unit =
    ignore (Sys.command("readelf -r "^f^" | awk \'/GLOB_DAT/ {print $5} \' > \
                        externdatas.info"))

  method ail_process (f : string) : unit =
    let processor = new ail in
    processor#sections;
    processor#userfuncs;
    processor#externdatas;
    processor#global_bss;
    ignore (Sys.command("python3 spliter.py"));
    processor#instr_process f arch
end

let check_strip (f : string) : bool =
  let filelines : string list = read_file "elf.info" in
  let info = List.nth filelines 0 in
  if String.exists info "ELF 32-bit" then
    (is_32 := true; true)
  else if String.exists info "ELF 64-bit" then
    (is_32 := false; true)
  else false

(*
 * some of the symbols have to cleared in this way as their
 * existence could affect the semantics of the framework
 *)
let clear_code : unit =
  (* check if there is final.s *)
  if Sys.file_exists "final.s" then
    ignore(Sys.command ("rm final.s"));
  if Sys.file_exists "inline_symbols.txt" then
    ignore(Sys.command ("rm inline_symbols.txt"));
  ()

let check_arch (arch : string) : bool =
  if String.exists arch "intel" || String.exists arch "arm" then
    true
  else
    false

let check_bit_mode (bit_mode : string) : bool =
  if String.exists bit_mode "32" || String.exists bit_mode "64" then
    true
  else
    false

let main () =
  if Array.length Sys.argv <> 4 then
    print_string "ail init: you must specify an ELF binary, arch, and bit mode\n"
  else if check_strip Sys.argv.(1) == false then
    print_string "error input! this tool can only handle \
                  dynamic linked stripped ELF binary on 32bit x86 platform.\n"
  else if check_arch Sys.argv.(2) == false then
    print_string "error input! this tool can only handle \
                  intel or arm platform.\n"
  else if check_bit_mode Sys.argv.(3) == false then
    print_string "error input! specify between 32 or 64.\n"
  else
    let elf = Sys.argv.(1) in
    let arch = Sys.argv.(2) in
    let bit_mode = Sys.argv.(3) in
    if Sys.file_exists elf then
      let _ = clear_code in
      (* Initialize Random so it won't use the default seed *)
      let _ = Random.self_init () in
      let init = new ailInit in
      ( init#init arch bit_mode;
        init#disassemble elf arch;  (* create text and data files *)
        init#process elf arch bit_mode;  (* create other text files *)
        init#ail_process(elf) )
    else
      print_string ("binary file "^elf^" doesn't exist\n")

let () = main ()
