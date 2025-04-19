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

  method init (arch_ : string) (bit_mode_ : string) =
    self#set_arch arch_;
    self#set_bit_mode bit_mode_;
    self#set_objdump_command;

  method set_arch (arch_ : string) =
    arch <- arch_

  method set_bit_mode (bit_mode_ : string) =
    bit_mode <- bit_mode_

  method set_objdump_command =
    if arch = "intel" then
      objdump_command <- "objdump"
    else if arch = "arm" then
      if bit_mode = "32" then
        begin
          objdump_command <- "arm-linux-gnueabihf-objdump";
        end
      else if bit_mode = "64" then
        begin
          objdump_command <- "aarch64-linux-gnu-objdump";
        end
    else
      failwith "unsupported architecture"

  method disassemble (f : string) (arch : string) =
    let disassemble_arm_thumb_binary (f : string) =
      let ret = ref 0 in
      ret := Sys.command("python3 arm_preprocess.py " ^ f);
    in

    let ret = ref 0 in
	  print_string "1: linearly disassemble\n";

    if arch = "arm" && bit_mode = "32" then
      disassemble_arm_thumb_binary f
    else
      ret := Sys.command(objdump_command ^ " -Dr -j .text "^f^" > "^f^".temp");

    self#checkret ret (f^".temp");

    ignore(Sys.command("python3 pic_process.py "^f^" "^(string_of_bool !is_32)));
    ignore(Sys.command("python3 extern_symbol_process64.py "^f));
    ignore(Sys.command("python3 pic_process64.py "^f^" "^(string_of_bool !is_32)^" "^arch));

    ret := Sys.command(objdump_command ^ " -s -j \
                        .rodata "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > rodata.info");
    self#checkret ret "rodata.info";
    ret := Sys.command(objdump_command ^ " -s -j \
                        .data "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > data.info");
    self#checkret ret "data.info";
    ret := Sys.command(objdump_command ^ " -s -j \
                        .data.rel.ro "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > data_rel_ro.info");
    self#checkret ret "data_rel_ro.info";
    ret := Sys.command(objdump_command ^ " -s -j \
                        rodata.cst32 "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > rodata_cst32.info");
    self#checkret ret "rodata_cst32.info";
    ret := Sys.command(objdump_command ^ " -s -j \
                        .eh_frame "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > eh_frame.info");
    ret := Sys.command(objdump_command ^ " -s -j \
                        .eh_frame_hdr "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > eh_frame_hdr.info");
    ret := Sys.command(objdump_command ^  " -s -j \
                        .got "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > got.info");
    let module EU = ELF_utils in
    if EU.elf_static () then
      begin
        ret := Sys.command(objdump_command ^ " -d -j \
                            .plt "^f^" | grep jmp | wc -l > plt_entries.info");
        ret := Sys.command(objdump_command ^ " -s -j \
                            __libc_IO_vtables "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > __libc_IO_vtables.info");
        self#checkret ret "__libc_IO_vtables.info";
        ret := Sys.command(objdump_command ^ " -s -j \
                            __libc_freeres_ptrs "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > __libc_freeres_ptrs.info");
        self#checkret ret "__libc_freeres_ptrs.info"
      end
    else ()


  method process(f : string) (arch : string) (bit_mode : string) =
    self#textProcess f arch bit_mode;
    if !is_32 = true then
      ( self#sectionProcess_32(f) )
    else
      ( self#sectionProcess_64(f) );
    self#bssHandler(f);
    let module EU = ELF_utils in
    if EU.elf_static () then
      self#__libc_freeres_ptrsHandler()
    else ();
    (* self#externFuncProcess(f); *)
    self#externDataProcess(f);
    self#export_tbl_dump(f);
    self#pltProcess(f);
    self#userFuncProcess(f);


  method __libc_freeres_ptrsHandler () =
    let _ = Sys.command("python3 sec_empty_creator.py __libc_freeres_ptrs") in ()

  method bssHandler (f : string) =
    let _ = Sys.command("python3 sec_empty_creator.py .bss") in
	(*
              let _ = Sys.command("readelf -s "^f^" | grep 'GLOBAL\|WEAK' \
                | awk \'/OBJECT/ {print $2,$8}\' > globalbss.info") in ()
	 *)
    (* let _ = Sys.command("readelf -sW "^f^" | grep OBJECT \
                                         | awk \'/GLOBAL/ {print $2,$8}\' > globalbss.info") in () *)
    let _ = Sys.command(objdump_command ^ " -R "^f^" | awk \'/GLOB_DAT/ {print $1, $2, $3}\' > globalbss.info") in
    let _ = Sys.command(objdump_command ^ " -R "^f^" | awk \'/COPY/ {print $1, $2, $3}\' >> globalbss.info") in ()

  method checkret r file =
    if !r = 0 then ()
    else let _ = Sys.command("rm "^file) in ()

  method textProcess (f : string) (arch : string) (bit_mode : string) =
    (*
          let black_list = ["_start"; "__do_global_dtors_aux"; "frame_dummy"; "__libc_csu_fini";
                        "__i686.get_pc_thunk.bx"; "__do_global_ctors_aux"; "__libc_csu_init"; "__do_global_ctors_aux"; ] in
              let bl_str = String.concat "|" black_list in
                let filter_str = "awk \'!/"^bl_str^"/\' RS=\"\" ORS=\"\\n\\n\" "^f^".temp \
                                   > "^f^".disassemble" in
            Sys.command(filter_str);
     *)
    ignore(Sys.command("python3 useless_func_del.py "^f));

    let fields =
      if arch = "intel" then "1,3"
      else if arch = "arm" then "1,3,4"
      else failwith "unsupported architecture for fields" in
    ignore(Sys.command("cat "^f^".disassemble | grep \"^ \" | cut -f"^fields^" > instrs.info"));

    ignore(Sys.command("python3 filter_nop.py"));

	ignore(Sys.command("cut -f 1 instrs.info > text_mem.info"))

  method userFuncProcess(f: string) =
    let _ = Sys.command("cat "^f^".disassemble | grep \"<\" | grep \">:\" > userfuncs.info") in
    ()

  method sectionProcess_32(f: string) =
    let module EU = ELF_utils in
    if EU.elf_static () then
        (ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk \'/data|bss|got|__libc_IO_vtables|__libc_freeres_ptrs/ {print $3,$5,$6,$7} \' | awk \' $1 != \
                    \".got.plt\" {print $1,$2,$3,$4}\' > sections.info"));
        ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk \'/data|bss|got|__libc_IO_vtables|__libc_freeres_ptrs/ {print $2,$4,$5,$6} \' | awk \' $1 != \
                    \".got.plt\" {print $1,$2,$3,$4}\' >> sections.info")))
    else
        (ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk \'/data|bss|got/ {print $3,$5,$6,$7} \' | awk \' $1 != \
                    \".got.plt\" {print $1,$2,$3,$4}\' > sections.info"));
        ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk \'/data|bss|got/ {print $2,$4,$5,$6} \' | awk \' $1 != \
                    \".got.plt\" {print $1,$2,$3,$4}\' >> sections.info")));
    ignore(Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk \'/text/ {print $3,$5,$6,$7} \' > text_sec.info"));
    ignore(Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk \'/text/ {print $2,$4,$5,$6} \' >> text_sec.info"));
    ignore(Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk \'/init/ {print $3,$5,$6,$7} \' | awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' > init_sec.info"));
    ignore(Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk \'/init/ {print $2,$4,$5,$6} \' | awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' >> init_sec.info"));
    ignore(Sys.command("rm init_array.info"));
    ignore(Sys.command(objdump_command ^ " -s -j .init_array "^f^" >>init_array.info 2>&1"));
    ignore(Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk '$3==\".plt\" {print $3,$5,$6,$7}' > plt_sec.info"));
    ignore(Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk '$2==\".plt\" {print $2,$4,$5,$6}' >> plt_sec.info"));
    (* Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk '$3==\".plt.sec\" {print $3,$5,$6,$7}' >> plt_sec.info");
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk '$2==\".plt.sec\" {print $2,$4,$5,$6}' >> plt_sec.info"); *)
    ()
    (* and _ = Sys.command("greadelf -S "^f^" | awk '$2==\".got.plt\" {print $2,$4,$5,$6}' > pic_secs.info") in *)

  method sectionProcess_64(f : string) =
    let module EU = ELF_utils in
    if EU.elf_static () then
        (ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk \'/data|bss|got|__libc_IO_vtables|__libc_freeres_ptrs/ {print $3,$5,$6,$7} \'  > sections.info"));
        ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk \'/data|bss|got|__libc_IO_vtables|__libc_freeres_ptrs/ {print $2,$4,$5,$6} \'  >> sections.info")))
    else
        (ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk \'/data|bss|got/ {print $3,$5,$6,$7} \'  > sections.info"));
        ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk \'/data|bss|got/ {print $2,$4,$5,$6} \'  >> sections.info")));
    ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk \'/text/ {print $3,$5,$6,$7} \' > text_sec.info"));
    ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk \'/text/ {print $2,$4,$5,$6} \' >> text_sec.info"));
    ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk \'/init/ {print $3,$5,$6,$7} \' | awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' > init_sec.info"));
    ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk \'/init/ {print $2,$4,$5,$6} \' | awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' >> init_sec.info"));
    ignore(Sys.command("rm init_array.info"));
    ignore(Sys.command(objdump_command ^ " -s -j .init_array " ^ f ^ " >>init_array.info 2>&1"));
    ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk '$3==\".plt\" {print $3,$5,$6,$7}' > plt_sec.info"));
    ignore(Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk '$2==\".plt\" {print $2,$4,$5,$6}' >> plt_sec.info"));
    ()
    (* and _ = Sys.command("greadelf -S "^f^" | awk '$2==\".got.plt\" {print $2,$4,$5,$6}' > pic_secs.info") in *)

  method externFuncProcess(f: string) =
    let _ = Sys.command("readelf -r "^f^" | awk \'/JUMP_SLOT/ {print $1,$5} \' > externfuncs.info") in
    ()

  method check_disassemble (f: string) =
    let ret = Sys.command("grep \"(bad)\" "^f^".temp > /dev/null") in
    if ret == 0 then
      failwith "detect disassembly error"
    else ()

  method export_tbl_dump (f: string) =
    ignore(Sys.command("readelf -s "^f^" | grep GLOBAL > export_tbl.info"))


  method pltProcess(f: string) =
    let _ = Sys.command(objdump_command ^  " -j .plt -Dr "^f^" | grep \">:\" > plts.info") in
    ()

  method externDataProcess(f: string) =
    let _ = Sys.command("readelf -r "^f^" | awk \'/GLOB_DAT/ {print $5} \' > externdatas.info") in
    ()

  method ailProcess (f: string) =
    let processor = new ail in
    (processor#sections;
     (* processor#externfuncs; *)
     processor#userfuncs;
     processor#externdatas;
     processor#global_bss;
     processor#instrProcess_2 f arch);
     (* processor#instrProcess) *)
end

let check_strip (f : string) : bool =
  (* let _ = Sys.command("file "^f^" > elf.info") in *)
  let filelines : string list = read_file "elf.info" in
    let info = List.nth filelines 0 in
        if String.exists info "ELF 32-bit" then
          (is_32 := true; true)
        else if String.exists info "ELF 64-bit" then
          (is_32 := false; true)
        else
            false

(* check whether it is a library or executable *)
let check_exe =
  let filelines : string list = read_file "elf.info" in
    let info = List.nth filelines 0 in
        if String.exists info "executable" then
          true
        else if String.exists info "LSB shared object" && String.exists info "ld-linux" then
          true
        else
          false

(*
 * some of the symbols have to cleared in this way as their
 * existence could affect the semantics of the framework
 *)
let clear_code =
  ignore(Sys.command ("rm final.s"));
  ignore(Sys.command ("rm inline_symbols.txt"));
  ()

let check_arch arch =
  if String.exists arch "intel" || String.exists arch "arm" then
    true
  else
    false

let check_bit_mode bit_mode =
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
        begin
          let _ = clear_code in
	  (* Initialize Random so it won't use the default seed *)
	  let _ = Random.self_init() in
          let init = new ailInit in
          init#init arch bit_mode;
          init#disassemble elf arch;
          init#process elf arch bit_mode;
          init#ailProcess(elf);
        end
      else
        print_string ("binary file "^elf^" doesn't exist\n")

let () =
  main ();
