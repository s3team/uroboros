(* init : use GNU utils objdump to disassemble
 * initialize file info, section info, generate disassemble file*)

open Batteries

open Ail

open Ail_utils

let is_32 = ref false

class ailInit =

object (self)

  method disassemble (f: string) =
    let ret = ref 0 in

    (* ret := Sys.command("objdump -Dr -j \
            .init "^f^" > "^f^".temp");
	 *)

	print_string "1: linearly disassemble\n";

    ret := Sys.command("objdump -Dr -j .text "^f^" > "^f^".temp");
    self#checkret ret (f^".temp");

    Sys.command("python pic_process.py "^f^" "^(string_of_bool !is_32));
    Sys.command("python extern_symbol_process64.py "^f);
    Sys.command("python pic_process64.py "^f^" "^(string_of_bool !is_32));

    ret := Sys.command("objdump -s -j \
                        .rodata "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > rodata.info");
    self#checkret ret "rodata.info";
    ret := Sys.command("objdump -s -j \
                        .data "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > data.info");
    self#checkret ret "data.info";
    ret := Sys.command("objdump -s -j \
                        .data.rel.ro "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > data_rel_ro.info");
    self#checkret ret "data_rel_ro.info";
    ret := Sys.command("objdump -s -j \
                        .eh_frame "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > eh_frame.info");
    ret := Sys.command("objdump -s -j \
                        .eh_frame_hdr "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > eh_frame_hdr.info");
    ret := Sys.command("objdump -s -j \
                        .got "^f^" | grep \"^ \" | cut -d \" \" -f3,4,5,6 > got.info");
    

  method process(f : string) =
    self#textProcess(f);
    if !is_32 = true then
      ( self#sectionProcess_32(f) )
    else
      ( self#sectionProcess_64(f) );
    self#bssHandler(f);
    (* self#externFuncProcess(f); *)
    self#externDataProcess(f);
    self#export_tbl_dump(f);
    self#pltProcess(f);
    self#userFuncProcess(f)

  method bssHandler (f : string) =
    let _ = Sys.command("python bss_creator.py") in
	(*
              let _ = Sys.command("readelf -s "^f^" | grep 'GLOBAL\|WEAK' \
                | awk \'/OBJECT/ {print $2,$8}\' > globalbss.info") in ()
	 *)
    (* let _ = Sys.command("readelf -sW "^f^" | grep OBJECT \
                                         | awk \'/GLOBAL/ {print $2,$8}\' > globalbss.info") in () *)
    let _ = Sys.command("objdump -R "^f^" | awk \'/GLOB_DAT/ {print $1, $2, $3}\' > globalbss.info") in
    let _ = Sys.command("objdump -R "^f^" | awk \'/COPY/ {print $1, $2, $3}\' >> globalbss.info") in ()

  method checkret r file =
    if !r = 0 then ()
    else let _ = Sys.command("rm "^file) in ()

  method textProcess (f: string) =
    (*
          let black_list = ["_start"; "__do_global_dtors_aux"; "frame_dummy"; "__libc_csu_fini";
                        "__i686.get_pc_thunk.bx"; "__do_global_ctors_aux"; "__libc_csu_init"; "__do_global_ctors_aux"; ] in
              let bl_str = String.concat "|" black_list in
                let filter_str = "awk \'!/"^bl_str^"/\' RS=\"\" ORS=\"\\n\\n\" "^f^".temp \
                                   > "^f^".disassemble" in
            Sys.command(filter_str);
     *)
    Sys.command("python useless_func_del.py "^f);
    Sys.command("cat "^f^".disassemble | grep \"^ \" | cut -f1,3 > instrs.info");

    Sys.command("python filter_nop.py");

	Sys.command("cut -f 1 instrs.info > text_mem.info")

  method userFuncProcess(f: string) =
    let _ = Sys.command("cat "^f^".disassemble | grep \"<\" | grep \">:\" > userfuncs.info") in
    ()

  method sectionProcess_32(f: string) = 
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk \'/data|bss|got/ {print $3,$5,$6,$7} \' | awk \' $1 != \
                \".got.plt\" {print $1,$2,$3,$4}\' > sections.info");
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk \'/data|bss|got/ {print $2,$4,$5,$6} \' | awk \' $1 != \
                \".got.plt\" {print $1,$2,$3,$4}\' >> sections.info");
    (* Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk \'/data|bss|got/ {print $3,$5,$6,$7} \' > sections.info");
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk \'/data|bss|got/ {print $2,$4,$5,$6} \' >> sections.info"); *)
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk \'/text/ {print $3,$5,$6,$7} \' > text_sec.info");
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk \'/text/ {print $2,$4,$5,$6} \' >> text_sec.info");
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk \'/init/ {print $3,$5,$6,$7} \' | awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' > init_sec.info");
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk \'/init/ {print $2,$4,$5,$6} \' | awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' >> init_sec.info");
    Sys.command("rm init_array.info");
    Sys.command("objdump -s -j .init_array "^f^" >>init_array.info 2>&1");
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk '$3==\".plt\" {print $3,$5,$6,$7}' > plt_sec.info");
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk '$2==\".plt\" {print $2,$4,$5,$6}' >> plt_sec.info");
    (* Sys.command("readelf -S " ^ f ^ " | awk \'FNR<15\' | awk '$3==\".plt.sec\" {print $3,$5,$6,$7}' >> plt_sec.info");
    Sys.command("readelf -S " ^ f ^ " | awk \'FNR>14\' | awk '$2==\".plt.sec\" {print $2,$4,$5,$6}' >> plt_sec.info"); *)
    ()
    (* and _ = Sys.command("greadelf -S "^f^" | awk '$2==\".got.plt\" {print $2,$4,$5,$6}' > pic_secs.info") in *)

  method sectionProcess_64(f : string) =
    Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk \'/data|bss|got/ {print $3,$5,$6,$7} \'  > sections.info");
    Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk \'/data|bss|got/ {print $2,$4,$5,$6} \'  >> sections.info");
    Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk \'/text/ {print $3,$5,$6,$7} \' > text_sec.info");
    Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk \'/text/ {print $2,$4,$5,$6} \' >> text_sec.info");
    Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk \'/init/ {print $3,$5,$6,$7} \' | awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' > init_sec.info");
    Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk \'/init/ {print $2,$4,$5,$6} \' | awk \'$1 != \".init_array\" {print $1,$2,$3,$4}\' >> init_sec.info");
    Sys.command("rm init_array.info");
    Sys.command("objdump -s -j .init_array " ^ f ^ " >>init_array.info 2>&1");
    Sys.command("readelf -SW " ^ f ^ " | awk \'FNR<15\' | awk '$3==\".plt\" {print $3,$5,$6,$7}' > plt_sec.info");
    Sys.command("readelf -SW " ^ f ^ " | awk \'FNR>14\' | awk '$2==\".plt\" {print $2,$4,$5,$6}' >> plt_sec.info");
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
    Sys.command("readelf -s "^f^" | grep GLOBAL > export_tbl.info")


  method pltProcess(f: string) =
    let _ = Sys.command("objdump -j .plt -Dr "^f^" | grep \">:\" > plts.info") in
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
     processor#instrProcess_2 f)
     (* processor#instrProcess) *)
end

let check_strip (f : string) : bool =
  let _ = Sys.command("file "^f^" > elf.info") in
  let filelines : string list = read_file "elf.info" in
    let info = List.nth filelines 0 in
        if String.exists info "ELF 32-bit" &&
           String.exists info "dynamically linked" then
          (is_32 := true; true)
        else if String.exists info "ELF 64-bit" &&
           String.exists info "dynamically linked" then
            (is_32 := false; true)
        else
            false

(* check whether it is a library or executable *)
let check_exe =
  let filelines : string list = read_file "elf.info" in
    let info = List.nth filelines 0 in
        if String.exists info "LSB shared object" then
           false
        else
           true

(*
 * some of the symbols have to cleared in this way as their
 * existence could affect the semantics of the framework
 *)
let clear_code =
  Sys.command ("rm final.s");
  Sys.command ("rm inline_symbols.txt");
  ()


  let main () =
    if Array.length Sys.argv <> 2 then
      print_string "ail init: you must specify an ELF binary\n"
    else if check_strip Sys.argv.(1) == false then
      print_string "error input! this tool can only handle \
                    dynamic linked stripped ELF binary on 32bit x86 platform.\n"
    else
      let elf = Sys.argv.(1) in
      if Sys.file_exists elf then
        begin
          let _ = clear_code in
	  (* Initialize Random so it won't use the default seed *)
	  let _ = Random.self_init() in
          let init = new ailInit in
          init#disassemble(elf);
          init#process(elf);
          init#ailProcess(elf)
        end
      else
        print_string ("binary file "^elf^" doesn't exist\n")

let () =
  main ()
