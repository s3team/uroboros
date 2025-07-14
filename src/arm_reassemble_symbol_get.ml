open Batteries
open Printf
open Visit
open Common_reassemble_symbol_get
open Type
open Ail_utils
open Arm_utils
open Tag_utils
open Pp_print
open Share_lib_helper
open Data_process

exception Reassemble_Error of string

let rev_map f l =
  let rec aux f l acc =
    match l with h :: t -> aux f t (f h :: acc) | [] -> acc
  in
  f l []

type ft = { fn : string; fbaddr : int; feaddr : int }

class datahandler (label' : (string * int) list) =
  object (self)
    val mutable sec : section list = []
    val mutable data : string list = []
    val mutable rodata : string list = []
    val mutable __libc_IO_vtables : string list = []
    val mutable __libc_freeres_ptrs : string list = []
    val mutable got : string list = []
    val mutable bss : string list = []
    val mutable tbss : string list = []
    val mutable text_as_data : string list = []

    (* .data.rel.ro section*)
    val mutable data_rel_ro : string list = []
    val mutable text_mem_addrs : int list = []
    val mutable label_mem_addrs : int list = []
    val mutable data_labels : (string * int) list = []
    val mutable data_labels_reloc : int list = []
    val mutable text_labels : int list = []
    val mutable text_labels_reloc : int list = []
    val mutable label : (string * int) list = label' (* data section, address *)
    val mutable label_set : int list = []
    val mutable text_as_data_list : (string * string) list = []
    val mutable data_list : (string * string) list = []
    val mutable rodata_list : (string * string) list = []
    val mutable __libc_IO_vtables_list : (string * string) list = []
    val mutable __libc_freeres_ptrs_list : (string * string) list = []
    val mutable got_list : (string * string) list = []
    val mutable bss_list : (string * string) list = []
    val mutable tbss_list : (string * string) list = []

    (* .data.rel.ro section*)
    val mutable data_rel_ro_list : (string * string) list = []
    val mutable data_array : (string * string) array = [||]
    val mutable rodata_array : (string * string) array = [||]
    val mutable __libc_IO_vtables_array : (string * string) array = [||]
    val mutable __libc_freeres_ptrs_array : (string * string) array = [||]
    val mutable got_array : (string * string) array = [||]
    val mutable bss_array : (string * string) array = [||]
    val mutable tbss_array : (string * string) array = [||]
    val mutable text_as_data_array : (string * string) array = [||]

    (* .data.rel.ro section*)
    val mutable data_rel_ro_array : (string * string) array = [||]
    val mutable text_sec : int * int = (0, 0) (* begin addr, size*)
    val mutable locations = []
    val mutable in_jmptable = false
    val mutable text_mem_arr = Array.make 1 0
    val mutable label_mem_arr = Array.make 1 0
    val mutable func_begin_arr = Array.make 1 0
    val mutable label_arr = Array.make 1 0
    val mutable fl_sort = Array.make 1 { fn = ""; fbaddr = 0; feaddr = 0 }
    val mutable cur_func_name = ""
    val mutable assumption_two = false
    val mutable assumption_three = false

    method set_datas funcs =
      let rec pl = function
        | (s1, s2) :: t ->
            print_string (s1 ^ " " ^ s2 ^ "|");
            pl t
        | [] -> ()
      in

      self#section_collect;
      self#data_collect;

      text_as_data_list <- self#data_trans text_as_data;
      data_list <- self#data_trans data;
      rodata_list <- self#data_trans rodata;
      got_list <- self#data_trans got;
      bss_list <- self#data_trans bss;
      data_rel_ro_list <- self#data_trans data_rel_ro;
      let module EU = ELF_utils in
      if EU.elf_static () then begin
        tbss_list <- self#data_trans tbss;
        __libc_IO_vtables_list <- self#data_trans __libc_IO_vtables;
        __libc_freeres_ptrs_list <- self#data_trans __libc_freeres_ptrs
      end
      else ();

      (* locations are sec,offset where labels need to be added *)
      locations <- self#label_locate;
      label_set <- List.map snd label;
      label_set <- List.sort compare label_set;
      label_arr <- Array.of_list label_set;
      let fl' =
        List.sort (fun f1 f2 -> f1.func_begin_addr - f2.func_begin_addr) funcs
      in
      let fl1 =
        List.map
          (fun f ->
            {
              fn = f.func_name;
              fbaddr = f.func_begin_addr;
              feaddr = f.func_end_addr;
            })
          fl'
      in
      fl_sort <- Array.of_list fl1;
      let module EU = ELF_utils in
      let addr_len =
        if EU.elf_32 () && EU.elf_arm () then 5
        else
          match (EU.elf_32 (), EU.elf_exe ()) with
          | true, true -> 7
          | false, true -> 6
          | _, false -> 4
      in

      text_mem_addrs <-
        List.map
          (fun a ->
            let a1 = String.trim a in
            int_of_string ("0x" ^ String.sub a1 0 addr_len))
          (read_file "text_mem.info");
      text_mem_arr <- Array.of_list text_mem_addrs;
      label_mem_arr <- Array.of_list (List.sort compare label_mem_addrs);

      self#set_assumption_flag ();

      if EU.elf_32 () then self#data_refer_solve funcs
      else self#data_refer_solve_64 funcs

    method set_assumption_flag () =
      (* this method read assumption configuration assumption_set.info and set
      two flags *)
      let ll = read_file "assumption_set.info" in
      List.nth ll 0 |> fun l ->
      assumption_two <- String.exists l "2";
      assumption_three <- String.exists l "3"

    method set_datas_1 =
      self#section_collect;
      self#data_collect;
      text_as_data_list <- self#data_trans text_as_data;
      data_list <- self#data_trans data;
      rodata_list <- self#data_trans rodata;
      got_list <- self#data_trans got;
      bss_list <- self#data_trans bss;
      locations <- self#label_locate;
      label_set <- List.map snd label;
      let module EU = ELF_utils in
      if EU.elf_static () then begin
        tbss_list <- self#data_trans tbss;
        __libc_IO_vtables_list <- self#data_trans __libc_IO_vtables;
        __libc_freeres_ptrs_list <- self#data_trans __libc_freeres_ptrs
      end
      else ();

      self#data_refer_solve1

    method get_textlabel =
      self#dump_d2c_labels text_labels;
      text_labels

    method text_sec_collect =
      let filelines = File.lines_of "text_sec.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x" ^ List.nth items 1)
        and size = int_of_string ("0x" ^ List.nth items 3) in
        text_sec <- (addr, size)
      in
      Enum.iter help filelines

    method check_text addr =
      let judge_mem_list addr = bbn_byloc addr text_mem_arr in
      (*List.mem addr text_mem_addrs in *)
      let b, size = text_sec in
      let e = b + size in
      if addr = 0xffff then false
        (* this is dangerous, when processing 64-bit shared object, for example,
         ssh, the text section range includes memory address 0xffff. However, I
         found instructions like this in the code:
         cmp $0xffff,%eax
      *)
      else if (addr >= b && addr < e) && judge_mem_list addr then true
      else false

    method dump_c2d_labels dl =
      let zip l = fst l ^ " : " ^ dec_hex (snd l) in
      let oc =
        open_out_gen [ Open_append; Open_creat ] 0o666 "final_c2d_label.txt"
      in
      Printf.fprintf oc "%s\n" (String.concat "\n" (List.map zip dl));
      close_out oc

    method dump_d2d_labels dl =
      let zip l = fst l ^ " : " ^ dec_hex (snd l) in
      let oc =
        open_out_gen [ Open_append; Open_creat ] 0o666 "final_d2d_label.txt"
      in
      Printf.fprintf oc "%s\n" (String.concat "\n" (List.map dec_hex dl));
      close_out oc

    method dump_d2c_labels dl =
      let oc =
        open_out_gen [ Open_append; Open_creat ] 0o666 "final_d2c_label.txt"
      in
      Printf.fprintf oc "%s\n" (String.concat "\n" (List.map dec_hex dl));
      close_out oc

    (* this method is used for 64-bit binaries*)
    (* this method solve all the references in the .data .rodata sections *)
    method data_refer_solve_64 funcs =
      let begin_addrs = List.map (fun f -> f.func_begin_addr) funcs in

      let check_func_begin v =
        (* List.mem v begin_addrs in *)
        bbn_byloc v (Array.of_list begin_addrs)
      in
      (* this method traverse data list, check each four byte value *)
      let get_v_old s =
        let items = Str.split (Str.regexp " +") s in
        let v = List.nth items 1 in
        String.sub v 2 (String.length v - 2)
      in
      (*.byte 0x80*)
      let get_v s = String.sub s 8 2 in
      let rec traverse acc l addr sec =
        (*
               There exists a bug in 64 bit ELF processing. The address
               produced in "let vs'" line contains a 0 at the beginning.
               It musted be removed though.

               aux function translate value_str to value, then to value_str' ;
             thus getting rid of capital zeros.
       *)
        let aux1 vs =
          let v = int_of_string vs in
          Printf.sprintf "%X" v
        in
        let aux v = Printf.sprintf "%X" v in
        match l with
        | [] -> acc
        | h :: [] -> h :: acc
        | [ h1; h2 ] -> h2 :: h1 :: acc
        | [ h1; h2; h3 ] -> h3 :: h2 :: h1 :: acc
        | [ h1; h2; h3; h4 ] -> h4 :: h3 :: h2 :: h1 :: acc
        | [ h1; h2; h3; h4; h5 ] -> h5 :: h4 :: h3 :: h2 :: h1 :: acc
        | [ h1; h2; h3; h4; h5; h6 ] -> h6 :: h5 :: h4 :: h3 :: h2 :: h1 :: acc
        | [ h1; h2; h3; h4; h5; h6; h7 ] ->
            h7 :: h6 :: h5 :: h4 :: h3 :: h2 :: h1 :: acc
        | (l1, v1)
          :: (l2, v2)
          :: (l3, v3)
          :: (l4, v4)
          :: (l5, v5)
          :: (l6, v6)
          :: (l7, v7)
          :: (l8, v8)
          :: t -> begin
            let b = Buffer.create 18 in
            Buffer.add_string b "0x";
            Buffer.add_string b (get_v v8);
            Buffer.add_string b (get_v v7);
            Buffer.add_string b (get_v v6);
            Buffer.add_string b (get_v v5);
            Buffer.add_string b (get_v v4);
            Buffer.add_string b (get_v v3);
            Buffer.add_string b (get_v v2);
            Buffer.add_string b (get_v v1);
            let v_str = Buffer.contents b in
            match string_to_int32 v_str with
            | Some v -> begin
                match self#check_sec v with
                | Some s ->
                    if assumption_two then begin
                      in_jmptable <- false;
                      traverse
                        ((l8, v8) :: (l7, v7) :: (l6, v6) :: (l5, v5)
                       :: (l4, v4) :: (l3, v3) :: (l2, v2) :: (l1, v1) :: acc)
                        t (addr + 8) sec
                    end
                    else
                      let v_str' = aux v in
                      data_labels <- (s.sec_name, v) :: data_labels;
                      data_labels_reloc <- addr :: data_labels_reloc;
                      traverse
                        ((l8, v8) :: (l7, v7) :: (l6, v6) :: (l5, v5)
                       :: ("", "") :: ("", "") :: ("", "")
                        :: (l1, ".long S_0x" ^ v_str')
                        :: acc)
                        t (addr + 8) sec
                | None -> begin
                    match self#check_text v with
                    | true ->
                        let c =
                          if assumption_three = true then check_func_begin v
                          else true
                        in
                        if c = false then
                          if self#check_jmptable l1 v = false then begin
                            in_jmptable <- false;
                            traverse
                              ((l8, v8) :: (l7, v7) :: (l6, v6) :: (l5, v5)
                             :: (l4, v4) :: (l3, v3) :: (l2, v2) :: (l1, v1)
                             :: acc)
                              t (addr + 8) sec
                          end
                          else begin
                            (* the code below is used to help the iteration
                                   processing; each time, we only symbolize suspecial symbols
                                   that are recorded in the label_mem_arr *)
                            in_jmptable <- true;
                            cur_func_name <- self#fn_byloc v;
                            let v_str' = aux v in
                            text_labels <- v :: text_labels;
                            text_labels_reloc <- addr :: text_labels_reloc;
                            traverse
                              ((l8, v8) :: (l7, v7) :: (l6, v6) :: (l5, v5)
                             :: ("", "") :: ("", "") :: ("", "")
                              :: (l1, ".long S_0x" ^ v_str')
                              :: acc)
                              t (addr + 8) sec
                          end
                        else begin
                          if self#check_jmptable_1 l1 = false then
                            in_jmptable <- false
                          else begin
                            in_jmptable <- true;
                            cur_func_name <- self#fn_byloc v
                          end;
                          let v_str' = aux v in
                          text_labels <- v :: text_labels;
                          text_labels_reloc <- addr :: text_labels_reloc;
                          traverse
                            ((l8, v8) :: (l7, v7) :: (l6, v6) :: (l5, v5)
                           :: ("", "") :: ("", "") :: ("", "")
                            :: (l1, ".long S_0x" ^ v_str')
                            :: acc)
                            t (addr + 8) sec
                        end
                    | false ->
                        ignore (self#check_offset_64 v_str addr sec);
                        in_jmptable <- false;
                        traverse
                          ((l8, v8) :: (l7, v7) :: (l6, v6) :: (l5, v5)
                         :: (l4, v4) :: (l3, v3) :: (l2, v2) :: (l1, v1) :: acc
                          )
                          t (addr + 8) sec
                  end
              end
            | None -> begin
                in_jmptable <- false;
                match self#check_offset_64 v_str addr sec with
                | Some s1, Some s2 ->
                    let v_str1 = aux s1 and v_str2 = aux s2 in
                    text_labels <- s1 :: s2 :: text_labels;
                    traverse
                      (("", "") :: ("", "") :: ("", "")
                      :: (l5, ".long S_0x" ^ v_str1)
                      :: ("", "") :: ("", "") :: ("", "")
                      :: (l1, ".long S_0x" ^ v_str2)
                      :: acc)
                      t (addr + 8) sec
                | None, Some s2 ->
                    let v_str2 = aux s2 in
                    text_labels <- s2 :: text_labels;
                    traverse
                      ((l8, v8) :: (l7, v7) :: (l6, v6) :: (l5, v5) :: ("", "")
                     :: ("", "") :: ("", "")
                      :: (l1, ".long S_0x" ^ v_str2)
                      :: acc)
                      t (addr + 8) sec
                | Some s1, None ->
                    let v_str1 = aux s1 in
                    text_labels <- s1 :: text_labels;
                    traverse
                      (("", "") :: ("", "") :: ("", "")
                      :: (l5, ".long S_0x" ^ v_str1)
                      :: (l4, v4) :: (l3, v3) :: (l2, v2) :: (l1, v1) :: acc)
                      t (addr + 8) sec
                | None, None ->
                    traverse
                      ((l8, v8) :: (l7, v7) :: (l6, v6) :: (l5, v5) :: (l4, v4)
                     :: (l3, v3) :: (l2, v2) :: (l1, v1) :: acc)
                      t (addr + 8) sec
              end
          end
      in

      (* add labels in data, rodata sections to support check jmp table*)
      self#add_data_label;

      data_list <- List.rev (traverse [] data_list 0x080500c4 ".data");
      rodata_array <- Array.of_list rodata_list;
      rodata_list <- List.rev (traverse [] rodata_list 0x0 ".rodata");
      data_rel_ro_list <-
        List.rev (traverse [] data_rel_ro_list 0x0 ".data.rel.ro");
      got_list <- List.rev (traverse [] got_list 0x0 ".got");
      let module EU = ELF_utils in
      if EU.elf_static () then begin
        __libc_IO_vtables_list <-
          List.rev (traverse [] __libc_IO_vtables_list 0x0 "__libc_IO_vtables");
        __libc_freeres_ptrs_list <-
          List.rev
            (traverse [] __libc_freeres_ptrs_list 0x0 "__libc_freeres_ptrs")
      end
      else ()

    (* this method solve all the references in the .data .rodata sections *)
    method data_refer_solve funcs =
      let begin_addrs = List.map (fun f -> f.func_begin_addr) funcs in
      let check_func_begin v =
        (* List.mem v begin_addrs in *)
        bbn_byloc v (Array.of_list begin_addrs)
      in
      (* this method traverse data list, check each four byte value *)
      let get_v_old s =
        let items = Str.split (Str.regexp " +") s in
        let v = List.nth items 1 in
        String.sub v 2 (String.length v - 2)
      in
      (*.byte 0x80*)
      let get_v s = String.sub s 8 2 in
      let rec traverse acc l addr sec =
        (*
               There exists a bug in 64 bit ELF processing. The address
               produced in "let vs'" line contains a 0 at the beginning.
               It musted be removed though.

               aux function translate value_str to value, then to value_str' ;
             thus getting rid of capital zeros.
       *)
        let aux1 vs =
          let v = int_of_string vs in
          Printf.sprintf "%X" v
        in
        let aux v is_odd =
          if is_odd then Printf.sprintf "%X + 1" v else Printf.sprintf "%X" v
        in

        match l with
        | [] -> acc
        | h :: [] -> h :: acc
        | [ h1; h2 ] -> h2 :: h1 :: acc
        | [ h1; h2; h3 ] -> h3 :: h2 :: h1 :: acc
        | (l1, v1) :: (h2, v2) :: (l3, v3) :: (l4, v4) :: t -> begin
            let b = Buffer.create 10 in
            Buffer.add_string b "0x";
            Buffer.add_string b (get_v v4);
            Buffer.add_string b (get_v v3);
            Buffer.add_string b (get_v v2);
            Buffer.add_string b (get_v v1);
            let v_str = Buffer.contents b in
            (* For ARM Thumb, d2c references might be odd numbers *)
            let v_str_num = string_to_int32 v_str in
            let is_odd =
              match v_str_num with Some n -> n mod 2 = 1 | None -> false
            in
            let v_str_num' =
              if is_odd then Option.map (fun n -> n - 1) v_str_num
              else v_str_num
            in
            match v_str_num' with
            | Some v -> begin
                match self#check_sec v with
                | Some s ->
                    if assumption_two then begin
                      in_jmptable <- false;

                      traverse
                        ((l4, v4) :: (l3, v3) :: (h2, v2) :: (l1, v1) :: acc)
                        t (addr + 4) sec
                    end
                    else
                      let v_str' = aux v is_odd in
                      data_labels <- (s.sec_name, v) :: data_labels;
                      data_labels_reloc <- addr :: data_labels_reloc;
                      traverse
                        (("", "") :: ("", "") :: ("", "")
                        :: (l1, ".long S_0x" ^ v_str')
                        :: acc)
                        t (addr + 4) sec
                | None -> begin
                    match self#check_text v with
                    | true ->
                        let c =
                          (* we use assumption three  *)
                          if assumption_three = true then check_func_begin v
                          else
                            (* we reject assumption three *)
                            true
                        in
                        if c = false then
                          if self#check_jmptable l1 v = false then begin
                            in_jmptable <- false;
                            traverse
                              ((l4, v4) :: (l3, v3) :: (h2, v2) :: (l1, v1)
                             :: acc)
                              t (addr + 4) sec
                          end
                          else begin
                            in_jmptable <- true;
                            cur_func_name <- self#fn_byloc v;
                            let v_str' = aux v is_odd in
                            (* text_labels <- v::text_labels;
                                text_labels_reloc <- addr::text_labels_reloc; *)
                            traverse
                              (("", "") :: ("", "") :: ("", "")
                              :: (l1, ".long S_0x" ^ v_str')
                              :: acc)
                              t (addr + 4) sec
                          end
                        else begin
                          if self#check_jmptable_1 l1 = false then
                            in_jmptable <- false
                          else begin
                            in_jmptable <- true;
                            cur_func_name <- self#fn_byloc v
                          end;
                          let v_str' = aux v is_odd in
                          text_labels <- v :: text_labels;
                          text_labels_reloc <- addr :: text_labels_reloc;
                          traverse
                            (("", "") :: ("", "") :: ("", "")
                            :: (l1, ".long S_0x" ^ v_str')
                            :: acc)
                            t (addr + 4) sec
                        end
                    | false -> begin
                        match self#check_offset v funcs sec with
                        | Some v ->
                            in_jmptable <- true;
                            cur_func_name <- self#fn_byloc v;
                            let v_str' = aux v is_odd in
                            text_labels <- v :: text_labels;
                            text_labels_reloc <- addr :: text_labels_reloc;
                            traverse
                              (("", "") :: ("", "") :: ("", "")
                              :: (l1, ".long S_0x" ^ v_str')
                              :: acc)
                              t (addr + 4) sec
                        | None ->
                            (*in_jmptable <- false;
                            let h::t = l in
                            traverse ((l1,v1)::acc) t (addr+1) *)
                            in_jmptable <- false;
                            traverse
                              ((l4, v4) :: (l3, v3) :: (h2, v2) :: (l1, v1)
                             :: acc)
                              t (addr + 4) sec
                      end
                  end
              end
            | None ->
                (*in_jmptable <- false;
                let h::t = l in  (*let it crash it not matched*)
                traverse ((l1,v1)::acc) t (addr+1)*)
                traverse
                  ((l4, v4) :: (l3, v3) :: (h2, v2) :: (l1, v1) :: acc)
                  t (addr + 4) sec
          end
      in

      (* add labels in data, rodata sections to support check jmp table*)
      self#add_data_label;

      data_list <- List.rev (traverse [] data_list 0x0 ".data");
      rodata_list <- List.rev (traverse [] rodata_list 0x0 ".rodata");
      got_list <- List.rev (traverse [] got_list 0x0 ".got");
      data_rel_ro_list <-
        List.rev (traverse [] data_rel_ro_list 0x0 ".data.rel.ro");
      let module EU = ELF_utils in
      if EU.elf_static () then begin
        __libc_IO_vtables_list <-
          List.rev (traverse [] __libc_IO_vtables_list 0x0 "__libc_IO_vtables");
        __libc_freeres_ptrs_list <-
          List.rev
            (traverse [] __libc_freeres_ptrs_list 0x0 "__libc_freeres_ptrs")
      end
      else ()
    (* 32-bit binary does not need this *)
    (* got_list <- List.rev (traverse [] got_list 0x0) *)

    method check_jmptable_1 addrs =
      try
        let addr = int_of_string addrs in
        if List.mem addr label_set then true else false
      with _ -> false

    method data_refer_solve1 =
      let get_v s =
        let items = Str.split (Str.regexp " +") s in
        let v = List.nth items 1 in
        String.sub v 2 (String.length v - 2)
      in
      let rec traverse acc l =
        (*
               There exists a bug in 64 bit ELF processing. The address
               produced in "let vs'" line contains a 0 at the beginning.
               It musted be removed though.

               aux function translate value_str to value, then to value_str' ;
             thus getting rid of capital zeros.
       *)
        let aux vs =
          let v = int_of_string vs in
          Printf.sprintf "%X" v
        in
        match l with
        | [] -> acc
        | h :: [] -> h :: acc
        | [ h1; h2 ] -> h2 :: h1 :: acc
        | [ h1; h2; h3 ] -> h3 :: h2 :: h1 :: acc
        | (l1, v1) :: (h2, v2) :: (l3, v3) :: (l4, v4) :: t -> begin
            let v_str = "0x" ^ get_v v4 ^ get_v v3 ^ get_v v2 ^ get_v v1 in
            match string_to_int32 v_str with
            | Some v -> begin
                match self#check_sec v with
                | Some s ->
                    in_jmptable <- false;
                    traverse
                      ((l4, v4) :: (l3, v3) :: (h2, v2) :: (l1, v1) :: acc)
                      t
                | None -> begin
                    match self#check_text v with
                    | true ->
                        if self#check_jmptable l1 v = false then begin
                          in_jmptable <- false;
                          traverse
                            ((l4, v4) :: (l3, v3) :: (h2, v2) :: (l1, v1) :: acc)
                            t
                        end
                        else begin
                          in_jmptable <- true;
                          let v_str' = aux v_str in

                          text_labels <- v :: text_labels;
                          traverse
                            (("", "") :: ("", "") :: ("", "")
                            :: (l1, ".long S_0x" ^ String.uppercase_ascii v_str')
                            :: acc)
                            t
                        end
                    | false ->
                        in_jmptable <- false;
                        traverse
                          ((l4, v4) :: (l3, v3) :: (h2, v2) :: (l1, v1) :: acc)
                          t
                  end
              end
            | None ->
                in_jmptable <- false;
                traverse ((l4, v4) :: (l3, v3) :: (h2, v2) :: (l1, v1) :: acc) t
          end
      in

      self#add_data_label;
      data_list <- List.rev (traverse [] data_list);
      rodata_list <- List.rev (traverse [] rodata_list);
      let module EU = ELF_utils in
      if EU.elf_static () then begin
        __libc_IO_vtables_list <- List.rev (traverse [] __libc_IO_vtables_list);
        __libc_freeres_ptrs_list <-
          List.rev (traverse [] __libc_freeres_ptrs_list)
      end
      else ()

    method fn_byloc addr =
      let cmp addr b =
        if b.fbaddr <= addr && b.feaddr >= addr then 0
        else if b.fbaddr > addr then -1
        else 1
      in
      (* implement a binary search, the original way seems too slow *)
      let rec bs min max =
        if min > max then begin
          print_string (dec_hex addr);
          print_string "\n";
          let blen = Array.length fl_sort in
          let blast = Array.get fl_sort (blen - 1) in
          let bfist = Array.get fl_sort 0 in
          print_string (dec_hex bfist.fbaddr);
          print_string "\n";
          print_string (dec_hex bfist.feaddr);
          print_string "\n";
          assert false
        end
        else
          let mid = (min + max) / 2 in
          let delta =
            try cmp addr (Array.get fl_sort mid)
            with _ ->
              print_string "failed ! ";
              print_int mid;
              print_string "\n";
              print_int (Array.length fl_sort);
              print_string "\n";
              0
          in
          if delta = 0 then (Array.get fl_sort mid).fn
          else if delta < 0 then bs min (mid - 1)
          else bs (mid + 1) max
      in
      bs 0 (Array.length fl_sort - 1)

    method check_jmptable addrs v =
      (* our judgement of jmptable in rodata section *)
      (*
        1. begin with a label (probably indicates a valid des?)
        2. last time we have defined a label (no byte value embedded)
        3. all these labels are within one function (how to check it?)
     *)
      let aux addrs =
        try
          let addr = int_of_string addrs in
          (*if List.mem addr label_set then *)
          if bbn_byloc addr label_arr then begin
            in_jmptable <- true;
            true
          end
          else false
        with _ -> false
      in
      if in_jmptable = true then
        if self#fn_byloc v = cur_func_name then true else aux addrs
      else aux addrs

    method pp_print l =
      let rec help l =
        match l with
        | (h1, h2) :: t ->
            (printf "item: (%s,%d) ") h1 h2;
            help t
        | [] -> print_string "end\n"
      in
      help l

    method section_collect =
      let filelines = File.lines_of "sections.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x" ^ List.nth items 1)
        and size = int_of_string ("0x" ^ List.nth items 3)
        and secname = List.nth items 0 in
        sec <-
          { sec_name = secname; sec_begin_addr = addr; sec_size = size } :: sec
      in
      Enum.iter help filelines

    method section_offset name addr =
      let rec help l =
        match l with
        | h :: t ->
            if h.sec_name = name then addr - h.sec_begin_addr else help t
        | [] -> raise (Reassemble_Error "fail in section offset")
      in
      help sec

    method section_addr name =
      let rec help l =
        match l with
        | h :: t -> if h.sec_name = name then h.sec_begin_addr else help t
        | [] -> raise (Reassemble_Error "fail in section addr")
      in
      help sec

    method print_section =
      let rec help l =
        match l with
        | h :: t ->
            Printf.printf "section: %s %d %d\n" h.sec_name h.sec_begin_addr
              h.sec_size;
            help t
        | [] -> print_string "end\n"
      in
      help sec

    method data_collect =
      let module EU = ELF_utils in
      ignore (Sys.command "python3 spliter.py");
      text_as_data <- self#collect "text_split.info";
      data <- self#collect "data_split.info";
      rodata <- self#collect "rodata_split.info";
      data_rel_ro <- self#collect "data_rel_ro_split.info";
      got <- self#collect "got_split.info";
      bss <- self#collect_bss "bss.info";
      if EU.elf_static () then begin
        __libc_IO_vtables <- self#collect "__libc_IO_vtables_split.info";
        __libc_freeres_ptrs <- self#collect "__libc_freeres_ptrs.info"
      end
      else ()

    method sec_transform s =
      match s with
      | ".rodata" -> ".rodata"
      | ".got" -> ".got"
      | "bss" -> ".bss"
      | _ ->
          let module EU = ELF_utils in
          if EU.elf_static () then
            match s with
            | "__libc_IO_vtables" -> "__libc_IO_vtables"
            | "__libc_freeres_ptrs" -> "__libc_freeres_ptrs"
            | _ -> ".rodata"
          else ".rodata"

    method check_sec (addr : int) : section option =
      let rec help l addr =
        match l with
        | h :: t ->
            let b = h.sec_begin_addr in
            let e = b + h.sec_size in
            if addr >= b && addr < e then Some h else help t addr
        | [] -> None
      in
      help sec addr

    method check_offset_64 offset addr sec =
      let rec find_jmp_table_base idx =
        if idx < 0 then None
        else
          let s, d = rodata_array.(idx) in
          if String.length s = 0 then find_jmp_table_base (idx - 4)
          else if String.exists s "0x" then Some (int_of_string s)
          else None
      in
      let help addr offset_s =
        let offset = int_of_string ("0x" ^ offset_s) in
        let offset' = Int32.to_int (Int32.of_int offset) in
        match find_jmp_table_base addr with
        | Some base ->
            if offset' < 0 then
              if self#check_text (offset' + base) then
                let target_addr = offset' + base in
                Some target_addr
              else None
            else None
        | None -> None
      in
      match sec with
      | ".rodata" ->
          let first = help (addr + 4) (String.sub offset 2 8) in
          let second = help addr (String.sub offset 10 8) in
          (first, second)
      | _ -> (None, None)

    method check_offset offset funcs sec =
      let begin_addrs = List.map (fun f -> f.func_begin_addr) funcs in
      let check_func_begin v = bbn_byloc v (Array.of_list begin_addrs) in
      let read_lines filename =
        File.with_file_in filename (fun input ->
            List.of_enum (IO.lines_of input))
      in
      (* read in file as list to ensure file is closed in a timely manner *)
      (* otherwise, we can get "Too many open files" error when check_offset is called many times *)
      let filelines = [] in
      let peek lst = match lst with [] -> None | x :: _ -> Some x in
      let help l offset =
        let items = Str.split (Str.regexp " +") l in
        let got_addr = int_of_string ("0x" ^ List.nth items 1)
        and offset' = Int32.to_int (Int32.of_int offset) in
        if offset' < 0 then
          if check_func_begin (offset' + got_addr) then
            let target_addr = offset' + got_addr in
            Some target_addr
          else None
        else None
      in
      match sec with
      | ".rodata" -> begin
          match peek filelines with Some l -> help l offset | _ -> None
        end
      | _ -> None

    method pp_print_2 l =
      let rec help l =
        match l with
        | (h1, h2) :: t ->
            (printf "item: (%s,%d) ") h1 h2;
            help t
        | [] -> print_string "end\n"
      in
      help l

    method pp_print_1 l =
      let rec help l =
        match l with
        | h :: t ->
            (printf "item: %s ") h;
            help t
        | [] -> print_string "end\n"
      in
      help l

    method data_transform data_str =
      let l = String.length data_str in
      if l mod 2 <> 0 then failwith "data collection error\n"
      else
        let p =
          object
            val mutable x : (string * string) list = []
            method get_list = x
            method set_list i = x <- ("", ".byte 0x" ^ i) :: x
          end
        and help s =
          let rec help' i l =
            if i < 0 then l
            else
              help' (i - 2) ((Char.escaped s.[i - 1] ^ Char.escaped s.[i]) :: l)
          in
          help' (String.length s - 1) []
        in
        let data_list = List.rev (help data_str) in
        let () = List.iter p#set_list data_list in
        p#get_list

    method data_trans data_list =
      let p =
        object
          val mutable x : (string * string) list = []
          method get_list = x
          method set_list i = x <- ("", i) :: x
        end
      in
      List.iter p#set_list data_list;
      p#get_list

    method label_locate =
      let rec help l acc =
        match l with
        | (s, l) :: t ->
            let offset = self#section_offset s l in
            (s, offset) :: acc
        | [] -> acc
      in
      List.fold_left help label []

    method add_data_label =
      let p =
        object (sp)
          method process lbs =
            let dec_hex (s : int) : string = Printf.sprintf "0x%X" s in
            let rec help loc_list =
              match loc_list with
              | (n, l) :: t -> begin
                  match n with
                  | ".data" ->
                      let off = l - self#section_addr ".data" in
                      let s' = dec_hex l in
                      let s, d = data_array.(off) in
                      data_array.(off) <- (s', d);
                      help t
                  | ".rodata" ->
                      let off = l - self#section_addr ".rodata" in
                      let s' = dec_hex l and s, d = rodata_array.(off) in
                      rodata_array.(off) <- (s', d);
                      help t
                  | _ ->
                      let module EU = ELF_utils in
                      if EU.elf_static () then
                        match n with
                        | "__libc_IO_vtables" ->
                            let off =
                              l - self#section_addr "__libc_IO_vtables"
                            in
                            let s' = dec_hex l
                            and s, d = __libc_IO_vtables_array.(off) in
                            __libc_IO_vtables_array.(off) <- (s', d);
                            help t
                        | "__libc_freeres_ptrs" ->
                            let off =
                              l - self#section_addr "__libc_freeres_ptrs"
                            in
                            let s' = dec_hex l
                            and s, d = __libc_freeres_ptrs_array.(off) in
                            __libc_freeres_ptrs_array.(off) <- (s', d);
                            help t
                        | _ -> help t
                      else help t
                end
              | _ -> ()
            in
            help lbs
        end
      in
      let module EU = ELF_utils in
      text_as_data_array <- Array.of_list text_as_data_list;
      rodata_array <- Array.of_list rodata_list;
      data_array <- Array.of_list data_list;
      if EU.elf_static () then begin
        __libc_IO_vtables_array <- Array.of_list __libc_IO_vtables_list;
        __libc_freeres_ptrs_array <- Array.of_list __libc_freeres_ptrs_list
      end
      else ();
      p#process locations;
      text_as_data_list <- Array.to_list text_as_data_array;
      rodata_list <- Array.to_list rodata_array;
      data_list <- Array.to_list data_array;
      if EU.elf_static () then begin
        __libc_IO_vtables_list <- Array.to_list __libc_IO_vtables_array;
        __libc_freeres_ptrs_list <- Array.to_list __libc_freeres_ptrs_array
      end
      else ()

    method data_output =
      let p =
        object (sp)
          method process lbs sym_addr2label =
            let dec_hex (s : int) : string = Printf.sprintf "S_0x%X:\n" s in
            let rec help loc_list =
              match loc_list with
              | (n, l) :: t -> begin
                  match n with
                  | ".text_as_data" ->
                      let off = l - fst text_sec in
                      let s' = dec_hex l in
                      let s, d = text_as_data_array.(off) in
                      ( match Hashtbl.find_opt sym_addr2label l with
                      | Some (sym_label, _) -> (text_as_data_array.(off) <- (sym_label^":\n"^s', d))
                      | None -> (text_as_data_array.(off) <- (s', d)) );
                      help t
                  | ".data" ->
                      let off = l - self#section_addr ".data" in
                      let s' = dec_hex l and s, d = data_array.(off) in
                      ( match Hashtbl.find_opt sym_addr2label l with
                      | Some (sym_label, _) -> (data_array.(off) <- (sym_label^":\n"^s', d))
                      | None -> (data_array.(off) <- (s', d)) );
                      help t
                  | ".rodata" ->
                      let off = l - self#section_addr ".rodata" in
                      let s' = dec_hex l and s, d = rodata_array.(off) in
                      ( match Hashtbl.find_opt sym_addr2label l with
                      | Some (sym_label, _) -> (rodata_array.(off) <- (sym_label^":\n"^s', d))
                      | None -> (rodata_array.(off) <- (s', d)) );
                      help t
                  | ".got" ->
                      let off = l - self#section_addr ".got" in
                      let s' = dec_hex l and s, d = got_array.(off) in
                      ( match Hashtbl.find_opt sym_addr2label l with
                      | Some (sym_label, _) -> (got_array.(off) <- (sym_label^":\n"^s', d))
                      | None -> (got_array.(off) <- (s', d)) );
                      help t
                  | ".bss" ->
                      let off = l - self#section_addr ".bss" in
                      let s' = dec_hex l and s, d = bss_array.(off) in
                      ( match Hashtbl.find_opt sym_addr2label l with
                      | Some (sym_label, _) -> (bss_array.(off) <- (sym_label^":\n"^s', d))
                      | None -> (bss_array.(off) <- (s', d)) );
                      help t
                  | ".data.rel.ro" ->
                      let off = l - self#section_addr ".data.rel.ro" in
                      let s' = dec_hex l and s, d = data_rel_ro_array.(off) in
                      ( match Hashtbl.find_opt sym_addr2label l with
                      | Some (sym_label, _) -> (data_rel_ro_array.(off) <- (sym_label^":\n"^s', d))
                      | None -> (data_rel_ro_array.(off) <- (s', d)) );
                      help t
                  | _ ->
                      let module EU = ELF_utils in
                      if EU.elf_static () then
                        match n with
                        | "__libc_IO_vtables" ->
                            let off =
                              l - self#section_addr "__libc_IO_vtables"
                            in
                            let s' = dec_hex l
                            and s, d = __libc_IO_vtables_array.(off) in
                            ( match Hashtbl.find_opt sym_addr2label l with
                            | Some (sym_label, _) -> (__libc_IO_vtables_array.(off) <- (sym_label^":\n"^s', d))
                            | None -> (__libc_IO_vtables_array.(off) <- (s', d)) );
                            help t
                        | "__libc_freeres_ptrs" ->
                            let off =
                              l - self#section_addr "__libc_freeres_ptrs"
                            in
                            let s' = dec_hex l
                            and s, d = __libc_freeres_ptrs_array.(off) in
                            ( match Hashtbl.find_opt sym_addr2label l with
                            | Some (sym_label, _) -> (__libc_freeres_ptrs_array.(off) <- (sym_label^":\n"^s', d))
                            | None -> (__libc_freeres_ptrs_array.(off) <- (s', d)) );
                            help t
                        | ".tbss" -> help t
                        | _ -> raise (Reassemble_Error ("data_output: " ^ n))
                      else raise (Reassemble_Error ("data_output: " ^ n))
                end
              | _ -> ()
            in
            help lbs

          method zip ll = List.map (fun (h, e) -> h ^ e) ll

          method zip2 ll =
            List.map (fun (h, e) -> h ^ " " ^ string_of_int e ^ "\n") ll

          method insert_dummy =
            let help li =
              match li with
              | (l, s) :: t -> ("s_dummy: \n" ^ l, s) :: t
              | _ ->
                  print_string "empty rodata list\n";
                  li
            in
            rodata_list <- help rodata_list

          method insert_head =
            let rec sublist b e l =
              match l with
              | [] -> failwith "sublist"
              | h :: t ->
                  let tail = if e = 0 then [] else sublist (b - 1) (e - 1) t in
                  if b > 0 then tail else h :: tail
            in
            text_as_data_list <-
              (".section .text_as_data\n", "") :: text_as_data_list;
            rodata_list <- (".section .rodata\n", "") :: rodata_list;
            got_list <- (".section .got\n", "") :: got_list;
            data_list <- (".section .data\n", "") :: data_list;
            bss_list <- (".section .bss\n", "") :: bss_list;
            data_rel_ro_list <-
              (".section .data.rel.ro\n", "") :: data_rel_ro_list;
            let module EU = ELF_utils in
            if EU.elf_static () then begin
              __libc_IO_vtables_list <-
                (".section __libc_IO_vtables, \"wa\"\n", "")
                :: __libc_IO_vtables_list;
              __libc_freeres_ptrs_list <-
                (".section __libc_freeres_ptrs, \"wa\"\n", "")
                :: __libc_freeres_ptrs_list;
              tbss_list <- (".section .tbss\n", "") :: tbss_list
            end
            else ()

          method write_file =
            let oc =
              open_out_gen [ Open_append; Open_creat ] 0o666 "final_data.s"
            in
            let write_file_opt l =
              List.iter
                (fun (x, y) ->
                  output_string oc x;
                  output_string oc y;
                  output_char oc '\n')
                l
            in
            write_file_opt text_as_data_list;
            write_file_opt rodata_list;
            write_file_opt data_list;
            write_file_opt got_list;
            write_file_opt bss_list;
            let module EU = ELF_utils in
            if EU.elf_static () then begin
              write_file_opt __libc_IO_vtables_list;
              write_file_opt __libc_freeres_ptrs_list;
              write_file_opt tbss_list
            end
            else ();
            if List.length data_rel_ro_list > 2 then
              write_file_opt data_rel_ro_list;
            close_out oc

          method de_redunt labels =
            let tset = Hashtbl.create 200 in
            let help item =
              let n, l = item in
              if Hashtbl.mem tset l then () else Hashtbl.replace tset l item;
              ()
            in
            List.iter help labels;
            let help' key value ll = value :: ll in
            Hashtbl.fold help' tset []
        end
      in
      (* process text and process data are two different approaches
       * process text resolve all the references from text section,
       * process data resolve interleave reference among rodata and data sections
       * current we just leverage a heristic methods, which consider all the value
       * inside .data and .rodata sections as addr *)
      (* print_int (List.length bss_list); *)
      (* let temp = p#zip2 locations in *)
      (* List.iter print_string temp; *)
      (* print_int (List.length bss_list); *)
      let module S = Symbol_table_get in
      let sym_addr2label = S.parse_nm () in
      text_as_data_array <- Array.of_list text_as_data_list;
      rodata_array <- Array.of_list rodata_list;
      got_array <- Array.of_list got_list;
      data_array <- Array.of_list data_list;
      bss_array <- Array.of_list bss_list;
      data_rel_ro_array <- Array.of_list data_rel_ro_list;
      let module EU = ELF_utils in
      if EU.elf_static () then begin
        __libc_IO_vtables_array <- Array.of_list __libc_IO_vtables_list;
        __libc_freeres_ptrs_array <- Array.of_list __libc_freeres_ptrs_list;
        tbss_array <- Array.of_list tbss_list
      end
      else ();
      (* data_labels <- p#de_redunt data_labels; *)
      (*
      self#dump_d2d_labels data_labels_reloc;
       *)
      (* )self#dump_c2d_labels label; *)
      p#process locations sym_addr2label;
      p#process data_labels sym_addr2label;
      text_as_data_list <- Array.to_list text_as_data_array;
      rodata_list <- Array.to_list rodata_array;
      got_list <- Array.to_list got_array;
      data_list <- Array.to_list data_array;
      bss_list <- Array.to_list bss_array;
      data_rel_ro_list <- Array.to_list data_rel_ro_array;
      if EU.elf_static () then begin
        __libc_IO_vtables_list <- Array.to_list __libc_IO_vtables_array;
        __libc_freeres_ptrs_list <- Array.to_list __libc_freeres_ptrs_array;
        tbss_list <- Array.to_list tbss_array;
        ()
      end
      else ();
      p#insert_dummy;
      p#insert_head;
      p#write_file

    (*
        method update_list li loc addr =
            let dec_hex (s:int) : string =
              "S_0x"^(Printf.sprintf "%X : " s) in
            let rec help l n acc =
                match (l,n) with
                | ((s,d)::t , 0) ->
                  let s' = dec_hex loc in
                                        acc@[(s',d)]@t
                | (h::t, n) -> help t (n-1) (acc@[h])
                | ([],_) ->
                    raise (Reassemble_Error "error in update list")
                | ([h], n) when n > 0 ->
                    raise (Reassemble_Error "error in update list") in
                let off = loc - addr in
                  if off < 0 then raise (Reassemble_Error "error in update list")
                  else help li off []
   *)
    (* this method might be too slow *)
    method collect_ocaml name =
      if Sys.file_exists name then begin
        let filelines = File.lines_of name
        and p =
          object
            val mutable c : string = ""

            method process line =
              let line' = String.trim line in
              let rev l =
                let s1 = Char.escaped l.[0] ^ Char.escaped l.[1]
                and s2 = Char.escaped l.[2] ^ Char.escaped l.[3]
                and s3 = Char.escaped l.[4] ^ Char.escaped l.[5]
                and s4 = Char.escaped l.[6] ^ Char.escaped l.[7] in
                s4 ^ s3 ^ s2 ^ s1
              in
              let items = Str.split (Str.regexp " +") line' in
              c <- c ^ List.fold_left (fun acc item -> acc ^ item) "" items

            method get_c = c
          end
        in
        Enum.iter p#process filelines;
        p#get_c
      end
      else ""

    (* let's try python3 script *)
    method collect name =
      if Sys.file_exists name then
        let lines = read_file name in
        List.map String.trim lines
      else []

    (* this is a optimizated solution for large size .bss section ELF binary *)
    method collect_bss name =
      if Sys.file_exists name then begin
        let filelines = File.lines_of name
        and p =
          object
            val mutable c : string list = []

            method process line =
              let line' = String.trim line in
              c <- line' :: c

            method get_c = c
          end
        in
        Enum.iter p#process filelines;
        p#get_c
      end
      else [ "" ]
  end

class instrhandler instr_list des' =
  (* this class handle control flow transfer, change concrete addr in the .text section
   * into symbols
   * WATCH OUT:
   *        basically as we use objdump to disassmble the binaries, then currently we can only
   *        handle control flow transfer destination which is provided by objdump
   *        Then if destination is inside one instruction (not one opcode!!), we cannot handle it!
   *        Exceptions will be thrown in that case.
   *)
  object (self)
    val mutable des : string list = des'
    val mutable locs : loc list = []
    val mutable new_instrlist = []

    method print_loclist =
      let dec_hex (s : int) : string = "S_0x" ^ Printf.sprintf "%X" s in
      let help l =
        let d = dec_hex l.loc_addr in
        print_string (l.loc_label ^ ":\n" ^ d ^ "\n")
      in
      List.iter help locs

    method get_instr_list =
      (* update each instr addr in instr_list to corresponding addr in locs *)
      let rec help acc i_list l_list =
        match (i_list, l_list) with
        | i :: ti, h :: tl -> help (set_loc i h :: acc) ti tl
        | [], [] -> List.rev acc
        | _ -> raise (Reassemble_Error "instrhandler error in get_instr_list")
      in
      help [] instr_list locs

    method set_instr_list =
      (* return list of instr addrs *)
      let rec help acc l =
        match l with h :: t -> help (get_loc h :: acc) t | [] -> List.rev acc
      in
      locs <- help [] instr_list
    (* locs <- List.rev locs *)
    (* self#print_loclist *)

    method pp_print l =
      let rec help l =
        match l with
        | (h1, h2) :: t ->
            (printf "item: (%s,%d) ") h1 h2;
            help t
        | [] -> print_string "end\n"
      in
      help l

    method pp_print1 l =
      let rec help l =
        match l with
        (*| h1::t ->  print_string ((string_of_int h1.loc_addr)^"\n"); help t *)
        | h1 :: t ->
            print_string (h1 ^ "\n");
            help t
        | [] -> print_string "end\n"
      in
      help l

    method pp_print2 l =
      let rec help l =
        match l with
        | h1 :: t ->
            print_string (string_of_int h1.loc_addr ^ "\n");
            help t
        | [] -> print_string "end\n"
      in
      help l

    method dump_c2c_labels dl =
      let oc =
        open_out_gen [ Open_append; Open_creat ] 0o666 "final_c2c_label.txt"
      in
      Printf.fprintf oc "%s\n" (String.concat "\n" (List.map dec_hex dl));
      close_out oc

    (* deforesting *)
    method clean_sort ll =
      let ll1 =
        List.map
          (fun l ->
            let l' =
              if String.contains l '=' then String.sub l 1 (String.length l - 1)
              else l
            in
            let l1 = String.sub l' 2 (String.length l' - 2) in
            int_of_string l1)
          ll
      in
      (* ll1 is list of int addrs from labels *)
      let lld' = List.filter (fun l -> l <> 0) ll1 in
      (* lld' is lld' without 0 *)
      let lld1 = unify_int_list lld' in
      List.sort Int.compare lld1

    method clean_sort_bdf ll =
      let ll' =
        List.map
          (fun l ->
            if String.contains l '$' then String.sub l 1 (String.length l - 1)
            else l)
          ll
      in
      let ll'' = List.map (fun l -> String.sub l 2 (String.length l - 2)) ll' in
      let lld = List.map int_of_string ll'' in
      let lld' = List.filter (fun l -> l <> 0) lld in
      let lld1 = unify_int_list lld' in
      let lld2 = List.sort Int.compare lld1 in
      List.sort Int.compare lld1
    (* let lld2 = List.sort Int.compare lld1 in
       List.map (Printf.sprintf "S_0x%X") lld2 *)

    (** Check if symbols in locs can be found in des *)
    method process =
      let dec_hex (s : int) : string = Printf.sprintf "S_0x%X" s in
      let do_update s n = if String.exists s n then s else s ^ "\n" ^ n in
      let rec help acc (llist : loc list) (dlist : int list) =
        match (llist, dlist) with
        | llist', [] -> List.rev_append llist' acc
        | lh :: lt, dh :: dt ->
            (* let _ = Printf.printf "loc addr: %s\n" (dec_hex lh.loc_addr) in
            let _ = Printf.printf "des addr: 0x%x\n" dh in
            let _ = Printf.printf "llist len: %d\n" (List.length lt) in
            let _ = Printf.printf "des len: %d\n" (List.length dt) in
            let _ = Printf.printf "\n" in *)
            let lhd = lh.loc_addr in
            if dh = lhd then
              let lhs = dec_hex lh.loc_addr in
              let label' = do_update lh.loc_label (lhs ^ ":\n") in
              let lh' = { lh with loc_label = label' } in
              help (lh' :: acc) lt dt
            else if dh < lhd then
              (* this is not a label indeed*)
              help (lh :: acc) lt dt
            else
              help (lh :: acc) lt dlist
        | a, b ->
            print_string "reassemble process\n";
            print_string (dec_hex (List.nth b 0) ^ "\n");
            print_string
              ("remaining loc list len: " ^ string_of_int (List.length a) ^ "\n");
            List.iter
              (Printf.printf "\tloc addr: %s\n")
              (List.map (fun l -> dec_hex l.loc_addr) a);
            print_string
              ("remaining des list len:" ^ string_of_int (List.length b) ^ "\n");
            List.iter (Printf.printf "\tdes addr: %s\n") (List.map dec_hex b);
            failwith "undefined des list"
      in
      (* giyeol: print all des *)
      (* let _ = List.iter (Printf.printf "des: %s\n") des in *)
      let des' = self#clean_sort des in
      locs <- List.rev (help [] locs des')
    (** Check if symbols in locs can be found in des *)

    method insert_dummy =
      let help = function
        | h :: t -> { h with loc_label = ".globl main\n" ^ h.loc_label } :: t
        | _ -> failwith "empty loc list"
      in
      locs <- help locs

    method update_loc locs d =
      let dec_hex (s : int) : string = "S_0x" ^ Printf.sprintf "%X:\n" s in
      let identify_des addr1 addr2 = addr1.loc_addr = int_of_string addr2
      and lift_addr addr = dec_hex addr
      and do_update s n = if String.exists s n then s else s ^ "\n" ^ n in
      let rec help d l =
        if identify_des l d then
          { l with loc_label = do_update l.loc_label (lift_addr l.loc_addr) }
        else l
      in
      let items = Str.split (Str.regexp "_") d in
      let d' = List.nth items 1 in
      List.map (help d') locs
  end

class funchandler instr_list u_funcs' =
  (* this class handle control flow transfer, change concrete addr in the .text section
   * into symbols
   * WATCH OUT:
   *        basically as we use objdump to disassmble the binaries, then currently we can only
   *        handle control flow transfer destination which is provided by objdump
   *        Then if destination is inside one instruction (not one opcode!!), we cannot handle it!
   *        Exceptions will be thrown in that case.
   *)
  object (self)
    val mutable funcs : func list = u_funcs'
    val mutable locs : loc list = []
    val mutable new_instrlist = []

    method print_loclist =
      let dec_hex (s : int) : string = "S_0x" ^ Printf.sprintf "%X" s in
      let help l =
        let d = dec_hex l.loc_addr in
        print_string (l.loc_label ^ ":\n" ^ d ^ "\n")
      in
      List.iter help locs

    method get_instr_list =
      let rec help acc i_list l_list =
        match (i_list, l_list) with
        | i :: ti, h :: tl -> help (set_loc i h :: acc) ti tl
        | [], [] -> List.rev acc
        | _ -> raise (Reassemble_Error "funchandler error in get_instr_list")
      in
      help [] instr_list locs

    method set_instr_list =
      let rec help acc l =
        match l with h :: t -> help (get_loc h :: acc) t | [] -> List.rev acc
      in
      locs <- help [] instr_list
    (* self#print_loclist *)

    method pp_print l =
      let rec help l =
        match l with
        | (h1, h2) :: t ->
            (printf "item: (%s,%d) ") h1 h2;
            help t
        | [] -> print_string "end\n"
      in
      help l

    method process =
      let rec help dlist =
        match dlist with
        | d :: t ->
            locs <- self#update_loc locs d;
            help t
        | _ -> ()
      in
      help funcs

    method func_sort ll =
      List.sort (fun f1 f2 -> f1.func_begin_addr - f2.func_begin_addr) ll

    method process2 =
      (*
          let dec_hex (s:int) : string =
              (Printf.sprintf "S_0x%X" s) in
          let do_update s n =
              if String.exists s n then s
              else s^"\n"^n in
*)
      let rec help acc llist flist =
        match (llist, flist) with
        | llist', [] -> List.rev_append llist' acc
        | lh :: lt, fh :: ft ->
            if lh.loc_addr == fh.func_begin_addr then
              let lh' =
                { lh with loc_label = fh.func_name ^ " :\n" ^ lh.loc_label }
              in
              help (lh' :: acc) lt ft
            else if lh.loc_addr > fh.func_begin_addr then help (lh :: acc) lt ft
            else help (lh :: acc) lt flist
        | a, b ->
            (* print_string ((List.nth b 0)^"\n"); *)
            print_string (string_of_int (List.length a) ^ "\n");
            print_string (string_of_int (List.length b) ^ "\n");
            failwith "undefined des list"
      in
      let funcs' = self#func_sort funcs in
      locs <- List.rev (help [] locs funcs')

    method insert_dummy =
      let help = function
        | h :: t -> { h with loc_label = ".globl main\n" ^ h.loc_label } :: t
        | _ -> failwith "empty loc list"
      in
      locs <- help locs

    method update_loc locs d =
      let identify_des addr1 d1 = addr1.loc_addr = d1.func_begin_addr in
      let help d l =
        if identify_des l d then
          { l with loc_label = d.func_name ^ " :\n" ^ l.loc_label }
        else l
      in
      List.map (help d) locs
  end

type instr_type = Single | Double | Triple | Four
type instr_dir = Left | Right

class arm_reassemble =
  let data_set = Hashtbl.create 200
  and plt_hash = Hashtbl.create 50
  and pic_hash = Hashtbl.create 3
  and text_set = Hashtbl.create 30
  and text_as_data_set = Hashtbl.create 200
  and got_as_data_set = Hashtbl.create 30
  and literal_pool_candidates : (int, instr) Hashtbl.t = Hashtbl.create 100
  and instr_addrs_set : (int, instr) Hashtbl.t = Hashtbl.create 100 in

  object (self)
    inherit common_reassemble
    val mutable arch : string = "arm"
    val mutable label : (string * int) list = []

    (* collect relocation info in c2d *)
    val mutable c2d_addr : int list = []

    val mutable c2d_inline_addr_list : int list =
      [] (* for inline data in text section *)

    (* text symbols that should be created
     * e.g.,
     * When we symbolize pointers in instructions like:
     * add r3, pc  --> ldr r3,=S_0x12344
     *
     * The definition of S_0x12344 should be created in the text section.
     * In such cases, we need to collect these symbols and create them
     * by matching corresponding addresses in the text section.
     *)
    val mutable deslist : string list = []

    (* only collect the relocated symbol *)
    val mutable deslist_reloc : int list = []
    val mutable init_array_list : string list = []
    val mutable eh_frame_list : string list = []
    val mutable excpt_tbl_list : string list = []
    val mutable jmpreflist : string list = []
    val mutable sec : section list = []
    val mutable instr_list : instr list = []
    val mutable text_sec : int * int = (0, 0) (* begin addr, size*)
    val mutable plt_sec : int * int = (0, 0) (* begin addr, size*)
    val mutable text_mem_addrs : string list = []

    (* collect all the symbols from code section or from data sections *)
    val mutable symbol_list : int list = []

    method set_arch (arch_name : string) = arch <- arch_name

    method section_collect =
      let filelines = File.lines_of "sections.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x" ^ List.nth items 1)
        and size = int_of_string ("0x" ^ List.nth items 3)
        and secname = List.nth items 0 in
        sec <-
          { sec_name = secname; sec_begin_addr = addr; sec_size = size } :: sec
      in
      Enum.iter help filelines;

      text_mem_addrs <- List.map String.trim (read_file "text_mem.info")

    method plt_collect =
      let filelines = File.lines_of "plts.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = List.nth items 0 in
        let addr' = "0x" ^ String.sub addr 1 (String.length addr - 1)
        and n = List.nth items 1 in
        let items = Str.split (Str.regexp_string "@") n in
        let n' = List.nth items 0 in
        let n'' = String.sub n' 1 (String.length n' - 1) in
        Hashtbl.replace plt_hash (int_of_string addr') n''
      in
      Enum.iter help filelines

    method pic_collect =
      let filelines = File.lines_of "pic_secs.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let name = List.nth items 0
        and addr = int_of_string ("0x" ^ List.nth items 1)
        and size = int_of_string ("0x" ^ List.nth items 3) in
        Hashtbl.replace pic_hash name (addr, size)
      in
      Enum.iter help filelines

    method text_sec_collect =
      let filelines = File.lines_of "text_sec.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x" ^ List.nth items 1)
        and size = int_of_string ("0x" ^ List.nth items 3) in
        text_sec <- (addr, size)
      in
      Enum.iter help filelines

    method plt_sec_collect =
      let filelines = File.lines_of "plt_sec.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x" ^ List.nth items 1)
        and size = int_of_string ("0x" ^ List.nth items 3) in
        plt_sec <- (addr, size)
      in
      Enum.iter help filelines

    method text_as_data_collect =
      let filelines = File.lines_of "text_section_as_data.txt"
      and help l =
        let items = Str.split (Str.regexp ":") l in
        let addr = int_of_string ("0x" ^ List.nth items 0) in
        let data = String.trim (List.nth items 1) in
        (* insert addr * data to text_as_data_set *)
        Hashtbl.replace text_as_data_set addr data
      in
      Enum.iter help filelines

    method got_as_data_collect =
      let filelines = File.lines_of "got_section_as_data.txt"
      and help l =
        let items = Str.split (Str.regexp ":") l in
        let addr = int_of_string ("0x" ^ List.nth items 0) in
        let data = String.trim (List.nth items 1) in
        (* insert addr * data to text_as_data_set *)
        Hashtbl.replace got_as_data_set addr data
      in
      Enum.iter help filelines

    method instr_addrs_collect (instrs : instr list) =
      List.iter
        (fun i ->
          let loc = get_loc i in
          let addr = loc.loc_addr in
          Hashtbl.replace instr_addrs_set addr i)
        instrs

    (* collect all the symbols from code section or from data sections *)

    (*          heriustic analysis
     *        check_sec; check_text; check_plt
     *        Heriustically, we consider any value that fall inside
     *         the range of data/rodata/bss/text/plt sections as suspicious symbols
     *  See this paper's table one for some examples
     *  https://www.utdallas.edu/~zxl111930/file/CCS13.pdf
     *)

    method check_sec (addr : int) : section option =
      let rec help l addr =
        match l with
        | h :: t ->
            let b = h.sec_begin_addr in
            let e = b + h.sec_size in
            if addr >= b && addr < e then Some h else help t addr
        | [] -> None
      in
      help sec addr

    method check_text addr =
      let dec_hex n = Printf.sprintf "%x:" n in
      let judge_mem_list addr =
        let addrs = dec_hex addr in
        (* List.mem addrs text_mem_addrs in *)
        bbn_byloc addrs (Array.of_list text_mem_addrs)
      in
      let b, size = text_sec in
      let e = b + size in
      if addr = 0xffff then false
        (* this is dangerous, when processing 64-bit shared object, for example,
         ssh, the text section range includes memory address 0xffff. However, I
         found instructions like this in the code:
         cmp $0xffff,%eax
      *)
        (*  else if (addr>=b && addr < e) && (judge_mem_list addr) then true *)
      else if addr >= b && addr < e then true
      else false

    method check_text_abd addr =
      let b, size = text_sec in
      let e = b + size in
      if addr >= b && addr <= e then true else false

    method check_plt addr =
      let b, size = plt_sec in
      let e = b + size in
      if addr >= b && addr < e then true else false

    method parse_const c =
      match c with Point s -> s | Normal s -> s | Immediate s -> s

    method build_symbol c =
      let dec_hex (s : int) : string = "0x" ^ Printf.sprintf "%X" s in
      match c with
      | Point s -> "=S_" ^ dec_hex s
      | Normal s -> "=S_" ^ dec_hex s
      | Immediate s -> begin
          failwith "build_symbol: check immediate value" (* "=S_" ^ dec_hex s *)
        end

    method build_plt_symbol c =
      let dec_hex (s : int) : string = Printf.sprintf "0x%x" s in
      let c' = match c with Point s -> s | Normal s -> s in
      try
        let n = Hashtbl.find plt_hash c' in
        match c with Point _ -> n (* is it possible ?*) | Normal _ -> "=" ^ n
      with _ ->
        failwith (Printf.sprintf "exception in build plt symbol: %d\n" c')

    method has_data l = Hashtbl.mem data_set l = true
    method has_text l = Hashtbl.mem text_set l = true
    method has_text_as_data l = Hashtbl.mem text_as_data_set l = true
    method has_got_as_data l = Hashtbl.mem got_as_data_set l = true

    method get_got_as_data_str (l : int) : string option =
      try Some (Hashtbl.find got_as_data_set l) with _ -> None

    method get_got_as_data_int (l : int) : int option =
      try
        let result = Some (Hashtbl.find got_as_data_set l) in
        match result with
        | Some v ->
            let v' = int_of_string ("0x" ^ v) in
            Some v'
        | _ -> None
      with _ -> None

    method add_del_to_literal_pool_candidates (il : instr list) =
      let module TU = TagUtils in
      List.map
        (fun i ->
          let loc = get_loc i in
          let addr = loc.loc_addr in
          if Hashtbl.mem literal_pool_candidates addr then
            TU.replace_tag i (Some Del)
          else i)
        il
    (** Preprocessing step should be done. See
        [ail_parser.ml#remove_literal_pools]. *)

    method v_exp2 (e : exp) (i : instr) (f : instr -> bool) (chk : bool) : exp =
      (* giyeol:  *)
      (* let _ = Printf.printf "v_exp2: %s\n" (pp_print_instr' i) in *)
      let check_test_condition l chk =
        match (l, chk) with
        | _, false -> true
        | Normal v, true -> false
        | _ -> true
      in
      let dec_hex (s : int) : string = Printf.sprintf "0x%X" s in
      match e with
      | Const l -> begin
        (* let _ = Printf.printf "\tConst\n" in *)
          let l' = self#parse_const l in
          match self#check_sec l' with
          | Some s ->
              if check_test_condition l chk = false then e
              else if self#has_data l' then begin
                let s_label = self#build_symbol l in
                let loc' = get_loc i in
                c2d_addr <- loc'.loc_addr :: c2d_addr;
                Label s_label
              end
              else begin
                Hashtbl.replace data_set l' "";
                let s_label = self#build_symbol l in
                label <- (s.sec_name, l') :: label;
                let loc' = get_loc i in
                c2d_addr <- loc'.loc_addr :: c2d_addr;
                Label s_label
              end
          | None ->
              if check_test_condition l chk = false then e
              else if self#check_text l' then
                if self#has_text l' then begin
                  let rb = f i in
                  ();

                  let s_label = self#build_symbol l in
                  let loc' = get_loc i in
                  deslist_reloc <- loc'.loc_addr :: deslist_reloc;
                  Label s_label
                end
                else
                  (* To avoid symbolizing the below cases:
                   * mov.w	r2, #131072	; 0x20000
                   * cmp.w	r3, #131072	; 0x20000
                   *)
                  e
                  (* else begin
                  Hashtbl.replace text_set l' "";
                  let s_label = self#build_symbol l in
                  deslist <- s_label :: deslist;
                  let loc' = get_loc i in
                  deslist_reloc <- loc'.loc_addr :: deslist_reloc;
                  Label s_label
                end *)
              else
                let module
                  (* in dynamically-linked binaries, call to library functions are replaced with their name *)
                  (* not the case in statically-linked binaries, for the reason the name does not exist *)
                  EU =
                  ELF_utils
                in
                if EU.elf_static () = false then
                  if self#check_plt l' then
                    let ns = self#build_plt_symbol l in
                    Label ns
                  else e
                else e
        end
      | Symbol s -> begin
        (* let _ = Printf.printf "\tSymbol\n" in *)
          match s with
          | JumpDes l ->
              if self#check_text l then
                if self#has_text l then
                  let s_label = "S_" ^ dec_hex l in
                  Label s_label
                else begin
                  Hashtbl.replace text_set l "";
                  let s_label = "S_" ^ dec_hex l in
                  deslist <- s_label :: deslist;
                  Label s_label
                end
              else e (* is it possible? *)
          | StarDes sd -> Symbol (StarDes (self#v_exp2 sd i f false))
          | CallDes f ->
              let nl = String.length f.func_name in
              let fn = String.sub f.func_name 2 (nl - 2) in
              let is_dig_loc =
                try
                  ignore (int_of_string fn);
                  true
                with _ ->
                  symbol_list <- f.func_begin_addr :: symbol_list;
                  false
              in
              if is_dig_loc = true then
                let addr = int_of_string fn in
                if self#check_text addr then
                  if self#has_text addr then
                    let s_label = "S_" ^ dec_hex addr in
                    Label s_label
                  else begin
                    Hashtbl.replace text_set addr "";
                    let s_label = "S_" ^ dec_hex addr in
                    deslist <- s_label :: deslist;
                    (* deslist_reloc <- addr::deslist_reloc; *)
                    Label s_label
                  end
                else e
              else e
          | _ -> e
        end
      | Ptr s -> begin
        (* giyeol:  *)
        (* let _ = Printf.printf "\tPtr\n" in *)
          match s with
          | BinOP_PLUS (Arm_Reg (Arm_PCReg r), offset)
            when offset mod 2 = 0
                 && not (Hashtbl.mem instr_addrs_set (get_loc i).loc_addr) ->
              let module AU = ArmUtils in
              let pc_relative_addr = AU.get_pc_relative_addr arch i in
              if self#has_data pc_relative_addr then begin
                (* Haven't seen this case yet *)
                let s_label = "=S_" ^ dec_hex pc_relative_addr in
                let loc' = get_loc i in
                c2d_addr <- loc'.loc_addr :: c2d_addr;
                Label s_label
              end
              else if self#has_text_as_data pc_relative_addr then begin
                let _ = Printf.printf "literal pool candidate: %s: 0x%x\n" (pp_print_instr' i ) pc_relative_addr in
                Hashtbl.replace literal_pool_candidates pc_relative_addr i;
                let s_label = "=S_" ^ dec_hex pc_relative_addr in
                let _ = Printf.printf "text_as_data symbol: %s\n" s_label in
                let _ = Printf.printf "\t%s\n" (pp_print_instr' i) in
                (* [label] is used to create c2d symbols *)
                label <- (".text_as_data", pc_relative_addr) :: label;
                (* for data in text section *)
                let loc' = get_loc i in
                c2d_inline_addr_list <- loc'.loc_addr :: c2d_inline_addr_list;
                Label s_label
              end
              else
                (* giyeol:  *)
                (* let _ = Printf.printf "addr: 0x%x\n" pc_relative_addr in *)
                raise
                  (Reassemble_Error
                     "pc relative addr not in data section and text section")
          | BinOP_PLUS (r, addr) -> begin
              match self#check_sec addr with
              | Some s ->
                  if self#has_data addr then begin
                    let s_label = "S_" ^ dec_hex addr in
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (BinOP_PLUS_S (r, s_label))
                  end
                  else begin
                    Hashtbl.replace data_set addr "";
                    let s_label = "S_" ^ dec_hex addr in
                    label <- (s.sec_name, addr) :: label;
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (BinOP_PLUS_S (r, s_label))
                  end
              | None ->
                  if self#check_text addr then
                    if self#has_text addr then begin
                      let s_label = "S_" ^ dec_hex addr in
                      Ptr (BinOP_PLUS_S (r, s_label))
                    end
                    else begin
                      Hashtbl.replace text_set addr "";
                      let s_label = "S_" ^ dec_hex addr in
                      deslist <- s_label :: deslist;
                      Ptr (BinOP_PLUS_S (r, s_label))
                    end
                  else begin
                    e
                  end
            end
          (* I don't know whether it can go this way, but it doesn't hurt *)
          | BinOP_MINUS (r, addr) -> begin
              match self#check_sec addr with
              | Some s ->
                  if self#has_data addr then begin
                    let s_label = "S_" ^ dec_hex addr in
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (BinOP_MINUS_S (r, s_label))
                  end
                  else begin
                    Hashtbl.replace data_set addr "";
                    let s_label = "S_" ^ dec_hex addr in
                    label <- (s.sec_name, addr) :: label;
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (BinOP_MINUS_S (r, s_label))
                  end
              | None -> e
            end
          | FourOP_PLUS (r1, r2, off, addr) -> begin
              match self#check_sec addr with
              | Some s ->
                  if self#has_data addr then begin
                    let s_label = "S_" ^ dec_hex addr in
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (FourOP_PLUS_S (r1, r2, off, s_label))
                  end
                  else begin
                    Hashtbl.replace data_set addr "";
                    let s_label = "S_" ^ dec_hex addr in
                    label <- (s.sec_name, addr) :: label;
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (FourOP_PLUS_S (r1, r2, off, s_label))
                  end
              | None -> e
            end
          (* I don't know whether it can go this way, but it doesn't hurt *)
          | FourOP_MINUS (r1, r2, off, addr) -> begin
              match self#check_sec addr with
              | Some s ->
                  if self#has_data addr then begin
                    let s_label = "S_" ^ dec_hex addr in
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (FourOP_PLUS_S (r1, r2, off, s_label))
                  end
                  else begin
                    Hashtbl.replace data_set addr "";
                    let s_label = "S_" ^ dec_hex addr in
                    label <- (s.sec_name, addr) :: label;
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (FourOP_PLUS_S (r1, r2, off, s_label))
                  end
              | None -> e
            end
          | JmpTable_PLUS (addr, r, off) -> begin
              match self#check_sec addr with
              | Some s ->
                  if self#has_data addr then begin
                    let s_label = "S_" ^ dec_hex addr in
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (JmpTable_PLUS_S (s_label, r, off))
                  end
                  else begin
                    Hashtbl.replace data_set addr "";
                    let s_label = "S_" ^ dec_hex addr in
                    label <- (s.sec_name, addr) :: label;
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (JmpTable_PLUS_S (s_label, r, off))
                  end
              | None ->
                  if self#check_text addr then
                    if self#has_text addr then begin
                      let s_label = "S_" ^ dec_hex addr in
                      let loc' = get_loc i in
                      deslist_reloc <- loc'.loc_addr :: deslist_reloc;
                      Ptr (JmpTable_PLUS_S (s_label, r, off))
                    end
                    else begin
                      Hashtbl.replace text_set addr "";
                      let s_label = "S_" ^ dec_hex addr in
                      let loc' = get_loc i in
                      deslist_reloc <- loc'.loc_addr :: deslist_reloc;
                      deslist <- s_label :: deslist;
                      Ptr (JmpTable_PLUS_S (s_label, r, off))
                    end
                  else e
            end
          | JmpTable_MINUS (addr, r, off) -> begin
              match self#check_sec addr with
              | Some s ->
                  if self#has_data addr then begin
                    let s_label = "-S_" ^ dec_hex addr in
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (JmpTable_MINUS_S (s_label, r, off))
                  end
                  else begin
                    Hashtbl.replace data_set addr "";
                    let s_label = "-S_" ^ dec_hex addr in
                    label <- (s.sec_name, addr) :: label;
                    let loc' = get_loc i in
                    c2d_addr <- loc'.loc_addr :: c2d_addr;
                    Ptr (JmpTable_MINUS_S (s_label, r, off))
                  end
              | None ->
                  if self#check_text addr then
                    if self#has_text addr then begin
                      let s_label = "-S_" ^ dec_hex addr in
                      let loc' = get_loc i in
                      deslist_reloc <- loc'.loc_addr :: deslist_reloc;
                      Ptr (JmpTable_MINUS_S (s_label, r, off))
                    end
                    else begin
                      Hashtbl.replace text_set addr "";
                      let s_label = "-S_" ^ dec_hex addr in
                      let loc' = get_loc i in
                      deslist_reloc <- loc'.loc_addr :: deslist_reloc;
                      deslist <- s_label :: deslist;
                      Ptr (JmpTable_MINUS_S (s_label, r, off))
                    end
                  else e
            end
          | _ -> e
        end
      | _ ->
        (* giyeol: *)
        (* let _ = Printf.printf "\tOther Exp\n" in *)
        e

    method vinst2 (arch : string) (f : instr -> bool) (i : instr) : instr =
      let tag' = get_tag i in
      if tag' = Some Del then i
      else
        let is_test op =
          match op with
          | Intel_OP io -> begin
              match io with
              | Intel_CommonOP (Intel_Compare TEST)
              | Intel_CommonOP (Intel_Compare TESTL)
              | Intel_CommonOP (Intel_Compare TESTW)
              | Intel_CommonOP (Intel_Compare TESTB) ->
                  true
              | _ -> false
            end
          | Arm_OP _ -> false
        in
        match i with
        | SingleInstr (p, l, pre, _, _) -> i
        | DoubleInstr (p, e, l, pre, tag, tags) ->
            DoubleInstr (p, self#v_exp2 e i f false, l, pre, tag, tags)
        | TripleInstr
            ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), _, _),
              Const (Point odd_addr),
              Reg (Arm_Reg (Arm_CommonReg reg)),
              l,
              pre,
              tag,
              tags )
          when odd_addr mod 2 = 1 && self#check_text (odd_addr - 1) ->
            (* To handle odd addresses in Thumb mode *)
            Hashtbl.replace text_set (odd_addr - 1) "";
            let s_label = self#build_symbol (Point (odd_addr - 1)) in
            deslist <- s_label :: deslist;
            TripleInstr
              ( Arm_OP (Arm_CommonOP (Arm_Assign LDR), None, None),
                Label s_label,
                Reg (Arm_Reg (Arm_CommonReg reg)),
                l,
                pre,
                tag,
                tags )
        | TripleInstr (p, e1, e2, l, pre, Some (Sym value), tags) -> begin
            (* The following patterns should not be handled here.
             * They might be converted into library calls.
             * For example:
             * ldr r3,=0x27110
             * will be converted into
             * ldr r3,=__stack_chk_guard
             *)
            (* `Point value` will be symbolized
             * when vinst2 is called by visit_type_infer_analysis *)
            let ldr_op = Arm_OP (Arm_CommonOP (Arm_Assign LDR), None) in
            let pointer_instr =
              TripleInstr (ldr_op, Const (Point value), e2, l, pre, None, tags)
            in
            let deref_instr =
              TripleInstr (ldr_op, Const (Point value), e2, l, pre, Some Deref, tags)
            in
            match self#check_sec value with
            | Some s ->
                (* giyeol: debug *)
                (* let _ = Printf.printf "\tsection: %s\n" s.sec_name in *)
                if s.sec_name = ".rodata" then pointer_instr
                else if s.sec_name = ".got" then begin
                  match self#get_got_as_data_int value with
                  | Some deref_value -> begin
                      match self#check_sec deref_value with
                      | Some deref_s ->
                          if deref_s.sec_name = ".data.rel.ro" then deref_instr
                          else if deref_s.sec_name = ".bss" then deref_instr
                          else pointer_instr
                      | None -> begin
                          if arch = "thumb" then begin
                            (* got-to-text references are odd numbers in ARM Thumb mode *)
                            let deref_value' =
                              if deref_value mod 2 = 1 then deref_value - 1
                              else deref_value
                            in
                            if self#check_text deref_value' then deref_instr
                            else pointer_instr
                          end
                          else pointer_instr
                        end
                    end
                  | None -> pointer_instr
                end
                else deref_instr
            | None -> pointer_instr
          end
        | TripleInstr (p, e1, e2, l, pre, tag, tags) when is_test p ->
            TripleInstr
              (p, self#v_exp2 e1 i f true, self#v_exp2 e2 i f true, l, pre, tag, tags)
        | TripleInstr (p, e1, e2, l, pre, tag, tags) ->
            TripleInstr
              ( p,
                self#v_exp2 e1 i f false,
                self#v_exp2 e2 i f false,
                l,
                pre,
                tag,
                tags )
        | FourInstr (p, e1, e2, e3, l, pre, tag, tags) ->
            FourInstr (p, e1, self#v_exp2 e2 i f false, e3, l, pre, tag, tags)
        | FifInstr (p, e1, e2, e3, e4, l, pre, tag, tags) ->
            FifInstr (p, e1, self#v_exp2 e2 i f false, e3, e4, l, pre, tag, tags)

    method vinst_symbol (f : instr -> bool) (i : instr) : instr =
      let module TU = TagUtils in
      match i with
      | TripleInstr (p, Label label, e2, l, pre, tag, tags) when contains ~str:label ~sub:"=S_"
        -> begin
          let sub_label = String.sub label 3 (String.length label - 3) in
          let addr = int_of_string sub_label in
          match self#check_sec addr with
          | Some s -> begin
              if s.sec_name = ".rodata" then
                (* Assume that the "pointer" matters, no need to change it *)
                i
              else
                (* Assume that the "dereferenced value" of the pointer matters *)
                let new_instr = TU.replace_tag i (Some Deref) in
                new_instr
            end
          | None ->
              if self#has_text_as_data addr then
                let new_instr = TU.replace_tag i (Some Deref) in
                new_instr
              else i
        end
      | _ -> i

    method jmp_table_rewrite (instrs : instr list) =
      instrs

    method jmp_table_rewrite64 (instrs : instr list) =
      instrs

    method visit_heuristic_analysis (instrs : instr list) =
      let func (i : instr) : bool =
        let l = get_loc i in
        self#check_text l.loc_addr
      in
      instr_list <- instrs;
      let tl = List.map (self#vinst2 arch func) instrs in
      let module EU = ELF_utils in
      let addr_len =
        match (EU.elf_32 (), EU.elf_exe ()) with
        (* ELF 32-bit LSB executable *)
        | true, true -> 7
        (* ELF 64-bit LSB executable *)
        | false, true -> 6
        (* ELF LSB shared object *)
        | _, false -> 4
      in
      let tl1 =
        List.map
          (fun l ->
            try
              let s' = String.sub l 2 addr_len in
              int_of_string s'
            with _ ->
              let s' = String.sub l 3 addr_len in
              int_of_string s')
          deslist
      in
      symbol_list <- List.rev_append (List.rev tl1) symbol_list;
      tl
    (* self#add_del_to_literal_pool_candidates tl; *)

    method visit_type_infer_analysis (bbl : bblock list) (instrs : instr list) =
      (*let's do type inference analysis on suspicious concerete value*)
      let func (i : instr) : bool = true in
      instr_list <- instrs;
      List.map (self#vinst2 arch func) instrs

    (* check whether it is a library or executable *)
    method check_32 =
      let filelines : string list = read_file "elf.info" in
      let info = List.nth filelines 0 in
      if String.exists info "32-bit" then true else false

    method check_exe =
      let filelines : string list = read_file "elf.info" in
      let info = List.nth filelines 0 in
      String.exists info "executable"
      || String.exists info "LSB shared object"
         && String.exists info "ld-linux"

    method share_lib_processing (instrs : instr list) =
      let is_32 = self#check_32 in
      let is_exe = self#check_exe in
      if is_32 = false || is_exe = true then instrs
      else
        let helper = new lib32_helper instrs in
        let labels = helper#traverse in
        (*
              let () = List.iter (fun addr -> print_string ((dec_hex addr)^"\n"))
                                 labels in
           *)

        (* print_int (List.length labels); *)
        let aux addr =
          match self#check_sec addr with
          | Some s -> label <- (s.sec_name, addr) :: label
          | None -> failwith "unsupport section info in share_lib processing"
        in
        List.iter aux labels;
        helper#get_instrs

    (* redundunt with check_sec *)
    method check_bss addr =
      let rec help l addr =
        match l with
        | h :: t ->
            if h.sec_name = ".bss" then
              let b = h.sec_begin_addr in
              let e = b + h.sec_size in
              if addr >= b && addr < e then true else help t addr
            else help t addr
        | [] -> false
      in
      help sec addr

    method update_deslist_with_initarray =
      let _ = Sys.command "python3 parse_init_array.py" in
      init_array_list <- read_file "init_array_new.info"

    method update_deslist_with_ehframe =
      let _ = Sys.command "python3 exception_process.py eh_frame" in
      eh_frame_list <- read_file "eh_frame.info"

    method update_deslist_with_excp_tbl =
      let _ = Sys.command "python3 exception_process.py gcc_exception_table" in
      excpt_tbl_list <- read_file "gcc_exception_table.info"

    method dump_c2c_labels dl =
      let oc =
        open_out_gen [ Open_append; Open_creat ] 0o666 "final_c2c_label.txt"
      in
      Printf.fprintf oc "%s\n" (String.concat "\n" (List.map dec_hex dl));
      close_out oc

    method dump_c2d_labels dl =
      let zip l = fst l ^ " : " ^ dec_hex (snd l) in
      let oc =
        open_out_gen [ Open_append; Open_creat ] 0o666 "final_c2d_label.txt"
      in
      Printf.fprintf oc "%s\n" (String.concat "\n" (List.map dec_hex dl));
      close_out oc

    method dump_c2d_inline_labels label_list =
      let oc =
        open_out_gen
          [ Open_append; Open_creat ]
          0o666 "final_c2d_inline_label.txt"
      in
      Printf.fprintf oc "%s\n"
        (String.concat "\n" (List.map dec_hex label_list));
      close_out oc

    method adjust_loclabel instr_list =
      self#update_deslist_with_initarray;
      self#dump_c2c_labels deslist_reloc;
      let t = List.rev_append (List.rev init_array_list) deslist in
      let p = new instrhandler instr_list t in
      (*
          self#update_deslist_with_ehframe;
          self#update_deslist_with_excp_tbl;
          let p = new instrhandler instr_list deslist in
*)

      p#set_instr_list;
      let _ = Printf.printf "adjust_loclabel\n" in
      p#process;
      p#get_instr_list

    (* in the data section and rodata section, jmptable could have ref into text section*)
    method adjust_jmpref instr_list =
      let p = new instrhandler instr_list jmpreflist in
      p#set_instr_list;
      let _ = Printf.printf "adjust_jmpref\n" in
      p#process;
      p#get_instr_list

    method adjust_funclabel u_funcs instr_list =
      let p = new funchandler instr_list u_funcs in
      p#set_instr_list;
      p#process2;
      p#insert_dummy;
      p#get_instr_list

    method adjust_globallabel (g_bss : (string * string * string) list)
        (instr_list : string list) =
      let remove_leading_zeros s =
        let rec aux s =
          if String.length s > 0 && String.get s 0 = '0' then
            aux (String.sub s 1 (String.length s - 1))
          else s
        in
        aux s
      in
      let g_bss' = List.filter (fun (a, n, t) -> String.exists n "@") g_bss in
      let labels = List.map (fun (a, n, t) -> remove_leading_zeros a) g_bss'
      and size = List.length g_bss' + 1 in
      let gbss_hs = Hashtbl.create size in

      let fill_gbss_hs (addr, n, t) =
        (* remove front 0s *)
        let addr' = "S_0x" ^ remove_leading_zeros addr in
        if String.exists t "COPY" then
          let n' = List.nth (Str.split (Str.regexp "@") n) 0 in
          Hashtbl.replace gbss_hs addr' n'
        else if String.exists t "GLOB_DAT" then
          (* TODO: *)
          let n' = List.nth (Str.split (Str.regexp "@") n) 0 in
          Hashtbl.replace gbss_hs addr' n'
        else Hashtbl.replace gbss_hs addr' n
      in

      let check_if_contains_gbss_symbol l =
        try Some (List.find (fun i -> String.exists l i) labels)
        with Not_found -> None
      and sub l key =
        let key' = "S_0x" ^ key in
        try
          let v = Hashtbl.find gbss_hs key' in
          Str.replace_first (Str.regexp key') v l
        with _ -> failwith "exception in adjust_globallabel"
      in

      let symbolize_gbss_symbols l =
        match check_if_contains_gbss_symbol l with
        | Some k -> sub l k
        | None -> l
      in
      List.iter fill_gbss_hs g_bss';
      List.map symbolize_gbss_symbols instr_list

    method data_dump funcs =
      let dec_hex (s : int) : string = "S_0x" ^ Printf.sprintf "%X" s in
      self#dump_c2d_labels c2d_addr;
      self#dump_c2d_inline_labels c2d_inline_addr_list;
      let export_datas = self#export_data_dump in
      let t = List.rev_append (List.rev label) export_datas in
      let p = new datahandler t in
      p#text_sec_collect;
      p#set_datas funcs;
      let templist = p#get_textlabel in
      jmpreflist <- List.map (fun l -> dec_hex l) templist;
      let ret = p#data_output in
      ret
    (* add labels to data sections *)

    method data_dump_1 =
      let dec_hex (s : int) : string = "S_0x" ^ Printf.sprintf "%X" s in
      let export_datas = self#export_data_dump in
      let t = List.rev_append (List.rev label) export_datas in
      let p = new datahandler t in
      p#text_sec_collect;
      p#set_datas_1;
      let templist = p#get_textlabel in
      jmpreflist <- List.map (fun l -> dec_hex l) templist;
      List.rev_append (List.rev templist) symbol_list

    method dump_funclist bs fn =
      let dec_hex (s : int) : string = "0x0" ^ Printf.sprintf "%x" s in
      let oc = open_out_gen [ Open_append; Open_creat ] 0o666 fn in
      List.iter (fun l -> Printf.fprintf oc "%s\n" (dec_hex l)) bs;
      close_out oc

    (* unify symbol list and pattern match list, to get the unified function
       list *)
    method unify_funclist sl1 sl2 =
      self#dump_funclist sl1 "union_1.txt";
      self#dump_funclist sl2 "union_2.txt";
      let sl1 = unify_int_list sl1 in
      let sl2 = unify_int_list sl2 in
      let sl1 = List.sort Int.compare sl1 in
      let sl2 = List.sort Int.compare sl2 in
      let rec intersect l1 l2 =
        match l1 with
        | [] -> []
        | h1 :: t1 -> begin
            match l2 with
            | [] -> []
            | h2 :: t2 when h1 < h2 -> intersect t1 l2
            | h2 :: t2 when h1 > h2 -> intersect l1 t2
            | h2 :: t2 -> begin
                match intersect t1 t2 with
                | [] -> [ h1 ]
                | h3 :: t3 as l when h3 = h1 -> l
                | h3 :: t3 as l -> h1 :: l
              end
          end
      in
      print_int (List.length sl1);
      print_string "\n";
      print_int (List.length sl2);
      print_string "\n";
      intersect sl1 sl2

    method init_array_dump =
      if List.length init_array_list <> 0 then begin
        let module EU = ELF_utils in
        if EU.elf_32 () then begin
          let oc =
            open_out_gen [ Open_append; Open_creat ] 0o666 "final_data.s"
          in
          Printf.fprintf oc
            "\n.section        .ctors,\"aw\",%%progbits\n.align 4\n";
          List.iter (fun l -> Printf.fprintf oc ".long %s\n" l) init_array_list;
          close_out oc
        end
        else begin
          let oc =
            open_out_gen [ Open_append; Open_creat ] 0o666 "final_data.s"
          in
          Printf.fprintf oc
            "\n.section        .ctors,\"aw\",%%progbits\n.align 8\n";
          List.iter (fun l -> Printf.fprintf oc ".quad %s\n" l) init_array_list;
          close_out oc
        end
      end
      else ()

    method export_data_dump =
      ignore (Sys.command "python3 export_data.py");
      let addrs = List.map String.trim (read_file "export_datas.info") in
      let aux l =
        match self#check_sec l with
        | Some s -> (s.sec_name, l)
        | None -> failwith "unsupport export data"
      in
      List.map (fun ls -> aux (int_of_string ls)) addrs

    method ehframe_dump = Sys.command "cat eh_frame.data >> final.s"

    method excpt_tbl_dump =
      Sys.command "cat gcc_exception_table.data >> final.s"

    method reassemble_dump u_funcs = self#data_dump u_funcs
    method reassemble_dump_1 = self#data_dump_1

    method add_func_label ufuncs instrs =
      let rec help acc fl il =
        match (fl, il) with
        | [], il' -> List.rev_append il' acc
        | hf :: tf, hi :: ti ->
            let iloc = get_loc hi in
            if hf.func_begin_addr = iloc.loc_addr then
              let iloc' =
                {
                  iloc with
                  loc_label = ".ltorg\n" ^ hf.func_name ^ ":\n" ^ iloc.loc_label;
                }
              in
              let hi' = set_loc hi iloc' in
              help acc tf (hi' :: ti)
            else if hf.func_begin_addr < iloc.loc_addr then help acc tf il
            else help (hi :: acc) fl ti
        | hf :: tf, [] ->
            (* failwith ("failed in adding function "^(hf.func_name)^" in add_func_label"); *)
            acc
      in
      List.rev (help [] ufuncs instrs)

    method add_bblock_label bbl instrs =
      let rec help acc bl il =
        match (bl, il) with
        | [], il' -> List.rev_append il' acc
        | ib :: tb, hi :: ti ->
            let iloc = get_loc hi and bloc = ib.bblock_begin_loc in
            if bloc.loc_addr = iloc.loc_addr then
              let iloc' =
                {
                  iloc with
                  loc_label = ib.bblock_name ^ ":\n" ^ iloc.loc_label;
                }
              in
              let hi' = set_loc hi iloc' in
              help (hi' :: acc) tb ti
            else help (hi :: acc) bl ti
        | _ -> failwith "failed in add_bblock_label"
      in
      let bbl' =
        List.sort
          (fun b1 b2 ->
            b1.bblock_begin_loc.loc_addr - b2.bblock_begin_loc.loc_addr)
          bbl
      in
      List.rev (help [] bbl' instrs)

    method pp_print_1 l =
      let rec help l =
        match l with
        | h :: t ->
            (printf "item: %s ") h;
            help t
        | [] -> print_string "end\n"
      in
      help l

    method unify_loc1 instrs =
      let p =
        object
          val mutable last_label = ""
          val mutable remove_l = []

          method update_instrs instrs =
            remove_l <- List.rev remove_l;
            let rec help il rl acc =
              match (il, rl) with
              | ih :: it, rh :: rt ->
                  let lo = get_loc ih in
                  if lo.loc_addr = rh then
                    let ih' =
                      set_loc ih
                        {
                          loc_label = "";
                          loc_addr = lo.loc_addr;
                          loc_visible = true;
                        }
                    in
                    help it rt (ih' :: acc)
                  else help it rl (ih :: acc)
              | il', [] -> List.rev_append acc il'
              | [], rl' ->
                  failwith "can't find coresponding address in unify_loc"
            in
            help instrs remove_l []

          method set_list instrs =
            let dec_hex (s : int) : string = "0x" ^ Printf.sprintf "%X" s in
            let help i =
              let lo = get_loc i in
              if lo.loc_label <> "" && lo.loc_label = last_label then begin
                (* remove it *)
                (* bug :  new inserted instruction would have same location
                            with the above instruction, which means it will remove
                          the above instruction's label *)
                remove_l <- lo.loc_addr :: remove_l;
                print_string last_label;
                print_string "\n";
                print_string (dec_hex lo.loc_addr);
                print_string "\n"
              end
              else last_label <- lo.loc_label
            in
            List.iter help instrs
        end
      in
      p#set_list instrs;
      p#update_instrs instrs

    method unify_loc instrs =
      let p =
        object
          val mutable last_label = ""

          method unify_instrs instrs =
            let rec help acc il =
              match il with
              | [] -> acc
              | ih :: it ->
                  let lo = get_loc ih in
                  if lo.loc_label <> "" && lo.loc_label = last_label then
                    (* remove it *)
                    let ih' =
                      set_loc ih
                        {
                          loc_label = "";
                          loc_addr = lo.loc_addr;
                          loc_visible = true;
                        }
                    in
                    help (ih' :: acc) it
                  else begin
                    last_label <- lo.loc_label;
                    help (ih :: acc) it
                  end
            in
            List.rev (help [] instrs)
        end
      in
      p#unify_instrs instrs

    initializer
      self#section_collect;
      self#plt_collect;
      self#plt_sec_collect;
      self#text_sec_collect;
      self#text_as_data_collect;
      self#got_as_data_collect
  end
