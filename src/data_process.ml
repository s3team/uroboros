open Batteries

open Printf
open Visit
open Type
open Ail_utils

open Pp_print


exception Data_Process_Error of string;;

let rev_map f l =
  let rec aux f l acc =
    match l with
    | h::t -> aux f t ((f h)::acc)
    | [] -> acc in
  f l []


class datahandler_tramp (label' : (string*int) list) =
object (self)
  val mutable sec : section list = []
  val mutable data : string = ""
  val mutable rodata : string = ""
  val mutable got : string = ""
  val mutable bss : string list = []

  val mutable text_mem_addrs: string list = []

  val mutable data_labels : (string*int) list = []
  val mutable text_labels : int list = []

  val mutable label : (string*int) list = label'
  val mutable label_set : (int) list = []

  val mutable data_list: (string*string) list = []
  val mutable rodata_list: (string*string) list = []
  val mutable got_list: (string*string) list = []
  val mutable bss_list: (string*string) list = []

  val mutable data_array: (string*string) array = [||]
  val mutable rodata_array: (string*string) array = [||]
  val mutable got_array: (string*string) array = [||]
  val mutable bss_array: (string*string) array = [||]

  val mutable text_sec: (int*int) = (0,0)  (* begin addr, size*)
  val mutable locations = []

  val mutable in_jmptable = false

  method set_datas funcs =
    self#section_collect;
    self#data_collect;

    data_list <- self#data_transform data;
    rodata_list <- self#data_transform rodata;
    got_list <- self#data_transform got;
    bss_list <- self#data_bss bss;
    locations <- self#label_locate;

    label_set <- List.map snd label;

    (* herustic one: reference cannot go to the middle of a instruction *)
    text_mem_addrs <- List.map (String.trim) (read_file "text_mem.info");

    self#data_refer_solve funcs

  method get_textlabel =
    (* generate trampoline section *)
    self#trampoline_generate text_labels;
    text_labels

  method text_sec_collect =
    let filelines = File.lines_of "text_sec.info"
    and help l =
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x"^(List.nth items 1))
      and size = int_of_string ("0x"^(List.nth items 3)) in
      text_sec <- (addr, size)
    in
    Enum.iter help filelines


  method check_text addr =
    let dec_hex n =
  	  (Printf.sprintf "%x:" n) in
    let judge_mem_list addr =
	  let addrs = dec_hex addr in
	  List.mem addrs text_mem_addrs in
    let (b,size) = text_sec in
    let e = b + size in
    if addr = 0xffff then false
    (* this is dangerous, when processing 64-bit shared object, for example,
    ssh, the text section range includes memory address 0xffff. However, I
    found instructions like this in the code:
     cmp $0xffff,%eax
     *)
    else if (addr>=b && addr < e) && (judge_mem_list addr) then true
    else false

  (* this method solve all the references in the .data .rodata sections *)
  method data_refer_solve funcs =
    let begin_addrs = List.map (fun f -> f.func_begin_addr) funcs in
    let check_func_begin v =
      List.mem v begin_addrs in
    (* this method traverse data list, check each four byte value *)
    let get_v s =
      let items = Str.split (Str.regexp " +") s in
      let v = List.nth items 1 in
      String.sub v 2 ((String.length v)-2) in
    let rec traverse acc l =
      let aux vs =
        let v = int_of_string vs in
        (Printf.sprintf "%X" v) in
      match l with
      | [] -> acc
      | h::[] -> h::acc
      | h1::h2::[] -> h2::h1::acc
      | h1::h2::h3::[] -> h3::h2::h1::acc
      | (l1,v1)::(h2,v2)::(l3,v3)::(l4,v4)::t ->
         (
           let v_str = "0x"^(get_v v4)^(get_v v3)^(get_v v2)^(get_v v1) in
           match string_to_int32 v_str with
           | Some v ->
              begin
                match (self#check_sec v) with
                | Some s ->
                   (
                     begin
                       in_jmptable <- false;
                       (* heuristic two:  we assume the addresses are all
  4-byte aligned. *)
                       traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                     end
                   )
                | None ->
                   (
                     match (self#check_text v) with
                     | true ->
                        (
                          text_labels <- v::text_labels;
                       	  traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                        (* if it is a begin addr of a function *)
                        (*
                          if (check_func_begin v) = false then
                            begin
                              if (self#check_jmptable l1) = false then
                                begin
                                  in_jmptable <- false;
                       		      traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                                end
                              else
                                begin
                                  in_jmptable <- true;
                                  let v_str' = aux v_str in
                                  text_labels <- v::text_labels;
                       		      traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                                end
                            end
                          else
                            begin
                              (*      in_jmptable <- false; *)
                              in_jmptable <- true;
                              let v_str' = aux v_str in
                              text_labels <- v::text_labels;
                       		  traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                            end
                         *)
                        )
                     | false ->
                        begin
                          (* in_jmptable <- false; *)
                       	  traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                        end
                   )
              end
           | None ->
              begin
                (* in_jmptable <- false; *)
                traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
              end
         ) in

    self#add_data_label;
    data_list <- List.rev (traverse [] data_list);
    rodata_list <- List.rev (traverse [] rodata_list);
    got_list <- List.rev (traverse [] got_list)

  method check_jmptable addrs =
    (* heuristic three :  jump table resolving *)
    (* our judgement of jmptable in rodata section

        1. begin with a label (probably indicates a valid des?)
        2. last time we have defined a label (no byte value embedded)
        3. all these labels are within one function (how to check it?)
     *)
    if in_jmptable = true then
      true
    else
      begin
        try
          let addr = int_of_string addrs in
          if List.mem addr label_set then
            begin
              in_jmptable <- true;
              true
            end
          else
            false
        with _ -> false
      end


  method pp_print l =
    let rec help l =
      match l with
      | (h1,h2)::t -> (printf "item: (%s,%d) ") h1 h2; help t
      | [] -> print_string "end\n" in
    help l

  method section_collect =
    let filelines = File.lines_of "sections.info"
    and help l =
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x"^(List.nth items 1))
      and size = int_of_string ("0x"^(List.nth items 3))
      and secname = List.nth items 0 in
      sec <- {sec_name=secname; sec_begin_addr=addr;
              sec_size=size}::sec
    in
    Enum.iter help filelines

  method section_offset name addr =
    let rec help l =
      match l with
      | h::t -> if h.sec_name = name then addr - h.sec_begin_addr
                else help t
      | [] -> raise (Data_Process_Error "fail in section offset") in
    help sec

  method section_addr name =
    let rec help l =
      match l with
      | h::t -> if h.sec_name = name then h.sec_begin_addr
                else help t
      | [] -> raise (Data_Process_Error "fail in section addr") in
    help sec

  method data_collect =
    data <- self#collect "data.info";
    rodata <- self#collect "rodata.info";
    (* got <- self#collect "got.info"; *)
    bss <- self#collect_bss "bss.info";

  method sec_transform s =
    match s with
    | ".rodata" -> ".rodata"
    | ".got" -> ".got"
    | "bss" -> ".bss"
    | _ -> ".rodata"

  method check_sec addr =
    let rec help l addr =
      match l with
      | h::t -> (
        let b = h.sec_begin_addr in
        let e = b + h.sec_size in
        if (addr>=b && addr < e) then
          Some (h)
        else  help t addr )
      | [] -> None in
    help sec addr

  method pp_print_2 l =
    let rec help l =
      match l with
      | (h1,h2)::t -> (printf "item: (%s,%d) ") h1 h2; help t
      | [] -> print_string "end\n" in
    help l

  method pp_print_1 l =
    let rec help l =
      match l with
      | h::t -> (printf "item: %s ") h; help t
      | [] -> print_string "end\n" in
    help l

  method data_transform data_str =
    let l = String.length data_str in
    if l mod 2 <> 0 then failwith "data collection error\n"
    else
      let p = object
          val mutable x: (string*string) list = []
          method get_list = x
          method set_list i = x <- ("",".byte 0x"^i)::x
        end
      and help s =
        let rec help' i l =
          if i < 0 then l else help'(i-2) ((Char.escaped(s.[i-1])^Char.escaped(s.[i]))::l) in
        help' (String.length s - 1) [] in
      let data_list = List.rev (help data_str) in
      let () = List.iter p#set_list data_list in
      p#get_list

  method data_bss data_list =
    let p = object
        val mutable x: (string*string) list = []
        method get_list = x
        method set_list i = x <- ("",i)::x
      end in
    List.iter p#set_list data_list;
    p#get_list


  method label_locate =
    let rec help l acc =
      match l with
      | (s,l)::t -> let offset = self#section_offset s l in
                    (s,offset)::acc
      | [] -> acc in
    List.fold_left help label []

  method add_data_label =
    let p = object(sp)
              method process lbs =
                let dec_hex (s:int) : string =
                  (Printf.sprintf "0x%X" s) in
                let rec help loc_list =
                  match loc_list with
                  | (n, l)::t -> begin
                      match n with
                      | ".data" ->
                         ( let off = l - (self#section_addr ".data") in
                           let s' = dec_hex l
                           and (s,d) = data_array.(off) in
                           data_array.(off) <- (s', d);
                           help t )
                      | ".rodata" ->
                         ( let off = l - (self#section_addr ".rodata") in
                           let s' = dec_hex l
                           and (s,d) = rodata_array.(off) in
                           rodata_array.(off) <- (s', d);
                           help t )
                      | _ -> help t
                    end
                  | _ -> ()
                in
                help lbs
            end in
    rodata_array <- Array.of_list rodata_list;
    data_array <- Array.of_list data_list;
    p#process locations;
    rodata_list <- Array.to_list rodata_array;
    data_list <- Array.to_list data_array;

  method data_output =
    let p = object(sp)
              method process lbs =
                let dec_hex (s:int) : string =
                  (Printf.sprintf "S_0x%X : " s) in
                let rec help loc_list =
                  match loc_list with
                  | (n, l)::t -> begin
                      match n with
                      | ".data" ->
                         ( let off = l - (self#section_addr ".data") in
                           let s' = dec_hex l
                           and (s,d) = data_array.(off) in
                           data_array.(off) <- (s', d);
                           help t )
                      | ".rodata" ->
                         ( let off = l - (self#section_addr ".rodata") in
                           let s' = dec_hex l
                           and (s,d) = rodata_array.(off) in
                           rodata_array.(off) <- (s', d);
                           help t )
                      | ".got" ->
                         ( let off = l - (self#section_addr ".got") in
                           let s' = dec_hex l
                           and (s,d) = got_array.(off) in
                           got_array.(off) <- (s', d);
                           help t )
                      | ".bss" ->
                         ( let off = l - (self#section_addr ".bss") in
                           let s' = dec_hex l
                           and (s,d) = bss_array.(off) in
                           bss_array.(off) <- (s', d);
                           help t )
                      | _ -> raise (Data_Process_Error n)
                    end
                  | _ -> ()
                in
                help lbs
              method zip ll =
                List.map (fun (h,e) -> h^e) ll
              method zip2 ll =
                List.map (fun (h,e) -> h^" "^(string_of_int e)^"\n") ll
              method insert_dummy =
                let help = function
                  | (l,s)::t -> ("s_dummy: \n"^l, s)::t
                  | _ -> failwith "empty rodata list" in
                rodata_list <- help rodata_list
              method insert_head =
                rodata_list <- (".section .rodata\n", "")::rodata_list;
                got_list <- (".section .got\n", "")::got_list;
                data_list <- (".section .data\n", "")::data_list;
                bss_list <- (".section .bss\n", "")::bss_list
              method write_file =
                let oc = open_out_gen [Open_append; Open_creat] 0o666 "final_data.s" in
                List.iter (fun l -> Printf.fprintf oc "%s\n" l) (sp#zip rodata_list);
                (* List.iter (fun l -> Printf.fprintf oc "%s\n" l) (sp#zip *)
                (* got_list); *)
                List.iter (fun l -> Printf.fprintf oc "%s\n" l) (sp#zip data_list);
                List.iter (fun l -> Printf.fprintf oc "%s\n" l) (sp#zip bss_list);
                close_out oc
              method de_redunt labels =
                let tset = Hashtbl.create 200 in
                let help item =
                  let (n, l) = item in
                  if Hashtbl.mem tset l then
                    ()
                  else
                    Hashtbl.replace tset l item;
                  () in
                List.iter help labels;
                let help' key value ll =
                  value::ll in
                Hashtbl.fold help' tset []
            end in
    (* process text and process data are two different approaches
     * process text resolve all the references from text section,
     * process data resolve interleave reference among rodata and data sections
     * current we just leverage a heristic methods, which consider all the value
     * inside .data and .rodata sections as addr *)
    (* print_int (List.length bss_list); *)
    (* let temp = p#zip2 locations in *)
    (* List.iter print_string temp; *)
    (* print_int (List.length bss_list); *)
    rodata_array <- Array.of_list rodata_list;
    got_array <- Array.of_list got_list;
    data_array <- Array.of_list data_list;
    bss_array <- Array.of_list bss_list;
    (* data_labels <- p#de_redunt data_labels; *)
    p#process locations;
    p#process data_labels;
    rodata_list <- Array.to_list rodata_array;
    got_list <- Array.to_list got_array;
    data_list <- Array.to_list data_array;
    bss_list <- Array.to_list bss_array;
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
  method collect name =
    if Sys.file_exists(name) then begin
        let filelines = File.lines_of name
        and p = object
            val mutable c : string = ""
            method process line =
              let line' = String.trim line in
              let rev l =
                let s1 = (Char.escaped(l.[0])^Char.escaped(l.[1]))
                and s2 = (Char.escaped(l.[2])^Char.escaped(l.[3]))
                and s3 = (Char.escaped(l.[4])^Char.escaped(l.[5]))
                and s4 = (Char.escaped(l.[6])^Char.escaped(l.[7])) in
                s4^s3^s2^s1 in
              let items = Str.split (Str.regexp " +") line' in
              c <-
                c^(List.fold_left (fun acc item -> acc^(item))
                                  "" items)
            method get_c = c
          end  in
        Enum.iter p#process filelines;
        p#get_c
      end
    else ""

  (* this is an optimizated solution for large size .bss section ELF binary *)
  method collect_bss name =
    if Sys.file_exists(name) then begin
        let filelines = File.lines_of name
        and p = object
            val mutable c : string list = []
            method process line =
              let line' = String.trim line in
              c <- line'::c;
            method get_c = c
          end
        in
        Enum.iter p#process filelines;
        p#get_c
      end
    else [""]

  method print_list ls =
    List.iter (fun l -> print_string ((dec_hex l)^"\n")) ls; 
    ls

  method sec_dump il =
    let s = String.concat "\n" (".section .trampoline , \"ax\"\n.align 16\n"::il) in
    let oc = open_out_gen [Open_append; Open_creat] 0o666 "final_trampoline.s" in
    Printf.fprintf oc "%s\n" s


  (* as we have collected all the references from data sections to code
  section in text_labels list, let's translate each label into a corresponding
  trampoline function!

    As typically each jmp S_label instruction takes 5 bytes, so it might be
   possible that the next label is inside the jmp instruction just inserted,
   alert to me and we might have to insert it to the original text? Maybe,
   but it is easy to do.
   *)
  method trampoline_generate labels =
    let b = fst text_sec in
    let s = snd text_sec in
    let func_create d = "jmp S_"^(dec_hex d) in
    let nop_create = "nop" in
    let rec sec_create i acc ls =
      match (i,ls) with
      | (_, []) -> List.rev acc
      (* the last line of .text section should be b+s-1 *)
      | (_, _) when i = s -> failwith "unsupport label found in trampline generate"
      | (_, h::t) when h < b+i ->
         begin
           (* we find two references that the address between them is less than 5 *)
           let last = b+i-5 in
           print_string ("two references with small range : "^(dec_hex last)^" "^(dec_hex h)^"\n");
           sec_create i acc t
         end
      | (_, h::t) when h = b+i ->
         let fi = func_create h in
         sec_create (i+5) (fi::acc) t
      | (_, h::t) when h > b+i ->
         let ni = nop_create in
         sec_create (i+1) (ni::acc) ls
      | _ -> raise (Data_Process_Error "unexpected in trampoline generate") in
    ( List.sort Int.compare labels
      |> unify_int_list
     (* |> self#print_list *)
      |> sec_create 0 []
      |> self#sec_dump
    )


end
