open Utils

let arch : string ref = ref ""
let bit_mode : string ref = ref ""

let init_config (a : string) (b : string) =
  arch := a;
  bit_mode := b

let get_arch () =
  assert (!arch <> "");
  !arch

let get_bit_mode () =
  assert (!bit_mode <> "");
  !bit_mode

let elf_32 () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:"ELF 32-bit"

let elf_64 () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:"ELF 64-bit"

let elf_dynamic () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:"dynamically linked"

let elf_static () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:"statically linked"

let is_vxworks_rtp () = Sys.file_exists "vxworks_rtp.info"

let elf_unstrip () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:"not stripped"

let elf_strip () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:", stripped"

let elf_exe () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:"LSB executable"

let elf_lib () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:"LSB shared object"

let elf_reloc () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:"relocatable"

let elf_arm () =
  (read_file "elf.info" |> fun l -> List.nth l 0) |> fun l ->
  contains ~str:l ~sub:"ARM"

let elf_intel () = not (elf_arm ())

let global_bss () =
  let filelines = read_file "globalbss.info"
  and help acc l =
    let items = Str.split (Str.regexp " +") l in
    let t = List.nth items 0 in
    let addr = String.sub t 1 (String.length t - 1) in
    let addr' = String.uppercase_ascii addr
    and value = String.trim (List.nth items 2) in
    let rtype = String.trim (List.nth items 1) in
    (addr', value, rtype) :: acc
  in
  List.fold_left help [] filelines
  |> List.filter (fun (a, n, t) -> contains ~str:n ~sub:"@")
