open Utils

let create (section_name : string) : unit =
  let lines = read_file "sections.info" in
  let size_search =
    List.filter_map
      (fun l ->
        if contains ~str:l ~sub:section_name then
          let items =
            String.split_on_char ' ' l |> List.filter (fun s -> s <> "")
          in
          match items with
          | _ :: _ :: _ :: sz :: _ -> Some sz
          | _ -> None
        else None)
      lines
  in
  let out_file = section_name ^ ".info" in
  let out_file =
    if String.length out_file > 0 && out_file.[0] = '.' then
      String.sub out_file 1 (String.length out_file - 1)
    else out_file
  in
  match size_search with
  | [] ->
      let oc = open_out out_file in
      close_out oc
  | _ ->
      let sz = last_ele size_search in
      let size = int_of_string ("0x" ^ sz) in
      let strs = String.concat "" (List.init size (fun _ -> ".byte 0x00\n")) in
      let oc = open_out out_file in
      output_string oc strs;
      close_out oc
