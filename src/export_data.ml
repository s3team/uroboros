(* This function selects export data from export table and dumps their memory
   addresses *)
open Utils

let export () =
  let lines = read_file "export_tbl.info" in
  let datas =
    List.filter_map
      (fun l ->
        let items =
          String.split_on_char ' ' l |> List.filter (fun s -> s <> "")
        in
        if
          List.mem "OBJECT" items
          && not (List.mem "@" items)
        then
          match items with
          | _ :: hex_str :: _ ->
              let v = int_of_string ("0x" ^ hex_str) in
              if v <> 0 then Some ("0x" ^ hex_str ^ "\n") else None
          | _ -> None
        else None)
      lines
  in
  let oc = open_out "export_datas.info" in
  List.iter (output_string oc) datas;
  close_out oc
