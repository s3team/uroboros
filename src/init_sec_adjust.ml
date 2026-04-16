open Utils

let adjust () =
  match read_file "init_sec.info" with
  | [] -> print_endline "init_sec.info is empty, exiting."
  | l :: _ when not (contains ~str:l ~sub:"]") -> ()
  | _ ->
      let bin =
        read_file "elf.info" |> List.hd |> String.split_on_char ':' |> List.hd
        |> String.trim
      in
      ignore (Sys.command ("readelf -SW " ^ bin ^ " > temp"));
      read_file "temp"
      |> List.iter (fun l ->
          let items = String.split_on_char ' ' l |> List.filter (( <> ) "") in
          (* [ 9] .init PROGBITS 00000000000006e8 0006e8 000018 *)
          match items with
          | _ :: _ :: ".init" :: _ :: a :: o :: s :: _ ->
              let oc = open_out "init_sec.info" in
              Printf.fprintf oc ".init %s %s %s\n" a o s;
              close_out oc
          | _ -> ())
