module IntSet = Set.Make (struct
  let compare = compare

  type t = int
end)

module StringSet = Set.Make (String)

module Time_Record = struct
  (* this module wrapper the UNIX time expansion facilities of OCaml *)
  let stamp () =
    Printf.printf "stamp : %f sec\n" (Unix.gettimeofday ());
    ()

  let get_utime () = Unix.gettimeofday ()

  let elapsed' ?(l = "") t =
    let end_time = get_utime () in
    let elapsed_time = end_time -. t in
    let minutes = int_of_float (elapsed_time /. 60.0) in
    let seconds = elapsed_time -. (float_of_int minutes *. 60.0) in
    Printf.printf "elapsed time (%s): %dm %.2fs\n" l minutes seconds

  let elapsed t =
    let t1 = Unix.gettimeofday () in
    Printf.printf "execution elapsed time: %f sec\n" (t1 -. t);
    t1
end

let read_file (filename : string) : string list =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    []
  with End_of_file ->
    close_in chan;
    List.rev !lines

let dec_hex (s : int) : string = Printf.sprintf "0x%X" s
let dec_hex_sym (s : int) : string = Printf.sprintf "S_0x%X" s
let dec_hex_col (s : int) : string = Printf.sprintf "%x:" s
let dec_hex_col_sym (s : int) : string = Printf.sprintf "S_0x%X:\n" s
let ( let* ) = Option.bind
let ( let+ ) x f = Option.map f x

let ( and* ) a b =
  let* a = a in
  let* b = b in
  Some (a, b)

let ( and+ ) a b = match (a, b) with Some a, Some b -> Some (a, b) | _ -> None
let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let flip (f : 'a -> 'b -> 'c) (x : 'b) (y : 'a) : 'c = f y x

let print_addrlist (ll : int list) : unit =
  List.iter (fun l -> print_string (dec_hex l ^ "\n")) ll

let unify_int_list (l : int list) : int list =
  let s = List.fold_left (fun s ele -> IntSet.add ele s) IntSet.empty l in
  (* remove dups *)
  (* elements should be returned in increasing order *)
  IntSet.elements s

let unify_str_list (l : string list) : string list =
  let s = List.fold_left (fun s ele -> StringSet.add ele s) StringSet.empty l in
  (* elements should be returned in increasing order *)
  StringSet.elements s

let unify_hash_list (h : (string, 'a) Hashtbl.t) : (string, 'a) Hashtbl.t =
  let l = Hashtbl.length h in
  let h' = Hashtbl.create (l + 1)
  and se = Hashtbl.fold (fun k v s -> StringSet.add k s) h StringSet.empty in
  let help n =
    let f = Hashtbl.find h n in
    Hashtbl.add h' n f
  in
  List.iter help (StringSet.elements se);
  h'

let split_by_list (s : string) (pl : int list) =
  let rec help s last_p pl' acc =
    match pl' with
    | hp :: tp ->
        let s' = String.sub s (last_p + 1) (hp - last_p - 1) in
        help s hp tp (s' :: acc)
    | [] ->
        let ep = String.length s - 1 in
        List.rev_append acc [ String.sub s (last_p + 1) (ep - last_p - 1) ]
  in
  help s (-1) pl []

(* this function remove element from list if this element appears more than once *)
(*d = [x for x in d if d.count(x) == 1]*)
let remove_over_once (l : 'a list) : 'a list =
  let len = List.length l in
  let m = Hashtbl.create (len + 1) in
  List.iter
    (fun ele ->
      let c = Hashtbl.find_opt m ele |> Option.value ~default:0 in
      Hashtbl.replace m ele (c + 1))
    l;
  List.filter (fun ele -> Hashtbl.find m ele <= 1) l

let rec last_ele : 'a list -> 'a = function
  | [ h ] -> h
  | h :: t -> last_ele t
  | [] -> failwith "error in last_ele"

let contains ~(str : string) ~(sub : string) : bool =
  let str_len = String.length str in
  let sub_len = String.length sub in
  if sub_len = 0 then true
  else if sub_len > str_len then false
  else
    let last = str_len - sub_len in
    let rec match_from i j =
      if j = sub_len then true
      else if String.unsafe_get str (i + j) = String.unsafe_get sub j then
        match_from i (j + 1)
      else false
    in
    let rec check_at i =
      if i > last then false
      else if match_from i 0 then true
      else check_at (i + 1)
    in
    check_at 0

(* on 32bit system, OCaml's maxinum integer is 0x3fffffff *)
let string_to_int32 (s : string) : int option =
  try Some (int_of_string s) with _ -> None

let zip (lst1 : 'a list) (lst2 : 'b list) : ('a * 'b) list =
  let rec aux lst1 lst2 acc =
    match (lst1, lst2) with
    | [], _ -> List.rev acc
    | _, [] -> List.rev acc
    | x :: xs, y :: ys -> aux xs ys ((x, y) :: acc)
  in
  aux lst1 lst2 []

let rec cat_from (l : string list) (f : int) (s : string) : string =
  (* for the reason of high efficiency, we don't write it tail-recursively *)
  match l with
  | h :: t -> if f == 0 then h ^ s ^ cat_from t 0 s else cat_from t (f - 1) s
  | [] -> ""

let sort_uniq (cmp : 'a -> 'a -> int) (l : 'a list) : 'a list =
  let rec aux acc = function
    | h1 :: h2 :: t ->
        if cmp h1 h2 = 0 then aux acc (h2 :: t) else aux (h1 :: acc) (h2 :: t)
    | h :: [] -> h :: acc
    | [] -> acc
  in
  l |> List.sort cmp |> aux [] |> List.rev

let b_search (e : 'a) (ls : 'a array) (cmp : 'a -> 'a -> int) : bool =
  (* a binary search with configurable cmp *)
  let rec bs min max =
    if min > max then false
    else
      let mid = (min + max) / 2 in
      let mid_num = Array.get ls mid in
      let r = cmp e mid_num in
      if r = 0 then true
      else if r < 0 then bs min (mid - 1)
      else bs (mid + 1) max
  in
  bs 0 (Array.length ls - 1)

let timed l f =
  let module TR = Time_Record in
  let t = TR.get_utime () in
  let r = f () in
  TR.elapsed' ~l t;
  r

let glob_match (pat : string) (s : string) : bool =
  if not (String.contains pat '*') then String.equal pat s
  else begin
    let n = String.length s in
    let star_start = pat.[0] = '*' in
    let star_end = pat.[String.length pat - 1] = '*' in
    let segs = String.split_on_char '*' pat |> List.filter (fun x -> x <> "") in
    let find_from sub from =
      let sl = String.length sub in
      let rec loop i =
        if i + sl > n then -1
        else if String.sub s i sl = sub then i
        else loop (i + 1)
      in
      loop from
    in
    let rec go segs pos first =
      match segs with
      | [] -> true
      | seg :: rest ->
          let idx = find_from seg pos in
          if idx < 0 then false
          else if first && (not star_start) && idx <> 0 then false
          else (
            match rest with
            | [] -> if not star_end then idx + String.length seg = n else true
            | _ -> go rest (idx + String.length seg) false)
    in
    if segs = [] then true (* all-'*' pattern matches anything *)
    else go segs 0 true
  end

