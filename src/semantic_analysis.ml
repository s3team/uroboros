module type DfaAbs = sig
  open Type

  type abstraction

  val initial : abstraction
  val equal : abstraction -> abstraction -> bool
  val merge : instr option list -> (instr, abstraction) Hashtbl.t -> abstraction
  val flow_through : instr -> abstraction -> abstraction
  val process_instr : instr -> abstraction -> unit
  val print_abstraction : abstraction -> unit

  val result :
    (int, instr list) Hashtbl.t (* term rewriting based on DFA results *)
end

module DFA (A : DfaAbs) = struct
  open Pp_print
  open Ail_utils
  open Utils
  open Type

  let in_map = Hashtbl.create 10
  let out_map = Hashtbl.create 10

  let flow_analysis (f : string) (cfg : cfgi) : unit =
    (* init each CFG node *)
    let { preds; succs; il } = cfg in
    let _ =
      List.iter
        (fun i ->
          Hashtbl.add in_map i A.initial;
          Hashtbl.add out_map i A.initial)
        il
    in
    (* init worklist *)
    let worklist = Queue.create () in
    let _ = List.iter (fun i -> Queue.push i worklist) il in
    (* iterate until fixpoint *)
    while not (Queue.is_empty worklist) do
      let i : instr = Queue.pop worklist in
      let preds : instr option list =
        Hashtbl.find_opt preds (Some i) |> Option.value ~default:[]
      in
      (* With [preds], get all outs from [out_map] and merge them *)
      let ins = A.merge preds out_map in
      let _ = Hashtbl.replace in_map i ins in

      A.process_instr i ins;

      let old_outs = Hashtbl.find out_map i in
      let outs = A.flow_through i ins in

      (* debug log *)
      (* print_endline ("> " ^ (pp_print_instr' i));
         print_endline " outs:";
         A.print_abstraction outs;
         print_endline " ins:";
         A.print_abstraction ins; *)
      if not (A.equal old_outs outs) then (
        Hashtbl.replace out_map i outs;
        let succs =
          match Hashtbl.find_opt succs (Some i) with Some v -> v | None -> []
        in
        (* debug log on successor instructions *)
        (* print_endline
             (" succs: ["
             ^ String.concat ", "
                 (List.map
                     (fun i_op ->
                       match i_op with
                       | Some i -> pp_print_instr' i
                       | None -> "None")
                     succs)
             ^ "]"); *)
        List.iter
          (fun i_op ->
            match i_op with Some i -> Queue.push i worklist | None -> ())
          succs)
    done
end
