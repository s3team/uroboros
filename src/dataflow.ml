open Bitstring

open Type
open Pp_print

open Ail_utils



type df_direction = Forward | Backward


class dataFlow (instr_list : instr list) (dir : df_direction) (cfg_t) =

        object (self)


          (* Accessors to query about the solution.*)


          (* the in-set is only available for forward problems *)
          method getInSet bb =
            assert(direction = Forward);
            df_solution.find(bb).second

          (* the out-set is only available for backward problems *)
          method getOutSet bb =
            assert(direction = Backward);
            df_solution.find(bb).second

          (* Functions needs by the solver. Must be implemented by the actual problems *)
          method virtual createGenSet : bblock -> unit
          method virtual createKillSet : bblock -> unit

          (* get initial state for state for the nodes.*)
          method virtual getInitialEntryState : unit -> unit
          (* transfer inset -> gen -> kill -> BitString*)
          method transfer inset gen kill = gen || (inset - kill)

          (* confluence function.*)
          method virtual confluence : unit -> unit

          (* utility functions that can be used in problem instance to implement *)
          (* Confluence function*)
          method union dataset =
            let size = List.length dataset in
            assert(size > 0);
            let out_set = List.nth dataset 0 in
            let aux out_set data =
              out_set | data in
            List.fold_left out_set List.tl dataset

          method interSect dataset =
            let size = List.length dataset in
            assert(size > 0);
            let out_set = List.nth dataset 0 in
            let aux out_set data =
              out_set & data in
            List.fold_left out_set List.tl dataset


          (* the number of bits each bitstring has, should be set in the constructor*)
          val mutable num_bits = 0

          (* only true when the problem is solved *)
          val mutable solved = false

          (* list:  contains function with its cfg structure *)
          val mutable cfg_tbl = []

          (* for backwards problem, save the output sets.*)
          (* for forward problem, save the input sets.*)
          val mutable df_solution  = Hashtbl.create 1

          val mutable direction = Forward


          val mutable dirty = false;
          val mutable iter_nums = 0;

          (* max number of iterations to try when looking for fixpoint *)
          val maxIterations = 1000

          method DumpState unit -> unit

          (* this function take the CFG for a single function as input *)
          method solve cfg f =
            (*
      Terminology :
      in order to create names that work in both forward and backwards
      problem the names "entry" and "exit" are used insterad of "in" and "out"
      when describing the state. Forward forward problems, the in set is
      called the entry set, and for backward problems, the set is called the
       entry set.
             *)

            (* algorithm used to solve the DF problem :
             * for i <- 1 to N
             initialize node i
         while (sets are still changing)
           for i <- 1 to N
              recompute sets at node i
             *)

            dirty <- false;
            iter_nums <- 0;

            print_string "begin to solve "^f.func_name^"\n";

            let len = List.length cfg in

            let entry_map = Hashtbl.create (len+1) in
            let exit_map = Hashtbl.create (len+1) in

            let gen_map = Hashtbl.create (len+1) in
            let kill_map = Hashtbl.create (len+1) in

            let init_bb bb =
              let entry_init = self#getInitialEntryState in
              let gen_init = self#createGenSet bb in
              let kill_init = self#createKillSet bb in
              let exit_init = self#transfer entry_init gen_init kill_init in
              Hashtbl.replace entry_map bb entry_init;
              Hashtbl.replace gen_map bb gen_init;
              Hashtbl.replace kill_map bb kill_init;
              Hashtbl.replace exit_map bb eixt_init in
            List.iter init_bb cfg;

            assert(bit_nums > 0);

            dirty <- true;
            while (dirty = true && (iter_nums <= maxiterations)) do

              let traverse bb =
                let con_set =
                  match direction with
                  | Backward -> bb_successors cfg b
                  | Forward -> bb_predecessors cfg b in
                if List.length con_set > 0 then
                  begin
                    let entry_new : Bitstring = self#confluence con_set in
                    if entry_new <> Hashtbl.find bb then
                      begin
                        Hashtbl.replace entry_map bb entry_new;
                        dirty <- true;

                        let gen = Hashtbl.find gen_map bb in
                        let kill = Hashtbl.find kill_map bb in
                        let exit_new = self#transfer entry_new gen kill in
                        Hashtbl.replace exit_map bb exit_new
                      end
                  end in
              List.iter traverse (get_bbl cfg);

              iter_nums <- iter_nums + 1;
            done;

            print_string "num itertionas : "^(string_of_int iter_nums)^"\n";
            df_solution <- entry_map;
            solved <- true;
            solved


          method init =
            direction <- dir;
            cfg_tbl <- cfg_t;
            solved <- false;


          initializer
            self#init;


        end
