open Lang
open Nondet.Syntax

(* Constraint simplification *)

let rec simplify_constraints (delta : hole_ctx) (sigma : datatype_ctx)
    ((f_prev, u_prev) as k_prev : constraints) : constraints Nondet.t =
  let* k_new =
    u_prev |> Hole_map.bindings
    |> List.map (fun (hole_name, worlds) ->
           match Hole_map.find_opt hole_name f_prev with
           | Some exp -> Uneval.check delta sigma f_prev exp worlds
           | None ->
               Nondet.pure @@ Constraints.unsolved_singleton hole_name worlds)
    |> Nondet.one_of_each
    |> Nondet.map Constraints.merge
    |> Nondet.collapse_option
    |> Nondet.map (Pair2.map_snd @@ Hole_map.map @@ List.sort_uniq compare)
  in
  let* k_merged =
    Nondet.lift_option
    @@ Constraints.merge [Constraints.from_hole_filling f_prev; k_new]
  in
  if k_merged = k_prev then Nondet.pure k_merged
  else simplify_constraints delta sigma k_merged

(* Core algorithm *)

let current_solution_count = ref 0

let should_continue () =
  match !Params.max_solution_count with
  | Some n -> !current_solution_count < n
  | None -> true

let rec iter_solve params delta sigma (hf, us_all) =
  let* _ = Nondet.guard @@ should_continue () in
  match Constraints.delete_min us_all with
  | None ->
      current_solution_count := !current_solution_count + 1 ;
      Nondet.pure (hf, delta)
  | Some ((hole_name, worlds), us) ->
      let* gamma, typ, dec, match_depth =
        Nondet.lift_option @@ List.assoc_opt hole_name delta
      in
      let* k_new, delta_new =
        Fill.fill
          {params with max_match_depth= params.max_match_depth - match_depth}
          delta sigma hf
          (hole_name, ((gamma, typ, dec), worlds))
      in
      let delta_merged = delta_new @ delta in
      let* k_merged =
        Constraints.merge [(hf, us); k_new]
        |> Nondet.lift_option
        |> Nondet.and_then (simplify_constraints delta_merged sigma)
      in
      iter_solve params delta_merged sigma k_merged

let iter_solve_once (hole_name, worlds) params delta sigma (hf, us_all) =
  let* _ = Nondet.guard @@ should_continue () in
  let us = Constraints.delete hole_name us_all in
  let* gamma, typ, dec, match_depth =
    Nondet.lift_option @@ List.assoc_opt hole_name delta
  in
  let* k_new, delta_new =
    Fill.fill
      {params with max_match_depth= params.max_match_depth - match_depth}
      delta sigma hf
      (hole_name, ((gamma, typ, dec), worlds))
  in
  let delta_merged = delta_new @ delta in
  let* hf', _ =
    Constraints.merge [(hf, us); k_new]
    |> Nondet.lift_option
    |> Nondet.and_then (simplify_constraints delta_merged sigma)
  in
  current_solution_count := !current_solution_count + 1 ;
  Nondet.pure (hf', delta_merged, us_all)

(* added us *)

(* Staging *)

type stage =
  | One
  | PreTwo
  | Two
  | PreThree
  | Three
  | PreFour
  | Four
  | PreFive
  | Five

let all_stages : stage list =
  [One; PreTwo; Two; PreThree; Three; PreFour; Four; PreFive; Five]

let expand_stages (xs : 'a list) : (stage * 'a) list =
  List2.concat_map (fun s -> List.map (fun x -> (s, x)) xs) all_stages

let solve_any delta sigma constraints_nd =
  let rec helper problems =
    match problems with
    | [] -> Nondet.none
    | (stage, constraints) :: rest_problems ->
        let max_scrutinee_size, max_match_depth, max_term_size =
          match stage with
          | One -> (1, 0, 13)
          | PreTwo -> (1, 1, 8)
          | Two -> (1, 1, 13)
          | PreThree -> (1, 2, 8)
          | Three -> (1, 2, 13)
          | PreFour -> (6, 2, 8)
          | Four -> (6, 2, 13)
          | PreFive -> (6, 3, 8)
          | Five -> (6, 3, 13)
        in
        let params = {max_scrutinee_size; max_match_depth; max_term_size} in
        current_solution_count := 0 ;
        Timer.Multi.reset Timer.Multi.Guess ;
        let solution_nd = iter_solve params delta sigma constraints in
        if Nondet.is_empty solution_nd then helper rest_problems
        else solution_nd
  in
  constraints_nd |> Nondet.to_list |> expand_stages |> helper

let solve_once hole_name delta sigma constraints_nd =
  let rec helper problems =
    match problems with
    | [] -> Nondet.none
    | (stage, constraints) :: rest_problems ->
        let max_scrutinee_size, max_match_depth, max_term_size =
          match stage with
          | One -> (1, 0, 13)
          | PreTwo -> (1, 1, 8)
          | Two -> (1, 1, 13)
          | PreThree -> (1, 2, 8)
          | Three -> (1, 2, 13)
          | PreFour -> (6, 2, 8)
          | Four -> (6, 2, 13)
          | PreFive -> (6, 3, 8)
          | Five -> (6, 3, 13)
        in
        let params = {max_scrutinee_size; max_match_depth; max_term_size} in
        current_solution_count := 0 ;
        Timer.Multi.reset Timer.Multi.Guess ;
        (* TODO: figure out what goes in place of worlds (the empty list)
           below, in the call to iter_solve_once. *)
        let solution_nd =
          iter_solve_once (hole_name, []) params delta sigma constraints
        in
        if Nondet.is_empty solution_nd then helper rest_problems
        else solution_nd
  in
  constraints_nd |> Nondet.to_list |> expand_stages |> helper
