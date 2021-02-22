open Lang
open Nondet.Syntax

(* NOTE: [refine] only works if all constraints agree, so excess constraints
   can hamper it. In Myth, this means that [branch] should take over and
   distribute the constraints which don't agree over different branches. In
   Smyth, however, we might have branched by hand, without removing the
   irrelevant constraints. *)
let refine_or_branch params delta sigma hf (hole_name, synthesis_goal) =
  let* additional_depth, ((exp, subgoals), choice_constraints) =
    (* Note: Try to branch FIRST! This results in more idiomatic solutions. *)
    Nondet.union
      [ ( if params.max_match_depth > 0 then
          Nondet.map (Pair2.pair 1)
          @@ Branch.branch params.max_scrutinee_size delta sigma hf
               synthesis_goal
        else Nondet.none )
      ; Nondet.map (fun x -> (0, (x, Constraints.empty)))
        @@ Nondet.lift_option
        @@ Refine.refine delta sigma synthesis_goal ]
  in
  let* _, parent_depth =
    Nondet.lift_option @@ List.assoc_opt hole_name delta
  in
  let match_depth = parent_depth + additional_depth in
  let delta' =
    List.map
      (Pair2.map_snd @@ fun (gen_goal, _) -> (gen_goal, match_depth))
      subgoals
  in
  let solved_constraints = Hole_map.singleton hole_name exp in
  let unsolved_constraints =
    subgoals
    |> List.map (fun (hole_name, (_, worlds)) ->
           Hole_map.singleton hole_name worlds)
    |> Constraints.merge_unsolved
  in
  let+ final_constraints =
    Nondet.lift_option
    @@ Constraints.merge
         [(solved_constraints, unsolved_constraints); choice_constraints]
  in
  (final_constraints, delta')

let guess_and_check params delta sigma hf
    (hole_name, (({goal_type; _} as gen_goal), worlds)) =
  (* Only guess if we have not exhausted all time allotted for guessing *)
  let* _ = Nondet.guard (Timer.Multi.check Timer.Multi.Guess) in
  (* Only guess at base types *)
  let* _ = Nondet.guard (Type.is_base goal_type) in
  let* exp =
    Timer.Multi.accumulate Timer.Multi.Guess
    @@ fun () -> Term_gen.up_to sigma params.max_term_size gen_goal
    (* TODO: remove term_kind=E? *)
  in
  (* print_endline ("guess: " ^ Pretty.exp exp) ; *)
  let binding = Hole_map.singleton hole_name exp in
  let* extended_hf =
    Nondet.lift_option @@ Constraints.merge_solved [binding; hf]
  in
  let* uneval_constraints =
    Uneval.check delta sigma extended_hf exp worlds
  in
  let+ merged_constraints =
    Nondet.lift_option
    @@ Constraints.merge
         [Constraints.from_hole_filling binding; uneval_constraints]
  in
  (merged_constraints, [])

let defer _params _delta _sigma _hf (hole_name, ({goal_type; _}, worlds)) =
  if
    (not (Type.equal goal_type (TTuple [])))
    && List.length worlds > 0
    && List.for_all (fun (_, ex) -> ex = ExTop) worlds
  then
    Nondet.pure (Constraints.solved_singleton hole_name (EHole hole_name), [])
  else Nondet.none

let fill params delta sigma hf fill_goal =
  Nondet.union
  @@ List.map
       (fun rule -> rule params delta sigma hf fill_goal)
       [refine_or_branch; guess_and_check; defer]
