open Lang;
open Nondet.Syntax;

let refine_or_branch =
    (params, delta, sigma, hf, (hole_name, synthesis_goal)) => {
  let* (additional_depth, ((exp, subgoals), choice_constraints)) =
    /* Note: Try to branch FIRST! This results in more idiomatic solutions. */
    Nondet.union([
      if (params.max_match_depth > 0) {
        Nondet.map(Pair2.pair(1)) @@
        Branch.branch(
          params.max_scrutinee_size,
          delta,
          sigma,
          hf,
          synthesis_goal,
        );
      } else {
        Nondet.none;
      },
      Nondet.map(x => (0, (x, Constraints.empty))) @@
      Nondet.lift_option @@
      Refine.refine(delta, sigma, synthesis_goal),
    ]);
  let* (_, _, _, parent_depth) =
    Nondet.lift_option @@ List.assoc_opt(hole_name, delta);
  let match_depth = parent_depth + additional_depth;

  let delta' =
    List.map(
      Pair2.map_snd @@
      (
        (((gamma, goal_type, goal_dec), _)) => (
          gamma,
          goal_type,
          goal_dec,
          match_depth,
        )
      ),
      subgoals,
    );

  let solved_constraints = Hole_map.singleton(hole_name, exp);

  let unsolved_constraints =
    subgoals
    |> List.map(((hole_name, (_, worlds))) =>
         Hole_map.singleton(hole_name, worlds)
       )
    |> Constraints.merge_unsolved;

  let+ final_constraints =
    Nondet.lift_option @@
    Constraints.merge([
      (solved_constraints, unsolved_constraints),
      choice_constraints,
    ]);
  (final_constraints, delta');
};

let guess_and_check =
    (
      params,
      delta,
      sigma,
      hf,
      (hole_name, ((_, goal_type, _) as gen_goal, worlds)),
    ) => {
  /* Only guess if we have not exhausted all time allotted for guessing */
  let* _ = Nondet.guard(Timer.Multi.check(Timer.Multi.Guess));
  /* Only guess at base types */
  let* _ = Nondet.guard(Type.is_base(goal_type));
  let* exp =
    Timer.Multi.accumulate(Timer.Multi.Guess) @@
    (() => Term_gen.up_to_e(sigma, params.max_term_size, gen_goal));
  let binding = Hole_map.singleton(hole_name, exp);

  let* extended_hf =
    Nondet.lift_option @@ Constraints.merge_solved([binding, hf]);
  let* uneval_constraints =
    Uneval.check(delta, sigma, extended_hf, exp, worlds);
  let+ merged_constraints =
    Nondet.lift_option @@
    Constraints.merge([
      Constraints.from_hole_filling(binding),
      uneval_constraints,
    ]);
  (merged_constraints, []);
};

let defer =
    (
      _params,
      _delta,
      _sigma,
      _hf,
      (hole_name, ((_, goal_type, _), worlds)),
    ) =>
  if (!Type.equal(goal_type, TTuple([]))
      && List.length(worlds) > 0
      && List.for_all(((_, ex)) => ex == ExTop, worlds)) {
    Nondet.pure((
      Constraints.solved_singleton(hole_name, EHole(hole_name)),
      [],
    ));
  } else {
    Nondet.none;
  };

let fill = (params, delta, sigma, hf, fill_goal) =>
  Nondet.union @@
  List.map(
    rule => rule(params, delta, sigma, hf, fill_goal),
    [refine_or_branch, guess_and_check, defer],
  );
