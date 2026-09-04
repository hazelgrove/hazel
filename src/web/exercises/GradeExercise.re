open Haz3lcore;
open Language;

let settings = CoreSettings.on;

type report = {
  summary: string,
  overall: Grading.score,
};

let evaluate_term = (term: Exp.t): option(Exp.t) => {
  let evaluated =
    term
    |> CachedStatics.init_from_term(~settings, ~is_dynamic_term=false)
    |> ((x: CachedStatics.t) => x.elaborated)
    |> Evaluator.evaluate_and_limit(
         ~step_limit=1000000,
         ~env=Builtins.env_init,
       );
  switch (evaluated) {
  | StepLimitExceeded => None
  | LimitedCompleted((result, _)) => Some(result)
  };
};

/* Count (correct, total) nodes across every tree in the verified proof.
   Total includes every node; correct counts only nodes with res = Correct. */
let count_verified_tree = (verified: DrvGrading.VerifiedTree.t): (int, int) => {
  let all_nodes = verified |> List.concat_map(Util_web.Tree.flatten);
  List.fold_left(
    ((c, t), info: DrvGrading.VerifiedTree.info) =>
      switch (info.res) {
      | Correct => (c + 1, t + 1)
      | _ => (c, t + 1)
      },
    (0, 0),
    all_nodes,
  );
};

/* All-or-nothing scoring: full credit only if the derivation has at least
   one node and every node is Correct; otherwise 0. */
let score_of_verified_tree =
    (spec: DerivationExercise.spec, verified: DrvGrading.VerifiedTree.t)
    : Grading.score => {
  let (correct, total) = count_verified_tree(verified);
  let max = float_of_int(spec.max_points);
  let all_correct = total > 0 && correct == total;
  (all_correct ? max : 0., max);
};

let grade_derivation =
    (
      spec: DerivationExercise.spec,
      persistent_state: DerivationExercise.persistent_state,
    )
    : report => {
  let zipper_spec: DerivationExercise.spec =
    DerivationExercise.mapi(persistent_state, pos =>
      PersistentZipper.unpersist(~root=DerivationExercise.root_of_pos(pos))
    );
  let editors: DerivationExercise.eds =
    DerivationExercise.mapi(zipper_spec, pos =>
      Editor.Model.mk(~root=DerivationExercise.root_of_pos(pos))
    );
  let stitched = DerivationExercise.stitch_term(editors);
  let stitched_results: DerivationExercise.stitched(option(Exp.t)) =
    DerivationExercise.map_stitched(
      (_, term_item: DerivationExercise.TermItem.t) =>
        evaluate_term(term_item.term),
      stitched,
    );
  let verified = DrvGrading.VerifiedTree.mk(editors, ~stitched_results);
  let (correct, total) = count_verified_tree(verified);
  let (earned, max) = score_of_verified_tree(spec, verified);
  let summary =
    Printf.sprintf(
      "Derivation: %.1f/%.1f (%d/%d nodes correct)\n",
      earned,
      max,
      correct,
      total,
    );
  {
    summary,
    overall: (earned, max),
  };
};

let grade_theorem = (spec: TheoremExercise.spec, _persistent_state): report => {
  let max = float_of_int(spec.max_points);
  {
    summary:
      Printf.sprintf(
        "Theorem: manual grading required (%.0f points possible)\n",
        max,
      ),
    overall: (0., max),
  };
};
