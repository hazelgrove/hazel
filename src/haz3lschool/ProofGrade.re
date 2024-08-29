open Haz3lcore;
open Util;

module F = (ExerciseEnv: Exercise.ExerciseEnv) => {
  open Exercise.F(ExerciseEnv);
  open ProofCore;
  open Derivation;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type percentage = float;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type points = float;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type score = (points, points);

  let score_of_percent = (percent, max_points) => {
    let max_points = float_of_int(max_points);
    (percent *. max_points, max_points);
  };

  module ExternalError = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | NoRule
      | NoAbbr
      | PremiseNotReady
      | NotAJudgment
      | EvalOff
      | EvalFail
      | EvalPending
      | EvalIndet
      | Stepper
      | NoElab;

    let repr =
      fun
      | NoRule => "Rule not specified"
      | NoAbbr => "Abbreviation not specified"
      | PremiseNotReady => "Premise(s) not ready"
      | NotAJudgment => "Conclusion not a judgement"
      | _ as a => show(a);
  };

  module ProofTree = {
    open ExternalError;

    type t = list(Tree.p(abbr(res)))
    and res = result(deduction(Judgement.t), ExternalError.t);

    let res_of_di = ({result, _}: DynamicsItem.t, rule): res =>
      switch (result) {
      | Evaluation({evaluation, _}) =>
        switch (evaluation) {
        | ResultOk({result, _}) =>
          switch (result) {
          | BoxedValue({term: Prop(Judgement(jdmt)), _}) =>
            Ok({jdmt, rule})
          // TODO(zhiyao): check it
          | BoxedValue(_) => Error(NotAJudgment)
          | Indet(_) => Error(EvalIndet)
          }
        | Off(_) => Error(EvalOff)
        | ResultFail(_) => Error(EvalFail)
        | ResultPending => Error(EvalPending)
        }
      | Stepper(_) => Error(Stepper)
      | NoElab => Error(NoElab)
      };

    let mk =
        (eds: model(Editor.t), stitch_dynamic: stitched(DynamicsItem.t)): t => {
      List.map2(Tree.combine, stitch_dynamic.trees, eds.trees)
      |> List.map(
           Tree.map(
             fun
             | (Some(di), Abbr.Just({rule, _})) =>
               Abbr.Just(res_of_di(di, rule))
             | (None, Abbr(i)) => Abbr(i)
             | _ => failwith("DerivationTree.mk: ed<>di inconsistent"),
           ),
         );
    };
  };
  module VerifiedTree = {
    type t = list(Tree.p(res))
    and res =
      | Correct
      | Incorrect(DerivationError.t)
      | Pending(ExternalError.t);

    let show_res: res => string =
      fun
      | Correct => "✅"
      | Pending(err) => "⌛️ " ++ ExternalError.show(err)
      | Incorrect(err) => "❌ " ++ DerivationError.repr(err);

    let verify_single =
        (
          acc: list((tree(res), option(Judgement.t))),
          concl: abbr(ProofTree.res),
          prems: list((tree(res), option(Judgement.t))),
        ) => {
      let (sub_trees, prems) = List.split(prems);
      let are_prems_ready = List.for_all(Option.is_some, prems);
      let res =
        switch (concl) {
        | Abbr(Some(i)) => List.nth(acc, i) |> fst |> Tree.value
        | Abbr(None) => Pending(NoAbbr)
        | Just(Error(exn)) => Pending(exn)
        | Just(Ok({rule: None, _})) => Pending(NoRule)
        | Just(Ok(_)) when !are_prems_ready => Pending(PremiseNotReady)
        | Just(Ok({jdmt: concl, rule: Some(rule)})) =>
          let prems = prems |> List.map(Option.get);
          switch (DerivationError.verify(rule, concl, prems)) {
          | Some(err) => Incorrect(err)
          | None => Correct
          };
        };
      let concl =
        switch (concl) {
        | Abbr(Some(i)) => List.nth(acc, i) |> snd
        | Just(Ok({jdmt, _})) => Some(jdmt)
        | _ => None
        };
      (Tree.Node(res, sub_trees), concl);
    };

    let verify =
      List.fold_left(
        (acc, tree) => acc @ [Tree.fold_deep(verify_single(acc), tree)],
        [],
      );

    let verify = ts => ts |> verify |> List.map(fst);
  };

  module ProofReport = {
    type t = {
      verified_tree: VerifiedTree.t,
      grade: percentage,
    };
    // strip the abbreviation in the tree
    // require:
    //   - all the abbreviation can be resolved
    //   - the abbreviation is not cyclic (only refer to previous nodes)
    //   - the abbreviation node is leaf (otherwise, children will be lost)
    let strip_abbr: list(Tree.p(abbr('a))) => list(Tree.p('a)) =
      List.fold_left(
        (acc: list(Tree.p('a)), tree: Tree.p(abbr('a))) =>
          acc
          @ [
            Tree.fold_deep(
              (value: abbr('a), children: list(Tree.p('a))) =>
                switch (value) {
                | Just(v) => Tree.Node(v, children)
                | Abbr(None) => Tree.Node(VerifiedTree.Pending(NoAbbr), [])
                | Abbr(Some(i)) => List.nth(acc, i)
                },
              tree,
            ),
          ],
        [],
      );

    let grade_tree: Tree.p(VerifiedTree.res) => percentage =
      Tree.fold_deep((value: VerifiedTree.res, children: list(percentage)) =>
        switch (value, children) {
        | (Correct, []) => 1.
        | (Correct, _) =>
          List.fold_left((acc, x) => acc +. x, 0., children)
          /. float_of_int(List.length(children))
          *. 0.5
          +. 0.5
        | _ => 0.
        }
      );

    let mk =
        (eds: model(Editor.t), stitch_dynamic: stitched(DynamicsItem.t)): t => {
      let proof_tree = ProofTree.mk(eds, stitch_dynamic);
      let verified_tree = VerifiedTree.verify(proof_tree);
      let combined_tree =
        List.map2(Tree.combine, proof_tree, verified_tree)
        |> List.map(
             Tree.map(
               fun
               | (Abbr.Just(_), b) => Abbr.Just(b)
               | (Abbr(i), _) => Abbr(i),
             ),
           );
      let stripped_trees = strip_abbr(combined_tree);
      let grade =
        switch (stripped_trees) {
        | [] => 100.
        | _ => stripped_trees |> List.rev |> List.hd |> grade_tree
        };
      {verified_tree, grade};
    };
  };

  module GradingReport = {
    type t = {proof_report: ProofReport.t};

    let mk = (eds: 'a, ~stitched_dynamics: stitched(DynamicsItem.t)): t => {
      proof_report: ProofReport.mk(eds, stitched_dynamics),
    };

    let overall_score: t => score =
      report => {
        let grade = report.proof_report.grade;
        let max_points = 100;
        score_of_percent(grade, max_points);
      };
  };
};
