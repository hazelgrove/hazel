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
    type t =
      | PremiseError
      | NotAJudgment
      | EvalOff
      | EvalFail
      | EvalPending
      | EvalIndet
      | Stepper
      | NoElab;
  };

  module ProofTree = {
    open ExternalError;

    type t = list(tree(res))
    and res = result(deduction(Judgement.t), ExternalError.t);

    let res_of_di = ({result, _}: DynamicsItem.t, rule: Rule.t): res =>
      switch (result) {
      | Evaluation({evaluation, _}) =>
        switch (evaluation) {
        | ResultOk({result, _}) =>
          switch (result) {
          | BoxedValue({term: Judgement(jdmt), _}) => Ok({jdmt, rule})
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
             | (Some(di), Just({rule, _})) => Just(res_of_di(di, rule))
             | (None, Abbr(i)) => Abbr(i)
             | _ => failwith("DerivationTree.mk: ed<>di inconsistent"),
           ),
         );
    };
  };
  module VerifiedTree = {
    type t = list(Tree.p(res))
    and res = result(verified, ExternalError.t)
    and verified = {
      concl: Judgement.t,
      err: option(DerivationError.t),
    };

    let strip_prems =
        (prems: list(Tree.p(res))): option(list(Judgement.t)) => {
      let prems' =
        prems
        |> List.map(Tree.value)
        |> List.filter_map(
             fun
             | Ok(v) => Some(v.concl)
             | _ => None,
           );
      List.length(prems') == List.length(prems) ? Some(prems') : None;
    };

    let verify_single =
        (acc: t, concl: abbr(ProofTree.res), prems: list(Tree.p(res))) => {
      let concl =
        switch (concl) {
        // TODO: Implement this
        // assert(List.length(prems) == 0);
        | Abbr(i) => List.nth(acc, i) |> Tree.value
        | Just(Error(exn)) => Error(exn)
        | Just(Ok({jdmt: concl, rule})) =>
          let err =
            switch (strip_prems(prems)) {
            | Some(prems) => DerivationError.verify(rule, concl, prems)
            | None => Some(External("PremiseError"))
            };
          Ok({concl, err});
        };
      Tree.Node(concl, prems);
    };

    let verify: ProofTree.t => t =
      List.fold_left(
        (acc: t, tree: tree(ProofTree.res)) =>
          acc @ [Tree.fold_deep(verify_single(acc), tree)],
        [],
      );
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
    let strip_abbr: list(tree('a)) => list(Tree.p('a)) =
      List.fold_left(
        (acc: list(Tree.p('a)), tree: tree('a)) =>
          acc
          @ [
            Tree.fold_deep(
              (value: abbr('a), children: list(Tree.p('a))) =>
                switch (value) {
                | Just(v) => Tree.Node(v, children)
                | Abbr(i) => List.nth(acc, i)
                },
              tree,
            ),
          ],
        [],
      );

    let grade_tree: Tree.p(VerifiedTree.res) => percentage =
      Tree.fold_deep((value: VerifiedTree.res, children: list(percentage)) =>
        switch (value, children) {
        | (Ok({err: None, _}), []) => 1.
        | (Ok({err: None, _}), _) =>
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
               | (Just(_), b) => Just(b)
               | (Abbr(i), _) => Abbr(i),
             ),
           );
      let tree = strip_abbr(combined_tree) |> List.rev |> List.hd;
      let grade = grade_tree(tree);
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
