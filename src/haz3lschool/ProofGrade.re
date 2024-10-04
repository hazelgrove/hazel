open Haz3lcore;
open Util;

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

  let show =
    fun
    | NoRule => "Rule not specified"
    | NoAbbr => "Abbreviation not specified"
    | PremiseNotReady => "Premise(s) not ready"
    | NotAJudgment => "Conclusion not a judgement"
    | EvalOff => "Evaluation is off"
    | EvalFail => "Evaluation failed"
    | EvalPending => "Evaluation pending"
    | EvalIndet => "Evaluation indet"
    | Stepper => "Stepper error"
    | NoElab => "No elaboration";
};

module VerifiedTree = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Tree.p(info))
  and info = {
    ghost: option(DrvSyntax.deduction(DrvSyntax.t)),
    rule: option(Rule.t),
    res,
  }
  and res =
    | Correct
    | Incorrect(RuleVerify.failure)
    | Pending(ExternalError.t);
};

module F = (ExerciseEnv: Exercise.ExerciseEnv) => {
  open Exercise.F(ExerciseEnv);
  open ProofCore;

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

  module ProofTree = {
    open ExternalError;

    type t = list(Tree.p(abbr(res)))
    and res = result(deduction(DrvSyntax.t), ExternalError.t);

    let res_of_di = ({result, _}: DynamicsItem.t, rule): res =>
      switch (result) {
      | Evaluation({evaluation, _}) =>
        switch (evaluation) {
        | ResultOk({result, _}) =>
          ignore(rule);
          switch (result) {
          | BoxedValue({term: Term(Drv(Exp(d))), _}) =>
            Ok({jdmt: DrvElab.elab_jdmt(d), rule})
          | Indet(_) => Error(EvalIndet)
          | _ => Error(EvalIndet)
          };
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
    include VerifiedTree;

    let show_res: res => string =
      fun
      | Correct => "✅"
      | Pending(err) => "⌛️ " ++ ExternalError.show(err)
      | Incorrect(err) => "❌ " ++ RuleVerify.repr_failure(err);

    let verify_single =
        (
          acc: list((tree(info), option(DrvSyntax.t))),
          concl: abbr(ProofTree.res),
          prems: list((tree(info), option(DrvSyntax.t))),
        ) => {
      let (sub_trees, prems) = List.split(prems);
      let are_prems_ready = List.for_all(Option.is_some, prems);
      let res =
        switch (concl) {
        | Abbr(Some(i)) => List.nth(acc, i) |> fst |> Tree.value
        | Abbr(None) => {res: Pending(NoAbbr), ghost: None, rule: None}
        | Just(Error(exn)) => {res: Pending(exn), ghost: None, rule: None}
        | Just(Ok({rule: None, _})) => {
            res: Pending(NoRule),
            ghost: None,
            rule: None,
          }
        | Just(Ok(_)) when !are_prems_ready => {
            res: Pending(PremiseNotReady),
            ghost: None,
            rule: None,
          }
        | Just(Ok({jdmt: concl, rule: Some(rule)})) =>
          let prems = prems |> List.map(Option.get);
          let deduction: DrvSyntax.deduction(DrvSyntax.t) = {prems, concl};
          let ghost = RuleExample.of_ghost(rule);
          let deduction = RuleVerify.bind_ghost(deduction, ghost);
          switch (
            RuleVerify.verify_original(rule, deduction.prems, deduction.concl)
          ) {
          | Ok(_) => {res: Correct, ghost: Some(ghost), rule: Some(rule)}
          | Error(err) => {
              res: Incorrect(err),
              ghost: Some(ghost),
              rule: Some(rule),
            }
          };
        };
      let res =
        switch (res.rule) {
        | Some(_) => res
        | None =>
          switch (concl) {
          | Just(Ok({rule: Some(rule), _})) => {
              ...res,
              rule: Some(rule),
              ghost: Some(RuleExample.of_ghost(rule)),
            }
          | _ => res
          }
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
                | Abbr(None) =>
                  Tree.Node(
                    VerifiedTree.{
                      res: VerifiedTree.Pending(NoAbbr),
                      ghost: None,
                      rule: None,
                    },
                    [],
                  )
                | Abbr(Some(i)) => List.nth(acc, i)
                },
              tree,
            ),
          ],
        [],
      );

    let grade_tree: Tree.p(VerifiedTree.info) => percentage =
      Tree.fold_deep((value: VerifiedTree.info, children: list(percentage)) =>
        switch (value, children) {
        | ({res: Correct, _}, []) => 1.
        | ({res: Correct, _}, _) =>
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
