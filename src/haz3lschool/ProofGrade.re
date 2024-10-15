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
  type t = list(Tree.p(info))
  and info = {
    rule: option(rule),
    res,
  }
  and res =
    | Correct
    | Incorrect(RuleVerify.failure)
    | Pending(ExternalError.t)
  and rule = {
    rule: Rule.t,
    spec: RuleVerify.spec,
    tests: RuleVerify.tests,
  };
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
          | BoxedValue(e) =>
            switch (DHExp.strip_casts(e)) {
            | {term: Term(Drv(Exp(d)), _), _} =>
              Ok({jdmt: DrvElab.elab_jdmt(d), rule})
            | _ =>
              Ok({jdmt: Hole("Not a Judgement") |> DrvSyntax.fresh, rule})
            }
          | Indet(_) =>
            Ok({jdmt: Hole("Eval Idet") |> DrvSyntax.fresh, rule})
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

    let verify_single =
        (
          acc: list((tree(info), option(DrvSyntax.t))),
          concl: abbr(ProofTree.res),
          prems: list((tree(info), option(DrvSyntax.t))),
        ) => {
      let (sub_trees, prems) = List.split(prems);
      let res =
        switch (concl) {
        | Abbr(Some(i)) => List.nth(acc, i) |> fst |> Tree.value
        | Abbr(None) => {res: Pending(NoAbbr), rule: None}
        | Just(Error(exn)) => {res: Pending(exn), rule: None}
        | Just(Ok({rule: None, _})) => {res: Pending(NoRule), rule: None}
        | Just(Ok({rule: Some(rule), jdmt: concl})) =>
          let spec = RuleSpec.of_spec(rule);
          let tests = RuleTest.of_tests(rule);
          let (spec, tests) = RuleVerify.fill_eq_tests(spec, tests);
          let res =
            if (List.for_all(Option.is_some, prems)) {
              let prems = prems |> List.map(Option.get);
              let res = RuleVerify.verify(spec, tests, (concl, prems));
              switch (res) {
              | [] => Correct
              // Note(zhiyao): we only show the first failure
              // i.e. the last one in the list
              | _ => Incorrect(res |> List.rev |> List.hd)
              };
            } else {
              Pending(PremiseNotReady);
            };
          let tests = RuleVerify.test_remove_eq_test(tests);
          {res, rule: Some({rule, spec, tests})};
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
