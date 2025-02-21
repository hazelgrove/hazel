open Haz3lcore;
open Util;

module ExternalError = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | NoRule
    | NotAvailable
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
    | NotAvailable => "Rule not available, try other rules or change version"
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

open DerivationTree;

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
  type t = list(Tree.p(abbr(res)))
  and res = result(deduction(Drv.Exp.t), ExternalError.t);

  let res_of_di = (result: option(Exp.t), rule): res => {
    let jdmt =
      switch (result) {
      | Some(e) =>
        switch (IdTagged.term_of(DHExp.strip_casts(e))) {
        | DrvExp(Exp(d), _) => d
        | _ =>
          Hole(Invalid("Not a Drv Exp: " ++ DHExp.show(e))) |> Drv.Exp.fresh
        }
      | None => Hole(Invalid("No Result")) |> Drv.Exp.fresh
      };
    Ok({jdmt, rule});
  };

  let mk =
      (eds: p(Editor.t), ~stitched_results: stitched(option(Exp.t))): t => {
    List.map2(Tree.combine, stitched_results.trees, eds.trees)
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
    spec: RuleSpec.t,
  };

  let verify_single =
      (
        version: RuleImage.version,
        acc: list((tree(info), option(Drv.Exp.t))),
        concl: abbr(ProofTree.res),
        prems: list((tree(info), option(Drv.Exp.t))),
      ) => {
    let (sub_trees, prems) = List.split(prems);
    let res =
      switch (concl) {
      | Abbr(Some(i)) => List.nth(acc, i) |> fst |> Tree.value
      | Abbr(None) => {res: Pending(NoAbbr), rule: None}
      | Just(Error(exn)) => {res: Pending(exn), rule: None}
      | Just(Ok({rule: None, _})) => {res: Pending(NoRule), rule: None}
      | Just(Ok({rule: Some(rule), jdmt: concl})) =>
        switch (RuleImage.to_rule(version, rule)) {
        | None => {res: Pending(NotAvailable), rule: None}
        | Some(rule) =>
          let spec = RuleSpec.of_spec(rule);
          // TODO(zhiyao): may not bring it back now
          // let (spec, tests) = RuleVerify.fill_eq_tests(spec, tests);
          let res =
            if (List.for_all(Option.is_some, prems)) {
              let prems = prems |> List.map(Option.get);
              let res = RuleVerify.verify(spec, (concl, prems));
              switch (res) {
              | [] => Correct
              // Note(zhiyao): we only show the first failure
              // i.e. the last one in the list
              | _ => Incorrect(res |> List.rev |> List.hd)
              };
            } else {
              Pending(PremiseNotReady);
            };
          // let tests = RuleVerify.test_remove_eq_test(tests);
          {res, rule: Some({rule, spec})};
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

  let verify = version =>
    List.fold_left(
      (acc, tree) =>
        acc @ [Tree.fold_deep(verify_single(version, acc), tree)],
      [],
    );

  let verify = (version, ts) => ts |> verify(version) |> List.map(fst);
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
      (eds: p(Editor.t), ~stitched_results: stitched(option(Exp.t))): t => {
    let proof_tree = ProofTree.mk(eds, ~stitched_results);
    let verified_tree = VerifiedTree.verify(eds.ruleset, proof_tree);
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

  let mk = (eds: 'a, ~stitched_results: stitched(option(Exp.t))): t => {
    proof_report: ProofReport.mk(eds, ~stitched_results),
  };

  let overall_score: t => score =
    report => {
      let grade = report.proof_report.grade;
      let max_points = 100;
      score_of_percent(grade, max_points);
    };
};
