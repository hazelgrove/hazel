/**
  Grading of a derivation: turning the user's editable proof trees into a
  [VerifiedTree.t] where each node is annotated [Correct], [PartialCorrect],
  [Incorrect], or [Pending].

  Pipeline:
    Editor trees  -- ProofTree.mk -->  trees with parsed conclusion terms
                  -- VerifiedTree.verify -->  trees annotated with [info].
 */
open Haz3lcore;
open Util_web;

/* Errors that arise from the surrounding exercise context rather than from
   rule verification itself (missing rule choice, unparseable conclusion, …). */
module ExternalError = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | NoRule
    | NotAvailable
    | NoAbbr
    | NotAJudgment
    | NoResult;

  let show =
    fun
    | NoRule => "Rule not specified"
    | NotAvailable => "Rule not available, try other rules or change to another rule_set"
    | NoAbbr => "Abbreviation not specified"
    | NotAJudgment => "Conclusion is not a judgement"
    | NoResult => "No result";

  let show = e => show(e) |> Printf.sprintf("❓ %s");
};

open DerivationExercise;
open Language;

/* Intermediate representation: editor trees paired with the parsed conclusion
   of each deduction (or a reason why it couldn't be parsed). */
module ProofTree = {
  type t = list(Tree.p(abbr(res)))
  and res = deduction(result(Drv.Exp.t, ExternalError.t));

  /* Extract the derivation conclusion from an evaluator result, which wraps
     it as a [DrvQuote(Exp(_), _)] if parsing succeeded. */
  let conclusion_of_result =
      (result: option(Exp.t)): result(Drv.Exp.t, ExternalError.t) => {
    switch (result) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | DrvQuote(Exp(d), _) => Ok(d)
      | _ => Error(NotAJudgment)
      }
    | None => Error(NoResult)
    };
  };

  let mk =
      (eds: p(Editor.t), ~stitched_results: stitched(option(Exp.t))): t => {
    List.map2(Tree.combine, stitched_results.trees, eds.trees)
    |> List.map(
         Tree.map(
           fun
           | (Some(result), Abbr.Just({rule, _})) =>
             Abbr.Just({
               jdmt: conclusion_of_result(result),
               rule,
             })
           | (None, Abbr(i)) => Abbr(i)
           | _ =>
             failwith(
               "DrvGrading.ProofTree.mk: editors/results inconsistent",
             ),
         ),
       );
  };
};

/* Verified trees: each node carries the rule the user selected (if any) and
   a per-node verification result. */
module VerifiedTree = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Tree.p(info))
  and info = {
    rule: option(rule),
    res,
  }
  and res =
    | Correct
    | PartialCorrect(RuleVerify.specced)
    | Incorrect(RuleVerify.failure)
    | Pending(ExternalError.t)
  and rule = {
    rule: Rule.t,
    spec: RuleSpec.t,
  };

  /* Verify a single deduction node against the selected rule in [rule_set].
     [acc] is the list of already-verified abbreviation trees (used to resolve
     [Abbr(Some(i))] references); [prems] carries the sub-trees for this
     deduction along with their parsed conclusions. */
  let verify_single =
      (
        rule_set: RuleImage.rule_set,
        acc: list((tree(info), option(Drv.Exp.t))),
        concl: abbr(ProofTree.res),
        prems: list((tree(info), option(Drv.Exp.t))),
      ) => {
    let (sub_trees, prems) = List.split(prems);
    let res =
      switch (concl) {
      | Abbr(Some(i)) => List.nth(acc, i) |> fst |> Tree.value
      | Abbr(None) => {
          res: Pending(NoAbbr),
          rule: None,
        }
      | Just({rule: None, _}) => {
          res: Pending(NoRule),
          rule: None,
        }
      | Just({rule: Some(rule), jdmt: concl}) =>
        switch (RuleImage.to_rule(rule_set, rule)) {
        | None => {
            res: Pending(NotAvailable),
            rule: None,
          }
        | Some(rule) =>
          let spec = RuleSpec.of_spec(rule);
          let res =
            switch (concl) {
            | Ok(concl) =>
              let prems =
                prems
                |> List.map(
                     fun
                     | Some(prem) => prem
                     | None => Drv.Exp.fresh(Hole(DrvGrammar.EmptyHole)),
                   );
              let res = RuleVerify.verify(spec, (concl, prems));
              switch (res) {
              | [] => Correct
              | _ =>
                switch (RuleVerify.all_partial_correct(res)) {
                | Some(specced) => PartialCorrect(specced)
                | None => Incorrect(res |> List.rev |> List.hd)
                }
              };
            | Error(e) => Pending(e)
            };
          {
            res,
            rule:
              Some({
                rule,
                spec,
              }),
          };
        }
      };
    let concl =
      switch (concl) {
      | Abbr(Some(i)) => List.nth(acc, i) |> snd
      | Just({jdmt: Ok(jdmt), _}) => Some(jdmt)
      | _ => None
      };
    (Tree.Node(res, sub_trees), concl);
  };

  let verify: (RuleImage.rule_set, ProofTree.t) => t =
    (rule_set, ts) => {
      let folded =
        List.fold_left(
          (acc, tree) =>
            acc @ [Tree.fold_deep(verify_single(rule_set, acc), tree)],
          [],
          ts,
        );
      List.map(fst, folded);
    };

  let mk =
      (eds: p(Editor.t), ~stitched_results: stitched(option(Exp.t))): t => {
    verify(eds.rule_set, ProofTree.mk(eds, ~stitched_results));
  };
};
