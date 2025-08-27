open Haz3lcore;
open Util;

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
    | NotAvailable => "Rule not available, try other rules or change to another corpus"
    | NoAbbr => "Abbreviation not specified"
    | NotAJudgment => "Conclusion is not a judgement"
    | NoResult => "No result";

  let show = e => show(e) |> Printf.sprintf("❓ %s");
};

open DerivationTree;
open Language;

module ProofTree = {
  type t = list(Tree.p(abbr(res)))
  and res = deduction(result(Drv.Exp.t, ExternalError.t));

  let res_of_di =
      (result: option(Exp.t)): result(Drv.Exp.t, ExternalError.t) => {
    switch (result) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | DrvExp(Exp(d), _) => Ok(d)
      | _ =>
        print_endline("Warning: expected a DrvExp, got " ++ Exp.show(e));
        Error(NotAJudgment);
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
           | (Some(di), Abbr.Just({rule, _})) =>
             Abbr.Just({
               jdmt: res_of_di(di),
               rule,
             })
           | (None, Abbr(i)) => Abbr(i)
           | _ => failwith("DerivationTree.mk: ed<>di inconsistent"),
         ),
       );
  };
};

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

  let verify_single =
      (
        corpus: RuleImage.corpus,
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
        switch (RuleImage.to_rule(corpus, rule)) {
        | None => {
            res: Pending(NotAvailable),
            rule: None,
          }
        | Some(rule) =>
          let spec = RuleSpec.of_spec(rule);
          // TODO(zhiyao): may not bring it back now
          // let (spec, tests) = RuleVerify.fill_eq_tests(spec, tests);
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
            // let tests = RuleVerify.test_remove_eq_test(tests);
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

  let verify = version =>
    List.fold_left(
      (acc, tree) =>
        acc @ [Tree.fold_deep(verify_single(version, acc), tree)],
      [],
    );

  let verify = (version, ts) => ts |> verify(version) |> List.map(fst);

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
                  {
                    res: Pending(NoAbbr),
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

  let mk =
      (eds: p(Editor.t), ~stitched_results: stitched(option(Exp.t))): t => {
    verify(eds.corpus, ProofTree.mk(eds, ~stitched_results));
  };
};
