exception FreeBoundVar(Var.t);
exception BadLetRec;

[@deriving sexp]
type level =
  | NoAnalysis
  | LocalAnalysis;

[@deriving sexp]
type opts = {level};

/**
 * Context mapping variables to the indet-ness of the expression to which it
 * refers.
 */
[@deriving sexp]
type indet_context = VarMap.t_(Anf.completeness);

let rec analyze_prog = (~opts, prog: Anf.prog, ictx): Anf.prog => {
  let {prog_body: (body, c), prog_ty, prog_complete: _}: Anf.prog = prog;

  let (body, ictx) = analyze_body(~opts, body, ictx);
  let c = analyze_comp(~opts, c, ictx);

  {prog_body: (body, c), prog_ty, prog_complete: c.comp_complete};
}

and analyze_body =
    (~opts, body: list(Anf.stmt), ictx): (list(Anf.stmt), indet_context) => {
  let (rev_body, ictx) =
    List.fold_left(
      ((body, ictx), stmt) => {
        let (stmt, ictx) = analyze_stmt(~opts, stmt, ictx);
        ([stmt, ...body], ictx);
      },
      ([], ictx),
      body,
    );

  (List.rev(rev_body), ictx);
}

and analyze_stmt = (~opts, stmt: Anf.stmt, ictx): (Anf.stmt, indet_context) => {
  let {stmt_kind, stmt_complete: _}: Anf.stmt = stmt;
  let (stmt_kind, stmt_complete, ictx) =
    switch (stmt_kind) {
    | SLet(p, c) =>
      let c = analyze_comp(~opts, c, ictx);
      let (p, ictx) = analyze_pat(~opts, p, c.comp_complete, ictx);
      (
        Anf.SLet(p, c),
        Completeness.join(p.pat_complete, c.comp_complete),
        ictx,
      );

    /* SLetRec rhs can only be a lambda. */
    | SLetRec(x, {comp_kind: CLam(_, _), comp_ty: _, comp_complete: _} as c) =>
      /* TODO: Lambda analysis */
      let ictx = VarMap.extend(ictx, (x, IndeterminatelyIncomplete));
      let c = analyze_comp(~opts, c, ictx);
      (Anf.SLetRec(x, c), c.comp_complete, ictx);

    | SLetRec(_, _) => raise(BadLetRec)
    };

  ({stmt_kind, stmt_complete}, ictx);
}

and analyze_comp = (~opts, c: Anf.comp, ictx): Anf.comp => {
  let {comp_kind, comp_ty, comp_complete: _}: Anf.comp = c;
  let (comp_kind, comp_complete): (Anf.comp_kind, Anf.completeness) =
    switch (comp_kind) {
    | CImm(im) =>
      let im = analyze_imm(~opts, im, ictx);
      (CImm(im), im.imm_complete);

    | CBinOp(op, im1, im2) =>
      let im1 = analyze_imm(~opts, im1, ictx);
      let im2 = analyze_imm(~opts, im2, ictx);
      (
        CBinOp(op, im1, im2),
        Completeness.join(im1.imm_complete, im2.imm_complete),
      );

    | CAp(fn, args) =>
      let fn = analyze_imm(~opts, fn, ictx);
      let args = args |> List.map(arg => analyze_imm(~opts, arg, ictx));

      let args_complete =
        args
        |> List.map((arg: Anf.imm) => arg.imm_complete)
        |> Completeness.join_fold;
      (CAp(fn, args), Completeness.join(fn.imm_complete, args_complete));

    | CLam(param, body) =>
      let (param, ictx) =
        analyze_pat(~opts, param, IndeterminatelyIncomplete, ictx);
      let body = analyze_prog(~opts, body, ictx);
      (
        CLam(param, body),
        Completeness.join(body.prog_complete, param.pat_complete),
      );

    | CCons(im1, im2) =>
      let im1 = analyze_imm(~opts, im1, ictx);
      let im2 = analyze_imm(~opts, im2, ictx);
      (
        CCons(im1, im2),
        Completeness.join(im1.imm_complete, im2.imm_complete),
      );

    | CPair(im1, im2) =>
      let im1 = analyze_imm(~opts, im1, ictx);
      let im2 = analyze_imm(~opts, im2, ictx);
      (
        CPair(im1, im2),
        Completeness.join(im1.imm_complete, im2.imm_complete),
      );

    | CInj(side, im) =>
      let im = analyze_imm(~opts, im, ictx);
      (CInj(side, im), im.imm_complete);

    | CCase(scrut, rules) =>
      let scrut = analyze_imm(~opts, scrut, ictx);
      let rules = analyze_rules(~opts, scrut, rules, ictx);

      let rules_complete =
        rules
        |> List.map((rule: Anf.rule) => rule.rule_complete)
        |> Completeness.join_fold;
      (
        CCase(scrut, rules),
        Completeness.join(scrut.imm_complete, rules_complete),
      );

    | CEmptyHole(u, i, sigma) =>
      let sigma = analyze_sigma(~opts, sigma, ictx);
      (CEmptyHole(u, i, sigma), NecessarilyIncomplete);

    | CNonEmptyHole(reason, u, i, sigma, im) =>
      let im = analyze_imm(~opts, im, ictx);
      (CNonEmptyHole(reason, u, i, sigma, im), NecessarilyIncomplete);

    | CCast(im, ty, ty') =>
      let im = analyze_imm(~opts, im, ictx);
      (CCast(im, ty, ty'), NecessarilyIncomplete);
    };

  {comp_kind, comp_ty, comp_complete};
}

and analyze_rules =
    (~opts, scrut: Anf.imm, rules: list(Anf.rule), ictx): list(Anf.rule) => {
  rules |> List.map(rule => analyze_rule(~opts, scrut, rule, ictx));
}

and analyze_rule = (~opts, scrut: Anf.imm, rule: Anf.rule, ictx): Anf.rule => {
  let {rule_pat, rule_branch, rule_complete: _}: Anf.rule = rule;
  let (rule_pat, ictx) =
    analyze_pat(~opts, rule_pat, scrut.imm_complete, ictx);
  let rule_branch = analyze_prog(~opts, rule_branch, ictx);
  {
    rule_pat,
    rule_branch,
    rule_complete:
      Completeness.join(rule_pat.pat_complete, rule_branch.prog_complete),
  };
}

and analyze_sigma =
    (~opts, sigma: VarMap.t_(Anf.comp), _ictx): VarMap.t_(Anf.comp) => {
  let _ = opts;
  /* TODO: Not sure if we need to do anything to this. */
  sigma;
}

and analyze_imm = (~opts, im: Anf.imm, ictx): Anf.imm => {
  let {imm_kind, imm_ty, imm_complete: _}: Anf.imm = im;
  let (imm_kind, imm_complete): (Anf.imm_kind, Anf.completeness) =
    switch (imm_kind) {
    | IConst(const) =>
      let const = analyze_const(~opts, const, ictx);
      (IConst(const), NecessarilyComplete);
    | IVar(x) =>
      switch (VarMap.lookup(ictx, x)) {
      | Some(x_complete) => (IVar(x), x_complete)
      | None => raise(FreeBoundVar(x))
      }
    };

  {imm_kind, imm_ty, imm_complete};
}

and analyze_const = (~opts, const: Anf.constant, _ictx): Anf.constant => {
  let _ = opts;
  const;
}

and analyze_pat =
    (~opts, p: Anf.pat, matchee_complete: Anf.completeness, ictx)
    : (Anf.pat, indet_context) =>
  analyze_pat'(~opts, p, matchee_complete, false, ictx)

and analyze_pat' =
    (
      ~opts,
      p: Anf.pat,
      matchee_complete: Anf.completeness,
      in_hole: bool,
      ictx,
    )
    : (Anf.pat, indet_context) => {
  let {pat_kind, pat_complete: _}: Anf.pat = p;
  let (pat_kind, pat_complete: Anf.completeness, ictx) =
    switch (pat_kind) {
    | PVar(x) =>
      /* We mark that the variable x refers to a possibly indeterminate
       * expression if the matchee is possible indeterminate or we are in a
       * non-empty pattern hole. */
      let ictx =
        VarMap.extend(
          ictx,
          (
            x,
            /* TODO: Not sure if this could be more specific. */
            if (in_hole) {IndeterminatelyIncomplete} else {matchee_complete},
          ),
        );
      (pat_kind, NecessarilyComplete, ictx);
    | PWild
    | PInt(_)
    | PFloat(_)
    | PBool(_)
    | PNil
    | PTriv => (pat_kind, NecessarilyComplete, ictx)
    | PInj(side, p') =>
      let (p', ictx) =
        analyze_pat'(~opts, p', matchee_complete, in_hole, ictx);
      (PInj(side, p'), p'.pat_complete, ictx);
    | PCons(p1, p2) =>
      let (p1, ictx) =
        analyze_pat'(~opts, p1, matchee_complete, in_hole, ictx);
      let (p2, ictx) =
        analyze_pat'(~opts, p2, matchee_complete, in_hole, ictx);
      (
        PCons(p1, p2),
        Completeness.join(p1.pat_complete, p2.pat_complete),
        ictx,
      );
    | PPair(p1, p2) =>
      let (p1, ictx) =
        analyze_pat'(~opts, p1, matchee_complete, in_hole, ictx);
      let (p2, ictx) =
        analyze_pat'(~opts, p2, matchee_complete, in_hole, ictx);
      (
        PPair(p1, p2),
        Completeness.join(p1.pat_complete, p2.pat_complete),
        ictx,
      );
    };

  ({pat_kind, pat_complete}, ictx);
};

let analyze = (~opts, prog: Anf.prog): Anf.prog =>
  switch (opts.level) {
  | NoAnalysis => prog
  | LocalAnalysis => analyze_prog(~opts, prog, VarMap.empty)
  };
