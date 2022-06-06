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
  let {prog_body: (body, c), prog_ty, prog_indet: _}: Anf.prog = prog;

  let (body, ictx) = analyze_body(~opts, body, ictx);
  let c = analyze_comp(~opts, c, ictx);

  {prog_body: (body, c), prog_ty, prog_indet: c.comp_indet};
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
  let {stmt_kind, stmt_indet: _}: Anf.stmt = stmt;
  let (stmt_kind, stmt_indet, ictx) =
    switch (stmt_kind) {
    | SLet(p, c) =>
      let c = analyze_comp(~opts, c, ictx);
      let (p, ictx) = analyze_pat(~opts, p, c.comp_indet, ictx);
      (Anf.SLet(p, c), Completeness.join(p.pat_indet, c.comp_indet), ictx);

    /* SLetRec rhs can only be a lambda. */
    | SLetRec(x, {comp_kind: CLam(_, _), comp_ty: _, comp_indet: _} as c) =>
      let ictx = VarMap.extend(ictx, (x, IndeterminatelyIncomplete));
      let c = analyze_comp(~opts, c, ictx);
      (Anf.SLetRec(x, c), c.comp_indet, ictx);

    | SLetRec(_, _) => raise(BadLetRec)
    };

  ({stmt_kind, stmt_indet}, ictx);
}

and analyze_comp = (~opts, c: Anf.comp, ictx): Anf.comp => {
  let {comp_kind, comp_ty, comp_indet: _}: Anf.comp = c;
  let (comp_kind, comp_indet): (Anf.comp_kind, Anf.completeness) =
    switch (comp_kind) {
    | CImm(im) =>
      let im = analyze_imm(~opts, im, ictx);
      (CImm(im), im.imm_indet);

    | CBinOp(op, im1, im2) =>
      let im1 = analyze_imm(~opts, im1, ictx);
      let im2 = analyze_imm(~opts, im2, ictx);
      (
        CBinOp(op, im1, im2),
        Completeness.join(im1.imm_indet, im2.imm_indet),
      );

    | CAp(fn, args) =>
      let fn = analyze_imm(~opts, fn, ictx);
      let args = args |> List.map(arg => analyze_imm(~opts, arg, ictx));

      let args_indet =
        args
        |> List.map((arg: Anf.imm) => arg.imm_indet)
        |> Completeness.join_fold;
      (CAp(fn, args), Completeness.join(fn.imm_indet, args_indet));

    | CLam(params, body) =>
      let (params_rev, ictx) =
        List.fold_left(
          ((params, ictx), param) => {
            let (param, ictx) =
              analyze_pat(~opts, param, IndeterminatelyIncomplete, ictx);
            ([param, ...params], ictx);
          },
          ([], ictx),
          params,
        );
      let params = List.rev(params_rev);

      let params_indet =
        params
        |> List.map((param: Anf.pat) => param.pat_indet)
        |> Completeness.join_fold;
      let body = analyze_prog(~opts, body, ictx);
      (
        CLam(params, body),
        Completeness.join(body.prog_indet, params_indet),
      );

    | CCons(im1, im2) =>
      let im1 = analyze_imm(~opts, im1, ictx);
      let im2 = analyze_imm(~opts, im2, ictx);
      (CCons(im1, im2), Completeness.join(im1.imm_indet, im2.imm_indet));

    | CPair(im1, im2) =>
      let im1 = analyze_imm(~opts, im1, ictx);
      let im2 = analyze_imm(~opts, im2, ictx);
      (CPair(im1, im2), Completeness.join(im1.imm_indet, im2.imm_indet));

    | CInj(side, im) =>
      let im = analyze_imm(~opts, im, ictx);
      (CInj(side, im), im.imm_indet);

    | CCase(scrut, rules) =>
      let scrut = analyze_imm(~opts, scrut, ictx);
      let rules = analyze_rules(~opts, scrut, rules, ictx);

      let rules_indet =
        rules
        |> List.map((rule: Anf.rule) => rule.rule_indet)
        |> Completeness.join_fold;
      (
        CCase(scrut, rules),
        Completeness.join(scrut.imm_indet, rules_indet),
      );

    | CEmptyHole(u, i, sigma) =>
      let sigma = analyze_sigma(~opts, sigma, ictx);
      (CEmptyHole(u, i, sigma), IndeterminatelyIncomplete);

    | CNonEmptyHole(reason, u, i, sigma, im) =>
      let im = analyze_imm(~opts, im, ictx);
      (CNonEmptyHole(reason, u, i, sigma, im), IndeterminatelyIncomplete);

    | CCast(im, ty, ty') =>
      let im = analyze_imm(~opts, im, ictx);
      (CCast(im, ty, ty'), IndeterminatelyIncomplete);
    };

  {comp_kind, comp_ty, comp_indet};
}

and analyze_rules =
    (~opts, scrut: Anf.imm, rules: list(Anf.rule), ictx): list(Anf.rule) => {
  rules |> List.map(rule => analyze_rule(~opts, scrut, rule, ictx));
}

and analyze_rule = (~opts, scrut: Anf.imm, rule: Anf.rule, ictx): Anf.rule => {
  let {rule_pat, rule_branch, rule_indet: _}: Anf.rule = rule;
  let (rule_pat, ictx) = analyze_pat(~opts, rule_pat, scrut.imm_indet, ictx);
  let rule_branch = analyze_prog(~opts, rule_branch, ictx);
  {
    rule_pat,
    rule_branch,
    rule_indet: Completeness.join(rule_pat.pat_indet, rule_branch.prog_indet),
  };
}

and analyze_sigma =
    (~opts, sigma: VarMap.t_(Anf.comp), _ictx): VarMap.t_(Anf.comp) => {
  let _ = opts;
  /* TODO: Not sure if we need to do anything to this. */
  sigma;
}

and analyze_imm = (~opts, im: Anf.imm, ictx): Anf.imm => {
  let {imm_kind, imm_ty, imm_indet: _}: Anf.imm = im;
  let (imm_kind, imm_indet): (Anf.imm_kind, Anf.completeness) =
    switch (imm_kind) {
    | IConst(const) =>
      let const = analyze_const(~opts, const, ictx);
      (IConst(const), NecessarilyComplete);
    | IVar(x) =>
      switch (VarMap.lookup(ictx, x)) {
      | Some(x_indet) => (IVar(x), x_indet)
      | None => raise(FreeBoundVar(x))
      }
    };

  {imm_kind, imm_ty, imm_indet};
}

and analyze_const = (~opts, const: Anf.constant, _ictx): Anf.constant => {
  let _ = opts;
  const;
}

and analyze_pat =
    (~opts, p: Anf.pat, matchee_indet: Anf.completeness, ictx)
    : (Anf.pat, indet_context) =>
  analyze_pat'(~opts, p, matchee_indet, false, ictx)

and analyze_pat' =
    (~opts, p: Anf.pat, matchee_indet: Anf.completeness, in_hole: bool, ictx)
    : (Anf.pat, indet_context) => {
  let {pat_kind, pat_indet: _}: Anf.pat = p;
  let (pat_kind, pat_indet: Anf.completeness, ictx) =
    switch (pat_kind) {
    | PVar(x) =>
      /* We mark that the variable x refers to a possibly indeterminate
       * expression if the matchee is possible indeterminate or we are in a
       * non-empty pattern hole. */
      let ictx =
        VarMap.extend(
          ictx,
          (x, if (in_hole) {IndeterminatelyIncomplete} else {matchee_indet}),
        );
      (pat_kind, NecessarilyComplete, ictx);
    | PWild
    | PInt(_)
    | PFloat(_)
    | PBool(_)
    | PNil
    | PTriv => (pat_kind, NecessarilyComplete, ictx)
    | PInj(side, p') =>
      let (p', ictx) = analyze_pat'(~opts, p', matchee_indet, in_hole, ictx);
      (PInj(side, p'), p'.pat_indet, ictx);
    | PCons(p1, p2) =>
      let (p1, ictx) = analyze_pat'(~opts, p1, matchee_indet, in_hole, ictx);
      let (p2, ictx) = analyze_pat'(~opts, p2, matchee_indet, in_hole, ictx);
      (PCons(p1, p2), Completeness.join(p1.pat_indet, p2.pat_indet), ictx);
    | PPair(p1, p2) =>
      let (p1, ictx) = analyze_pat'(~opts, p1, matchee_indet, in_hole, ictx);
      let (p2, ictx) = analyze_pat'(~opts, p2, matchee_indet, in_hole, ictx);
      (PPair(p1, p2), Completeness.join(p1.pat_indet, p2.pat_indet), ictx);
    };

  ({pat_kind, pat_indet}, ictx);
};

let analyze = (~opts, prog: Anf.prog): Anf.prog =>
  switch (opts.level) {
  | NoAnalysis => prog
  | LocalAnalysis => analyze_prog(~opts, prog, VarMap.empty)
  };
