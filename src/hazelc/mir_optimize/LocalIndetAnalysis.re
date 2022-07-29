open Mir_anf;

/**
 * Context mapping variables to the indet-ness of the expression to which it
 * refers.
 */
[@deriving sexp]
type complete_context = VarMap.t_(completeness);

let rec analyze_block = (block: block, ictx): block => {
  let {block_body: (body, im), block_ty, block_complete: _, block_label}: block = block;

  let (body, ictx) = analyze_body(body, ictx);
  let im = analyze_imm(im, ictx);

  {
    block_body: (body, im),
    block_ty,
    block_complete: im.imm_complete,
    block_label,
  };
}

and analyze_body = (body: list(stmt), ictx): (list(stmt), complete_context) => {
  let (rev_body, ictx) =
    List.fold_left(
      ((body, ictx), stmt) => {
        let (stmt, ictx) = analyze_stmt(stmt, ictx);
        ([stmt, ...body], ictx);
      },
      ([], ictx),
      body,
    );

  (List.rev(rev_body), ictx);
}

and analyze_stmt = (stmt: stmt, ictx): (stmt, complete_context) => {
  let {stmt_kind, stmt_complete: _, stmt_label}: stmt = stmt;
  let (stmt_kind, stmt_complete, ictx) =
    switch (stmt_kind) {
    | SLet(p, c) =>
      let c = analyze_comp(c, ictx);
      let (p, ictx) = analyze_pat(p, c.comp_complete, ictx);
      (
        SLet(p, c),
        Completeness.join(p.pat_complete, c.comp_complete),
        ictx,
      );

    /* SLetRec rhs can only be a lambda. */
    | SLetRec(
        x,
        {comp_kind: CFun(_, _), comp_ty: _, comp_complete: _, comp_label: _} as c,
      ) =>
      /* TODO: Funbda analysis */
      let ictx = VarMap.extend(ictx, (x, IndeterminatelyIncomplete));
      let c = analyze_comp(c, ictx);
      (SLetRec(x, c), c.comp_complete, ictx);

    | SLetRec(_, _) => failwith("bad let rec without function rhs")
    };

  ({stmt_kind, stmt_complete, stmt_label}, ictx);
}

and analyze_comp = (c: comp, ictx): comp => {
  let {comp_kind, comp_ty, comp_complete: _, comp_label}: comp = c;
  let (comp_kind, comp_complete): (comp_kind, completeness) =
    switch (comp_kind) {
    | CImm(im) =>
      let im = analyze_imm(im, ictx);
      (CImm(im), im.imm_complete);

    | CBinOp(op, im1, im2) =>
      let im1 = analyze_imm(im1, ictx);
      let im2 = analyze_imm(im2, ictx);
      (
        CBinOp(op, im1, im2),
        Completeness.join(im1.imm_complete, im2.imm_complete),
      );

    | CAp(fn, arg) =>
      let fn = analyze_imm(fn, ictx);
      let arg = analyze_imm(arg, ictx);
      (CAp(fn, arg), Completeness.join(fn.imm_complete, arg.imm_complete));

    | CFun(param, body) =>
      let (param, ictx) =
        analyze_pat(param, IndeterminatelyIncomplete, ictx);
      let body = analyze_block(body, ictx);
      (
        CFun(param, body),
        Completeness.join(body.block_complete, param.pat_complete),
      );

    | CCons(im1, im2) =>
      let im1 = analyze_imm(im1, ictx);
      let im2 = analyze_imm(im2, ictx);
      (
        CCons(im1, im2),
        Completeness.join(im1.imm_complete, im2.imm_complete),
      );

    | CPair(im1, im2) =>
      let im1 = analyze_imm(im1, ictx);
      let im2 = analyze_imm(im2, ictx);
      (
        CPair(im1, im2),
        Completeness.join(im1.imm_complete, im2.imm_complete),
      );

    | CInj(side, im) =>
      let im = analyze_imm(im, ictx);
      (CInj(side, im), im.imm_complete);

    | CCase(scrut, rules) =>
      let scrut = analyze_imm(scrut, ictx);
      let rules = analyze_rules(scrut, rules, ictx);

      let rules_complete =
        rules
        |> List.map((rule: rule) => rule.rule_complete)
        |> Completeness.join_fold;
      (
        CCase(scrut, rules),
        Completeness.join(scrut.imm_complete, rules_complete),
      );

    | CEmptyHole(u, i, sigma) =>
      let sigma = analyze_sigma(sigma, ictx);
      (CEmptyHole(u, i, sigma), NecessarilyIncomplete);

    | CNonEmptyHole(reason, u, i, sigma, im) =>
      let im = analyze_imm(im, ictx);
      (CNonEmptyHole(reason, u, i, sigma, im), NecessarilyIncomplete);

    | CCast(im, ty, ty') =>
      let im = analyze_imm(im, ictx);
      (CCast(im, ty, ty'), NecessarilyIncomplete);
    };

  {comp_kind, comp_ty, comp_complete, comp_label};
}

and analyze_rules = (scrut: imm, rules: list(rule), ictx): list(rule) => {
  rules |> List.map(rule => analyze_rule(scrut, rule, ictx));
}

and analyze_rule = (scrut: imm, rule: rule, ictx): rule => {
  let {rule_pat, rule_branch, rule_complete: _, rule_label}: rule = rule;
  let (rule_pat, ictx) = analyze_pat(rule_pat, scrut.imm_complete, ictx);
  let rule_branch = analyze_block(rule_branch, ictx);
  {
    rule_pat,
    rule_branch,
    rule_complete:
      Completeness.join(rule_pat.pat_complete, rule_branch.block_complete),
    rule_label,
  };
}

and analyze_sigma = (sigma: VarMap.t_(imm), _ictx): VarMap.t_(imm) => {
  /* TODO: Not sure if we need to do anything to this. */
  sigma;
}

and analyze_imm = (im: imm, ictx): imm => {
  let {imm_kind, imm_ty, imm_complete: _, imm_label}: imm = im;
  let (imm_kind, imm_complete): (imm_kind, completeness) =
    switch (imm_kind) {
    | IConst(const) =>
      let const = analyze_const(const, ictx);
      (IConst(const), NecessarilyComplete);
    | IVar(x) =>
      switch (VarMap.lookup(ictx, x)) {
      | Some(x_complete) => (IVar(x), x_complete)
      | None => failwith("bad free variable " ++ x)
      }
    };

  {imm_kind, imm_ty, imm_complete, imm_label};
}

and analyze_const = (const: constant, _ictx): constant => {
  const;
}

and analyze_pat =
    (p: pat, matchee_complete: completeness, ictx): (pat, complete_context) =>
  analyze_pat'(p, matchee_complete, false, ictx)

and analyze_pat' =
    (p: pat, matchee_complete: completeness, in_hole: bool, ictx)
    : (pat, complete_context) => {
  let {pat_kind, pat_complete: _, pat_label}: pat = p;
  let (pat_kind, pat_complete: completeness, ictx) =
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
      let (p', ictx) = analyze_pat'(p', matchee_complete, in_hole, ictx);
      (PInj(side, p'), p'.pat_complete, ictx);
    | PCons(p1, p2) =>
      let (p1, ictx) = analyze_pat'(p1, matchee_complete, in_hole, ictx);
      let (p2, ictx) = analyze_pat'(p2, matchee_complete, in_hole, ictx);
      (
        PCons(p1, p2),
        Completeness.join(p1.pat_complete, p2.pat_complete),
        ictx,
      );
    | PPair(p1, p2) =>
      let (p1, ictx) = analyze_pat'(p1, matchee_complete, in_hole, ictx);
      let (p2, ictx) = analyze_pat'(p2, matchee_complete, in_hole, ictx);
      (
        PPair(p1, p2),
        Completeness.join(p1.pat_complete, p2.pat_complete),
        ictx,
      );
    };

  ({pat_kind, pat_complete, pat_label}, ictx);
};

let analyze = (block: block): block => analyze_block(block, VarMap.empty);
