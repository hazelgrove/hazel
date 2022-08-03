open Mir_anf;

/**
 * Context mapping variables to the indet-ness of the expression to which it
 * refers.
 */
[@deriving sexp]
type completes = Ident.Map.t(Complete.t);

let rec analyze_block = (block: block, cctx): block => {
  let {block_body: (body, im), block_ty, block_complete: _, block_label}: block = block;

  let (body, cctx) = analyze_body(body, cctx);
  let im = analyze_imm(im, cctx);

  {
    block_body: (body, im),
    block_ty,
    block_complete: im.imm_complete,
    block_label,
  };
}

and analyze_body = (body: list(stmt), cctx): (list(stmt), completes) => {
  let (rev_body, cctx) =
    List.fold_left(
      ((body, cctx), stmt) => {
        let (stmt, cctx) = analyze_stmt(stmt, cctx);
        ([stmt, ...body], cctx);
      },
      ([], cctx),
      body,
    );

  (List.rev(rev_body), cctx);
}

and analyze_stmt = (stmt: stmt, cctx): (stmt, completes) => {
  let {stmt_kind, stmt_complete: _, stmt_label}: stmt = stmt;
  let (stmt_kind, stmt_complete, cctx) =
    switch (stmt_kind) {
    | SLet(p, c) =>
      let c = analyze_comp(c, cctx);
      let (p, cctx) = analyze_pat(p, c.comp_complete, cctx);
      (SLet(p, c), Complete.join(p.complete, c.comp_complete), cctx);

    /* SLetRec rhs can only be a lambda. */
    | SLetRec(
        x,
        {comp_kind: CFun(_, _), comp_ty: _, comp_complete: _, comp_label: _} as c,
      ) =>
      let cctx = Ident.Map.add(x, Complete.IndeterminatelyIncomplete, cctx);
      let c = analyze_comp(c, cctx);
      (SLetRec(x, c), c.comp_complete, cctx);

    | SLetRec(_, _) => failwith("bad let rec without function rhs")
    };

  ({stmt_kind, stmt_complete, stmt_label}, cctx);
}

and analyze_comp = (c: comp, cctx): comp => {
  let {comp_kind, comp_ty, comp_complete: _, comp_label}: comp = c;
  let (comp_kind, comp_complete) =
    switch (comp_kind) {
    | CImm(im) =>
      let im = analyze_imm(im, cctx);
      (CImm(im), im.imm_complete);

    | CBinOp(op, im1, im2) =>
      let im1 = analyze_imm(im1, cctx);
      let im2 = analyze_imm(im2, cctx);
      (
        CBinOp(op, im1, im2),
        Complete.join(im1.imm_complete, im2.imm_complete),
      );

    | CAp(fn, arg) =>
      let fn = analyze_imm(fn, cctx);
      let arg = analyze_imm(arg, cctx);
      (CAp(fn, arg), Complete.join(fn.imm_complete, arg.imm_complete));

    | CFun(param, body) =>
      let (param, cctx) =
        analyze_pat(param, IndeterminatelyIncomplete, cctx);
      let body = analyze_block(body, cctx);
      (
        CFun(param, body),
        Complete.join(body.block_complete, param.complete),
      );

    | CCons(im1, im2) =>
      let im1 = analyze_imm(im1, cctx);
      let im2 = analyze_imm(im2, cctx);
      (CCons(im1, im2), Complete.join(im1.imm_complete, im2.imm_complete));

    | CPair(im1, im2) =>
      let im1 = analyze_imm(im1, cctx);
      let im2 = analyze_imm(im2, cctx);
      (CPair(im1, im2), Complete.join(im1.imm_complete, im2.imm_complete));

    | CInj(side, im) =>
      let im = analyze_imm(im, cctx);
      (CInj(side, im), im.imm_complete);

    | CCase(scrut, rules) =>
      let scrut = analyze_imm(scrut, cctx);
      let rules = analyze_rules(scrut, rules, cctx);

      let rules_complete =
        rules
        |> List.map((rule: rule) => rule.rule_complete)
        |> Complete.join_fold;
      (
        CCase(scrut, rules),
        Complete.join(scrut.imm_complete, rules_complete),
      );

    | CEmptyHole(u, i, sigma) =>
      let sigma = analyze_sigma(sigma, cctx);
      (CEmptyHole(u, i, sigma), NecessarilyIncomplete);

    | CNonEmptyHole(reason, u, i, sigma, im) =>
      let im = analyze_imm(im, cctx);
      (CNonEmptyHole(reason, u, i, sigma, im), NecessarilyIncomplete);

    | CCast(im, ty, ty') =>
      let im = analyze_imm(im, cctx);
      (CCast(im, ty, ty'), NecessarilyIncomplete);
    };

  {comp_kind, comp_ty, comp_complete, comp_label};
}

and analyze_rules = (scrut: imm, rules: list(rule), cctx): list(rule) => {
  rules |> List.map(rule => analyze_rule(scrut, rule, cctx));
}

and analyze_rule = (scrut: imm, rule: rule, cctx): rule => {
  let {rule_pat, rule_branch, rule_complete: _, rule_label}: rule = rule;
  let (rule_pat, cctx) = analyze_pat(rule_pat, scrut.imm_complete, cctx);
  let rule_branch = analyze_block(rule_branch, cctx);
  {
    rule_pat,
    rule_branch,
    rule_complete:
      Complete.join(rule_pat.complete, rule_branch.block_complete),
    rule_label,
  };
}

and analyze_sigma = (sigma: Ident.Map.t(imm), _cctx): Ident.Map.t(imm) => {
  /* TODO: Not sure if we need to do anything to this. */
  sigma;
}

and analyze_imm = (im: imm, cctx): imm => {
  let {imm_kind, imm_ty, imm_complete: _, imm_label}: imm = im;
  let (imm_kind, imm_complete) =
    switch (imm_kind) {
    | IConst(const) =>
      let const = analyze_const(const, cctx);
      (IConst(const), Complete.NecessarilyComplete);
    | IVar(x) =>
      switch (Ident.Map.find_opt(x, cctx)) {
      | Some(x_complete) => (IVar(x), x_complete)
      | None => failwith("bad free variable " ++ Ident.to_string(x))
      }
    };

  {imm_kind, imm_ty, imm_complete, imm_label};
}

and analyze_const = (const: constant, _cctx): constant => {
  const;
}

and analyze_pat =
    (p: pat, matchee_complete: Complete.t, cctx): (pat, completes) =>
  analyze_pat'(p, matchee_complete, false, cctx)

and analyze_pat' =
    (p: pat, matchee_complete: Complete.t, in_hole: bool, cctx)
    : (pat, completes) => {
  let {kind, complete: _, label}: pat = p;
  let (kind, complete, cctx) =
    switch (kind) {
    | PVar(x) =>
      /* We mark that the variable x refers to a possibly indeterminate
       * expression if the matchee is possible indeterminate or we are in a
       * non-empty pattern hole. */
      /* TODO: Not sure if this could be more specific. */
      let x_complete =
        if (in_hole) {Complete.IndeterminatelyIncomplete} else {
          matchee_complete
        };
      let cctx = Ident.Map.add(x, x_complete, cctx);
      (kind, Complete.NecessarilyComplete, cctx);
    | PWild
    | PInt(_)
    | PFloat(_)
    | PBool(_)
    | PNil
    | PTriv => (kind, NecessarilyComplete, cctx)
    | PInj(side, p') =>
      let (p', cctx) = analyze_pat'(p', matchee_complete, in_hole, cctx);
      (PInj(side, p'), p'.complete, cctx);
    | PCons(p1, p2) =>
      let (p1, cctx) = analyze_pat'(p1, matchee_complete, in_hole, cctx);
      let (p2, cctx) = analyze_pat'(p2, matchee_complete, in_hole, cctx);
      (PCons(p1, p2), Complete.join(p1.complete, p2.complete), cctx);
    | PPair(p1, p2) =>
      let (p1, cctx) = analyze_pat'(p1, matchee_complete, in_hole, cctx);
      let (p2, cctx) = analyze_pat'(p2, matchee_complete, in_hole, cctx);
      (PPair(p1, p2), Complete.join(p1.complete, p2.complete), cctx);
    };

  ({kind, complete, label}, cctx);
};

let analyze = (block: block): block => analyze_block(block, Ident.Map.empty);
