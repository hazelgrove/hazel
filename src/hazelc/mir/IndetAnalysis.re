exception NotImplemented;
exception FreeBoundVar(Var.t);

[@deriving sexp]
type analysis_level =
  | Local;

[@deriving sexp]
type opts = {analysis_level};

[@deriving sexp]
type indet_context = VarMap.t_(Anf.has_indet);

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
    | SLet(p, NoRec, c) =>
      let c = analyze_comp(~opts, c, ictx);
      let (p, ictx) = analyze_pat(~opts, p, c.comp_indet, ictx);
      (Anf.SLet(p, NoRec, c), p.pat_indet || c.comp_indet, ictx);

    /* TODO: Recursive functions. */
    | SLet(_p, Rec, _c) => raise(NotImplemented)
    };

  ({stmt_kind, stmt_indet}, ictx);
}

and analyze_comp = (~opts, c: Anf.comp, ictx): Anf.comp => {
  let {comp_kind, comp_ty, comp_indet: _}: Anf.comp = c;
  let (comp_kind, comp_indet): (Anf.comp_kind, Anf.has_indet) =
    switch (comp_kind) {
    | CImm(im) =>
      let im = analyze_imm(~opts, im, ictx);
      (CImm(im), im.imm_indet);

    | CBinOp(op, im1, im2) =>
      let im1 = analyze_imm(~opts, im1, ictx);
      let im2 = analyze_imm(~opts, im2, ictx);
      (CBinOp(op, im1, im2), im1.imm_indet || im2.imm_indet);

    | CAp(fn, args) =>
      let fn = analyze_imm(~opts, fn, ictx);
      let args = args |> List.map(arg => analyze_imm(~opts, arg, ictx));
      (
        CAp(fn, args),
        fn.imm_indet || List.exists((arg: Anf.imm) => arg.imm_indet, args),
      );

    | CLam(params, body) =>
      let (params_rev, ictx) =
        List.fold_left(
          ((params, ictx), param) => {
            let (param, ictx) = analyze_pat(~opts, param, true, ictx);
            ([param, ...params], ictx);
          },
          ([], ictx),
          params,
        );
      let params = List.rev(params_rev);

      let body = analyze_prog(~opts, body, ictx);
      (
        CLam(params, body),
        body.prog_indet
        || List.exists((param: Anf.pat) => param.pat_indet, params),
      );

    | CCons(im1, im2) =>
      let im1 = analyze_imm(~opts, im1, ictx);
      let im2 = analyze_imm(~opts, im2, ictx);
      (CCons(im1, im2), im1.imm_indet || im2.imm_indet);

    | CPair(im1, im2) =>
      let im1 = analyze_imm(~opts, im1, ictx);
      let im2 = analyze_imm(~opts, im2, ictx);
      (CPair(im1, im2), im1.imm_indet || im2.imm_indet);

    | CInj(side, im) =>
      let im = analyze_imm(~opts, im, ictx);
      (CInj(side, im), im.imm_indet);

    | CEmptyHole(u, i, sigma) =>
      let sigma = analyze_sigma(~opts, sigma, ictx);
      (CEmptyHole(u, i, sigma), true);

    | CNonEmptyHole(reason, u, i, sigma, im) =>
      let im = analyze_imm(~opts, im, ictx);
      (CNonEmptyHole(reason, u, i, sigma, im), true);

    | CCast(im, ty, ty') =>
      let im = analyze_imm(~opts, im, ictx);
      (CCast(im, ty, ty'), true);
    };

  {comp_kind, comp_ty, comp_indet};
}

and analyze_sigma =
    (~opts, sigma: VarMap.t_(Anf.comp), _ictx): VarMap.t_(Anf.comp) => {
  let _ = opts;
  /* TODO: Not sure if we need to do anything to this. */
  sigma;
}

and analyze_imm = (~opts, im: Anf.imm, ictx): Anf.imm => {
  let {imm_kind, imm_ty, imm_indet: _}: Anf.imm = im;
  let (imm_kind, imm_indet): (Anf.imm_kind, Anf.has_indet) =
    switch (imm_kind) {
    | IConst(const) =>
      let const = analyze_const(~opts, const, ictx);
      (IConst(const), false);
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
    (~opts, p: Anf.pat, matchee_indet: bool, ictx): (Anf.pat, indet_context) =>
  analyze_pat'(~opts, p, matchee_indet, false, ictx)

and analyze_pat' =
    (~opts, p: Anf.pat, matchee_indet: bool, in_hole: bool, ictx)
    : (Anf.pat, indet_context) => {
  let {pat_kind, pat_indet: _}: Anf.pat = p;
  let (pat_kind, pat_indet, ictx) =
    switch (pat_kind) {
    | PVar(x) =>
      let ictx = VarMap.extend(ictx, (x, matchee_indet || in_hole));
      (pat_kind, false, ictx);
    | PWild
    | PInt(_)
    | PFloat(_)
    | PBool(_)
    | PNil
    | PTriv => (pat_kind, false, ictx)
    | PInj(side, p') =>
      let (p', ictx) = analyze_pat'(~opts, p', matchee_indet, in_hole, ictx);
      (PInj(side, p'), p'.pat_indet, ictx);
    | PCons(p1, p2) =>
      let (p1, ictx) = analyze_pat'(~opts, p1, matchee_indet, in_hole, ictx);
      let (p2, ictx) = analyze_pat'(~opts, p2, matchee_indet, in_hole, ictx);
      (PCons(p1, p2), p1.pat_indet || p2.pat_indet, ictx);
    | PPair(p1, p2) =>
      let (p1, ictx) = analyze_pat'(~opts, p1, matchee_indet, in_hole, ictx);
      let (p2, ictx) = analyze_pat'(~opts, p2, matchee_indet, in_hole, ictx);
      (PPair(p1, p2), p1.pat_indet || p2.pat_indet, ictx);
    };

  ({pat_kind, pat_indet}, ictx);
};

let analyze = (~opts, prog: Anf.prog): Anf.prog =>
  analyze_prog(~opts, prog, VarMap.empty);
