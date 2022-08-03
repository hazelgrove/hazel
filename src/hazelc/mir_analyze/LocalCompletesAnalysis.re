open Mir_anf;
open Mir_anf.Complete;

module CompleteMonad = {
  module State = {
    [@deriving sexp]
    type t = {completes: Completes.t};

    let init = {completes: ExprLabel.Map.empty};
  };

  include Util.StateMonad.Make(State);

  let init = State.init;
};

open CompleteMonad;
open CompleteMonad.Syntax;

let extend = (l, cc) => {
  let+ () =
    update(({completes}) => {
      let completes = Completes.add(l, cc, completes);
      {completes: completes};
    });
  cc;
};

let rec analyze_block =
        (
          ctx: CompleteContext.t,
          {block_body: (body, im), block_label: l, _}: block,
        ) => {
  let* (ctx, cc) = analyze_stmts(ctx, body);
  let* cc' = analyze_imm(ctx, im);
  extend(l, Complete.join(cc, cc'));
}

and analyze_stmts = (ctx, stmts: list(stmt)) => {
  switch (stmts) {
  | [] => (ctx, IndeterminatelyIncomplete) |> return
  | [stmt, ...stmts] =>
    stmts
    |> List.fold_left(
         (acc, stmt) => {
           let* (ctx, cc) = acc;
           let+ (ctx, cc') = analyze_stmt(ctx, stmt);
           (ctx, Complete.join(cc, cc'));
         },
         analyze_stmt(ctx, stmt),
       )
  };
}

and analyze_stmt = (ctx, {stmt_kind, stmt_label: _, _}) =>
  switch (stmt_kind) {
  | SLet(x, c) =>
    let+ cc = analyze_comp(ctx, c);
    let ctx = CompleteContext.add(x, cc, ctx);
    (ctx, cc);

  | SLetRec(x, param, _param_ty, _o_ty, body) =>
    /* Parameter completeness is unknown locally. */
    let ctx = CompleteContext.add(param, IndeterminatelyIncomplete, ctx);
    let ctx = CompleteContext.add(x, IndeterminatelyIncomplete, ctx);

    let+ _body_cc = analyze_block(ctx, body);
    (ctx, IndeterminatelyIncomplete);
  }

and analyze_comp = (ctx, {comp_kind, comp_label: l, _}) => {
  let* cc =
    switch (comp_kind) {
    | CImm(im) => analyze_imm(ctx, im)

    | CBinOp(_op, im1, im2) =>
      let* cc1 = analyze_imm(ctx, im1);
      let+ cc2 = analyze_imm(ctx, im2);
      Complete.join(cc1, cc2);

    | CAp(fn, arg) =>
      let* cc = analyze_imm(ctx, fn);
      let+ cc' = analyze_imm(ctx, arg);
      Complete.join(cc, cc');

    | CFun(param, _param_ty, body) =>
      let ctx = CompleteContext.add(param, IndeterminatelyIncomplete, ctx);
      let+ _body_cc = analyze_block(ctx, body);
      IndeterminatelyIncomplete;

    | CCons(im1, im2) =>
      let* cc1 = analyze_imm(ctx, im1);
      let+ cc2 = analyze_imm(ctx, im2);
      Complete.join(cc1, cc2);

    | CPair(im1, im2) =>
      let* cc1 = analyze_imm(ctx, im1);
      let+ cc2 = analyze_imm(ctx, im2);
      Complete.join(cc1, cc2);

    | CInj(_other_ty, _side, im) => analyze_imm(ctx, im)

    | CCase(scrut, rules) =>
      let* scrut_cc = analyze_imm(ctx, scrut);
      let+ rules_cc = analyze_rules(ctx, rules, scrut_cc);
      Complete.join(scrut_cc, rules_cc);

    | CEmptyHole(_u, _i, _sigma) => NecessarilyIncomplete |> return

    | CNonEmptyHole(_reason, _u, _i, _sigma, im) =>
      let* _cc = analyze_imm(ctx, im);
      NecessarilyIncomplete |> return;

    | CCast(im, _ty, _ty') =>
      let* _ = analyze_imm(ctx, im);
      IndeterminatelyIncomplete |> return;
    };

  extend(l, cc);
}

and analyze_rules = (ctx, rules, scrut_complete) =>
  rules
  |> List.fold_left(
       (acc, rule) => {
         let* cc = acc;
         let+ cc' = analyze_rule(ctx, rule, scrut_complete);
         Complete.join(cc, cc');
       },
       NecessarilyComplete |> return,
     )

and analyze_rule =
    (ctx, {rule_pat, rule_branch, rule_label: _, _}, scrut_complete) => {
  let* (pat_cc, ctx) = analyze_pat(ctx, rule_pat, scrut_complete);
  let+ branch_cc = analyze_block(ctx, rule_branch);
  Complete.join(pat_cc, branch_cc);
}

and analyze_imm = (ctx, {imm_kind, imm_label: l, _}: imm) =>
  switch (imm_kind) {
  | IConst(const) =>
    let* cc = analyze_const(ctx, const);
    extend(l, cc);

  | IVar(x) =>
    switch (CompleteContext.find_opt(x, ctx)) {
    | Some(x_complete) => extend(l, x_complete)
    /* FIXME: Fail here or return error? */
    | None => failwith("bad free variable " ++ Ident.to_string(x))
    }
  }

and analyze_const = (_ctx, const: constant) =>
  switch (const) {
  | ConstBool(_)
  | ConstInt(_)
  | ConstFloat(_)
  | ConstTriv
  | ConstNil(_) => NecessarilyComplete |> return
  }

and analyze_pat = (ctx, p, matchee_complete) =>
  analyze_pat'(ctx, p, matchee_complete, false)

and analyze_pat' =
    (ctx: CompleteContext.t, {kind, label: _, _}, matchee_complete, in_hole) =>
  switch (kind) {
  | PVar(x) =>
    /* We mark that the variable x refers to a possibly indeterminate
     * expression if the matchee is possible indeterminate or we are in a
     * non-empty pattern hole. */
    /* TODO: Not sure if this could be more specific. */
    let x_complete =
      if (in_hole) {IndeterminatelyIncomplete} else {matchee_complete};
    (NecessarilyComplete, CompleteContext.add(x, x_complete, ctx)) |> return;

  | PWild
  | PInt(_)
  | PFloat(_)
  | PBool(_)
  | PNil
  | PTriv => (NecessarilyComplete, ctx) |> return

  | PInj(_side, p') => analyze_pat'(ctx, p', matchee_complete, in_hole)

  | PCons(p1, p2) =>
    let* (cc1, ctx) = analyze_pat'(ctx, p1, matchee_complete, in_hole);
    let+ (cc2, ctx) = analyze_pat'(ctx, p2, matchee_complete, in_hole);
    (Complete.join(cc1, cc2), ctx);

  | PPair(p1, p2) =>
    let* (cc1, ctx) = analyze_pat'(ctx, p1, matchee_complete, in_hole);
    let+ (cc2, ctx) = analyze_pat'(ctx, p2, matchee_complete, in_hole);
    (Complete.join(cc1, cc2), ctx);
  };

let analyze = block => {
  let (State.{completes}, cc) =
    analyze_block(CompleteContext.empty, block, CompleteMonad.init);
  (cc, completes);
};
