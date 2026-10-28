open Util;
open Language;
open StepInterface;
open Calc.Syntax;

/* `have <exp> proof <subproof> => <body>` as a stepper row
 * (docs/prover-obligations.md §3.3's "prove here" exit).
 *
 * Have is the CUT rule: an ordinary two-premise proof rule whose two
 * premises are its two proof children (ProofCheck's Have arm records one
 * obligation for each on this node). So it renders like any other
 * multi-premise rule — induction being the other one — as TWO nested
 * obligation regions at the SAME indent, each with the same chrome and a
 * header naming the goal it discharges:
 *
 * - the SUBPROOF proves `<exp>`, checked in the ENCLOSING scope (it may
 *   not cite the have's own hypothesis);
 * - the BODY proves the incoming goal, in the scope EXTENDED with
 *   `<exp>` under the auto-name `have`.
 *
 * Neither is "the outer chain continuing": both are premises of this
 * step, and the reader should be able to tell which is which without
 * counting indents. They deliberately reuse `.induction-case` /
 * `.induction-case-header` rather than any have-specific chrome — have
 * is not a big enough feature to earn a stepper shape of its own.
 *
 * Hence: one keyword+argument row (ProofFormView.view_arg, the
 * proposition inline-editable as child 0 like assume's), then the two
 * premise regions.
 *
 * BOTH children's goals come from the checker's ProofMap (looked up by
 * each child's own `rep_id`) and are never re-derived here: the
 * subproof's incoming is the env-inlined proposition, the body's is the
 * outer goal passed through (have's outgoing is still the body's). That
 * is what makes step-picking inside either child root at the right goal.
 *
 * Types are defined outside the functor to make it easier to use them in
 * other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  /* The SUBPROOF's goal, as the checker computed it: ProofCheck's Have
   * arm checks `sub` against `Some(hyp)` where `hyp` is `<exp>`
   * env-inlined. Read out of the ProofMap by the subproof's own rep_id
   * so the two never drift. */
  sub_exp: Calc.saved(Exp.t),
  sub_stepper: 'stepper,
  /* The subproof's outgoing, for the target row: the subproof discharges
   * the have's obligation only by reaching literal `true` with a clean
   * subtree, so the box always shows how far it actually got against
   * that target (cf. InductionCase's last_exp). */
  sub_last_exp: Calc.saved(Exp.t),
  /* The BODY premise's goal and scope. The goal is the checker's body
   * incoming (the outer goal, passed through — `have` never moves it);
   * the scope mirrors ProofCheck's `add_hypothesis(ctx, "have", hyp)`,
   * which is what makes the auto-name `have` citable from the body's
   * rows and what discharges obligations incurred BY BODY STEPS through
   * ordinary channel-1 lookup even while the subproof is still open. */
  body_exp: Calc.saved(Exp.t),
  body_ctx: Calc.saved(SemanticCtx.t),
  body_stepper: 'stepper,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  | SubProof('step)
  | Body('step);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  | SubProof('step)
  | Body('step)
  /* The form's own expression argument, edited in place as a SubEditor
   * window onto the main editor (see ProofFormView.view_arg). `have`'s
   * proposition is child 0 of `mk_pre_c(L, ["have", "proof", "=>"], …,
   * [Exp, Proof])`, the same slot index as assume's, so the shared
   * view_arg target applies unchanged. Carries no local model: the
   * splice IS the proof text. */
  | Arg(CodeEditable.Selection.t);

let init = init_step => {
  sub_exp: Calc.Pending,
  sub_stepper: init_step,
  sub_last_exp: Calc.Pending,
  body_exp: Calc.Pending,
  body_ctx: Calc.Pending,
  body_stepper: init_step,
};

/* D54 side-condition clause `<step> with <cond> by <proof> end` is the
 * same two-premise shape and reuses this step: the CONDITION plays the
 * proposition (child 0 of both tiles), the CLAUSE PROOF plays the
 * subproof (checked in the enclosing scope, ProofCheck's With arm), and
 * the STEP plays the body -- but WITHOUT any added hypothesis: a clause
 * introduces nothing, it only discharges one of its step's conditions. */
let is_with = (proof: option(Proof.t)): bool =>
  switch (proof) {
  | Some({term: With(_, _, _), _}) => true
  | _ => false
  };

/* The proposition, read from syntax (single source of truth). */
let arg_of_proof = (proof: option(Proof.t)): option(Exp.t) =>
  switch (proof) {
  | Some({term: Have(e, _, _), _})
  | Some({term: With(_, e, _), _}) => Some(e)
  | _ => None
  };

/* The subproof / body children, or the node itself when the proof in
 * scope is not a `have` (a pass mid-rewrite). Descending matters for the
 * same reason it does in AssumeStep: a nested stepper acting on the
 * `Have` node itself would target / replace the whole form, destroying
 * its structure. */
let sub_of_proof = (p: Proof.t): Proof.t =>
  switch (p) {
  | {term: Have(_, sub, _), _}
  | {term: With(_, _, sub), _} => sub
  | p => p
  };

let body_of_proof = (p: Proof.t): Proof.t =>
  switch (p) {
  | {term: Have(_, _, body), _}
  | {term: With(body, _, _), _} => body
  | p => p
  };

module F =
       (Stepper: STEPPER)

         : (
           STEP with
             type model = model'(Stepper.model) and
             type action = action'(Stepper.action) and
             type focus = focus'(Stepper.focus)
       ) => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = model'(Stepper.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = action'(Stepper.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = focus'(Stepper.focus);

  let update = (~settings: Settings.t, action: action, model: model) => {
    Updated.(
      switch (action) {
      | SubProof(a) =>
        let* new_sub = Stepper.update(~settings, a, model.sub_stepper);
        {
          ...model,
          sub_stepper: new_sub,
        };
      | Body(a) =>
        let* new_body = Stepper.update(~settings, a, model.body_stepper);
        {
          ...model,
          body_stepper: new_body,
        };
      }
    );
  };

  let can_undo = (a: action) =>
    switch (a) {
    | SubProof(step) => Stepper.can_undo(step)
    | Body(step) => Stepper.can_undo(step)
    };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~hidden as _: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        ~info_map as _,
        ~proof_info_map as _,
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(Proof.t),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    let {sub_exp, sub_stepper, sub_last_exp, body_exp, body_ctx, body_stepper} = model;
    let descend = (f, p: Calc.t(Proof.t)): Calc.t(Proof.t) =>
      switch (p) {
      | OldValue(p) => Calc.OldValue(f(p))
      | NewValue(p) => Calc.NewValue(f(p))
      };
    let sub_proof = descend(sub_of_proof, proof);
    let body_proof = descend(body_of_proof, proof);
    /* The proposition as written, used only as the fallback goal when
     * nothing has been checked yet (no entry: the theorem hasn't been
     * evaluated, or an upstream step failed). Falls back further to the
     * outer goal when the proof in scope is not a `have` at all. */
    let written_arg =
      exp
      |> {
        let.calc_t proof = proof
        and.calc exp = exp;
        switch (arg_of_proof(Some(proof))) {
        | Some(e) => e
        | None => exp
        };
      };
    /* SUBPROOF goal: the checker's incoming for the subproof node. */
    let sub_exp =
      sub_exp
      |> {
        let.calc written_arg = written_arg
        and.calc sub_proof = sub_proof
        and.calc proof_map = proof_map;
        switch (ProofMap.lookup(Proof.rep_id(sub_proof), proof_map)) {
        | Some({incoming: Some(goal), _}) => goal
        | _ => written_arg
        };
      };
    /* How far the subproof got. Defaults to its own goal, i.e. "no
     * progress", which is what an unchecked / hole subproof means. */
    let sub_last_exp =
      sub_last_exp
      |> {
        let.calc sub_exp = sub_exp
        and.calc sub_proof = sub_proof
        and.calc proof_map = proof_map;
        switch (ProofMap.lookup(Proof.rep_id(sub_proof), proof_map)) {
        | Some({outgoing: Some(e), _}) => e
        | _ => sub_exp
        };
      };
    /* BODY goal: the checker's incoming for the body node — the outer
     * goal, since `have` is a pass-through. */
    let body_exp =
      body_exp
      |> {
        let.calc exp = exp
        and.calc body_proof = body_proof
        and.calc proof_map = proof_map;
        switch (ProofMap.lookup(Proof.rep_id(body_proof), proof_map)) {
        | Some({incoming: Some(goal), _}) => goal
        | _ => exp
        };
      };
    /* Mirror ProofCheck's Have scope exactly: the env-substituted
     * proposition under the auto-name "have". Installed
     * UNCONDITIONALLY, whether or not the subproof is finished — that is
     * what lets a body obligation discharge against it the moment the
     * wrapper is written. */
    let body_ctx =
      body_ctx
      |> {
        let.calc ctx = ctx
        and.calc proof = proof;
        switch (proof.term) {
        | Have(e, _, _) =>
          let hyp = e |> Substitution.in_exp(SemanticCtx.get_env(ctx));
          SemanticCtx.add_hypothesis(ctx, "have", hyp) |> fst;
        | _ => ctx
        };
      };
    /* The subproof is checked in the ENCLOSING scope (it may not cite the
     * have's own hypothesis), so `~ctx` goes through unchanged. */
    let sub_stepper =
      Stepper.calculate(
        ~settings,
        ~ctx,
        ~exp=sub_exp,
        ~ana,
        ~proof=sub_proof,
        ~proof_map,
        sub_stepper,
      );
    let body_stepper =
      Stepper.calculate(
        ~settings,
        ~ctx=body_ctx,
        ~exp=body_exp,
        ~ana,
        ~proof=body_proof,
        ~proof_map,
        body_stepper,
      );
    Some({
      sub_exp: sub_exp |> Calc.save,
      sub_stepper,
      sub_last_exp: sub_last_exp |> Calc.save,
      body_exp: body_exp |> Calc.save,
      body_ctx: body_ctx |> Calc.save,
      body_stepper,
    });
  };

  let get_cursor_info = (~inject, ~focus: focus, model: model) =>
    Cursor.(
      switch (focus) {
      | SubProof(a) =>
        let+ ci =
          Stepper.get_cursor_info(
            ~inject=a => inject(SubProof(a): action),
            ~focus=a,
            model.sub_stepper,
          );
        (SubProof(ci): action);
      | Body(a) =>
        let+ ci =
          Stepper.get_cursor_info(
            ~inject=a => inject(Body(a): action),
            ~focus=a,
            model.body_stepper,
          );
        (Body(ci): action);
      /* The arg editor's actions belong to the main editor (they are
       * injected through its own channel), so this row contributes no
       * cursor info of its own. */
      | Arg(_) => Cursor.empty
      }
    );

  let view_justification =
      (
        ~globals as _: Globals.t,
        ~focus as _: option(focus),
        ~inject as _: action => Ui_effect.t(unit),
        ~take_focus as _: focus => Ui_effect.t(unit),
        ~hide_stepper as _: Ui_effect.t(unit),
        ~is_toplevel as _: bool,
        ~proof: option(Proof.t),
        ~edit_syntax as
          _: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        _: model,
      ) =>
    WebUtil.Node.text(is_with(proof) ? "With" : "Have");

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        /* Unused: BOTH premises render as nested regions with
         * `~is_toplevel=false`, so the parent's flag never reaches a
         * child. Letting the body inherit it made its picker render as
         * the root working step — the "this is the outer chain" misread
         * this step deliberately drops. */
        ~is_toplevel as _: bool,
        ~proof: option(Proof.t),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
        model: model,
      ) => {
    module StepperTargetBox = StepperTargetBox.F(Stepper);
    let goal_code = (e: Exp.t) =>
      CodeViewable.view_any(
        ~globals,
        ~settings=
          Haz3lcore.ExpToSegment.Settings.of_core(
            ~inline=true,
            ~fold_fn_bodies=`Text,
            globals.settings.core,
          ),
        Exp(e),
      );
    /* One premise region: `.induction-case` chrome (shared with
     * induction's cases, the other multi-premise rule), a header naming
     * the goal, and the premise's own chain inside. Both of have's
     * premises go through this, so neither can accidentally read as more
     * or less nested than the other. */
    let premise =
        (~header: list(WebUtil.Node.t), rows: list(WebUtil.Node.t)) =>
      WebUtil.div_c(
        "induction-case",
        [WebUtil.div_c("induction-case-header", header)] @ rows,
      );
    /* Premise 1 — the subproof of `<exp>`. Target row against literal
     * `true`, the same test ProofCheck's Have arm applies before dropping
     * this premise's obligation, so an unfinished subproof shows the gap
     * it still has to close. */
    let sub_box = {
      /* Header shows the proposition AS WRITTEN when the syntax is in
       * scope: the checker's incoming is env-inlined, so a goal
       * mentioning a let-bound `f` would arrive with f's whole lambda
       * spliced in at every occurrence — unreadable as a label. The
       * nested chain itself still runs on the checker's goal. */
      let header_goal =
        switch (arg_of_proof(proof)) {
        | Some(e) => e
        | None => model.sub_exp |> Calc.get_saved_exc(~print="have sub_exp")
        };
      let rows =
        StepperTargetBox.target_box(
          ~globals,
          ~inject=x => inject(SubProof(x)),
          ~take_focus=x => take_focus(SubProof(x)),
          ~hide_stepper,
          ~focus=
            switch (focus) {
            | Some(SubProof(f)) => Some(f)
            | Some(Body(_))
            | Some(Arg(_))
            | None => None
            },
          ~is_toplevel=false,
          ~edit_syntax,
          ~main_editor,
          model.sub_stepper,
          Exp.fresh(Atom(Bool(true))),
          model.sub_last_exp |> Calc.get_saved_exc(~print="have sub_last_exp"),
        );
      [
        premise(
          ~header=[
            WebUtil.Node.text(is_with(proof) ? "with clause, proof of " : "proof of "),
            goal_code(header_goal),
          ],
          rows,
        ),
      ];
    };
    /* Premise 2 — the incoming goal, WITH `have` in scope. Same chrome
     * and same indent as premise 1: it is the have's second premise, not
     * the outer chain resuming. Its normal trailing picker is the
     * "target", so no target box here.
     *
     * `~is_toplevel=false`, like premise 1: inheriting the parent's flag
     * made the body's picker render as the ROOT working step, which is
     * precisely the "this is the outer chain" misread. */
    let body_box = {
      let rows =
        Stepper.view(
          ~globals,
          ~focus=
            switch (focus) {
            | Some(Body(f)) => Some(f)
            | Some(SubProof(_))
            | Some(Arg(_))
            | None => None
            },
          ~inject=x => inject(Body(x)),
          ~take_focus=x => take_focus(Body(x)),
          ~hide_stepper,
          ~is_toplevel=false,
          ~edit_syntax,
          ~main_editor,
          model.body_stepper,
        );
      [
        premise(
          ~header=[
            WebUtil.Node.text(
              is_with(proof) ? "the step, proof of " : "with `have`, proof of ",
            ),
            goal_code(
              model.body_exp |> Calc.get_saved_exc(~print="have body_exp"),
            ),
          ],
          rows,
        ),
      ];
    };
    ProofFormView.view_arg(
      ~globals,
      ~label=is_with(proof) ? "With: " : "Have: ",
      ~proof,
      ~main_editor,
      ~focused=
        switch (focus) {
        | Some(Arg(_)) => true
        | Some(SubProof(_))
        | Some(Body(_))
        | None => false
        },
      ~take_focus=() => take_focus(Arg()),
      arg_of_proof(proof),
    )
    /* A clause reads step-first: the step, then the proof of its
       condition. */
    @ (is_with(proof) ? body_box @ sub_box : sub_box @ body_box);
  };
};
