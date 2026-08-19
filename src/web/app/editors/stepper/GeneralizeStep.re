open Util;
open Language;
open StepInterface;
open Calc.Syntax;

/* `generalize <exp> => <proof>` as a stepper row
 * (docs/prover-obligations.md, Phase 4b). Like Forall, this is a WRAPPING
 * proof form: it owns a body proof rather than continuing a `Seq` chain,
 * so the row renders the keyword and its re-quantified variable and then
 * hands the body to a nested stepper — structurally the same shape as
 * ForallStep.
 *
 * Types are defined outside the functor to make it easier to use them in
 * other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  /* The body's goal and scope. The goal comes from the checker (the
   * body's own ProofMap `incoming`) — `forall x -> G`, or
   * `forall x where g -> G` when x's `where` restriction travelled back
   * onto the binder — so neither the re-quantification nor the
   * restriction recovery is re-derived here. The scope mirrors
   * `ProofCheck`'s capture handling: every fact mentioning x is removed
   * (it is about the OLD x), so the nested rows only offer facts the
   * checker will actually accept. */
  inner_exp: Calc.saved(Exp.t),
  inner_ctx: Calc.saved(SemanticCtx.t),
  inner_stepper: 'stepper,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  | InnerExp('step);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  | InnerExp('step);

let init = init_step => {
  inner_exp: Calc.Pending,
  inner_ctx: Calc.Pending,
  inner_stepper: init_step,
};

/* The generalized variable, read from syntax (single source of truth). */
let arg_of_proof = (proof: option(Proof.t)): option(Exp.t) =>
  switch (proof) {
  | Some({term: Generalize(e, _), _}) => Some(e)
  | _ => None
  };

/* The body's scope under a `generalize x`: every fact whose statement
 * mentions x FREE becomes unavailable, because it is about the old,
 * ambient x rather than the newly-quantified binder. This mirrors the
 * removal `ProofCheck` performs (both the `ProofOf` ctx entries that
 * drive discharge-channel-1 lookup and the `ProofObject` env entries that
 * drive rule lookup); over-removal would be sound but needlessly hide
 * global lemmas that bind their own x, hence the FREE-occurrence test via
 * the same co-context machinery `ProofCtx.of_env` uses.
 *
 * Recovery: an argument that is not a bare in-scope variable is what
 * `ProofCheck` marks `MalformedGeneralize` and passes through, so the
 * scope is left alone here too. */
let generalized_ctx = (ctx: SemanticCtx.t, arg: Exp.t): SemanticCtx.t => {
  let base_ctx = SemanticCtx.get_ctx(ctx);
  switch (ProofCheck.unwrap_head(arg) |> Exp.term_of) {
  | Var(x) when Ctx.lookup_var(base_ctx, x) != None =>
    let mentions_x = (fact: Exp.t) =>
      ProofRule.mentions_any(ProofRule.exp_to_rule(fact), [x]);
    SemanticCtx.of_ctx_and_env(
      {
        ...base_ctx,
        entries:
          List.filter(
            (entry: Ctx.entry) =>
              switch (entry) {
              | VarEntry({typ, _}) =>
                switch (Typ.term_of(typ)) {
                | ProofOf(fact) => !mentions_x(fact)
                | _ => true
                }
              | _ => true
              },
            base_ctx.entries,
          ),
      },
      Environment.filter(
        (_, v) =>
          switch (Exp.term_of(v)) {
          | Grammar.ProofObject(fact) => !mentions_x(fact)
          | _ => true
          },
        SemanticCtx.get_env(ctx),
      ),
    );
  | _ => ctx
  };
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
      | InnerExp(a) =>
        let* new_inner_step =
          Stepper.update(~settings, a, model.inner_stepper);
        {
          ...model,
          inner_stepper: new_inner_step,
        };
      }
    );
  };

  let can_undo = (a: action) =>
    switch (a) {
    | InnerExp(step) => Stepper.can_undo(step)
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
    let {inner_exp, inner_ctx, inner_stepper} = model;
    /* Descend into the body proof: the inner stepper operates on `body`,
     * not on the Generalize node itself (otherwise inner-stepper actions
     * would target / replace the Generalize, destroying its
     * structure). */
    let descend = (p: Proof.t): Proof.t =>
      switch (p) {
      | {term: Generalize(_, body), _} => body
      | p => p
      };
    let inner_proof =
      switch (proof) {
      | OldValue(p) => Calc.OldValue(descend(p))
      | NewValue(p) => Calc.NewValue(descend(p))
      };
    /* The body's goal, as the checker computed it. No entry yet (nothing
     * checked, or an upstream step failed) falls back to this row's own
     * expression — re-quantifying it here would only guess at the
     * restriction recovery the checker already does. */
    let inner_exp =
      inner_exp
      |> {
        let.calc exp = exp
        and.calc inner_proof = inner_proof
        and.calc proof_map = proof_map;
        switch (ProofMap.lookup(Proof.rep_id(inner_proof), proof_map)) {
        | Some({incoming: Some(goal), _}) => goal
        | _ => exp
        };
      };
    let inner_ctx =
      inner_ctx
      |> {
        let.calc ctx = ctx
        and.calc proof = proof;
        switch (proof.term) {
        | Generalize(e, _) => generalized_ctx(ctx, e)
        | _ => ctx
        };
      };
    let inner_stepper =
      Stepper.calculate(
        ~settings,
        ~ctx=inner_ctx,
        ~exp=inner_exp,
        ~ana,
        ~proof=inner_proof,
        ~proof_map,
        inner_stepper,
      );
    Some({
      inner_exp: inner_exp |> Calc.save,
      inner_ctx: inner_ctx |> Calc.save,
      inner_stepper,
    });
  };

  let get_cursor_info = (~inject, ~focus: focus, model: model) =>
    Cursor.(
      switch (focus) {
      | InnerExp(a) =>
        let+ ci =
          Stepper.get_cursor_info(
            ~inject=a => inject(InnerExp(a): action),
            ~focus=a,
            model.inner_stepper,
          );
        (InnerExp(ci): action);
      }
    );

  let view_justification =
      (
        ~globals as _: Globals.t,
        ~focus as _: option(focus),
        ~inject as _: action => Ui_effect.t(unit),
        ~take_focus as _: focus => Ui_effect.t(unit),
        ~hide_stepper as _: Ui_effect.t(unit),
        ~undo as _: option(Ui_effect.t(unit)),
        ~is_toplevel as _: bool,
        ~proof as _: option(Proof.t),
        ~edit_syntax as
          _: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        _: model,
      ) =>
    WebUtil.Node.text("Generalize");

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~undo as _: option(Ui_effect.t(unit)),
        ~is_toplevel: bool,
        ~proof: option(Proof.t),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
        model: model,
      ) => {
    let inner_stepper =
      Stepper.view(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(InnerExp(f)) => Some(f)
          | None => None
          },
        ~inject=x => inject(InnerExp(x)),
        ~take_focus=x => take_focus(InnerExp(x)),
        ~hide_stepper,
        ~is_toplevel,
        ~edit_syntax,
        ~main_editor,
        model.inner_stepper,
      );
    ProofFormView.view_arg(
      ~globals,
      ~label="Generalize: ",
      arg_of_proof(proof),
    )
    @ inner_stepper;
  };
};
