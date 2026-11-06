open Util;
open Language;
open StepInterface;
open Calc.Syntax;

/* A row for a wrapping proof form, `<keyword> <arg> => <body>`: assume,
 * revert, generalize and funext. The form owns a body proof rather than
 * continuing a `Seq` chain, so the row shows the keyword and its argument
 * (edited in place, see ProofFormView) and hands the body to a nested
 * stepper.
 *
 * The body's goal is the checker's, the body's own ProofMap `incoming`,
 * and is never re-derived here. With no entry yet (nothing checked, or an
 * upstream step failed) the row's own goal stands in, which is also what
 * each form passes through when it doesn't apply. The body's scope is the
 * one ProofCheck checks the body in, so the nested rows offer only facts
 * the checker accepts.
 *
 * Types are defined outside the functor to make it easier to use them in
 * other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  inner_exp: Calc.saved(Exp.t),
  inner_ctx: Calc.saved(SemanticCtx.t),
  inner_stepper: 'stepper,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  | InnerExp('step);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  | InnerExp('step)
  /* The argument is a SubEditor window onto the main editor, so it has
   * no model of its own: this records only that it holds focus. */
  | Arg(CodeEditable.Selection.t);

let init = init_step => {
  inner_exp: Calc.Pending,
  inner_ctx: Calc.Pending,
  inner_stepper: init_step,
};

/* What sets one wrapping form apart from the others. */
module type FORM = {
  type arg;
  let justification: string;
  /* Labels the argument in the row. */
  let arg_label: string;
  /* The argument and the body, when the proof is this form. */
  let parts: Proof.t => option((arg, Proof.t));
  let any_of_arg: arg => Any.t;
  /* The body's scope, from this row's goal and scope. `info_maps` type
   * the goal, in order of preference. */
  let scope:
    (~goal: Exp.t, ~info_maps: list(Statics.Map.t), SemanticCtx.t, arg) =>
    SemanticCtx.t;
};

/* `assume e => body`. The checker reads it as an implication intro (the
 * body proves the goal without its antecedent `e`) or, failing that,
 * passes the goal through unchanged. */
module Assume = {
  type arg = Exp.t;
  let justification = "Assume";
  let arg_label = "Assume: ";
  let parts = (p: Proof.t) =>
    switch (p.term) {
    | Assume(e, _, body) => Some((e, body))
    | _ => None
    };
  let any_of_arg = (e): Any.t => Exp(e);
  /* The hypothesis, env-substituted, under the name `assume`, which the
   * nested rows can cite. */
  let scope = (~goal as _, ~info_maps as _, ctx, e) =>
    SemanticCtx.add_hypothesis(
      ctx,
      "assume",
      Substitution.in_exp(SemanticCtx.get_env(ctx), e),
    )
    |> fst;
};

/* `revert e => body` moves the fact `e` into the goal: the body proves
 * `e ==> goal`, or the goal unchanged when no such fact is in scope. */
module Revert = {
  type arg = Exp.t;
  let justification = "Revert";
  let arg_label = "Revert: ";
  let parts = (p: Proof.t) =>
    switch (p.term) {
    | Revert(e, _, body) => Some((e, body))
    | _ => None
    };
  let any_of_arg = (e): Any.t => Exp(e);
  /* The fact stays in scope, which is what makes the ex-falso idiom
   * (revert, then rewrite with the other facts) work. */
  let scope = (~goal as _, ~info_maps as _, ctx, _) => ctx;
};

/* `generalize x => body` re-quantifies x: the body proves
 * `forall x -> goal`, with x's `where` restriction moved back onto the
 * binder if it had one. */
module Generalize = {
  type arg = Exp.t;
  let justification = "Generalize";
  let arg_label = "Generalize: ";
  let parts = (p: Proof.t) =>
    switch (p.term) {
    | Generalize(e, body) => Some((e, body))
    | _ => None
    };
  let any_of_arg = (e): Any.t => Exp(e);
  /* A fact whose statement mentions x free is about the old, ambient x,
   * not the new binder, so it leaves the scope. Only free occurrences
   * count, so global lemmas that bind their own x stay. An argument that
   * isn't an in-scope variable is marked by the checker, which leaves the
   * scope alone. */
  let scope = (~goal as _, ~info_maps as _, ctx: SemanticCtx.t, arg: Exp.t) => {
    let base_ctx = SemanticCtx.get_ctx(ctx);
    switch (ProofSyntax.unwrap(arg) |> Exp.term_of) {
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
                | TheoremEntry({prop: Some(fact), _}) => !mentions_x(fact)
                | _ => true
                },
              base_ctx.entries,
            ),
        },
        SemanticCtx.get_env(ctx),
      );
    | _ => ctx
    };
  };
};

/* `funext x => body`: functional extensionality as an intro rule. The
 * goal `f == g` names no argument, so unlike `forall` the binder comes
 * from the keyword, and the body proves `f(x) == g(x)`. */
module Funext = {
  type arg = Pat.t;
  let justification = "extensionality";
  let arg_label = "argument: ";
  let parts = (p: Proof.t) =>
    switch (p.term) {
    | Funext(pat, body) => Some((pat, body))
    | _ => None
    };
  let any_of_arg = (p): Any.t => Pat(p);
  /* The binder at the functions' domain type. `ProofCheck.Funext.goal` is the
   * checker's own detector, so the row and the rule agree on the type. A
   * determined domain from any of the info maps beats an unknown one. */
  let scope = (~goal, ~info_maps, ctx, pat) => {
    let env = SemanticCtx.get_env(ctx);
    let domains =
      List.filter_map(
        info_map =>
          ProofCheck.Funext.goal(~info_map, ~env, goal)
          |> Option.map(((_, _, domain)) => domain),
        info_maps,
      );
    let determined = t =>
      switch (Typ.term_of(t)) {
      | Unknown(_) => false
      | _ => true
      };
    let domain =
      switch (List.find_opt(determined, domains), domains) {
      | (Some(d), _)
      | (None, [d, ..._]) => d
      | (None, []) => Typ.fresh(Unknown(Internal))
      };
    SemanticCtx.add_from_pattern(ctx, pat, domain);
  };
};

module F =
       (Form: FORM, Stepper: STEPPER)

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

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~info_map: Calc.t(Statics.Map.t),
        ~proof_info_map: Calc.t(Statics.Map.t),
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(Proof.t),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    let {inner_exp, inner_ctx, inner_stepper} = model;
    /* The nested stepper works on the body: working on this node would
     * replace the whole form. */
    let inner_proof =
      proof
      |> Calc.map(p =>
           switch (Form.parts(p)) {
           | Some((_, body)) => body
           | None => p
           }
         );
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
        and.calc proof = proof
        and.calc goal = exp
        and.calc info_map = info_map
        and.calc proof_info_map = proof_info_map;
        switch (Form.parts(proof)) {
        /* A theorem's rows get an empty `info_map`, with which
         * `ProofCheck.Funext.goal` reads the type off the syntax, so the whole
         * theorem's statics come first. */
        | Some((arg, _)) =>
          Form.scope(~goal, ~info_maps=[proof_info_map, info_map], ctx, arg)
        | None => ctx
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
      /* The argument's actions go to the main editor through its own
       * channel. */
      | Arg(_) => Cursor.empty
      }
    );

  let justification = _ => Form.justification;

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~is_toplevel: bool,
        ~proof: option(Proof.t),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
        model: model,
      ) => {
    let arg =
      ProofFormView.view_arg(
        ~globals,
        ~label=Form.arg_label,
        ~proof,
        ~main_editor,
        ~focused=
          switch (focus) {
          | Some(Arg(_)) => true
          | Some(InnerExp(_))
          | None => false
          },
        ~take_focus=take_focus(Arg()),
        proof
        |> Option.bind(_, Form.parts)
        |> Option.map(((arg, _)) => Form.any_of_arg(arg)),
      );
    let body =
      Stepper.view(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(InnerExp(f)) => Some(f)
          | Some(Arg(_))
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
    arg @ body;
  };
};
