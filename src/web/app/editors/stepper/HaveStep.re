open Util;
open Language;
open StepInterface;
open Calc.Syntax;

/* `have <exp> proof <sub> => <body>` as a stepper row: the cut rule, a
 * rule with two premises, which are its two proof children.
 *
 * - `sub` proves `<exp>`, in the enclosing scope (it can't cite the
 *   have's own hypothesis);
 * - `body` proves the incoming goal, with `<exp>` in scope as `have`.
 *
 * Neither is the outer chain continuing, so the row renders them as two
 * regions at the same indent, each headed by the goal it proves. They
 * reuse induction's case chrome (`.induction-case`), the other rule with
 * several premises. Both goals are the checker's (each child's ProofMap
 * `incoming`), so step-picking in either region starts at the right goal.
 *
 * Types are defined outside the functor to make it easier to use them in
 * other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  /* `<exp>`, env-substituted by the checker. */
  sub_exp: Calc.saved(Exp.t),
  sub_stepper: 'stepper,
  /* How far `sub` got, for its target row: it discharges the premise only
   * by reaching `true`. */
  sub_last_exp: Calc.saved(Exp.t),
  /* The outer goal, which `have` passes through. */
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
  /* `<exp>`, edited in place like a wrapping form's argument (see
   * WrappingStep.focus'). */
  | Arg(CodeEditable.Selection.t);

let init = init_step => {
  sub_exp: Calc.Pending,
  sub_stepper: init_step,
  sub_last_exp: Calc.Pending,
  body_exp: Calc.Pending,
  body_ctx: Calc.Pending,
  body_stepper: init_step,
};

let arg_of_proof = (proof: option(Proof.t)): option(Exp.t) =>
  switch (proof) {
  | Some({term: Have(e, _, _), _}) => Some(e)
  | _ => None
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

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~info_map as _,
        ~proof_info_map as _,
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(Proof.t),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    let {sub_exp, sub_stepper, sub_last_exp, body_exp, body_ctx, body_stepper} = model;
    /* The nested steppers work on the children: working on this node
     * would replace the whole form. */
    let sub_proof =
      proof
      |> Calc.map((p: Proof.t) =>
           switch (p) {
           | {term: Have(_, sub, _), _} => sub
           | p => p
           }
         );
    let body_proof =
      proof
      |> Calc.map((p: Proof.t) =>
           switch (p) {
           | {term: Have(_, _, body), _} => body
           | p => p
           }
         );
    /* `<exp>` as written, the subproof's goal until the checker has one
     * (nothing checked yet, or an upstream step failed). */
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
    /* An unchecked subproof has made no progress. */
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
    /* As in the checker, `have` is in scope whether or not the subproof
     * is finished, so a body obligation discharges against it as soon as
     * the form is written. */
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
      /* The argument's actions go to the main editor through its own
       * channel. */
      | Arg(_) => Cursor.empty
      }
    );

  let justification = _ => "Have";

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
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
    let premise =
        (~header: list(WebUtil.Node.t), rows: list(WebUtil.Node.t)) =>
      WebUtil.div_c(
        "induction-case",
        [WebUtil.div_c("induction-case-header", header)] @ rows,
      );
    /* The subproof's target row shows the gap left to `true`. */
    let sub_box = {
      /* The header shows `<exp>` as written: the checker's goal has every
       * let-bound function's body spliced in, unreadable as a label. */
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
          ~header=[WebUtil.Node.text("proof of "), goal_code(header_goal)],
          rows,
        ),
      ];
    };
    /* The body's own trailing picker is its target row. Like the
     * subproof it is not top-level, or its picker would render as the
     * outer chain's working step. */
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
            WebUtil.Node.text("with `have`, proof of "),
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
      ~label="Have: ",
      ~proof,
      ~main_editor,
      ~focused=
        switch (focus) {
        | Some(Arg(_)) => true
        | Some(SubProof(_))
        | Some(Body(_))
        | None => false
        },
      ~take_focus=take_focus(Arg()),
      arg_of_proof(proof) |> Option.map((e): Any.t => Exp(e)),
    )
    @ sub_box
    @ body_box;
  };
};
