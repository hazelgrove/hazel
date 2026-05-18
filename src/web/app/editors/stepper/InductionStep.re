open Util;
open Language;
open Haz3lcore;
open StepInterface;
open Calc.Syntax;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  /* In proof scope the scrutinee lives in the main editor's syntax
   * (the `Induction(scrut, _)` term); the sub-editor view renders and
   * edits that segment directly (see SubEditor.re). This local model
   * is DERIVED from the proof's scrut (rebuilt in `calculate` whenever
   * `scrut_src` changes) and is used only for statics, plus as the
   * editable model for legacy cell-level steppers with no backing
   * syntax. */
  scrut: CodeEditable.Model.t,
  cases: list(InductionCase.model'('stepper)),
  // Calculated
  /* Last proof-side scrut the local model was rebuilt from. */
  scrut_src: Calc.saved(Exp.t),
  elab_scrut_raw: Calc.saved(Exp.t),
  elab_scrut_sub: Calc.saved(Exp.t),
  scrut_ty: Calc.saved(Typ.t),
  scrut_co_ctx: Calc.saved(CoCtx.t),
  result: Calc.saved(Exp.t),
  join_exp: Calc.saved(Exp.t),
  is_exhaustive: Calc.saved(bool),
  validity: Calc.saved(option(bool)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  | ScrutUpdate(CodeEditable.Update.t)
  | CaseUpdate(int, InductionCase.action'('step))
  | AddCase
  | RemoveCase(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  | Scrut(CodeEditable.Selection.t)
  | Case(int, InductionCase.focus'('step));

let init = (~exp: option(Exp.t)=?, ()) => {
  let scrut =
    switch (exp) {
    | Some(e) =>
      CodeEditable.Model.mk(
        Editor.Model.mk(
          Zipper.unzip(
            ExpToSegment.exp_to_segment(
              ~settings=ExpToSegment.Settings.editable(~inline=true),
              e,
            ),
          ),
          ~root=Exp,
        ),
      )
    | None =>
      CodeEditable.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp))
    };
  {
    scrut,
    cases: [],
    scrut_src: Calc.Pending,
    elab_scrut_raw: Calc.Pending,
    elab_scrut_sub: Calc.Pending,
    scrut_ty: Calc.Pending,
    scrut_co_ctx: Calc.Pending,
    result: Calc.Pending,
    join_exp: Calc.Pending,
    is_exhaustive: Calc.Pending,
    validity: Calc.Pending,
  };
};

/* The methods in this file, like the other step files, are
   parameterized by a Stepper module that implements the
   stepper interface. This allows us to use steppers inside
   steps inside steppers. The lines below can be copied as
   boilerplate to other steps.*/
module F =
       (Stepper: STEPPER)

         : (
           STEP with
             type model = model'(Stepper.model) and
             type action = action'(Stepper.action) and
             type focus = focus'(Stepper.focus)
       ) => {
  /* Capture the outer `InductionCase.init_with` before the local
   * `module InductionCase = InductionCase.F(Stepper)` shadow takes
   * effect, so `calculate` can synthesise fresh case slots when the
   * surrounding `Induction` proof grows. */
  let fresh_case = () => InductionCase.init_with(Stepper.init);

  module InductionCase = InductionCase.F(Stepper);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = model'(Stepper.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = action'(Stepper.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = focus'(Stepper.focus);

  let update = (~settings: Settings.t, action: action, model: model) => {
    Updated.(
      switch (action) {
      | ScrutUpdate(a) =>
        let* new_scrut =
          CodeEditable.Update.update(~settings, a, model.scrut);
        {
          ...model,
          scrut: new_scrut,
        };
      | CaseUpdate(i, a) =>
        switch (List.nth_opt(model.cases, i)) {
        | Some(case) =>
          let* new_case = InductionCase.update(~settings, a, case);
          {
            ...model,
            cases: ListUtil.put_nth(i, new_case, model.cases),
          };
        | None => model |> raise_invalid_action
        }
      | AddCase =>
        let new_case = InductionCase.init;
        {
          ...model,
          cases: model.cases @ [new_case],
        }
        |> return;
      | RemoveCase(i) =>
        switch (ListUtil.remove_nth(i, model.cases)) {
        | Some(new_cases) =>
          {
            ...model,
            cases: new_cases,
          }
          |> return
        | None => model |> raise_invalid_action
        }
      }
    );
  };

  let can_undo = a =>
    switch (a) {
    | ScrutUpdate(action) => CodeEditable.Update.can_undo(action)
    | CaseUpdate(_, action) => InductionCase.can_undo(action)
    | AddCase => true
    | RemoveCase(_) => true
    };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~editor as _,
        ~info_map,
        ~proof_info_map,
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(option(Proof.t)),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    let {
      scrut,
      cases,
      scrut_src,
      elab_scrut_raw,
      elab_scrut_sub,
      scrut_ty,
      scrut_co_ctx,
      result: _,
      join_exp,
      is_exhaustive,
      validity,
    }: model = model;
    /* Sync the UI model's case list to match the surrounding
     * `Induction(_, proof_cases)` proof shape. The proof drives:
     * `emit_add_case` / `emit_remove_case` in the view emit
     * `ProofPatch`es that lengthen or shorten `proof_cases`; here we
     * grow / shrink `model.cases` to follow so the view re-renders
     * the right number of case rows on the next pass.
     *
     * Case rows below `proof_cases` length keep their existing local
     * model state (pattern editor, inner stepper). Extra rows beyond
     * `proof_cases` length are dropped. Out of proof scope we leave
     * `model.cases` untouched, preserving the legacy mutate-the-model
     * behaviour for cell-level steppers. */
    let cases =
      switch (Calc.get_value(proof)) {
      | Some({term: Induction(_, proof_cases), _}) =>
        let target = List.length(proof_cases);
        let current = List.length(cases);
        if (target == current) {
          cases;
        } else if (target > current) {
          cases @ List.init(target - current, _ => fresh_case());
        } else {
          ListUtil.take(target, cases);
        };
      | _ => cases
      };
    /* Proof-backed sync: in proof scope the scrutinee's surface syntax
     * lives in the main editor and is edited there (via the sub-editor
     * view); this local model is derived from it. Rebuild whenever the
     * proof's scrut changes — the local model is never focused in
     * proof scope, so no caret state is lost. */
    let (scrut, scrut_src) =
      switch (Calc.get_value(proof)) {
      | Some({term: Induction(proof_scrut, _), _}) =>
        let src =
          Calc.set(~eq=Exp.fast_equal_with_lexemes, proof_scrut, scrut_src);
        let scrut =
          switch (src) {
          | NewValue(e) =>
            CodeEditable.Model.mk(
              Editor.Model.mk(
                Zipper.unzip(
                  ExpToSegment.exp_to_segment(
                    ~settings=ExpToSegment.Settings.editable(~inline=true),
                    e,
                  ),
                ),
                ~root=Exp,
              ),
            )
          | OldValue(_) => scrut
          };
        (scrut, src |> Calc.save);
      | _ => (scrut, scrut_src)
      };
    let scrut =
      CodeEditable.Update.calculate(
        ~settings=Calc.get_value(settings),
        ~ctx=Calc.get_value(ctx).ctx,
        ~dynamics=Dynamics.Map.empty,
        ~is_edited=true,
        ~stitch=x => x,
        ~is_dynamic_term=true,
        scrut,
      );
    let elab_scrut_raw =
      Calc.set(
        ~eq=Exp.fast_equal_with_lexemes,
        CodeEditable.Model.get_statics(scrut).elaborated,
        elab_scrut_raw,
      );
    let elab_scrut_sub =
      elab_scrut_sub
      |> {
        let.calc raw = elab_scrut_raw
        and.calc sem_ctx = ctx;
        let env = SemanticCtx.get_env(sem_ctx);
        Substitution.in_exp(env, raw);
      };
    let scrut_statics = CodeEditable.Model.get_statics(scrut);
    let scrut_rep_id = Exp.rep_id(scrut_statics.elaborated);
    let scrut_ty = {
      let self_ty =
        switch (Statics.Map.ty_of(scrut_rep_id, scrut_statics.info_map)) {
        | Some(ty) => ty
        | None =>
          raise(Failure("Missing type info for induction step scrutinee"))
        };
      Calc.set(~eq=Typ.fast_equal, self_ty, scrut_ty);
    };
    let scrut_co_ctx = {
      let self_co_ctx =
        switch (Statics.Map.lookup_exp(scrut_rep_id, scrut_statics.info_map)) {
        | Some({co_ctx, _}) => co_ctx
        | None => CoCtx.empty
        };
      Calc.set(self_co_ctx, scrut_co_ctx);
    };
    /* Per-case body proof: when `~proof` is `Induction(_, proof_cases)`,
     * pass each case row its own `body_i` sub-proof so the case's
     * inner stepper operates on (and emits patches against) the
     * correct sub-tree. Outside induction scope (or when the proof's
     * case list is shorter than the model's), the row receives `None`
     * which makes inner step emits fall back to model mutation. */
    let case_body_proof = (i: int): Calc.t(option(Proof.t)) => {
      let descend = (p: option(Proof.t)): option(Proof.t) =>
        switch (p) {
        | Some({term: Induction(_, proof_cases), _}) =>
          List.nth_opt(proof_cases, i) |> Option.map(snd)
        | _ => None
        };
      switch (proof) {
      | OldValue(p) => Calc.OldValue(descend(p))
      | NewValue(p) => Calc.NewValue(descend(p))
      };
    };
    /* Per-case surface pattern from the proof, for the derived local
     * pattern model (see InductionCase.calculate's ~pat). */
    let case_pat = (i: int): Calc.t(option(Pat.t)) => {
      let descend = (p: option(Proof.t)): option(Pat.t) =>
        switch (p) {
        | Some({term: Induction(_, proof_cases), _}) =>
          List.nth_opt(proof_cases, i) |> Option.map(fst)
        | _ => None
        };
      switch (proof) {
      | OldValue(p) => Calc.OldValue(descend(p))
      | NewValue(p) => Calc.NewValue(descend(p))
      };
    };
    let (cases, validities) =
      List.mapi(
        (i, case) =>
          InductionCase.calculate(
            ~settings,
            ~scrut_ty,
            ~scrut_co_ctx,
            ~elab_scrut=elab_scrut_sub,
            ~ctx,
            ~info_map,
            ~exp,
            ~ana,
            ~proof=case_body_proof(i),
            ~proof_map,
            ~pat=case_pat(i),
            case,
          ),
        cases,
      )
      |> List.split;

    let new_join_exp =
      List.fold_left(
        (acc, case: InductionCase.model) =>
          switch (acc, case.last_exp) {
          | (None, Calc.Pending) => None
          | (None, Calc.Calculated(last_exp)) => Some(last_exp)
          | (Some(acc), Calc.Pending) => Some(acc)
          | (Some(acc), Calc.Calculated(last_exp))
              when Exp.fast_equal(acc, last_exp) =>
            Some(acc)
          | (Some(_), Calc.Calculated(_)) => Some(Exp.fresh(EmptyHole))
          },
        None,
        cases,
      );
    let join_exp =
      Calc.set(
        ~eq=Exp.fast_equal_with_lexemes,
        new_join_exp |> Option.value(~default=Exp.fresh(EmptyHole)),
        join_exp,
      );

    /* Exhaustiveness label reads the *static* result rather than recomputing:
     * the theorem's statics flags an inexhaustive induction with an
     * `InexhaustiveMatch` mark on the induction proof node (see
     * proof_to_info_map). We look that mark up in `proof_info_map` (the
     * whole-theorem info map) by the proof node's id, so the label stays in
     * sync with the editor error. */
    let is_exhaustive =
      is_exhaustive
      |> {
        let.calc proof = proof
        and.calc proof_info_map = proof_info_map;
        switch (proof) {
        | Some(p) =>
          switch (Statics.Map.lookup(Proof.rep_id(p), proof_info_map)) {
          | Some(info) =>
            !
              List.exists(
                fun
                | Mark.InexhaustiveMatch(_) => true
                | _ => false,
                Info.marks_of(info),
              )
          | None => true
          }
        | None => true
        };
      };

    let validity =
      validity
      |> {
        let.calc validities = Calc.combine_list(validities)
        and.calc is_exhaustive = is_exhaustive;
        List.fold_left(
          (v1, v2) =>
            switch (v1, v2) {
            | (Some(true), Some(true)) => Some(true)
            | (Some(false), Some(false)) => Some(false)
            | (_, _) => None
            },
          is_exhaustive ? Some(true) : None,
          validities,
        );
      };

    let result = exp |> Calc.save;

    Some((
      {
        scrut,
        cases,
        scrut_src,
        elab_scrut_raw: elab_scrut_raw |> Calc.save,
        elab_scrut_sub: elab_scrut_sub |> Calc.save,
        scrut_ty: scrut_ty |> Calc.save,
        scrut_co_ctx: scrut_co_ctx |> Calc.save,
        result,
        join_exp: join_exp |> Calc.save,
        is_exhaustive: is_exhaustive |> Calc.save,
        validity: validity |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some(Calc.OldValue(Exp.fresh(Atom(Bool(true))))),
      validity,
    ));
  };

  let get_cursor_info = (~inject, ~focus: focus, model: model) =>
    Cursor.(
      switch (focus) {
      | Scrut(a) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(
            ~inject=a => inject(ScrutUpdate(a)),
            ~selection=a,
            model.scrut,
          );
        ScrutUpdate(ci);
      | Case(i, a) =>
        switch (List.nth_opt(model.cases, i)) {
        | Some(case) =>
          let+ ci =
            InductionCase.get_cursor_info(
              ~inject=x => inject(CaseUpdate(i, x)),
              ~focus=a,
              case,
            );
          CaseUpdate(i, ci);
        | None => Cursor.empty
        }
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
    WebUtil.Node.text("Induction");

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~undo as _: option(Ui_effect.t(unit)),
        ~is_toplevel as _: bool,
        ~proof: option(Proof.t),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
        model: model,
      ) => {
    let scrut_focus: option(unit) =
      switch (focus) {
      | Some(Scrut(_)) => Some()
      | Some(_)
      | None => None
      };
    /* Local-model rendering of the scrutinee, used outside proof scope
     * (editable; legacy cell-level stepper) and read-only while the
     * backing segment is momentarily unresolvable. */
    let local_scrut_editor = (~read_only: bool) =>
      CodeEditable.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => take_focus(Scrut()),
        ~edit_mode=
          read_only
            ? EditMode.ReadOnly
            : EditMode.Editable({
                inject: x => inject(ScrutUpdate(x)),
                escape: _ => Ui_effect.Ignore,
                take_focus: _ => Ui_effect.Ignore,
                focus: scrut_focus,
              }),
        ~dynamics=Dynamics.Map.empty,
        model.scrut,
      );
    /* In proof scope the scrutinee is rendered as a sub-editor over
     * the main editor's own segment (see SubEditor.re): both views
     * show the same pieces and edits route to the main editor, so no
     * write-through / re-sync machinery is needed. The target is
     * anchored on the induction TILE's id (stable while editing inside
     * it), not the scrutinee term's id (which churns as the user
     * types): the scrutinee spans the tile's first child up to the
     * first `| pat => body` rule tile. */
    let scrut_editor =
      switch (main_editor, proof) {
      | (Some(channel), Some({term: Induction(_), _} as p)) =>
        let target =
          SubEditor.Target.(
            child(~anchor=Proof.rep_id(p), 0)
            /* BeforeOrEnd: with no cases there is no rule tile and the
             * scrutinee spans the whole child. */
            |> until(BeforeOrEnd(nthTile(["|", "=>"], 0)))
          );
        switch (SubEditor.mk(channel.model.editor, ~target)) {
        | Some(sub) =>
          CodeEditable.View.view(
            ~globals,
            ~signal=
              fun
              | MakeActive => take_focus(Scrut()),
            ~edit_mode=
              EditMode.Editable({
                /* Perform actions are rewritten to PerformConfined (and
                 * TAB swallowed) inside CodeEditable.View.view when a
                 * sub-editor is given. */
                inject: channel.inject,
                escape: _ => Ui_effect.Ignore,
                take_focus: _ => Ui_effect.Ignore,
                focus: scrut_focus,
              }),
            ~dynamics=Dynamics.Map.empty,
            ~sub_editor=Some(sub),
            channel.model,
          )
        | None => local_scrut_editor(~read_only=true)
        };
      | _ => local_scrut_editor(~read_only=false)
      };

    /* "Add Case" / "Remove Case" emit a `ProofPatch` that rewrites the
     * surrounding `Induction(scrut, cases)` node in syntax. The UI
     * model is then re-synced to match the new case count on the next
     * `calculate` pass. Outside of proof scope (cell-level stepper)
     * we fall back to the legacy stepper-local mutations so the
     * features still work, just without backing syntax. */
    let induction_patch =
        (new_cases: list((Pat.t, Proof.t)))
        : option(Haz3lcore.EditorTransform.patch) =>
      switch (proof) {
      | Some({term: Induction(scrut, _), _} as p) =>
        Some(
          Haz3lcore.EditorTransform.mk_proof_patch(
            ~target_id=Proof.rep_id(p),
            Proof.fresh(Induction(scrut, new_cases)),
          ),
        )
      | _ => None
      };
    let emit_add_case = () =>
      switch (proof) {
      | Some({term: Induction(_, cases), _}) =>
        let new_cases =
          cases @ [(Pat.fresh(EmptyHole), Proof.fresh(EmptyHole))];
        switch (induction_patch(new_cases)) {
        | Some(patch) => edit_syntax(patch)
        | None => inject(AddCase)
        };
      | _ => inject(AddCase)
      };
    let emit_remove_case = (i: int) =>
      switch (proof) {
      | Some({term: Induction(_, cases), _}) =>
        switch (ListUtil.remove_nth(i, cases)) {
        | Some(new_cases) =>
          switch (induction_patch(new_cases)) {
          | Some(patch) => edit_syntax(patch)
          | None => inject(RemoveCase(i))
          }
        | None => inject(RemoveCase(i))
        }
      | _ => inject(RemoveCase(i))
      };

    let add_case_button =
      Widgets.button(
        WebUtil.Node.text("Case ..."),
        ~tooltip="Add case",
        ~clss=["subtle-button", "add-case-button"],
        _ =>
        emit_add_case()
      );

    /* Structural reference to the case's pattern slot in the main
     * editor: the i-th `| pat => body` rule tile's pattern child,
     * anchored on the induction tile's id (see SubEditor.Target for
     * why this is keyed by the host tile). */
    let case_slot = (i: int): option(SubEditor.Target.t) =>
      switch (proof) {
      | Some({term: Induction(_), _} as p) =>
        Some(
          SubEditor.Target.(
            child(~anchor=Proof.rep_id(p), 0)
            |> descend(nthTile(["|", "=>"], i), ~child=0)
          ),
        )
      | _ => None
      };
    let cases =
      List.mapi(
        (i, case) =>
          InductionCase.view(
            ~globals,
            ~inject=(x: InductionCase.action) => inject(CaseUpdate(i, x)),
            ~take_focus=x => take_focus(Case(i, x)),
            ~remove_case=emit_remove_case(i),
            ~hide_stepper,
            ~edit_syntax,
            ~main_editor,
            ~slot=case_slot(i),
            ~focus=
              switch (focus) {
              | Some(Case(j, s)) when i == j => Some(s)
              | Some(_)
              | None => None
              },
            case,
          ),
        model.cases,
      );

    [
      WebUtil.div_c(
        "induction-scrut",
        [
          WebUtil.Node.text("Induction on: "),
          WebUtil.div_c("inline-editor-wrapper", [scrut_editor]),
        ],
      ),
    ]
    @ cases
    @ [add_case_button]
    @ [
      model.is_exhaustive |> Calc.get_saved_exc(~print="exhaustive")
        ? WebUtil.Node.text("exhaustive") : WebUtil.Node.text("inexhaustive"),
    ];
  };
};
