open Util;
open Language;
open Haz3lcore;
open StepInterface;
open Calc.Syntax;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  /* In a theorem, the scrutinee is edited in the main editor through a
   * sub-editor, and this copy, rebuilt whenever the proof's scrutinee
   * changes, serves only for statics. The cell-level stepper, with no
   * proof text, edits this copy. */
  scrut: CodeEditable.Model.t,
  cases: list(InductionCase.model'('stepper)),
  // Calculated
  /* The proof's scrutinee that `scrut` was last rebuilt from. */
  scrut_src: Calc.saved(Exp.t),
  elab_scrut_raw: Calc.saved(Exp.t),
  elab_scrut_sub: Calc.saved(Exp.t),
  scrut_ty: Calc.saved(Typ.t),
  scrut_co_ctx: Calc.saved(CoCtx.t),
  result: Calc.saved(Exp.t),
  join_exp: Calc.saved(Exp.t),
  /* None = exhaustive; Some(example) = inexhaustive, with the statics
   * checker's witness for a missing pattern. */
  inexhaustive: Calc.saved(option(Any.t)),
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
    inexhaustive: Calc.Pending,
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

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~info_map,
        ~proof_info_map,
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(Proof.t),
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
      inexhaustive,
      validity,
    }: model = model;
    /* One case row per case of the proof, keeping the existing rows' UI
     * state. Adding or removing a case patches the proof (see the view),
     * and this follows on the next pass. With no proof text (the
     * cell-level stepper) the buttons edit `cases` directly. */
    let cases =
      switch (Calc.get_value(proof)) {
      | {term: Induction(_, _, proof_cases), _} =>
        let target = List.length(proof_cases);
        let current = List.length(cases);
        if (target == current) {
          cases;
        } else if (target > current) {
          cases @ List.init(target - current, _ => InductionCase.init);
        } else {
          ListUtil.take(target, cases);
        };
      | _ => cases
      };
    /* Rebuilding loses no caret: in a theorem this copy is never
     * focused. */
    let (scrut, scrut_src) =
      switch (Calc.get_value(proof)) {
      | {term: Induction(proof_scrut, _, _), _} =>
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
    /* Each case's nested stepper works on (and patches) that case's body. */
    let case_body_proof = (i: int): Calc.t(Proof.t) =>
      proof
      |> Calc.map((p: Proof.t) =>
           switch (p) {
           | {term: Induction(_, _, proof_cases), _} =>
             switch (List.nth_opt(proof_cases, i)) {
             | Some((_, body)) => body
             | None => Proof.fresh(EmptyHole)
             }
           | _ => Proof.fresh(EmptyHole)
           }
         );

    let case_pat = (i: int): Calc.t(option(Pat.t)) =>
      proof
      |> Calc.map((p: Proof.t) =>
           switch (p) {
           | {term: Induction(_, _, proof_cases), _} =>
             List.nth_opt(proof_cases, i) |> Option.map(fst)
           | _ => None
           }
         );
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

    /* The theorem's statics mark an inexhaustive induction, with a
     * missing pattern, so the row reads that mark rather than checking
     * again, and agrees with the editor's error. */
    let inexhaustive =
      inexhaustive
      |> {
        let.calc proof = proof
        and.calc proof_info_map = proof_info_map;
        switch (Statics.Map.lookup(Proof.rep_id(proof), proof_info_map)) {
        | Some(info) =>
          List.find_map(
            fun
            | Mark.InexhaustiveMatch(_, _, example) => Some(example)
            | _ => None,
            Info.marks_of(info),
          )
        | None => None
        };
      };

    let validity =
      validity
      |> {
        let.calc validities = Calc.combine_list(validities)
        and.calc inexhaustive = inexhaustive;
        List.fold_left(
          (v1, v2) =>
            switch (v1, v2) {
            | (Some(true), Some(true)) => Some(true)
            | (Some(false), Some(false)) => Some(false)
            | (_, _) => None
            },
          Option.is_none(inexhaustive) ? Some(true) : None,
          validities,
        );
      };

    let result = exp |> Calc.save;

    Some({
      scrut,
      cases,
      scrut_src,
      elab_scrut_raw: elab_scrut_raw |> Calc.save,
      elab_scrut_sub: elab_scrut_sub |> Calc.save,
      scrut_ty: scrut_ty |> Calc.save,
      scrut_co_ctx: scrut_co_ctx |> Calc.save,
      result,
      join_exp: join_exp |> Calc.save,
      inexhaustive: inexhaustive |> Calc.save,
      validity: validity |> Calc.save,
    });
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

  let justification = _ => "induction";

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
    let scrut_focus: option(unit) =
      switch (focus) {
      | Some(Scrut(_)) => Some()
      | Some(_)
      | None => None
      };
    /* The local copy: editable with no proof text (the cell-level
     * stepper), read-only while the scrutinee can't be found in the main
     * editor. */
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
    /* The scrutinee spans the induction tile's first child, up to its
     * first `| pat => body` rule. */
    let scrut_editor =
      switch (main_editor, proof) {
      | (Some(channel), Some({term: Induction(_), _} as p)) =>
        let target =
          SubEditor.Target.(
            child(~anchor=Proof.rep_id(p), 0)
            /* With no cases there is no rule. */
            |> until(BeforeOrEnd(NthTile(["|", "=>"], 0)))
          );
        switch (SubEditor.mk(channel.model.editor, ~target)) {
        | Some(sub) =>
          CodeEditable.View.view_sub(
            ~globals,
            ~channel,
            ~take_focus=take_focus(Scrut()),
            ~focused=scrut_focus != None,
            sub,
          )
        | None => local_scrut_editor(~read_only=true)
        };
      | _ => local_scrut_editor(~read_only=false)
      };

    /* Adding or removing a case rewrites the `Induction` node, or, with no
     * proof text, the local model. */
    let induction_patch =
        (new_cases: list((Pat.t, Proof.t)))
        : option(Haz3lcore.EditorTransform.patch) =>
      switch (proof) {
      | Some({term: Induction(scrut, as_name, _), _} as p) =>
        Some(
          Haz3lcore.EditorTransform.mk_proof_patch(
            ~target_id=Proof.rep_id(p),
            Proof.fresh(Induction(scrut, as_name, new_cases)),
          ),
        )
      | _ => None
      };
    let emit_add_case = () =>
      switch (proof) {
      | Some({term: Induction(_, _, cases), _}) =>
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
      | Some({term: Induction(_, _, cases), _}) =>
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

    /* The i-th `| pat => body` rule's pattern. */
    let case_slot = (i: int): option(SubEditor.Target.t) =>
      switch (proof) {
      | Some({term: Induction(_), _} as p) =>
        Some(
          SubEditor.Target.(
            child(~anchor=Proof.rep_id(p), 0)
            |> descend(NthTile(["|", "=>"], i), ~child=0)
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
          WebUtil.Node.text("induction on: "),
          WebUtil.div_c("inline-editor-wrapper", [scrut_editor]),
        ],
      ),
    ]
    @ cases
    @ [add_case_button];
  };
};
