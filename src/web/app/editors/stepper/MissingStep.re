open Language;
open Util;
open WebUtil;
open Calc.Syntax;
open Haz3lcore;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type open_box =
    | AxiomsOpen(AxiomsBox.Model.t)
    | RewritesOpen({
        editor: CodeEditable.Model.t,
        cached_exp: Calc.saved(Exp.t),
        cached_result: option(bool),
      })
    | NoneOpen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type assumptions = list(AssumptionBox.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    next_steps: Calc.saved(EvaluatorStep.status),
    refls: Calc.saved(list(Exp.t)),
    selected_id: Calc.saved(option(Id.t)),
    selected_exp: Calc.saved(option(Exp.t)),
    full_exp: Calc.saved(Exp.t),
    assumptions: Calc.saved(option(assumptions)),
    open_box,
    cached_env: Calc.saved(Environment.t(Exp.t)) // TODO[Matt]: remove this later, just to get env into view for now.
  };

  let init = {
    next_steps: Calc.Pending,
    refls: Calc.Pending,
    selected_id: Calc.Pending,
    selected_exp: Calc.Pending,
    full_exp: Calc.Pending,
    assumptions: Calc.Pending,
    open_box: NoneOpen,
    cached_env: Calc.Pending,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = unit;

  let persist = (_: t): persistent => ();

  let unpersist = (_: persistent): t => init;
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | ToggleAxioms
    | ProposeRewrite
    | UpdateResult(bool)
    | RewriteEditorAction(CodeEditable.Update.t)
    | AxiomBoxAction(AxiomsBox.Update.t);

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    switch (action, model.open_box) {
    | (ToggleAxioms, _) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | RewritesOpen(_) => Model.AxiomsOpen(AxiomsBox.Model.init)
        | AxiomsOpen(_) => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~logged=true);
    | (ProposeRewrite, _) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | AxiomsOpen(_) =>
          Model.RewritesOpen({
            editor:
              CodeEditable.Model.mk(
                Editor.Model.mk(Zipper.init(), ~root=Exp),
              ),
            cached_exp: Calc.Pending,
            cached_result: None,
          })
        | RewritesOpen(_) => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~recalculate=true, ~logged=true);
    | (RewriteEditorAction(action), RewritesOpen({editor, _} as r)) =>
      let* new_editor = CodeEditable.Update.update(~settings, action, editor);
      Model.{
        ...model,
        open_box:
          Model.RewritesOpen({
            ...r,
            editor: new_editor,
          }),
      };
    | (RewriteEditorAction(_), _) => model |> Updated.raise_invalid_action
    | (UpdateResult(result), RewritesOpen(r)) =>
      Model.{
        ...model,
        open_box:
          Model.RewritesOpen({
            ...r,
            cached_result: Some(result),
          }),
      }
      |> Updated.return_quiet(~logged=true)
    | (UpdateResult(_), _) => model |> Updated.raise_invalid_action
    | (AxiomBoxAction(action), AxiomsOpen(m)) =>
      let* updated = AxiomsBox.Update.update(~settings, action, m);
      Model.{
        ...model,
        open_box: Model.AxiomsOpen(updated),
      };
    | (AxiomBoxAction(_), _) => model |> Updated.raise_invalid_action
    };
  };

  let calculate =
      (
        ~settings,
        exp,
        info_map,
        ctx: Calc.t(SemanticCtx.t),
        new_next_steps,
        {
          next_steps: _,
          refls,
          assumptions,
          selected_exp,
          full_exp: _,
          selected_id,
          open_box,
          cached_env,
        }: Model.t,
        editor,
      )
      : Model.t => {
    let selected_id =
      // hacky way to get a currently-selected id
      {
        let editor: CodeSelectable.Model.t = editor |> Calc.get_value;
        try({
          let zipper = editor.editor.state.zipper;
          let root_id = seg =>
            TermData.get_root_id_using_ranges(
              seg,
              editor.editor.syntax.term_data,
              editor.editor.syntax.measured,
            );
          /* Grouping parentheses in the stepper are inserted by
             ExpToSegment with fresh ids that don't occur in the
             expression, so if the selection's root is one of those,
             retry ignoring them (i.e. treat "(e)" as "e"). Drag
             selections contain the bare "(" and ")" shards. */
          let rec strip_parens = (seg: Segment.t): Segment.t =>
            seg
            |> List.concat_map((p: Piece.t) =>
                 switch (p) {
                 | Tile({form: Form.Compound(Parens), children, _}) =>
                   List.concat_map(strip_parens, children)
                 | _ => [p]
                 }
               );
          /* Folded function bodies are projectors wrapping fresh
             parentheses, and their contents aren't measured, so recover
             the folded term's id from the projector's syntax instead. */
          let rec unparen_exp = (e: Exp.t) =>
            switch (Exp.term_of(e)) {
            | Parens(e') => unparen_exp(e')
            | _ => e
            };
          let projector_id = (seg: Segment.t) =>
            switch (List.filter(p => !Piece.is_secondary(p), seg)) {
            | [Projector(pr)] =>
              switch (MakeTerm.for_projection([pr.syntax])) {
              | Some(Exp(e)) => Some(e |> unparen_exp |> Exp.rep_id)
              | _ => None
              }
            | _ => None
            };
          let full_exp = Calc.get_value(exp);
          let in_exp = id => ProofHacks.find_exp_id(id, full_exp) != None;
          let content = zipper.selection.content;
          switch (root_id(content)) {
          | Some(id) when in_exp(id) => Some(id)
          | _ =>
            switch (projector_id(strip_parens(content))) {
            | Some(id) when in_exp(id) => Some(id)
            | _ => root_id(strip_parens(content))
            }
          };
        }) {
        | _ => None
        };
      }
      |> Calc.set(_, selected_id);
    let selected_exp =
      selected_exp
      |> {
        let.calc selected_id = selected_id
        and.calc exp = exp;
        open OptUtil.Syntax;
        let* id = selected_id;
        let* exp' = ProofHacks.find_exp_id(id, exp);
        Some(exp');
      };
    let assumptions =
      assumptions
      |> {
        let.calc _exp = selected_exp
        and.calc ctx = ctx;
        let proof_ctx =
          ctx
          |> SemanticCtx.get_env
          |> Environment.to_list
          |> List.filter_map(((name, exp)) =>
               switch (Exp.term_of(exp)) {
               | Grammar.ProofObject(e) => Some((name, e))
               | _ => None
               }
             )
          |> List.fold_left(
               (acc, (name, exp)) => ProofCtx.add_exp(name, exp, acc),
               Axioms.v,
             )
          |> List.map(ctx_entry => AssumptionBox.Model.{ctx_entry: ctx_entry});
        Some(proof_ctx);
      };
    let refls =
      refls
      |> {
        let.calc exp = exp
        and.calc ctx = ctx
        and.calc new_next_steps = new_next_steps
        and.calc info_map = info_map;
        let next_steps =
          new_next_steps
          |> (
            fun
            | EvaluatorStep.AutoStep(_) => []
            | EvaluatorStep.AvailableSteps(steps) => steps
          );
        ProofHacks.find_refls(~info_map, ~env=SemanticCtx.get_env(ctx), exp)
        |> List.filter(e =>
             !
               List.exists(
                 s => e |> Exp.rep_id == EvaluatorStep.get_step_id(s),
                 next_steps,
               )
           );
      };
    let open_box =
      switch (open_box) {
      | RewritesOpen({editor, cached_exp, cached_result}) =>
        // Calculate syntax, holes, types, etc for the editor
        let editor =
          CodeEditable.Update.calculate(
            ~settings,
            ~is_edited=true,
            ~is_dynamic_term=true,
            ~dynamics=Dynamics.Map.empty,
            ~stitch=x => x,
            ~ctx=Calc.get_value(ctx) |> SemanticCtx.get_ctx,
            editor,
          );
        // Extract an exp from the editor
        let cached_exp =
          Calc.set(
            ~eq=Exp.fast_equal_with_lexemes,
            CodeEditable.Model.get_statics(editor).elaborated,
            cached_exp,
          );
        // Reset result if editor changes
        let cached_result =
          Calc.Calculated(cached_result)
          |> {
            let.calc _ = cached_exp;
            None;
          };
        Model.RewritesOpen({
          editor,
          cached_exp: cached_exp |> Calc.save,
          cached_result: cached_result |> Calc.get_value,
        });
      | AxiomsOpen(m) =>
        AxiomsOpen(
          AxiomsBox.Update.calculate(~info_map, ~ctx, ~selected_exp, m),
        )
      | NoneOpen => NoneOpen
      };
    let cached_env =
      cached_env
      |> {
        let.calc ctx = ctx;
        SemanticCtx.get_env(ctx);
      };
    {
      next_steps: new_next_steps |> Calc.save,
      refls: refls |> Calc.save,
      assumptions: assumptions |> Calc.save,
      full_exp: exp |> Calc.save,
      selected_exp: selected_exp |> Calc.save,
      selected_id: selected_id |> Calc.save,
      cached_env: cached_env |> Calc.save,
      open_box,
    };
  };
};

module Selection = {
  open Cursor;
  // Selection handles focus

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | RewriteEditor(CodeEditable.Selection.t)
    | AxiomBoxSelection(AxiomsBox.Selection.t);

  let get_cursor_info =
      (~inject, ~selection: t, model: Model.t): cursor(Update.t) => {
    switch (selection, model.open_box) {
    | (RewriteEditor(selection), RewritesOpen({editor, _})) =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(
          ~inject=a => inject(Update.RewriteEditorAction(a)),
          ~selection,
          editor,
        );
      Update.RewriteEditorAction(ci);
    | (RewriteEditor(_), _) => empty
    | (AxiomBoxSelection(selection), AxiomsOpen(m)) =>
      let+ ci = AxiomsBox.Selection.get_cursor_info(~selection, m);
      Update.AxiomBoxAction(ci);
    | (AxiomBoxSelection(_), _) => empty
    };
  };
};

module View = {
  open OptUtil.Syntax;
  type event =
    | AddInduction(option(Exp.t))
    | AddForall
    | HideStepper
    | AddAxiomStep(string, int, Exp.t, Direction.t, string)
    | AddAlgebriteStep(int, Exp.t, Exp.t)
    | MakeActive(Selection.t)
    | TakeStep(int)
    | Refl(int);

  let get_segment_bounds = (~measured: Measured.t, segment: Segment.t) => {
    let* first_piece = ListUtil.hd_opt(segment);
    let Point.{row: start_y, col: start_x} =
      Measured.find_p(~msg="get_segment_bounds", first_piece, measured)
      |> (m => m.origin);
    let* last_piece = ListUtil.last_opt(segment);
    let Point.{row: end_y, col: end_x} =
      Measured.find_p(~msg="get_segment_bounds", last_piece, measured)
      |> (m => m.last);
    let rec get_left = (current_left: int, row: int, final_row: int) =>
      if (row > final_row) {
        current_left;
      } else {
        get_left(
          Int.min(
            current_left,
            Measured.Rows.find(row, measured.rows).content_start,
          ),
          row + 1,
          final_row,
        );
      };
    let left = get_left(start_x, start_y, end_y);
    let rec get_right = (current_right: int, row: int, final_row: int) =>
      if (row == final_row) {
        current_right;
      } else {
        get_right(
          Int.max(
            current_right,
            Measured.Rows.find(row, measured.rows).max_col,
          ),
          row + 1,
          final_row,
        );
      };
    let right = get_right(end_x, start_y, end_y);
    Some((left, right, start_y, end_y + 1));
  };

  let view_overlay =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~editor: CodeSelectable.Model.t,
        ~selected: option(Selection.t),
        ~info_map,
        model: Model.t,
      ) =>
    {
      let+ (left, right, top, bottom) =
        get_segment_bounds(
          ~measured=editor.editor.syntax.measured,
          editor.editor.state.zipper.selection.content,
        );

      let proof_button = (~callback: Ui_effect.t(unit), label: string) => {
        Node.div(
          ~attrs=[
            Attr.classes(["proof-button"]),
            Attr.on_pointerdown(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
            Attr.on_click(_ =>
              Ui_effect.Many([
                callback,
                Virtual_dom.Vdom.Effect.Stop_propagation,
              ])
            ),
          ],
          [Node.text(label)],
        );
      };

      let show_step_button =
        switch (
          model.selected_exp |> Calc.get_saved_exc(~print="Selected Exp")
        ) {
        | Some(selected_exp) =>
          List.find_index(
            x => x == (selected_exp |> Exp.rep_id),
            model.next_steps
            |> Calc.get_saved_exc(~print="next_steps")
            |> (
              fun
              | AutoStep(_) => []
              | AvailableSteps(steps) => steps
            )
            |> List.map(step => step |> EvaluatorStep.get_step_id),
          )
        | None => None
        };

      let show_refl_button =
        switch (
          model.selected_exp |> Calc.get_saved_exc(~print="Selected Exp")
        ) {
        | Some(selected_exp) =>
          List.find_index(
            x => x == (selected_exp |> Exp.rep_id),
            model.refls
            |> Calc.get_saved_exc(~print="refls")
            |> List.map(refl => refl |> Exp.rep_id),
          )
        | None => None
        };

      let show_function_body_button = {
        Calc.get_saved_exc(model.selected_exp)
        == Some(Calc.get_saved_exc(model.full_exp))
        && Exp.is_fun(Calc.get_saved_exc(model.full_exp));
      };

      // I want to make a bunch of buttons here:
      // Evaluate [TODO], Rewrite, Axioms, Cases,
      let buttons =
        Node.div(
          ~attrs=[Attr.classes(["proof-selection-buttons"])],
          (
            switch (show_step_button) {
            | None => []
            | Some(idx) => [
                proof_button(
                  ~callback=Ui_effect.Many([signal(TakeStep(idx))]),
                  "Step",
                ),
              ]
            }
          )
          @ (
            switch (show_refl_button) {
            | None => []
            | Some(idx) => [
                proof_button(
                  ~callback=
                    Ui_effect.Many([
                      globals.inject_global(
                        Set(Evaluation(ForceShowRecord)),
                      ),
                      signal(Refl(idx)),
                    ]),
                  "Reflexivity",
                ),
              ]
            }
          )
          @ (
            show_function_body_button
              ? [
                proof_button(
                  ~callback=
                    Ui_effect.Many([
                      globals.inject_global(
                        Set(Evaluation(ForceShowRecord)),
                      ),
                      signal(AddForall),
                    ]),
                  "Function Body",
                ),
              ]
              : []
          )
          @ [
            proof_button(~callback=inject(ProposeRewrite), "Algebra ▼"),
            proof_button(~callback=inject(ToggleAxioms), "Assumptions ▼"),
            proof_button(
              ~callback=
                Ui_effect.Many([
                  globals.inject_global(Set(Evaluation(ForceShowRecord))),
                  signal(
                    AddInduction(
                      model.selected_exp
                      |> Calc.get_saved_exc(~print="Selected Exp"),
                    ),
                  ),
                ]),
              "Cases/Induction",
            ),
          ],
        );

      [
        Node.div(
          ~attrs=[
            Attr.classes(["missing-step-overlay-align"]),
            DecUtil.position(
              ~width=right - left,
              ~height=bottom - top,
              ~font_metrics=globals.font_metrics,
              Point.{
                col: left,
                row: top,
              },
            ),
          ],
          [
            Node.div(
              ~attrs=[
                Attr.class_("proof-context-box"),
                /* This box is rendered inside the stepper's code editor,
                   whose pointer handlers would otherwise treat clicks and
                   drags in the box (e.g. in the rewrite editor) as
                   selection gestures and clobber the selected expression.
                   Pointer state is shared between editors, so all of
                   down/move/up must be stopped, not just pointerdown. */
                Attr.on_pointerdown(_ =>
                  Virtual_dom.Vdom.Effect.Stop_propagation
                ),
                Attr.on_pointerup(_ =>
                  Virtual_dom.Vdom.Effect.Stop_propagation
                ),
                Attr.on_mousemove(_ =>
                  Virtual_dom.Vdom.Effect.Stop_propagation
                ),
                Attr.on_contextmenu(_ =>
                  Virtual_dom.Vdom.Effect.Stop_propagation
                ),
              ],
              [buttons]
              @ {
                switch (model.open_box) {
                | NoneOpen => []
                | AxiomsOpen(m) => [
                    div_c(
                      "axiom-box",
                      AxiomsBox.View.view(
                        ~globals,
                        ~info_map,
                        ~env=
                          model.cached_env
                          |> Calc.get_saved_exc(~print="env not cached"),
                        ~inject=
                          (a: AxiomsBox.Update.t) =>
                            inject(AxiomBoxAction(a)),
                        ~take_focus=
                          (s: AxiomsBox.Selection.t) =>
                            signal(MakeActive(AxiomBoxSelection(s))),
                        ~add_axiom_step=
                          (a, b, c, d, e) =>
                            signal(AddAxiomStep(a, b, c, d, e)),
                        ~full_exp=
                          model.full_exp
                          |> Calc.get_saved_exc(~print="full_exp not cached"),
                        ~selected_exp=
                          model.selected_exp
                          |> Calc.get_saved_exc(~print="Selected Exp")
                          |> Option.value(~default=EmptyHole |> Exp.fresh, _),
                        m,
                      ),
                    ),
                  ]
                | RewritesOpen({editor, cached_exp, cached_result}) =>
                  let unboxed_cached_exp =
                    Calc.get_saved_exc(
                      ~print="cached exp not calculated",
                      cached_exp,
                    );
                  let env =
                    model.cached_env
                    |> Calc.get_saved_exc(~print="env not cached");
                  /* Names in the rewrite refer to the functions shown in
                     this step (whose let-bindings may already have been
                     substituted away), falling back to the environment. */
                  let resolve = exp =>
                    exp
                    |> Substitution.in_exp(
                         ProofHacks.named_fns(
                           model.full_exp
                           |> Calc.get_saved_exc(~print="full_exp"),
                         ),
                       )
                    |> Substitution.in_exp(env);
                  let unboxed_selected_exp =
                    Option.value(
                      ~default=EmptyHole |> Exp.fresh,
                      Calc.get_saved_exc(
                        ~print="selected exp not calculated",
                        model.selected_exp,
                      ),
                    );
                  [
                    // one element list with a div
                    // with a list containing two elements
                    // an Editor for user to propose their rewrite
                    // a button to submit the rewrite
                    div_c(
                      "rewrite-box",
                      [
                        Node.text("Replace: "),
                        CodeViewable.view_any(
                          ~globals,
                          ~settings=
                            ExpToSegment.Settings.of_core(
                              ~inline=false,
                              ~fold_fn_bodies=`Text,
                              globals.settings.core,
                            ),
                          Exp(unboxed_selected_exp),
                        ),
                        Node.text("With: "),
                        div_c(
                          "inline-editor-wrapper",
                          [
                            CodeEditable.View.view(
                              ~globals,
                              ~signal=
                                fun
                                | MakeActive =>
                                  signal(MakeActive(RewriteEditor())),
                              ~edit_mode=
                                EditMode.Editable({
                                  inject: x =>
                                    inject(RewriteEditorAction(x)),
                                  escape: _ => Ui_effect.Ignore,
                                  take_focus: _ => Ui_effect.Ignore,
                                  focus:
                                    switch (selected) {
                                    | Some(RewriteEditor ()) => Some()
                                    | _ => None
                                    },
                                }),
                              ~dynamics=Dynamics.Map.empty,
                              editor,
                            ),
                          ],
                        ),
                      ]
                      @ {
                        switch (cached_result) {
                        | Some(true) => [
                            Node.text("Valid"),
                            Widgets.button(
                              ~clss=["proof-button"],
                              Node.text("Replace"),
                              ~tooltip="replace",
                              _ =>
                              signal(
                                AddAlgebriteStep(
                                  ProofHacks.exp_idx(
                                    unboxed_selected_exp,
                                    model.full_exp
                                    |> Calc.get_saved_exc(~print="full_exp"),
                                  ),
                                  unboxed_selected_exp,
                                  resolve(unboxed_cached_exp),
                                ),
                              )
                            ),
                          ]
                        | Some(false) => [Node.text("Invalid")]
                        | None => [
                            Widgets.button(
                              ~clss=["proof-button"],
                              Node.text("Check"),
                              _ =>
                                inject(
                                  UpdateResult(
                                    RewriteChecker.check_rewrite(
                                      unboxed_selected_exp
                                      |> Substitution.in_exp(env),
                                      resolve(unboxed_cached_exp),
                                    ),
                                  ),
                                ),
                              ~tooltip="check",
                            ),
                          ]
                        };
                      },
                    ),
                  ];
                };
              },
            ),
          ],
        ),
      ];
    }
    |> Option.value(~default=[]);

  let view_justification =
      (
        ~globals: Globals.t,
        ~hide_stepper: Ui_effect.t(unit),
        ~undo: option(Ui_effect.t(unit)),
        ~is_toplevel: bool,
        _model: Model.t,
      ) => {
    let button_back =
      Widgets.button_d(
        Icons.undo,
        switch (undo) {
        | Some(u) => u
        | None => Ui_effect.Ignore
        },
        ~disabled=Option.is_none(undo),
        ~tooltip="Step Backwards",
      );
    let button_hide_stepper =
      Widgets.toggle(~tooltip="Show Stepper", "s", true, _ => hide_stepper);
    let toggle_show_history =
      Widgets.toggle(
        ~tooltip="Show History",
        "h",
        globals.settings.core.evaluation.stepper_history,
        _ =>
        globals.inject_global(Set(Evaluation(ShowRecord)))
      );
    let eval_settings =
      Widgets.button(Icons.gear, _ =>
        globals.inject_global(Set(Evaluation(ShowSettings)))
      );
    Node.div(
      ~attrs=[Attr.classes(["stepper-controls"])],
      [button_back]
      @ (
        is_toplevel
          ? [eval_settings, toggle_show_history, button_hide_stepper] : []
      ),
    );
  };
};
