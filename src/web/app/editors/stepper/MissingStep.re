open Language;
open Util;
open WebUtil;
open Calc.Syntax;
open Haz3lcore;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type open_box =
    | AxiomsOpen
    | RewritesOpen({
        editor: CodeEditable.Model.t,
        cached_exp: Calc.saved(Exp.t),
        cached_result: option(bool),
      })
    | NoneOpen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type rewrites = {rewrites: list(Exp.t)};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    next_steps: Calc.saved(EvaluatorStep.status),
    refls: Calc.saved(list(Exp.t)),
    selected_id: Calc.saved(option(Id.t)),
    selected_exp: Calc.saved(option(Exp.t)),
    full_exp: Calc.saved(Exp.t),
    rewrites: Calc.saved(option(rewrites)),
    open_box,
  };

  let init = {
    next_steps: Calc.Pending,
    refls: Calc.Pending,
    selected_id: Calc.Pending,
    selected_exp: Calc.Pending,
    full_exp: Calc.Pending,
    rewrites: Calc.Pending,
    open_box: NoneOpen,
  };
  let get_selected_exp = (m: t): Exp.t =>
    m.selected_exp
    |> Calc.saved_to_option
    |> Option.join
    |> OptUtil.get(() => EmptyHole |> Exp.fresh);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = unit;

  let persist = (_: t): persistent => ();

  let unpersist = (_: persistent): t => init;
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | ToggleAxioms
    | ProposeRewrite
    | UpdateResult(bool)
    | RewriteEditorAction(CodeEditable.Update.t);

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    switch (action, model.open_box) {
    | (ToggleAxioms, _) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | RewritesOpen(_) => Model.AxiomsOpen
        | AxiomsOpen => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet;
    | (ProposeRewrite, _) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | AxiomsOpen =>
          Model.RewritesOpen({
            editor: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
            cached_exp: Calc.Pending,
            cached_result: None,
          })
        | RewritesOpen(_) => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~recalculate=true);
    | (RewriteEditorAction(action), RewritesOpen({editor, _} as r)) =>
      open Updated;
      let* new_editor = CodeEditable.Update.update(~settings, action, editor);
      Model.{
        ...model,
        open_box:
          Model.RewritesOpen({
            ...r,
            editor: new_editor,
          }),
      };
    | (RewriteEditorAction(_), _) => model |> Updated.return_quiet
    | (UpdateResult(result), RewritesOpen(r)) =>
      Model.{
        ...model,
        open_box:
          Model.RewritesOpen({
            ...r,
            cached_result: Some(result),
          }),
      }
      |> Updated.return_quiet
    | (UpdateResult(_), _) => model |> Updated.return_quiet
    };
  };

  let can_undo = (action: t): bool => {
    switch (action) {
    | ToggleAxioms
    | ProposeRewrite
    | UpdateResult(_)
    | RewriteEditorAction(_) => false
    };
  };

  let calculate =
      (
        ~settings,
        exp,
        ctx: Calc.t(Ctx.t),
        _state,
        new_next_steps,
        {
          next_steps: _,
          refls,
          rewrites,
          selected_exp,
          full_exp: _,
          selected_id,
          open_box,
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
          let selection = zipper.selection.content;
          let skel = Segment.skel(selection);
          let root = Skel.root(skel);
          let idx = Aba.first_a(root);
          let piece = List.nth(selection, idx);
          let id = Piece.id(piece);
          Some(id);
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
    let rewrites =
      rewrites
      |> {
        let.calc exp = selected_exp;
        open OptUtil.Syntax;
        let* exp' = exp;
        Some(Model.{rewrites: ProofCtx.get_rewrites(Axioms.v, exp')});
      };
    let refls =
      refls
      |> {
        let.calc exp = exp
        and.calc new_next_steps = new_next_steps;
        let next_steps =
          new_next_steps
          |> (
            fun
            | EvaluatorStep.AutoStep(_) => []
            | EvaluatorStep.AvailableSteps(steps) => steps
          );
        ProofHacks.find_refls(exp)
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
            ~ctx=Calc.get_value(ctx),
            editor,
          );
        // Extract an exp from the editor
        let cached_exp =
          Calc.set(
            ~eq=Exp.fast_equal,
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
      | AxiomsOpen => AxiomsOpen
      | NoneOpen => NoneOpen
      };
    {
      next_steps: new_next_steps |> Calc.save,
      refls: refls |> Calc.save,
      rewrites: rewrites |> Calc.save,
      full_exp: exp |> Calc.save,
      selected_exp: selected_exp |> Calc.save,
      selected_id: selected_id |> Calc.save,
      open_box,
    };
  };
};

module Selection = {
  open Cursor;
  // Selection handles focus

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | RewriteEditor(CodeEditable.Selection.t);

  let get_cursor_info = (~selection: t, model: Model.t): cursor(Update.t) => {
    switch (selection, model.open_box) {
    | (RewriteEditor(selection), RewritesOpen({editor, _})) =>
      let+ ci = CodeEditable.Selection.get_cursor_info(~selection, editor);
      Update.RewriteEditorAction(ci);
    | (RewriteEditor(_), _) => empty
    };
  };

  let handle_key_event = (~selection: t, ~event, ~model: Model.t) => {
    switch (selection, model.open_box) {
    | (RewriteEditor(selection), RewritesOpen({editor, _})) =>
      CodeEditable.Selection.handle_key_event(~selection, editor, event)
      |> Option.map(x => Update.RewriteEditorAction(x))
    | (RewriteEditor(_), _) => None
    };
  };
};

module View = {
  open OptUtil.Syntax;
  type event =
    | AddInduction(option(Exp.t))
    | AddForall
    | HideStepper
    | AddAxiomStep(string, int, Exp.t, Exp.t)
    | MakeActive(Selection.t);

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
            Measured.Rows.find(row, measured.rows).indent,
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

  let view_rewrites = (~globals, ~signal, model: Model.t) => {
    let unpacked_rewrites =
      model.rewrites
      |> Calc.get_saved_exc(~print="view_step_rewrites")
      |> Option.value(~default=Model.{rewrites: []})
      |> (r => r.rewrites);
    (
      unpacked_rewrites |> List.is_empty
        ? [] : [WebUtil.Node.text("Rewrites:")]
    )
    @ (
      List.map(
        (exp: Exp.t) =>
          [
            WebUtil.div_c(
              "axiom-row",
              [
                Widgets.button(Icons.star, _ =>
                  signal(
                    AddAxiomStep(
                      "axiom step",
                      ProofHacks.exp_idx(
                        Model.get_selected_exp(model),
                        model.full_exp
                        |> Calc.get_saved_exc(~print="full_exp"),
                      ),
                      Model.get_selected_exp(model),
                      exp,
                    ),
                  )
                ),
                exp
                |> Haz3lcore.ExpToSegment.(
                     exp_to_segment(
                       ~settings=
                         Settings.of_core(
                           ~inline=false,
                           globals.settings.core,
                         ),
                     )
                   )
                |> CodeViewable.view_segment(
                     ~globals,
                     ~sort=Exp,
                     ~shape_map=Haz3lcore.Id.Map.empty,
                   ),
              ],
            ),
          ],
        unpacked_rewrites,
      )
      |> List.flatten
    );
  };

  let view_overlay =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~editor: CodeSelectable.Model.t,
        ~selected: option(Selection.t),
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
            proof_button(~callback=inject(ProposeRewrite), "Rewrite ▼"),
            proof_button(~callback=inject(ToggleAxioms), "Axioms ▼"),
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
              "Cases",
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
                Attr.on_pointerdown(_ =>
                  Virtual_dom.Vdom.Effect.Stop_propagation
                ),
              ],
              [buttons]
              @ {
                switch (model.open_box) {
                | NoneOpen => []
                | AxiomsOpen => [
                    div_c(
                      "axiom-box",
                      view_rewrites(~globals, ~signal, model),
                    ),
                  ]
                | RewritesOpen({editor, cached_exp, cached_result}) =>
                  let unboxed_cached_exp =
                    Calc.get_saved_exc(
                      ~print="cached exp not calculated",
                      cached_exp,
                    );
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
                              globals.settings.core,
                            ),
                          ~shape_map=Haz3lcore.Id.Map.empty,
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
                              ~inject=x => inject(RewriteEditorAction(x)),
                              ~schedule_global=_ => (), //TODO(andrew)
                              ~selected=
                                switch (selected) {
                                | Some(RewriteEditor ()) => true
                                | _ => false
                                },
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
                                AddAxiomStep(
                                  "rewrite",
                                  ProofHacks.exp_idx(
                                    unboxed_selected_exp,
                                    model.full_exp
                                    |> Calc.get_saved_exc(~print="full_exp"),
                                  ),
                                  unboxed_selected_exp,
                                  unboxed_cached_exp,
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
                                      unboxed_selected_exp,
                                      unboxed_cached_exp,
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
