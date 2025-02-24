open Haz3lcore;
open Util;
open Calc.Syntax;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type open_box =
    | AxiomsOpen
    | RewritesOpen(CodeEditable.Model.t)
    | NoneOpen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type rewrites = {rewrites: list(Exp.t)};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    next_steps: Calc.saved(EvaluatorStep.status),
    selected_id: Calc.saved(option(Id.t)),
    selected_exp: Calc.saved(option(Exp.t)),
    rewrites: Calc.saved(option(rewrites)),
    open_box,
  };

  let init = {
    next_steps: Calc.Pending,
    selected_id: Calc.Pending,
    selected_exp: Calc.Pending,
    rewrites: Calc.Pending,
    open_box: NoneOpen,
  };
  let get_selected_exp = (m: t): Exp.t =>
    m.selected_exp
    |> Calc.saved_to_option
    |> Option.join
    |> OptUtil.get(() => EmptyHole |> Exp.fresh);
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | ToggleAxioms
    | ProposeRewrite
    | RewriteEditorAction(CodeEditable.Update.t);

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    switch (action) {
    | ToggleAxioms =>
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
    | ProposeRewrite =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | AxiomsOpen =>
          Model.RewritesOpen(
            CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
          )
        | RewritesOpen(_) => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet;
    | RewriteEditorAction(action) =>
      switch (model.open_box) {
      | RewritesOpen(editor) =>
        open Updated;
        let* new_editor =
          CodeEditable.Update.update(~settings, action, editor);
        Model.{
          ...model,
          open_box: Model.RewritesOpen(new_editor),
        };
      | _ => model |> Updated.return_quiet
      }
    };
  };

  let calculate =
      (
        ~settings as _,
        exp,
        _state,
        new_next_steps,
        {next_steps: _, rewrites, selected_exp, selected_id, open_box}: Model.t,
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
    {
      next_steps: new_next_steps |> Calc.save,
      rewrites: rewrites |> Calc.save,
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
    | (RewriteEditor(selection), RewritesOpen(editor)) =>
      let+ ci = CodeEditable.Selection.get_cursor_info(~selection, editor);
      Update.RewriteEditorAction(ci);
    | (RewriteEditor(_), _) => empty
    };
  };

  let handle_key_event = (~selection: t, ~event, ~model: Model.t) => {
    switch (selection, model.open_box) {
    | (RewriteEditor(selection), RewritesOpen(editor)) =>
      CodeEditable.Selection.handle_key_event(~selection, editor, event)
      |> Option.map(x => Update.RewriteEditorAction(x))
    | (RewriteEditor(_), _) => None
    };
  };
};

module View = {
  open OptUtil.Syntax;
  type event =
    | AddInduction
    | AddForall
    | HideStepper
    | AddAxiomStep(Exp.t, Exp.t)
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
    (unpacked_rewrites |> List.is_empty ? [] : [Web.Node.text("Rewrites:")])
    @ (
      List.map(
        (exp: Exp.t) =>
          [
            exp
            |> Haz3lcore.ExpToSegment.(
                 exp_to_segment(
                   ~settings=
                     Settings.of_core(~inline=false, globals.settings.core),
                 )
               )
            |> CodeViewable.view_segment(
                 ~globals,
                 ~sort=Exp,
                 ~shape_map=Haz3lcore.Id.Map.empty,
               ),
            Widgets.button(Icons.star, _ =>
              signal(AddAxiomStep(Model.get_selected_exp(model), exp))
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
        model: Model.t,
      ) =>
    {
      let+ (left, right, top, bottom) =
        get_segment_bounds(
          ~measured=editor.editor.syntax.measured,
          editor.editor.state.zipper.selection.content,
        );

      let proof_button = (~callback: Ui_effect.t(unit), label: string) => {
        Web.Node.div(
          ~attrs=[
            Web.Attr.classes(["proof-button"]),
            Web.Attr.on_mousedown(_ =>
              Virtual_dom.Vdom.Effect.Stop_propagation
            ),
            Web.Attr.on_click(_ =>
              Bonsai.Effect.Many([
                callback,
                Virtual_dom.Vdom.Effect.Stop_propagation,
              ])
            ),
          ],
          [Web.Node.text(label)],
        );
      };

      // I want to make a bunch of buttons here:
      // Evaluate [TODO], Rewrite [TODO], Axioms, Cases,
      let buttons =
        Web.Node.div(
          ~attrs=[Web.Attr.classes(["proof-selection-buttons"])],
          [
            proof_button(~callback=Ui_effect.Ignore, "Evaluate"),
            proof_button(~callback=Ui_effect.Ignore, "Rewrite ▼"),
            proof_button(~callback=inject(ToggleAxioms), "Axioms ▼"),
            proof_button(~callback=signal(AddInduction), "Cases"),
          ],
        );

      [
        Web.Node.div(
          ~attrs=[
            Web.Attr.classes(["missing-step-overlay-align"]),
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
            Web.div_c(
              "proof-context-box",
              [buttons]
              @ {
                switch (model.open_box) {
                | NoneOpen => []
                | AxiomsOpen => [
                    Web.div_c(
                      "axiom-box",
                      view_rewrites(~globals, ~signal, model),
                    ),
                  ]
                | RewritesOpen(editor) => [
                    // one element list with a div
                    // with a list containing two elements
                    // an Editor for user to propose their rewrite
                    // a button to submit the rewrite
                    Web.div_c(
                      "rewrite-box",
                      [
                        CodeEditable.View.view(
                          ~globals,
                          ~signal=
                            fun
                            | MakeActive =>
                              signal(MakeActive(RewriteEditor())),
                          ~inject=x => inject(ProposeRewrite),
                          ~selected=false,
                          editor,
                        ),
                        Widgets.button(Icons.star, _ =>
                          inject(ProposeRewrite)
                        ),
                      ],
                    ),
                  ]
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
        ~signal,
        ~undo: option(Ui_effect.t(unit)),
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
    let button_induction =
      Widgets.button_d(
        Icons.star,
        signal(AddInduction),
        ~disabled=false,
        ~tooltip="Begin a proof by induction",
      );
    let button_forall =
      Widgets.button_d(
        Icons.star,
        signal(AddForall),
        ~disabled=false,
        ~tooltip="Prove a forall",
      );
    let button_hide_stepper =
      Widgets.toggle(~tooltip="Show Stepper", "s", true, _ =>
        signal(HideStepper)
      );
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
    Web.Node.div(
      ~attrs=[Web.Attr.classes(["stepper-controls"])],
      [
        button_back,
        button_induction,
        button_forall,
        eval_settings,
        toggle_show_history,
        button_hide_stepper,
      ],
    );
  };
};
