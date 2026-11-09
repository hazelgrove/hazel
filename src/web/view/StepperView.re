open Util;
open Language;

/* The stepper: a proof drawn as a chain of rows, one per step, each
 * showing the goal the step starts from and the step's justification.
 *
 * The rows are a projection of the proof syntax and the checker's
 * ProofMap (StepperRows), walked again on every render. A row changes
 * the proof only by patching its syntax (StepperPicker.Patches); the
 * next check gives the next rows. The only state is each row's own UI
 * state: its goal's editor, which holds the selection, and the picker
 * box it has open. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Rows = StepperRows;
module Picker = StepperPicker;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type box =
    | Closed
    /* The search typed into the Assumptions box. */
    | Axioms(string)
    /* The replacement typed into the Algebra box, and whether the
     * rewrite checker accepted it. */
    | Rewrite({
        editor: CodeEditable.Model.t,
        valid: option(bool),
      })
    | Facts(Picker.fact_form);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type local = {
    goal: Picker.Goal.t,
    box,
  };

  /* Each row's state, by its key (StepperRows.key). */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    incoming: Id.Map.t(local),
    outgoing: Id.Map.t(local),
  };

  let init = {
    incoming: Id.Map.empty,
    outgoing: Id.Map.empty,
  };

  let find = ((id, side): Rows.key, model: t): option(local) =>
    switch (side) {
    | Incoming => Id.Map.find_opt(id, model.incoming)
    | Outgoing => Id.Map.find_opt(id, model.outgoing)
    };

  let add = ((id, side): Rows.key, local: local, model: t): t =>
    switch (side) {
    | Incoming => {
        ...model,
        incoming: Id.Map.add(id, local, model.incoming),
      }
    | Outgoing => {
        ...model,
        outgoing: Id.Map.add(id, local, model.outgoing),
      }
    };
};

/* A row's actions. The stepper's other kind of action, a patch to the
 * proof, goes to whoever owns the proof text (`~edit_syntax`). */
module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Goal(CodeSelectable.Update.t)
    | Toggle(Picker.box)
    | Filter(string)
    | RewriteEditor(CodeEditable.Update.t)
    | Checked(bool);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Rows.key, action);

  let replacement = (editor: CodeEditable.Model.t): Exp.t =>
    CodeEditable.Model.get_statics(editor).elaborated;

  let update =
      (~settings: Settings.t, (key, action): t, model: Model.t)
      : Updated.t(Model.t) =>
    switch (Model.find(key, model)) {
    | None => model |> Updated.raise_invalid_action
    | Some(local) =>
      open Updated;
      let set = (local: Model.local) => Model.add(key, local, model);
      /* The replacement's statics, which give its elaborated form. */
      let calculate_rewrite =
        CodeEditable.Update.calculate(
          ~settings=settings.core,
          ~is_edited=true,
          ~is_dynamic_term=true,
          ~dynamics=Dynamics.Map.empty,
          ~stitch=x => x,
          ~ctx=SemanticCtx.get_ctx(local.goal.ctx),
        );
      switch (action, local.box) {
      | (Goal(a), _) =>
        let* editor =
          CodeSelectable.Update.update(~settings, a, local.goal.editor);
        set({
          ...local,
          goal: {
            ...local.goal,
            editor,
          },
        });
      /* The button that opened a box closes it. */
      | (Toggle(box), _) =>
        let box: Model.box =
          switch (box, local.box) {
          | (Axioms, Axioms(_))
          | (Rewrite, Rewrite(_)) => Closed
          | (Facts(f), Facts(open_f)) when f == open_f => Closed
          | (Axioms, _) => Axioms("")
          | (Rewrite, _) =>
            Rewrite({
              editor:
                CodeEditable.Model.mk(
                  Haz3lcore.Editor.Model.mk(
                    Haz3lcore.Zipper.init(),
                    ~root=Exp,
                  ),
                )
                |> calculate_rewrite,
              valid: None,
            })
          | (Facts(f), _) => Facts(f)
          };
        set({
          ...local,
          box,
        })
        |> return_quiet(~logged=true);
      | (Filter(filter), Axioms(_)) =>
        set({
          ...local,
          box: Axioms(filter),
        })
        |> return_quiet
      /* A changed replacement has to be checked again. */
      | (RewriteEditor(a), Rewrite({editor: old, valid})) =>
        let* editor = CodeEditable.Update.update(~settings, a, old);
        let editor = calculate_rewrite(editor);
        let unchanged =
          Exp.fast_equal_with_lexemes(
            replacement(old),
            replacement(editor),
          );
        set({
          ...local,
          box:
            Rewrite({
              editor,
              valid: unchanged ? valid : None,
            }),
        });
      | (Checked(valid), Rewrite(r)) =>
        set({
          ...local,
          box:
            Rewrite({
              ...r,
              valid: Some(valid),
            }),
        })
        |> return_quiet(~logged=true)
      | (Filter(_) | RewriteEditor(_) | Checked(_), _) =>
        model |> raise_invalid_action
      };
    };

  /* The state for `rows`, rebuilt from the walk: a row carries over its
   * last state, preparing its goal again only if the goal changed, and a
   * row the walk doesn't reach is dropped with its state. A row with no
   * goal has nothing to pick on, so no state. */
  let calculate =
      (~settings: CoreSettings.t, rows: list(Rows.t), model: Model.t)
      : Model.t =>
    List.fold_left(
      (acc, row: Rows.t) =>
        switch (row.goal, Rows.ctx(row)) {
        | (Some(written), Some(ctx)) =>
          let goal = () => Picker.Goal.mk(~settings, ~ctx, written);
          let local: Model.local =
            switch (Model.find(row.key, model)) {
            | Some(local) when Picker.Goal.is_for(local.goal, ~ctx, written) => {
                ...local,
                goal: {
                  ...local.goal,
                  ctx,
                },
              }
            | Some(local) => {
                ...local,
                goal: goal(),
              }
            | None => {
                goal: goal(),
                box: Closed,
              }
            };
          Model.add(row.key, local, acc);
        | (None, _)
        | (_, None) => acc
        },
      Model.init,
      Rows.all(rows),
    );
};

module Focus = {
  /* Which sub-editor of a step: a form's argument, an induction's
   * scrutinee, or its i-th case's pattern. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type slot =
    | Arg
    | Scrut
    | Pattern(int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Goal(Rows.key)
    /* The input of the box a row has open. */
    | Box(Rows.key)
    /* A sub-editor onto the proof text (SubEditor), by its step's id. Its
     * caret is the backing editor's, so this only says which one has
     * focus. */
    | Splice(Id.t, slot);

  open Cursor;

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~focus: t, model: Model.t)
      : cursor(Update.t) =>
    switch (focus) {
    | Goal(key) =>
      switch (Model.find(key, model)) {
      | Some(local) =>
        let+ ci =
          CodeSelectable.Selection.get_cursor_info(
            ~inject=a => inject((key, Goal(a))),
            ~selection=(),
            local.goal.editor,
          );
        (key, Update.Goal(ci));
      | None => empty
      }
    | Box(key) =>
      switch (Model.find(key, model)) {
      | Some({box: Rewrite({editor, _}), _}) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(
            ~inject=a => inject((key, RewriteEditor(a))),
            ~selection=(),
            editor,
          );
        (key, Update.RewriteEditor(ci));
      | Some(_)
      | None => empty
      }
    | Splice(_) => empty
    };
};

module View = {
  open WebUtil;

  type event =
    | MakeActive(Focus.t)
    | HideStepper;

  /* One row's chrome: the goal it starts from, then the transition to the
   * next row, which holds the justification and a delete button.
   *
   * The same syntax always takes the same space, so status never moves the
   * proof (no scroll jumps). It shows only as a class on the row, which
   * draws an indicator along the row's right edge (red broken, tan
   * incomplete, dotted light red blocked), and as a popup above the
   * justification, hidden until the justification is clicked, holding the
   * message, any detail and the repair buttons. Both are out of the flow. */
  module Row = {
    let view =
        (
          ~globals: Globals.t,
          ~status: Rows.Status.t,
          /* Off for a row that shows the error of the step above instead
           * of a goal: hidden rather than removed, so the text stays
           * aligned. */
          ~show_equiv: bool,
          ~delete: option(Ui_effect.t(unit)),
          /* Detail for the popup, e.g. an induction's missing pattern. */
          ~popup_extra: list(Node.t)=[],
          ~repair: list(Node.t)=[],
          ~editor_view: Node.t,
          ~justification: Node.t,
          ~content: list(Node.t)=[],
          (),
        ) => {
      let (row_classes, popup_body) =
        switch (status) {
        | Clean => (["ok"], [])
        | Pending => (["pending"], [])
        | Incomplete => (["incomplete"], [])
        | Broken(mark) => (
            ["broken"],
            ProofMarkView.message(~globals, mark) @ popup_extra @ repair,
          )
        | Blocked => (
            ["blocked"],
            [Node.text("Waiting on the step above.")] @ popup_extra @ repair,
          )
        };
      /* The popup sits inside the focusable wrapper, so a click on a
       * repair button keeps focus within it (`:focus-within` holds) and
       * lands before the popup hides. */
      let justification =
        switch (popup_body) {
        | [] => div_c("step-justification-wrap", [justification])
        | _ =>
          Node.div(
            ~attrs=[
              Attr.classes(["step-justification-wrap", "has-popup"]),
              Attr.create("tabindex", "0"),
            ],
            [div_c("step-error-popup", popup_body), justification],
          )
        };
      let delete_button =
        switch (delete) {
        | Some(d) => [
            Widgets.button(
              ~clss=["step-delete"],
              ~tooltip="Delete this step",
              Icons.trash,
              _ =>
              d
            ),
          ]
        | None => []
        };
      let transition =
        div_c("step-transition", [justification] @ delete_button);
      /* A row with block content (induction cases, a forall's body) puts
       * its transition after the whole block. */
      let row_classes = row_classes @ (content == [] ? [] : ["has-content"]);
      [
        Node.div(
          ~attrs=[Attr.classes(["step-border"] @ row_classes)],
          [
            div_c(
              "step-display",
              [
                Node.div(
                  ~attrs=[
                    Attr.classes(
                      ["equiv"] @ (show_equiv ? [] : ["equiv-hidden"]),
                    ),
                  ],
                  [Node.text("≡")],
                ),
                div_c("step-output", [editor_view]),
                transition,
              ],
            ),
          ]
          @ content,
        ),
      ];
    };

    /* In place of the goal, when the step above broke: the checker passed
     * its goal through, so printing it again would be noise. */
    let view_error_above = (~globals: Globals.t, mark: ProofMark.t) =>
      div_c("step-error-inline", ProofMarkView.message(~globals, mark));

    /* The repairs a broken step offers: removing it, and, for an axiom
     * that doesn't apply, trying the other direction. */
    let repairs =
        (
          ~edit_syntax: Picker.Patches.patch => Ui_effect.t(unit),
          status: Rows.Status.t,
          step: Proof.t,
        )
        : list(Node.t) => {
      let button = (~tooltip, label, patch) =>
        Widgets.button(~clss=["proof-button"], ~tooltip, Node.text(label), _ =>
          edit_syntax(patch)
        );
      switch (status) {
      | Broken(mark) =>
        let flip =
          switch (mark, Picker.Patches.flip_direction(step)) {
          | (RuleDoesNotApply(_), Some(patch)) => [
              button(
                ~tooltip="Apply the equality in the other direction",
                "Try other direction",
                patch,
              ),
            ]
          | _ => []
          };
        flip
        @ [
          button(
            ~tooltip="Replace this step with a hole",
            "Remove step",
            Picker.Patches.remove(step),
          ),
        ];
      | Clean
      | Incomplete
      | Blocked
      | Pending => []
      };
    };
  };

  /* The picker over a row's goal: its buttons for the selection, and the
   * box one of them opened, placed under the selection. */
  module Overlay = {
    open Haz3lcore;

    let segment_bounds = (~measured: Measured.t, segment: Segment.t) => {
      open OptUtil.Syntax;
      let* first_piece = ListUtil.hd_opt(segment);
      let Point.{row: start_y, col: start_x} =
        Measured.find_p(~msg="segment_bounds", first_piece, measured).origin;
      let* last_piece = ListUtil.last_opt(segment);
      let Point.{row: end_y, col: end_x} =
        Measured.find_p(~msg="segment_bounds", last_piece, measured).last;
      let rows = List.init(end_y - start_y + 1, i => start_y + i);
      let row = r => Measured.Rows.find(r, measured.rows);
      let left =
        List.fold_left(
          (acc, r) => Int.min(acc, row(r).content_start),
          start_x,
          rows,
        );
      let right =
        List.fold_left(
          (acc, r) => r == end_y ? acc : Int.max(acc, row(r).max_col),
          end_x,
          rows,
        );
      Some((left, right, start_y, end_y + 1));
    };

    let view =
        (
          ~globals: Globals.t,
          /* Writes a picked step into the proof. */
          ~write: TermBase.Proof.term => Ui_effect.t(unit),
          ~inject: Update.action => Ui_effect.t(unit),
          ~take_box_focus: Ui_effect.t(unit),
          ~box_focused: bool,
          local: Model.local,
        )
        : list(Node.t) => {
      let goal = local.goal;
      let selected = Picker.Goal.selected(goal);
      let selected_or_hole =
        selected |> Option.value(~default=Exp.fresh(EmptyHole));
      let env = Picker.Goal.env(goal);
      let info_map = Picker.Goal.info_map(goal);
      let code = (e: Exp.t) =>
        CodeViewable.view_any(
          ~globals,
          ~settings=
            ExpToSegment.Settings.of_core(
              ~inline=true,
              ~fold_fn_bodies=`Text,
              globals.settings.core,
            ),
          Exp(e),
        );
      /* Picking a step forces the record open, or the row just written
       * would not be on screen. */
      let write = term =>
        Ui_effect.Many([
          globals.inject_global(Set(Evaluation(ForceShowRecord))),
          write(term),
        ]);
      let index_of_selected = () =>
        Occurrence.index(selected_or_hole, goal.display);
      let axioms = filter =>
        div_c(
          "axiom-box",
          [
            Node.input(
              ~attrs=[
                Attr.value(filter),
                Attr.placeholder("search assumptions..."),
                Attr.on_focus(_ => take_box_focus),
                Attr.on_input((_, s) => inject(Filter(s))),
              ],
              (),
            ),
          ]
          @ List.map(
              (ctx_entry: ProofCtx.entry) => {
                let apply = (direction, equality) =>
                  write(
                    Picker.Terms.axiom_step(
                      ~at_idx=index_of_selected() |> Option.value(~default=0),
                      ~at_exp=selected_or_hole,
                      ~direction,
                      ~equality,
                    ),
                  );
                AssumptionBox.View.view(
                  ~globals,
                  ~info_map,
                  ~env,
                  ~active_selection=
                    Some((
                      selected_or_hole,
                      [],
                      fun
                      | AssumptionBox.EqualityLeft(e) => apply(Left, e)
                      | AssumptionBox.EqualityRight(e) => apply(Right, e),
                    )),
                  {ctx_entry: ctx_entry},
                );
              },
              Picker.rules(goal, ~filter, ~selected),
            ),
        );
      /* Replace the selection with an expression the user types, once the
       * rewrite checker accepts the two as equal. */
      let rewrite = (editor: CodeEditable.Model.t, valid: option(bool)) => {
        let replacement =
          Substitution.in_exp(env, Update.replacement(editor));
        let selection = Substitution.in_exp(env, selected_or_hole);
        let check_or_replace =
          switch (valid) {
          | Some(true) => [
              Node.text("Valid"),
              Widgets.button(
                ~clss=["proof-button"],
                Node.text("Replace"),
                ~tooltip="replace",
                _ =>
                switch (index_of_selected()) {
                | Some(at_idx) =>
                  write(
                    Picker.Terms.algebrite_step(
                      ~at_idx,
                      ~at_exp=selected_or_hole,
                      ~with_exp=replacement,
                    ),
                  )
                | None => Ui_effect.Ignore
                }
              ),
            ]
          | Some(false) => [Node.text("Invalid")]
          | None => [
              Widgets.button(
                ~clss=["proof-button"],
                Node.text("Check"),
                ~tooltip="check",
                _ =>
                inject(
                  Checked(
                    RewriteChecker.check_rewrite(selection, replacement),
                  ),
                )
              ),
            ]
          };
        div_c(
          "rewrite-box",
          [
            Node.text("Replace: "),
            code(selected_or_hole),
            Node.text("With: "),
            div_c(
              "inline-editor-wrapper",
              [
                CodeEditable.View.view(
                  ~globals,
                  ~signal=
                    fun
                    | MakeActive => take_box_focus,
                  ~edit_mode=
                    EditMode.Editable({
                      inject: a => inject(RewriteEditor(a)),
                      escape: _ => Ui_effect.Ignore,
                      take_focus: _ => Ui_effect.Ignore,
                      focus: box_focused ? Some() : None,
                    }),
                  ~dynamics=Dynamics.Map.empty,
                  editor,
                ),
              ],
            ),
          ]
          @ check_or_replace,
        );
      };
      let facts = (form: Picker.fact_form) => {
        let keyword = Picker.keyword(form);
        let pick = (entry: ProofCtx.entry) =>
          Widgets.button(
            ~clss=["proof-button"],
            ~tooltip=
              "Write `"
              ++ keyword
              ++ " "
              ++ entry.name
              ++ " =>` into the proof",
            Node.text(entry.name),
            _ =>
            write(Picker.pick_fact(form, entry.exp))
          );
        div_c(
          "proof-form-box",
          switch (Picker.facts(goal.ctx)) {
          | [] => [Node.text("No in-scope facts to " ++ keyword ++ ".")]
          | facts => [Node.text(keyword ++ " "), ...List.map(pick, facts)]
          },
        );
      };
      let box =
        switch (local.box) {
        | Closed => []
        | Axioms(filter) => [axioms(filter)]
        | Rewrite({editor, valid}) => [rewrite(editor, valid)]
        | Facts(form) => [facts(form)]
        };
      let button = ((label: string, action: Picker.action)) =>
        Node.div(
          ~attrs=[
            Attr.classes(["proof-button"]),
            Attr.on_pointerdown(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
            Attr.on_click(_ =>
              Ui_effect.Many([
                switch (action) {
                | Write(term) => write(term)
                | Open(box) => inject(Toggle(box))
                },
                Virtual_dom.Vdom.Effect.Stop_propagation,
              ])
            ),
          ],
          [Node.text(label)],
        );
      let editor = goal.editor.editor;
      switch (
        segment_bounds(
          ~measured=CachedSyntax.measured(editor.syntax),
          editor.state.zipper.selection.content,
        )
      ) {
      | None => []
      | Some((left, right, top, bottom)) => [
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
                [
                  div_c(
                    "proof-selection-buttons",
                    List.map(button, Picker.offers(goal, ~selected)),
                  ),
                ]
                @ box,
              ),
            ],
          ),
        ]
      };
    };
  };

  /* The row's justification: the step's keyword, or the cited equality's
   * name. */
  let justification = (step: Proof.t): string =>
    switch (step.term) {
    | AxiomStep({equality, _}) =>
      ProofSyntax.exp_to_name(equality) |> Option.value(~default="")
    | AlgebriteStep(_) => "algebra"
    | EvalStep(_) => "eval"
    | Contradiction(_) => "contradiction"
    | Forall(_) => "forall"
    | Funext(_) => "extensionality"
    | Assume(_) => "Assume"
    | Revert(_) => "Revert"
    | Generalize(_) => "Generalize"
    | Have(_) => "Have"
    | Alias(_) => "alias"
    | Induction(_) => "induction"
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_)
    | Parens(_) => ""
    };

  /* A form's argument and how its row labels it. */
  let arg = (step: Proof.t): option((string, Any.t)) =>
    switch (step.term) {
    | Funext(p, _) => Some(("argument: ", Pat(p)))
    | Assume(e, _, _) => Some(("Assume: ", Exp(e)))
    | Revert(e, _, _) => Some(("Revert: ", Exp(e)))
    | Generalize(e, _) => Some(("Generalize: ", Exp(e)))
    | Have(e, _, _) => Some(("Have: ", Exp(e)))
    | Forall(_)
    | Alias(_)
    | Induction(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_)
    | Contradiction(_)
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_)
    | Parens(_) => None
    };

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        ~is_toplevel: bool,
        /* Writes a patch into the proof text. */
        ~edit_syntax: Picker.Patches.patch => Ui_effect.t(unit),
        /* The editor holding the proof text, which the sub-editors splice
         * into. */
        ~backing: CodeEditable.Channel.t,
        /* The statics of the proof text. */
        ~info_map: Statics.Map.t,
        ~proof_map: ProofMap.t,
        proof: Proof.t,
        model: Model.t,
      )
      : list(Node.t) => {
    let take_focus = f => signal(MakeActive(f));
    let focused = f => selected == Some(f);
    let code = (any: Any.t) =>
      CodeViewable.view_any(
        ~globals,
        ~settings=
          Haz3lcore.ExpToSegment.Settings.of_core(
            ~inline=true,
            ~fold_fn_bodies=`Text,
            globals.settings.core,
          ),
        any,
      );
    /* Writes `term` where `row`'s picker writes. A form with an argument
     * then takes the caret into it: the backing editor's caret moves into
     * the new argument (a sub-editor takes no edits, and draws no caret,
     * while the caret is outside it), the new step's sub-editor takes
     * focus, and DOM focus follows after the render, as the click left it
     * on the button. */
    let write = (row: Rows.t, term: TermBase.Proof.term) => {
      let step = Proof.fresh(term);
      let into_arg =
        switch (Picker.Terms.arg_id(term)) {
        | Some(arg_id) => [
            backing.inject(Perform(Move(Goal(TileId(arg_id))))),
            take_focus(Splice(Proof.rep_id(step), Arg)),
            Ui_effect.of_sync_fun(
              () => Haz3lcore.FocusEffect.schedule_cell(),
              (),
            ),
          ]
        | None => []
        };
      Ui_effect.Many(
        [edit_syntax(Picker.Patches.write(row, step))] @ into_arg,
      );
    };
    /* A slice of the proof text, edited in place in the backing editor.
     * Read-only while the slice can't be found there (the statics lagging
     * an edit). */
    let splice = (~step: Proof.t, ~slot: Focus.slot, ~target, fallback: Any.t) => {
      let focus = Focus.Splice(Proof.rep_id(step), slot);
      div_c(
        "inline-editor-wrapper",
        [
          switch (SubEditor.mk(backing.model.editor, ~target)) {
          | Some(sub) =>
            CodeEditable.View.view_sub(
              ~globals,
              ~channel=backing,
              ~take_focus=take_focus(focus),
              ~focused=focused(focus),
              sub,
            )
          | None => div_c("proof-form-arg-static", [code(fallback)])
          },
        ],
      );
    };
    /* The goal's editor, with the picker over its selection. Step rows
     * draw no passive hints (step underlines, reflexivity markers): those
     * belong to the rows where the proof continues. */
    let goal_editor = (~hints: bool, row: Rows.t, local: Model.local) => {
      let goal = local.goal;
      let selected_id = Option.map(Exp.rep_id, Picker.Goal.selected(goal));
      let write_opt = term =>
        term
        |> Option.map(write(row))
        |> Option.value(~default=Ui_effect.Ignore);
      StepperEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => take_focus(Goal(row.key))
          | TakeStep(idx) => write_opt(Picker.take_step(goal, idx))
          | Refl(idx) =>
            write_opt(
              Option.bind(List.nth_opt(goal.refls, idx), Picker.refl(goal)),
            ),
        ~inject=a => inject((row.key, Goal(a))),
        ~selected=focused(Goal(row.key)),
        ~selected_id,
        ~overlays=
          Overlay.view(
            ~globals,
            ~write=write(row),
            ~inject=a => inject((row.key, a)),
            ~take_box_focus=take_focus(Box(row.key)),
            ~box_focused=focused(Box(row.key)),
            local,
          ),
        {
          editor: goal.editor,
          next_steps: hints ? Picker.Goal.step_ids(goal) : [],
          refls: hints ? List.map(Exp.rep_id, goal.refls) : [],
        },
      );
    };
    let goal_view = (~hints, ~prev_broken, row: Rows.t) =>
      switch (prev_broken, Model.find(row.key, model)) {
      | (Some(mark), _) => Row.view_error_above(~globals, mark)
      | (None, Some(local)) => goal_editor(~hints, row, local)
      | (None, None) => div_c("step-placeholder", [Node.text("—")])
      };
    let controls = (~top) =>
      top && is_toplevel
        ? [
          Widgets.button(Icons.gear, _ =>
            globals.inject_global(Set(Evaluation(ShowSettings)))
          ),
          Widgets.toggle(
            ~tooltip="Show History",
            "h",
            globals.settings.core.evaluation.stepper_history,
            _ =>
            globals.inject_global(Set(Evaluation(ShowRecord)))
          ),
          Widgets.toggle(~tooltip="Show Stepper", "s", true, _ =>
            signal(HideStepper)
          ),
        ]
        : [];
    let rec chain = (~top=false, rows: list(Rows.t)): list(Node.t) => {
      let last = ListUtil.last_opt(rows) |> Option.map((r: Rows.t) => r.key);
      let (nodes, _) =
        List.fold_left(
          ((nodes, prev_broken), row: Rows.t) => {
            let status = Rows.Status.of_row(row);
            let node =
              view_row(
                ~top,
                ~prev_broken,
                ~status,
                ~only=List.length(rows) == 1,
                ~is_last=last == Some(row.key),
                row,
              );
            (nodes @ node, Rows.Status.broken_mark(status));
          },
          ([], None),
          rows,
        );
      [Node.div(~attrs=[Attr.classes(["stepper", "cell-result"])], nodes)];
    }
    and view_row =
        (~top, ~prev_broken, ~status, ~only, ~is_last, row: Rows.t)
        : list(Node.t) =>
      switch (row.kind) {
      /* With history off, only the rows where the proof continues show. */
      | Step(_) when !globals.settings.core.evaluation.stepper_history => []
      | Step(nested) =>
        let popup_extra =
          switch (Statics.Map.lookup(Proof.rep_id(row.step), info_map)) {
          | Some(info) =>
            List.concat_map(
              fun
              | Mark.InexhaustiveMatch(_, _, example) =>
                ProofMarkView.inexhaustive_message(~globals, example)
              | _ => [],
              Info.marks_of(info),
            )
          | None => []
          };
        Row.view(
          ~globals,
          ~status,
          ~show_equiv=prev_broken == None,
          ~delete=Some(edit_syntax(Picker.Patches.remove(row.step))),
          ~popup_extra,
          ~repair=Row.repairs(~edit_syntax, status, row.step),
          ~editor_view=goal_view(~hints=false, ~prev_broken, row),
          ~justification=Node.text(justification(row.step)),
          ~content=content(row, nested),
          (),
        );
      | Hole
      | End =>
        /* A `?` marks a hole written mid-proof, a step to come back for.
         * A row at the end needs no mark: the picker already says the
         * proof continues here. */
        let unknown =
          row.kind == Hole && !is_last
            ? [div_c("step-unknown", [Node.text("?")])] : [];
        let justification =
          div_c(
            "missing-step-justification",
            unknown
            @ [
              Node.div(
                ~attrs=[Attr.classes(["stepper-controls"])],
                controls(~top),
              ),
            ],
          );
        /* Splices the written `?`, and its `;`, out of the proof. A hole
         * that is the whole chain is left alone. */
        let delete =
          row.kind == Hole && !only
            ? Some(edit_syntax(Picker.Patches.remove(row.step))) : None;
        switch (prev_broken, Model.find(row.key, model)) {
        | (Some(_), _)
        | (None, Some(_)) =>
          Row.view(
            ~globals,
            ~status,
            ~show_equiv=prev_broken == None,
            ~delete,
            ~editor_view=goal_view(~hints=true, ~prev_broken, row),
            ~justification,
            (),
          )
        | (None, None) => [justification]
        };
      }
    /* What a step row holds below its goal: a form's argument and body,
     * an induction's cases, a have's premises. */
    and content = (row: Rows.t, nested: Rows.nested): list(Node.t) => {
      let step = row.step;
      let form_arg =
        switch (arg(step)) {
        | Some((label, fallback)) => [
            div_c(
              "proof-form-arg",
              [
                Node.text(label),
                splice(
                  ~step,
                  ~slot=Arg,
                  ~target=
                    SubEditor.Target.child(~anchor=Proof.rep_id(step), 0),
                  fallback,
                ),
              ],
            ),
          ]
        | None => []
        };
      switch (nested, step.term) {
      | (Leaf, _) => []
      | (Body(rows), _) => form_arg @ chain(rows)
      | (Cases(case_rows), Induction(scrut, _, cases)) =>
        induction(row, scrut, cases, case_rows)
      | (Premises({sub, body}), Have(e, _, _)) =>
        let premise = (~header, rows) =>
          div_c(
            "induction-case",
            [div_c("induction-case-header", header)] @ rows,
          );
        let body_goal =
          switch (body) {
          | [{goal: Some(g), _}, ..._] => g
          | _ => row.goal |> Option.value(~default=Exp.fresh(EmptyHole))
          };
        form_arg
        @ [
          premise(
            ~header=[Node.text("proof of "), code(Exp(e))],
            target_box(sub),
          ),
          /* The body's own trailing row is its target row. */
          premise(
            ~header=[
              Node.text("with `have`, proof of "),
              code(Exp(body_goal)),
            ],
            chain(body),
          ),
        ];
      | (Cases(_) | Premises(_), _) => []
      };
    }
    and induction = (row: Rows.t, scrut: Exp.t, cases, case_rows) => {
      let step = row.step;
      let anchor = Proof.rep_id(step);
      let set_cases = cases =>
        switch (Picker.Patches.set_cases(step, cases)) {
        | Some(patch) => edit_syntax(patch)
        | None => Ui_effect.Ignore
        };
      /* The scrutinee spans the induction tile's first child, up to its
       * first `| pat => body` rule, if any. */
      let scrut_view =
        splice(
          ~step,
          ~slot=Scrut,
          ~target=
            SubEditor.Target.(
              child(~anchor, 0)
              |> until(BeforeOrEnd(NthTile(["|", "=>"], 0)))
            ),
          Exp(scrut),
        );
      /* The facts a case adds to the induction's scope: its pattern's
       * `where` facts, the case equation and the inductive hypotheses. */
      let outer_facts =
        Rows.ctx(row)
        |> Option.map(SemanticCtx.facts)
        |> Option.value(~default=[])
        |> List.map(((_, id, _)) => id);
      let hypotheses = (rows: list(Rows.t)) =>
        switch (rows) {
        | [first, ..._] =>
          Rows.ctx(first)
          |> Option.map(SemanticCtx.facts)
          |> Option.value(~default=[])
          |> List.filter(((_, id, _)) => !List.mem(id, outer_facts))
          |> List.rev_map(((_, _, fact)) =>
               div_c(
                 "induction-case-hypothesis",
                 [
                   Node.text("assume "),
                   code(
                     Exp(
                       ProofRule.conclusion_exp(ProofRule.exp_to_rule(fact)),
                     ),
                   ),
                 ],
               )
             )
        | [] => []
        };
      let case = (i, (pat: Pat.t, rows)) => {
        let remove_case =
          Widgets.button(
            Icons.trash,
            _ =>
              switch (ListUtil.remove_nth(i, cases)) {
              | Some(cases) => set_cases(cases)
              | None => Ui_effect.Ignore
              },
            ~tooltip="Remove case",
            ~clss=["subtle-button"],
          );
        let pattern =
          splice(
            ~step,
            ~slot=Pattern(i),
            ~target=
              SubEditor.Target.(
                child(~anchor, 0)
                |> descend(NthTile(["|", "=>"], i), ~child=0)
              ),
            Pat(pat),
          );
        div_c(
          "induction-case",
          [
            /* Hypotheses share the header line: pattern on the left,
             * "assume …"s off to the right after a fixed gap. */
            div_c(
              "induction-case-header",
              [
                remove_case,
                Node.text("Case "),
                pattern,
                Node.text(" : "),
                div_c("induction-case-hypotheses", hypotheses(rows)),
              ],
            ),
          ]
          @ target_box(rows),
        );
      };
      let add_case =
        Widgets.button(
          Node.text("Case ..."),
          ~tooltip="Add case",
          ~clss=["subtle-button", "add-case-button"],
          _ =>
          set_cases(
            cases @ [(Pat.fresh(EmptyHole), Proof.fresh(EmptyHole))],
          )
        );
      [div_c("induction-scrut", [Node.text("induction on: "), scrut_view])]
      @ List.mapi(case, case_rows)
      @ [add_case];
    }
    /* A premise's rows, then, until they reach `true`, the gap left and
     * the `true` they have to reach: a quiet row shaped like a step row,
     * so the chain reads evenly. */
    and target_box = (rows: list(Rows.t)): list(Node.t) => {
      let target = Exp.fresh(Atom(Bool(true)));
      let reached =
        switch (Rows.reached(rows)) {
        | Some(e) => Exp.fast_equal(e, target)
        | None => false
        };
      let quiet_row = (~clss, ~equiv, content) =>
        Node.div(
          ~attrs=[Attr.classes(["stepper", "cell-result", clss])],
          [
            div_c(
              "step-border",
              [div_c("step-display", [equiv] @ content)],
            ),
          ],
        );
      chain(rows)
      @ (
        reached
          ? []
          : [
            quiet_row(
              ~clss="step-ellipsis",
              ~equiv=
                Node.div(
                  ~attrs=[Attr.classes(["equiv", "equiv-hidden"])],
                  [Node.text("≡")],
                ),
              [div_c("step-output", [Node.text("⋯")])],
            ),
            quiet_row(
              ~clss="target-step",
              ~equiv=div_c("equiv", [Node.text("≡?")]),
              [
                div_c("step-output", [code(Exp(target))]),
                Node.text("target"),
              ],
            ),
          ]
      );
    };
    let settings_modal =
      is_toplevel && globals.settings.core.evaluation.show_settings
        ? SettingsModal.view(
            ~inject=u => globals.inject_global(Set(u)),
            globals.settings.core.evaluation,
          )
        : [];
    settings_modal @ chain(~top=true, Rows.chain(~proof_map, proof));
  };
};
