open Util;
open Language;

/* The stepper: a proof drawn as a chain of rows, one per step, each
 * showing the goal the step starts from and the step's justification.
 * Where a chain is unfinished, its last row holds the write box, where the
 * user writes the next step.
 *
 * The rows are a projection of the proof syntax and the checker's
 * ProofMap (StepperRows), walked again on every render. A row changes
 * the proof only by patching its syntax (StepperPicker.Patches); the
 * next check gives the next rows. The only state is each row's own UI
 * state: its goal's editor, which holds the selection, and its write
 * box. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Rows = StepperRows;
module Picker = StepperPicker;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type tab =
    | Write
    | Step
    | Strategy;

  /* The write box at the end of an unfinished chain. Its text stays while
   * the row exists, and goes with it. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type box = {
    /* None until the user picks a tab: then the stepper's default. */
    tab: option(tab),
    editor: CodeEditable.Model.t,
    /* What Tab takes, for the goal and its selection. */
    ghost: option(Picker.Ghost.t),
    /* The search for steps to the typed goal, while the box holds a
     * complete expression. */
    search: option(StepSearch.t),
    /* The rules' uses listed under the box, while it holds text. */
    suggestions: option(StepSearch.Suggestions.t),
    /* The listed item chosen with the arrow keys. */
    chosen: option(int),
    /* Counts the searches started and the slices run, so that each
     * render of a running search asks for its next slice once. */
    generation: int,
    /* The strategy whose menu is open, by its label. */
    menu: option(string),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type local = {
    goal: Picker.Goal.t,
    /* For a row where its chain continues. */
    box: option(box),
    /* The side conditions shown open, by index into the row's
     * `StepperRows.side_conditions`. */
    expanded: list(int),
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

  /* Whether the box's search or suggestions have work left. */
  let running = (box: box): bool =>
    switch (box.search, box.suggestions) {
    | (Some({status: Searching, _}), _) => true
    | (_, Some(suggestions)) => !StepSearch.Suggestions.is_done(suggestions)
    | (_, None) => false
    };

  /* Whether nothing is typed in the box. */
  let is_empty = (box: box): bool =>
    switch (Exp.term_of(CodeEditable.Model.get_statics(box.editor).term)) {
    | EmptyHole => true
    | _ => false
    };
};

/* A row's actions. The stepper's other kind of action, a patch to the
 * proof, goes to whoever owns the proof text (`~edit_syntax`). */
module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Goal(CodeSelectable.Update.t)
    | Tab(Model.tab)
    | BoxEditor(CodeEditable.Update.t)
    /* Runs a slice of the box's search, if it is still the search of
     * that generation. */
    | SearchSlice(int)
    /* An item listed under the box, by index, or none. */
    | Choose(option(int))
    /* Replaces the box's text with a name it completes to. */
    | Complete(string)
    | Menu(option(string))
    | ToggleCondition(int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Rows.key, action);

  /* The box's text is an expression in the goal's scope. */
  let calculate_box = (~settings, goal: Picker.Goal.t, editor) =>
    CodeEditable.Update.calculate(
      ~settings,
      ~is_edited=true,
      ~is_dynamic_term=true,
      ~dynamics=Dynamics.Map.empty,
      ~stitch=x => x,
      ~ctx=SemanticCtx.get_ctx(goal.ctx),
      editor,
    );

  let ghost = (goal: Picker.Goal.t) =>
    Picker.Ghost.mk(goal, ~selected=Picker.Goal.selected(goal));

  /* The goal typed into the box, or None while it is empty or
   * incomplete. */
  let wanted = (editor: CodeEditable.Model.t): option(Exp.t) => {
    let statics = CodeEditable.Model.get_statics(editor);
    let rec complete = (e: Exp.t) =>
      switch (Exp.term_of(e)) {
      | EmptyHole
      | MultiHole(_)
      | Invalid(_) => false
      | _ => List.for_all(complete, Occurrence.children(e))
      };
    statics.error_ids == [] && complete(statics.term)
      ? Some(statics.elaborated) : None;
  };

  /* The box's text, if it is a single name. */
  let typed_name = (editor: CodeEditable.Model.t): option(string) =>
    switch (Exp.term_of(CodeEditable.Model.get_statics(editor).term)) {
    | Var(x) => Some(x)
    | _ => None
    };

  /* The part of the goal selected, when it is less than all of it. */
  let selected_part = (goal: Picker.Goal.t): option(Exp.t) =>
    switch (Picker.Goal.selected(goal)) {
    | Some(part) when Exp.rep_id(part) != Exp.rep_id(goal.written) =>
      Some(part)
    | Some(_)
    | None => None
    };

  /* What the box's text asks for against `goal`: a search for it as a
   * goal and the uses of every rule, or, when it names a rule, that
   * rule's uses alone. With part of the goal selected, the text is that
   * part's replacement, and the uses are those inside it. Suggestions
   * already listed for the same rules and part are kept, unless the goal
   * is new. */
  let search =
      (~new_goal=false, goal: Picker.Goal.t, box: Model.box): Model.box => {
    let part = selected_part(goal);
    let wanted =
      switch (part) {
      | Some(part) =>
        Option.map(
          typed =>
            Exp.map_term(
              ~f_exp=
                (cont, e) =>
                  Exp.rep_id(e) == Exp.rep_id(part) ? typed : cont(e),
              goal.written,
            ),
          wanted(box.editor),
        )
      | None => wanted(box.editor)
      };
    let at = Option.map(Exp.rep_id, part);
    let rule =
      switch (typed_name(box.editor)) {
      | Some(name) when List.mem_assoc(name, StepSearch.names(goal)) =>
        Some(name)
      | Some(_)
      | None => None
      };
    {
      ...box,
      search:
        rule == None
          ? Option.map(wanted => StepSearch.start(goal, ~wanted), wanted)
          : None,
      suggestions:
        switch (box.suggestions) {
        | _ when Model.is_empty(box) => None
        | Some(suggestions)
            when suggestions.rule == rule && suggestions.at == at && !new_goal =>
          Some(suggestions)
        | Some(_)
        | None => Some(StepSearch.Suggestions.start(~rule, ~at))
        },
      chosen: None,
      generation: box.generation + 1,
    };
  };

  /* How long a slice of the search may run before the browser gets its
   * turn. A slice ends only between candidates, so it can run over by
   * one check. */
  let slice_ms = 15.;

  let new_box = (~settings, goal: Picker.Goal.t): Model.box => {
    tab: None,
    editor:
      CodeEditable.Model.mk(
        Haz3lcore.Editor.Model.mk(Haz3lcore.Zipper.init(), ~root=Exp),
      )
      |> calculate_box(~settings, goal),
    ghost: ghost(goal),
    search: None,
    suggestions: None,
    chosen: None,
    generation: 0,
    menu: None,
  };

  let update =
      (~settings: Settings.t, (key, action): t, model: Model.t)
      : Updated.t(Model.t) =>
    switch (Model.find(key, model)) {
    | None => model |> Updated.raise_invalid_action
    | Some(local) =>
      open Updated;
      let set = (local: Model.local) => Model.add(key, local, model);
      let set_box = (box: Model.box) =>
        set({
          ...local,
          box: Some(box),
        });
      switch (action, local.box) {
      /* A changed selection changes what Tab takes, and what the box's
       * text replaces. */
      | (Goal(a), _) =>
        let* editor =
          CodeSelectable.Update.update(~settings, a, local.goal.editor);
        let goal = {
          ...local.goal,
          editor,
        };
        let part = goal => Option.map(Exp.rep_id, selected_part(goal));
        set({
          ...local,
          goal,
          box:
            Option.map(
              (box: Model.box) => {
                let box = {
                  ...box,
                  ghost: ghost(goal),
                };
                part(goal) == part(local.goal) ? box : search(goal, box);
              },
              local.box,
            ),
        });
      /* Step and Proof strategy act on the whole goal, so they drop a
       * selection made to write a part's replacement. */
      | (Tab(tab), Some(box)) =>
        let (goal, box) =
          switch (tab, selected_part(local.goal)) {
          | (Step | Strategy, Some(_)) =>
            let unselected =
              CodeSelectable.Update.update(
                ~settings,
                Unselect(None),
                local.goal.editor,
              );
            let goal = {
              ...local.goal,
              editor: unselected.model,
            };
            (
              goal,
              search(
                goal,
                {
                  ...box,
                  ghost: ghost(goal),
                },
              ),
            );
          | (Write | Step | Strategy, _) => (local.goal, box)
          };
        set({
          ...local,
          goal,
          box:
            Some({
              ...box,
              tab: Some(tab),
              menu: None,
            }),
        })
        |> return_quiet(~logged=true);
      /* A changed text is searched for afresh. */
      | (BoxEditor(a), Some(box)) =>
        let* editor = CodeEditable.Update.update(~settings, a, box.editor);
        let editor =
          calculate_box(~settings=settings.core, local.goal, editor);
        let changed =
          !
            Exp.fast_equal_with_lexemes(
              CodeEditable.Model.get_statics(box.editor).term,
              CodeEditable.Model.get_statics(editor).term,
            );
        let box = {
          ...box,
          editor,
        };
        /* An empty box showing a ghost keeps its caret before the hole,
         * where the ghost starts. */
        let box =
          Model.is_empty(box) && box.ghost != None
            ? {
              ...box,
              editor:
                CodeEditable.Update.update(
                  ~settings,
                  Perform(Move(Start)),
                  box.editor,
                ).
                  model,
            }
            : box;
        set_box(changed ? search(local.goal, box) : box);
      /* The search first, then the suggestions. */
      | (SearchSlice(generation), Some(box))
          when generation == box.generation && Model.running(box) =>
        let deadline = JsUtil.precise_timestamp() +. slice_ms;
        let continue_ = () => JsUtil.precise_timestamp() < deadline;
        set_box({
          ...box,
          search:
            Option.map(
              (search: StepSearch.t) =>
                search.status == Searching
                  ? StepSearch.run(
                      ~continue_,
                      ~algebra=RewriteChecker.check_rewrite,
                      local.goal,
                      search,
                    )
                  : search,
              box.search,
            ),
          suggestions:
            Option.map(
              StepSearch.Suggestions.run(~continue_, local.goal),
              box.suggestions,
            ),
          generation: generation + 1,
        })
        |> return_quiet;
      | (SearchSlice(_), _) => model |> return_quiet
      | (Choose(chosen), Some(box)) =>
        set_box({
          ...box,
          chosen,
        })
        |> return_quiet(~logged=true)
      | (Complete(name), Some(box)) =>
        let editor =
          Haz3lcore.Parser.to_zipper(~root=Exp, name)
          |> Option.value(~default=Haz3lcore.Zipper.init())
          |> Haz3lcore.Editor.Model.mk(~root=Exp)
          |> CodeEditable.Model.mk
          |> calculate_box(~settings=settings.core, local.goal);
        set_box(
          search(
            local.goal,
            {
              ...box,
              editor,
            },
          ),
        )
        |> return(~scroll=IfCaretVisible);
      | (Menu(menu), Some(box)) =>
        set_box({
          ...box,
          menu,
        })
        |> return_quiet(~logged=true)
      | (ToggleCondition(i), _) =>
        set({
          ...local,
          expanded:
            List.mem(i, local.expanded)
              ? List.filter((!=)(i), local.expanded)
              : [i, ...local.expanded],
        })
        |> return_quiet(~logged=true)
      | (Tab(_) | BoxEditor(_) | Choose(_) | Complete(_) | Menu(_), None) =>
        model |> raise_invalid_action
      };
    };

  /* The state for `rows`, rebuilt from the walk: a row carries over its
   * last state, preparing its goal again only if the goal or the settings
   * changed, and a row the walk doesn't reach is dropped with its state.
   * A row with no goal has nothing to step, so no state. */
  let calculate =
      (~settings: CoreSettings.t, rows: list(Rows.t), model: Model.t)
      : Model.t =>
    List.fold_left(
      (acc, row: Rows.t) =>
        switch (row.goal, Rows.ctx(row)) {
        | (Some(written), Some(ctx)) =>
          /* A box carries its text over to its row's new goal, and
           * searches again from it. */
          let with_goal = (local: Model.local, goal): Model.local => {
            ...local,
            goal,
            box:
              Option.map(
                (box: Model.box) =>
                  search(
                    ~new_goal=true,
                    goal,
                    {
                      ...box,
                      editor: calculate_box(~settings, goal, box.editor),
                      ghost: ghost(goal),
                    },
                  ),
                local.box,
              ),
          };
          let local: Model.local =
            switch (Model.find(row.key, model)) {
            | Some(local) when Picker.Goal.is_for(local.goal, ~ctx, written) =>
              let goal = {
                ...local.goal,
                ctx,
              };
              CoreSettings.eq_ignoring_stepper_modals(goal.settings, settings)
                ? {
                  ...local,
                  goal,
                }
                : with_goal(local, Picker.Goal.rebuild(~settings, goal));
            | Some(local) =>
              with_goal(local, Picker.Goal.mk(~settings, ~ctx, written))
            | None =>
              let goal = Picker.Goal.mk(~settings, ~ctx, written);
              {
                goal,
                box: None,
                expanded: [],
              };
            };
          /* A row keeps its key when the steps after it change whether a
           * step can be written there. */
          let local =
            switch (row.continues, local.box) {
            | (true, None) => {
                ...local,
                box: Some(new_box(~settings, local.goal)),
              }
            | (false, Some(_)) => {
                ...local,
                box: None,
              }
            | (true, Some(_))
            | (false, None) => local
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
    /* A row's write box. */
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
      | Some({box: Some({editor, _}), _}) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(
            ~inject=a => inject((key, BoxEditor(a))),
            ~selection=(),
            editor,
          );
        (key, Update.BoxEditor(ci));
      | Some({box: None, _})
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

  let code = (~globals: Globals.t, any: Any.t) =>
    CodeViewable.view_any(
      ~globals,
      ~settings=ProofMarkView.code_view_settings(globals.settings.core),
      any,
    );

  /* One row's chrome: the goal it starts from, then the step to the next
   * row. A plain step is the line between the two rows, with its lozenge
   * (the justification and a delete button) on it. A step with goals of
   * its own, or with side conditions shown open, opens its line into a
   * block holding them, with the lozenge on the block's top edge.
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
          /* Collapsed side conditions, drawn left of the lozenge. */
          ~dots: list(Node.t)=[],
          /* The sections of the step's block, if it has one. */
          ~block: option(list(Node.t))=None,
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
        div_c("step-transition", dots @ [justification] @ delete_button);
      let display = transition_in_display =>
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
          ]
          @ transition_in_display,
        );
      switch (block) {
      | None => [
          Node.div(
            ~attrs=[Attr.classes(["step-border"] @ row_classes)],
            [display([transition])],
          ),
        ]
      | Some(sections) => [
          Node.div(
            ~attrs=[
              Attr.classes(["step-border", "has-block"] @ row_classes),
            ],
            [display([]), div_c("step-block", [transition] @ sections)],
          ),
        ]
      };
    };

    /* In place of the goal, when the step above broke: the checker passed
     * its goal through, so printing it again would be noise. */
    let view_error_above = (~globals: Globals.t, mark: ProofMark.t) =>
      div_c("step-error-inline", ProofMarkView.message(~globals, mark));

    /* A row drawn for reading only, with no step below it. */
    let quiet = (~clss, ~equiv, content) =>
      Node.div(
        ~attrs=[Attr.classes(["step-border", clss])],
        [div_c("step-display", [equiv] @ content)],
      );

    let equiv_hidden = () =>
      Node.div(
        ~attrs=[Attr.classes(["equiv", "equiv-hidden"])],
        [Node.text("≡")],
      );

    /* The `true` a chain aims at. */
    let target = (~globals) =>
      quiet(
        ~clss="target-step",
        ~equiv=div_c("equiv", [Node.text("≡?")]),
        [
          div_c(
            "step-output",
            [code(~globals, Exp(Exp.fresh(Atom(Bool(true)))))],
          ),
        ],
      );

    /* The repairs a broken step offers: removing it, and, for an axiom
     * that doesn't apply, trying the other direction. */
    let repairs =
        (
          ~edit_syntax: Picker.Patches.patch => Ui_effect.t(unit),
          ~info_map: Statics.Map.t,
          ~proof_map: ProofMap.t,
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
          switch (mark) {
          | RuleDoesNotApply(_) =>
            Picker.Patches.flip_direction(~info_map, ~proof_map, step)
            |> Option.map(
                 button(
                   ~tooltip="Apply the equality in the other direction",
                   "Try other direction",
                 ),
               )
            |> Option.to_list
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

  /* A side condition: a circle on a line, which opens into a section
   * holding a read-only stepper from its goal to `true`. A closed one
   * shows how the checker closed it; a pending one's `?` row holds the
   * exits that discharge it. A step's circles sit on its line, a
   * theorem's contracts' on its header. */
  module Condition = {
    /* The circle's icon and the section's label. */
    let kind = (kind: Obligation.kind): (string, string) =>
      switch (kind) {
      | Definedness => ("÷", "definedness")
      | Contract => ("ƒ", "contract")
      | Premise => ("⇒", "premise")
      | Assumption => ("⊢", "assumption")
      };

    let dot = (~toggle: Ui_effect.t(unit), ob: Obligation.t) => {
      let (icon, label) = kind(ob.kind);
      let unproven = Obligation.is_pending(ob);
      Node.div(
        ~attrs=[
          Attr.classes(["condition-dot"] @ (unproven ? ["unproven"] : [])),
          Attr.title(
            "Side condition, "
            ++ label
            ++ (unproven ? ", unproven" : "")
            ++ ". Click to show it.",
          ),
          Attr.on_click(_ => toggle),
        ],
        [Node.text(icon)],
      );
    };

    let view =
        (
          ~globals: Globals.t,
          ~edit_syntax: Picker.Patches.patch => Ui_effect.t(unit),
          ~exits: list(Obligations.Exits.t),
          ~toggle: Ui_effect.t(unit),
          ob: Obligation.t,
        ) => {
      let (_, label) = kind(ob.kind);
      let code = code(~globals);
      let goal = ob.goal;
      let truth = Exp.fresh(Atom(Bool(true)));
      /* Closed by evaluating it or citing a fact, so the step acts on all
       * of the goal. */
      let closed = justification =>
        Row.view(
          ~globals,
          ~status=Clean,
          ~show_equiv=true,
          ~delete=None,
          ~editor_view=div_c("step-acts-on-whole", [code(Exp(goal))]),
          ~justification=Node.text(justification),
          (),
        )
        @ [
          Row.quiet(
            ~clss="condition-closed",
            ~equiv=div_c("equiv", [Node.text("≡")]),
            [div_c("step-output", [code(Exp(truth))])],
          ),
        ];
      let exit = (e: Obligations.Exits.t) =>
        Node.button(
          ~attrs=
            (
              switch (e.patch) {
              | Some(patch) => [
                  Attr.classes(["condition-exit"]),
                  Attr.on_click(_ => edit_syntax(patch)),
                ]
              | None => [
                  Attr.classes(["condition-exit", "disabled"]),
                  Attr.disabled,
                ]
              }
            )
            @ [Attr.title(e.title)],
          [Node.text(e.label)],
        );
      let rows =
        switch (ob.discharge, Obligation.remote_fact(ob)) {
        | (Remote(_), Some((name, _))) => closed(name)
        | (Remote(_), None) => closed("a fact in scope")
        | (Evaluated, _) => closed("by evaluation")
        /* TODO: a `?` here holds a write box once a side condition can be
         * proved in place (`with <condition> by <proof>`). */
        | (Pending, _) => [
            Row.quiet(
              ~clss="condition-goal",
              ~equiv=div_c("equiv", [Node.text("≡")]),
              [div_c("step-output", [code(Exp(goal))])],
            ),
            Row.quiet(
              ~clss="condition-missing",
              ~equiv=Row.equiv_hidden(),
              [
                div_c("step-unknown", [Node.text("?")]),
                switch (exits) {
                | [] =>
                  div_c("condition-hint", [Node.text("Not proven yet.")])
                | _ => div_c("condition-exits", List.map(exit, exits))
                },
              ],
            ),
            Row.target(~globals),
          ]
        };
      div_c(
        "step-section condition",
        [
          Node.div(
            ~attrs=[
              Attr.classes(["step-section-bar"]),
              Attr.title("Click to collapse"),
              Attr.on_click(_ => toggle),
            ],
            [Node.text(label)],
          ),
          div_c("step-slot", [div_c("stepper cell-result", rows)]),
        ],
      );
    };
  };

  /* Runs the write box's search a slice at a time. Each render of a new
   * generation of a running search asks for one more slice, after the
   * browser has had its turn, so typing never waits on the search. A new
   * search waits a moment first, in case the user is still typing. */
  module SearchSlices = {
    module Spec = {
      module State = Unit;
      module Input = {
        type t = {
          generation: int,
          fresh: bool,
          slice: Ui_effect.t(unit),
        };
        let sexp_of_t = (input: t) =>
          Sexplib0.Sexp.Atom(string_of_int(input.generation));
        let combine = (_, latest) => latest;
      };
      let ask = (input: Input.t) =>
        JsUtil.delay(input.fresh ? 150. : 0., () =>
          Virtual_dom.Vdom.Effect.Expert.handle_non_dom_event_exn(input.slice)
        );
      let init = (_, _) => ();
      let on_mount = (input, (), _) => ask(input);
      let update = (~old_input: Input.t, ~new_input: Input.t, (), _) =>
        if (old_input.generation != new_input.generation) {
          ask(new_input);
        };
      let destroy = (_, (), _) => ();
    };
    include Virtual_dom.Vdom.Attr.Hooks.Make(Spec);
  };

  /* The write box, under the goal of an unfinished chain, with its tabs
   * above it. */
  module Box = {
    let label = (tab: Model.tab) =>
      switch (tab) {
      | Write => "Write next step"
      | Step => "Step"
      | Strategy => "Proof strategy"
      };

    /* The tab the user picked, or the stepper's default, its first. */
    let current = (~tabs: list(Model.tab), box: Model.box): Model.tab =>
      switch (box.tab, tabs) {
      | (Some(tab), _) when List.mem(tab, tabs) => tab
      | (_, [first, ..._]) => first
      | (_, []) => Write
      };

    /* What the box lists, at most eight, and which is highlighted: the
     * one chosen with the arrows, or else the first completion, the first
     * use of a rule named in full, or the use that gives the typed goal. */
    let listing =
        (goal: Picker.Goal.t, box: Model.box)
        : (list(StepSearch.item), option(int)) => {
      let items =
        StepSearch.items(
          goal,
          ~typed_name=Update.typed_name(box.editor),
          box.suggestions,
        )
        |> ListUtil.take(8);
      let gives_typed = (item: StepSearch.item) =>
        switch (item, box.search) {
        | (Use(use), Some(search)) =>
          StepSearch.reaches(
            goal,
            StepSearch.comparable(use.result),
            search.wanted,
          )
        | _ => false
        };
      let named =
        switch (items, box.suggestions) {
        | ([Complete(_), ..._], _)
        | ([_, ..._], Some({rule: Some(_), _})) => Some(0)
        | _ => List.find_index(gives_typed, items)
        };
      (
        items,
        switch (box.chosen) {
        | Some(i) when i < List.length(items) => Some(i)
        | Some(_)
        | None => named
        },
      );
    };

    /* The first and last columns of the goal's selected part, on its
     * first line. */
    let columns = (goal: Picker.Goal.t): option((int, int)) => {
      open OptUtil.Syntax;
      let editor = goal.editor.editor;
      let measured = Haz3lcore.CachedSyntax.measured(editor.syntax);
      let segment = editor.state.zipper.selection.content;
      let* first = ListUtil.hd_opt(segment);
      let+ last = ListUtil.last_opt(segment);
      let start =
        Haz3lcore.Measured.find_p(~msg="Box.columns", first, measured).origin;
      let end_ =
        Haz3lcore.Measured.find_p(~msg="Box.columns", last, measured).last;
      (start.col, end_.row == start.row ? end_.col : start.col + 1);
    };

    /* A use's step, as written: `axiom name` or `axiomrev name`. */
    let rule_label = (step: Proof.t) =>
      switch (Rows.single_step(step) |> Option.value(~default=step)) {
      | {term: AxiomStep({direction, equality, _}), _} =>
        (direction == Left ? "axiomrev " : "axiom ")
        ++ (ProofSyntax.exp_to_name(equality) |> Option.value(~default=""))
      | _ => ""
      };

    let keycap = name => div_c("box-key", [Node.text(name)]);

    /* A key with no modifiers held. */
    let plain = (name, key: Key.t) =>
      switch (key) {
      | {key: D(k), shift: Up, meta: Up, ctrl: Up, alt: Up, _} => k == name
      | _ => false
      };

    let view =
        (
          ~globals: Globals.t,
          /* The first is the stepper's default. */
          ~tabs: list(Model.tab),
          ~inject: Update.action => Ui_effect.t(unit),
          ~write: Picker.pick => Ui_effect.t(unit),
          /* Writes the steps the search found, one row each. */
          ~write_steps: list(Proof.t) => Ui_effect.t(unit),
          ~take_focus: Ui_effect.t(unit),
          ~focused: bool,
          /* Controls for the hole the box writes in place of. */
          ~hole_actions: list(Node.t),
          /* The columns of the goal's selected part, when the box writes
           * its replacement: the box moves right of it, with an arrow
           * from it. */
          ~replacing: option((int, int)),
          box: Model.box,
          goal: Picker.Goal.t,
        )
        : Node.t => {
      let tab = current(~tabs, box);
      let code = (e: Exp.t) => code(~globals, Exp(e));
      let tab_bar =
        div_c(
          "box-tabs",
          List.map(
            t =>
              Node.div(
                ~attrs=[
                  Attr.classes(["box-tab"] @ (t == tab ? ["on"] : [])),
                  Attr.on_click(_ => inject(Tab(t))),
                ],
                [Node.text(label(t))],
              ),
            tabs,
          )
          @ [div_c("box-hole-actions", hole_actions)],
        );
      let input = () => {
        let empty = Model.is_empty(box);
        let ghost = empty ? box.ghost : None;
        let found =
          switch (box.search) {
          | Some({status: Found([_, ..._] as steps), _}) => Some(steps)
          | Some({status: Found([]) | Searching | NotFound, _})
          | None => None
          };
        let (items, highlighted) = listing(goal, box);
        let take = (item: StepSearch.item) =>
          switch (item) {
          | Complete(name) => inject(Complete(name))
          | Use(use) => write_steps([use.step])
          };
        let highlighted_item =
          Option.bind(highlighted, i => List.nth_opt(items, i));
        let count = List.length(items);
        let keys = (key: Key.t) =>
          switch (ghost, highlighted_item, found) {
          | (Some(ghost), _, _) when plain("Tab", key) =>
            Some(write(ghost.pick))
          | (_, Some(Complete(name)), _) when plain("Tab", key) =>
            Some(inject(Complete(name)))
          /* The arrows choose among the items, and so don't move between
           * the box's lines while there are any. */
          | _ when plain("ArrowDown", key) && count > 0 =>
            let next = Option.fold(~none=0, ~some=i => i + 1, highlighted);
            Some(inject(Choose(Some(min(next, count - 1)))));
          | _ when plain("ArrowUp", key) && count > 0 =>
            Some(
              inject(
                Choose(
                  switch (highlighted) {
                  | Some(i) when i > 0 => Some(i - 1)
                  | Some(_)
                  | None => None
                  },
                ),
              ),
            )
          | (_, Some(item), _) when plain("Enter", key) =>
            Some(take(item))
          | (_, None, Some(steps)) when plain("Enter", key) =>
            Some(write_steps(steps))
          /* With nothing found, Enter does nothing. Shift+Enter still
           * breaks the line. */
          | _ when plain("Enter", key) => Some(Ui_effect.Ignore)
          | _ => None
          };
        let editor =
          CodeEditable.View.view(
            ~globals,
            ~signal=
              fun
              | MakeActive => take_focus,
            ~edit_mode=
              EditMode.Editable({
                inject: a => inject(BoxEditor(a)),
                escape: _ => Ui_effect.Ignore,
                take_focus: _ => Ui_effect.Ignore,
                focus: focused ? Some() : None,
              }),
            ~dynamics=Dynamics.Map.empty,
            ~keys,
            box.editor,
          );
        let named =
          switch (Update.typed_name(box.editor)) {
          | Some(name) =>
            List.assoc_opt(name, StepSearch.names(goal))
            |> Option.map(kind =>
                 switch (kind) {
                 | `Scope => "a fact in scope"
                 | `Axiom => "an axiom"
                 }
               )
          | None => None
          };
        let (ghost, hint) =
          switch (ghost, named, box.search) {
          | (Some(g), _, _) => (
              [div_c("box-ghost", [code(g.result)])],
              [keycap("Tab"), Node.text(g.rule)],
            )
          | (None, Some(kind), _) => ([], [Node.text(kind)])
          | (None, None, Some({status: Found([_, ..._]), _})) => (
              [],
              [keycap("Enter")],
            )
          | (None, None, Some({status: NotFound, _})) => (
              [],
              [
                keycap("Enter"),
                div_c(
                  "box-hint-none",
                  [Node.text("nothing proves this step")],
                ),
              ],
            )
          | (None, None, Some({status: Found([]), _})) => (
              [],
              [Node.text("the goal already")],
            )
          | (None, None, Some({status: Searching, _}) | None) => ([], [])
          };
        let slices =
          Model.running(box)
            ? Attr.create_hook(
                "write-box-search",
                SearchSlices.create({
                  generation: box.generation,
                  fresh:
                    switch (box.search) {
                    | Some(search) =>
                      search.checks == 0 && search.expanding == None
                    | None => false
                    },
                  slice: inject(SearchSlice(box.generation)),
                }),
              )
            : Attr.empty;
        let item = (i, item: StepSearch.item) =>
          Node.div(
            ~attrs=[
              Attr.classes(
                ["box-item"] @ (highlighted == Some(i) ? ["on"] : []),
              ),
              Attr.on_click(_ => Ui_effect.Many([take_focus, take(item)])),
            ],
            switch (item) {
            | Complete(name) => [
                code(Exp.fresh(Var(name))),
                div_c(
                  "box-item-note",
                  [
                    Node.text(
                      switch (List.assoc_opt(name, StepSearch.names(goal))) {
                      | Some(`Axiom) => "axiom"
                      | Some(`Scope)
                      | None => "in scope"
                      },
                    ),
                  ],
                ),
              ]
            | Use(use) => [
                code(use.result),
                div_c("box-item-rule", [Node.text(rule_label(use.step))]),
              ]
            },
          );
        [
          Node.div(
            ~attrs=[Attr.classes(["box-input"]), slices],
            [
              Node.div(
                ~attrs=[
                  Attr.classes(["box-editor"] @ (empty ? ["empty"] : [])),
                ],
                [editor] @ ghost,
              ),
              div_c("box-hint", hint),
            ],
          ),
        ]
        @ (items == [] ? [] : [div_c("box-items", List.mapi(item, items))]);
      };
      let strategies = () => {
        let button = (~clss=[], ~tooltip, contents, effect) =>
          Node.div(
            ~attrs=[
              Attr.classes(["strategy-button"] @ clss),
              Attr.title(tooltip),
              Attr.on_click(_ => effect),
            ],
            contents,
          );
        /* A strategy is an occasional intervention: after one, the box
         * goes back to writing steps. */
        let write_whole = term =>
          Ui_effect.Many([
            inject(Tab(Write)),
            write({
              at: None,
              step: Proof.fresh(term),
            }),
          ]);
        let strategy = (~intro, s: Picker.Strategies.strategy) =>
          switch (s.writes) {
          | Writes(term) =>
            button(
              ~clss=intro ? ["intro"] : [],
              ~tooltip=s.tooltip,
              [Node.text(s.label)]
              @ (
                switch (s.argument) {
                | Some(e) => [div_c("strategy-argument", [code(e)])]
                | None => []
                }
              ),
              write_whole(term),
            )
          | Chooses(choices) =>
            let is_open = box.menu == Some(s.label);
            let choice = (c: Picker.Strategies.choice) =>
              Node.div(
                ~attrs=[
                  Attr.classes(["strategy-choice"]),
                  Attr.on_click(_ => write_whole(c.term)),
                ],
                [
                  switch (Exp.term_of(c.label)) {
                  | EmptyHole =>
                    div_c("strategy-hole", [Node.text("? to fill in")])
                  | _ => code(c.label)
                  },
                ],
              );
            div_c(
              "strategy-chooser",
              [
                button(
                  ~clss=is_open ? ["open"] : [],
                  ~tooltip=s.tooltip,
                  [
                    Node.text(s.label),
                    div_c("strategy-caret", [Node.text("▾")]),
                  ],
                  inject(Menu(is_open ? None : Some(s.label))),
                ),
              ]
              @ (
                is_open
                  ? [div_c("strategy-menu", List.map(choice, choices))] : []
              ),
            );
          };
        let intros = Picker.Strategies.intros(goal);
        div_c(
          "strategy-buttons",
          List.map(strategy(~intro=true), intros)
          @ (intros == [] ? [] : [div_c("strategy-separator", [])])
          @ List.map(
              strategy(~intro=false),
              Picker.Strategies.general(goal),
            ),
        );
      };
      /* In pixels from the goal's first column, which the box's text
       * lines up with. The arrow starts just under the goal and ends at
       * the middle of the box's input, 55px further down. */
      let (shift, arrow) =
        switch (replacing) {
        | Some((left, right)) =>
          let col = c => float_of_int(c) *. globals.font_metrics.col_width;
          let start = (col(left) +. col(right)) /. 2.;
          let end_ = col(right) +. 10.;
          let path =
            Printf.sprintf(
              "M %.1f 0 C %.1f 45, %.1f 55, %.1f 55 M %.1f 50 L %.1f 55 L %.1f 60",
              start,
              start,
              start +. 10.,
              end_,
              end_ -. 6.,
              end_,
              end_ -. 6.,
            );
          (
            [
              Attr.create(
                "style",
                Printf.sprintf("--shift: %.1fpx", end_ +. 4.),
              ),
            ],
            [
              Node.create_svg(
                "svg",
                ~attrs=[
                  Attr.classes(["write-arrow"]),
                  Attr.create("width", Printf.sprintf("%.0f", end_ +. 2.)),
                  Attr.create("height", "62"),
                ],
                [
                  Node.create_svg(
                    "path",
                    ~attrs=[Attr.create("d", path)],
                    [],
                  ),
                ],
              ),
            ],
          );
        | None => ([], [])
        };
      Node.div(
        ~attrs=[
          Attr.classes(
            ["write-box"] @ (replacing == None ? [] : ["replacing"]),
          ),
          ...shift,
        ],
        arrow
        @ [tab_bar]
        @ (
          switch (tab) {
          | Write => input()
          | Step => []
          | Strategy => [strategies()]
          }
        ),
      );
    };
  };

  /* The row's justification: the step's keyword, or the cited equality's
   * name. A one-step focus is drawn as its step. */
  let rec justification = (step: Proof.t): string =>
    switch (step.term) {
    | AxiomStep({equality, _}) =>
      ProofSyntax.exp_to_name(equality) |> Option.value(~default="")
    | AlgebriteStep(_) => "algebra"
    | EvalStep => "eval"
    | Focus(_) =>
      Rows.single_step(step)
      |> Option.fold(~none="focus", ~some=justification)
    | Contradiction(_) => "contradiction"
    | Forall(_) => "forall"
    | Funext(_) => "extensionality"
    | Assume(_) => "assume"
    | Revert(_) => "revert"
    | Generalize(_) => "generalize"
    | Have(_) => "have"
    | Alias(_) => "alias"
    | Induction(_) => "induction"
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_)
    | Parens(_) => ""
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
        /* Extra controls for a hole that ends its chain, drawn beside the
         * write box's tabs. */
        ~hole_actions: Proof.t => list(Node.t)=_ => [],
        /* The editor holding the proof text, which the sub-editors splice
         * into. */
        ~backing: CodeEditable.Channel.t,
        /* The statics of the proof text. */
        ~info_map: Statics.Map.t,
        ~proof_map: ProofMap.t,
        /* What a pending side condition offers to discharge it. */
        ~exits: Obligation.t => list(Obligations.Exits.t),
        proof: Proof.t,
        model: Model.t,
      )
      : list(Node.t) => {
    let take_focus = f => signal(MakeActive(f));
    let focused = f => selected == Some(f);
    let code = code(~globals);
    /* Writes `pick` as the step after `row`'s goal, forcing the record
     * open, or the row just written would not be on screen. A form with an
     * argument then takes the caret into it: the backing editor's caret
     * moves into the new argument (a sub-editor takes no edits, and draws
     * no caret, while the caret is outside it), the new step's sub-editor
     * takes focus, and DOM focus follows after the render, as the click
     * left it on the button. */
    let write = (row: Rows.t, pick: Picker.pick) => {
      let step = pick.step;
      let into_arg =
        switch (pick.at, Picker.Terms.arg_id(step.term)) {
        | (None, Some(arg_id)) => [
            backing.inject(Perform(Move(Goal(TileId(arg_id))))),
            take_focus(Splice(Proof.rep_id(step), Arg)),
            Ui_effect.of_sync_fun(
              () => Haz3lcore.FocusEffect.schedule_cell(),
              (),
            ),
          ]
        | (_, None)
        | (Some(_), _) => []
        };
      Ui_effect.Many(
        [
          globals.inject_global(Set(Evaluation(ForceShowRecord))),
          edit_syntax(Picker.write(row, pick)),
        ]
        @ into_arg,
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
    /* The goal's editor. With `hints`, the steps it offers are
     * highlighted, and a click on one takes it. */
    /* With `into_box`, what is typed goes to the row's write box, which
     * writes the selected part's replacement. */
    let goal_editor =
        (
          ~hints: bool,
          ~previewing,
          ~into_box,
          row: Rows.t,
          local: Model.local,
        ) => {
      let keys = (key: Key.t) =>
        switch (key) {
        | {key: D(text), meta: Up, ctrl: Up, alt: Up, _}
            when into_box && String.length(text) == 1 =>
          Some(
            Ui_effect.Many([
              take_focus(Box(row.key)),
              inject((row.key, BoxEditor(Perform(Insert(text))))),
              Ui_effect.of_sync_fun(
                () => Haz3lcore.FocusEffect.schedule_cell(),
                (),
              ),
            ]),
          )
        | _ => None
        };
      let goal = local.goal;
      let write_opt = pick =>
        pick
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
              Option.map(Picker.refl(goal), List.nth_opt(goal.refls, idx)),
            ),
        ~inject=a => inject((row.key, Goal(a))),
        ~selected=focused(Goal(row.key)),
        ~keys,
        {
          editor: goal.editor,
          next_steps: hints ? Picker.Goal.step_ids(goal) : [],
          refls: hints ? List.map(Exp.rep_id, goal.refls) : [],
          /* The selected part the box writes a replacement for, underlined
           * as a focus's part is. */
          acts_on:
            into_box
              ? Option.map(Exp.rep_id, Update.selected_part(goal))
              : Picker.Goal.acts_on(goal, row),
          previewing,
        },
      );
    };
    let goal_view =
        (~hints, ~previewing=None, ~into_box=false, ~prev_broken, row: Rows.t) =>
      switch (prev_broken, Model.find(row.key, model)) {
      | (Some(mark), _) => Row.view_error_above(~globals, mark)
      | (None, Some(local)) =>
        goal_editor(~hints, ~previewing, ~into_box, row, local)
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
    /* The cell's own stepper steps a program, so it has no Proof strategy
     * tab, and starts on Step. */
    let tabs: list(Model.tab) =
      is_toplevel ? [Step, Write] : [Write, Step, Strategy];
    /* A chain's rows, then, until they reach it, the `true` a proof aims
     * at: a quiet row shaped like a step row, so the chain reads evenly. A
     * focus's chain rewrites part of a goal, so it aims at nothing. */
    let rec chain =
            (~top=false, ~target=!is_toplevel, rows: list(Rows.t))
            : list(Node.t) => [
      Node.div(
        ~attrs=[Attr.classes(["stepper", "cell-result"])],
        rows_view(~top, ~prev_broken=None, rows)
        @ (
          target && !ProofCheck.is_true(Rows.reached(rows))
            ? [Row.target(~globals)] : []
        ),
      ),
    ]
    and rows_view = (~top, ~prev_broken, rows: list(Rows.t)): list(Node.t) => {
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
          ([], prev_broken),
          rows,
        );
      nodes;
    }
    and view_row =
        (~top, ~prev_broken, ~status, ~only, ~is_last, row: Rows.t)
        : list(Node.t) =>
      switch (row.kind) {
      /* With history off, only the rows where the proof continues show. */
      | Step(_) when !globals.settings.core.evaluation.stepper_history => []
      | Step(nested) =>
        let step = row.step;
        let popup_extra =
          switch (Statics.Map.lookup(Proof.rep_id(step), info_map)) {
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
        let conditions = Rows.side_conditions(row);
        let expanded =
          switch (Model.find(row.key, model)) {
          | Some(local) => local.expanded
          | None => []
          };
        let toggle = i => inject((row.key, ToggleCondition(i)));
        let layout = Rows.layout(row, ~expanded);
        let is_open = i =>
          switch (layout) {
          | Block({conditions, _}) => List.mem(i, conditions)
          | Line => false
          };
        let dots =
          List.concat(
            List.mapi(
              (i, ob) =>
                is_open(i) ? [] : [Condition.dot(~toggle=toggle(i), ob)],
              conditions,
            ),
          );
        let block =
          switch (layout) {
          | Line => None
          | Block({conditions: open_conditions, arms: has_arms}) =>
            Some(
              List.filter_map(
                i =>
                  List.nth_opt(conditions, i)
                  |> Option.map(ob =>
                       Condition.view(
                         ~globals,
                         ~edit_syntax,
                         ~exits=exits(ob),
                         ~toggle=toggle(i),
                         ob,
                       )
                     ),
                open_conditions,
              )
              @ (has_arms ? arms(row, nested) : []),
            )
          };
        Row.view(
          ~globals,
          ~status,
          ~show_equiv=prev_broken == None,
          ~delete=Some(edit_syntax(Picker.Patches.remove(step))),
          ~popup_extra,
          ~repair=
            Row.repairs(~edit_syntax, ~info_map, ~proof_map, status, step),
          ~editor_view=goal_view(~hints=false, ~prev_broken, row),
          ~justification=Node.text(justification(step)),
          ~dots,
          ~block,
          (),
        )
        @ (
          switch (Rows.continuation(row)) {
          | Some(rows) =>
            rows_view(
              ~top,
              ~prev_broken=Rows.Status.broken_mark(status),
              rows,
            )
          | None => []
          }
        );
      | Hole
      | End =>
        /* A `?` marks a hole written mid-proof, whose box writes between
         * rows. At the end, the box alone says the proof continues here. */
        let unknown =
          row.kind == Hole && !is_last
            ? [div_c("step-unknown", [Node.text("?")])] : [];
        /* What Enter would write, previewed on the line above the box. */
        let preview =
          switch (Model.find(row.key, model)) {
          | Some({box: Some(box), _})
              when row.continues && Box.current(~tabs, box) == Write =>
            let lozenge = (clss, text) => [
              div_c("write-preview " ++ clss, [Node.text(text)]),
            ];
            switch (box.search) {
            | Some({status: Found([_, ..._] as steps), _}) =>
              lozenge(
                "",
                String.concat(" · ", List.map(justification, steps)),
              )
            | Some({status: NotFound, _}) =>
              lozenge("none", "no step gets here")
            | Some({status: Searching, _}) => lozenge("searching", "…")
            | Some({status: Found([]), _})
            | None => []
            };
          | _ => []
          };
        let justification =
          div_c(
            "missing-step-justification",
            unknown
            @ preview
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
        /* A finished chain, or one after a broken step, has nothing to
         * write. */
        let box =
          switch (prev_broken, Model.find(row.key, model)) {
          | (None, Some({box: Some(box), goal, _}))
              when row.continues && !ProofCheck.is_true(row.goal) =>
            Some((box, goal))
          | _ => None
          };
        let hints =
          switch (box) {
          | Some((box, _)) => Box.current(~tabs, box) == Step
          | None => false
          };
        /* With part of the goal selected, the box writes its
         * replacement. */
        let replacing =
          switch (box) {
          | Some((box, goal)) when Box.current(~tabs, box) == Write =>
            Update.selected_part(goal) == None ? None : Box.columns(goal)
          | _ => None
          };
        /* Where the highlighted use of a rule would act. */
        let previewing =
          switch (box) {
          | Some((box, goal)) when Box.current(~tabs, box) == Write =>
            let (items, highlighted) = Box.listing(goal, box);
            switch (Option.bind(highlighted, List.nth_opt(items))) {
            | Some(Use(use)) => Some(Exp.rep_id(use.part))
            | Some(Complete(_))
            | None => None
            };
          | _ => None
          };
        let box =
          switch (box) {
          | Some((box, goal)) => [
              Box.view(
                ~globals,
                ~tabs,
                ~inject=a => inject((row.key, a)),
                ~write=write(row),
                ~write_steps=
                  steps =>
                    switch (Picker.write_steps(row, steps)) {
                    | Some(patch) =>
                      Ui_effect.Many([
                        globals.inject_global(
                          Set(Evaluation(ForceShowRecord)),
                        ),
                        edit_syntax(patch),
                      ])
                    | None => Ui_effect.Ignore
                    },
                ~take_focus=take_focus(Box(row.key)),
                ~focused=focused(Box(row.key)),
                ~hole_actions=row.kind == Hole ? hole_actions(row.step) : [],
                ~replacing,
                box,
                goal,
              ),
            ]
          | None => []
          };
        switch (prev_broken, Model.find(row.key, model)) {
        | (Some(_), _)
        | (None, Some(_)) =>
          Row.view(
            ~globals,
            ~status,
            ~show_equiv=prev_broken == None,
            ~delete,
            ~editor_view=
              goal_view(
                ~hints,
                ~previewing,
                ~into_box=replacing != None,
                ~prev_broken,
                row,
              ),
            ~justification,
            (),
          )
          @ box
        | (None, None) => [justification]
        };
      }
    /* The goals a step creates, as dark arms: a bar naming what the arm
     * adds over a slot holding the arm's own stepper. */
    and arms = (row: Rows.t, nested: Rows.nested): list(Node.t) => {
      let step = row.step;
      let note = content => div_c("step-arm-note", content);
      let arm = (bar, slot) => [
        div_c("step-arm-bar", bar),
        div_c("step-slot", slot),
      ];
      let dark = sections => [div_c("step-section arm", sections)];
      let form_arg = fallback =>
        splice(
          ~step,
          ~slot=Arg,
          ~target=SubEditor.Target.child(~anchor=Proof.rep_id(step), 0),
          fallback,
        );
      switch (nested, step.term) {
      | (Body(rows), Forall(p, _)) =>
        let typ =
          switch (Pat.get_var(p), rows) {
          | (Some(x), [first, ..._]) =>
            Rows.ctx(first)
            |> Option.bind(_, ctx =>
                 Ctx.lookup_var(SemanticCtx.get_ctx(ctx), x)
               )
            |> Option.map((entry: Ctx.var_entry) =>
                 [Node.text(", any "), code(Typ(entry.typ))]
               )
          | _ => None
          };
        dark(
          arm(
            [
              div_c("step-arm-code", [code(Pat(p))]),
              note([Node.text("fixed")] @ Option.value(~default=[], typ)),
            ],
            chain(rows),
          ),
        );
      | (Body(rows), Funext(p, _)) =>
        dark(
          arm(
            [form_arg(Pat(p)), note([Node.text("any argument")])],
            chain(rows),
          ),
        )
      | (Body(rows), Assume(e, as_name, _)) =>
        let name =
          as_name
          |> Option.bind(_, Pat.get_var)
          |> Option.value(~default="assume");
        dark(
          arm(
            [form_arg(Exp(e)), note([Node.text("as " ++ name)])],
            chain(rows),
          ),
        );
      | (Body(rows), Revert(e, _, _)) =>
        dark(
          arm(
            [
              form_arg(Exp(e)),
              note([Node.text("now a premise of the goal")]),
            ],
            chain(rows),
          ),
        )
      | (Body(rows), Generalize(e, _)) =>
        dark(
          arm(
            [form_arg(Exp(e)), note([Node.text("quantified again")])],
            chain(rows),
          ),
        )
      | (Premises({sub, _}), Have(e, _, _)) =>
        dark(
          arm(
            [form_arg(Exp(e)), note([Node.text("as have")])],
            chain(sub),
          ),
        )
      | (Cases(case_rows), Induction(scrut, _, cases)) =>
        induction(row, scrut, cases, case_rows)
      /* The selector, its ordinal included, is edited in place in the bar
       * that opens the C. */
      | (Focus(rows), Focus({selector, _})) =>
        dark(
          arm(
            [
              Node.text("from "),
              form_arg(Exp(selector)),
              note([Node.text("rewritten in place")]),
            ],
            chain(~target=false, rows),
          ),
        )
      | (Leaf | Body(_) | Cases(_) | Premises(_) | Focus(_), _) => []
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
          |> List.rev_map(((name, _, fact)) =>
               div_c(
                 "step-arm-fact",
                 [
                   Node.text(name ++ ": "),
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
        [
          div_c(
            "step-arm-bar",
            [
              Node.text("| "),
              pattern,
              div_c("step-arm-note", hypotheses(rows)),
              remove_case,
            ],
          ),
          div_c("step-slot", chain(rows)),
        ];
      };
      let add_case =
        Picker.Patches.add_case(
          step,
          ~marks=
            Option.fold(
              ~none=[],
              ~some=(e: ProofMap.entry) => e.marks,
              row.entry,
            ),
        )
        |> Option.map(patch =>
             Widgets.button(
               Node.text("Case ..."),
               ~tooltip="Add case",
               ~clss=["subtle-button", "add-case-button"],
               _ =>
               edit_syntax(patch)
             )
           )
        |> Option.to_list;
      /* The scrutinee is edited in place, in the bar that opens the C. */
      let scrut_bar =
        div_c(
          "step-arm-bar",
          [
            Node.text("induction on "),
            splice(
              ~step,
              ~slot=Scrut,
              ~target=
                SubEditor.Target.(
                  child(~anchor, 0)
                  |> until(BeforeOrEnd(NthTile(["|", "=>"], 0)))
                ),
              Exp(scrut),
            ),
          ],
        );
      [
        div_c(
          "step-section arm",
          [scrut_bar] @ List.concat(List.mapi(case, case_rows)) @ add_case,
        ),
      ];
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
