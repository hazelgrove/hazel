open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
// open ExplainThisUpdate;
open Util;
/* The exercises mode interface for a single exercise. Composed of multiple editors and results. */
/* This file follows conventions in [docs/ui-architecture.md] */
module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type editing_flags = {
    editing_title: bool,
    editing_prompt: bool,
    editing_display_hint: bool,
    editing_task_reference: bool,
  };

  let editing_flags_false = {
    editing_title: false,
    editing_prompt: false,
    editing_display_hint: false,
    editing_task_reference: false,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    spec: Tutorial.spec, // The spec that the model will be reset to on ResetExercise
    /* We keep a separate editors field below (even though each cell technically also has its own editor)
       for two reasons:
         1. There are two synced cells that have the same internal `editor` model
         2. The editors need to be `stitched` together before any cell calculations can be done */
    editors: Tutorial.p(Editor.t),
    cells: Tutorial.stitched(CellEditor.Model.t),
    editing_flags,
  };
  let of_spec = (~settings as _, ~instructor_mode as _: bool, spec) => {
    let editors = Tutorial.map(spec, Editor.Model.mk, Editor.Model.mk);
    let term_item_to_cell = (item: Tutorial.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      Tutorial.stitch_term(editors)
      |> Tutorial.map_stitched(_ => term_item_to_cell);
    {
      spec,
      editors,
      cells,
      editing_flags: editing_flags_false,
    };
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = Tutorial.persistent_tutorial_mode;

  let persist = (exercise: t, ~instructor_mode: bool) => {
    Tutorial.{
      editors:
        Tutorial.positioned_editors(exercise.editors)
        |> List.filter(((pos, _)) =>
             Tutorial.visible_in(pos, ~instructor_mode)
           )
        |> List.map(((pos, editor: Editor.t)) =>
             (pos, editor.state.zipper |> PersistentZipper.persist)
           ),
      prompt: exercise.editors.prompt,
      title: exercise.editors.title,
      display_hint: exercise.editors.display_hint,
      task_reference: exercise.editors.task_reference,
    };
  };

  let unpersist = (~settings, ~instructor_mode, persistent, spec) => {
    let spec = Tutorial.unpersist(~instructor_mode, persistent, spec);
    of_spec(~settings, ~instructor_mode, spec);
  };

  let all_tests_passed = (exercise: t) => {
    let test_results =
      Tutorial.map_stitched(
        (_, cell_editor: CellEditor.Model.t) =>
          cell_editor.result |> EvalResult.Model.test_results,
        exercise.cells,
      );

    switch (Tutorial.get_stitched(HiddenTests, test_results)) {
    | Some(test_results) =>
      test_results.total > 0 && test_results.passing == test_results.total
    | _ => false
    };
  };
  let test_count = (exercise: t) => {
    let test_results =
      Tutorial.map_stitched(
        (_, cell_editor: CellEditor.Model.t) =>
          cell_editor.result |> EvalResult.Model.test_results,
        exercise.cells,
      );

    switch (Tutorial.get_stitched(HiddenTests, test_results)) {
    | Some(test_results) => test_results.total
    | None => 0
    };
  };

  let return_title = (exercise: t) =>
    if (all_tests_passed(exercise)) {
      exercise.editors.title ++ " ✔";
    } else {
      exercise.editors.title;
    };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type instructor =
    | EditingTitle
    | EditingPrompt
    | EditingDisplayHint
    | EditingTaskReference
    | UpdateTitle(string)
    | UpdatePrompt(string)
    | UpdateDisplayHint(string)
    | UpdateTaskReference(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Editor(Tutorial.pos, CellEditor.Update.t)
    | ResetEditor(Tutorial.pos)
    | ResetTutorial
    | MoveToNextExercise
    | MoveToPrevExercise
    | Change_report_view
    | Instructor(instructor);

  let instructor_update =
      (action: instructor, model: Model.t): Updated.t(Model.t) =>
    switch (action) {
    | EditingTitle =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_title: !model.editing_flags.editing_title,
        },
      })
    | EditingPrompt =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_prompt: !model.editing_flags.editing_prompt,
        },
      })
    | EditingDisplayHint =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_display_hint: !model.editing_flags.editing_display_hint,
        },
      })
    | EditingTaskReference =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_task_reference: !model.editing_flags.editing_task_reference,
        },
      })
    | UpdateTitle(title) =>
      Updated.return_quiet(
        {
          ...model,
          editors: Tutorial.update_title({eds: model.editors}, title).eds,
        },
        ~is_edit=true,
      )
    | UpdatePrompt(prompt) =>
      Updated.return({
        ...model,
        editors: Tutorial.update_prompt({eds: model.editors}, prompt).eds,
      })
    | UpdateDisplayHint(hint) =>
      Updated.return({
        ...model,
        editors:
          Tutorial.update_display_hint({eds: model.editors}, hint).eds,
      })
    | UpdateTaskReference(ref_) =>
      Updated.return({
        ...model,
        editors:
          Tutorial.update_task_reference({eds: model.editors}, ref_).eds,
      })
    };

  let instructor_update =
      (~settings: Settings.t, action: instructor, model: Model.t)
      : Updated.t(Model.t) =>
    if (settings.instructor_mode) {
      instructor_update(action, model);
    } else {
      Updated.return_quiet(model);
    };

  let update =
      (~settings: Settings.t, ~schedule_action as _, action, model: Model.t)
      : Updated.t(Model.t) => {
    let instructor_mode = settings.instructor_mode;
    switch (action) {
    // This MoveToNextExercise is only here so that Tutorial(TutorialMode.Update.MoveToNextExercise)
    // is called in TutorialsMode. This is a dummy update because this function requires Updated(Model.t)
    | MoveToNextExercise =>
      Updated.return_quiet({
        ...model,
        editors: {
          ...model.editors,
          show_report: model.editors.show_report,
        },
      })
    | MoveToPrevExercise =>
      Updated.return_quiet({
        ...model,
        editors: {
          ...model.editors,
          show_report: model.editors.show_report,
        },
      })
    | Editor(pos, MainEditor(action))
        when Tutorial.visible_in(pos, ~instructor_mode) =>
      // Redirect to editors
      let editor =
        Tutorial.main_editor_of_state(~selection=pos, model.editors);
      let (statics, dynamics) =
        switch (Tutorial.get_stitched(pos, model.cells)) {
        | cell_editor => (
            cell_editor.editor.statics,
            cell_editor.editor.dynamics,
          )
        | exception (Failure(_)) => (
            CachedStatics.empty,
            Language.Dynamics.Map.empty,
          )
        };
      let* new_editor =
        // Hack[Matt]: put Editor.t into a CodeEditor.t to use its update function
        editor
        |> CodeEditable.Model.mk(~statics, ~dynamics)
        |> CodeEditable.Update.update(~settings, action);
      {
        ...model,
        editors:
          Tutorial.put_main_editor(
            ~selection=pos,
            model.editors,
            new_editor.editor,
          ),
      };
    | Editor(pos, MainEditor(action)) =>
      switch (CodeSelectable.Update.convert_action(action)) {
      | Some(action) =>
        let editor =
          Tutorial.main_editor_of_state(~selection=pos, model.editors);
        let* new_editor =
          // Hack[Matt]: put Editor.t into a CodeSelectable.t to use its update function
          editor
          |> CodeSelectable.Model.mk
          |> CodeSelectable.Update.update(~settings, action);
        {
          ...model,
          editors:
            Tutorial.put_main_editor(
              ~selection=pos,
              model.editors,
              new_editor.editor,
            ),
        };
      | None => Updated.return_quiet(model)
      }
    | Editor(pos, ResultAction(_) as action)
        when
          Tutorial.visible_in(pos, ~instructor_mode)
          || action
          |> (
            fun
            | ResultAction(UpdateResult(_)) => true
            | _ => false
          ) =>
      let cell = Tutorial.get_stitched(pos, model.cells);
      let* new_cell = CellEditor.Update.update(~settings, action, cell);
      {
        ...model,
        cells: Tutorial.put_stitched(pos, model.cells, new_cell),
      };
    | Editor(_, ResultAction(_)) => Updated.raise_invalid_action(model) // TODO: I think this case should never happen
    | ResetEditor(pos) =>
      let spec = Tutorial.main_editor_of_state(~selection=pos, model.spec);
      let new_editor = Editor.Model.mk(spec);
      {
        ...model,
        editors:
          Tutorial.put_main_editor(~selection=pos, model.editors, new_editor),
      }
      |> Updated.return;
    | ResetTutorial =>
      let new_editors =
        Tutorial.map(model.spec, Editor.Model.mk, Editor.Model.mk);
      {
        ...model,
        editors: new_editors,
      }
      |> Updated.return;
    | Change_report_view =>
      Updated.return_quiet({
        ...model,
        editors: {
          ...model.editors,
          show_report: !model.editors.show_report,
        },
      })
    | Instructor(action) => instructor_update(~settings, action, model)
    };
  };

  let can_undo = (action: t) => {
    switch (action) {
    | Editor(_, action) => CellEditor.Update.can_undo(action)
    | ResetEditor(_) => true
    | ResetTutorial => true
    | MoveToNextExercise
    | MoveToPrevExercise
    | Change_report_view
    | Instructor(_) => false
    };
  };

  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    let stitched_elabs = Tutorial.stitch_term(model.editors);
    let worker_request = ref([]);
    let queue_worker = (pos, req_value: WorkerServer.Request.value) => {
      worker_request :=
        worker_request^ @ [(pos |> Tutorial.key_for_statics, req_value)];
    };
    let cells =
      Tutorial.map2_stitched(
        (pos, {term, editor}: Tutorial.TermItem.t, cell: CellEditor.Model.t) =>
          {
            editor: {
              editor,
              statics: cell.editor.statics,
              dynamics: EvalResult.Model.dynamics(cell.result),
              context_menu: None,
            },
            result: cell.result,
          }
          |> CellEditor.Update.calculate(
               ~settings,
               ~is_edited,
               ~queue_worker=Some(queue_worker(pos)),
               ~stitch=_ =>
               term
             ),
        stitched_elabs,
        model.cells,
      );
    WorkerClient.request(
      worker_request^,
      ~handler=
        List.iter(((pos, result)) => {
          let pos' = Tutorial.pos_of_key(pos);
          let result': Language.ProgramResult.t(Language.ProgramResult.inner) =
            switch (result) {
            | Ok((r, s)) =>
              ResultOk({
                result: r,
                state: s,
              })
            | Error(e) => ResultFail(e)
            };
          schedule_action(
            Editor(pos', ResultAction(UpdateResult(result'))),
          );
        }),
      ~timeout=_ => {
        let _ =
          Tutorial.map_stitched(
            (pos, _) =>
              schedule_action(
                Editor(
                  pos,
                  ResultAction(UpdateResult(ResultFail(Timeout))),
                ),
              ),
            model.cells,
          );
        ();
      },
    );
    /* The following section pulls statics back from cells into the editors
       There are many ad-hoc things about this code, including the fact that
       one of the editors is shown in two cells, so we arbitrarily choose which
       statics to take */
    let editors: Tutorial.p('a) = {
      let calculate = Editor.Update.calculate(~settings, ~is_edited);
      {
        id: model.editors.id,
        title: model.editors.title,
        version: model.editors.version,
        module_name: model.editors.module_name,
        prompt: model.editors.prompt,
        your_impl:
          calculate(
            cells.user_impl.editor.statics,
            cells.user_impl.editor.dynamics,
            model.editors.your_impl,
          ),
        display_hint: model.editors.display_hint,
        task_reference: model.editors.task_reference,
        hidden_tests: {
          tests:
            calculate(
              cells.hidden_tests.editor.statics,
              cells.hidden_tests.editor.dynamics,
              model.editors.hidden_tests.tests,
            ),
          hints: model.editors.hidden_tests.hints,
        },
        wrapper: model.editors.wrapper,
        show_report: model.editors.show_report,
        // syntax_tests: model.editors.syntax_tests,
      };
    };
    {
      spec: model.spec,
      editors,
      cells,
      editing_flags: model.editing_flags,
    };
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cell(Tutorial.pos, CellEditor.Selection.t)
    | TextBox;
  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    switch (selection) {
    | Cell(pos, s) =>
      switch (Tutorial.get_stitched(pos, model.cells)) {
      | cell_editor =>
        let+ a =
          CellEditor.Selection.get_cursor_info(~selection=s, cell_editor);
        Update.Editor(pos, a);
      | exception (Failure(_)) => empty
      }
    | TextBox => empty
    };
  };

  let handle_key_event =
      (~selection: t, ~event, model: Model.t): option(Update.t) => {
    switch (selection) {
    | Cell(pos, s) =>
      switch (Tutorial.get_stitched(pos, model.cells)) {
      | cell_editor =>
        CellEditor.Selection.handle_key_event(
          ~selection=s,
          ~event,
          cell_editor,
        )
        |> Option.map(a => Update.Editor(pos, a))
      | exception (Failure(_)) => None
      }
    | TextBox => None
    };
  };
  let jump_to_tile =
      (~settings: Settings.t, tile, model: Model.t): option((Update.t, t)) => {
    Tutorial.positioned_editors(model.editors)
    |> List.find_opt(((p, e: Editor.t)) =>
         TermData.root_tile(tile, e.syntax.term_data) != None
         && Tutorial.visible_in(p, ~instructor_mode=settings.instructor_mode)
       )
    |> Option.map(((pos, _)) =>
         (
           Update.Editor(
             pos,
             MainEditor(Perform(Move(Goal(TileId(tile))))),
           ),
           Cell(pos, CellEditor.Selection.MainEditor),
         )
       );
  };
};

module View = {
  type event =
    | MakeActive(Selection.t);

  type vis_marked('a) =
    | InstructorOnly(unit => 'a)
    | Always('a);
  let render_cells = (settings: Settings.t, v: list(vis_marked(Node.t))) => {
    List.filter_map(
      vis =>
        switch (vis) {
        | InstructorOnly(f) => settings.instructor_mode ? Some(f()) : None
        | Always(node) => Some(node)
        },
      v,
    );
  };
  let view =
      (
        ~globals: Globals.t,
        ~signal: event => 'b,
        ~inject: Update.t => 'b,
        ~inject_explainthis: ExplainThisUpdate.update => 'b,
        ~selection: option(Selection.t),
        model: Model.t,
      ) => {
    let editing_flags = model.editing_flags;
    let eds = model.editors;
    //let has_checkmark = Model.all_tests_passed(model);
    let {user_impl, hidden_tests}: Tutorial.stitched('a) = model.cells;

    let stitched_tests =
      Tutorial.map_stitched(
        (_, cell_editor: CellEditor.Model.t) =>
          cell_editor.result |> EvalResult.Model.test_results,
        model.cells,
      );
    let test_count =
      switch (Tutorial.get_stitched(HiddenTests, stitched_tests)) {
      | None => 0 /* No test cases */
      | Some(test_results) => test_results.total
      };
    let grading_report =
      TutorialGrading.GradingReport.mk(eds, ~stitched_tests);

    let editor_view =
        (
          ~caption: string,
          ~subcaption: option(string)=?,
          ~result_kind=`NoResults,
          this_pos: Tutorial.pos,
          cell: CellEditor.Model.t,
        ) => {
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => signal(MakeActive(Cell(this_pos, a))),
        ~selected=
          switch (selection) {
          | Some(Cell(pos, s)) when pos == this_pos => Some(s)
          | _ => None
          },
        ~inject=a => inject(Editor(this_pos, a)),
        ~result_kind,
        ~caption=CellCommon.caption(caption, ~rest=?subcaption),
        ~lines=true,
        cell,
      );
    };
    let update_title = _ => {
      let new_title =
        Obj.magic(
          Js_of_ocaml.Js.some(
            JsUtil.get_elem_by_id("tutorial-title-input-box"),
          ),
        )##.value;
      let update_events = [
        inject(Instructor(UpdateTitle(new_title))),
        inject(Instructor(EditingTitle)),
      ];
      Virtual_dom.Vdom.Effect.Many(update_events);
    };

    let title_view = {
      let title_placeholder =
        eds.title == "" ? "Untitled Tutorial" : eds.title;
      CellCommon.simple_cell_view([
        div(
          ~attrs=[Attr.class_("title-cell")],
          [
            globals.settings.instructor_mode
              ? editing_flags.editing_title
                  ? div(
                      ~attrs=[Attr.class_("title-edit")],
                      [
                        input(
                          ~attrs=[
                            Attr.class_("title-text"),
                            Attr.id("tutorial-title-input-box"),
                            Attr.value(eds.title),
                            Attr.on_focus(_ => signal(MakeActive(TextBox))),
                          ],
                          (),
                        ),
                        div(
                          ~attrs=[Attr.class_("edit-icon")],
                          [Widgets.button(Icons.confirm, update_title)],
                        ),
                        div(
                          ~attrs=[Attr.class_("edit-icon")],
                          [
                            Widgets.button(Icons.cancel, _ =>
                              inject(Instructor(EditingTitle))
                            ),
                          ],
                        ),
                      ],
                    )
                  : div(
                      ~attrs=[Attr.class_("title-edit")],
                      [
                        div(
                          ~attrs=[
                            Attr.classes([
                              "title-text",
                              eds.title == "" ? "title-placeholder" : "",
                            ]),
                          ],
                          [text(title_placeholder)],
                        ),
                        div(
                          ~attrs=[Attr.class_("edit-icon")],
                          [
                            Widgets.button(Icons.pencil, _ =>
                              inject(Instructor(EditingTitle))
                            ),
                          ],
                        ),
                      ],
                    )
              : div(~attrs=[Attr.class_("title-text")], [text(eds.title)]),
          ],
        ),
      ]);
    };

    let update_prompt = _ => {
      let new_prompt =
        Obj.magic(
          Js_of_ocaml.Js.some(
            JsUtil.get_elem_by_id("tutorial-prompt-input-box"),
          ),
        )##.value;
      let update_events = [
        inject(Instructor(EditingPrompt)),
        inject(Instructor(UpdatePrompt(new_prompt))),
      ];
      Virtual_dom.Vdom.Effect.Many(update_events);
    };

    let prompt_view = {
      let prompt_placeholder = eds.prompt == "" ? "Empty Prompt" : eds.prompt;
      let (msg, _) =
        ExplainThis.mk_translation(
          ~globals,
          ~inject=inject_explainthis,
          prompt_placeholder,
        );
      div(
        ~attrs=[Attr.class_("cell-prompt")],
        [
          globals.settings.instructor_mode
            ? editing_flags.editing_prompt
                ? div(
                    ~attrs=[Attr.class_("prompt-edit")],
                    [
                      div(
                        ~attrs=[Attr.id("prompt-textarea-container")],
                        [
                          textarea(
                            ~attrs=[
                              Attr.class_("prompt-text"),
                              Attr.id("tutorial-prompt-input-box"),
                              Attr.on_focus(_ =>
                                signal(MakeActive(TextBox))
                              ),
                              Attr.create("rows", "5"),
                              Attr.create("cols", "30"),
                            ],
                            [text(eds.prompt)],
                          ),
                        ],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [Widgets.button(Icons.confirm, update_prompt)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.cancel, _ =>
                            inject(Instructor(EditingPrompt))
                          ),
                        ],
                      ),
                    ],
                  )
                : div(
                    ~attrs=[Attr.class_("prompt-edit")],
                    [
                      div(
                        ~attrs=[
                          Attr.classes([
                            "prompt-content",
                            eds.prompt == "" ? "prompt-placeholder" : "",
                          ]),
                        ],
                        msg,
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-pencil")],
                        [
                          Widgets.button(Icons.pencil, _ =>
                            inject(Instructor(EditingPrompt))
                          ),
                        ],
                      ),
                    ],
                  )
            : div(~attrs=[Attr.class_("prompt-content")], msg),
        ],
      );
    };

    let prev_button_view =
      if (model.editors.version > 1) {
        div(
          ~attrs=[Attr.class_("prev-button")],
          [Widgets.button(Icons.prev, _ => inject(MoveToPrevExercise))],
        );
      } else {
        div([]);
      };

    let your_impl_view = {
      Always(
        div(
          ~attrs=[Attr.class_("your-impl-wrapper")], // 🆕 Add this wrapper
          [
            editor_view(
              YourImpl,
              user_impl,
              ~caption="Your Implementation",
              ~result_kind=`EvalResults,
            ),
          ],
        ),
      );
    };

    let hidden_tests_view =
      InstructorOnly(
        () => editor_view(HiddenTests, hidden_tests, ~caption="Hidden Tests"),
      );
    let update_display_hint = _ => {
      let new_hint =
        Obj.magic(
          Js_of_ocaml.Js.some(
            JsUtil.get_elem_by_id("tutorial-hint-input-box"),
          ),
        )##.value;
      let update_events = [
        inject(Instructor(EditingDisplayHint)),
        inject(Instructor(UpdateDisplayHint(new_hint))),
      ];
      Virtual_dom.Vdom.Effect.Many(update_events);
    };

    let hint_view = {
      let hint_placeholder =
        eds.display_hint == "" ? "No hints available." : eds.display_hint;
      let (msg, _) =
        ExplainThis.mk_translation(
          ~globals,
          ~inject=_ => (),
          hint_placeholder,
        );
      globals.settings.instructor_mode
        ? editing_flags.editing_display_hint
            ? div(
                ~attrs=[Attr.class_("hint-cell")],
                [
                  div(~attrs=[Attr.class_("hint-title")], [text("Hint")]),
                  div(
                    ~attrs=[Attr.class_("hint-edit")],
                    [
                      textarea(
                        ~attrs=[
                          Attr.class_("hint-text"),
                          Attr.id("tutorial-hint-input-box"),
                          Attr.on_focus(_ => signal(MakeActive(TextBox))),
                          Attr.create("rows", "3"),
                          Attr.create("cols", "30"),
                        ],
                        [text(eds.display_hint)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [Widgets.button(Icons.confirm, update_display_hint)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.cancel, _ =>
                            inject(Instructor(EditingDisplayHint))
                          ),
                        ],
                      ),
                    ],
                  ),
                ],
              )
            : div(
                ~attrs=[Attr.class_("hint-cell")],
                [
                  div(~attrs=[Attr.class_("hint-title")], [text("Hint")]),
                  div(
                    ~attrs=[Attr.class_("hint-edit")],
                    [
                      div(~attrs=[Attr.class_("hint-content")], msg),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.pencil, _ =>
                            inject(Instructor(EditingDisplayHint))
                          ),
                        ],
                      ),
                    ],
                  ),
                ],
              )
        : div(
            ~attrs=[Attr.class_("hint-cell")],
            [
              div(~attrs=[Attr.class_("hint-title")], [text("Hint")]),
              div(~attrs=[Attr.class_("hint-content")], msg),
            ],
          );
    };
    let update_task_reference = _ => {
      let new_ref =
        Obj.magic(
          Js_of_ocaml.Js.some(
            JsUtil.get_elem_by_id("tutorial-taskref-input-box"),
          ),
        )##.value;
      let update_events = [
        inject(Instructor(EditingTaskReference)),
        inject(Instructor(UpdateTaskReference(new_ref))),
      ];
      Virtual_dom.Vdom.Effect.Many(update_events);
    };

    let task_reference_view =
      if (globals.settings.instructor_mode) {
        let ref_placeholder =
          eds.task_reference == "" ? "No task reference." : eds.task_reference;
        let (msg, _) =
          ExplainThis.mk_translation(
            ~globals,
            ~inject=_ => (),
            ref_placeholder,
          );
        editing_flags.editing_task_reference
          ? div(
              ~attrs=[Attr.class_("task-reference-cell")],
              [
                div(
                  ~attrs=[Attr.class_("task-reference-title")],
                  [text("Task Reference")],
                ),
                div(
                  ~attrs=[Attr.class_("task-reference-edit")],
                  [
                    textarea(
                      ~attrs=[
                        Attr.class_("task-reference-text"),
                        Attr.id("tutorial-taskref-input-box"),
                        Attr.on_focus(_ => signal(MakeActive(TextBox))),
                        Attr.create("rows", "3"),
                        Attr.create("cols", "30"),
                      ],
                      [text(eds.task_reference)],
                    ),
                    div(
                      ~attrs=[Attr.class_("edit-icon")],
                      [Widgets.button(Icons.confirm, update_task_reference)],
                    ),
                    div(
                      ~attrs=[Attr.class_("edit-icon")],
                      [
                        Widgets.button(Icons.cancel, _ =>
                          inject(Instructor(EditingTaskReference))
                        ),
                      ],
                    ),
                  ],
                ),
              ],
            )
          : div(
              ~attrs=[Attr.class_("task-reference-cell")],
              [
                div(
                  ~attrs=[Attr.class_("task-reference-title")],
                  [text("Task Reference")],
                ),
                div(
                  ~attrs=[Attr.class_("task-reference-edit")],
                  [
                    div(~attrs=[Attr.class_("task-reference-content")], msg),
                    div(
                      ~attrs=[Attr.class_("edit-icon")],
                      [
                        Widgets.button(Icons.pencil, _ =>
                          inject(Instructor(EditingTaskReference))
                        ),
                      ],
                    ),
                  ],
                ),
              ],
            );
      } else {
        div([]);
      };

    let report_icon_view =
      div(
        ~attrs=[Attr.class_("checkmark-container")],
        [
          div(
            ~attrs=[Attr.class_("report-icon")],
            [
              Widgets.button(Icons.infoIcon, _ => inject(Change_report_view)),
            ],
          ),
        ],
      );
    let next_button_view =
      model.editors.version < 10
        ? div(
            ~attrs=[Attr.class_("next-button")],
            [Widgets.button(Icons.next, _ => inject(MoveToNextExercise))],
          )
        : div(~attrs=[Attr.class_("done-message")], [text("Done! 🎉")]);

    let impl_grading_view =
      if (test_count > 0) {
        let checkmark_view =
          switch (Tutorial.get_stitched(HiddenTests, stitched_tests)) {
          | Some(test_results) =>
            let inner_result = hidden_tests.result.result;
            let result = inner_result |> Util.Calc.get_value;
            switch (result) {
            | ResultPending =>
              div(
                ~attrs=[Attr.classes(["checkmark-grey", "pending"])],
                [text("🤔")],
              )
            | ResultFail(Timeout) =>
              div(
                ~attrs=[Attr.class_("checkmark-container")],
                [
                  div(~attrs=[Attr.class_("cross")], [text("✖")]),
                  div(
                    ~attrs=[Attr.class_("report-icon")],
                    [
                      Widgets.button(Icons.infoIcon, _ =>
                        inject(Change_report_view)
                      ),
                    ],
                  ),
                ],
              )
            | ResultOk(_) =>
              if (test_results.total == test_results.passing) {
                div(
                  ~attrs=[Attr.class_("checkmark-container")],
                  [
                    div(~attrs=[Attr.class_("checkmark")], [text("🤩")]),
                  ],
                );
              } else {
                div(
                  ~attrs=[Attr.class_("checkmark-grey")],
                  [text("🤔")],
                );
              }
            | _ => div([])
            };
          | None => div([]) // No test results available yet
          };

        div([checkmark_view]);
      } else if (test_count > 1) {
        TutorialGrading.ImplGradingReport.view(
          ~signal_jump=
            id =>
              inject(
                Editor(
                  HiddenTests,
                  MainEditor(Perform(Move(Goal(TileId(id))))),
                ),
              ),
          ~report=grading_report.impl_grading_report,
          ~max_points=1,
        );
      } else {
        div(
          [] // Ensure nothing appears if test_count is 0
        );
      };
    [title_view, prompt_view]
    @ (
      eds.display_hint == "" && !globals.settings.instructor_mode
        ? [] : [hint_view]
    )
    @ [task_reference_view]
    @ render_cells(
        globals.settings,
        [
          your_impl_view,
          hidden_tests_view,
          Always(
            div(
              ~attrs=[],
              [
                div(
                  ~attrs=[Attr.class_("nav-buttons-row")],
                  [
                    prev_button_view,
                    div(
                      ~attrs=[Attr.class_("right-nav-cluster")],
                      [impl_grading_view, report_icon_view, next_button_view],
                    ),
                  ],
                ),
                eds.show_report
                  ? TutorialGrading.ImplGradingReport.view(
                      ~signal_jump=
                        id =>
                          inject(
                            Editor(
                              HiddenTests,
                              MainEditor(Perform(Move(Goal(TileId(id))))),
                            ),
                          ),
                      ~report=grading_report.impl_grading_report,
                      ~max_points=1,
                    )
                  : div([]),
              ],
            ),
          ),
        ],
      );
  };
};
