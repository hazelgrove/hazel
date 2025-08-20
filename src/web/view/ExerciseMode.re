open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util;

/* The exercises mode interface for a single exercise. Composed of multiple editors and results. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type editing_flags = {
    editing_title: bool,
    editing_prompt: bool,
    editing_test_val_rep: bool,
    editing_mut_test_rep: bool,
    editing_impl_grd_rep: bool,
    editing_module_name: bool,
    editing_syntax_rep: bool,
  };

  let editing_flags_false = {
    editing_title: false,
    editing_prompt: false,
    editing_test_val_rep: false,
    editing_mut_test_rep: false,
    editing_impl_grd_rep: false,
    editing_module_name: false,
    editing_syntax_rep: false,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    spec: Exercise.spec, // The spec that the model will be reset to on ResetExercise
    /* We keep a separate editors field below (even though each cell technically also has its own editor)
       for two reasons:
          1. There are two synced cells that have the same internal `editor` model
          2. The editors need to be `stitched` together before any cell calculations can be done */
    editors: Exercise.p(Editor.t),
    cells: Exercise.stitched(CellEditor.Model.t),
    editing_flags,
  };

  let of_spec = (~settings as _, ~instructor_mode as _: bool, spec) => {
    let editors = Exercise.map(spec, Editor.Model.mk, Editor.Model.mk);
    let term_item_to_cell = (item: Exercise.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      Exercise.stitch_term(editors)
      |> Exercise.map_stitched(_ => term_item_to_cell);
    {
      spec,
      editors,
      cells,
      editing_flags: editing_flags_false,
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = Exercise.persistent_state;

  let persist = (exercise: t, ~instructor_mode: bool) =>
    Exercise.persist({eds: exercise.editors}, ~instructor_mode);

  let unpersist = (~instructor_mode, spec, persistent) => {
    let editors = Exercise.unpersist(~spec, ~instructor_mode, persistent).eds;
    let term_item_to_cell = (item: Exercise.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      Exercise.stitch_term(editors)
      |> Exercise.map_stitched(_ => term_item_to_cell);
    {
      spec,
      editors,
      cells,
      editing_flags: editing_flags_false,
    };
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type instructor =
    | EditingTitle
    | EditingPrompt
    | EditingTestValRep
    | EditingMutTestRep
    | EditingImplGrdRep
    | EditingModuleName
    | EditingSyntaxRep
    | UpdateTitle(string)
    | AddBuggyImplementation
    | DeleteBuggyImplementation(int)
    | UpdatePrompt(string)
    | UpdateTestValRep(int, int)
    | UpdateMutTestRep(int, list(string))
    | UpdateImplGrdRep(int, list(string))
    | UpdateSyntaxRep(list(string))
    | UpdateModuleName(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Editor(Exercise.pos, CellEditor.Update.t)
    | ResetEditor(Exercise.pos)
    | ResetExercise
    | Instructor(instructor);

  let instructor_update =
      (action: instructor, model: Model.t): Updated.t(Model.t) =>
    switch (action) {
    // TODO: replace Update.return when appropriate
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
    | EditingTestValRep =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_test_val_rep: !model.editing_flags.editing_test_val_rep,
        },
      })
    | EditingMutTestRep =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_mut_test_rep: !model.editing_flags.editing_mut_test_rep,
        },
      })
    | EditingImplGrdRep =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_impl_grd_rep: !model.editing_flags.editing_impl_grd_rep,
        },
      })
    | EditingModuleName =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_module_name: !model.editing_flags.editing_module_name,
        },
      })
    | EditingSyntaxRep =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_syntax_rep: !model.editing_flags.editing_syntax_rep,
        },
      })
    | UpdateTitle(title) =>
      Updated.return_quiet(
        {
          ...model,
          editors:
            Exercise.update_exercise_title({eds: model.editors}, title).eds,
        },
        ~is_edit=true,
      )
    | AddBuggyImplementation =>
      Updated.return({
        ...model,
        editors: Exercise.add_buggy_impl({eds: model.editors}).eds,
        cells: {
          ...model.cells,
          hidden_bugs:
            model.cells.hidden_bugs
            @ [CellEditor.Model.mk(Editor.Model.mk(Zipper.init()))],
        },
      })
    | DeleteBuggyImplementation(i) =>
      Updated.return({
        ...model,
        editors: Exercise.delete_buggy_impl({eds: model.editors}, i).eds,
        cells: {
          ...model.cells,
          hidden_bugs:
            List.filteri((j, _) => j != i, model.cells.hidden_bugs),
        },
      })
    | UpdatePrompt(prompt) =>
      Updated.return({
        ...model,
        editors:
          Exercise.update_exercise_prompt({eds: model.editors}, prompt).eds,
      })
    | UpdateTestValRep(test_num, dist) =>
      Updated.return({
        ...model,
        editors:
          Exercise.update_test_val_rep({eds: model.editors}, test_num, dist).
            eds,
      })
    | UpdateMutTestRep(test_num, new_hints) =>
      Updated.return({
        ...model,
        editors:
          Exercise.update_mut_test_rep(
            {eds: model.editors},
            test_num,
            new_hints,
          ).
            eds,
      })
    | UpdateSyntaxRep(new_hints) =>
      Updated.return({
        ...model,
        editors:
          Exercise.update_syntax_rep({eds: model.editors}, new_hints).eds,
      })
    | UpdateImplGrdRep(test_num, new_hints) =>
      Updated.return({
        ...model,
        editors:
          Exercise.update_impl_grd_rep(
            {eds: model.editors},
            test_num,
            new_hints,
          ).
            eds,
      })
    | UpdateModuleName(module_name) =>
      Updated.return({
        ...model,
        editors:
          Exercise.update_module_name({eds: model.editors}, module_name).eds,
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

  let can_undo = (action: t) => {
    switch (action) {
    | Editor(_, action) => CellEditor.Update.can_undo(action)
    | ResetEditor(_) => true
    | ResetExercise => true
    | Instructor(_) => false
    };
  };

  let update =
      (~settings: Settings.t, ~schedule_action as _, action, model: Model.t)
      : Updated.t(Model.t) => {
    let instructor_mode = settings.instructor_mode;
    switch (action) {
    | Editor(pos, MainEditor(action))
        when Exercise.visible_in(pos, ~instructor_mode) =>
      // Redirect to editors
      let editor =
        Exercise.main_editor_of_state(~selection=pos, model.editors);
      let* new_editor =
        // Hack[Matt]: put Editor.t into a CodeEditor.t to use its update function
        editor
        |> CodeEditable.Model.mk
        |> CodeEditable.Update.update(~settings, action);
      {
        ...model,
        editors:
          Exercise.put_main_editor(
            ~selection=pos,
            model.editors,
            new_editor.editor,
          ),
      };
    | Editor(pos, MainEditor(action)) =>
      switch (CodeSelectable.Update.convert_action(action)) {
      | Some(action) =>
        let editor =
          Exercise.main_editor_of_state(~selection=pos, model.editors);
        let* new_editor =
          // Hack[Matt]: put Editor.t into a CodeSelectable.t to use its update function
          editor
          |> CodeSelectable.Model.mk
          |> CodeSelectable.Update.update(~settings, action);
        {
          ...model,
          editors:
            Exercise.put_main_editor(
              ~selection=pos,
              model.editors,
              new_editor.editor,
            ),
        };
      | None => Updated.return_quiet(model)
      }
    | Editor(pos, ResultAction(_) as action)
        when
          Exercise.visible_in(pos, ~instructor_mode)
          || action
          |> (
            fun
            | ResultAction(UpdateResult(_)) => true
            | _ => false
          ) =>
      let cell = Exercise.get_stitched(pos, model.cells);
      let* new_cell = CellEditor.Update.update(~settings, action, cell);
      {
        ...model,
        cells: Exercise.put_stitched(pos, model.cells, new_cell),
      };
    | Editor(_, ResultAction(_)) => Updated.return_quiet(model) // TODO: I think this case should never happen
    | ResetEditor(pos) =>
      let spec = Exercise.main_editor_of_state(~selection=pos, model.spec);
      let new_editor = Editor.Model.mk(spec);
      {
        ...model,
        editors:
          Exercise.put_main_editor(~selection=pos, model.editors, new_editor),
      }
      |> Updated.return;
    | ResetExercise =>
      let new_editors =
        Exercise.map(model.spec, Editor.Model.mk, Editor.Model.mk);
      {
        ...model,
        editors: new_editors,
      }
      |> Updated.return;
    | Instructor(action) => instructor_update(~settings, action, model)
    };
  };

  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    let stitched_elabs = Exercise.stitch_term(model.editors);
    let worker_request = ref([]);
    let queue_worker = (pos, expr) => {
      worker_request :=
        worker_request^ @ [(pos |> Exercise.key_for_statics, expr)];
    };
    let cells =
      Exercise.map2_stitched(
        (pos, {term, editor}: Exercise.TermItem.t, cell: CellEditor.Model.t) =>
          {
            editor: {
              editor,
              statics: cell.editor.statics,
              dynamics: EvalResult.Model.dynamics(cell.result),
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
          let pos' = Exercise.pos_of_key(pos);
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
          Exercise.map_stitched(
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
    let editors: Exercise.p('a) = {
      let calculate = (statics, dynamics, ed) =>
        Editor.Update.calculate(~settings, statics, dynamics, ~is_edited, ed);

      {
        id: model.editors.id,
        title: model.editors.title,
        module_name: model.editors.module_name,
        prompt: model.editors.prompt,
        point_distribution: model.editors.point_distribution,
        prelude:
          calculate(
            cells.prelude.editor.statics,
            cells.prelude.editor.dynamics,
            model.editors.prelude,
          ),
        correct_impl:
          calculate(
            cells.test_validation.editor.statics,
            cells.test_validation.editor.dynamics,
            model.editors.correct_impl,
          ),
        your_tests: {
          tests:
            calculate(
              cells.user_tests.editor.statics,
              cells.user_tests.editor.dynamics,
              model.editors.your_tests.tests,
            ),
          required: model.editors.your_tests.required,
          provided: model.editors.your_tests.provided,
        },
        your_impl:
          calculate(
            cells.user_impl.editor.statics,
            cells.user_impl.editor.dynamics,
            model.editors.your_impl,
          ),
        hidden_bugs:
          List.map2(
            (cell: CellEditor.Model.t, editor: Exercise.wrong_impl('a)):
              Exercise.wrong_impl('a) =>
              {
                impl:
                  calculate(
                    cell.editor.statics,
                    cell.editor.dynamics,
                    editor.impl,
                  ),
                hint: editor.hint,
              },
            cells.hidden_bugs,
            model.editors.hidden_bugs,
          ),
        hidden_tests: {
          tests:
            calculate(
              cells.hidden_tests.editor.statics,
              cells.hidden_tests.editor.dynamics,
              model.editors.hidden_tests.tests,
            ),
          hints: model.editors.hidden_tests.hints,
        },
        syntax_tests: model.editors.syntax_tests,
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
    | Cell(Exercise.pos, CellEditor.Selection.t)
    | TextBox;

  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    switch (selection) {
    | Cell(pos, s) =>
      switch (Exercise.get_stitched(pos, model.cells)) {
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
      switch (Exercise.get_stitched(pos, model.cells)) {
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
      (~settings: Settings.t, id: Id.t, model: Model.t)
      : option((Update.t, t)) => {
    Exercise.positioned_editors(model.editors)
    |> List.find_opt(((p, e: Editor.t)) =>
         TermData.root_tile_opt(id, e.syntax.term_data) != None
         && Exercise.visible_in(p, ~instructor_mode=settings.instructor_mode)
       )
    |> Option.map(((pos, _)) =>
         (
           Update.Editor(pos, MainEditor(Perform(Jump(TileId(id))))),
           Cell(pos, CellEditor.Selection.MainEditor),
         )
       );
  };
};

module View = {
  type event =
    | MakeActive(Selection.t);

  /* The exercises mode interface for a single exercise. Composed of multiple editors and results. */

  /* This file follows conventions in [docs/ui-architecture.md] */

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
    let {
      test_validation,
      user_impl,
      user_tests,
      prelude,
      instructor,
      hidden_bugs,
      hidden_tests,
    }:
      Exercise.stitched('a) =
      model.cells;

    let stitched_tests =
      Exercise.map_stitched(
        (_, cell_editor: CellEditor.Model.t) =>
          cell_editor.result |> EvalResult.Model.test_results,
        model.cells,
      );

    let grading_report = Grading.GradingReport.mk(eds, ~stitched_tests);

    let score_view = Grading.GradingReport.view_overall_score(grading_report);

    let editor_view =
        (
          ~caption: string,
          ~subcaption: option(string)=?,
          ~result_kind=`NoResults,
          this_pos: Exercise.pos,
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
        ~caption=
          switch (this_pos) {
          | HiddenBugs(n) =>
            CellCommon.wrong_impl_caption(
              ~inject_delete=
                i => inject(Instructor(DeleteBuggyImplementation(i))),
              caption,
              n,
            )
          | _ => CellCommon.caption(caption, ~rest=?subcaption)
          },
        cell,
      );
    };

    let update_title = _ => {
      let new_title =
        Obj.magic(
          Js_of_ocaml.Js.some(JsUtil.get_elem_by_id("title-input-box")),
        )##.value;
      let update_events = [
        inject(Instructor(UpdateTitle(new_title))),
        inject(Instructor(EditingTitle)),
      ];
      Virtual_dom.Vdom.Effect.Many(update_events);
    };

    let title_view = {
      let title_placeholder =
        eds.title == "" ? "Untitled Exercise" : eds.title;
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
                            Attr.id("title-input-box"),
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

    let update_module_name = _ => {
      let new_module_name =
        Obj.magic(
          Js_of_ocaml.Js.some(JsUtil.get_elem_by_id("module-name-input")),
        )##.value;
      let update_events = [
        inject(Instructor(EditingModuleName)),
        inject(Instructor(UpdateModuleName(new_module_name))),
      ];
      Virtual_dom.Vdom.Effect.Many(update_events);
    };

    let module_name_view = {
      let module_placeholder =
        eds.module_name == "" ? "Unnamed Module" : eds.module_name;
      globals.settings.instructor_mode
        ? div(
            ~attrs=[Attr.class_("cell-module-name")],
            [
              editing_flags.editing_module_name
                ? div(
                    ~attrs=[Attr.class_("module-name-edit")],
                    [
                      label([text("Module name:")]),
                      input(
                        ~attrs=[
                          Attr.type_("text"),
                          Attr.class_("text-input"),
                          Attr.id("module-name-input"),
                          Attr.value(eds.module_name),
                          Attr.on_focus(_ => signal(MakeActive(TextBox))),
                        ],
                        (),
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [Widgets.button(Icons.confirm, update_module_name)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.cancel, _ =>
                            inject(Instructor(EditingModuleName))
                          ),
                        ],
                      ),
                    ],
                  )
                : div(
                    ~attrs=[Attr.class_("module-name-text")],
                    [
                      text("Module name: "),
                      div(
                        ~attrs=[
                          Attr.classes([
                            eds.module_name == "" ? "module-placeholder" : "",
                          ]),
                        ],
                        [text(module_placeholder)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.pencil, _ =>
                            inject(Instructor(EditingModuleName))
                          ),
                        ],
                      ),
                    ],
                  ),
            ],
          )
        : Node.none;
    };

    let update_prompt = _ => {
      let new_prompt =
        Obj.magic(
          Js_of_ocaml.Js.some(JsUtil.get_elem_by_id("prompt-input-box")),
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
                              Attr.id("prompt-input-box"),
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

    let prelude_view =
      Always(
        editor_view(
          Prelude,
          prelude,
          ~subcaption=globals.settings.instructor_mode ? "" : " (Read-Only)",
          ~caption="Prelude",
        ),
      );

    let correct_impl_view =
      InstructorOnly(
        () =>
          editor_view(
            CorrectImpl,
            instructor,
            ~caption="Correct Implementation",
          ),
      );

    // determine trailing hole
    // TODO: module
    let correct_impl_ctx_view =
      Always(
        {
          let exp_ctx_view = {
            let correct_impl_trailing_hole_ctx =
              Haz3lcore.Editor.Model.trailing_hole_ctx(
                eds.correct_impl,
                instructor.editor.statics.info_map,
              );
            let prelude_trailing_hole_ctx =
              Haz3lcore.Editor.Model.trailing_hole_ctx(
                eds.prelude,
                prelude.editor.statics.info_map,
              );
            switch (correct_impl_trailing_hole_ctx, prelude_trailing_hole_ctx) {
            | (None, _) => Node.div([text("No context available (1)")])
            | (_, None) => Node.div([text("No context available (2)")]) // TODO show exercise configuration error
            | (
                Some(correct_impl_trailing_hole_ctx),
                Some(prelude_trailing_hole_ctx),
              ) =>
              let specific_ctx =
                Language.Ctx.subtract_prefix(
                  correct_impl_trailing_hole_ctx,
                  prelude_trailing_hole_ctx,
                );
              switch (specific_ctx) {
              | None => Node.div([text("No context available")]) // TODO show exercise configuration error
              | Some(specific_ctx) =>
                ContextInspector.ctx_view(~globals, specific_ctx)
              };
            };
          };
          CellCommon.simple_cell_view([
            CellCommon.simple_cell_item([
              CellCommon.caption(
                "Correct Implementation",
                ~rest=" (Type Signatures Only)",
              ),
              exp_ctx_view,
            ]),
          ]);
        },
      );

    let rm_probe_data = (editor: CellEditor.Model.t): CellEditor.Model.t => {
      editor: {
        editor: editor.editor.editor,
        statics: editor.editor.statics,
        dynamics: Language.Dynamics.Map.empty,
      },
      result: editor.result,
    };

    let your_tests_view = {
      let subcaption =
        globals.settings.instructor_mode
          ? ": Student Tests vs. Correct Implementation"
          : ": Your Tests vs. Correct Implementation";
      Always(
        editor_view(
          YourTestsValidation,
          // Remove probe data from this cell to prevent data leaks from correct implementation
          rm_probe_data(test_validation),
          ~caption="Test Validation",
          ~subcaption,
          ~result_kind=
            `Custom(
              Grading.TestValidationReport.view(
                ~globals,
                ~signal_jump=
                  id =>
                    inject(
                      Editor(
                        YourTestsValidation,
                        MainEditor(Perform(Jump(TileId(id)))),
                      ),
                    ),
                ~signal_editing_test_val_rep=
                  inject(Instructor(EditingTestValRep)),
                ~signal_update_test_val=
                  (x, y) => inject(Instructor(UpdateTestValRep(x, y))),
                ~signal_textbox_active=signal(MakeActive(TextBox)),
                ~editing_test_val_rep=editing_flags.editing_test_val_rep,
                grading_report.test_validation_report,
                grading_report.point_distribution.test_validation,
                eds.your_tests.required,
              ),
            ),
        ),
      );
    };

    let wrong_impl_views =
      List.mapi(
        (i, (_, cell)) => {
          editor_view(
            HiddenBugs(i),
            cell,
            ~caption="Mutant " ++ string_of_int(i + 1),
          )
        },
        List.combine(eds.hidden_bugs, hidden_bugs),
      );

    let add_wrong_impl_view =
      CellCommon.simple_cell_view([
        CellCommon.simple_cell_item([
          div(
            ~attrs=[Attr.class_("wrong-impl-cell-caption")],
            [
              div(
                ~attrs=[
                  Attr.class_("instructor-edit-icon"),
                  Attr.id("add-icon"),
                ],
                [
                  Widgets.button(
                    Icons.add,
                    _ =>
                      Ui_effect.Many([
                        inject(Instructor(AddBuggyImplementation)),
                        signal(
                          MakeActive(
                            Cell(
                              HiddenBugs(List.length(hidden_bugs)),
                              MainEditor,
                            ),
                          ),
                        ),
                      ]),
                    ~tooltip="Add Buggy Implementation",
                  ),
                ],
              ),
            ],
          ),
        ]),
      ]);

    let mutation_testing_view =
      Always(
        Grading.MutationTestingReport.view(
          ~globals,
          ~editing_mut_test_rep=editing_flags.editing_mut_test_rep,
          ~inject_editing_mut_test_rep=inject(Instructor(EditingMutTestRep)),
          ~inject_update_mut_test_rep=
            (x, y) => inject(Instructor(UpdateMutTestRep(x, y))),
          ~select_textbox=signal(MakeActive(TextBox)),
          grading_report.mutation_testing_report,
          grading_report.point_distribution.mutation_testing,
        ),
      );

    let your_impl_view = {
      let caption =
        globals.settings.instructor_mode
          ? "Student's Implementation" : "Your Implementation";
      Always(
        editor_view(YourImpl, user_impl, ~caption, ~result_kind=`EvalResults),
      );
    };

    let syntax_grading_view =
      Always(
        Grading.SyntaxReport.view(
          ~globals,
          ~editing_syntax_rep=editing_flags.editing_syntax_rep,
          ~inject_set_editing_syntax_rep=
            inject(Instructor(EditingSyntaxRep)),
          ~inject_update_syntax_rep=
            hints => inject(Instructor(UpdateSyntaxRep(hints))),
          ~select_textbox=signal(MakeActive(TextBox)),
          grading_report.syntax_report,
        ),
      );

    let impl_validation_view = {
      let subcaption =
        globals.settings.instructor_mode
          ? ": Student's Tests vs. Student's Implementation"
          : ": Your Tests (code synchronized with Test Validation cell above) vs. Your Implementation";
      Always(
        editor_view(
          YourTestsTesting,
          user_tests,
          ~caption="Implementation Validation",
          ~subcaption,
          ~result_kind=`TestResults,
        ),
      );
    };

    let hidden_tests_view =
      InstructorOnly(
        () => editor_view(HiddenTests, hidden_tests, ~caption="Hidden Tests"),
      );

    let impl_grading_view =
      Always(
        Grading.ImplGradingReport.view(
          ~globals,
          ~signal_jump=
            id =>
              inject(
                Editor(
                  YourTestsTesting,
                  MainEditor(Perform(Jump(TileId(id)))),
                ),
              ),
          ~inject_set_editing_impl_grd_rep=
            inject(Instructor(EditingImplGrdRep)),
          ~inject_update_impl_grd_rep=
            (x, y) => inject(Instructor(UpdateImplGrdRep(x, y))),
          ~select_textbox=signal(MakeActive(TextBox)),
          ~editing_impl_grd_rep=editing_flags.editing_impl_grd_rep,
          ~report=grading_report.impl_grading_report,
          ~syntax_report=grading_report.syntax_report,
          ~max_points=grading_report.point_distribution.impl_grading,
        ),
      );

    let wrong_impl_views =
      InstructorOnly(
        () =>
          CellCommon.simple_cell_view([
            CellCommon.simple_cell_item(
              [CellCommon.caption("Mutation Tests")]
              @ wrong_impl_views
              @ [add_wrong_impl_view],
            ),
          ]),
      );

    [score_view, title_view, module_name_view, prompt_view]
    @ render_cells(
        globals.settings,
        [
          prelude_view,
          correct_impl_view,
          correct_impl_ctx_view,
          your_tests_view,
          wrong_impl_views,
        ]
        @ [
          mutation_testing_view,
          your_impl_view,
          syntax_grading_view,
          impl_validation_view,
          hidden_tests_view,
          impl_grading_view,
        ],
      );
  };
};
