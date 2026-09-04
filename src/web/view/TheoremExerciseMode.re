open Haz3lcore;
open Util_web;

/* The exercises mode interface for a theorem exercise. Composed of multiple editors and results. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cells = {
    prelude: CellEditor.Model.t,
    lemmas: CellEditor.Model.t,
    theorem: CellEditor.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type editing_flags = {
    editing_title: bool,
    editing_module_name: bool,
    editing_prompt: bool,
  };

  let editing_flags_false = {
    editing_title: false,
    editing_module_name: false,
    editing_prompt: false,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    title: string,
    module_name: string,
    prompt: string,
    max_points: int,
    cells,
    editing_flags,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    lemmas: CellEditor.Model.persistent,
    theorem: EvalResult.Model.persistent,
  };

  let persist = (model: t): persistent => {
    lemmas: model.cells.lemmas |> CellEditor.Model.persist,
    theorem: model.cells.theorem.result |> EvalResult.Model.persist,
  };

  let unpersist =
      (~settings, spec: TheoremExercise.spec, persistent: persistent): t => {
    {
      id: spec.id,
      title: spec.title,
      module_name: spec.module_name,
      prompt: spec.prompt,
      max_points: spec.max_points,
      cells: {
        prelude:
          CellEditor.Model.mk(Editor.Model.mk(spec.prelude, ~root=Exp)),
        lemmas: persistent.lemmas |> CellEditor.Model.unpersist(~settings),
        theorem: {
          editor:
            CellEditor.Model.mk(Editor.Model.mk(spec.theorem, ~root=Exp)).
              editor,
          result: persistent.theorem |> EvalResult.Model.unpersist,
        },
      },
      editing_flags: editing_flags_false,
    };
  };

  let of_spec = (spec: TheoremExercise.spec): t => {
    {
      id: spec.id,
      title: spec.title,
      module_name: spec.module_name,
      prompt: spec.prompt,
      max_points: spec.max_points,
      cells: {
        prelude:
          CellEditor.Model.mk(Editor.Model.mk(spec.prelude, ~root=Exp)),
        lemmas: CellEditor.Model.mk(Editor.Model.mk(spec.lemmas, ~root=Exp)),
        theorem:
          CellEditor.Model.mk(Editor.Model.mk(spec.theorem, ~root=Exp)),
      },
      editing_flags: editing_flags_false,
    };
  };

  let spec_of_t = (model: t): TheoremExercise.spec => {
    {
      id: model.id,
      title: model.title,
      module_name: model.module_name,
      prompt: model.prompt,
      max_points: model.max_points,
      prelude: model.cells.prelude.editor.editor.state.zipper,
      lemmas: model.cells.lemmas.editor.editor.state.zipper,
      theorem: model.cells.theorem.editor.editor.state.zipper,
    };
  };

  let export_module = (model: t): string => {
    let prefix = "let exercise : Exercise.t = Theorem\n";
    let spec = spec_of_t(model);
    prefix ++ TheoremExercise.show_spec(spec) ++ "\n";
  };

  /* Editors whose problems should appear in the Problems sidebar. All three
     cells are always rendered (Prelude read-only, Theorem prove-only) and
     all are jumpable, so all are listed. */
  let get_problem_editors =
      (model: t): list((option(string), list(CodeEditable.Model.t))) => [
    (Some("Prelude"), [model.cells.prelude.editor]),
    (Some("Lemmas"), [model.cells.lemmas.editor]),
    (Some("Theorem"), [model.cells.theorem.editor]),
  ];
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type instructor =
    | EditingTitle
    | EditingModuleName
    | EditingPrompt
    | UpdateTitle(string)
    | UpdateModuleName(string)
    | UpdatePrompt(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Instructor(instructor)
    | Prelude(CellEditor.Update.t)
    | Lemmas(CellEditor.Update.t)
    | Theorem(CellEditor.Update.t)
    | RefreshStatics;

  let instructor_update =
      (action: instructor, model: Model.t): Updated.t(Model.t) => {
    switch (action) {
    | EditingTitle =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_title: !model.editing_flags.editing_title,
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
    | EditingPrompt =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_prompt: !model.editing_flags.editing_prompt,
        },
      })
    | UpdateTitle(title) =>
      Updated.return({
        ...model,
        title,
      })
    | UpdateModuleName(module_name) =>
      Updated.return({
        ...model,
        module_name,
      })
    | UpdatePrompt(prompt) =>
      Updated.return({
        ...model,
        prompt,
      })
    };
  };

  let instructor_update =
      (~settings: Settings.t, action: instructor, model: Model.t)
      : Updated.t(Model.t) =>
    if (settings.instructor_mode) {
      instructor_update(action, model);
    } else {
      Updated.return_quiet(model);
    };

  let update = (~settings: Settings.t, action: t, model: Model.t) => {
    switch (action) {
    | Instructor(action) => instructor_update(~settings, action, model)
    | Prelude(action) when settings.instructor_mode =>
      let* new_cell =
        CellEditor.Update.update(~settings, action, model.cells.prelude);
      {
        ...model,
        cells: {
          ...model.cells,
          prelude: new_cell,
        },
      };
    | Prelude(MainEditor(action))
        when CodeSelectable.Update.convert_action(action) != None =>
      let* new_cell =
        CellEditor.Update.update(
          ~settings,
          MainEditor(action),
          model.cells.prelude,
        );
      {
        ...model,
        cells: {
          ...model.cells,
          prelude: new_cell,
        },
      };
    | Prelude(_) =>
      print_endline("Instructor-only action");
      Updated.return_quiet(model);
    | Lemmas(action) =>
      let* new_cell =
        CellEditor.Update.update(~settings, action, model.cells.lemmas);
      {
        ...model,
        cells: {
          ...model.cells,
          lemmas: new_cell,
        },
      };
    | Theorem(action) when settings.instructor_mode =>
      let* new_cell =
        CellEditor.Update.update(~settings, action, model.cells.theorem);
      {
        ...model,
        cells: {
          ...model.cells,
          theorem: new_cell,
        },
      };
    | Theorem(MainEditor(action))
        when CodeSelectable.Update.convert_action(action) != None =>
      let* new_cell =
        CellEditor.Update.update(
          ~settings,
          MainEditor(action),
          model.cells.theorem,
        );
      {
        ...model,
        cells: {
          ...model.cells,
          theorem: new_cell,
        },
      };
    | Theorem(ResultAction(action)) =>
      let* new_cell =
        CellEditor.Update.update(
          ~settings,
          ResultAction(action),
          model.cells.theorem,
        );
      {
        ...model,
        cells: {
          ...model.cells,
          theorem: new_cell,
        },
      };
    | Theorem(MainEditor(_)) =>
      print_endline("Instructor-only action");
      Updated.return_quiet(model);
    | RefreshStatics =>
      CodeWithStatics.StaticsDebounce.force_on_next := true;
      model |> Updated.return_quiet(~recalculate=true);
    };
  };

  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    let statics_mode =
      CodeWithStatics.StaticsDebounce.consume(~is_edited, ~schedule_refresh=() =>
        schedule_action(RefreshStatics)
      );

    // Work out the terms
    let just_prelude_term =
      MakeTerm.from_zip_for_sem(
        model.cells.prelude.editor.editor.state.zipper,
        ~root=Exp,
      ).
        term;
    let just_lemmas_term =
      MakeTerm.from_zip_for_sem(
        model.cells.lemmas.editor.editor.state.zipper,
        ~root=Exp,
      ).
        term;
    let just_theorem_term =
      MakeTerm.from_zip_for_sem(
        model.cells.theorem.editor.editor.state.zipper,
        ~root=Exp,
      ).
        term;

    let stitched_scratch =
      EditorUtil.append_exp(just_prelude_term, just_lemmas_term);
    let stitched_theorem =
      stitched_scratch
      |> EditorUtil.append_exp(
           _,
           just_prelude_term
           |> Language.ProofHacks.strip_theorems
           |> Language.Exp.replace_all_ids,
         )
      |> EditorUtil.append_exp(_, just_theorem_term);

    // Worker Setup
    let worker_request: ref(list((string, WorkerServer.Request.value))) =
      ref([]);
    let queue_worker = (pos, req_value: WorkerServer.Request.value) => {
      worker_request := worker_request^ @ [(pos, req_value)];
    };

    // Calculate each cell
    let cells: Model.cells =
      Model.{
        prelude:
          model.cells.prelude
          |> CellEditor.Update.calculate(
               ~settings,
               ~is_edited,
               ~statics_mode,
               ~queue_worker=Some(queue_worker("prelude")),
               ~stitch=_ =>
               just_prelude_term
             ),
        lemmas:
          model.cells.lemmas
          |> CellEditor.Update.calculate(
               ~settings,
               ~is_edited,
               ~statics_mode,
               ~queue_worker=Some(queue_worker("lemmas")),
               ~stitch=_ =>
               stitched_scratch
             ),
        theorem:
          model.cells.theorem
          |> CellEditor.Update.calculate(
               ~settings,
               ~is_edited,
               ~statics_mode,
               ~queue_worker=Some(queue_worker("theorem")),
               ~stitch=_ =>
               stitched_theorem
             ),
      };

    // Send to worker

    let dispatch = (key, action) =>
      switch (key) {
      | "lemmas" =>
        schedule_action(Prelude(ResultAction(action)));
        schedule_action(Lemmas(ResultAction(action)));
      | "theorem" => schedule_action(Theorem(ResultAction(action)))
      | _ => ()
      };
    EvalRequest.request(
      worker_request^,
      ~pos_of_key=key => key,
      ~dispatch,
      ~on_timeout=
        List.iter(((key, _)) =>
          dispatch(key, UpdateResult(ResultFail(Timeout)))
        ),
    );

    {
      ...model,
      cells,
    };
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TextBox
    | Prelude(CellEditor.Selection.t)
    | Lemmas(CellEditor.Selection.t)
    | Theorem(CellEditor.Selection.t);

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection: t, model: Model.t)
      : cursor(Update.t) => {
    switch (selection) {
    | TextBox => Cursor.empty
    | Prelude(s) =>
      let+ a =
        CellEditor.Selection.get_cursor_info(
          ~inject=a => inject(Prelude(a)),
          ~selection=s,
          model.cells.prelude,
        );
      Update.Prelude(a);
    | Lemmas(s) =>
      let+ a =
        CellEditor.Selection.get_cursor_info(
          ~inject=a => inject(Lemmas(a)),
          ~selection=s,
          model.cells.lemmas,
        );
      Update.Lemmas(a);
    | Theorem(s) =>
      let+ a =
        CellEditor.Selection.get_cursor_info(
          ~inject=a => inject(Theorem(a)),
          ~selection=s,
          model.cells.theorem,
        );
      Update.Theorem(a);
    };
  };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) => {
    open OptUtil.Syntax;
    let (let.or) = (v: option('b), f: unit => option('b)) => {
      switch (v) {
      | Some(x) => Some(x)
      | None => f()
      };
    };

    let.or () = {
      let* _ =
        TermData.root_piece(
          tile,
          model.cells.prelude.editor.editor.syntax.term_data,
        );
      Some((
        Update.Prelude(MainEditor(Perform(Move(Goal(TileId(tile)))))),
        Prelude(CellEditor.Selection.MainEditor),
      ));
    };
    let.or () = {
      let* _ =
        TermData.root_piece(
          tile,
          model.cells.lemmas.editor.editor.syntax.term_data,
        );
      Some((
        Update.Lemmas(MainEditor(Perform(Move(Goal(TileId(tile)))))),
        Lemmas(CellEditor.Selection.MainEditor),
      ));
    };

    let* _ =
      TermData.root_piece(
        tile,
        model.cells.theorem.editor.editor.syntax.term_data,
      );
    Some((
      Update.Theorem(MainEditor(Perform(Move(Goal(TileId(tile)))))),
      Theorem(CellEditor.Selection.MainEditor),
    ));
  };
};

module View = {
  let view =
      (
        ~globals: Globals.t,
        ~take_focus: Selection.t => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~inject_explainthis: ExplainThisUpdate.update => Ui_effect.t(unit),
        ~selection: option(Selection.t),
        model: Model.t,
      ) => {
    let editing_flags = model.editing_flags;
    let on_focus_textbox = _ => take_focus(TextBox);

    let title_view =
      InstructorEditViews.title_view(
        ~instructor_mode=globals.settings.instructor_mode,
        ~is_editing=editing_flags.editing_title,
        ~title=model.title,
        ~on_focus_textbox,
        ~toggle_editing=_ => inject(Instructor(EditingTitle)),
        ~update_title=t => inject(Instructor(UpdateTitle(t))),
      );

    let module_name_view =
      InstructorEditViews.module_name_view(
        ~instructor_mode=globals.settings.instructor_mode,
        ~is_editing=editing_flags.editing_module_name,
        ~module_name=model.module_name,
        ~on_focus_textbox,
        ~toggle_editing=_ => inject(Instructor(EditingModuleName)),
        ~update_module_name=m => inject(Instructor(UpdateModuleName(m))),
      );

    let prompt_view =
      InstructorEditViews.prompt_view(
        ~globals,
        ~inject_explainthis,
        ~instructor_mode=globals.settings.instructor_mode,
        ~is_editing=editing_flags.editing_prompt,
        ~prompt=model.prompt,
        ~on_focus_textbox,
        ~toggle_editing=_ => inject(Instructor(EditingPrompt)),
        ~update_prompt=p => inject(Instructor(UpdatePrompt(p))),
      );

    let prelude_view =
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => take_focus(Prelude(a)),
        ~selected=
          switch (selection) {
          | Some(Prelude(s)) => Some(s)
          | _ => None
          },
        ~inject=a => inject(Prelude(a)),
        ~result_kind=`NoResults,
        ~caption=CellCommon.caption("Prelude (Read-Only)"),
        model.cells.prelude,
      );

    let lemmas_view =
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => take_focus(Lemmas(a)),
        ~selected=
          switch (selection) {
          | Some(Lemmas(s)) => Some(s)
          | _ => None
          },
        ~inject=a => inject(Lemmas(a)),
        ~result_kind=`NoTheorems,
        ~caption=CellCommon.caption("Lemmas / Scratch Space"),
        model.cells.lemmas,
      );

    let theorem_view =
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => take_focus(Theorem(a)),
        ~selected=
          switch (selection) {
          | Some(Theorem(s)) => Some(s)
          | _ => None
          },
        ~inject=a => inject(Theorem(a)),
        ~result_kind=`JustTheorems,
        ~caption=CellCommon.caption("Theorem (Prove-Only)"),
        model.cells.theorem,
      );

    let score_view =
      Grading.score_view(
        Theorems.Model.get_score(model.cells.theorem.result.theorems)
        |> Option.value(~default=(Float.nan, Float.nan)),
      );

    [
      score_view,
      title_view,
      module_name_view,
      prompt_view,
      prelude_view,
      lemmas_view,
      theorem_view,
    ];
  };
};
