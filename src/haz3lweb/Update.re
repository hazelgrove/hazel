open Haz3lcore;

include UpdateAction; // to prevent circularity

let update_settings =
    (a: settings_action, {settings, _} as model: Model.t): Model.t =>
  switch (a) {
  | Statics =>
    /* NOTE: dynamics depends on statics, so if dynamics is on and
       we're turning statics off, turn dynamics off as well */
    {
      ...model,
      settings: {
        ...settings,
        core: {
          statics: !settings.core.statics,
          assist: !settings.core.statics,
          elaborate: settings.core.elaborate,
          dynamics: !settings.core.statics && settings.core.dynamics,
          evaluation: settings.core.evaluation,
        },
      },
    }
  | Elaborate => {
      ...model,
      settings: {
        ...settings,
        core: {
          statics: !settings.core.elaborate || settings.core.statics,
          assist: settings.core.assist,
          elaborate: !settings.core.elaborate,
          dynamics: settings.core.dynamics,
          evaluation: settings.core.evaluation,
        },
      },
    }
  | Dynamics => {
      ...model,
      settings: {
        ...settings,
        core: {
          statics: !settings.core.dynamics || settings.core.statics,
          assist: settings.core.assist,
          elaborate: settings.core.elaborate,
          dynamics: !settings.core.dynamics,
          evaluation: settings.core.evaluation,
        },
      },
    }
  | Assist => {
      ...model,
      settings: {
        ...settings,
        core: {
          statics: !settings.core.assist || settings.core.statics,
          assist: !settings.core.assist,
          elaborate: settings.core.elaborate,
          dynamics: settings.core.dynamics,
          evaluation: settings.core.evaluation,
        },
      },
    }
  | Evaluation(u) =>
    let evaluation = settings.core.evaluation;
    let evaluation' = {
      switch (u) {
      | ShowRecord => {
          ...evaluation,
          stepper_history: !evaluation.stepper_history,
        }
      | ShowCaseClauses => {
          ...evaluation,
          show_case_clauses: !evaluation.show_case_clauses,
        }
      | ShowFnBodies => {
          ...evaluation,
          show_fn_bodies: !evaluation.show_fn_bodies,
        }
      | ShowCasts => {...evaluation, show_casts: !evaluation.show_casts}
      | ShowFixpoints => {
          ...evaluation,
          show_fixpoints: !evaluation.show_fixpoints,
        }
      | ShowLookups => {
          ...evaluation,
          show_lookup_steps: !evaluation.show_lookup_steps,
        }
      | ShowFilters => {
          ...evaluation,
          show_stepper_filters: !evaluation.show_stepper_filters,
        }
      | ShowSettings => {
          ...evaluation,
          show_settings: !evaluation.show_settings,
        }
      | ShowHiddenSteps => {
          ...evaluation,
          show_hidden_steps: !evaluation.show_hidden_steps,
        }
      };
    };
    {
      ...model,
      settings: {
        ...settings,
        core: {
          ...settings.core,
          evaluation: evaluation',
        },
      },
    };
  | ExplainThis(ToggleShow) =>
    let explainThis = {
      ...settings.explainThis,
      show: !settings.explainThis.show,
    };
    let settings = {...settings, explainThis};
    {...model, settings};
  | ExplainThis(ToggleShowFeedback) =>
    let explainThis = {
      ...settings.explainThis,
      show_feedback: !settings.explainThis.show_feedback,
    };
    let settings = {...settings, explainThis};
    {...model, settings};
  | ExplainThis(SetHighlight(a)) =>
    let highlight: ExplainThisModel.Settings.highlight =
      switch (a, settings.explainThis.highlight) {
      | (Toggle, All) => NoHighlight
      | (Toggle, _) => All
      | (Hover(_), All) => All
      | (Hover(id), _) => One(id)
      | (UnsetHover, All) => All
      | (UnsetHover, _) => NoHighlight
      };
    let explainThis = {...settings.explainThis, highlight};
    let settings = {...settings, explainThis};
    {...model, settings};
  | Benchmark => {
      ...model,
      settings: {
        ...settings,
        benchmark: !settings.benchmark,
      },
    }
  | Captions => {
      ...model,
      settings: {
        ...settings,
        captions: !settings.captions,
      },
    }
  | SecondaryIcons => {
      ...model,
      settings: {
        ...settings,
        secondary_icons: !settings.secondary_icons,
      },
    }
  | ContextInspector => {
      ...model,
      settings: {
        ...settings,
        context_inspector: !settings.context_inspector,
      },
    }
  | InstructorMode =>
    let new_mode = !settings.instructor_mode;
    {
      ...model,
      editors: Editors.set_instructor_mode(model.editors, new_mode),
      settings: {
        ...settings,
        instructor_mode: !settings.instructor_mode,
      },
    };
  | EditingPrompt =>
    let editing = !settings.editing_prompt;
    {
      ...model,
      editors: Editors.set_editing_prompt(model.editors, editing),
      settings: {
        ...settings,
        editing_prompt: editing,
      },
    };
  | Mode(mode) => {
      ...model,
      settings: {
        ...settings,
        mode,
      },
    }
  };

let schedule_evaluation = (~schedule_action, model: Model.t): unit =>
  if (model.settings.core.dynamics) {
    let elabs =
      Editors.get_spliced_elabs(
        ~settings=model.settings,
        model.statics,
        model.editors,
      );
    let eval_rs = ModelResults.to_evaluate(model.results, elabs);
    if (!ModelResults.is_empty(eval_rs)) {
      schedule_action(UpdateResult(eval_rs));
      WorkerClient.request(
        eval_rs,
        ~handler=rs => schedule_action(UpdateResult(rs)),
        ~timeout=
          rqs =>
            schedule_action(UpdateResult(ModelResults.timeout_all(rqs))),
      );
    };
    /* Not sending stepper to worker for now bc closure perf */
    let step_rs = ModelResults.to_step(model.results);
    if (!ModelResults.is_empty(step_rs)) {
      let new_rs =
        step_rs
        |> ModelResults.update_elabs(
             ~settings=model.settings.core.evaluation,
             elabs,
           )
        |> ModelResults.run_pending(~settings=model.settings.core);
      schedule_action(UpdateResult(new_rs));
    };
  };

let update_cached_data = (~schedule_action, update, m: Model.t): Model.t => {
  let update_statics = is_edit(update) || reevaluate_post_update(update);
  let update_dynamics = reevaluate_post_update(update);
  let m =
    update_statics || update_dynamics && m.settings.core.statics
      ? {...m, statics: Editors.mk_statics(~settings=m.settings, m.editors)}
      : m;
  if (update_dynamics && m.settings.core.dynamics) {
    schedule_evaluation(~schedule_action, m);
    m;
  } else {
    m;
  };
};

let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) =>
  switch (
    model.editors
    |> Editors.get_editor
    |> Haz3lcore.Perform.go(~settings=model.settings.core, a)
  ) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok(ed) =>
    let model = {...model, editors: Editors.put_editor(ed, model.editors)};
    /* Note: Not saving here as saving is costly to do each keystroke,
       we wait a second after the last edit action (see Main.re) */
    Ok(model);
  };

let switch_scratch_slide =
    (editors: Editors.t, ~instructor_mode, idx: int): option(Editors.t) =>
  switch (editors) {
  | Documentation(_) => None
  | Scratch(n, _) when n == idx => None
  | Scratch(_, slides) when idx >= List.length(slides) => None
  | Scratch(_, slides) => Some(Scratch(idx, slides))
  | Exercises(_, specs, _) when idx >= List.length(specs) => None
  | Exercises(_, specs, _) =>
    let spec = List.nth(specs, idx);
    let key = Exercise.key_of(spec);
    let exercise = Store.Exercise.load_exercise(key, spec, ~instructor_mode);
    Some(Exercises(idx, specs, exercise));
  };

let switch_exercise_editor =
    (editors: Editors.t, ~pos, ~instructor_mode): option(Editors.t) =>
  switch (editors) {
  | Documentation(_)
  | Scratch(_) => None
  | Exercises(m, specs, exercise) =>
    let exercise = Exercise.switch_editor(~pos, instructor_mode, ~exercise);
    //Note: now saving after each edit (delayed by 1 second) so no need to save here
    //Store.Exercise.save_exercise(exercise, ~instructor_mode);
    Some(Exercises(m, specs, exercise));
  };

/* This action saves a file which serializes all current editor
   settings, including the states of all Scratch and Example slides.
   This saved file can directly replace Haz3lweb/Init.ml, allowing
   you to make your current state the default startup state.

   This does NOT save any Exercises mode state or any langdocs
   state. The latter is intentional as we don't want to persist
   this between users. The former is a TODO, currently difficult
   due to the more complex architecture of Exercises. */
let export_persistent_data = () => {
  let settings = Store.Settings.load();
  let data: PersistentData.t = {
    documentation:
      Store.Documentation.load(~settings=settings.core.evaluation)
      |> Store.Documentation.to_persistent,
    scratch:
      Store.Scratch.load(~settings=settings.core.evaluation)
      |> Store.Scratch.to_persistent,
    settings,
  };
  let contents =
    "let startup : PersistentData.t = " ++ PersistentData.show(data);
  JsUtil.download_string_file(
    ~filename="Init.ml",
    ~content_type="text/plain",
    ~contents,
  );
  print_endline("INFO: Persistent data exported to Init.ml");
};

let ui_state_update =
    (ui_state: Model.ui_state, update: set_meta, ~schedule_action as _)
    : Model.ui_state => {
  switch (update) {
  | Mousedown => {...ui_state, mousedown: true}
  | Mouseup => {...ui_state, mousedown: false}
  | ShowBackpackTargets(b) => {...ui_state, show_backpack_targets: b}
  | FontMetrics(font_metrics) => {...ui_state, font_metrics}
  };
};

let rec apply =
        (model: Model.t, update: t, state: State.t, ~schedule_action)
        : Result.t(Model.t) => {
  let m: Result.t(Model.t) =
    switch (update) {
    | Reset => Ok(Model.reset(model))
    | Set(Evaluation(_) as s_action) => Ok(update_settings(s_action, model))
    | Set(s_action) =>
      let model = update_settings(s_action, model);
      Model.save(model);
      switch (update) {
      // NOTE: Load here necessary to load editors on switching mode
      | Set(Mode(_)) => Ok(Model.load(model))
      | _ => Ok(model)
      };
    | SetMeta(action) =>
      let ui_state =
        ui_state_update(model.ui_state, action, ~schedule_action);
      Ok({...model, ui_state});
    | UpdateExplainThisModel(u) =>
      let explainThisModel =
        ExplainThisUpdate.set_update(model.explainThisModel, u);
      Model.save_and_return({...model, explainThisModel});
    | DebugConsole(key) =>
      DebugConsole.print(model, key);
      Ok(model);
    | Save => Model.save_and_return(model)
    | InitImportAll(file) =>
      JsUtil.read_file(file, data => schedule_action(FinishImportAll(data)));
      Ok(model);
    | FinishImportAll(data) =>
      switch (data) {
      | None => Ok(model)
      | Some(data) =>
        Export.import_all(data, ~specs=ExerciseSettings.exercises);
        Ok(Model.load(model));
      }
    | InitImportScratchpad(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(FinishImportScratchpad(data))
      );
      Ok(model);
    | FinishImportScratchpad(data) =>
      let editors = Editors.import_current(model.editors, data);
      Model.save_and_return({...model, editors});
    | ExportPersistentData =>
      export_persistent_data();
      Ok(model);
    | ResetCurrentEditor =>
      let instructor_mode = model.settings.instructor_mode;
      let editors = Editors.reset_current(model.editors, ~instructor_mode);
      Model.save_and_return({...model, editors});
    | SwitchScratchSlide(n) =>
      let instructor_mode = model.settings.instructor_mode;
      switch (switch_scratch_slide(model.editors, ~instructor_mode, n)) {
      | None => Error(FailedToSwitch)
      | Some(editors) => Model.save_and_return({...model, editors})
      };
    | SwitchDocumentationSlide(name) =>
      switch (Editors.switch_example_slide(model.editors, name)) {
      | None => Error(FailedToSwitch)
      | Some(editors) => Model.save_and_return({...model, editors})
      }
    | SwitchEditor(pos) =>
      let instructor_mode = model.settings.instructor_mode;
      switch (switch_exercise_editor(model.editors, ~pos, ~instructor_mode)) {
      | None => Error(FailedToSwitch)
      | Some(editors) => Ok({...model, editors})
      };
    | TAB =>
      /* Attempt to act intelligently when TAB is pressed.
       * TODO(andrew): Consider more advanced TAB logic. Instead
       * of simply moving to next hole, if the backpack is non-empty
       * but can't immediately put down, move to next position of
       * interest, which is closet of: nearest position where can
       * put down, farthest position where can put down, next hole */
      let z =
        model.editors
        |> Editors.get_editor
        |> ((ed: Editor.t) => ed.state.zipper);
      let a =
        Selection.is_buffer(z.selection)
          ? Assistant(AcceptSuggestion)
          : Zipper.can_put_down(z)
              ? PerformAction(Put_down) : MoveToNextHole(Right);
      apply(model, a, state, ~schedule_action);
    | PerformAction(a)
        when model.settings.core.assist && model.settings.core.statics =>
      let model = UpdateAssistant.reset_buffer(model);
      switch (perform_action(model, a)) {
      | Ok(model) when Action.is_edit(a) =>
        UpdateAssistant.apply(
          model,
          Prompt(TyDi),
          ~schedule_action,
          ~state,
          ~main=apply,
        )
      | x => x
      };
    | PerformAction(a) => perform_action(model, a)
    | ReparseCurrentEditor =>
      /* This serializes the current editor to text, resets the current
         editor, and then deserializes. It is intended as a (tactical)
         nuclear option for weird backpack states */
      let ed = Editors.get_editor(model.editors);
      let zipper_init = Zipper.init();
      let ed_str = Printer.to_string_editor(ed);
      switch (Printer.zipper_of_string(~zipper_init, ed_str)) {
      | None => Error(CantReset)
      | Some(z) =>
        //TODO: add correct action to history (Pick_up is wrong)
        let editor = Haz3lcore.Editor.new_state(Pick_up, z, ed);
        let editors = Editors.put_editor(editor, model.editors);
        Ok({...model, editors});
      };
    | Cut =>
      // system clipboard handling itself is done in Page.view handlers
      perform_action(model, Destruct(Left))
    | Copy =>
      // system clipboard handling itself is done in Page.view handlers
      // doesn't change the state but including as an action for logging purposes
      Ok(model)
    | Paste(clipboard) =>
      let ed = Editors.get_editor(model.editors);
      switch (Printer.paste_into_zip(ed.state.zipper, clipboard)) {
      | None => Error(CantPaste)
      | Some(z) =>
        //HACK(andrew): below is not strictly a insert action...
        let ed = Haz3lcore.Editor.new_state(Insert(clipboard), z, ed);
        let editors = Editors.put_editor(ed, model.editors);
        Ok({...model, editors});
      };
    | Undo =>
      let ed = Editors.get_editor(model.editors);
      switch (Haz3lcore.Editor.undo(ed)) {
      | None => Error(CantUndo)
      | Some(ed) =>
        Ok({...model, editors: Editors.put_editor(ed, model.editors)})
      };
    | Redo =>
      let ed = Editors.get_editor(model.editors);
      switch (Haz3lcore.Editor.redo(ed)) {
      | None => Error(CantRedo)
      | Some(ed) =>
        Ok({...model, editors: Editors.put_editor(ed, model.editors)})
      };
    | MoveToNextHole(d) =>
      perform_action(model, Move(Goal(Piece(Grout, d))))
    | Assistant(action) =>
      UpdateAssistant.apply(
        model,
        action,
        ~schedule_action,
        ~state,
        ~main=apply,
      )
    | Benchmark(Start) =>
      List.iter(schedule_action, Benchmark.actions_1);
      Benchmark.start();
      Ok(model);
    | Benchmark(Finish) =>
      Benchmark.finish();
      Ok(model);
    | StepperAction(key, StepForward(idx)) =>
      let r =
        model.results
        |> ModelResults.find(key)
        |> ModelResult.step_forward(idx);
      Ok({...model, results: model.results |> ModelResults.add(key, r)});
    | StepperAction(key, StepBackward) =>
      let r =
        model.results
        |> ModelResults.find(key)
        |> ModelResult.step_backward(~settings=model.settings.core.evaluation);
      Ok({...model, results: model.results |> ModelResults.add(key, r)});
    | ToggleStepper(key) =>
      Ok({
        ...model,
        results:
          model.results
          |> ModelResults.update(key, v =>
               Some(
                 v
                 |> Option.value(~default=NoElab: ModelResult.t)
                 |> ModelResult.toggle_stepper(
                      ~settings=model.settings.core.evaluation,
                    ),
               )
             ),
      })
    | UpdateResult(results) =>
      let results =
        ModelResults.union((_, _a, b) => Some(b), model.results, results);
      Ok({...model, results});
    | UpdatePrompt(new_prompt) =>
      Model.save_and_return({
        ...model,
        editors:
          Editors.update_exercise_prompt(
            model.editors,
            new_prompt,
            model.settings.instructor_mode,
          ),
      })
    };
  m |> Result.map(~f=update_cached_data(~schedule_action, update));
};
