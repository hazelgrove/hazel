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
  | Mode(mode) => {
      ...model,
      settings: {
        ...settings,
        mode,
      },
    }
  };

let reevaluate_post_update = (settings: Settings.t) =>
  fun
  | _ when !settings.core.dynamics => false
  | Set(s_action) =>
    switch (s_action) {
    | Captions
    | SecondaryIcons
    | Statics
    | Benchmark
    | Evaluation(
        ShowCaseClauses | ShowFnBodies | ShowCasts | ShowRecord | ShowFixpoints |
        ShowLookups |
        ShowFilters,
      ) =>
      false
    | Assist
    | Elaborate
    | Dynamics
    | InstructorMode
    | ContextInspector
    | Mode(_) => true
    | Evaluation(ShowSettings) => false
    }
  | SetMeta(meta_action) =>
    switch (meta_action) {
    | Mousedown
    | Mouseup
    | ShowBackpackTargets(_)
    | FontMetrics(_) => false
    }
  | PerformAction(
      Move(_) | MoveToNextHole(_) | Select(_) | Unselect(_) | RotateBackpack |
      MoveToBackpackTarget(_) |
      Jump(_),
    )
  | MoveToNextHole(_)
  | Save
  | Copy
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateExplainThisModel(_)
  | DoTheThing => false
  | ExportPersistentData
  | UpdateResult(_)
  | DebugConsole(_) => false
  | Benchmark(_)
  // may not be necessary on all of these
  // TODO review and prune
  | StepperAction(_, StepForward(_) | StepBackward)
  | ToggleStepper(_)
  | ReparseCurrentEditor
  | PerformAction(Destruct(_) | Insert(_) | Pick_up | Put_down)
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetCurrentEditor
  | SwitchEditor(_)
  | SwitchScratchSlide(_)
  | SwitchExampleSlide(_)
  | Cut
  | Paste(_)
  | Assistant(_)
  | Undo
  | Redo
  | Reset => true;

let should_scroll_to_caret =
  fun
  | Set(s_action) =>
    switch (s_action) {
    | Mode(_) => true
    | Captions
    | SecondaryIcons
    | Statics
    | Assist
    | Elaborate
    | Dynamics
    | Benchmark
    | ContextInspector
    | InstructorMode
    | Evaluation(_) => false
    }
  | SetMeta(meta_action) =>
    switch (meta_action) {
    | FontMetrics(_) => true
    | Mousedown
    | Mouseup
    | ShowBackpackTargets(_) => false
    }
  | Assistant(Prompt(_))
  | UpdateResult(_)
  | ToggleStepper(_)
  | StepperAction(_, StepBackward | StepForward(_)) => false
  | Assistant(AcceptSuggestion) => true
  | FinishImportScratchpad(_)
  | FinishImportAll(_)
  | ResetCurrentEditor
  | SwitchEditor(_)
  | SwitchScratchSlide(_)
  | SwitchExampleSlide(_)
  | ReparseCurrentEditor
  | Reset
  | Copy
  | Paste(_)
  | Cut
  | Undo
  | Redo
  | MoveToNextHole(_)
  | DoTheThing => true
  | PerformAction(a) =>
    switch (a) {
    | Move(_)
    | MoveToNextHole(_)
    | Jump(_)
    | Select(Resize(_) | Term(_) | Smart | Tile(_))
    | Destruct(_)
    | Insert(_)
    | Pick_up
    | Put_down
    | RotateBackpack
    | MoveToBackpackTarget(_) => true
    | Unselect(_)
    | Select(All) => false
    }
  | Save
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateExplainThisModel(_)
  | ExportPersistentData
  | DebugConsole(_)
  | Benchmark(_) => false;

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
  | DebugLoad
  | Examples(_) => None
  | Scratch(n, _) when n == idx => None
  | Scratch(_, slides) when idx >= List.length(slides) => None
  | Scratch(_, slides) => Some(Scratch(idx, slides))
  | Exercise(_, specs, _) when idx >= List.length(specs) => None
  | Exercise(_, specs, _) =>
    let spec = List.nth(specs, idx);
    let key = Exercise.key_of(spec);
    let exercise = Store.Exercise.load_exercise(key, spec, ~instructor_mode);
    Some(Exercise(idx, specs, exercise));
  };

let switch_exercise_editor =
    (editors: Editors.t, ~pos, ~instructor_mode): option(Editors.t) =>
  switch (editors) {
  | DebugLoad
  | Examples(_)
  | Scratch(_) => None
  | Exercise(m, specs, exercise) =>
    let exercise = Exercise.switch_editor(~pos, instructor_mode, ~exercise);
    //Note: now saving after each edit (delayed by 1 second) so no need to save here
    //Store.Exercise.save_exercise(exercise, ~instructor_mode);
    Some(Exercise(m, specs, exercise));
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
  let data: PersistentData.t = {
    examples: Store.Examples.load() |> Store.Examples.to_persistent,
    scratch: Store.Scratch.load() |> Store.Scratch.to_persistent,
    settings: Store.Settings.load(),
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

let rec apply =
        (model: Model.t, update: t, state: State.t, ~schedule_action)
        : Result.t(Model.t) => {
  let m: Result.t(Model.t) =
    switch (update) {
    | Reset => Ok(Model.reset(model))
    | Set(s_action) =>
      let model = update_settings(s_action, model);
      Model.save(model);
      // NOTE: Load here necessary to load editors on switching mode
      Ok(Model.load(model));
    | SetMeta(action) =>
      Ok({...model, meta: meta_update(model, action, ~schedule_action)})
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
    | SwitchExampleSlide(name) =>
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
    | DoTheThing =>
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
    | StepperAction(key, StepForward(obj)) =>
      let r =
        model.results
        |> ModelResults.find(key)
        |> ModelResult.step_forward(obj);
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
                 |> ModelResult.toggle_stepper,
               )
             ),
      })
    | UpdateResult(k, mr) =>
      switch (mr) {
      | Some(mr) =>
        Ok({...model, results: model.results |> ModelResults.add(k, mr)})
      | None =>
        Ok({
          ...model,
          results:
            ModelResults.lookup(model.results, k)
            |> Option.value(~default=ModelResult.NoElab)
            |> ModelResults.add(k, _, model.results),
        })
      }
    };
  let m =
    reevaluate_post_update(model.settings, update)
      ? m |> Result.map(~f=Model.update_elabs) : m;
  Result.map(
    ~f=
      m =>
        {
          ...m,
          results:
            ModelResults.run_pending(~settings=m.settings.core, m.results),
        },
    m,
  );
}
and meta_update =
    (model: Model.t, update: set_meta, ~schedule_action as _): Model.meta => {
  switch (update) {
  | Mousedown => {
      ui_state: {
        ...model.meta.ui_state,
        mousedown: true,
      },
    }
  | Mouseup => {
      ui_state: {
        ...model.meta.ui_state,
        mousedown: false,
      },
    }
  | ShowBackpackTargets(b) => {
      ui_state: {
        ...model.meta.ui_state,
        show_backpack_targets: b,
      },
    }
  | FontMetrics(font_metrics) => {
      ui_state: {
        ...model.meta.ui_state,
        font_metrics,
      },
    }
  };
};
