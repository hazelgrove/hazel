open Haz3lcore;

include UpdateAction; // to prevent circularity

let reset_editor = (editors: Editors.t, ~instructor_mode): Editors.t =>
  switch (editors) {
  | DebugLoad => failwith("impossible")
  | Scratch(n, slides) =>
    let slides =
      Util.ListUtil.put_nth(n, ScratchSlidesInit.init_nth(n), slides);
    Scratch(n, slides);
  | Examples(name, slides) =>
    let slides =
      slides
      |> List.remove_assoc(name)
      |> List.cons((name, Examples.init_name(name)));
    Examples(name, slides);
  | Exercise(n, specs, _) =>
    Exercise(
      n,
      specs,
      List.nth(specs, n) |> Exercise.state_of_spec(~instructor_mode),
    )
  };

let update_settings = (a: settings_action, model: Model.t): Model.t => {
  let settings = model.settings;
  let model =
    switch (a) {
    | Statics =>
      /* NOTE: dynamics depends on statics, so if dynamics is on and
         we're turning statics off, turn dynamics off as well */
      {
        ...model,
        settings: {
          ...settings,
          statics: !settings.statics,
          dynamics: !settings.statics && settings.dynamics,
        },
      }
    | Dynamics => {
        ...model,
        settings: {
          ...settings,
          dynamics: !settings.dynamics,
        },
      }
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
  Store.save_model(model);
  model;
};

let reevaluate_post_update =
  fun
  | Set(s_action) =>
    switch (s_action) {
    | Captions
    | SecondaryIcons
    | Statics
    | Benchmark => false
    | Dynamics
    | InstructorMode
    | ContextInspector
    | Mode(_) => true
    }
  | PerformAction(
      Move(_) | Select(_) | Unselect | RotateBackpack | MoveToBackpackTarget(_) |
      Jump(_),
    )
  | MoveToNextHole(_) //
  | UpdateDoubleTap(_)
  | Mousedown
  | Mouseup
  | Save
  | SetShowBackpackTargets(_)
  | SetFontMetrics(_)
  | SetLogoFontMetrics(_)
  | Copy
  | UpdateResult(_)
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | FailedInput(_)
  | UpdateLangDocMessages(_)
  | DebugAction(_) => false
  // may not be necessary on all of these
  // TODO review and prune
  | ResetCurrentEditor
  | PerformAction(Destruct(_) | Insert(_) | Pick_up | Put_down)
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetSlide
  | SwitchEditor(_)
  | SwitchScratchSlide(_)
  | SwitchExampleSlide(_)
  | SetMode(_)
  | Cut
  | Paste(_)
  | Undo
  | Redo => true;

let evaluate_and_schedule =
    (_state: State.t, ~schedule_action as _, model: Model.t): Model.t => {
  let model = {
    ...model,
    results:
      Util.TimeUtil.measure_time(
        "ModelResults.init", model.settings.benchmark, () =>
        ModelResults.init(
          model.settings.dynamics
            ? Editors.get_spliced_elabs(model.editors) : [],
        )
      ),
  };

  // if (model.settings.dynamics) {
  //   Editors.get_spliced_elabs(model.editors)
  //   |> List.iter(((key, d)) => {
  //        /* Send evaluation request. */
  //        let pushed = State.evaluator_next(state, key, d);

  //        /* Set evaluation to pending after short timeout. */
  //        /* FIXME: This is problematic if evaluation finished in time, but UI hasn't
  //         * updated before below action is scheduled. */
  //        Delay.delay(
  //          () =>
  //            if (pushed |> Lwt.is_sleeping) {
  //              schedule_action(UpdateResult(key, ResultPending));
  //            },
  //          300,
  //        );
  //      });
  // };
  model;
};

let perform_action =
    (model: Model.t, a: Action.t, _state: State.t, ~schedule_action as _)
    : Result.t(Model.t) => {
  let (id, ed_init) = Editors.get_editor_and_id(model.editors);
  switch (Haz3lcore.Perform.go(a, ed_init, id)) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok((ed, id)) =>
    Ok({...model, editors: Editors.put_editor_and_id(id, ed, model.editors)})
  };
};

let apply =
    (model: Model.t, update: t, state: State.t, ~schedule_action)
    : Result.t(Model.t) => {
  let m: Result.t(Model.t) =
    switch (update) {
    | Set(s_action) => Ok(update_settings(s_action, model))
    | UpdateDoubleTap(double_tap) => Ok({...model, double_tap})
    | Mousedown => Ok({...model, mousedown: true})
    | Mouseup => Ok({...model, mousedown: false})
    | Save =>
      Store.save_model(model);
      Ok(model);
    | InitImportAll(file) =>
      JsUtil.read_file(file, data => schedule_action(FinishImportAll(data)));
      Ok(model);
    | FinishImportAll(data) =>
      switch (data) {
      | None => Ok(model)
      | Some(data) =>
        Export.import_all(data, ~specs=ExerciseSettings.exercises);
        Ok(Store.load_model(model));
      }
    | InitImportScratchpad(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(FinishImportScratchpad(data))
      );
      Ok(model);
    | FinishImportScratchpad(data) =>
      switch (model.editors) {
      | DebugLoad
      | Examples(_)
      | Exercise(_) => failwith("impossible")
      | Scratch(idx, slides) =>
        switch (data) {
        | None => Ok(model)
        | Some(data) =>
          let state = ScratchSlide.import(data);
          let slides = Util.ListUtil.put_nth(idx, state, slides);
          let model = {...model, editors: Scratch(idx, slides)};
          Store.save_model(model);
          Ok(model);
        }
      }
    | ResetSlide =>
      let model = {
        ...model,
        editors:
          reset_editor(
            model.editors: Editors.t,
            ~instructor_mode=model.settings.instructor_mode,
          ),
      };
      Store.save_model(model);
      Ok(model);
    | SwitchScratchSlide(n) =>
      switch (model.editors) {
      | DebugLoad
      | Examples(_) => failwith("impossible")
      | Scratch(m, _) when m == n => Error(FailedToSwitch)
      | Scratch(_, slides) =>
        switch (n < List.length(slides)) {
        | false => Error(FailedToSwitch)
        | true =>
          let model = {...model, editors: Scratch(n, slides)};
          Store.save_model(model);
          Ok(model);
        }
      | Exercise(_, specs, _) =>
        switch (n < List.length(specs)) {
        | false => Error(FailedToSwitch)
        | true =>
          let instructor_mode = model.settings.instructor_mode;
          let spec = List.nth(specs, n);
          let key = Exercise.key_of(spec);
          let exercise =
            Store.Exercise.load_exercise(key, spec, ~instructor_mode);
          let model = {...model, editors: Exercise(n, specs, exercise)};
          Store.save_model(model);
          Ok(model);
        }
      }
    | SwitchExampleSlide(name) =>
      switch (model.editors) {
      | DebugLoad
      | Scratch(_)
      | Exercise(_) => Error(FailedToSwitch)
      | Examples(cur, slides) =>
        if (!List.mem_assoc(name, slides) || cur == name) {
          Error(FailedToSwitch);
        } else {
          let model = {...model, editors: Examples(name, slides)};
          Store.save_model(model);
          Ok(model);
        }
      }
    | SwitchEditor(pos) =>
      switch (model.editors) {
      | DebugLoad
      | Examples(_)
      | Scratch(_) => Error(FailedToSwitch)
      | Exercise(m, specs, exercise) =>
        let exercise =
          Exercise.switch_editor(
            ~pos,
            model.settings.instructor_mode,
            ~exercise,
          );
        Store.Exercise.save_exercise(
          exercise,
          ~instructor_mode=model.settings.instructor_mode,
        );
        Ok({...model, editors: Exercise(m, specs, exercise)});
      }
    | SetMode(mode) =>
      let model = update_settings(Mode(mode), model);
      Ok(Store.load_model(model));
    | SetShowBackpackTargets(b) => Ok({...model, show_backpack_targets: b})
    | SetFontMetrics(font_metrics) => Ok({...model, font_metrics})
    | SetLogoFontMetrics(logo_font_metrics) =>
      Ok({...model, logo_font_metrics})
    | PerformAction(a) => perform_action(model, a, state, ~schedule_action)
    | FailedInput(reason) => Error(UnrecognizedInput(reason))
    | Cut =>
      // system clipboard handling itself is done in Page.view handlers
      perform_action(model, Destruct(Left), state, ~schedule_action)
    | Copy =>
      // system clipboard handling itself is done in Page.view handlers
      // doesn't change the state but including as an action for logging purposes
      Ok(model)
    | Paste(clipboard) =>
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      switch (
        Printer.zipper_of_string(~zipper_init=ed.state.zipper, id, clipboard)
      ) {
      | None => Error(CantPaste)
      | Some((z, id)) =>
        /* NOTE(andrew): These two perform calls are a hack to
           deal with the fact that pasting something like "let a = b in"
           won't trigger the barfing of the "in"; to trigger this, we
           insert a space, and then we immediately delete it. */
        switch (Haz3lcore.Perform.go_z(Insert(" "), z, id)) {
        | Error(_) => Error(CantPaste)
        | Ok((z, id)) =>
          switch (Haz3lcore.Perform.go_z(Destruct(Left), z, id)) {
          | Error(_) => Error(CantPaste)
          | Ok((z, id)) =>
            let ed = Haz3lcore.Editor.new_state(Pick_up, z, ed);
            //TODO: add correct action to history (Pick_up is wrong)
            let editors = Editors.put_editor_and_id(id, ed, model.editors);
            Ok({...model, editors});
          }
        }
      };
    | ResetCurrentEditor =>
      /* This serializes the current editor to text, resets the current
         editor, and then deserializes. It is intended as a (tactical)
         nuclear option for weird backpack states */
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      let zipper_init = Zipper.init(id);
      let ed_str = Printer.to_string_editor(ed);
      switch (Printer.zipper_of_string(~zipper_init, id + 1, ed_str)) {
      | None => Error(CantReset)
      | Some((z, id)) =>
        //TODO: add correct action to history (Pick_up is wrong)
        let editor = Haz3lcore.Editor.new_state(Pick_up, z, ed);
        let editors = Editors.put_editor_and_id(id, editor, model.editors);
        Ok({...model, editors});
      };
    | Undo =>
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      switch (Haz3lcore.Editor.undo(ed)) {
      | None => Error(CantUndo)
      | Some(ed) =>
        Ok({
          ...model,
          editors: Editors.put_editor_and_id(id, ed, model.editors),
        })
      };
    | Redo =>
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      switch (Haz3lcore.Editor.redo(ed)) {
      | None => Error(CantRedo)
      | Some(ed) =>
        Ok({
          ...model,
          editors: Editors.put_editor_and_id(id, ed, model.editors),
        })
      };
    | MoveToNextHole(_d) =>
      // TODO restore
      Ok(model)
    | UpdateLangDocMessages(u) =>
      let model = {
        ...model,
        langDocMessages: LangDocMessages.set_update(model.langDocMessages, u),
      };
      Store.save_model(model);
      Ok(model);
    | UpdateResult(key, res) =>
      /* If error, print a message. */
      switch (res) {
      | ResultFail(Program_EvalError(reason)) =>
        let serialized =
          reason |> EvaluatorError.sexp_of_t |> Sexplib.Sexp.to_string_hum;
        print_endline(
          "[Program.EvalError(EvaluatorError.Exception(" ++ serialized ++ "))]",
        );
      | ResultFail(Program_DoesNotElaborate) =>
        print_endline("[Program.DoesNotElaborate]")
      | _ => ()
      };
      let r =
        model.results
        |> ModelResults.find(key)
        |> ModelResult.update_current(res);
      let results = model.results |> ModelResults.add(key, r);
      Ok({...model, results});
    | DebugAction(a) =>
      DebugAction.perform(a);
      Ok(model);
    };
  reevaluate_post_update(update)
    ? m |> Result.map(~f=evaluate_and_schedule(state, ~schedule_action)) : m;
};
