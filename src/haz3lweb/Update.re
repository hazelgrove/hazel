open Haz3lcore;

include UpdateAction; // to prevent circularity

let save_editors = (model: Model.t): unit =>
  switch (model.editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(n, slides) => LocalStorage.Scratch.save((n, slides))
  | School(n, specs, exercise) =>
    LocalStorage.School.save(
      (n, specs, exercise),
      ~instructor_mode=model.settings.instructor_mode,
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
  LocalStorage.Settings.save(model.settings);
  save_editors(model);
  model;
};

let load_model = (model: Model.t): Model.t => {
  let settings = LocalStorage.Settings.load();
  let langDocMessages = LocalStorage.LangDocMessages.load();
  let model = {...model, settings, langDocMessages};
  let model =
    switch (model.settings.mode) {
    | DebugLoad => model
    | Scratch =>
      let (idx, slides) = LocalStorage.Scratch.load();
      {...model, editors: Scratch(idx, slides)};
    | School =>
      let instructor_mode = model.settings.instructor_mode;
      let specs = School.exercises;
      let (n, specs, exercise) =
        LocalStorage.School.load(~specs, ~instructor_mode);
      {...model, editors: School(n, specs, exercise)};
    };
  {
    ...model,
    results:
      ModelResults.init(
        model.settings.dynamics
          ? Editors.get_spliced_elabs(model.editors) : [],
      ),
  };
};

let load_default_editor = (model: Model.t): Model.t =>
  switch (model.editors) {
  | DebugLoad => model
  | Scratch(_) =>
    let (idx, editors) = LocalStorage.Scratch.init();
    {...model, editors: Scratch(idx, editors)};
  | School(_) =>
    let instructor_mode = model.settings.instructor_mode;
    let (n, specs, exercise) = LocalStorage.School.init(~instructor_mode);
    {...model, editors: School(n, specs, exercise)};
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
      Move(_) | Select(_) | Unselect(_) | RotateBackpack |
      MoveToBackpackTarget(_) |
      Jump(_) |
      SetSelectionFocus(_),
    )
  | MoveToNextHole(_)
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
  | StoreKey(_)
  | DebugAction(_) => false
  // may not be necessary on all of these
  // TODO review and prune
  | ResetCurrentEditor
  | PerformAction(
      Destruct(_) | Insert(_) | Pick_up | Put_down | RemoteAction(_),
    )
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetSlide
  | SwitchEditor(_)
  | SwitchSlide(_)
  | ToggleMode
  | Cut
  | Paste(_)
  | Agent(_)
  | Execute(_)
  | MVUSet(_)
  | Script(_)
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

let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) => {
  let (id, ed_init) = Editors.get_editor_and_id(model.editors);
  switch (Haz3lcore.Perform.go(a, ed_init, id)) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok((ed, id)) =>
    Ok({...model, editors: Editors.put_editor_and_id(id, ed, model.editors)})
  };
};

let rec apply =
        (model: Model.t, update: t, state: State.t, ~schedule_action)
        : Result.t(Model.t) => {
  //print_endline(update |> yojson_of_t |> Yojson.Safe.to_string);
  //print_endline(update |> sexp_of_t |> Sexplib.Sexp.to_string);
  let m: Result.t(Model.t) =
    switch (update) {
    | Set(s_action) => Ok(update_settings(s_action, model))
    | UpdateDoubleTap(double_tap) => Ok({...model, double_tap})
    | Mousedown => Ok({...model, mousedown: true})
    | Mouseup => Ok({...model, mousedown: false})
    | Save =>
      save_editors(model);
      Ok(model);
    | InitImportAll(file) =>
      JsUtil.read_file(file, data => schedule_action(FinishImportAll(data)));
      Ok(model);
    | FinishImportAll(data) =>
      switch (data) {
      | None => Ok(model)
      | Some(data) =>
        let specs = School.exercises;
        Export.import_all(data, ~specs);
        Ok(load_model(model));
      }
    | InitImportScratchpad(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(FinishImportScratchpad(data))
      );
      Ok(model);
    | FinishImportScratchpad(data) =>
      switch (model.editors) {
      | DebugLoad => failwith("impossible")
      | School(_) => failwith("impossible")
      | Scratch(idx, slides) =>
        switch (data) {
        | None => Ok(model)
        | Some(data) =>
          let state = ScratchSlide.import(data);
          let slides = Util.ListUtil.put_nth(idx, state, slides);
          LocalStorage.Scratch.save((idx, slides));

          Ok({...model, editors: Scratch(idx, slides)});
        }
      }
    | ResetSlide =>
      let model =
        switch (model.editors) {
        | DebugLoad => failwith("impossible")
        | Scratch(n, slides) =>
          let slides =
            Util.ListUtil.put_nth(n, ScratchSlidesInit.init_nth(n), slides);
          {...model, editors: Scratch(n, slides)};
        | School(n, specs, _) =>
          let instructor_mode = model.settings.instructor_mode;
          {
            ...model,
            editors:
              School(
                n,
                specs,
                List.nth(specs, n)
                |> SchoolExercise.state_of_spec(~instructor_mode),
              ),
          };
        };
      save_editors(model);
      Ok(model);
    | SwitchSlide(n) =>
      switch (model.editors) {
      | DebugLoad => failwith("impossible")
      | Scratch(m, _) when m == n => Error(FailedToSwitch)
      | Scratch(_, slides) =>
        switch (n < List.length(slides)) {
        | false => Error(FailedToSwitch)
        | true =>
          LocalStorage.Scratch.save((n, slides));
          Ok({...model, editors: Scratch(n, slides)});
        }
      | School(_, specs, _) =>
        switch (n < List.length(specs)) {
        | false => Error(FailedToSwitch)
        | true =>
          let instructor_mode = model.settings.instructor_mode;
          let spec = List.nth(specs, n);
          let key = SchoolExercise.key_of(spec);
          let exercise =
            LocalStorage.School.load_exercise(key, spec, ~instructor_mode);
          Ok({...model, editors: School(n, specs, exercise)});
        }
      }
    | SwitchEditor(n) =>
      switch (model.editors) {
      | DebugLoad => failwith("impossible")
      | Scratch(_) => Error(FailedToSwitch) // one editor per scratch
      | School(m, specs, exercise) =>
        let exercise = SchoolExercise.switch_editor(n, exercise);
        LocalStorage.School.save_exercise(
          exercise,
          ~instructor_mode=model.settings.instructor_mode,
        );
        Ok({...model, editors: School(m, specs, exercise)});
      }
    | ToggleMode =>
      let new_mode = Editors.rotate_mode(model.editors);
      let model = update_settings(Mode(new_mode), model);
      Ok(load_model(model));
    | SetShowBackpackTargets(b) => Ok({...model, show_backpack_targets: b})
    | SetFontMetrics(font_metrics) => Ok({...model, font_metrics})
    | SetLogoFontMetrics(logo_font_metrics) =>
      Ok({...model, logo_font_metrics})
    | PerformAction(Insert("?") as a) =>
      let editor = model.editors |> Editors.get_editor;
      AgentUpdate.schedule_prompt(editor.state.zipper, ~schedule_action);
      perform_action(model, a);
    | PerformAction(a) =>
      //NOTE: effectful
      switch (a) {
      | Destruct(_)
      | Insert(_) => schedule_action(Agent(Prompt(TyDi)))
      | _ => ()
      };
      let model = AgentUpdate.reset_buffer(model);
      perform_action(model, a);
    | FailedInput(reason) => Error(UnrecognizedInput(reason))
    | Cut =>
      // system clipboard handling itself is done in Page.view handlers
      perform_action(model, Destruct(Left))
    | Copy =>
      // system clipboard handling itself is done in Page.view handlers
      // doesn't change the state but including as an action for logging purposes
      Ok(model)
    | MVUSet(name, dh) =>
      Ok({
        ...model,
        mvu_states: VarMap.extend(model.mvu_states, (name, dh)),
      })
    | Execute(fake_str) =>
      print_endline("fake: " ++ fake_str);
      let editor = model.editors |> Editors.get_editor;
      let str = Printer.to_string_selection(editor);
      print_endline("EXECUTE: " ++ str);
      let update: UpdateAction.t =
        try(str |> Sexplib.Sexp.of_string |> t_of_sexp) {
        | _ =>
          print_endline("execute parse failes, saving instead lol");
          Save; //TODO
        };
      apply(model, update, state, ~schedule_action);
    | StoreKey(key, str) =>
      LocalStorage.Generic.save(key, str);
      Ok(model);
    | Agent(action) =>
      AgentUpdate.apply(model, action, ~schedule_action, ~state, ~main=apply)
    | Paste(clipboard) =>
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      switch (Printer.paste_into_zip(ed.state.zipper, id, clipboard)) {
      | None => Error(CantPaste)
      | Some((z, id)) =>
        //HACK(andrew): below is not strictly a insert action...
        let ed = Haz3lcore.Editor.new_state(Insert(clipboard), z, ed);
        let editors = Editors.put_editor_and_id(id, ed, model.editors);
        Ok({...model, editors});
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
    | MoveToNextHole(d) =>
      let p: Piece.t => bool = (
        fun
        | Grout(_) => true
        | _ => false
      );
      perform_action(model, Move(Goal(Piece(p, d))));
    | UpdateLangDocMessages(u) =>
      let langDocMessages =
        LangDocMessages.set_update(model.langDocMessages, u);
      LocalStorage.LangDocMessages.save(langDocMessages);
      Ok({...model, langDocMessages});
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
    | Script(StartTest ()) =>
      let sketch_str = "let lol: Int = FILL_ME in lol + 2000";
      let script = Scripter.mk_script(sketch_str);
      List.iter(schedule_action, script);
      Ok(model);
    | Script(EndTest ()) =>
      schedule_action(Agent(AcceptSuggestion));
      schedule_action(Script(LogTest()));
      Ok(model);
    | Script(LogTest ()) =>
      let script = model.script;
      print_endline("LOG TEST. Statics results:");
      let info_map =
        ChatLSP.get_info_from_zipper(
          ~ctx=Ctx.empty, //TODO(andrew): better ctx
          Editors.get_editor(model.editors).state.zipper,
        );
      print_endline(
        ChatLSP.Errors.collect_static(info_map) |> String.concat("\n"),
      );
      Ok({...model, script});
    };
  reevaluate_post_update(update)
    ? m |> Result.map(~f=evaluate_and_schedule(state, ~schedule_action)) : m;
};
