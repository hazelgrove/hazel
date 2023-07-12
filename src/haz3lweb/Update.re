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
  Model.save(model);
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
  | SetMeta(meta_action) =>
    switch (meta_action) {
    | DoubleTap(_)
    | Mousedown
    | Mouseup
    | ShowBackpackTargets(_)
    | FontMetrics(_)
    | Result(_) => false
    | MVU(_)
    | Auto(_) => true
    }
  | PerformAction(
      Move(_) | MoveToNextHole(_) | Select(_) | Unselect(_) | RotateBackpack |
      MoveToBackpackTarget(_) |
      Jump(_) |
      SetSelectionFocus(_),
    )
  | MoveToNextHole(_)
  | Save
  | Copy
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateLangDocMessages(_)
  | DebugAction(_) => false
  // may not be necessary on all of these
  // TODO review and prune
  | ReparseCurrentEditor
  | PerformAction(
      Destruct(_) | Insert(_) | Pick_up | Put_down | RemoteAction(_),
    )
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetCurrentEditor
  | SwitchEditor(_)
  | SwitchScratchSlide(_)
  | SwitchExampleSlide(_)
  | Cut
  | Paste(_)
  | Agent(_)
  | Execute(_)
  | Undo
  | Redo => true;

let evaluate_and_schedule =
    (_state: State.t, ~schedule_action as _, model: Model.t): Model.t => {
  let model = {
    ...model,
    meta: {
      ...model.meta,
      results:
        Util.TimeUtil.measure_time(
          "ModelResults.init", model.settings.benchmark, () =>
          ModelResults.init(
            model.settings.dynamics
              ? Editors.get_spliced_elabs(model.editors) : [],
          )
        ),
    },
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
    | SetMeta(action) =>
      Ok({...model, meta: meta_update(model, action, ~schedule_action)})
    | UpdateLangDocMessages(u) =>
      let model = {
        ...model,
        langDocMessages: LangDocMessages.set_update(model.langDocMessages, u),
      };
      Model.save(model);
      Ok(model);
    | DebugAction(a) =>
      DebugAction.perform(a);
      Ok(model);
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
    | Save =>
      Model.save(model);
      Ok(model);
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
          Model.save(model);
          Ok(model);
        }
      }
    | ResetCurrentEditor =>
      let model = {
        ...model,
        editors:
          reset_editor(
            model.editors: Editors.t,
            ~instructor_mode=model.settings.instructor_mode,
          ),
      };
      Model.save(model);
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
          Model.save(model);
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
          Model.save(model);
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
          Model.save(model);
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
    | PerformAction(Insert("?") as a) =>
      let editor = model.editors |> Editors.get_editor;
      let ctx_init = Editors.get_ctx_init(model.editors);
      AgentUpdate.schedule_prompt(
        ~ctx_init,
        editor.state.zipper,
        ~schedule_action,
      );
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
    /*| Paste(clipboard) =>
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
      };*/
    | ReparseCurrentEditor =>
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
    | Cut =>
      // system clipboard handling itself is done in Page.view handlers
      perform_action(model, Destruct(Left))
    | Copy =>
      // system clipboard handling itself is done in Page.view handlers
      // doesn't change the state but including as an action for logging purposes
      Ok(model)
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
    | Agent(action) =>
      AgentUpdate.apply(model, action, ~schedule_action, ~state, ~main=apply)
    };
  reevaluate_post_update(update)
    ? m |> Result.map(~f=evaluate_and_schedule(state, ~schedule_action)) : m;
}
and meta_update =
    (model: Model.t, update: set_meta, ~schedule_action): Model.meta => {
  switch (update) {
  | DoubleTap(double_tap) => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        double_tap,
      },
    }
  | Mousedown => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        mousedown: true,
      },
    }
  | Mouseup => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        mousedown: false,
      },
    }
  | ShowBackpackTargets(b) => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        show_backpack_targets: b,
      },
    }
  | FontMetrics(font_metrics) => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        font_metrics,
      },
    }
  | MVU(name, dh) => {
      ...model.meta,
      mvu_states: VarMap.extend(model.meta.mvu_states, (name, dh)),
    }
  | Result(key, res) =>
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
      model.meta.results
      |> ModelResults.find(key)
      |> ModelResult.update_current(res);
    let results = model.meta.results |> ModelResults.add(key, r);
    {...model.meta, results};
  | Auto(action) => UpdateAuto.go(model, action, ~schedule_action)
  };
};
