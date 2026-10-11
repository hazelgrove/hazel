open Alcotest;
open Web;

/* Integration tests for the page-level undo/redo mechanics in History.re.
 * These drive History.Update.update with real page actions, the same code
 * path the UI uses (Globals(ActiveEditor(_)) for edits, Globals(Undo/Redo)
 * for history navigation). */

let mk_model = (): History.Model.t => {
  let globals = Globals.Model.init();
  let (default_current, slides) = Lazy.force(Init.startup).scratch;
  let default_names = List.map(fst, slides);
  let scratch =
    ScratchMode.Persist.load_all(
      "scratch",
      ~settings=globals.settings.core,
      ~default_names,
      ~default_current,
    );
  let editors: Editors.Model.t = Scratch(scratch);
  let page: Page.Model.t = {
    globals,
    editors,
    explain_this: ExplainThisModel.init,
    selection: Editors.Selection.default_selection(editors),
  };
  {
    current: page,
    undo_stack: [],
    redo_stack: [],
  };
};

let apply = (model: History.Model.t, action: Page.Update.t): History.Model.t =>
  History.Update.update(
    ~import_log=_ => (),
    ~get_log_and=_ => (),
    ~schedule_action=_ => (),
    action,
    model,
  ).
    model;

let insert = (s: string): Page.Update.t =>
  Globals(ActiveEditor(Insert(s)));
let move_right: Page.Update.t =
  Globals(ActiveEditor(Move(Local(Right, ByChar))));
let undo: Page.Update.t = Globals(Undo);
let redo: Page.Update.t = Globals(Redo);

let text_of = (model: History.Model.t): string =>
  Page.Update.get_editor(model.current).editor.state.zipper
  |> Haz3lcore.Zipper.zip
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

let undo_len = (model: History.Model.t) => List.length(model.undo_stack);
let redo_len = (model: History.Model.t) => List.length(model.redo_stack);

let rejected = (label, model, action) =>
  switch (apply(model, action)) {
  | _ => fail(label ++ " should raise InvalidAction")
  | exception Updated.InvalidAction => ()
  };

/* Slides: a tutorial of two Probes lessons, applying what each action
   schedules (a slide's defaults on arrival) as the app does. */

let lesson = title =>
  List.find(
    (s: Tutorial.spec) => s.title == "Probes / " ++ title,
    TutorialSettings.lessons,
  );

let mk_tutorial = (): History.Model.t => {
  let globals = Globals.Model.init();
  let exercises =
    List.map(
      title =>
        TutorialMode.Model.of_spec(
          ~settings=globals.settings.core,
          ~instructor_mode=false,
          lesson(title),
        ),
      ["Environment Explorer", "Case Expressions"],
    );
  let editors: Editors.Model.t =
    Tutorial({
      current: 0,
      exercises,
    });
  {
    current: {
      globals,
      editors,
      explain_this: ExplainThisModel.init,
      selection: Editors.Selection.default_selection(editors),
    },
    undo_stack: [],
    redo_stack: [],
  };
};

/* Entering a Probes lesson sets the probe settings, which are global. */
let restoring_probe_settings = f => {
  let saved = Haz3lcore.ProbeProj.Settings.s^;
  Fun.protect(
    ~finally=
      () => {
        TutorialSlideInit.maybe_apply_on_change(
          ~autoprobe=Haz3lcore.AutoProbe.Off,
          ~set_autoprobe=_ => (),
          None,
        );
        Haz3lcore.ProbeProj.Settings.s := saved;
      },
    f,
  );
};

let step = (model: History.Model.t, action: Page.Update.t): History.Model.t => {
  let scheduled = ref([]);
  let model =
    History.Update.update(
      ~import_log=_ => (),
      ~get_log_and=_ => (),
      ~schedule_action=a => scheduled := scheduled^ @ [a],
      action,
      model,
    ).
      model;
  List.fold_left(apply, model, scheduled^);
};

let go_to = (slide: int): Page.Update.t =>
  Editors(Tutorial(SwitchExercise(slide)));
let slide_of = (model: History.Model.t) =>
  switch (model.current.editors) {
  | Tutorial(m) => m.current
  | _ => fail("not in the tutorial")
  };
let exercise = (model: History.Model.t, slide: int) =>
  switch (model.current.editors) {
  | Tutorial(m) => List.nth(m.exercises, slide)
  | _ => fail("not in the tutorial")
  };

let tests = (
  "Undo",
  [
    test_case(
      "edit then undo restores the original state",
      `Quick,
      () => {
        let m0 = mk_model();
        let t0 = text_of(m0);
        let m1 = apply(m0, insert("1"));
        check(bool, "insert changed the program", true, text_of(m1) != t0);
        check(int, "edit pushed one undo entry", 1, undo_len(m1));
        let m2 = apply(m1, undo);
        check(string, "undo restores original text", t0, text_of(m2));
        check(
          bool,
          "undo restores the exact pre-edit model",
          true,
          m2.current === m0.current,
        );
        check(int, "undo stack is empty again", 0, undo_len(m2));
        check(int, "undone edit moved to redo stack", 1, redo_len(m2));
      },
    ),
    test_case(
      "undo does not undo the undo",
      `Quick,
      () => {
        let m0 = mk_model();
        let t0 = text_of(m0);
        let m1 = apply(m0, insert("1"));
        let t1 = text_of(m1);
        let m2 = apply(m1, insert("2"));
        check(int, "two edits pushed two undo entries", 2, undo_len(m2));
        let m3 = apply(m2, undo);
        check(
          string,
          "first undo returns to the intermediate state",
          t1,
          text_of(m3),
        );
        /* If undo were itself historic, this second undo would bounce
         * forward to the post-"2" state instead of walking further back. */
        let m4 = apply(m3, undo);
        check(
          string,
          "second undo keeps walking back to the original",
          t0,
          text_of(m4),
        );
        check(int, "undo stack is empty", 0, undo_len(m4));
        check(int, "both edits are on the redo stack", 2, redo_len(m4));
      },
    ),
    test_case(
      "undo with no history is rejected and changes nothing",
      `Quick,
      () => {
        let m0 = mk_model();
        switch (apply(m0, undo)) {
        | _ => fail("undo on an empty stack should raise InvalidAction")
        | exception Updated.InvalidAction => ()
        };
      },
    ),
    test_case(
      "redo restores the undone edit",
      `Quick,
      () => {
        let m0 = mk_model();
        let m1 = apply(m0, insert("1"));
        let t1 = text_of(m1);
        let m2 = apply(m1, undo);
        let m3 = apply(m2, redo);
        check(string, "redo restores the edited text", t1, text_of(m3));
        check(
          bool,
          "redo restores the exact post-edit model",
          true,
          m3.current === m1.current,
        );
        check(int, "redo moved the entry back to undo", 1, undo_len(m3));
        check(int, "redo stack is empty again", 0, redo_len(m3));
        switch (apply(m3, redo)) {
        | _ => fail("redo with nothing to redo should raise InvalidAction")
        | exception Updated.InvalidAction => ()
        };
      },
    ),
    test_case(
      "a new edit clears the redo stack",
      `Quick,
      () => {
        let m0 = mk_model();
        let m1 = apply(m0, insert("1"));
        let m2 = apply(m1, undo);
        check(int, "undo left a redo entry", 1, redo_len(m2));
        let m3 = apply(m2, insert("2"));
        check(int, "new edit cleared the redo stack", 0, redo_len(m3));
        check(int, "new edit pushed an undo entry", 1, undo_len(m3));
      },
    ),
    test_case(
      "non-historic actions leave both stacks untouched",
      `Quick,
      () => {
        let m0 = mk_model();
        let m1 = apply(m0, insert("1"));
        let t1 = text_of(m1);
        let m2 = apply(m1, undo);
        /* Caret movement is not historic (Action.is_historic) */
        let m3 = apply(m2, move_right);
        check(int, "move did not push an undo entry", 0, undo_len(m3));
        check(int, "move preserved the redo stack", 1, redo_len(m3));
        let m4 = apply(m3, redo);
        check(
          string,
          "redo still works after a non-historic action",
          t1,
          text_of(m4),
        );
      },
    ),
    test_case("undo and redo never leave the slide", `Quick, () =>
      restoring_probe_settings(() => {
        let m1 = step(mk_tutorial(), insert("1"));
        check(int, "arriving pushed no entry", 1, undo_len(m1));
        let m2 = step(m1, go_to(1));
        rejected("undo of the previous slide's edit", m2, undo);
        let t2 = text_of(m2);
        let m3 = step(m2, insert("2"));
        let t3 = text_of(m3);
        let m4 = apply(m3, undo);
        check(string, "undo reverts this slide's edit", t2, text_of(m4));
        rejected("undo past this slide's first entry", m4, undo);
        let m5 = apply(m4, redo);
        check(string, "redo restores it", t3, text_of(m5));
        check(int, "still on the slide", 1, slide_of(m5));
        rejected("redo on another slide", step(m4, go_to(0)), redo);
      })
    ),
    test_case("redo survives a visit to another slide", `Quick, () =>
      restoring_probe_settings(() => {
        let m1 = step(mk_tutorial(), insert("1"));
        let t1 = text_of(m1);
        let m2 = apply(m1, undo);
        let m3 = step(step(m2, go_to(1)), go_to(0));
        check(int, "arrivals kept the redo stack", 1, redo_len(m3));
        let m4 = apply(m3, redo);
        check(string, "redo restores the edit", t1, text_of(m4));
      })
    ),
    test_case("undo on one slide leaves the others as they are", `Quick, () =>
      restoring_probe_settings(() => {
        let m1 = step(mk_tutorial(), insert("1"));
        let m2 = step(step(m1, go_to(1)), move_right);
        let m3 = step(m2, go_to(0));
        let m4 = apply(m3, undo);
        check(int, "on the edited slide", 0, slide_of(m4));
        check(
          bool,
          "the other slide is the one left behind, not the one in the entry",
          true,
          exercise(m4, 1) === exercise(m3, 1),
        );
      })
    ),
  ],
);
