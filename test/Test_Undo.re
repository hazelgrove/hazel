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
let move_left: Page.Update.t =
  Globals(ActiveEditor(Move(Local(Left, ByChar))));
let undo: Page.Update.t = Globals(Undo);
let redo: Page.Update.t = Globals(Redo);

let text_of = (model: History.Model.t): string =>
  Page.Update.get_editor(model.current).editor.state.zipper
  |> Haz3lcore.Zipper.zip
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

let undo_len = (model: History.Model.t) => List.length(model.undo_stack);
let redo_len = (model: History.Model.t) => List.length(model.redo_stack);

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
        let m2 = apply(m1, insert("2"));
        let t2 = text_of(m2);
        /* Undo one of the two edits so both stacks are non-empty, and so
         * there is program text for the caret to move over (the empty
         * program has no characters at all). */
        let m3 = apply(m2, undo);
        check(int, "one edit remains undoable", 1, undo_len(m3));
        check(int, "the undone edit is on the redo stack", 1, redo_len(m3));
        /* Caret movement is not historic (Action.is_historic) */
        let m4 = apply(m3, move_left);
        check(int, "move did not push an undo entry", 1, undo_len(m4));
        check(int, "move preserved the redo stack", 1, redo_len(m4));
        let m5 = apply(m4, redo);
        check(
          string,
          "redo still works after a non-historic action",
          t2,
          text_of(m5),
        );
      },
    ),
  ],
);
