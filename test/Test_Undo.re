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
    tentative_redo: None,
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

/* a stacked slide driven as the page drives it: each action, then a
   calculate, then the actions it scheduled */
module Cells = {
  let src = "let a = 1 in\nlet b = 2 in\na + b";

  let mk = (~src=src, ()): History.Model.t => {
    let seg = CorpusUtil.seg_of(src);
    let editors: Editors.Model.t =
      Scratch(
        ScratchModel.Model.{
          current: 0,
          scratchpads: [
            ScratchModel.Scratchpad.mk_code(
              ~name="Focus",
              ~editor=ScratchFocus.cell_of_seg(seg),
              (),
            ),
          ],
        },
      );
    {
      current: {
        globals: Globals.Model.init(),
        editors,
        explain_this: ExplainThisModel.init,
        selection: Editors.Selection.default_selection(editors),
      },
      undo_stack: [],
      redo_stack: [],
      tentative_redo: None,
    };
  };

  let rec run = (m: History.Model.t, action: Page.Update.t): History.Model.t => {
    let scheduled = ref([]);
    let schedule_action = a => scheduled := scheduled^ @ [a];
    let m =
      History.Update.update(
        ~import_log=_ => (),
        ~get_log_and=_ => (),
        ~schedule_action,
        action,
        m,
      ).
        model;
    let m =
      History.Update.calculate(
        ~schedule_action,
        ~is_edited=true,
        ~dynamics=false,
        m,
      );
    List.fold_left(run, m, scheduled^);
  };

  let program = (m: History.Model.t): Program.t =>
    switch (m.current.editors) {
    | Scratch(s) => Option.get(ScratchModel.Model.current_program(s))
    | _ => failwith("not a deck")
    };

  let divided = (m: History.Model.t): Divided.t =>
    switch (program(m)) {
    | Divided(d) => d
    | Whole(_) => failwith("not divided")
    };

  let row = (m: History.Model.t, label: string): Haz3lcore.Id.t =>
    CorpusUtil.row_by_label(Program.outline(program(m)), label);

  let workspace = (a: Workspace.Action.t): Page.Update.t =>
    Editors(Scratch(Workspace(a)));

  let body = (m: History.Model.t, id: Haz3lcore.Id.t): string =>
    Option.get(Divided.cell(id, divided(m))).e_body.editor.editor.state.
      zipper
    |> Haz3lcore.Zipper.zip
    |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

  let focus = (m: History.Model.t) =>
    Option.map(
      ((e: ScratchCell.t, side)) => (e.e_id, side),
      Divided.focused(divided(m)),
    );
};

let focused = (m, id, side: ScratchCell.side) =>
  Cells.focus(m) == Some((id, side));

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
        /* compacted snapshots: text, not physical identity, is the contract */
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
      "outline opens focus their cell, closes leave the focus",
      `Quick,
      () => {
        let m = Cells.run(Cells.mk(), Refresh);
        let (a, b) = (Cells.row(m, "a"), Cells.row(m, "b"));
        let m = Cells.run(m, Cells.workspace(FocusToggle(b)));
        check(bool, "b opens with the focus", true, focused(m, b, Body));
        let m = Cells.run(m, Cells.workspace(FocusToggle(a)));
        check(bool, "then a", true, focused(m, a, Body));
        let m =
          Cells.run(
            m,
            Cells.workspace(
              Stack(
                b,
                Header,
                MainEditor(Perform(Move(Local(Right, ByChar)))),
              ),
            ),
          );
        check(
          bool,
          "an action takes it to b's header",
          true,
          focused(m, b, Header),
        );
        let m = Cells.run(m, Cells.workspace(FocusEnsure(a)));
        check(
          bool,
          "an ensure gives it a's body",
          true,
          focused(m, a, Body),
        );
        let m = Cells.run(m, Cells.workspace(FocusToggle(b)));
        check(bool, "closing b leaves it", true, focused(m, a, Body));
        check(
          bool,
          "b closed",
          true,
          Divided.cell(b, Cells.divided(m)) == None,
        );
      },
    ),
    test_case(
      "an outline close leaves the keys in an open cell",
      `Quick,
      () => {
        let m = Cells.run(Cells.mk(), Refresh);
        let (a, b) = (Cells.row(m, "a"), Cells.row(m, "b"));
        let m = Cells.run(m, Cells.workspace(FocusToggle(a)));
        let m = Cells.run(m, Cells.workspace(FocusToggle(b)));
        check(bool, "b has the focus", true, focused(m, b, Body));
        let stack_selected = (m: History.Model.t) =>
          switch (m.current.selection) {
          | Scratch(Stack(_)) => true
          | _ => false
          };
        /* the view gives the focused pane the id FocusEffect's Cell
           focuses, while the stack is selected */
        let keys_to = (m, id) => {
          let d = Cells.divided(m);
          Divided.cell(id, d) != None
          && Divided.active(d) == Some((id, Body))
          && focused(m, id, Body)
          && stack_selected(m)
          && Haz3lcore.FocusEffect.scheduled^ == Some(Cell);
        };
        Haz3lcore.FocusEffect.scheduled := None;
        let m = Cells.run(m, Cells.workspace(FocusToggle(b)));
        check(
          bool,
          "b closed",
          true,
          Divided.cell(b, Cells.divided(m)) == None,
        );
        check(bool, "its keys go to a", true, keys_to(m, a));
        /* an unfocused cell closing above the focused one moves its
           element, so the DOM focus is placed again */
        let m = Cells.run(m, Cells.workspace(FocusToggle(b)));
        check(bool, "b has the focus again", true, focused(m, b, Body));
        Haz3lcore.FocusEffect.scheduled := None;
        let m = Cells.run(m, Cells.workspace(FocusToggle(a)));
        check(bool, "b keeps the keys", true, keys_to(m, b));
      },
    ),
    test_case(
      "undo restores the focused cell",
      `Quick,
      () => {
        let m = Cells.run(Cells.mk(), Refresh);
        let (a, b) = (Cells.row(m, "a"), Cells.row(m, "b"));
        let m = Cells.run(m, Cells.workspace(FocusToggle(b)));
        let m = Cells.run(m, insert("0"));
        check(string, "typing reaches b", "02", Cells.body(m, b));
        let m = Cells.run(m, Cells.workspace(FocusToggle(a)));
        check(bool, "a has the focus", true, focused(m, a, Body));
        /* the snapshot has only b open: realizing the view opens a again,
           and the focus stays where the undone edit was */
        let m = Cells.run(m, undo);
        check(string, "the edit is undone", "2", Cells.body(m, b));
        check(
          bool,
          "a is open again",
          true,
          Divided.cell(a, Cells.divided(m)) != None,
        );
        check(bool, "b has the focus", true, focused(m, b, Body));
        let m = Cells.run(m, insert("9"));
        check(string, "and takes the keys", "92", Cells.body(m, b));
        check(string, "a is untouched", "1", Cells.body(m, a));
      },
    ),
    test_case(
      "undo and redo hand the keys to the cell they restore",
      `Quick,
      () => {
        let m = Cells.run(Cells.mk(), Refresh);
        let (a, b) = (Cells.row(m, "a"), Cells.row(m, "b"));
        let m = Cells.run(m, Cells.workspace(FocusToggle(a)));
        let m = Cells.run(m, Cells.workspace(FocusToggle(b)));
        let caret_in = (id): Page.Update.t =>
          Cells.workspace(
            Stack(
              id,
              Body,
              MainEditor(Perform(Move(Local(Right, ByChar)))),
            ),
          );
        let m = Cells.run(m, caret_in(a));
        let m = Cells.run(m, insert("0"));
        let m = Cells.run(m, caret_in(b));
        /* the restored focus names an open cell, the stack is selected,
           and the DOM focus follows unless something else holds it */
        let keys_to = (m: History.Model.t, id) => {
          let d = Cells.divided(m);
          Divided.cell(id, d) != None
          && Divided.active(d) == Some((id, Body))
          && (
            switch (m.current.selection) {
            | Scratch(Stack(_)) => true
            | _ => false
            }
          )
          && Haz3lcore.FocusEffect.scheduled^ == Some(CellIfLost);
        };
        Haz3lcore.FocusEffect.scheduled := None;
        let m = Cells.run(m, undo);
        check(string, "the edit is undone", "1", Cells.body(m, a));
        check(bool, "undo gives a the keys", true, keys_to(m, a));
        Haz3lcore.FocusEffect.scheduled := None;
        let m = Cells.run(m, redo);
        check(string, "the edit is redone", "10", Cells.body(m, a));
        check(bool, "redo gives b the keys", true, keys_to(m, b));
        let m = Cells.run(m, undo);
        let m = Cells.run(m, insert("9"));
        check(string, "typing reaches a", "19", Cells.body(m, a));
        check(string, "b is untouched", "2", Cells.body(m, b));
      },
    ),
    test_case(
      "an undo that closes the cells hands the keys to one editor",
      `Quick,
      () => {
        let m = Cells.run(Cells.mk(), Refresh);
        let a = Cells.row(m, "a");
        let m = Cells.run(m, Cells.workspace(FocusToggle(a)));
        let m = Cells.run(m, insert("0"));
        let m = Cells.run(m, Cells.workspace(FocusToggle(a)));
        Haz3lcore.FocusEffect.scheduled := None;
        /* the snapshot has a open, the view has nothing pinned */
        let m = Cells.run(m, undo);
        check(
          string,
          "the edit is undone",
          "let a = 1 in\nlet b = 2 in\na + b",
          text_of(m),
        );
        check(
          bool,
          "one editor, selected, with the keys",
          true,
          (
            switch (Cells.program(m), m.current.selection) {
            | (Whole(_), Scratch(Cell(MainEditor))) => true
            | _ => false
            }
          )
          && Haz3lcore.FocusEffect.scheduled^ == Some(CellIfLost),
        );
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
  ],
);
