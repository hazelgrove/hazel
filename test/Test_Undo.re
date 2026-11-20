open Alcotest;
open Web;

/* Integration tests for the page-level undo/redo mechanics in History.re.
 * These drive History.Update.update with real page actions, the same code
 * path the UI uses (Globals(ActiveEditor(_)) for edits, Globals(Undo/Redo)
 * for history navigation). */

let mk_model = (): History.Model.t => {
  let globals = Globals.Model.init();
  let (default_current, slides) = Lazy.force(Init.startup).scratch;
  let default_names = List.map(~f=fst, slides);
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

let set = (a: Settings.Update.t): Page.Update.t => Globals(Set(a));
let hover: Page.Update.t =
  set(ExplainThis(SetHighlight(Hover(Haz3lcore.Id.mk()))));
let unset_hover: Page.Update.t = set(ExplainThis(SetHighlight(UnsetHover)));

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
        check(
          bool,
          "insert changed the program",
          true,
          !String.equal(text_of(m1), t0),
        );
        check(int, "edit pushed one undo entry", 1, undo_len(m1));
        let m2 = apply(m1, undo);
        check(string, "undo restores original text", t0, text_of(m2));
        check(
          bool,
          "undo restores the exact pre-edit model",
          true,
          phys_equal(m2.current, m0.current),
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
          phys_equal(m3.current, m1.current),
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
    test_case(
      "ExplainThis hover leaves undo and redo untouched",
      `Quick,
      () => {
        let m0 = mk_model();
        let t0 = text_of(m0);
        let m1 = apply(m0, insert("1"));
        let t1 = text_of(m1);
        let m2 = apply(apply(m1, hover), unset_hover);
        check(int, "hover pushed no undo entry", 1, undo_len(m2));
        let m3 = apply(m2, undo);
        check(string, "one undo reverts the edit", t0, text_of(m3));
        /* Mousing over the ExplainThis panel after an undo used to clear
           the redo stack ("Cannot redo"). */
        let m4 = apply(apply(m3, hover), unset_hover);
        check(int, "hover preserved the redo stack", 1, redo_len(m4));
        check(int, "hover pushed no undo entry", 0, undo_len(m4));
        let m5 = apply(m4, redo);
        check(string, "redo still works after hover", t1, text_of(m5));
      },
    ),
    test_case(
      "panel toggles and settings applied on the user's behalf are not undoable",
      `Quick,
      () => {
        let m0 = mk_model();
        let m1 = apply(m0, insert("1"));
        let m2 = apply(m1, undo);
        let m3 =
          List.fold(
            ~init=m2,
            ~f=apply,
            [
              /* what Page.update schedules on entering a Probes lesson */
              set(ApplyAutoprobe(All)),
              set(SetShortcutOverrides([("Undo", None)])),
              set(Sidebar(ToggleShow)),
              set(ShowDebugPanel),
            ],
          );
        check(
          bool,
          "the settings did change",
          true,
          Haz3lcore.AutoProbe.equal(
            m3.current.globals.settings.autoprobe_mode,
            All,
          ),
        );
        check(int, "no undo entry pushed", 0, undo_len(m3));
        check(int, "redo stack preserved", 1, redo_len(m3));
      },
    ),
    test_case(
      "deliberate settings toggles and edits are still undoable",
      `Quick,
      () => {
        let m0 = mk_model();
        let m1 = apply(m0, set(Statics));
        check(int, "Statics toggle pushed an undo entry", 1, undo_len(m1));
        let m2 = apply(m1, undo);
        check(
          bool,
          "undo restores the Statics setting",
          m0.current.globals.settings.core.statics,
          m2.current.globals.settings.core.statics,
        );
        let m3 = apply(apply(m2, hover), insert("1"));
        check(int, "an edit after hover still pushes", 1, undo_len(m3));
        check(int, "and still clears redo", 0, redo_len(m3));
      },
    ),
    test_case(
      "the probe toolbar's auto-probe toggle is undoable",
      `Quick,
      () => {
        let m0 = mk_model();
        let before = m0.current.globals.settings.autoprobe_mode;
        let m1 = apply(m0, set(SetAutoprobe(All)));
        check(int, "toolbar toggle pushed an undo entry", 1, undo_len(m1));
        let m2 = apply(m1, undo);
        check(
          bool,
          "undo restores the previous auto-probe mode",
          true,
          Haz3lcore.AutoProbe.equal(
            m2.current.globals.settings.autoprobe_mode,
            before,
          ),
        );
      },
    ),
    test_case(
      "hover is not an edit and is not logged",
      `Quick,
      () => {
        let updated =
          Settings.Update.update(
            ~action=ExplainThis(SetHighlight(Hover(Haz3lcore.Id.mk()))),
            ~settings=Settings.Model.init,
          );
        check(bool, "not historic", false, updated.historic);
        check(
          bool,
          "not an edit (no autosave/recompute)",
          false,
          updated.is_edit,
        );
        check(bool, "not logged", false, updated.logged);
        let sidebar =
          Settings.Update.update(
            ~action=Sidebar(ToggleShow),
            ~settings=Settings.Model.init,
          );
        check(bool, "sidebar toggle not historic", false, sidebar.historic);
        check(bool, "sidebar toggle still autosaved", true, sidebar.is_edit);
      },
    ),
    test_case(
      "undo keeps the current font metrics",
      `Quick,
      () => {
        /* On load a slide's default is applied (historic) before the font
           is first measured, so the undo entry holds placeholder metrics */
        let m0 = mk_model();
        let m1 = apply(m0, insert("1"));
        let fm: FontMetrics.t = {
          row_height: 25.,
          col_width: 10.4,
        };
        let m2 = apply(m1, Globals(SetFontMetrics(fm)));
        let m3 = apply(m2, undo);
        check(
          bool,
          "undo kept the measured font",
          true,
          FontMetrics.equal(m3.current.globals.font_metrics, fm),
        );
        /* Re-measure (e.g. zoom) while undone: redo keeps the new metrics,
           not the ones the redo entry was taken with */
        let fm2: FontMetrics.t = {
          row_height: 30.,
          col_width: 12.,
        };
        let m4 = apply(m3, Globals(SetFontMetrics(fm2)));
        let m5 = apply(m4, redo);
        check(
          bool,
          "redo kept the re-measured font",
          true,
          FontMetrics.equal(m5.current.globals.font_metrics, fm2),
        );
      },
    ),
    test_case(
      "undo and redo keep the held modifier keys",
      `Quick,
      () => {
        /* Cmd+Z runs with Cmd held; the snapshot was taken with it up */
        let m0 = mk_model();
        let m1 = apply(m0, insert("1"));
        let m2 = apply(m1, Globals(SetMetaDown(true)));
        let m3 = apply(m2, undo);
        check(bool, "undo kept Cmd held", true, m3.current.globals.meta_down);
        /* Redo from the command palette, with Cmd released; the redo entry
           was taken with it held */
        let m4 = apply(m3, Globals(SetMetaDown(false)));
        let m5 = apply(m4, redo);
        check(
          bool,
          "redo kept Cmd released",
          false,
          m5.current.globals.meta_down,
        );
      },
    ),
    test_case(
      "undo keeps the visible rows within the same editor",
      `Quick,
      () => {
        let rows = (first, last): Globals.VisibleRows.t => {
          first,
          last,
        };
        let m0 =
          apply(mk_model(), Globals(UpdateVisibleRows(rows(0, 40))));
        let m1 = apply(m0, insert("1"));
        /* Scroll down: the snapshot still holds the old range */
        let m2 = apply(m1, Globals(UpdateVisibleRows(rows(30, 70))));
        let m3 = apply(m2, undo);
        check(
          bool,
          "undo kept the scrolled range",
          true,
          Option.equal(
            Globals.VisibleRows.equal,
            m3.current.globals.visible_rows,
            Some(rows(30, 70)),
          ),
        );
      },
    ),
    test_case(
      "undo into another editor resets the visible rows",
      `Quick,
      () => {
        /* The live range belongs to the current slide; a restored state
           showing a different slide has it re-measured */
        let rows = (first, last): Globals.VisibleRows.t => {
          first,
          last,
        };
        let key = (m: History.Model.t) =>
          Editors.Model.editor_key(m.current.editors);
        let m0 =
          apply(mk_model(), Globals(UpdateVisibleRows(rows(0, 40))));
        /* Adding a slide is undoable and switches to the new slide */
        let m1 = apply(m0, Editors(Scratch(AddSlide)));
        check(
          bool,
          "switched to another slide",
          true,
          !String.equal(key(m1), key(m0)),
        );
        let m2 = apply(m1, Globals(UpdateVisibleRows(rows(30, 70))));
        let m3 = apply(m2, undo);
        check(
          string,
          "undo went back to the first slide",
          key(m0),
          key(m3),
        );
        check(
          bool,
          "undo reset the range",
          true,
          Option.is_none(m3.current.globals.visible_rows),
        );
      },
    ),
  ],
);
