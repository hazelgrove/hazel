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

let canvas_fixture = (): History.Model.t => {
  let m = mk_model();
  let seg =
    Test_StackFocus.parse(
      "type A = Int in type B = Int in let f : A -> B = fun a -> a in f(3)",
    );
  let ed = ScratchMode.Focus.cell_of_seg(seg);
  let statics =
    Haz3lcore.CachedStatics.init_compositional_term(
      ~settings=m.current.globals.settings.core,
      ~probe_ids=Haz3lcore.Id.Map.empty,
      Haz3lcore.MakeTerm.go(seg).term,
    );
  let ed: CellEditor.Model.t = {
    ...ed,
    editor: {
      ...ed.editor,
      statics,
    },
  };
  let scratch =
    switch (m.current.editors) {
    | Scratch(sm) => sm
    | _ => failwith("expected scratch")
    };
  let sp = List.nth(scratch.scratchpads, scratch.current);
  let sp =
    switch (sp.kind) {
    | Code({agent, _}) => {
        ...sp,
        kind:
          Code({
            editor: ed,
            agent,
          }),
      }
    | _ => failwith("expected code")
    };
  {
    ...m,
    current: {
      ...m.current,
      editors:
        Scratch({
          ...scratch,
          focus: None,
          scratchpads:
            Util.ListUtil.put_nth(scratch.current, sp, scratch.scratchpads),
        }),
    },
  };
};

let tests = (
  "Undo",
  [
    test_case(
      "Canvas deletion undo survives selection and render ticks",
      `Quick,
      () => {
        let m0 = canvas_fixture();
        let fid =
          Test_StackFocus.outline_id(
            Page.Update.get_editor(m0.current).statics.term,
            "f",
          );
        let m1 = apply(m0, Editors(Scratch(FocusDef(fid))));
        let m2 =
          apply(
            m1,
            Editors(Scratch(OutlineDefOp(OutlineSidebar.Delete, fid))),
          );
        let actions: list(Page.Update.t) = [
          Globals(Set(Sidebar(SetCanvasFocusTy(None)))),
          Globals(Set(Sidebar(SetCanvasFocus(None)))),
          Globals(Set(Sidebar(SetCanvasPanelHidden(false)))),
          Globals(Set(CanvasTick)),
        ];
        let m3 = List.fold_left(apply, m2, actions);
        check(
          int,
          "display updates do not push history",
          undo_len(m2),
          undo_len(m3),
        );
        let restored = apply(m3, undo);
        let restored =
          History.Update.calculate(
            ~schedule_action=_ => (),
            ~is_edited=true,
            ~dynamics=false,
            restored,
          );
        let term = Page.Update.get_editor(restored.current).statics.term;
        check(
          bool,
          "restored outline has the deleted definition",
          true,
          Test_StackFocus.outline_id(term, "f") == fid,
        );
        let m4 = List.fold_left(apply, restored, actions);
        check(int, "display updates preserve redo", 1, redo_len(m4));
        let m5 = apply(m4, redo);
        check(
          bool,
          "redo removes the definition",
          false,
          switch (
            Str.search_forward(Str.regexp_string("let f"), text_of(m5), 0)
          ) {
          | _ => true
          | exception Not_found => false
          },
        );
      },
    ),
    test_case(
      "undo and redo recalculate compacted view-only snapshots",
      `Quick,
      () => {
        let m = apply(canvas_fixture(), Globals(Set(SetCanvasZoom(1.2))));
        let restore = (action, m) =>
          History.Update.update(
            ~import_log=_ => (),
            ~get_log_and=_ => (),
            ~schedule_action=_ => (),
            action,
            m,
          );
        let u = restore(undo, m);
        check(bool, "undo recalculates", true, u.recalculate && u.is_edit);
        let r = restore(redo, u.model);
        check(bool, "redo recalculates", true, r.recalculate && r.is_edit);
      },
    ),
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
        /* snapshots are COMPACTED (derived caches dropped, recomputed
           on restore), so physical identity no longer holds — source
           state (the text, checked above) is the restoration contract */
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
        /* compacted snapshots: text equality (above) is the contract */
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
  ],
);
