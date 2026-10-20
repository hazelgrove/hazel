open Alcotest;
open Web;
open Haz3lcore;

/* Undo and redo restore a program, not its analysis. As after a key, the
   restored program shows the live statics and run until the typing
   debounce analyzes it and asks for a run ([refresh]: the debounce
   firing). List, 65 members, with List.length's `| [] => 0` as `0.1` (a
   Float where an Int goes): the 1 undone and redone, both versions have
   the error, so the marks shown meanwhile are real ones */

module Cells = Test_Undo.Cells;
module OM = Test_OpenMember;

/* the page's loop with dynamics on: an action, its calculate, then what
   it scheduled */
let rec run = (m: History.Model.t, action: Page.Update.t): History.Model.t => {
  let scheduled = ref([]);
  let schedule_action = a => scheduled := scheduled^ @ [a];
  let u =
    History.Update.update(
      ~import_log=_ => (),
      ~get_log_and=_ => (),
      ~schedule_action,
      action,
      m,
    );
  let m =
    History.Update.calculate(
      ~schedule_action,
      ~is_edited=u.is_edit,
      ~dynamics=true,
      u.model,
    );
  List.fold_left(run, m, scheduled^);
};
let refresh = m => run(m, Editors(Scratch(RefreshStatics)));

/* dynamics are on: eval requests go to a worker that never answers */
let with_worker = (f: unit => unit, ()) => {
  let worker = WorkerClient.worker_ref^;
  WorkerClient.worker_ref :=
    Some(
      Js_of_ocaml.Js.Unsafe.obj([|
        (
          "postMessage",
          Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.wrap_callback(_ => ())),
        ),
      |]),
    );
  Fun.protect(
    ~finally=
      () => {
        WorkerClient.cancel();
        WorkerClient.worker_ref := worker;
      },
    f,
  );
};

/* the analyses (DefStatics runs) and eval requests during [f] */
let counted = (f: unit => 'a): ('a, int, int) => {
  let hook = DefStatics.after_calc^;
  let (runs, requests) = (ref(0), WorkerClient.next_request_id^);
  DefStatics.after_calc :=
    Some(
      (settings, t) => {
        incr(runs);
        Option.iter(h => h(settings, t), hook);
      },
    );
  let x = Fun.protect(~finally=() => DefStatics.after_calc := hook, f);
  (x, runs^, WorkerClient.next_request_id^ - requests);
};

let whole = (m: History.Model.t): CellEditor.Model.t =>
  switch (Cells.program(m)) {
  | Whole(e) => e
  | Divided(_) => fail("divided")
  };

/* [statics] are an analysis of [doc] as parsed now that agrees with a
   cold one (and a monolithic one) */
let analyze_fresh =
    (
      what,
      m: History.Model.t,
      ~root,
      doc: Segment.t,
      statics: CachedStatics.t,
    ) => {
  check(
    bool,
    what ++ ": the term is the program's",
    true,
    Language.Exp.fast_equal(
      MakeTerm.semantic_term(~root, doc),
      statics.term,
    ),
  );
  switch (DefStatics.cached(statics.term)) {
  | None => fail(what ++ ": no analysis of the term")
  | Some(ds) =>
    check(
      bool,
      what ++ ": the statics are that analysis",
      true,
      ds.merged === statics.info_map,
    );
    check(
      list(string),
      what ++ ": as a cold analysis",
      [],
      DefStaticsCheck.divergences(
        ~settings=m.current.globals.settings.core,
        ~cold=true,
        ds,
      ),
    );
  };
};

/* [action] (an undo or redo) analyzes nothing and asks for no run: what
   [shown] reads stays the live program's; then the debounce analyzes */
let deferred =
    (
      what,
      action: Page.Update.t,
      ~shown: History.Model.t => list(CachedStatics.t),
      ~fresh: (string, History.Model.t) => unit,
      m: History.Model.t,
    ) => {
  let live = shown(m);
  check(
    bool,
    what ++ ": the live program has errors",
    true,
    List.exists((s: CachedStatics.t) => s.error_ids != [], live),
  );
  let (m, runs, requests) = counted(() => run(m, action));
  check(int, what ++ ": no analysis", 0, runs);
  check(int, what ++ ": no eval request", 0, requests);
  List.iter2(
    (a: CachedStatics.t, b: CachedStatics.t) => {
      check(
        bool,
        what ++ ": the live errors stay",
        true,
        a.error_ids === b.error_ids,
      );
      check(
        bool,
        what ++ ": the live analysis stays",
        true,
        a.info_map === b.info_map,
      );
    },
    live,
    shown(m),
  );
  let (m, runs, requests) = counted(() => refresh(m));
  check(bool, what ++ ": the debounce analyzes", true, runs > 0);
  check(int, what ++ ": and asks for one run", 1, requests);
  fresh(what, m);
  m;
};

let program_statics = m => Program.statics(Cells.program(m));

let one_member = (~root: Sort.t, ()) => {
  let src = OM.src(~root);
  let m = refresh(run(OM.mk(~root, src), Refresh));
  let m =
    run(
      m,
      Globals(
        ActiveEditor(Move(Point(OM.after(src, "    | [] => 0"), None))),
      ),
    );
  let m = refresh(run(m, Test_Undo.insert(".")));
  let undone = Test_Undo.text_of(m);
  let m = refresh(run(m, Test_Undo.insert("1")));
  let edited = Test_Undo.text_of(m);
  let shown = m => [whole(m).editor.statics];
  let fresh = (what, m) => {
    let e = whole(m);
    analyze_fresh(
      what,
      m,
      ~root,
      MakeTerm.semantic_source(e.editor.editor.state.zipper),
      e.editor.statics,
    );
  };
  let m = deferred("undo", Test_Undo.undo, ~shown, ~fresh, m);
  check(string, "undo restores the program", undone, Test_Undo.text_of(m));
  let m = deferred("redo", Test_Undo.redo, ~shown, ~fresh, m);
  check(string, "redo restores the edit", edited, Test_Undo.text_of(m));
};

/* the same in List.length's open cell: the cell's and the program's */
let in_cell = () => {
  let root = Sort.Mod;
  let m = refresh(run(OM.mk(~root, OM.src(~root)), Refresh));
  let id = OM.row(m, ["List", "length"]);
  let m = refresh(run(m, Cells.workspace(FocusToggle(id))));
  let to_ = OM.after(Cells.body(m, id), "| [] => 0");
  let m =
    run(
      m,
      Cells.workspace(
        Stack(id, Body, MainEditor(Perform(Move(Point(to_, None))))),
      ),
    );
  let m = refresh(run(m, Test_Undo.insert(".")));
  let undone = Cells.body(m, id);
  let m = refresh(run(m, Test_Undo.insert("1")));
  let edited = Cells.body(m, id);
  check(bool, "typing reaches the cell", true, undone != edited);
  let shown = m => [
    program_statics(m),
    Option.get(Divided.cell(id, Cells.divided(m))).e_body.editor.statics,
  ];
  let fresh = (what, m) =>
    analyze_fresh(
      what,
      m,
      ~root,
      Divided.document(Cells.divided(m)),
      program_statics(m),
    );
  let m = deferred("undo", Test_Undo.undo, ~shown, ~fresh, m);
  check(string, "undo restores the cell", undone, Cells.body(m, id));
  let m = deferred("redo", Test_Undo.redo, ~shown, ~fresh, m);
  check(string, "redo restores the edit", edited, Cells.body(m, id));
};

let tests = (
  "UndoStatics",
  [
    test_case(
      "undo and redo show the live statics until the debounce",
      `Quick,
      with_worker(one_member(~root=Mod)),
    ),
    test_case(
      "the same at an Exp root, the module bound by `in`",
      `Quick,
      with_worker(one_member(~root=Exp)),
    ),
    test_case("the same in an open cell", `Quick, with_worker(in_cell)),
  ],
);
