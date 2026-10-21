open Alcotest;
open Web;
open Haz3lcore;

/* An undo snapshot drops its editors' caches. Restored, it takes the live
   editors': the members the step didn't change are the live program's
   pieces, so the parse, the measure and the analysis redo only what the
   step changed, and the code view keeps its chunks (keyed by the measure
   cache). The program is List to `cons` (OM.short_src), 16 members: a
   whole parse is 17 items */

module Cells = Test_Undo.Cells;
module OM = Test_OpenMember;

/* what a step redid, per layer */
type work = {
  frames: int, /* its own and those it scheduled */
  parsed: int, /* items go_incr parsed */
  measured: int, /* chunks Measured.Incr measured */
  compared: int /* item heads DefStatics compared structurally */
};

/* [action] through the app's loop (as Cells.run): its frame, then the
   frames it scheduled (an undo's realizes the view, analyzing the program) */
let rec frames = (m: History.Model.t, action: Page.Update.t) => {
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
      model
    |> History.Update.calculate(
         ~schedule_action,
         ~is_edited=true,
         ~dynamics=false,
       );
  List.fold_left(
    ((m, n), a) => {
      let (m, k) = frames(m, a);
      (m, n + k);
    },
    (m, 1),
    scheduled^,
  );
};

let step = (m: History.Model.t, action: Page.Update.t) => {
  let (p0, m0, c0) = (
    MakeTerm.Incr.full_analyzed^,
    Measured.Incr.built^,
    DefStatics.head_compares^,
  );
  let (m, n) = frames(m, action);
  (
    m,
    {
      frames: n,
      parsed: MakeTerm.Incr.full_analyzed^ - p0,
      measured: Measured.Incr.built^ - m0,
      compared: DefStatics.head_compares^ - c0,
    },
  );
};

let at_most = (what, bound, n) =>
  check(
    bool,
    Printf.sprintf("%s: %d, at most %d", what, n, bound),
    true,
    n <= bound,
  );

let editor = (m: History.Model.t): CellEditor.Model.t =>
  switch (Cells.program(m)) {
  | Whole(e) => e
  | Divided(_) => fail("divided")
  };

/* the code view's chunk key (ChunkViews: the measure cache) */
let owner = (e: CellEditor.Model.t) => e.editor.editor.syntax.measured.owner;

/* the member parses in [e]'s go_incr cache, by anchor: below the top
   level, a module's members and a let chain's links */
let member_parses =
    (e: CellEditor.Model.t): list((Id.t, MakeTerm.Incr.entry_full)) => {
  let rec below = (e: MakeTerm.Incr.entry_full) =>
    switch (e.f_kids) {
    | Some((_, kids)) =>
      List.concat_map(((id, k)) => [(id, k), ...below(k)], kids)
    | None => []
    };
  switch (e.editor.editor.syntax.t_cache.c_prev) {
  | Some((_, _, entries, _, _, _)) =>
    List.concat_map(((_, e)) => below(e), entries)
  | None => []
  };
};

/* those of [after] that aren't [before]'s */
let reparsed = (~before, after) =>
  List.filter(
    ((id, e)) =>
      switch (List.assoc_opt(id, before)) {
      | Some(e') => e' !== e
      | None => true
      },
    after,
  );

let at_list_length = (~root: Sort.t, ()) => {
  let src = OM.short_src(~root);
  let m = OM.refresh(Cells.run(OM.mk(~root, src), Refresh));
  OM.refresh(
    Cells.run(
      m,
      Globals(
        ActiveEditor(Move(Point(OM.after(src, "    | [] => 0"), None))),
      ),
    ),
  );
};

/* List.length edited, then the edit undone and redone. Each frame
   re-parses the member and the module around it at most, and re-measures
   their chunks; the analysis compares the heads of the items holding the
   change (two parts each), not each member's (32) */
let one_member = (~root: Sort.t, ()) => {
  let m = at_list_length(~root, ());
  let before = Test_Undo.text_of(m);
  let m = OM.refresh(Cells.run(m, Test_Undo.insert("0")));
  let edited = Test_Undo.text_of(m);
  check(bool, "the key edits List.length", true, edited != before);
  let restores = (what, action, text, m) => {
    let (o, parses) = (owner(editor(m)), member_parses(editor(m)));
    let (m, w) = step(m, action);
    check(
      string,
      what ++ " restores the program",
      text,
      Test_Undo.text_of(m),
    );
    check(
      bool,
      what ++ ": the members were parsed",
      true,
      List.length(parses) >= 16,
    );
    check(
      int,
      what ++ ": members parsed again",
      1,
      List.length(reparsed(~before=parses, member_parses(editor(m)))),
    );
    at_most(what ++ ": items parsed", 2 * w.frames, w.parsed);
    at_most(what ++ ": chunks measured", 2 * w.frames, w.measured);
    at_most(what ++ ": item heads compared", 6, w.compared);
    check(int, what ++ " keeps the view's chunks", o, owner(editor(m)));
    OM.refresh(m);
  };
  m
  |> restores("undo", Test_Undo.undo, before)
  |> restores("redo", Test_Undo.redo, edited)
  |> ignore;
};

let length = m => OM.row(m, ["List", "length"]);
let is_open = (m, id) =>
  switch (Cells.program(m)) {
  | Divided(d) => Divided.cell(id, d) != None
  | Whole(_) => false
  };
let body_editor = (m, id) =>
  Option.get(Divided.cell(id, Cells.divided(m))).e_body;

/* List.length open as a cell, the caret after its `| [] => 0` */
let in_cell = () => {
  let m =
    OM.refresh(
      Cells.run(OM.mk(~root=Mod, OM.short_src(~root=Mod)), Refresh),
    );
  let id = length(m);
  let m = OM.refresh(Cells.run(m, Cells.workspace(FocusToggle(id))));
  check(bool, "List.length's cell opens", true, is_open(m, id));
  let to_ = OM.after(Cells.body(m, id), "| [] => 0");
  let m =
    Cells.run(
      m,
      Cells.workspace(
        Stack(id, Body, MainEditor(Perform(Move(Point(to_, None))))),
      ),
    );
  (OM.refresh(m), id);
};

/* in a stack, each open cell's panes keep theirs */
let cell_edit = () => {
  let (m, id) = in_cell();
  let body = Cells.body(m, id);
  let m = OM.refresh(Cells.run(m, Test_Undo.insert("0")));
  check(bool, "typing reaches the cell", true, Cells.body(m, id) != body);
  let o = owner(body_editor(m, id));
  let (m, _) = step(m, Test_Undo.undo);
  check(string, "undo restores the cell", body, Cells.body(m, id));
  check(
    int,
    "the cell keeps its view's chunks",
    o,
    owner(body_editor(m, id)),
  );
};

/* the whole editor a stack was cut from (its shell) has the program's
   caches: an undo back to before the cut takes them, and so does the join
   when the last cell closes. Each re-parses a few items of 17 */
let across_cells = () => {
  let m = at_list_length(~root=Mod, ());
  let before = OM.doc(m);
  let m = OM.refresh(Cells.run(m, Test_Undo.insert("0")));
  let id = length(m);
  let m = OM.refresh(Cells.run(m, Cells.workspace(FocusToggle(id))));
  check(bool, "List.length's cell opens", true, is_open(m, id));
  /* the snapshot is whole; realizing the view opens the cell again */
  let (m, w) = step(m, Test_Undo.undo);
  check(string, "undo restores the program", before, OM.doc(m));
  check(bool, "the cell is open again", true, is_open(m, id));
  at_most("the undo: items parsed", 6, w.parsed);
  let (m, w) = step(OM.refresh(m), Cells.workspace(FocusToggle(id)));
  check(bool, "the last cell closes", false, is_open(m, id));
  at_most("the join: items parsed", 6, w.parsed);
};

/* restored with a cell the view doesn't show: realizing it closes the
   cell, and that join takes the caches the live whole editor had */
let undo_closes = () => {
  let (m, id) = in_cell();
  let before = OM.doc(m);
  let m = OM.refresh(Cells.run(m, Test_Undo.insert("0")));
  let m = OM.refresh(Cells.run(m, Cells.workspace(FocusToggle(id))));
  check(bool, "closed", false, is_open(m, id));
  let o = owner(editor(m));
  let (m, w) = step(m, Test_Undo.undo);
  check(bool, "the view keeps it closed", false, is_open(m, id));
  check(string, "undo restores the program", before, OM.doc(m));
  at_most("items parsed", 6, w.parsed);
  check(
    int,
    "the whole editor keeps its view's chunks",
    o,
    owner(editor(m)),
  );
};

let tests = (
  "UndoCaches",
  [
    test_case(
      "undo and redo of a member's edit redo only that member",
      `Quick,
      one_member(~root=Mod),
    ),
    test_case(
      "the same at an Exp root, the module bound by `in`",
      `Quick,
      one_member(~root=Exp),
    ),
    test_case("an open cell's edit, undone", `Quick, cell_edit),
    test_case(
      "an undo before a cell opened, then the cell closed",
      `Quick,
      across_cells,
    ),
    test_case("an undo whose view closes the cell", `Quick, undo_closes),
  ],
);
