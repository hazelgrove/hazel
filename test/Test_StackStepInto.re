open Alcotest;
open Language;
open Web;

/* a stack's cells: their probes sample in the program's run */

module Cells = Test_Undo.Cells;

let src = {|let f = fun n -> n + 1 in
let g(x) = x * 2 in
let w(x) = let y = x + 1 in y * 2 in
let r = fun n -> if n == 0 then 0 else r(n - 1) in
let t = let sq = fun x -> x * x in sq(3) in
module M = {
  let h = fun y -> y - 1;
  let k(z) = z + 10;
  let u = h(3)
} in
f(1) + g(2) + w(1) + r(2) + t + M.h(3) + M.k(4) + M.u|};

let member = (m: History.Model.t, label: string): Id.t =>
  CorpusUtil.row(Program.statics(Cells.program(m)).term, ["M", label]);

/* open [rows] as cells, each with its statics, as a stack shows them */
let opened = (rows: list(History.Model.t => Id.t)): History.Model.t => {
  let m = Cells.run(Cells.mk(~src, ()), Refresh);
  List.fold_left(
    (m, row) => Cells.run(m, Cells.workspace(FocusToggle(row(m)))),
    m,
    rows,
  )
  |> Cells.run(_, Editors(Scratch(RefreshStatics)));
};

let pane = (m: History.Model.t, cell: Id.t): CellEditor.Model.t =>
  Option.get(Divided.cell(cell, Cells.divided(m))).e_body;

let zipper = (m, cell) => pane(m, cell).editor.editor.state.zipper;

/* the program's run samples every probe of [cell]'s body */
let sampled = (m: History.Model.t, cell: Id.t): bool => {
  let targets = Divided.statics(Cells.divided(m)).targets;
  Id.Map.for_all(
    (id, _) => Id.Map.mem(id, targets),
    Haz3lcore.CachedStatics.probe_ids_of_zipper(zipper(m, cell)),
  );
};

/* a multi probe in a cell (Cmd+E on a definition) samples, as in one
   editor: the run's targets were the manual probes alone */
let multi_samples = () => {
  let m = opened([m => Cells.row(m, "M")]);
  let mid = Cells.row(m, "M");
  let in_cell = a =>
    Cells.workspace(Stack(mid, Body, MainEditor(Perform(a))));
  let m = Cells.run(m, in_cell(Move(Goal(TileId(member(m, "u"))))));
  let m = Cells.run(m, in_cell(Probe(ToggleManual)));
  check(
    bool,
    "u's multi expands",
    false,
    Id.Map.is_empty(zipper(m, mid).refractors.multis.ephemerals),
  );
  check(bool, "the run samples it", true, sampled(m, mid));
};

let tests = (
  "StackStepInto",
  [test_case("a cell's multi probe samples", `Quick, multi_samples)],
);
