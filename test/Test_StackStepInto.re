open Alcotest;
open Language;
open Web;

/* a stack's cells: their probes sample in the program's run, and a step
   into a function another item defines opens that item and steps in
   there */

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

/* the program's ⇒ row (a function's body has one of its own) */
let trail = (m: History.Model.t): Id.t =>
  List.find_map(
    (n: OutlineTree.node) => n.o_kind == KTrail ? n.o_id : None,
    OutlineTree.of_term(Program.statics(Cells.program(m)).term),
  )
  |> Option.get;

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

/* the call to [name] (bare or through a module) in [cell]'s body */
let call = (m: History.Model.t, cell: Id.t, name: string): Id.t => {
  let named = (fn: Exp.t) =>
    switch (fn.term) {
    | Var(x)
    | Dot(_, {term: Label(x), _}) => x == name
    | _ => false
    };
  Id.Map.fold(
    (id, info: Info.t, found) =>
      switch (found, info) {
      | (None, InfoExp({user_term: {term: Ap(_, fn, _), _}, _}))
          when named(fn) =>
        Some(id)
      | _ => found
      },
    pane(m, cell).editor.statics.info_map,
    None,
  )
  |> Option.get;
};

/* the call's first sample in the whole program's run */
let sample = (m: History.Model.t, ap: Id.t): Sample.t => {
  let statics = Divided.statics(Cells.divided(m));
  let (_, state) =
    Evaluator.evaluate(
      ~eval_info=
        EvalInfo.of_targets(
          Id.Map.singleton(
            ap,
            {
              refs: [],
              full: false,
            }: Sample.capture_spec,
          ),
        ),
      ~env=Builtins.env_init(),
      statics.elaborated,
    );
  switch (Id.Map.find_opt(ap, EvaluatorState.get_probes(state))) {
  | Some([s, ..._]) => s
  | _ => fail("no sample")
  };
};

/* the probe menu's step into on that sample, as ProbeProj sends it */
let step_into = (m: History.Model.t, cell: Id.t, ap: Id.t) => {
  let s = sample(m, ap);
  let frame: CallStack.frame = {
    id: ap,
    name: Option.bind(s.frame, (f: CallStack.frame) => f.name),
    fn_def_id: Option.bind(s.frame, (f: CallStack.frame) => f.fn_def_id),
  };
  let action: Page.Update.t =
    Cells.workspace(
      Stack(
        cell,
        Body,
        MainEditor(Perform(Probe(StepInto(s.call_stack, frame)))),
      ),
    );
  (action, [frame, ...s.call_stack]);
};

/* the terms [cell]'s body probes */
let probed = (m: History.Model.t, cell: Id.t): list(string) => {
  let ed = pane(m, cell).editor.editor;
  List.map(
    ((id, ())) =>
      switch (Haz3lcore.TermData.segment(id, ed.syntax.term_data)) {
      | Some(seg) =>
        Haz3lcore.Printer.of_segment(
          ~holes="?",
          ~indent="",
          ~is_single_line=true,
          seg,
        )
        |> String.trim
      | None => "<not here>"
      },
    Id.Map.bindings(ed.state.zipper.refractors.multis.ids),
  )
  |> List.sort(compare);
};

let pinned = (m: History.Model.t, cell: Id.t): option(CallStack.t) =>
  zipper(m, cell).refractors.sample_focus.pinned_stack;

let manuals = (m: History.Model.t, cell: Id.t): list(Id.t) =>
  List.map(fst, zipper(m, cell).refractors.manuals);

let stack = testable(Fmt.of_to_string(CallStack.show), CallStack.equal);
let id = testable(Fmt.of_to_string(Id.str3), Id.equal);

/* the page's follow-ups for [action], None when it isn't routed */
let follow_ups =
    (m: History.Model.t, action: Page.Update.t)
    : option(list(ScratchMode.Update.t)) =>
  switch (m.current.editors, action) {
  | (Scratch(s), Editors(Scratch(a))) =>
    ScratchMode.Selection.stack_jump_override(a, s)
    |> Option.map(((_, _, ks)) => ks)
  | _ => fail("not a deck action")
  };

let routed = (m, action) => follow_ups(m, action) != None;

let cell_count = (m: History.Model.t): int =>
  List.length(Divided.cells(Cells.divided(m)));

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

/* from ⇒, a step into [name]'s call opens [target]'s cell, probes
   [body] there and focuses the stepped-into call's sample */
let across = (~name, ~target, ~body, ()) => {
  let m = opened([trail]);
  let t = trail(m);
  let (action, stepped) = step_into(m, t, call(m, t, name));
  let before = zipper(m, t);
  let m = Cells.run(m, action);
  let target = target(m);
  check(
    bool,
    "opens",
    true,
    Divided.cell(target, Cells.divided(m)) != None,
  );
  check(list(string), "probes the body", body, probed(m, target));
  check(bool, "the run samples the body", true, sampled(m, target));
  check(
    option(stack),
    "the call's sample",
    Some(stepped),
    pinned(m, target),
  );
  check(
    bool,
    "takes the focus",
    true,
    Cells.focus(m) == Some((target, Body)),
  );
  check(
    bool,
    "the calling cell is as it was",
    true,
    Haz3lcore.Zipper.same_content(before, zipper(m, t)),
  );
  check(
    list(id),
    "no probe on the other cell's call",
    [],
    manuals(m, target),
  );
};

let bare = () =>
  across(
    ~name="f",
    ~target=m => Cells.row(m, "f"),
    ~body=["n", "n + 1"],
    (),
  );

/* `let g(x) = …`: its own definition, its params in the header */
let sugar = () => {
  across(~name="g", ~target=m => Cells.row(m, "g"), ~body=["x * 2"], ());
  /* a let chain definition is a row itself: w opens, not y (its probe
     anchors at the chain's first definition) */
  across(~name="w", ~target=m => Cells.row(m, "w"), ~body=["x + 1"], ());
};

let recursive = () =>
  across(
    ~name="r",
    ~target=m => Cells.row(m, "r"),
    ~body=["if n == 0 then 0 else r(n - 1)", "n"],
    (),
  );

let qualified = () => {
  across(~name="h", ~target=m => member(m, "h"), ~body=["y", "y - 1"], ());
  /* reached only through its closure: the sugar's Fun is in no text */
  across(~name="k", ~target=m => member(m, "k"), ~body=["z + 10"], ());
};

/* a member calling a member: the module's other item opens */
let between_members = () => {
  let m = opened([m => member(m, "u")]);
  let u = member(m, "u");
  let (action, stepped) = step_into(m, u, call(m, u, "h"));
  let m = Cells.run(m, action);
  let h = member(m, "h");
  check(list(string), "probes h", ["y", "y - 1"], probed(m, h));
  check(option(stack), "the call's sample", Some(stepped), pinned(m, h));
};

let already_open = () => {
  let m = opened([m => Cells.row(m, "f"), trail]);
  let (f, t) = (Cells.row(m, "f"), trail(m));
  let cells = cell_count(m);
  let (action, stepped) = step_into(m, t, call(m, t, "f"));
  check(
    bool,
    "steps with no statics refresh",
    true,
    switch (follow_ups(m, action)) {
    | Some([Workspace(Stack(_, Body, _))]) => true
    | _ => false
    },
  );
  let m = Cells.run(m, action);
  check(int, "no cell opens", cells, cell_count(m));
  check(list(string), "probes the body", ["n", "n + 1"], probed(m, f));
  check(option(stack), "the call's sample", Some(stepped), pinned(m, f));
  check(bool, "takes the focus", true, Cells.focus(m) == Some((f, Body)));
  /* a module open as its members: the member's body is there */
  let m = opened([m => Cells.row(m, "M"), trail]);
  let (mid, t) = (Cells.row(m, "M"), trail(m));
  let cells = cell_count(m);
  let (action, stepped) = step_into(m, t, call(m, t, "h"));
  let m = Cells.run(m, action);
  check(int, "no member cell opens", cells, cell_count(m));
  check(bool, "probes h's body", true, List.mem("y - 1", probed(m, mid)));
  check(option(stack), "h's call's sample", Some(stepped), pinned(m, mid));
};

/* the function in the calling cell: not routed, the cell steps itself */
let same_cell = () => {
  let m = opened([m => Cells.row(m, "t")]);
  let t = Cells.row(m, "t");
  let (action, stepped) = step_into(m, t, call(m, t, "sq"));
  check(bool, "not routed", false, routed(m, action));
  let cells = cell_count(m);
  let m = Cells.run(m, action);
  check(int, "no cell opens", cells, cell_count(m));
  check(list(string), "probes sq's body", ["x", "x * x"], probed(m, t));
  check(option(stack), "the call's sample", Some(stepped), pinned(m, t));
  check(
    bool,
    "keeps the call's probe",
    true,
    List.mem(List.hd(stepped).id, manuals(m, t)),
  );
};

/* inside r's own cell, its recursive call resolves there */
let recursive_same_cell = () => {
  let m = opened([m => Cells.row(m, "r")]);
  let r = Cells.row(m, "r");
  let (action, _) = step_into(m, r, call(m, r, "r"));
  check(bool, "not routed", false, routed(m, action));
};

/* one undo step takes the step back; the opened cell stays, as views do */
let undo = () => {
  let m = opened([trail]);
  let t = trail(m);
  let (action, _) = step_into(m, t, call(m, t, "f"));
  let depth = Test_Undo.undo_len(m);
  let m = Cells.run(m, action);
  check(int, "one undo step", depth + 1, Test_Undo.undo_len(m));
  let m = Cells.run(m, Test_Undo.undo);
  let f = Cells.row(m, "f");
  check(
    bool,
    "f stays open",
    true,
    Divided.cell(f, Cells.divided(m)) != None,
  );
  check(list(string), "no probe", [], probed(m, f));
  check(option(stack), "no pin", None, pinned(m, f));
};

let tests = (
  "StackStepInto",
  [
    test_case("a cell's multi probe samples", `Quick, multi_samples),
    test_case("opens the function's item", `Quick, bare),
    test_case("function sugar", `Quick, sugar),
    test_case("recursive function", `Quick, recursive),
    test_case("calls through a module", `Quick, qualified),
    test_case("between module members", `Quick, between_members),
    test_case("an open target's cell", `Quick, already_open),
    test_case("same cell unchanged", `Quick, same_cell),
    test_case("recursive call in its own cell", `Quick, recursive_same_cell),
    test_case("one undo step", `Quick, undo),
  ],
);
