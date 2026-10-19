open Alcotest;
open Web;
open Haz3lcore;

/* An outline op builds a new editor for its program. It takes the live
   editor's caches, which hit for every piece the op didn't change, so the
   parse, the measure and the analysis redo only the items it changed
   (UndoCaches) */

module Cells = Test_Undo.Cells;
module OM = Test_OpenMember;
module UC = Test_UndoCaches;

/* Mega 4k; at a Mod root, its modules end in `;` */
let mega_4k = (~root: Sort.t) => {
  let src = Option.get(CorpusUtil.mega_src("mega-4k.hz"));
  root == Mod
    ? String.split_on_char('\n', src)
      |> List.map(
           fun
           | "} in" => "};"
           | "let final = MetaRunner.run_all(()) in" => "let final = MetaRunner.run_all(());"
           | l => l,
         )
      |> String.concat("\n")
    : src;
};

/* a module's rows, in order */
let rows = (m, name: string): list(string) =>
  switch (
    List.find_opt(
      (n: OutlineTree.node) => n.o_label == name,
      OutlineTree.of_term(OM.term(m)),
    )
  ) {
  | Some(n) => List.map((n: OutlineTree.node) => n.o_label, n.o_children)
  | None => fail("no module " ++ name)
  };

let move_down = (m, path) =>
  UC.step(
    m,
    Editors(Scratch(Outline(DefOp(MoveDown, OM.row(m, path))))),
  );

/* [rows] with [name] and the row after it swapped */
let swapped = (name, rows) => {
  let rec go =
    fun
    | [a, b, ...rest] when a == name => [b, a, ...rest]
    | [a, ...rest] => [a, ...go(rest)]
    | [] => [];
  go(rows);
};

/* Alt-Down on a member of one of Mega 4k's modules: the two members it
   swaps parse again, of 625, and the module around them */
let move_4k = (~root: Sort.t, ()) => {
  let m = OM.refresh(Cells.run(OM.mk(~root, mega_4k(~root)), Refresh));
  let order = rows(m, "HarvestLedger");
  let (o, parses) = (
    UC.owner(UC.editor(m)),
    UC.member_parses(UC.editor(m)),
  );
  let (m, w) = move_down(m, ["HarvestLedger", "cropValue"]);
  let again = UC.reparsed(~before=parses, UC.member_parses(UC.editor(m)));
  check(
    list(string),
    "cropValue moves down",
    swapped("cropValue", order),
    rows(OM.refresh(m), "HarvestLedger"),
  );
  check(bool, "the members were parsed", true, List.length(parses) >= 600);
  check(int, "members parsed again", 2, List.length(again));
  UC.at_most("items parsed", 4, w.parsed);
  UC.at_most("chunks measured", 4, w.measured);
  UC.at_most("item heads compared", 6, w.compared);
  check(int, "the view keeps its chunks", o, UC.owner(UC.editor(m)));
};

/* in a stack the op re-cuts the cells from an editor of the whole
   program, which takes the shell's caches: a few items of 66 parse */
let move_in_stack = () => {
  let (m, id) = UC.in_cell();
  let order = rows(m, "List");
  let (m, w) = move_down(m, ["List", "map"]);
  check(bool, "List.length's cell stays", true, UC.is_open(m, id));
  check(
    list(string),
    "map moves down",
    swapped("map", order),
    rows(OM.refresh(m), "List"),
  );
  UC.at_most("items parsed", 6, w.parsed);
};

/* an op reads the program first (Workspace.item_ctx): from the editor's
   parses, whatever program the statics last read */
let op_context = (~root: Sort.t, ()) => {
  let m = OM.refresh(Cells.run(OM.mk(~root, OM.src(~root)), Refresh));
  ignore(
    MakeTerm.semantic_term(~root, CorpusUtil.seg_of("let a = 1 in a")),
  );
  ignore(Workspace.fresh_term(Cells.program(m)));
  check(int, "items parsed", 0, MakeTerm.Incr.analyzed^);
};

let tests = (
  "EditCaches",
  [
    test_case(
      "Alt-Down at 4k parses the two members it swaps",
      `Quick,
      move_4k(~root=Exp),
    ),
    test_case("the same at a Mod root", `Quick, move_4k(~root=Mod)),
    test_case(
      "an op beside an open cell takes the shell's caches",
      `Quick,
      move_in_stack,
    ),
    test_case(
      "an op's reading of the program takes the editor's parses",
      `Quick,
      op_context(~root=Mod),
    ),
    test_case("the same at an Exp root", `Quick, op_context(~root=Exp)),
  ],
);
