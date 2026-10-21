open Alcotest;
open Web;
open Haz3lcore;

/* An outline op builds a new editor for its program, and a paste
   rebuilds every tile when it re-indents its lines. The editor takes the
   live one's caches, and the pieces the edit didn't change stay the live
   program's objects, so the parse, the measure and the analysis redo only
   what it changed, and so does the undo of a paste (UndoCaches) */

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
   program, which takes the shell's caches: a few items of 17 parse */
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
  let m =
    OM.refresh(Cells.run(OM.mk(~root, OM.short_src(~root)), Refresh));
  ignore(
    MakeTerm.semantic_term(~root, CorpusUtil.seg_of("let a = 1 in a")),
  );
  ignore(Workspace.fresh_term(Cells.program(m)));
  check(int, "items parsed", 0, MakeTerm.Incr.analyzed^);
};

let block =
  List.init(20, i =>
    i == 0
      ? "let pa01 = fun (x: Int) -> x + 1;"
      : Printf.sprintf(
          "let pa%02d = fun (x: Int) -> pa%02d(x) * 2 + %d;",
          i + 1,
          i,
          i,
        )
  )
  |> String.concat("\n");

/* List before and after 20 definitions pasted on a line of their own
   after List.length's `end;`: its member parses before (the cache is the
   editor's, updated in place), and what the paste redid */
let pasted = (~root: Sort.t) => {
  let src = OM.short_src(~root);
  let m = OM.refresh(Cells.run(OM.mk(~root, src), Refresh));
  let m =
    OM.refresh(
      Cells.run(
        m,
        Globals(
          ActiveEditor(Move(Point(OM.after(src, "    end;"), None))),
        ),
      ),
    );
  let m = OM.refresh(Cells.run(m, Test_Undo.insert("\n")));
  let parses = UC.member_parses(UC.editor(m));
  let (m', w) = UC.step(m, Globals(ActiveEditor(Paste(block))));
  (m, parses, m', w);
};

/* the definitions parse, and List.map after them (its leading lines
   changed), of 36 members */
let paste = (~root: Sort.t, ()) => {
  let (m, parses, m', w) = pasted(~root);
  check(
    bool,
    "the definitions land at the members' indentation",
    true,
    CorpusUtil.contains(
      "  let pa01 = fun (x: Int) -> x + 1;\n  let pa02",
      Test_Undo.text_of(m'),
    ),
  );
  check(bool, "the members were parsed", true, List.length(parses) >= 16);
  UC.at_most(
    "members parsed again",
    21,
    List.length(
      UC.reparsed(~before=parses, UC.member_parses(UC.editor(m'))),
    ),
  );
  UC.at_most("items parsed", 22, w.parsed);
  UC.at_most("chunks measured", 22, w.measured);
  check(
    int,
    "the view keeps its chunks",
    UC.owner(UC.editor(m)),
    UC.owner(UC.editor(m')),
  );
};

/* its undo parses List.map again, and the module around it */
let paste_undo = (~root: Sort.t, ()) => {
  let (m, _, m', _) = pasted(~root);
  let m' = OM.refresh(m');
  let parses = UC.member_parses(UC.editor(m'));
  let (u, w) = UC.step(m', Test_Undo.undo);
  check(
    string,
    "undo restores the program",
    Test_Undo.text_of(m),
    Test_Undo.text_of(u),
  );
  UC.at_most(
    "members parsed again",
    1,
    List.length(
      UC.reparsed(~before=parses, UC.member_parses(UC.editor(u))),
    ),
  );
  UC.at_most("items parsed", 2 * w.frames, w.parsed);
  UC.at_most("chunks measured", 2 * w.frames, w.measured);
  UC.at_most("item heads compared", 8, w.compared);
  check(
    int,
    "the view keeps its chunks",
    UC.owner(UC.editor(m')),
    UC.owner(UC.editor(u)),
  );
};

/* the restore only gives back objects: an editor's program with every tile
   rebuilt (as re-indenting a paste's lines rebuilds them) comes back as
   its own pieces, the same program */
let restore_content = () => {
  let src = OM.src(~root=Mod);
  let m = Cells.run(OM.mk(~root=Mod, src), Refresh);
  let m =
    Cells.run(
      m,
      Globals(ActiveEditor(Move(Point(OM.after(src, "    end;"), None)))),
    );
  let z = UC.editor(m).editor.editor.state.zipper;
  let rebuilt = ZipperBase.MapSegment.go(seg => seg, z);
  check(
    bool,
    "rebuilt",
    false,
    Segment.ptr_eq(
      fst(rebuilt.relatives.siblings),
      fst(z.relatives.siblings),
    ),
  );
  let restored = Zipper.restore_identity(~old=z, rebuilt);
  check(bool, "the same zipper", true, compare(restored, z) == 0);
  check(
    bool,
    "its own pieces",
    true,
    Segment.ptr_eq(
      fst(restored.relatives.siblings),
      fst(z.relatives.siblings),
    )
    && Segment.ptr_eq(
         snd(restored.relatives.siblings),
         snd(z.relatives.siblings),
       ),
  );
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
    test_case("a paste parses what it changed", `Quick, paste(~root=Mod)),
    test_case("the same at an Exp root", `Quick, paste(~root=Exp)),
    test_case(
      "the undo of a paste parses what it changed",
      `Quick,
      paste_undo(~root=Mod),
    ),
    test_case("the same at an Exp root", `Quick, paste_undo(~root=Exp)),
    test_case(
      "restoring identity keeps the program",
      `Quick,
      restore_content,
    ),
  ],
);
