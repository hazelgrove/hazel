open Alcotest;
open Web;

/* `let q = (1 + 2` typed after List.length's `end;` in the List module at
   a Mod root. Read raw, its open `(` takes every member below it into one
   item: the outline's ops on it took the rest of the module along */

module Cells = Test_Undo.Cells;

let src = () => Option.get(Test_ModAudit.stdlib("List.hz"));

let mk = (src: string): History.Model.t => {
  let editor =
    CorpusUtil.seg_of(~root=Mod, src)
    |> Haz3lcore.Zipper.unzip(~direction=Left)
    |> Haz3lcore.Editor.Model.mk(~root=Mod)
    |> CellEditor.Model.mk;
  let editors: Editors.Model.t =
    Scratch(
      ScratchModel.Model.{
        current: 0,
        scratchpads: [
          ScratchModel.Scratchpad.mk_code(~name="List", ~editor, ()),
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

/* the debounced statics, now */
let refresh = m => Cells.run(m, Editors(Scratch(RefreshStatics)));

let type_ = (m, s: string) =>
  List.fold_left(
    (m, i) => Cells.run(m, Test_Undo.insert(String.sub(s, i, 1))),
    m,
    List.init(String.length(s), i => i),
  );

/* the end of [needle]'s first line in [text] */
let after = (text: string, needle: string): Util.Point.t =>
  Option.get(
    Test_ModAudit.point_of(~off=String.length(needle), text, needle),
  );

/* the caret after List.length's `end;` */
let loaded = () => {
  let src = src();
  let m = Cells.run(mk(src), Refresh);
  Cells.run(
    m,
    Globals(ActiveEditor(Move(Point(after(src, "    end;"), None)))),
  );
};

let typed = "\nlet q = (1 + 2";
/* models are values: the tests share one */
let with_q = {
  let m = lazy(refresh(type_(loaded(), typed)));
  () => Lazy.force(m);
};

let term = m => Program.statics(Cells.program(m)).term;
let row = (m, path) => CorpusUtil.row(term(m), path);
let doc = m =>
  Haz3lcore.Printer.of_segment(
    ~holes="?",
    ~refractors=[],
    Program.document(Cells.program(m)),
  );

/* an op takes an item whole: the open form's would take the members below
   it, so nothing moves it, or anything it runs into */
let ops_refuse = () => {
  let m = with_q();
  let before = doc(m);
  let op = (o, name) =>
    doc(
      Cells.run(
        m,
        Editors(Scratch(Outline(DefOp(o, row(m, ["List", name]))))),
      ),
    );
  List.iter(
    ((what, o, name)) =>
      check(string, what ++ " refuses", before, op(o, name)),
    [
      ("deleting q", OutlineEdit.Delete, "q"),
      ("duplicating q", Duplicate, "q"),
      ("moving q up", MoveUp, "q"),
      ("deleting a member below it", Delete, "filter"),
      ("moving one below it up", MoveUp, "map"),
      ("moving one below it down", MoveDown, "map"),
      ("moving the one above it down", MoveDown, "length"),
    ],
  );
};

let tests = (
  "OpenMember",
  [test_case("outline ops refuse the open form's item", `Quick, ops_refuse)],
);
