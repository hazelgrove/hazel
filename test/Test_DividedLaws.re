open Alcotest;
open Haz3lcore;
open Language;
open CorpusUtil;

/* laws of a divided program (Web.Divided) */

module Divided = Web.Divided;
module Focus = Web.ScratchFocus;

let info_map_of = (~root=Sort.Exp, seg: Segment.t) =>
  DefStatics.calc(
    ~settings=CoreSettings.on,
    MakeTerm.Incr.term_of_root(~root, seg),
  ).
    merged;

let editor_of = (~root=Sort.Exp, seg: Segment.t): Web.CellEditor.Model.t =>
  Focus.cell_of_seg(~root, seg);

let rows = (outline: Web.OutlineTree.t): list(Id.t) =>
  Web.OutlineTree.(collect(rows(outline)));

let probe = Refractors.mk_entry(ProjectorKind.Probe);
let manuals = (e: Web.CellEditor.Model.t): list(Id.t) =>
  List.map(fst, e.editor.editor.state.zipper.refractors.manuals);
let with_manuals = (f, e: Web.CellEditor.Model.t): Web.CellEditor.Model.t => {
  let z =
    ZipperBase.update_refractors(e.editor.editor.state.zipper, r =>
      Refractors.{
        ...r,
        manuals: f(r.manuals),
      }
    );
  Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=e.editor.editor.root));
};
let with_probe = (id: Id.t) => with_manuals(ms => [(id, probe), ...ms]);

/* the row labeled [label] opened as a cell over [onto] (default: [seg]
   whole) */
let split =
    (~root=Sort.Exp, ~onto=?, seg: Segment.t, label: string): Divided.t => {
  let outline = CorpusUtil.outline_of(~root, seg);
  switch (
    Divided.open_(
      ~info_map=info_map_of(~root, seg),
      ~outline,
      row_by_label(outline, label),
      Option.value(onto, ~default=Divided.of_whole(editor_of(~root, seg))),
    )
  ) {
  | Some(d) => d
  | None => fail(label ++ " refused")
  };
};

/* [d] re-cut for the edited program, its cells still open */
let recut = (~root=Sort.Exp, edited: Segment.t, d: Divided.t): Divided.t =>
  switch (
    Divided.resplit(
      ~info_map=info_map_of(~root, edited),
      ~outline=CorpusUtil.outline_of(~root, edited),
      editor_of(~root, edited),
      d,
    )
  ) {
  | Still(d) => d
  | Joined(_) => fail("the open cells closed")
  };

let body_cell = (d: Divided.t): Web.ScratchCell.t =>
  switch (Divided.cells(d)) {
  | [c, ..._] => c
  | [] => fail("no cells")
  };
let set_body = (e: Web.CellEditor.Model.t, d: Divided.t): Divided.t => {
  let id = body_cell(d).e_id;
  Divided.update_cell(
    id,
    (c: Web.ScratchCell.t) =>
      {
        ...c,
        e_body: e,
      },
    d,
  )
  |> Divided.set_active(id, Divided.Body);
};

/* split at every row (every [sample]th), join: the same text, ids and
   root; returns how many rows opened */
let roundtrip_rows = (~root=Sort.Exp, ~sample=1, seg: Segment.t): int => {
  let info_map = info_map_of(~root, seg);
  let outline = CorpusUtil.outline_of(~root, seg);
  let whole = Divided.of_whole(editor_of(~root, seg));
  let text = text_of(seg);
  let ids = Segment.ids(seg);
  List.fold_left(
    (opened, id) =>
      switch (Divided.open_(~info_map, ~outline, id, whole)) {
      | None => opened
      | Some(d) =>
        let joined = Divided.join(d);
        let seg' = Focus.zip_of_cell(joined);
        check(string, "text after join", text, text_of(seg'));
        check(bool, "ids after join", true, Segment.ids(seg') == ids);
        check(
          bool,
          "root after join",
          true,
          joined.editor.editor.root == root,
        );
        opened + 1;
      },
    0,
    List.filteri((i, _) => i mod sample == 0, rows(outline)),
  );
};

let src = "let a = 1 + 2 in
let f = fun x ->
  let g = x * 3 in
  g + a
in
type T = Int in
f(4)";

let src_edited = "let a = 1 + 2 in
let f = fun x ->
  let g = x * 30 in
  g + a
in
type T = Int in
f(4)";

let mod_src = "let x = 1;
type T = Int;
module M = {
  let y = x + 16
};
x";

let roundtrip = () => {
  let opened = roundtrip_rows(seg_of(src));
  check(bool, "rows opened", true, opened >= 4);
};

let roundtrip_mod = () => {
  let opened = roundtrip_rows(~root=Mod, seg_of(~root=Mod, mod_src));
  check(bool, "rows opened", true, opened >= 3);
};

/* probes placed before the split and inside a cell both come back */
let probes = () => {
  let seg = seg_of(src);
  let (one, three) = (tile("1", seg), tile("3", seg));
  let d =
    split(
      ~onto=Divided.of_whole(with_probe(one, editor_of(seg))),
      seg,
      "f",
    );
  let joined =
    Divided.join(set_body(with_probe(three, body_cell(d).e_body), d));
  check(
    bool,
    "probe from before the split",
    true,
    List.mem(one, manuals(joined)),
  );
  check(
    bool,
    "probe placed in the cell",
    true,
    List.mem(three, manuals(joined)),
  );
};

/* a probe follows its text: into the cell that opens over it, back out
   when that cell closes; removed in the cell, it stays removed */
let probes_follow = () => {
  let seg = seg_of(src);
  let (one, three) = (tile("1", seg), tile("3", seg));
  let editor = with_probe(one, with_probe(three, editor_of(seg)));
  let d = split(~onto=Divided.of_whole(editor), seg, "f");
  check(
    bool,
    "shown in the cell",
    true,
    List.mem(three, manuals(body_cell(d).e_body)),
  );
  let without =
    with_manuals(List.filter(((i, _)) => i != three), body_cell(d).e_body);
  let joined = Divided.join(set_body(without, d));
  check(
    bool,
    "removed stays removed",
    false,
    List.mem(three, manuals(joined)),
  );
  check(
    bool,
    "the one outside stays",
    true,
    List.mem(one, manuals(joined)),
  );
  let f = row_by_label(CorpusUtil.outline_of(seg), "f");
  switch (Divided.close(f, split(~onto=d, seg, "a"))) {
  | Still(d3) =>
    let joined = Divided.join(d3);
    check(
      bool,
      "kept past its cell's close",
      true,
      List.mem(three, manuals(joined)),
    );
    check(
      bool,
      "and the other cell's",
      true,
      List.mem(one, manuals(joined)),
    );
  | Joined(_) => fail("a cell should still be open")
  };
};

/* joining puts the caret where it was in the active cell: [side] of 3,
   so right of it, whitespace */
let caret = (side: Util.Direction.t, ()) => {
  let seg = seg_of(src);
  let three = tile("3", seg);
  let d = split(seg, "f");
  let z =
    switch (
      Move.jump_to_side_of_id(
        side,
        body_cell(d).e_body.editor.editor.state.zipper,
        three,
      )
    ) {
    | Some(z) => z
    | None => fail("no caret beside 3 in the cell")
    };
  let d =
    set_body(Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=Exp)), d);
  let joined = Divided.join(d).editor.editor.state.zipper;
  let (left, right) = Siblings.neighbors(joined.relatives.siblings);
  check(
    bool,
    "the caret beside 3",
    true,
    Option.map(Piece.id, side == Util.Direction.Left ? right : left)
    == Some(three),
  );
};

/* a cell's edit is the document's; the rest keeps its ids */
let cell_edit = () => {
  let seg = seg_of(src);
  let a_ids =
    Divided.cells(split(seg, "a"))
    |> List.concat_map((c: Web.ScratchCell.t) =>
         Segment.ids(Focus.zip_of_cell(c.e_body))
       );
  let d = set_body(editor_of(seg_of("x * 30")), split(seg, "g"));
  let doc = Divided.document(d);
  check(
    string,
    "document has the edit",
    text_of(seg_of(src_edited)),
    text_of(doc),
  );
  let ids = Segment.ids(doc);
  check(
    bool,
    "other items keep their ids",
    true,
    List.for_all(id => List.mem(id, ids), a_ids),
  );
};

/* the problems panel's "elsewhere" editor carries the document's terms,
   not the split's: it keeps only problems whose ids it holds */
let outside_terms = () => {
  let seg = seg_of(src);
  let d = set_body(editor_of(seg_of("x * 30")), split(seg, "g"));
  let keys = m => Id.Map.bindings(m) |> List.map(fst);
  let cold =
    MakeTerm.Incr.go_incr(
      ~cache=MakeTerm.Incr.mk_cache(),
      Divided.document(d),
    ).
      term_data;
  check(
    bool,
    "outside term ids are the document's",
    true,
    keys(Divided.outside_editor(d).editor.syntax.term_data) == keys(cold),
  );
};

/* a join from an earlier join's caches matches a cold one after a cell
   edit (each join mints some fresh ids, so rows compare by label) */
let join_from_caches = () => {
  let seg = seg_of(src);
  let d = split(seg, "g");
  let before = Divided.join(d);
  let d = set_body(editor_of(seg_of("x * 30")), d);
  let cold = Divided.join(d).editor.editor.syntax;
  let warm = Divided.join(~prev=before, d).editor.editor.syntax;
  let labels = (m: Measured.t) =>
    List.map(
      List.map((p: Piece.t) =>
        switch (p) {
        | Tile(t) => String.concat("", Tile.label(t))
        | Grout(_) => "_"
        | Secondary(_) => " "
        | Projector(_) => "P"
        }
      ),
      Measured.piece_rows(m),
    );
  check(string, "same text", text_of(cold.segment), text_of(warm.segment));
  check(
    bool,
    "same rows",
    true,
    labels(cold.measured) == labels(warm.measured),
  );
  check(
    int,
    "same term data",
    Id.Map.cardinal(cold.term_data),
    Id.Map.cardinal(warm.term_data),
  );
};

/* an edit to the joined program survives re-dividing; the cell stays open */
let resplit = () => {
  let seg = seg_of(src);
  let d = split(seg, "a");
  let joined = Focus.zip_of_cell(Divided.join(d));
  let g = row_by_label(CorpusUtil.outline_of(joined), "g");
  let edited = Focus.splice_def(~root=Exp, g, seg_of("x * 5"), joined);
  let d' = recut(edited, d);
  check(
    string,
    "edit kept",
    text_of(edited),
    text_of(Divided.document(d')),
  );
  check(int, "one cell", 1, List.length(Divided.cells(d')));
};

/* agent edits act on the joined program; only the touched cell is re-cut */
let agent_edit = () => {
  let seg = seg_of(src);
  let outline = CorpusUtil.outline_of(seg);
  let d = split(~onto=split(seg, "a"), seg, "f");
  let before = Divided.cells(d);
  let z = Divided.join(d).editor.editor.state.zipper;
  let edited =
    switch (
      Perform.go(
        ~settings=CoreSettings.on,
        ~statics=CachedStatics.empty,
        ~syntax=CachedSyntax.init(z),
        ~root=Exp,
        Structural(Replace("a:definition", "5")),
        {
          zipper: z,
          col_target: None,
        },
      )
    ) {
    | Ok(z) => Zipper.unselect_and_zip(z)
    | Error(e) => fail("agent edit failed: " ++ Action.Failure.show(e))
    };
  let d' = recut(edited, d);
  let cell = (label, cells) =>
    List.find(
      (e: Web.ScratchCell.t) => e.e_id == row_by_label(outline, label),
      cells,
    );
  check(
    bool,
    "the untouched cell keeps its editor",
    true,
    cell("f", Divided.cells(d')) === cell("f", before),
  );
  check(
    bool,
    "the edited cell is re-cut",
    false,
    cell("a", Divided.cells(d')) === cell("a", before),
  );
  check(
    string,
    "edit kept",
    text_of(edited),
    text_of(Divided.document(d')),
  );
};

/* a rename keeps its tokens' ids: open cells holding the binder or a use
   still take the new name */
let rename = () => {
  let seg = seg_of(src);
  let d = split(~onto=split(seg, "a"), seg, "f");
  let joined = Focus.zip_of_cell(Divided.join(d));
  let outline = CorpusUtil.outline_of(joined);
  let renamed =
    switch (
      CorpusUtil.rename_row(
        ~info_map=info_map_of(joined),
        ~outline,
        row_by_label(outline, "a"),
        "b",
        joined,
      )
    ) {
    | Ok(seg) => seg
    | Error(why) => fail("rename refused: " ++ why)
    };
  check(
    string,
    "the rename survives re-dividing",
    text_of(renamed),
    text_of(Divided.document(recut(renamed, d))),
  );
};

/* closing the last cell joins; a module-rooted program stays one */
let close_all = () => {
  let seg = seg_of(~root=Mod, mod_src);
  let d = split(~root=Mod, seg, "x");
  switch (Divided.close(body_cell(d).e_id, d)) {
  | Still(_) => fail("still divided")
  | Joined(e) =>
    check(bool, "root kept", true, e.editor.editor.root == Sort.Mod);
    check(
      string,
      "text kept",
      text_of(seg),
      text_of(Focus.zip_of_cell(e)),
    );
  };
};

/* opening a parent folds its open children in; an id inside an open
   cell opens nothing */
let no_overlap = () => {
  let seg = seg_of(src);
  let d = split(~onto=split(seg, "g"), seg, "f");
  check(int, "one cell", 1, List.length(Divided.cells(d)));
  check(string, "text kept", text_of(seg), text_of(Divided.document(d)));
  let outline = CorpusUtil.outline_of(seg);
  check(
    bool,
    "a child of an open cell opens nothing",
    true,
    Option.is_none(
      Divided.open_(
        ~info_map=info_map_of(seg),
        ~outline,
        row_by_label(outline, "g"),
        d,
      ),
    ),
  );
};

/* closing the last cell splices its text in once: an edge space typed
   in the cell isn't doubled, and no piece appears twice */
let close_once = () => {
  let seg = seg_of(src);
  let d = split(seg, "g");
  let space =
    Piece.Secondary({
      id: Id.mk(),
      content: Whitespace(" "),
    });
  let d =
    set_body(
      editor_of(Focus.zip_of_cell(body_cell(d).e_body) @ [space]),
      d,
    );
  let expected = text_of(Divided.document(d));
  switch (Divided.close(body_cell(d).e_id, d)) {
  | Still(_) => fail("still divided")
  | Joined(e) =>
    let seg = Focus.zip_of_cell(e);
    check(string, "spliced once", expected, text_of(seg));
    let ids = Segment.ids(seg);
    check(
      int,
      "no piece twice",
      List.length(ids),
      List.length(List.sort_uniq(compare, ids)),
    );
  };
};

/* an outside edit carries an open member into an open module: the module's
   cell holds it now, so the member's cell closes, and the caret's cell with
   it */
let resplit_folds = () => {
  let src = "let y = 1;\nmodule B = {\n  let z = 2\n};\nB.z";
  let seg = seg_of(~root=Mod, src);
  let y = row_by_label(CorpusUtil.outline_of(~root=Mod, seg), "y");
  let d =
    split(~root=Mod, ~onto=split(~root=Mod, seg, "y"), seg, "B")
    |> Divided.set_active(y, Divided.Body);
  let joined = Focus.zip_of_cell(Divided.join(d));
  let edited =
    switch (
      CorpusUtil.outline_op(
        ~root=Mod,
        ~is_open=_ => true,
        MoveDown,
        row_by_label(CorpusUtil.outline_of(~root=Mod, joined), "y"),
        joined,
      )
    ) {
    | Ok((seg, _)) => seg
    | Error(why) => fail("move refused: " ++ why)
    };
  let d' = recut(~root=Mod, edited, d);
  let cells = Divided.cells(d');
  let overlaps =
    List.exists(
      (e: Web.ScratchCell.t) =>
        List.exists(
          (o: Web.ScratchCell.t) =>
            o !== e
            && List.exists(
                 id =>
                   List.mem(
                     id,
                     Segment.ids(Focus.zip_of_cell(o.e_header))
                     @ Segment.ids(Focus.zip_of_cell(o.e_body)),
                   ),
                 Web.ScratchCell.covers(e),
               ),
          cells,
        ),
      cells,
    );
  check(bool, "no cell inside another", false, overlaps);
  check(int, "the module's cell alone", 1, List.length(cells));
  check(
    string,
    "edit kept",
    text_of(edited),
    text_of(Divided.document(d')),
  );
  check(
    bool,
    "the closed cell isn't active",
    false,
    Divided.active(d') == Some((y, Divided.Body)),
  );
};

/* cells show their item from its own left edge: the indentation every
   line after the first has in the program is cut on the way in and put
   back, as the program's own pieces, on the way out */
let nested_src = "module M = {\n  let f = fun x ->\n    let y = x + 1 in\n    y * 2;\n  let g = 2\n} in\nM.g";

let relative_cells = () => {
  let d = split(seg_of(nested_src), "f");
  check(
    string,
    "a member's body",
    "fun x ->\n  let y = x + 1 in\n  y * 2",
    text_of(Focus.zip_of_cell(body_cell(d).e_body)),
  );
  let src = "module Outer = {\n  module Inner = {\n    let v = 1;\n    let w =\n      v + 1\n  };\n  let u = Inner.w\n};\nlet top = Outer.u";
  let d = split(~root=Mod, seg_of(~root=Mod, src), "Inner");
  check(
    string,
    "a nested module, its closing brace included",
    "{\n  let v = 1;\n  let w =\n    v + 1\n}",
    text_of(Focus.zip_of_cell(body_cell(d).e_body)),
  );
};

let indent_round_trip = () => {
  let seg = seg_of(nested_src);
  let d = split(seg, "f");
  check(
    bool,
    "an untouched cell splices back the program's own pieces",
    true,
    Segment.ptr_eq(Divided.document(d), seg),
  );
  let d = set_body(editor_of(seg_of("fun x ->\n  x * 3")), d);
  let doc = Divided.document(d);
  check(
    string,
    "a line typed in the cell lands at the program's depth",
    "module M = {\n  let f = fun x ->\n    x * 3;\n  let g = 2\n} in\nM.g",
    text_of(doc),
  );
  check(
    bool,
    "and its indentation is the same from one assembly to the next",
    true,
    Segment.ids(doc) == Segment.ids(Divided.document(d)),
  );
};

/* every row, every kind of cell, untouched: the program's own pieces */
let kinds_src = "module M = {\n  module N = {\n    let a = 1;\n    test a == 1 end;\n    test a > 0 end\n  };\n  let f = fun x ->\n    let y = x + 1 in\n    y * 2;\n  let g = 2\n} in\nM.g";

let own_pieces = () => {
  let seg = seg_of(kinds_src);
  let info_map = info_map_of(seg);
  let kinds = [
    ("def", id => Focus.mk_entry(~root=Exp, ~info_map, id, seg)),
    ("members", id => Focus.mk_members_entry(~root=Exp, ~info_map, id, seg)),
    ("run", id => Focus.mk_run_entry(~root=Exp, ~info_map, id, seg)),
  ];
  let opened =
    List.fold_left(
      (opened, id) =>
        List.fold_left(
          (opened, (kind, mk)) =>
            switch (mk(id)) {
            | None => opened
            | Some(e) =>
              check(
                bool,
                kind ++ " " ++ Id.show(id),
                true,
                Segment.ptr_eq(Focus.splice_entry(~root=Exp, e, seg), seg),
              );
              opened + 1;
            },
          opened,
          kinds,
        ),
      0,
      rows(CorpusUtil.outline_of(seg)),
    );
  check(bool, "cells opened", true, opened >= 8);
};

/* every fourth row of mega-1k: its modules and their members */
let mega = () => {
  let seg = mega_seg("mega-1k.hz");
  check(
    int,
    "rows opened",
    (List.length(rows(CorpusUtil.outline_of(seg))) + 3) / 4,
    roundtrip_rows(~sample=4, seg),
  );
};

/* a ⇒ cell whose expression gets a new root follows it, and the program
   keeps taking its edits: before, the program's text still held the old
   root, every later splice missed, and the cell's edits were dropped */
let headless_follows = () => {
  /* "": the ⇒ row */
  let d = split(seg_of("let a = 1 in\n0"), "");
  let d = set_body(editor_of(seg_of("a + 2")), d);
  let doc = Divided.document(d);
  check(
    bool,
    "an edit reaches the program",
    true,
    contains("a + 2", text_of(doc)),
  );
  let outline = CorpusUtil.outline_of(doc);
  let (d, moves) =
    Divided.follow_headless(
      ~rows=Web.OutlineTree.row_ids(outline),
      ~headless=Web.OutlineTree.headless_row_ids(outline),
      d,
    );
  check(bool, "the cell follows its new root", true, moves != []);
  let d = set_body(editor_of(seg_of("a + 3")), d);
  check(
    bool,
    "a later edit still reaches the program",
    true,
    contains("a + 3", text_of(Divided.document(d))),
  );
};

let tests = (
  "DividedLaws",
  [
    test_case("split then join", `Quick, roundtrip),
    test_case("split then join, module root", `Quick, roundtrip_mod),
    test_case("probes", `Quick, probes),
    test_case("probes follow their text", `Quick, probes_follow),
    test_case("caret", `Quick, caret(Left)),
    test_case("caret beside a space", `Quick, caret(Right)),
    test_case("cell edit", `Quick, cell_edit),
    test_case("resplit keeps an outside edit", `Quick, resplit),
    test_case("an agent edit re-cuts only its cell", `Quick, agent_edit),
    test_case("a rename reaches open cells", `Quick, rename),
    test_case("a join from caches", `Quick, join_from_caches),
    test_case(
      "the outside editor has the document's terms",
      `Quick,
      outside_terms,
    ),
    test_case("close all", `Quick, close_all),
    test_case("no overlap", `Quick, no_overlap),
    test_case("closing the last cell splices once", `Quick, close_once),
    test_case("an edit into an open module folds", `Quick, resplit_folds),
    test_case("cells are relative to their item", `Quick, relative_cells),
    test_case("indentation round-trips", `Quick, indent_round_trip),
    test_case(
      "untouched cells keep the program's pieces",
      `Quick,
      own_pieces,
    ),
    test_case("a ⇒ cell follows its expression", `Quick, headless_follows),
    test_case("mega-1k rows", `Slow, mega),
  ],
);
