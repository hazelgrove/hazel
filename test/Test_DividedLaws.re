open Alcotest;
open Haz3lcore;
open Language;

/* laws of a divided program (Web.Divided) */

module Divided = Web.Divided;
module Focus = Web.ScratchFocus;

let parse = (~root=Sort.Exp, src: string): Segment.t =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root,
      src,
    )
  ) {
  | Some(seg) => seg
  | None =>
    switch (MarkerParse.of_text(~root, src)) {
    | Some(z) => Zipper.unselect_and_zip(z)
    | None => failwith("parse failed: " ++ src)
    }
  };

let text_of = (seg: Segment.t): string =>
  seg |> Zipper.unzip |> MarkerParse.to_text;

let term_of = (~root=Sort.Exp, seg: Segment.t): Exp.t =>
  root == Sort.Mod
    ? MakeTerm.Incr.term_of_mod(seg) : MakeTerm.Incr.term_of(seg);

let info_map_of = (~root=Sort.Exp, seg: Segment.t) =>
  DefStatics.calc(~settings=CoreSettings.on, term_of(~root, seg)).merged;

let editor_of = (~root=Sort.Exp, seg: Segment.t): Web.CellEditor.Model.t =>
  Focus.cell_of_seg(~root, seg);

let seg_of = (e: Web.CellEditor.Model.t): Segment.t => Focus.zip_of_cell(e);

let rows = (~top_only=false, term: Exp.t): list(Id.t) => {
  let rec go = (acc, ns: list(Web.OutlineTree.node)) =>
    List.fold_left(
      (acc, n: Web.OutlineTree.node) => {
        let acc =
          switch (n.o_id) {
          | Some(id) => [id, ...acc]
          | None => acc
          };
        top_only ? acc : go(acc, n.o_children);
      },
      acc,
      ns,
    );
  List.rev(go([], Web.OutlineTree.of_term(term)));
};

let row = (term: Exp.t, label: string): Id.t => {
  let rec find = (ns: list(Web.OutlineTree.node)) =>
    List.fold_left(
      (acc, n: Web.OutlineTree.node) =>
        switch (acc) {
        | Some(_) => acc
        | None => n.o_label == label ? n.o_id : find(n.o_children)
        },
      None,
      ns,
    );
  switch (find(Web.OutlineTree.of_term(term))) {
  | Some(id) => id
  | None => failwith("no outline row: " ++ label)
  };
};

let rec tile_id = (label: string, seg: Segment.t): option(Id.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (acc, p) {
      | (Some(_), _) => acc
      | (None, Tile(t)) =>
        Tile.label(t) == [label]
          ? Some(t.id)
          : List.fold_left(
              (acc, kid) => acc == None ? tile_id(label, kid) : acc,
              None,
              t.children,
            )
      | (None, _) => None
      },
    None,
    seg,
  );
let tile = (label, seg) =>
  switch (tile_id(label, seg)) {
  | Some(id) => id
  | None => failwith("no tile: " ++ label)
  };

let probe = Refractors.mk_entry(ProjectorKind.Probe);
let manuals = (e: Web.CellEditor.Model.t): list(Id.t) =>
  List.map(fst, e.editor.editor.state.zipper.refractors.manuals);
let with_probe = (id: Id.t, e: Web.CellEditor.Model.t): Web.CellEditor.Model.t => {
  let z =
    ZipperBase.update_refractors(e.editor.editor.state.zipper, r =>
      Refractors.{
        ...r,
        manuals: [(id, probe), ...r.manuals],
      }
    );
  Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=e.editor.editor.root));
};

let split = (~root=Sort.Exp, seg: Segment.t, id: Id.t): Divided.t =>
  switch (
    Divided.split(
      ~info_map=info_map_of(~root, seg),
      editor_of(~root, seg),
      id,
    )
  ) {
  | Some(d) => d
  | None => fail("split refused")
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

/* split at every row, join: the same text, ids and root; returns how
   many rows opened */
let roundtrip_rows = (~root=Sort.Exp, ~top_only=false, seg: Segment.t): int => {
  let info_map = info_map_of(~root, seg);
  let editor = editor_of(~root, seg);
  let text = text_of(seg);
  let ids = Segment.ids(seg);
  List.fold_left(
    (opened, id) =>
      switch (Divided.split(~info_map, editor, id)) {
      | None => opened
      | Some(d) =>
        let joined = Divided.join(d);
        let seg' = seg_of(joined);
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
    rows(~top_only, term_of(~root, seg)),
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
  let opened = roundtrip_rows(parse(src));
  check(bool, "rows opened", true, opened >= 4);
};

let roundtrip_mod = () => {
  let opened = roundtrip_rows(~root=Mod, parse(~root=Mod, mod_src));
  check(bool, "rows opened", true, opened >= 3);
};

/* probes placed before the split and inside a cell both come back */
let probes = () => {
  let seg = parse(src);
  let one = tile("1", seg);
  let three = tile("3", seg);
  let editor = with_probe(one, editor_of(seg));
  let d =
    switch (
      Divided.split(
        ~info_map=info_map_of(seg),
        editor,
        row(term_of(seg), "f"),
      )
    ) {
    | Some(d) => d
    | None => fail("split refused")
    };
  let d = set_body(with_probe(three, body_cell(d).e_body), d);
  let joined = Divided.join(d);
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
  let seg = parse(src);
  let (one, three) = (tile("1", seg), tile("3", seg));
  let term = term_of(seg);
  let info_map = info_map_of(seg);
  let editor = with_probe(one, with_probe(three, editor_of(seg)));
  let d =
    switch (Divided.split(~info_map, editor, row(term, "f"))) {
    | Some(d) => d
    | None => fail("split refused")
    };
  check(
    bool,
    "shown in the cell",
    true,
    List.mem(three, manuals(body_cell(d).e_body)),
  );
  let without = (id, e: Web.CellEditor.Model.t) => {
    let z =
      ZipperBase.update_refractors(e.editor.editor.state.zipper, r =>
        Refractors.{
          ...r,
          manuals: List.filter(((i, _)) => i != id, r.manuals),
        }
      );
    Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=e.editor.editor.root));
  };
  let joined =
    Divided.join(set_body(without(three, body_cell(d).e_body), d));
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
  let d2 =
    switch (Divided.open_(~info_map, ~term, row(term, "a"), d)) {
    | Some(d2) => d2
    | None => fail("open refused")
    };
  switch (Divided.close(row(term, "f"), d2)) {
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

/* joining puts the caret where it was in the active cell */
let caret = () => {
  let seg = parse(src);
  let three = tile("3", seg);
  let d = split(seg, row(term_of(seg), "f"));
  let body = body_cell(d).e_body;
  let z =
    switch (
      Move.jump_to_side_of_id(
        Util.Direction.Left,
        body.editor.editor.state.zipper,
        three,
      )
    ) {
    | Some(z) => z
    | None => fail("no caret before 3 in the cell")
    };
  let d =
    set_body(Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=Exp)), d);
  let joined = Divided.join(d).editor.editor.state.zipper;
  switch (Siblings.neighbors(joined.relatives.siblings)) {
  | (_, Some(p)) =>
    check(bool, "caret sits before 3", true, Piece.id(p) == three)
  | (_, None) => fail("nothing right of the caret")
  };
};

/* the same with whitespace right of the caret */
let caret_beside_space = () => {
  let seg = parse(src);
  let three = tile("3", seg);
  let d = split(seg, row(term_of(seg), "f"));
  let body = body_cell(d).e_body;
  let z =
    switch (
      Move.jump_to_side_of_id(
        Util.Direction.Right,
        body.editor.editor.state.zipper,
        three,
      )
    ) {
    | Some(z) => z
    | None => fail("no caret after 3 in the cell")
    };
  let d =
    set_body(Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=Exp)), d);
  let joined = Divided.join(d).editor.editor.state.zipper;
  switch (Siblings.neighbors(joined.relatives.siblings)) {
  | (Some(p), _) =>
    check(bool, "caret sits after 3", true, Piece.id(p) == three)
  | (None, _) => fail("nothing left of the caret")
  };
};

/* a cell's edit is the document's; the rest keeps its ids */
let cell_edit = () => {
  let seg = parse(src);
  let a_ids = {
    let d = split(seg, row(term_of(seg), "a"));
    Web.Divided.cells(d)
    |> List.concat_map((c: Web.ScratchCell.t) =>
         Segment.ids(seg_of(c.e_body))
       );
  };
  let d = split(seg, row(term_of(seg), "g"));
  let d = set_body(editor_of(parse("x * 30")), d);
  let doc = Divided.document(d);
  check(
    string,
    "document has the edit",
    text_of(parse(src_edited)),
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

/* a join from an earlier join's caches matches a cold one after a cell
   edit (each join mints some fresh ids, so rows compare by label) */
let join_from_caches = () => {
  let seg = parse(src);
  let d = split(seg, row(term_of(seg), "g"));
  let before = Divided.join(d);
  let d = set_body(editor_of(parse("x * 30")), d);
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
  let seg = parse(src);
  let d = split(seg, row(term_of(seg), "a"));
  let joined = seg_of(Divided.join(d));
  let g = row(term_of(joined), "g");
  let edited = Focus.splice_def(g, parse("x * 5"), joined);
  switch (
    Divided.resplit(
      ~info_map=info_map_of(edited),
      ~term=term_of(edited),
      editor_of(edited),
      d,
    )
  ) {
  | Joined(_) => fail("the open cell closed")
  | Still(d') =>
    check(
      string,
      "edit kept",
      text_of(edited),
      text_of(Divided.document(d')),
    );
    check(int, "one cell", 1, List.length(Divided.cells(d')));
  };
};

/* agent edits act on the joined program; only the touched cell is re-cut */
let agent_edit = () => {
  let seg = parse(src);
  let term = term_of(seg);
  let d =
    switch (
      Divided.open_(
        ~info_map=info_map_of(seg),
        ~term,
        row(term, "f"),
        split(seg, row(term, "a")),
      )
    ) {
    | Some(d) => d
    | None => fail("second cell refused")
    };
  let before = Divided.cells(d);
  let z = Divided.join(d).editor.editor.state.zipper;
  let edited =
    switch (
      Perform.go(
        ~settings=CoreSettings.on,
        ~statics=CachedStatics.empty,
        ~syntax=CachedSyntax.init(z),
        ~root=Exp,
        Structural(Update(Definition, "a", "5")),
        {
          zipper: z,
          col_target: None,
        },
      )
    ) {
    | Ok(z) => Zipper.unselect_and_zip(z)
    | Error(e) => fail("agent edit failed: " ++ Action.Failure.show(e))
    };
  switch (
    Divided.resplit(
      ~info_map=info_map_of(edited),
      ~term=term_of(edited),
      editor_of(edited),
      d,
    )
  ) {
  | Joined(_) => fail("the open cells closed")
  | Still(d') =>
    let cell = (label, cells) =>
      List.find(
        (e: Web.ScratchCell.t) => e.e_id == row(term, label),
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
};

/* a rename keeps its tokens' ids: open cells holding the binder or a use
   still take the new name */
let rename = () => {
  let seg = parse(src);
  let term = term_of(seg);
  let d =
    switch (
      Divided.open_(
        ~info_map=info_map_of(seg),
        ~term,
        row(term, "f"),
        split(seg, row(term, "a")),
      )
    ) {
    | Some(d) => d
    | None => fail("second cell refused")
    };
  let joined = seg_of(Divided.join(d));
  let renamed =
    switch (
      Web.OutlineRename.rename(
        ~info_map=info_map_of(joined),
        ~term=term_of(joined),
        row(term_of(joined), "a"),
        "b",
        joined,
      )
    ) {
    | Ok(seg) => seg
    | Error(why) => fail("rename refused: " ++ why)
    };
  switch (
    Divided.resplit(
      ~info_map=info_map_of(renamed),
      ~term=term_of(renamed),
      editor_of(renamed),
      d,
    )
  ) {
  | Joined(_) => fail("the open cells closed")
  | Still(d') =>
    check(
      string,
      "the rename survives re-dividing",
      text_of(renamed),
      text_of(Divided.document(d')),
    )
  };
};

/* closing the last cell joins; a module-rooted program stays one */
let close_all = () => {
  let seg = parse(~root=Mod, mod_src);
  let d = split(~root=Mod, seg, row(term_of(~root=Mod, seg), "x"));
  switch (Divided.close(body_cell(d).e_id, d)) {
  | Still(_) => fail("still divided")
  | Joined(e) =>
    check(bool, "root kept", true, e.editor.editor.root == Sort.Mod);
    check(string, "text kept", text_of(seg), text_of(seg_of(e)));
  };
};

/* opening a parent folds its open children in; an id inside an open
   cell opens nothing */
let no_overlap = () => {
  let seg = parse(src);
  let term = term_of(seg);
  let info_map = info_map_of(seg);
  let d = split(seg, row(term, "g"));
  switch (Divided.open_(~info_map, ~term, row(term, "f"), d)) {
  | None => fail("parent refused")
  | Some(d) =>
    check(int, "one cell", 1, List.length(Divided.cells(d)));
    check(string, "text kept", text_of(seg), text_of(Divided.document(d)));
    switch (Divided.open_(~info_map, ~term, row(term, "g"), d)) {
    | None => ()
    | Some(_) => fail("a child of an open cell opened")
    };
  };
};

/* every row of mega-1k: its modules and their members */
let mega = () =>
  switch (CorpusUtil.corpus_seg("mega-1k.hz")) {
  | None => fail("corpus unreadable")
  | Some(seg) =>
    let opened = roundtrip_rows(seg);
    check(int, "rows opened", List.length(rows(term_of(seg))), opened);
  };

let tests = (
  "DividedLaws",
  [
    test_case("split then join", `Quick, roundtrip),
    test_case("split then join, module root", `Quick, roundtrip_mod),
    test_case("probes", `Quick, probes),
    test_case("probes follow their text", `Quick, probes_follow),
    test_case("caret", `Quick, caret),
    test_case("caret beside a space", `Quick, caret_beside_space),
    test_case("cell edit", `Quick, cell_edit),
    test_case("resplit keeps an outside edit", `Quick, resplit),
    test_case("an agent edit re-cuts only its cell", `Quick, agent_edit),
    test_case("a rename reaches open cells", `Quick, rename),
    test_case("a join from caches", `Quick, join_from_caches),
    test_case("close all", `Quick, close_all),
    test_case("no overlap", `Quick, no_overlap),
    test_case("mega-1k rows", `Slow, mega),
  ],
);
