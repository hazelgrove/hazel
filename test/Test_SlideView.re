open Alcotest;
open Haz3lcore;
open Language;

/* slide views (Web.SlideView): pins, zoom and parking change which cells
   are open, never the program */

module V = Web.SlideView;
module Divided = Web.Divided;
module Focus = Web.ScratchFocus;

let src = "let a = 1 in
module M = {
  let x = a + 1;
  let y = x * 2
} in
let b = M.y in
b";

let parse = (src: string): Segment.t =>
  switch (FastParse.of_text(~root=Exp, src)) {
  | Some(seg) => seg
  | None => failwith("parse failed")
  };

let text_of = (seg: Segment.t): string =>
  seg |> Zipper.unzip |> MarkerParse.to_text;

let seg = parse(src);
let term = MakeTerm.Incr.term_of(seg);
let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;

let row = (label: string): Id.t => {
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
  | None => failwith("no row " ++ label)
  };
};

let whole = (): Web.Program.t => Whole(Focus.cell_of_seg(seg));

let pinned = (id: Id.t, v: V.t): bool =>
  List.exists((p: V.pin) => p.p_id == id, v.pins);

/* apply view changes in order, realizing after each; the program text
   never changes */
let run = (steps: list(V.t => V.t)): (V.t, Web.Program.t) =>
  List.fold_left(
    ((v, p), f) => {
      let (v, p) = V.realize(~info_map, ~term, f(v), p);
      check(
        string,
        "program unchanged",
        src,
        text_of(Web.Program.document(p)),
      );
      (v, p);
    },
    (V.init, whole()),
    steps,
  );

let open_ids = (p: Web.Program.t): list(Id.t) =>
  switch (p) {
  | Whole(_) => []
  | Divided(d) =>
    List.map((c: Web.ScratchCell.t) => c.e_id, Divided.cells(d))
  };

let pin_unpin = () => {
  let (_, p) = run([V.pin(~term, row("a"))]);
  check(bool, "a open", true, open_ids(p) == [row("a")]);
  let (v, p) = run([V.pin(~term, row("a")), V.unpin(row("a"))]);
  check(bool, "whole again", true, open_ids(p) == []);
  check(int, "no pins", 0, List.length(v.pins));
};

let zoom = () => {
  let m = row("M");
  let (v, p) = run([V.zoom_in(~term, m)]);
  check(bool, "the module as one cell", true, open_ids(p) == [m]);
  check(bool, "shown as the zoom cell", true, V.showing_zoom_cell(~term, v));
  let (_, p) = run([V.zoom_in(~term, m), V.zoom_out]);
  check(bool, "whole after zoom out", true, open_ids(p) == []);
};

let contains = (needle, hay) => {
  let (nl, hl) = (String.length(needle), String.length(hay));
  let rec go = i =>
    i + nl <= hl && (String.sub(hay, i, nl) == needle || go(i + 1));
  go(0);
};

/* zoomed, the module shows as its members alone; edits land inside
   its braces */
let zoom_members = () => {
  let m = row("M");
  let (_, p) = run([V.zoom_in(~term, m)]);
  switch (p) {
  | Whole(_) => fail("not divided")
  | Divided(d) =>
    let body = List.hd(Divided.cells(d)).e_body;
    let txt = text_of(Focus.zip_of_cell(body));
    check(bool, "no braces", false, String.contains(txt, '{'));
    check(bool, "the members", true, contains("let x", txt));
    let members =
      switch (FastParse.of_text(~root=Mod, "let x = a + 5")) {
      | Some(seg) => seg
      | None => fail("mod parse")
      };
    let d =
      Divided.update_cell(
        List.hd(Divided.cells(d)).e_id,
        (c: Web.ScratchCell.t) =>
          {
            ...c,
            e_body: Focus.cell_of_seg(~root=Sort.Mod, members),
          },
        d,
      );
    let doc = text_of(Divided.document(d));
    check(
      bool,
      "edit inside the braces",
      true,
      contains("module M = {", doc) && contains("a + 5", doc),
    );
  };
};

let pins_outside_zoom = () => {
  let (a, m) = (row("a"), row("M"));
  let (v, p) = run([V.pin(~term, a), V.zoom_in(~term, m)]);
  check(bool, "a hidden inside M", true, open_ids(p) == [m]);
  check(bool, "a still pinned", true, pinned(a, v));
  let (_, p) = run([V.pin(~term, a), V.zoom_in(~term, m), V.zoom_out]);
  check(bool, "a back on zoom out", true, open_ids(p) == [a]);
};

let pins_inside_zoom = () => {
  let (m, x) = (row("M"), row("x"));
  let (_, p) = run([V.zoom_in(~term, m), V.pin(~term, x)]);
  check(bool, "member replaces the module cell", true, open_ids(p) == [x]);
  let (_, p) = run([V.zoom_in(~term, m), V.pin(~term, x), V.unpin(x)]);
  check(bool, "module cell again", true, open_ids(p) == [m]);
};

let park = () => {
  let a = row("a");
  let (v, p) = run([V.pin(~term, a), V.park(true)]);
  check(bool, "whole while parked", true, open_ids(p) == []);
  check(bool, "pin kept", true, pinned(a, v));
  let (_, p) = run([V.pin(~term, a), V.park(true), V.park(false)]);
  check(bool, "cells again", true, open_ids(p) == [a]);
};

/* going from one editor to cells, the caret lands in its cell */
let caret_travels = () => {
  let x = row("x");
  let two =
    switch (
      List.find_opt(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) => Tile.label(t) == ["2"]
          | _ => false
          },
        {
          let rec all = (s: Segment.t) =>
            List.concat_map(
              (p: Piece.t) =>
                switch (p) {
                | Tile(t) => [p, ...List.concat_map(all, t.children)]
                | _ => [p]
                },
              s,
            );
          all(seg);
        },
      )
    ) {
    | Some(p) => Piece.id(p)
    | None => failwith("no 2")
    };
  let z =
    switch (
      Move.jump_to_side_of_id(Util.Direction.Left, Zipper.unzip(seg), two)
    ) {
    | Some(z) => z
    | None => failwith("no caret before 2")
    };
  let p: Web.Program.t =
    Whole(Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=Exp)));
  let y = row("y");
  let (_, p) =
    V.realize(
      ~info_map,
      ~term,
      V.pin(~term, y, V.pin(~term, x, V.init)),
      p,
    );
  switch (p) {
  | Whole(_) => fail("still whole")
  | Divided(d) =>
    let joined = Divided.join(d).editor.editor.state.zipper;
    switch (Siblings.neighbors(joined.relatives.siblings)) {
    | (_, Some(p)) =>
      check(bool, "caret before 2", true, Piece.id(p) == two)
    | _ => fail("nothing right of the caret")
    };
  };
};

/* closing every cell keeps the caret, whichever cell holds it */
let close_keeps_caret = () => {
  let rec find = (s: Segment.t): option(Id.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when Tile.label(t) == ["+"] => Some(t.id)
        | Tile(t) => List.find_map(find, t.children)
        | _ => None
        },
      s,
    );
  let plus =
    switch (find(seg)) {
    | Some(id) => id
    | None => failwith("no +")
    };
  let z =
    switch (
      Move.jump_to_side_of_id(Util.Direction.Left, Zipper.unzip(seg), plus)
    ) {
    | Some(z) => z
    | None => failwith("no caret before +")
    };
  let p: Web.Program.t =
    Whole(Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=Exp)));
  /* the caret is in x, the first of the two cells */
  let (v, p) =
    V.realize(
      ~info_map,
      ~term,
      V.pin(~term, row("y"), V.pin(~term, row("x"), V.init)),
      p,
    );
  switch (V.realize(~info_map, ~term, V.discard(~term, v), p)) {
  | (_, Divided(_)) => fail("still divided")
  | (_, Whole(e)) =>
    switch (
      Siblings.neighbors(e.editor.editor.state.zipper.relatives.siblings)
    ) {
    | (_, Some(p)) =>
      check(bool, "caret before +", true, Piece.id(p) == plus)
    | _ => fail("nothing right of the caret")
    }
  };
};

let vanished = () => {
  let v = {
    ...V.init,
    pins: [
      {
        p_id: Id.mk(),
        p_run: false,
      },
    ],
    zoom: [Id.mk()],
  };
  let (v, p) = V.realize(~info_map, ~term, v, whole());
  check(int, "vanished pin dropped", 0, List.length(v.pins));
  check(int, "vanished zoom dropped", 0, List.length(v.zoom));
  check(bool, "whole", true, open_ids(p) == []);
};

let discard = () => {
  let (a, m, x) = (row("a"), row("M"), row("x"));
  let (v, _) =
    run([
      V.pin(~term, a),
      V.zoom_in(~term, m),
      V.pin(~term, x),
      V.discard(~term),
    ]);
  check(bool, "x dropped", false, pinned(x, v));
  check(bool, "a kept", true, pinned(a, v));
};

/* a ⇒ cell whose expression gets a new root (`f(1)` to `f(1) + 1`)
   follows it: it stays open, and pinned, when another row opens */
let tail_follows = () => {
  let src = "let f = fun x -> x in\nf(1)";
  let seg = parse(src);
  let rows_of = (term, keep: Web.OutlineTree.node => bool) => {
    let rec find = (ns: list(Web.OutlineTree.node)) =>
      List.find_map(
        (n: Web.OutlineTree.node) => keep(n) ? n.o_id : find(n.o_children),
        ns,
      );
    switch (find(Web.OutlineTree.of_term(term))) {
    | Some(id) => id
    | None => failwith("no such row")
    };
  };
  let trail = term => rows_of(term, n => n.o_kind == Web.OutlineTree.KTrail);
  let term = MakeTerm.Incr.term_of(seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  let t0 = trail(term);
  let (v, p) =
    V.realize(
      ~info_map,
      ~term,
      V.pin(~term, t0, V.init),
      Whole(Focus.cell_of_seg(seg)),
    );
  let d =
    switch (p) {
    | Divided(d) => d
    | Whole(_) => fail("the ⇒ cell didn't open")
    };
  /* type ` + 1` at the end of the cell, keeping its pieces */
  let cell =
    List.find((c: Web.ScratchCell.t) => c.e_id == t0, Divided.cells(d));
  let body = Focus.zip_of_cell(cell.e_body);
  let last =
    switch (List.find_map(Piece.is_tile, List.rev(body))) {
    | Some(t) => t.id
    | None => failwith("empty cell")
    };
  let z =
    switch (
      Move.jump_to_side_of_id(Util.Direction.Right, Zipper.unzip(body), last)
    ) {
    | Some(z) => z
    | None => failwith("no caret at the end")
    };
  let z =
    Test_Editing.perform(
      z,
      [Insert(" "), Insert("+"), Insert(" "), Insert("1")],
    );
  let d =
    Divided.update_cell(
      t0,
      c =>
        {
          ...c,
          e_body: Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=Exp)),
        },
      d,
    );
  let seg = Divided.document(d);
  let term = MakeTerm.Incr.term_of(seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  let t1 = trail(term);
  check(bool, "the ⇒ row has a new root", true, t1 != t0);
  let f = rows_of(term, n => n.o_label == "f");
  let (v, p) = V.realize(~info_map, ~term, V.pin(~term, f, v), Divided(d));
  check(bool, "the ⇒ pin follows", true, pinned(t1, v));
  check(bool, "the ⇒ cell stays open", true, List.mem(t1, open_ids(p)));
  check(bool, "f opens beside it", true, List.mem(f, open_ids(p)));
};

let tests = (
  "SlideView",
  [
    test_case("pin and unpin", `Quick, pin_unpin),
    test_case("zoom", `Quick, zoom),
    test_case("zoom shows the members", `Quick, zoom_members),
    test_case("pins outside the zoom", `Quick, pins_outside_zoom),
    test_case("pins inside the zoom", `Quick, pins_inside_zoom),
    test_case("park", `Quick, park),
    test_case("caret travels into its cell", `Quick, caret_travels),
    test_case("closing all keeps the caret", `Quick, close_keeps_caret),
    test_case("vanished pins and zoom", `Quick, vanished),
    test_case("discard at a level", `Quick, discard),
    test_case(
      "a ⇒ cell follows its expression's new root",
      `Quick,
      tail_follows,
    ),
  ],
);
