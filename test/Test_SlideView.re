open Alcotest;
open Haz3lcore;
open Language;
open CorpusUtil;

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

let seg = seg_of(src);
let term = MakeTerm.Incr.term_of(seg);
let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;

let row = row_by_label(term);

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

/* the first open cell's body */
let body = (p: Web.Program.t): option(Web.CellEditor.Model.t) =>
  switch (p) {
  | Divided(d) => Some(List.hd(Divided.cells(d)).e_body)
  | Whole(_) => None
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
  /* as a pinned module opens it (its edits: module_row) */
  check(
    option(string),
    "its members, without braces",
    Some("let x = a + 1;\nlet y = x * 2"),
    Option.map(e => text_of(Focus.zip_of_cell(e)), body(p)),
  );
  let (_, p) = run([V.zoom_in(~term, m), V.zoom_out]);
  check(bool, "whole after zoom out", true, open_ids(p) == []);
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
  let two = tile("2", seg);
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
  let plus = tile("+", seg);
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
  let seg = seg_of(src);
  /* "": the ⇒ row */
  let trail = term => row_by_label(term, "");
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
  let f = row_by_label(term, "f");
  let (v, p) = V.realize(~info_map, ~term, V.pin(~term, f, v), Divided(d));
  check(bool, "the ⇒ pin follows", true, pinned(t1, v));
  check(bool, "the ⇒ cell stays open", true, List.mem(t1, open_ids(p)));
  check(bool, "f opens beside it", true, List.mem(f, open_ids(p)));
};

/* an opened module row shows its members, without braces, under its
   name; zooming in and out keeps that cell */
let module_row = () => {
  let (m, x) = (row("M"), row("x"));
  let (v, p) = run([V.pin(~term, x), V.pin(~term, m)]);
  check(bool, "one cell, the member folded in", true, open_ids(p) == [m]);
  check(bool, "not the zoom cell", false, V.showing_zoom_cell(~term, v));
  switch (p) {
  | Whole(_) => fail("not divided")
  | Divided(d) =>
    let e = List.hd(Divided.cells(d));
    check(bool, "a members cell", true, e.e_kind == Members);
    check(
      string,
      "the name above",
      "M",
      text_of(Focus.zip_of_cell(e.e_header)),
    );
    check(
      string,
      "the members below, without braces",
      "let x = a + 1;\nlet y = x * 2",
      text_of(Focus.zip_of_cell(e.e_body)),
    );
    let d =
      Divided.update_cell(
        m,
        (c: Web.ScratchCell.t) =>
          {
            ...c,
            /* a module name parses only in place: take N's */
            e_header:
              Focus.cell_of_seg(
                ~root=Sort.Pat,
                Focus.core_ws(
                  Option.get(
                    List.find_map(
                      (p: Piece.t) =>
                        switch (p) {
                        | Tile(t) => List.nth_opt(t.children, 0)
                        | _ => None
                        },
                      seg_of("module N = {} in 0"),
                    ),
                  ),
                ),
              ),
            e_body:
              Focus.cell_of_seg(
                ~root=Sort.Mod,
                seg_of(~root=Mod, "let x = a + 5;\nlet y = x * 2"),
              ),
          },
        d,
      );
    check(
      string,
      "edits land inside the braces, at depth",
      "let a = 1 in\nmodule N = {\n  let x = a + 5;\n  let y = x * 2\n} in\nlet b = M.y in\nb",
      text_of(Divided.document(d)),
    );
  };
  let (v, p) = run([V.pin(~term, m)]);
  let (v, p') = V.realize(~info_map, ~term, V.zoom_in(~term, m, v), p);
  check(
    bool,
    "zoomed in: the zoom cell",
    true,
    V.showing_zoom_cell(~term, v),
  );
  check(
    bool,
    "and the same cell",
    true,
    switch (body(p), body(p')) {
    | (Some(a), Some(b)) => a === b
    | _ => false
    },
  );
  let (v, p'') = V.realize(~info_map, ~term, V.zoom_out(v), p');
  check(
    bool,
    "zoomed out: its name again",
    false,
    V.showing_zoom_cell(~term, v),
  );
  check(
    bool,
    "still the same cell",
    true,
    switch (body(p), body(p'')) {
    | (Some(a), Some(b)) => a === b
    | _ => false
    },
  );
};

/* `{}` has no members to show: it opens as its definition */
let empty_module_row = () => {
  let seg = seg_of("module E = {} in\n1");
  let term = MakeTerm.Incr.term_of(seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  let e = row_by_label(term, "E");
  check(
    bool,
    "a module row",
    true,
    Web.OutlineTree.kind_of(e, term) == Some(Web.OutlineTree.KModule),
  );
  switch (
    V.realize(
      ~info_map,
      ~term,
      V.pin(~term, e, V.init),
      Whole(Focus.cell_of_seg(seg)),
    )
  ) {
  | (_, Whole(_)) => fail("didn't open")
  | (_, Divided(d)) =>
    let c = List.hd(Divided.cells(d));
    check(
      bool,
      "a definition cell",
      true,
      c.e_kind == Def({binds_module: true}),
    );
    check(string, "its braces", "{}", text_of(Focus.zip_of_cell(c.e_body)));
  };
};

/* opening a cell leaves the focus where it is; when the focused cell
   closes, a cell opening at once takes it, else the first body has it,
   and keeps it as others open */
let focus_stays = () => {
  let focus = (p: Web.Program.t) =>
    switch (p) {
    | Divided(d) =>
      Option.map(
        ((c: Web.ScratchCell.t, side)) => (c.e_id, side),
        Divided.focused(d),
      )
    | Whole(_) => None
    };
  let (a, b, m) = (row("a"), row("b"), row("M"));
  let (v, p) = run([V.pin(~term, a), V.pin(~term, b)]);
  check(
    bool,
    "opening b leaves it with a",
    true,
    focus(p) == Some((a, Body)),
  );
  let (_, p') =
    V.realize(~info_map, ~term, V.pin(~term, m, V.unpin(a, v)), p);
  check(bool, "M opens as a closes", true, focus(p') == Some((m, Body)));
  let (v, p) = V.realize(~info_map, ~term, V.unpin(a, v), p);
  check(bool, "a closes alone", true, focus(p) == Some((b, Body)));
  let (_, p) = V.realize(~info_map, ~term, V.pin(~term, m, v), p);
  check(bool, "and keeps it as M opens", true, focus(p) == Some((b, Body)));
};

let tests = (
  "SlideView",
  [
    test_case("pin and unpin", `Quick, pin_unpin),
    test_case("zoom", `Quick, zoom),
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
    test_case("a module row opens as its members", `Quick, module_row),
    test_case("an empty module opens whole", `Quick, empty_module_row),
    test_case("the focused cell keeps the focus", `Quick, focus_stays),
  ],
);
