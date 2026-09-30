open Alcotest;
open Haz3lcore;
open Language;

/* What a slide shows (Web.SlideView): pins, zoom and parking only
   change which cells are open, never the program; pins outside the
   zoom come back on zoom out; parking keeps the pins.
   Run: bash test/run_node.sh test 'SlideView' */

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

let pins_outside_zoom = () => {
  let (a, m) = (row("a"), row("M"));
  let (v, p) = run([V.pin(~term, a), V.zoom_in(~term, m)]);
  check(bool, "a hidden inside M", true, open_ids(p) == [m]);
  check(bool, "a still pinned", true, V.pinned(a, v));
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
  check(bool, "pin kept", true, V.pinned(a, v));
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
  check(bool, "x dropped", false, V.pinned(x, v));
  check(bool, "a kept", true, V.pinned(a, v));
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
    test_case("vanished pins and zoom", `Quick, vanished),
    test_case("discard at a level", `Quick, discard),
  ],
);
