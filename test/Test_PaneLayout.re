open Alcotest;
open Haz3lcore;

/* A livelit's open eye shows its syntax as an editor in a pane under the
   GUI (ProjectorPerform.ToggleSyntax). Syntax written on one long line is
   laid out to fit the pane; a layout that already fits is left as written.
   Either way only whitespace moves, so every tile keeps its id: that is
   what keeps the model's splices, and the caret, where they were. */

let parse = text =>
  switch (Parser.to_segment(text, ~root=Exp)) {
  | Some(seg) => seg
  | None => fail("parse: " ++ text)
  };

let rec tile_ids = (seg: Segment.t): list(Id.t) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) => [t.id, ...List.concat_map(tile_ids, t.children)]
      | _ => []
      },
    seg,
  );

let text = (seg: Segment.t): string =>
  Base.segment_to_string(
    ~holes="?",
    ~refractor_seg_to_seg=(r, seg) => (r, seg),
    ~projector_to_segment=(pr: Base.projector) => pr.syntax,
    seg,
  );

let lines = seg => String.split_on_char('\n', text(seg));

let widest = seg =>
  List.fold_left((w, l) => max(w, String.length(l)), 0, lines(seg));

let no_space = s =>
  String.concat("", String.split_on_char(' ', s))
  |> String.split_on_char('\n')
  |> String.concat("");

/* The Kids' Choice face's model, as an older copy of the slide wrote it. */
let one_line = "kid_face((smile = 85, brow = 92, drag = Idle, color = (head), burst = (boom), candy = (sweets), stars = (stars), hearts = (hearts), eyes = (eyes), sides = (sides)))";

let tests = (
  "PaneLayout",
  [
    test_case(
      "A one-line model is laid out to fit the pane",
      `Quick,
      () => {
        let seg = parse(one_line);
        check(bool, "written wider than the pane", true, widest(seg) > 80);
        let laid = ProjectorPerform.laid_out_for_pane(seg);
        check(
          bool,
          "now several lines",
          true,
          List.length(lines(laid)) > 1,
        );
        check(bool, "each fits", true, widest(laid) <= 80);
        check(
          string,
          "only whitespace moved",
          no_space(text(seg)),
          no_space(text(laid)),
        );
        check(
          list(string),
          "every tile keeps its id",
          List.map(id => Id.to_string(id), tile_ids(seg)),
          List.map(id => Id.to_string(id), tile_ids(laid)),
        );
      },
    ),
    test_case(
      "A cell nested in the pane is measured as wide as its text",
      `Quick,
      () => {
        /* f((head), x), with (head) a splice, as a regenerated use holds a
           cell, and the whole use one more, as the open eye holds it. */
        let use =
          switch (parse("f((head), x)")) {
          | [f, Tile({children: [[Tile(_) as cell, ...rest]], _} as args)] => [
              f,
              Tile({
                ...args,
                children: [[Piece.mk_splice([cell]), ...rest]],
              }),
            ]
          | _ => fail("unexpected shape for f((head), x)")
          };
        let pane = [Piece.mk_splice(use)];
        let rec find_x = (seg: Segment.t): option(Tile.t) =>
          List.find_map(
            (p: Piece.t) =>
              switch (p) {
              | Tile(t) when Tile.label(t) == ["x"] => Some(t)
              | Tile(t) => List.find_map(find_x, t.children)
              | Splice(s) => find_x(s.content)
              | _ => None
              },
            seg,
          );
        let x =
          switch (find_x(use)) {
          | Some(t) => t
          | None => fail("no x")
          };
        let m = Measured.of_segment(pane, Id.Map.empty, Id.Map.empty);
        let col = Measured.find_p(Tile(x), m).origin.col;
        check(int, "x sits where the text puts it", 10, col);
        check(string, "the text", "f((head), x)", text(use));
        /* The cell's own extent, which places a caret beside it. */
        let cell =
          switch (use) {
          | [_, Tile({children: [[Splice(_) as cell, ..._]], _})] => cell
          | _ => fail("no cell")
          };
        let cm = Measured.find_p(cell, m);
        check(
          pair(int, int),
          "the cell spans (head)",
          (2, 8),
          (cm.origin.col, cm.last.col),
        );
      },
    ),
    test_case(
      "A short model is left alone",
      `Quick,
      () => {
        let seg = parse("die(3)");
        check(
          string,
          "same text",
          text(seg),
          text(ProjectorPerform.laid_out_for_pane(seg)),
        );
      },
    ),
    test_case(
      "A hand layout that fits is left alone",
      `Quick,
      () => {
        let seg =
          parse(
            "kid_face((smile = 85, brow = 30, drag = Idle,\n  color = (head : Int), burst = (boom : Int), candy = (sweets : Int)))",
          );
        check(
          string,
          "same text",
          text(seg),
          text(ProjectorPerform.laid_out_for_pane(seg)),
        );
      },
    ),
  ],
);
