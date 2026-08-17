open Alcotest;
open Haz3lcore;

/* GEOMETRY pins — the test class whose absence let the joined step
   pass green while decorations/caret were broken live. Everything
   here is measured truth, headless:

   G-GRID        rendering every piece at its MEASURED position
                 reproduces the felt render exactly (measured and the
                 felt spec cannot disagree about where things are)
   G-NO-OVERLAP  no two width-bearing atoms share a cell (P5a at the
                 measured level — the sigil-over-delimiter bug class)
   G-MEASURED-INVISIBILITY
                 per-row max_col of the placed segment equals the
                 stripped segment's, except rows whose final atom is
                 a LineEndFree hole (+1 allowed)

   Pinch-class grout is zero-width (origin == last) and paints a
   boundary mark, not a cell — excluded from grid/overlap by
   construction. */

let string_testable = testable(Fmt.string, String.equal);

let parse = (s: string): Segment.t =>
  switch (Parser.to_zipper(~root=Sort.Exp, s)) {
  | None => Alcotest.fail("parse failed: " ++ String.escaped(s))
  | Some(z) => Zipper.unselect_and_zip(z)
  };

let measured_of = seg => Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);

/* (measurement, text) for every visible width-bearing atom */
let visible_atoms =
    (seg: Segment.t, m: Measured.t): list((Measured.measurement, string)) => {
  let cells = GroutCells.classify(seg);
  let rec go = (sg: Segment.t) =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Grout(g) =>
          switch (GroutCells.cls_of(cells, g.id)) {
          | Some(Pinch)
          | None => []
          | Some(c) =>
            let sigil =
              switch (g.shape) {
              | Convex => "?"
              | Concave => "~"
              };
            /* a LineEndPadded hole measures TWO columns: a blank pad
               cell then the glyph, so the glyph sits at origin+1 */
            let sigil = c == GroutCells.LineEndPadded ? " " ++ sigil : sigil;
            [(Measured.find_g(g, m), sigil)];
          }
        | Secondary(w) when Secondary.is_space(w) =>
          GroutCells.is_consumed(cells, w.id)
            ? [] : [(Measured.find_w(w, m), " ")]
        | Secondary(w) when Secondary.is_linebreak(w) => []
        | Secondary(w) => [
            (Measured.find_w(w, m), Secondary.get_string(w.content)),
          ]
        | Tile(t) =>
          let shards =
            Measured.find_shards(t, m)
            |> List.map(((i, meas)) => (meas, List.nth(t.label, i)));
          shards @ List.concat_map(go, t.children);
        | Projector(_) => []
        },
      sg,
    );
  go(seg);
};

/* paint atoms into a row/col grid; None = write conflict */
let grid = (seg: Segment.t): option(string) => {
  let m = measured_of(seg);
  let atoms = visible_atoms(seg, m);
  let tbl: Hashtbl.t((int, int), char) = Hashtbl.create(64);
  let ok = ref(true);
  List.iter(
    ((meas: Measured.measurement, text)) => {
      String.iteri(
        (i, ch) => {
          let key = (meas.origin.row, meas.origin.col + i);
          switch (Hashtbl.find_opt(tbl, key)) {
          | Some(_) => ok := false
          | None => Hashtbl.replace(tbl, key, ch)
          };
        },
        text,
      )
    },
    atoms,
  );
  if (ok^) {
    let max_row = Hashtbl.fold(((r, _), _, a) => max(r, a), tbl, 0);
    let rows =
      List.init(
        max_row + 1,
        r => {
          let max_col =
            Hashtbl.fold(
              ((r', c), _, a) => r' == r ? max(c, a) : a,
              tbl,
              -1,
            );
          String.init(max_col + 1, c =>
            switch (Hashtbl.find_opt(tbl, (r, c))) {
            | Some(ch) => ch
            | None => ' '
            }
          );
        },
      );
    Some(String.concat("\n", rows));
  } else {
    None;
  };
};

/* strip trailing spaces per row for comparison (grid pads rows with
   spaces where the felt render has none) */
let rstrip_rows = (s: string): string =>
  s
  |> String.split_on_char('\n')
  |> List.map(r => {
       let n = ref(String.length(r));
       while (n^ > 0 && r.[n^ - 1] == ' ') {
         n := n^ - 1;
       };
       String.sub(r, 0, n^);
     })
  |> String.concat("\n");

let corpus: list(string) = [
  "let x = in x",
  "let x =  in x",
  "1 2",
  "1 + + 2",
  "(1 +)",
  "(* 2)",
  "* 2",
  "  * 2",
  "1 +",
  "1 + ",
  "1 +    ",
  "1 +\n",
  "let x = 1 in\n",
  "1 +\n  ",
  "1 +\n\n",
  "let x =\nin x",
  "case\n| 1 => 1 end",
  "let x =\n\nin x",
  "let a =\n  #c#\nin a",
  "if then 2 else 3",
  "[1, , 3]",
  "fun -> 2",
  "f( )",
  {|"a" "b"|},
  "1 + #c#",
  "1 +\n#c#\n2",
];

/* combined per-segment check for the fuzzer: overlap + measured
   invisibility (grid==felt is corpus-only; too string-heavy per
   fuzz state) */
let invariants = (placed: Segment.t): option(string) => {
  switch (grid(placed)) {
  | None => Some("cell overlap")
  | Some(_) =>
    let stripped = GroutPlace.strip(placed);
    let mp = measured_of(placed);
    let ms = measured_of(stripped);
    let cells = GroutCells.classify(placed);
    /* AMENDED 2026-07-26: allowance is the WIDTH of the line-end hole
       on the row — one for LineEndFree, TWO for LineEndPadded (blank
       pad + glyph). Both sit past the line's text, so neither
       displaces anything; interior rows keep zero growth. */
    let free_rows = {
      let rec go = (sg: Segment.t, acc) =>
        List.fold_left(
          (acc, p: Piece.t) =>
            switch (p) {
            | Piece.Grout(g)
                when
                  switch (GroutCells.cls_of(cells, g.id)) {
                  | Some(c) => GroutCells.is_line_end(c)
                  | None => false
                  } => [
                (
                  Measured.find_g(g, mp).origin.row,
                  switch (GroutCells.cls_of(cells, g.id)) {
                  | Some(c) => GroutCells.width(c)
                  | None => 0
                  },
                ),
                ...acc,
              ]
            | Tile(t) => List.fold_left((a, k) => go(k, a), acc, t.children)
            | _ => acc
            },
          acc,
          sg,
        );
      go(placed, []);
    };
    let bad = ref(None);
    Measured.Rows.iter(
      (r, shape: Measured.Rows.shape) => {
        let sw =
          switch (Measured.Rows.find_opt(r, ms.rows)) {
          | Some(sh) => sh.max_col
          | None => 0
          };
        let allowance =
          free_rows
          |> List.filter(((row, _)) => row == r)
          |> List.fold_left((mx, (_, w)) => max(mx, w), 0);
        if (shape.max_col > sw + allowance || shape.max_col < sw) {
          bad :=
            Some(
              Printf.sprintf(
                "row %d placed=%d stripped=%d",
                r,
                shape.max_col,
                sw,
              ),
            );
        };
      },
      mp.rows,
    );
    bad^;
  };
};

let sweep = (name: string, f: (string, Segment.t) => option(string)) =>
  test_case(
    name,
    `Quick,
    () => {
      let out =
        corpus
        |> List.map(s =>
             switch (f(s, parse(s) |> GroutPlace.place)) {
             | None => ""
             | Some(v) =>
               Printf.sprintf("input=%s %s\n", String.escaped(s), v)
             }
           )
        |> String.concat("");
      check(string_testable, name, "", out);
    },
  );

let flat = (s: string): string =>
  String.split_on_char('\n', s) |> String.concat(" ⏎ ");

let tests = [
  (
    "GroutGeometry",
    [
      sweep("G-GRID measured render == felt render", (_, placed) =>
        switch (grid(placed)) {
        | None => Some("overlap during grid paint")
        | Some(g) =>
          /* pinch sigils are zero-width boundary marks, not cells —
             absent from the grid by design; trailing blank rows own
             no cells */
          let rec remove_all = (needle, s) =>
            switch (Test_CompletionDisplay.split_first(needle, s)) {
            | None => s
            | Some((pre, post)) => pre ++ remove_all(needle, post)
            };
          let depinch = s => s |> remove_all({|‽|}) |> remove_all({|∻|});
          let rstrip_trailing_rows = s => {
            let rows = String.split_on_char('\n', s);
            let rec drop = l =>
              switch (l) {
              | ["", ...tl] => drop(tl)
              | l => l
              };
            rows |> List.rev |> drop |> List.rev |> String.concat("\n");
          };
          let felt =
            FeltPrint.render(placed)
            |> depinch
            |> rstrip_rows
            |> rstrip_trailing_rows;
          let g = g |> rstrip_rows |> rstrip_trailing_rows;
          g == felt
            ? None : Some("grid=" ++ flat(g) ++ " felt=" ++ flat(felt));
        }
      ),
      sweep("G-NO-OVERLAP width-bearing atoms disjoint", (_, placed) =>
        switch (grid(placed)) {
        | None => Some("two atoms share a cell")
        | Some(_) => None
        }
      ),
      sweep("G-MEASURED-INVISIBILITY row widths match stripped", (_, placed) => {
        let stripped = GroutPlace.strip(placed);
        let mp = measured_of(placed);
        let ms = measured_of(stripped);
        let cells = GroutCells.classify(placed);
        /* AMENDED 2026-07-26 for the trailing pad: a row's placed
           width may exceed its stripped width by the columns the
           line-end classes occupy — one for LineEndFree, TWO for
           LineEndPadded (blank pad + glyph). Justified because both
           sit past the line's text and so displace nothing; interior
           rows keep the zero-growth bound (layout invisibility). The
           allowance is per row, taking the widest line-end hole on
           it. */
        let free_rows = {
          let rec go = (sg: Segment.t, acc) =>
            List.fold_left(
              (acc, p: Piece.t) =>
                switch (p) {
                | Piece.Grout(g)
                    when
                      switch (GroutCells.cls_of(cells, g.id)) {
                      | Some(c) => GroutCells.is_line_end(c)
                      | None => false
                      } => [
                    (
                      Measured.find_g(g, mp).origin.row,
                      switch (GroutCells.cls_of(cells, g.id)) {
                      | Some(c) => GroutCells.width(c)
                      | None => 0
                      },
                    ),
                    ...acc,
                  ]
                | Tile(t) =>
                  List.fold_left((a, k) => go(k, a), acc, t.children)
                | _ => acc
                },
              acc,
              sg,
            );
          go(placed, []);
        };
        let bad = ref([]);
        Measured.Rows.iter(
          (r, shape: Measured.Rows.shape) => {
            let sw =
              switch (Measured.Rows.find_opt(r, ms.rows)) {
              | Some(s) => s.max_col
              | None => 0
              };
            let allowance =
              free_rows
              |> List.filter(((row, _)) => row == r)
              |> List.fold_left((mx, (_, w)) => max(mx, w), 0);
            if (shape.max_col > sw + allowance || shape.max_col < sw) {
              bad :=
                [
                  Printf.sprintf(
                    "row %d placed=%d stripped=%d",
                    r,
                    shape.max_col,
                    sw,
                  ),
                  ...bad^,
                ];
            };
          },
          mp.rows,
        );
        bad^ == [] ? None : Some(String.concat("; ", bad^));
      }),
    ],
  ),
];
