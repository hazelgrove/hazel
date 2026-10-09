open Alcotest;
open Haz3lcore;

/* chunked measurement (Measured.Incr) matches the monolithic one on every
   map, and rebuilds only changed chunks */

let corpus_seg = (name: string): option(Segment.t) => {
  let path = "hazel-programs/mega/" ++ name;
  let path = Sys.file_exists(path) ? path : "../hazel-programs/mega/" ++ name;
  Option.bind(CorpusUtil.read_file(path), src =>
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  );
};

let empty_shapes: Id.Map.t(ProjectorCore.Shape.t) = Id.Map.empty;

/* the chunks as one flat measurement */
let flatten = (m: Measured.t): Measured.flat =>
  Array.fold_left(
    (acc: Measured.flat, ch: Measured.chunk): Measured.flat => {
      let s = ch.c_start;
      let f = ch.c_flat;
      {
        tiles:
          Id.Map.union(
            (_, _, y) => Some(y),
            acc.tiles,
            Id.Map.map(
              List.map(((i, ms)) => (i, Measured.shift_m(s, ms))),
              f.tiles,
            ),
          ),
        grout:
          Id.Map.union(
            (_, _, y) => Some(y),
            acc.grout,
            Id.Map.map(Measured.shift_m(s), f.grout),
          ),
        secondary:
          Id.Map.union(
            (_, _, y) => Some(y),
            acc.secondary,
            Id.Map.map(Measured.shift_m(s), f.secondary),
          ),
        projectors:
          Id.Map.union(
            (_, _, y) => Some(y),
            acc.projectors,
            Id.Map.map(Measured.shift_m(s), f.projectors),
          ),
        rows:
          Measured.Rows.union(
            (_, _, y) => Some(y),
            acc.rows,
            f.rows
            |> Measured.Rows.bindings
            |> List.map(((r, sh)) => (r + s, sh))
            |> List.to_seq
            |> Measured.Rows.of_seq,
          ),
        piece_rows: f.piece_rows @ acc.piece_rows,
      };
    },
    Measured.empty_flat,
    m.chunks,
  );

let mono = (seg: Segment.t): Measured.flat =>
  flatten(Measured.of_segment(seg, empty_shapes, Id.Map.empty));

/* piece_rows' linebreaks get fresh ids at flush time, so compare the rest */
let canon_rows = (rows: list(list(Piece.t))): list(list(Id.t)) =>
  List.map(
    row =>
      row
      |> List.filter(p =>
           switch ((p: Piece.t)) {
           | Secondary(s) => !Secondary.is_linebreak(s)
           | _ => true
           }
         )
      |> List.map(Piece.id),
    rows,
  );

let meas_eq = (name, a: Measured.measurement, b: Measured.measurement) => {
  check(int, name ++ ":origin.row", a.origin.row, b.origin.row);
  check(int, name ++ ":origin.col", a.origin.col, b.origin.col);
  check(int, name ++ ":last.row", a.last.row, b.last.row);
  check(int, name ++ ":last.col", a.last.col, b.last.col);
};

let flats_agree = (name: string, m: Measured.flat, c: Measured.flat): unit => {
  let ids = f =>
    List.map(fst, Id.Map.bindings(f.Measured.tiles))
    @ List.map(fst, Id.Map.bindings(f.Measured.grout))
    @ List.map(fst, Id.Map.bindings(f.Measured.secondary))
    @ List.map(fst, Id.Map.bindings(f.Measured.projectors));
  check(
    int,
    name ++ ":id count",
    List.length(ids(m)),
    List.length(ids(c)),
  );
  check(
    bool,
    name ++ ":tiles",
    true,
    Id.Map.equal((==), m.Measured.tiles, c.Measured.tiles),
  );
  check(
    bool,
    name ++ ":grout",
    true,
    Id.Map.equal((==), m.Measured.grout, c.Measured.grout),
  );
  check(
    bool,
    name ++ ":secondary",
    true,
    Id.Map.equal((==), m.Measured.secondary, c.Measured.secondary),
  );
  check(
    bool,
    name ++ ":projectors",
    true,
    Id.Map.equal((==), m.Measured.projectors, c.Measured.projectors),
  );
  check(
    bool,
    name ++ ":rows",
    true,
    Measured.Rows.equal((==), m.Measured.rows, c.Measured.rows),
  );
  check(
    bool,
    name ++ ":piece_rows",
    true,
    canon_rows(m.Measured.piece_rows) == canon_rows(c.Measured.piece_rows),
  );
};

let check_parity = (name: string, seg: Segment.t): Measured.Incr.cache => {
  let cache = Measured.Incr.mk_cache();
  let chunked =
    Measured.Incr.of_segment(~cache, seg, empty_shapes, Id.Map.empty);
  flats_agree(name, mono(seg), flatten(chunked));
  /* spot-check the query path (translation), not just flatten */
  let m_t = Measured.of_segment(seg, empty_shapes, Id.Map.empty);
  check(
    int,
    name ++ ":num_rows",
    Measured.num_rows(m_t),
    Measured.num_rows(chunked),
  );
  List.iteri(
    (i, p) =>
      if (i mod 7 == 0) {
        switch (
          Measured.find_by_id(Piece.id(p), m_t),
          Measured.find_by_id(Piece.id(p), chunked),
        ) {
        | (Some(a), Some(b)) => meas_eq(name ++ ":find_by_id", a, b)
        | _ => fail(name ++ ": find_by_id missing")
        };
      },
    seg,
  );
  for (r0 in 0 to (Measured.num_rows(m_t) - 1) / 3) {
    let r = r0 * 3;
    check(
      int,
      name ++ ":row_indent " ++ string_of_int(r),
      Measured.row_indent(r, m_t),
      Measured.row_indent(r, chunked),
    );
  };
  cache;
};

let corpus_case = (file: string, min_chunks: int, ()) =>
  switch (corpus_seg(file)) {
  | None => fail("corpus unreadable/unparseable: " ++ file)
  | Some(seg) =>
    let cache = check_parity(file, seg);
    let chunked =
      Measured.Incr.of_segment(~cache, seg, empty_shapes, Id.Map.empty);
    check(
      bool,
      file ++ ": actually chunked",
      true,
      Array.length(chunked.chunks) >= min_chunks,
    );
    let b0 = Measured.Incr.built^;
    let _ = Measured.Incr.of_segment(~cache, seg, empty_shapes, Id.Map.empty);
    check(
      int,
      file ++ ": stable rebuild builds nothing",
      b0,
      Measured.Incr.built^,
    );
    /* copying one top-level piece (breaking ===) rebuilds about one chunk */
    let n = List.length(seg);
    let seg' =
      List.mapi(
        (i, p: Piece.t) =>
          i == n / 2
            ? switch (p) {
              | Tile(t) =>
                Piece.Tile({
                  ...t,
                  id: t.id,
                })
              | Grout(g) =>
                Piece.Grout({
                  ...g,
                  id: g.id,
                })
              | Secondary(w) =>
                Piece.Secondary({
                  ...w,
                  id: w.id,
                })
              | Projector(pr) =>
                Piece.Projector({
                  ...pr,
                  id: pr.id,
                })
              }
            : p,
        seg,
      );
    let b1 = Measured.Incr.built^;
    let chunked' =
      Measured.Incr.of_segment(~cache, seg', empty_shapes, Id.Map.empty);
    check(
      bool,
      file
      ++ ": localized rebuild (built "
      ++ string_of_int(Measured.Incr.built^ - b1)
      ++ ")",
      true,
      Measured.Incr.built^ - b1 <= 2,
    );
    flats_agree(file ++ ":after edit", mono(seg'), flatten(chunked'));
  };

let edge_programs = [
  ("two defs", "let a = 1 in\nlet b = 2 in\na + b"),
  ("blank lines", "let a = 1 in\n\n\nlet b = 2 in\na + b"),
  ("same-line defs", "let a = 1 in let b = 2 in\na + b"),
  ("continuation", "let a = 1 +\n2 +\n3 in\na"),
  ("case", "let f = fun x ->\ncase x\n| 1 => 2\n| _ => 3\nend in\nf(1)"),
  ("multiline tuple", "let t = (1,\n2,\n3) in\nt"),
  ("trailing blanks", "let a = 1 in\na\n\n"),
  ("leading blank", "\nlet a = 1 in\na"),
  ("single line", "1 + 2"),
  ("comment between", "let a = 1 in\n# note #\nlet b = 2 in\na + b"),
];

let edge_case = ((name, src), ()) =>
  switch (ParsedCorpus.to_segment(~root=Exp, src)) {
  | None => fail("unparseable edge program: " ++ name)
  | Some(seg) => ignore(check_parity(name, seg))
  };

let incomplete_case = ((name, src), ()) =>
  switch (CorpusUtil.typed_seg(src)) {
  | None => fail("untypeable program: " ++ name)
  | Some(seg) => ignore(check_parity(name, seg))
  };

let tests = (
  "MeasuredChunks",
  List.map(
    ((name, src)) => test_case(name, `Quick, edge_case((name, src))),
    edge_programs,
  )
  @ List.map(
      ((name, src)) =>
        test_case(
          "incomplete: " ++ name,
          `Quick,
          incomplete_case((name, src)),
        ),
      CorpusUtil.incomplete_programs,
    )
  @ [
    test_case("mega-1k parity", `Quick, corpus_case("mega-1k.hz", 20)),
    test_case("mega-2k parity", `Quick, corpus_case("mega-2k.hz", 20)),
    test_case("mega-4k parity", `Quick, corpus_case("mega-4k.hz", 20)),
  ]
  @ List.map(
      c => {
        let cache = Measured.Incr.mk_cache();
        Test_DrawerMeasurement.case_(
          ~measure=
            (seg, rows) =>
              Measured.Incr.of_segment(~cache, seg, empty_shapes, rows),
          c,
        );
      },
      Test_DrawerMeasurement.cases,
    ),
);
