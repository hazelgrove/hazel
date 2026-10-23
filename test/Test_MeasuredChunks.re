open Alcotest;
open Haz3lcore;

/* chunked measurement (Measured.Incr) matches the monolithic one on every
   map and lookup, and rebuilds only changed chunks. Chunks follow items
   (ItemSpans): a module literal splits into a head, its members' chunks and
   a foot; a member that is a let chain, into its items' chunks */

module M = Measured;

let empty_shapes: Id.Map.t(ProjectorCore.Shape.t) = Id.Map.empty;
let no_rows: Id.Map.t(int) = Id.Map.empty;

/* ---- text ---- */

let rec seg_text = (seg: Segment.t): string =>
  String.concat("", List.map(piece_text, seg))
and piece_text = (p: Piece.t): string =>
  switch (p) {
  | Tile(t) =>
    String.concat(
      "",
      Util.Aba.join(
        Tile.token(t),
        seg_text,
        Util.Aba.mk(t.shards, t.children),
      ),
    )
  | Grout(_) => "?"
  | Secondary(w) => Secondary.get_string(w.content)
  | Projector(_) => "#"
  };

let token_text = (tok: M.token): string =>
  switch (tok) {
  | Whole(p) => piece_text(p)
  | Shard(t, i) => Tile.token(t, i)
  };

let chunk_texts = (m: M.t): list(string) =>
  Array.to_list(
    Array.map(
      (ch: M.chunk) => String.concat("", List.map(token_text, ch.c_tokens)),
      m.chunks,
    ),
  );

/* ---- parity ---- */

let by_index = ((i, _), (j, _)) => Int.compare(i, j);

/* the chunks as one flat measurement: a split tile's shards from both */
let flatten = (m: M.t): M.flat =>
  Array.fold_left(
    (acc: M.flat, ch: M.chunk): M.flat => {
      let s = ch.c_start;
      let f = ch.c_flat;
      let union = (a, b) => Id.Map.union((_, _, y) => Some(y), a, b);
      {
        tiles:
          Id.Map.union(
            (_, a, b) => Some(List.sort(by_index, a @ b)),
            acc.tiles,
            Id.Map.map(M.shift_shards(s), f.tiles),
          ),
        grout: union(acc.grout, Id.Map.map(M.shift_m(s), f.grout)),
        secondary:
          union(acc.secondary, Id.Map.map(M.shift_m(s), f.secondary)),
        projectors:
          union(acc.projectors, Id.Map.map(M.shift_m(s), f.projectors)),
        rows:
          M.Rows.union(
            (_, _, y) => Some(y),
            acc.rows,
            f.rows
            |> M.Rows.bindings
            |> List.map(((r, sh)) => (r + s, sh))
            |> List.to_seq
            |> M.Rows.of_seq,
          ),
        piece_rows: f.piece_rows @ acc.piece_rows,
      };
    },
    M.empty_flat,
    m.chunks,
  );

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

let flats_agree = (name: string, m: M.flat, c: M.flat): unit => {
  /* the first tile measured differently, for the failure's log */
  switch (
    List.find_opt(
      ((id, shards)) => Id.Map.find_opt(id, c.tiles) != Some(shards),
      Id.Map.bindings(m.tiles),
    )
  ) {
  | Some((id, shards)) =>
    print_endline(
      name
      ++ ": tile "
      ++ Id.to_string(id)
      ++ " monolithic "
      ++ M.Shards.show(shards)
      ++ " chunked "
      ++ Option.fold(
           ~none="none",
           ~some=M.Shards.show,
           Id.Map.find_opt(id, c.tiles),
         ),
    )
  | None => ()
  };
  let maps = [
    ("tiles", Id.Map.equal((==), m.tiles, c.tiles)),
    ("grout", Id.Map.equal((==), m.grout, c.grout)),
    ("secondary", Id.Map.equal((==), m.secondary, c.secondary)),
    ("projectors", Id.Map.equal((==), m.projectors, c.projectors)),
    ("rows", M.Rows.equal((==), m.rows, c.rows)),
    ("piece_rows", canon_rows(m.piece_rows) == canon_rows(c.piece_rows)),
  ];
  List.iter(
    ((what, ok)) => check(bool, name ++ ":" ++ what, true, ok),
    maps,
  );
};

/* each piece in one chunk, a split tile in its head and foot, under one
   anchor per chunk; chunk_of_id maps every id (a split tile to its foot),
   and no other */
let structure = (name: string, m: M.t): unit => {
  let fail' = what => fail(name ++ ": " ++ what);
  let anchors = Hashtbl.create(64);
  let homes = Hashtbl.create(1024);
  Array.iter(
    (ch: M.chunk) => {
      if (Hashtbl.mem(anchors, ch.c_anchor)) {
        fail'("two chunks under one anchor");
      };
      Hashtbl.replace(anchors, ch.c_anchor, ());
      List.iter(
        id =>
          Hashtbl.replace(
            homes,
            id,
            [
              ch.c_anchor,
              ...Option.value(~default=[], Hashtbl.find_opt(homes, id)),
            ],
          ),
        M.ids_of_flat(ch.c_flat),
      );
    },
    m.chunks,
  );
  Hashtbl.iter(
    (id, hs) =>
      switch (Id.Map.find_opt(id, m.split), hs) {
      | (Some((head, foot)), [h2, h1]) when h1 == head && h2 == foot =>
        if (Id.Map.find_opt(id, m.chunk_of_id) != Some(foot)) {
          fail'("a split tile not mapped to its foot");
        }
      | (Some(_), _) => fail'("a split tile not in its head and foot")
      | (None, [h]) =>
        if (Id.Map.find_opt(id, m.chunk_of_id) != Some(h)) {
          fail'("an id mapped to another chunk");
        }
      | (None, _) => fail'("an id in two chunks")
      },
    homes,
  );
  check(
    int,
    name ++ ": chunk_of_id holds the chunks' ids",
    Hashtbl.length(homes),
    Id.Map.cardinal(m.chunk_of_id),
  );
};

let rec tiles_of = (seg: Segment.t): list(Tile.t) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) => [t, ...List.concat_map(tiles_of, t.children)]
      | _ => []
      },
    seg,
  );

/* chunked ≡ monolithic: the maps, and every lookup by id, shard and row */
let agree = (name: string, seg: Segment.t, mono: M.t, chunked: M.t): unit => {
  flats_agree(name, flatten(mono), flatten(chunked));
  structure(name, chunked);
  check(int, name ++ ":num_rows", M.num_rows(mono), M.num_rows(chunked));
  List.iter(
    id =>
      if (M.find_by_id(id, mono) != M.find_by_id(id, chunked)) {
        fail(name ++ ": find_by_id " ++ Id.to_string(id));
      },
    Segment.ids(seg),
  );
  List.iter(
    (t: Tile.t) =>
      if (M.find_shards_by_id(t.id, mono)
          != M.find_shards_by_id(t.id, chunked)
          || M.find_p(Tile(t), mono) != M.find_p(Tile(t), chunked)) {
        fail(name ++ ": shards of " ++ String.concat(" ", Tile.label(t)));
      },
    tiles_of(seg),
  );
  for (r in 0 to M.num_rows(mono) - 1) {
    if (M.row_shape(r, mono) != M.row_shape(r, chunked)) {
      fail(name ++ ": row_shape " ++ string_of_int(r));
    };
  };
};

/* one incremental build, checked against the monolithic one; [at_most]
   bounds the chunks it re-measures */
let step =
    (
      ~root: Sort.t,
      ~shapes=empty_shapes,
      ~rows=no_rows,
      ~at_most: option(int)=?,
      ~cache: M.Incr.cache,
      name: string,
      seg: Segment.t,
    )
    : M.t => {
  let b0 = M.Incr.built^;
  let chunked = M.Incr.of_segment(~root, ~cache, seg, shapes, rows);
  let built = M.Incr.built^ - b0;
  Option.iter(
    n =>
      check(
        bool,
        Printf.sprintf("%s: re-measured %d, at most %d", name, built, n),
        true,
        built <= n,
      ),
    at_most,
  );
  agree(name, seg, M.of_segment(seg, shapes, rows), chunked);
  chunked;
};

let parity = (~root=Sort.Exp, ~shapes=?, ~rows=?, name, seg) =>
  ignore(step(~root, ~shapes?, ~rows?, ~cache=M.Incr.mk_cache(), name, seg));

/* ---- programs ---- */

/* the first tile with this one token, replaced; untouched pieces stay
   physically shared, as an edit leaves them */
let replace_tile =
    (label: string, f: Tile.t => Piece.t, seg: Segment.t): Segment.t => {
  let found = ref(false);
  let rec go = (seg: Segment.t): Segment.t => {
    let seg' = List.map(piece, seg);
    List.for_all2((===), seg, seg') ? seg : seg';
  }
  and piece = (p: Piece.t): Piece.t =>
    switch (p) {
    | _ when found^ => p
    | Tile(t) when Tile.label(t) == [label] =>
      found := true;
      f(t);
    | Tile(t) =>
      let children = List.map(go, t.children);
      List.for_all2((===), t.children, children)
        ? p
        : Tile({
            ...t,
            children,
          });
    | _ => p
    };
  let seg' = go(seg);
  if (! found^) {
    fail("no tile " ++ label);
  };
  seg';
};

let tile_id = (label: string, seg: Segment.t): Id.t =>
  CorpusUtil.tile(label, seg);

/* a fold over the tile, so a projector of [shape] stands in for it */
let project = (label: string, seg: Segment.t): (Segment.t, Id.t) => {
  let id = Id.mk();
  let seg' =
    replace_tile(
      label,
      t => Projector(ProjectorCore.mk(~id, Fold, Piece.Tile(t), "")),
      seg,
    );
  (seg', id);
};

let shape = (horizontal, vertical): ProjectorCore.Shape.t => {
  horizontal,
  vertical,
};

/* the brace of the module whose head is the tile [label] */
let brace_after = (label: string, seg: Segment.t): Id.t => {
  let rec find = (seg: Segment.t): option(Id.t) =>
    switch (seg) {
    | [] => None
    | [Tile(t), ...rest] when Tile.label(t) == [label, "="] =>
      List.find_map(
        (p: Piece.t) =>
          switch (p) {
          | Tile(b) when Tile.label(b) == ["{", "}"] => Some(b.id)
          | _ => None
          },
        rest,
      )
    | [Tile(t), ...rest] =>
      switch (List.find_map(find, t.children)) {
      | Some(_) as r => r
      | None => find(rest)
      }
    | [_, ...rest] => find(rest)
    };
  switch (find(seg)) {
  | Some(id) => id
  | None => fail("no brace after " ++ label)
  };
};

let mod_program = "# header #

module A = {
  let x = 1;

  # about y #
  let y = 2;
  module B = {
    let c = 3;
    let d = 4
  };
  let chain = 1 in
  let more = 2 in
  chain + more;
  test x == 1 end
};
module One = { let p = 1 };
A.x";

let mod_chunks = [
  "# header #\n\n",
  "module A = {\n",
  "  let x = 1;\n\n",
  "  # about y #\n  let y = 2;\n",
  "  module B = {\n",
  "    let c = 3;\n",
  "    let d = 4\n",
  "  };\n",
  "  let chain = 1 in\n",
  "  let more = 2 in\n",
  "  chain + more;\n",
  "  test x == 1 end\n",
  "};\n",
  "module One = { let p = 1 };\n",
  "A.x",
];

let exp_program = "module M = {
  let x = 1;
  module N = {
    let y = 2;
    let z = 3
  };
  test x == 1 end
} in
let m = {
  let a = 3;
  let b = 4
} in
module O = { let one = 1; let two = 2 } in
module E = {} in
module H = {
  let a = 1 in
  module K = {
    let c = 3;
    let e = 5
  } in
  a + K.c
} in
M.x";

let exp_chunks = [
  "module M = {\n",
  "  let x = 1;\n",
  "  module N = {\n",
  "    let y = 2;\n",
  "    let z = 3\n",
  "  };\n",
  "  test x == 1 end\n",
  "} in\n",
  "let m = {\n",
  "  let a = 3;\n",
  "  let b = 4\n",
  "} in\n",
  "module O = { let one = 1; let two = 2 } in\n",
  "module E = {} in\n",
  "module H = {\n",
  "  let a = 1 in\n",
  "  module K = {\n",
  "    let c = 3;\n",
  "    let e = 5\n",
  "  } in\n",
  "  a + K.c\n",
  "} in\n",
  "M.x",
];

/* comment blocks at a body's edges are chunks of their own; members
   sharing the brace lines stay with the head and foot */
let body_edges_program = "module C = {
  # intro #

  let x = 1;
  # trailing #
};
module D = { let a = 1;
  let b = 2;
  let c = 3 };
C.x";

let body_edges_chunks = [
  "module C = {\n",
  "  # intro #\n\n",
  /* the hole the parser leaves after a last `;` */
  "  let x = 1;?\n",
  "  # trailing #\n",
  "};\n",
  "module D = { let a = 1;\n",
  "  let b = 2;\n",
  "  let c = 3 };\n",
  "C.x",
];

/* ---- cases ---- */

let chunks_case = (~root, src, want, ()) => {
  let seg = CorpusUtil.seg_of(~root, src);
  let m =
    M.Incr.of_segment(
      ~root,
      ~cache=M.Incr.mk_cache(),
      seg,
      empty_shapes,
      no_rows,
    );
  /* the texts first: a wrong cut reads plainest there */
  check(list(string), "chunks", want, chunk_texts(m));
  agree("chunks", seg, M.of_segment(seg, empty_shapes, no_rows), m);
};

/* a member's edit re-measures its chunk alone: the head and foot keep
   their shards across the rebuilt module tiles */
let member_edits = (~root, src, edits, ()) => {
  let cache = M.Incr.mk_cache();
  let seg = CorpusUtil.seg_of(~root, src);
  ignore(step(~root, ~cache, "before", seg));
  ignore(
    List.fold_left(
      (seg, (needle, repl)) => {
        let (seg', found) =
          CorpusUtil.edit_token(
            ~at=`First,
            ~keep_id=true,
            ~needle,
            ~repl,
            seg,
          );
        check(bool, "edited " ++ needle, true, found);
        ignore(
          step(~root, ~at_most=1, ~cache, needle ++ "->" ++ repl, seg'),
        );
        seg';
      },
      seg,
      edits,
    ),
  );
};

/* refractor rows and projectors inside members, on a split tile and on a
   module's head tile; changing one re-measures its chunk */
let shapes_case = () => {
  let root = Sort.Mod;
  let seg = CorpusUtil.seg_of(~root, mod_program);
  let (seg, tab) = project("3", seg);
  let (seg, block) = project("1", seg);
  let (seg, inline) = project("4", seg);
  let shapes =
    Id.Map.of_list([
      (tab, shape(3, Tab(2))),
      (block, shape(2, Block(1))),
      (inline, shape(5, Inline)),
    ]);
  let rows =
    Id.Map.of_list([
      (tile_id("2", seg), 2),
      (brace_after("module", seg), 1),
      (tile_id("more", seg), 1),
    ]);
  let cache = M.Incr.mk_cache();
  ignore(step(~root, ~shapes, ~rows, ~cache, "drawers", seg));
  ignore(step(~root, ~shapes, ~rows, ~at_most=0, ~cache, "again", seg));
  let shapes = Id.Map.add(tab, shape(3, Tab(3)), shapes);
  ignore(
    step(~root, ~shapes, ~rows, ~at_most=1, ~cache, "a tab grows", seg),
  );
  let rows = Id.Map.add(tile_id("x", seg), 3, rows);
  ignore(step(~root, ~shapes, ~rows, ~at_most=1, ~cache, "a drawer", seg));
  /* the Exp form: rows on a split `…in` tile, applied in its foot */
  let root = Sort.Exp;
  let seg = CorpusUtil.seg_of(~root, exp_program);
  let m_tile =
    switch (seg) {
    | [Tile(t), ..._] => t.id
    | _ => fail("no module tile")
    };
  let rows = Id.Map.of_list([(m_tile, 2), (tile_id("y", seg), 1)]);
  let (seg, tab) = project("5", seg);
  let shapes = Id.Map.singleton(tab, shape(1, Tab(1)));
  parity(~root, ~shapes, ~rows, "exp drawers", seg);
};

let split_counts = () => {
  let m = (~root, src) =>
    M.Incr.of_segment(
      ~root,
      ~cache=M.Incr.mk_cache(),
      CorpusUtil.seg_of(~root, src),
      empty_shapes,
      no_rows,
    );
  /* A and B's braces; M, N, m, H and K's, and the `…in` tiles of M, m, H
     and K */
  check(
    int,
    "mod splits",
    2,
    Id.Map.cardinal(m(~root=Mod, mod_program).split),
  );
  check(
    int,
    "exp splits",
    9,
    Id.Map.cardinal(m(~root=Exp, exp_program).split),
  );
  check(
    int,
    "one-line modules don't split",
    0,
    Id.Map.cardinal(
      m(~root=Exp, "module O = { let one = 1; let two = 2 } in\nO.one").split,
    ),
  );
};

/* an edit in one module partitions that module's body alone */
let bodies_case = () => {
  let root = Sort.Mod;
  let seg = CorpusUtil.mega_seg(~root, "mega-mod-1k.hz");
  let cache = M.Incr.mk_cache();
  ignore(step(~root, ~cache, "before", seg));
  ignore(
    List.fold_left(
      (seg, (needle, repl)) => {
        let (seg', _) =
          CorpusUtil.edit_token(
            ~at=`First,
            ~keep_id=true,
            ~needle,
            ~repl,
            seg,
          );
        let b0 = M.Partition.bodies_built^;
        ignore(step(~root, ~at_most=1, ~cache, needle, seg'));
        check(int, needle ++ ": bodies", 1, M.Partition.bodies_built^ - b0);
        seg';
      },
      seg,
      [("180", "1800"), ("75", "750")],
    ),
  );
};

/* ids spread over an id table's buckets: parsed ids share bytes */
let id_hash_case = () => {
  let ids = Segment.ids(CorpusUtil.mega_seg("mega-4k.hz"));
  let tbl = M.IdTbl.create(16);
  List.iter(id => M.IdTbl.replace(tbl, id, ()), ids);
  let stats = M.IdTbl.stats(tbl);
  check(
    bool,
    Printf.sprintf(
      "%d ids, longest bucket %d",
      stats.num_bindings,
      stats.max_bucket_length,
    ),
    true,
    stats.max_bucket_length <= 16,
  );
};

let edge_case = ((name, src), ()) => parity(name, CorpusUtil.seg_of(src));

let incomplete_case = ((name, src), ()) =>
  switch (CorpusUtil.typed_seg(src)) {
  | None => fail("untypeable program: " ++ name)
  | Some(seg) => parity(name, seg)
  };

let shapes_of = (~root, programs, ()) =>
  List.iter(
    ((name, src)) => parity(~root, name, CorpusUtil.seg_of(~root, src)),
    programs,
  );

let corpus_case = (~root=Sort.Exp, file: string, ~min_chunks: int, ()) => {
  let seg = CorpusUtil.mega_seg(~root, file);
  let cache = M.Incr.mk_cache();
  let m = step(~root, ~cache, file, seg);
  check(
    bool,
    file ++ ": actually chunked",
    true,
    Array.length(m.chunks) >= min_chunks,
  );
  check(bool, file ++ ": modules split", true, !Id.Map.is_empty(m.split));
  ignore(step(~root, ~at_most=0, ~cache, file ++ ": stable rebuild", seg));
  /* copying one top-level piece (breaking ===) rebuilds about one chunk */
  let n = List.length(seg);
  let copied =
    List.mapi((i, p) => i == n / 2 ? CorpusUtil.copy_piece(p) : p, seg);
  ignore(
    step(~root, ~at_most=2, ~cache, file ++ ": a copied piece", copied),
  );
  /* a token deep in a member: early and mid-program */
  ignore(
    List.fold_left(
      (seg, (needle, repl)) => {
        let (seg', found) =
          CorpusUtil.edit_token(
            ~at=`First,
            ~keep_id=true,
            ~needle,
            ~repl,
            seg,
          );
        check(bool, file ++ ": edited " ++ needle, true, found);
        ignore(
          step(~root, ~at_most=1, ~cache, file ++ ": " ++ needle, seg'),
        );
        seg';
      },
      copied,
      [("180", "1800"), ("1800", "18"), ("75", "750")],
    ),
  );
};

/* every slide, and the stdlib's sources where the repo has them */
let slides = () => {
  let checked = ref(0);
  List.iter(
    ((path, src)) =>
      switch (CorpusUtil.parse(src)) {
      | Some(seg) =>
        incr(checked);
        parity(path, seg);
      | None => ()
      },
    CorpusUtil.slide_corpus(),
  );
  check(bool, "slides read", true, checked^ > 20);
  List.iter(
    ((path, src)) =>
      parity(~root=Mod, path, CorpusUtil.seg_of(~root=Mod, src)),
    Test_ItemSpans.stdlib_corpus(),
  );
};

let tests = (
  "MeasuredChunks",
  List.map(
    ((name, src)) => test_case(name, `Quick, edge_case((name, src))),
    CorpusUtil.edge_programs,
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
    test_case(
      "module chunks, Mod root",
      `Quick,
      chunks_case(~root=Mod, mod_program, mod_chunks),
    ),
    test_case(
      "module chunks, Exp root",
      `Quick,
      chunks_case(~root=Exp, exp_program, exp_chunks),
    ),
    test_case(
      "module body edges",
      `Quick,
      chunks_case(~root=Mod, body_edges_program, body_edges_chunks),
    ),
    test_case("split tiles", `Quick, split_counts),
    test_case(
      "member edits, Mod root",
      `Quick,
      member_edits(
        ~root=Mod,
        mod_program,
        /* the last in a head: its split tiles stay mapped to the foot */
        [
          ("2", "22"),
          ("3", "33"),
          ("more", "most"),
          ("p", "q"),
          ("A", "Z"),
        ],
      ),
    ),
    test_case(
      "member edits, Exp root",
      `Quick,
      member_edits(
        ~root=Exp,
        exp_program,
        [("y", "w"), ("4", "44"), ("5", "55"), ("c", "cc"), ("M", "Z")],
      ),
    ),
    test_case("drawers and projectors in members", `Quick, shapes_case),
    test_case("one module's body per edit", `Quick, bodies_case),
    test_case("id hashing", `Quick, id_hash_case),
    test_case(
      "item shapes, Exp root",
      `Quick,
      shapes_of(~root=Exp, Test_ItemSpans.exp_shapes),
    ),
    test_case(
      "item shapes, Mod root",
      `Quick,
      shapes_of(~root=Mod, Test_ItemSpans.letin_members),
    ),
    /* Mega 2k and 4k repeat its shapes */
    test_case(
      "mega-1k parity",
      `Quick,
      corpus_case("mega-1k.hz", ~min_chunks=20),
    ),
    test_case(
      "mega-mod-1k parity",
      `Quick,
      corpus_case(~root=Mod, "mega-mod-1k.hz", ~min_chunks=20),
    ),
    test_case("slides and stdlib parity", `Slow, slides),
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
