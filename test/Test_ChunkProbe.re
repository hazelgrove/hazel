/* Code.view_chunked reuses a chunk's view while the sorts its tiles show
   are unchanged, and probes only some tiles for that (Code.chunk_probes):
   the parser's sorts of its top-level tiles, and the refined sorts of
   its Drv tiles. Over real edits, wherever that probe would reuse a
   chunk, probing every tile in it (as the view did before) must agree. */
open Alcotest;
open Haz3lcore;

let settings = {
  ...Language.CoreSettings.off,
  statics: true,
};

/* the two maps a tile's view reads its sorts from */
type world = {
  td: TermData.t,
  info: Language.Statics.Map.t,
};

let world = (seg: Segment.t): world => {
  let r = MakeTerm.go(seg);
  {
    td: r.term_data,
    info:
      CachedStatics.init_from_term(~settings, ~is_dynamic_term=true, r.term).
        info_map,
  };
};

let refined = (w: world, id: Id.t, s: Sort.t): Sort.t =>
  Language.Info.refine_sort_from_mold(~info_map=w.info, ~id, s);

let parsed = (w: world, id: Id.t): option(Sort.t) =>
  Option.map((d: TermData.data) => d.sort, Id.Map.find_opt(id, w.td));

/* what Code.view reads per tile, for every tile under a top-level one */
let rec all_tiles = (seg: Segment.t, acc: list(Tile.t)): list(Tile.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        List.fold_left((a, s) => all_tiles(s, a), [t, ...acc], t.children)
      | Splice(s) => all_tiles(s.content, acc)
      | _ => acc
      },
    acc,
    seg,
  );

let full_probe = (w: world, w': world, seg: Segment.t): bool =>
  List.for_all(
    (t: Tile.t) => {
      let s = Tile.mold(t).out;
      refined(w, t.id, s) == refined(w', t.id, s)
      && parsed(w, t.id) == parsed(w', t.id);
    },
    all_tiles(seg, []),
  );

let chunk_probe = (w: world, w': world, seg: Segment.t): bool => {
  let (top, drv) = Web.Code.chunk_probes(seg);
  List.for_all((t: Tile.t) => parsed(w, t.id) == parsed(w', t.id), top)
  && List.for_all(
       ((id, s)) => refined(w, id, s) == refined(w', id, s),
       drv,
     );
};

let perform = (z: Zipper.t, a: Action.t): Zipper.t => {
  let statics =
    CachedStatics.init_from_term(
      ~settings,
      ~is_dynamic_term=true,
      MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
    );
  switch (
    Perform.go(
      ~settings,
      ~statics,
      ~syntax=CachedSyntax.init(z),
      ~root=Exp,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => z
  | Error(err) => failwith("action failed: " ++ Action.Failure.show(err))
  };
};

let at = (z: Zipper.t, row: int, col: int): Zipper.t =>
  perform(
    z,
    Move(
      Point(
        {
          row,
          col,
        },
        None,
      ),
    ),
  );

/* small programs, derivations among them; edits on the last line keep
   the tiles before it (those after the caret are rebuilt, as in the app) */
let programs = [
  "let a = 1 in\nlet b = (2, [3]) in\nlet c = fun x -> x in\na + b",
  "let f = fun x ->\ncase x\n| 1 => 2\n| _ => 3\nend in\nlet g = f in\nf(0)",
  "type t = Int in\nlet x: t = 1 in\nlet y = (x, x) in\nx",
  "let j = of_jdmt [] |- 1 : Int end in\nlet p = of_prop $a /\\ $b end in\nj",
  "let c = of_ctx [x : Int] end in\nlet j = of_jdmt [] |- 1 : Int end in\nc",
];

let last_row = (src: string): int =>
  List.length(String.split_on_char('\n', src)) - 1;

let edits: list((string, int, Action.t)) = [
  ("operator", 0, Paste("1 +\n")),
  ("let", 0, Paste("let q = 0 in\n")),
  ("type alias", 0, Paste("type u = Bool in\n")),
  ("open paren", 0, Insert("(")),
  ("delete", 1, Destruct(Local(Left, ByChar))),
  ("insert", 1, Insert("z")),
];

type tally = {
  mutable shared: int, /* top-level tiles kept across an edit */
  mutable reused: int, /* of which the chunk probe reuses */
  mutable rerendered: int, /* of which the full probe re-renders */
  mutable drv: int /* Drv tiles the chunk probe read */
};

/* every top-level tile the edit kept (physically, as Measured keeps a
   chunk's layout) is probed as a one-tile chunk: both probes are
   conjunctions over tiles, so a chunk of several agrees if each does */
let check_edit = (tally, name: string, before: Segment.t, after: Segment.t) => {
  let (w, w') = (world(before), world(after));
  let old = Hashtbl.create(64);
  List.iter(p => Hashtbl.replace(old, Piece.id(p), p), before);
  List.iter(
    (p: Piece.t) =>
      switch (p, Hashtbl.find_opt(old, Piece.id(p))) {
      | (Tile(_), Some(q)) when q === p =>
        let seg = [p];
        tally.shared = tally.shared + 1;
        tally.drv =
          tally.drv + List.length(snd(Web.Code.chunk_probes(seg)));
        let full = full_probe(w, w', seg);
        if (!full) {
          tally.rerendered = tally.rerendered + 1;
        };
        if (chunk_probe(w, w', seg)) {
          tally.reused = tally.reused + 1;
          check(bool, name ++ ": reused only if unchanged", true, full);
        };
      | _ => ()
      },
    after,
  );
};

let small_edit = (tally, i: int, src: string, (what, col, a)) => {
  let z =
    switch (ParsedCorpus.to_segment(~root=Exp, src)) {
    | Some(seg) => Zipper.unzip(seg)
    | None => fail(Printf.sprintf("program %d unparseable", i))
    };
  check_edit(
    tally,
    Printf.sprintf("program %d, %s", i, what),
    Zipper.unselect_and_zip(z),
    Zipper.unselect_and_zip(perform(at(z, last_row(src), col), a)),
  );
};

/* mega-1k, as Test_EditLocality edits it: keystrokes, a paste, an undo,
   an item moved down (context changes around kept tiles), an agent's
   rewrite of a definition */
let mega_edits: list((string, unit => Test_EditLocality.edit)) = [
  ("insert", Test_EditLocality.insert),
  ("delete", Test_EditLocality.delete),
  ("paste", Test_EditLocality.paste),
  ("undo", Test_EditLocality.undo),
  ("restructure", Test_EditLocality.restructure),
  ("agent", Test_EditLocality.agent),
];

let probe_agrees = () => {
  let tally = {
    shared: 0,
    reused: 0,
    rerendered: 0,
    drv: 0,
  };
  List.iteri(
    (i, src) => List.iter(small_edit(tally, i, src), edits),
    programs,
  );
  let drv = tally.drv;
  List.iter(
    ((what, mk)) => {
      let e: Test_EditLocality.edit = mk();
      check_edit(tally, "mega-1k, " ++ what, e.before, e.after);
    },
    mega_edits,
  );
  Printf.printf(
    "shared %d, reused %d, full probe re-renders %d, drv probed %d\n",
    tally.shared,
    tally.reused,
    tally.rerendered,
    tally.drv,
  );
  check(bool, "edits keep top-level tiles", true, tally.shared >= 200);
  check(bool, "the probe reuses most", true, 2 * tally.reused > tally.shared);
  check(bool, "the derivations have Drv tiles", true, drv >= 10);
};

/* and it re-renders when a sort does move, from either source: without
   statics a Drv tile keeps its mold sort, and a top-level tile's parser
   sort is changed by hand (an edit that does so rebuilds the tile too) */
let probe_sees_moves = () => {
  let seg =
    switch (ParsedCorpus.to_segment(~root=Exp, List.nth(programs, 3))) {
    | Some(seg) => seg
    | None => fail("unparseable")
    };
  let w = world(seg);
  let unrefined = {
    ...w,
    info: Id.Map.empty,
  };
  let tiles =
    List.filter_map(
      fun
      | Piece.Tile(t) => Some(t)
      | _ => None,
      seg,
    );
  let with_drv =
    List.filter(
      (t: Tile.t) => snd(Web.Code.chunk_probes([Tile(t)])) != [],
      tiles,
    );
  check(bool, "a top-level tile holds Drv tiles", true, with_drv != []);
  let refines =
    List.exists(
      (t: Tile.t) => !full_probe(w, unrefined, [Tile(t)]),
      with_drv,
    );
  check(bool, "statics refine some of them", true, refines);
  List.iter(
    (t: Tile.t) => {
      let seg = [Piece.Tile(t)];
      check(
        bool,
        "Drv sorts: the probes agree",
        full_probe(w, unrefined, seg),
        chunk_probe(w, unrefined, seg),
      );
    },
    with_drv,
  );
  List.iter(
    (t: Tile.t) => {
      let seg = [Piece.Tile(t)];
      let moved = {
        ...w,
        td:
          Id.Map.update(
            t.id,
            Option.map((d: TermData.data) =>
              {
                ...d,
                sort: d.sort == Sort.Typ ? Sort.Pat : Sort.Typ,
              }
            ),
            w.td,
          ),
      };
      check(
        bool,
        "parser sort: full probe sees it",
        false,
        full_probe(w, moved, seg),
      );
      check(
        bool,
        "parser sort: chunk probe sees it",
        false,
        chunk_probe(w, moved, seg),
      );
    },
    tiles,
  );
};

let tests = (
  "ChunkProbe",
  [
    test_case(
      "reuses a chunk only if no tile's sort moved",
      `Quick,
      probe_agrees,
    ),
    test_case("re-renders when a sort moves", `Quick, probe_sees_moves),
  ],
);
