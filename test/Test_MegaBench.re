open Alcotest;
open Haz3lcore;
open Language;

/* Informational per-keystroke timing on the mega corpus, through the
   same stages the editor runs after a key: Perform.go (edit),
   CachedSyntax.mk (measurement + display term, incremental caches
   carried), CachedStatics.init_compositional (semantic term +
   statics), and the per-frame view completion
   (CanonicalCompletion.for_editor on the master segment). Always
   passes; numbers print to the log.
     bash test/run_node.sh test 'MegaBench' */

let settings = CoreSettings.on;

let ms = (f: unit => 'a): ('a, float) => {
  let t0 = Sys.time();
  let r = f();
  (r, (Sys.time() -. t0) *. 1000.);
};

/* the k-th int literal tile, depth-first */
let nth_int_literal = (k: int, seg: Segment.t): option(Id.t) => {
  let n = ref(0);
  let found = ref(None);
  let rec go = (sg: Segment.t) =>
    List.iter(
      (p: Piece.t) =>
        if (found^ == None) {
          switch (p) {
          | Tile(t) =>
            switch (Tile.single_token(t)) {
            | Some(tok) when Token.is_int(tok) =>
              if (n^ == k) {
                found := Some(t.id);
              };
              incr(n);
            | _ => List.iter(go, t.children)
            }
          | _ => ()
          };
        },
      sg,
    );
  go(seg);
  found^;
};

/* last piece of the k-th top-level item slice */
let nth_item_end = (k: int, seg: Segment.t): option(Id.t) =>
  switch (List.nth_opt(MakeTerm.Incr.slices(seg), k)) {
  | Some(item) => Option.map(Piece.id, Util.ListUtil.last_opt(item))
  | None => None
  };

type stage_ms = {
  perform: float,
  syntax: float,
  statics: float,
  view: float,
};

let zero = {
  perform: 0.,
  syntax: 0.,
  statics: 0.,
  view: 0.,
};
let add = (a, b) => {
  perform: a.perform +. b.perform,
  syntax: a.syntax +. b.syntax,
  statics: a.statics +. b.statics,
  view: a.view +. b.view,
};

let run_keys =
    (~settings=settings, ~label: string, z0: Zipper.t, keys: list(string))
    : (Zipper.t, stage_ms) => {
  let syntax0 = CachedSyntax.init(~root=Exp, z0);
  let statics0 =
    CachedStatics.init_compositional(
      ~settings,
      ~stitch=Fun.id,
      ~root=Exp,
      z0,
    );
  MakeTerm.Incr.fell_back := 0;
  MakeTerm.Incr.full_analyzed := 0;
  MakeTerm.Incr.analyzed := 0;
  Measured.Incr.built := 0;
  Measured.Incr.reused := 0;
  Zipper.sparse_hits := 0;
  Zipper.sparse_fallbacks := 0;
  let (z, _, _, tot, n) =
    List.fold_left(
      ((z, syntax: CachedSyntax.t, statics, tot, n), key) => {
        let (res, t_perform) =
          ms(() =>
            Perform.go(
              ~settings,
              ~statics,
              ~syntax,
              ~root=Exp,
              Action.Insert(key),
              {
                zipper: z,
                col_target: None,
              },
            )
          );
        switch (res) {
        | Error(_) =>
          Printf.printf("%s: Insert(%S) failed\n", label, key);
          (z, syntax, statics, tot, n);
        | Ok(z') =>
          {
            /* how much of the program survives the key physically */
            let old_slices =
              MakeTerm.Incr.slices(Zipper.unselect_and_zip(z));
            let new_slices =
              MakeTerm.Incr.slices(Zipper.unselect_and_zip(z'));
            let shared =
              List.length(
                List.filter(
                  ns =>
                    List.exists(os => Segment.ptr_eq(os, ns), old_slices),
                  new_slices,
                ),
              );
            Printf.printf(
              "  key %S: items %d, physically unchanged %d\n",
              key,
              List.length(new_slices),
              shared,
            );
          };
          let (syntax', t_syntax) =
            ms(() =>
              CachedSyntax.mk(
                ~root=Exp,
                ~m_cache=syntax.m_cache,
                ~t_cache=syntax.t_cache,
                ~info_map=Id.Map.empty,
                ~dyn_map=Id.Map.empty,
                z',
              )
            );
          let (statics', t_statics) =
            ms(() =>
              CachedStatics.init_compositional(
                ~settings,
                ~stitch=Fun.id,
                ~root=Exp,
                z',
              )
            );
          let (_, t_view) =
            ms(() =>
              CanonicalCompletion.for_editor(
                Zipper.unselect_and_zip(~erase_buffer=true, z'),
              )
            );
          (
            z',
            syntax',
            statics',
            add(
              tot,
              {
                perform: t_perform,
                syntax: t_syntax,
                statics: t_statics,
                view: t_view,
              },
            ),
            n + 1,
          );
        };
      },
      (z0, syntax0, statics0, zero, 0),
      keys,
    );
  let n = float_of_int(max(n, 1));
  Printf.printf(
    "MEGABENCH %s: keys=%d  perform=%.1f  syntax=%.1f  statics=%.1f  view=%.1f  total=%.1f ms/key  (fell_back=%d full_analyzed=%d term_analyzed=%d measured built=%d reused=%d sparse hits=%d fallbacks=%d)\n",
    label,
    int_of_float(n),
    tot.perform /. n,
    tot.syntax /. n,
    tot.statics /. n,
    tot.view /. n,
    (tot.perform +. tot.syntax +. tot.statics +. tot.view) /. n,
    MakeTerm.Incr.fell_back^,
    MakeTerm.Incr.full_analyzed^,
    MakeTerm.Incr.analyzed^,
    Measured.Incr.built^,
    Measured.Incr.reused^,
    Zipper.sparse_hits^,
    Zipper.sparse_fallbacks^,
  );
  (z, tot);
};

let explode = (s: string): list(string) =>
  List.init(String.length(s), i => String.make(1, s.[i]));

let bench = (file: string, ()) => {
  /* the runner turns the sparse/global parity check on; the editor does
     not — off for the measurement, restored after */
  let parity = Zipper.normalize_parity^;
  Zipper.normalize_parity := false;
  switch (CorpusUtil.corpus_seg(~root=Exp, file)) {
  | None => fail("corpus unreadable/unparseable: " ++ file)
  | Some(seg) =>
    let z0 = Zipper.unzip(seg);
    let n_items = List.length(MakeTerm.Incr.slices(seg));
    /* 1. complete program: digits typed into a literal deep inside */
    switch (nth_int_literal(400, seg)) {
    | None => Printf.printf("MEGABENCH %s: no literal\n", file)
    | Some(id) =>
      switch (Move.jump_to_side_of_id(Right, z0, id)) {
      | None => Printf.printf("MEGABENCH %s: jump failed\n", file)
      | Some(z) =>
        ignore(
          run_keys(~label=file ++ " complete/literal", z, explode("12345")),
        )
      }
    };
    /* 2. a new incomplete item on a fresh line between two items, then
       more typing */
    switch (nth_item_end(n_items / 2, seg)) {
    | None => Printf.printf("MEGABENCH %s: no item\n", file)
    | Some(id) =>
      switch (Move.jump_to_side_of_id(Right, z0, id)) {
      | None => Printf.printf("MEGABENCH %s: jump failed\n", file)
      | Some(z) =>
        let (z, _) =
          run_keys(
            ~label=file ++ " incomplete/new-let",
            z,
            explode("\nlet q = 1"),
          );
        ignore(
          run_keys(~label=file ++ " incomplete/digits", z, explode("2345")),
        );
      }
    };
  };
  Zipper.normalize_parity := parity;
};

let tests = (
  "MegaBench",
  [
    test_case("mega-1k keystrokes", `Slow, bench("mega-1k.hz")),
    test_case("mega-2k keystrokes", `Slow, bench("mega-2k.hz")),
  ],
);
