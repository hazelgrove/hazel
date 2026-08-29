open Alcotest;
open Haz3lcore;
open Language;

/* Stage C (plans/mod-root.md): Mod-as-root incrementality.
   Phase 1 gates: go_incr(~root=Mod) ≡ go_mod_root (term + maps), and
   an identity-preserving one-item edit re-parses exactly one slice. */

let mod_src = "let x = 1;
type T = Int;
module M = {
  let y = x + 16
};
x";

let parse_mod = (src: string): Segment.t =>
  switch (FastParse.of_text(~root=Mod, src)) {
  | Some(seg) => seg
  | None => fail("mod-root parse failed")
  };

let probe = () => {
  let seg = parse_mod(mod_src);
  let slices = MakeTerm.Incr.slices(seg);
  check(int, "4 slices", 4, List.length(slices));
  let mono = MakeTerm.go_mod_root(seg);
  switch (mono.term.term) {
  | Module(items) => check(int, "4 items", 4, List.length(items))
  | _ => fail("go_mod_root did not produce a Module term")
  };
};

/* is_semi from Incr is not exposed piecemeal; top-level `;` ids are
   the tile ids with label [";"] at the top level of the segment */
let semi_ids = (seg: Segment.t): list(Id.t) =>
  List.filter_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when t.label == [";"] => Some(t.id)
      | _ => None
      },
    seg,
  );

let check_parity = (~allow_semi_diff=true, seg: Segment.t, incr: MakeTerm.t) => {
  let mono = MakeTerm.go_mod_root(seg);
  /* term: exact */
  if (compare(incr.term, mono.term) != 0) {
    fail("term mismatch incr vs mono");
  };
  /* term_data: exact */
  let td_diff =
    Id.Map.merge(
      (_, a, b) =>
        switch (a, b) {
        | (Some(a), Some(b)) => compare(a, b) == 0 ? None : Some("value")
        | (Some(_), None) => Some("incr-only")
        | (None, Some(_)) => Some("mono-only")
        | (None, None) => None
        },
      incr.term_data,
      mono.term_data,
    );
  if (!Id.Map.is_empty(td_diff)) {
    Id.Map.iter(
      (id, why) => Printf.printf("TD DIFF %s: %s\n", Id.to_string(id), why),
      td_diff,
    );
    fail(
      Printf.sprintf("term_data mismatch: %d ids", Id.Map.cardinal(td_diff)),
    );
  };
  /* terms: exact except (documented) the top-level `;` entries — the
     per-slice parse records a partial MultiHole([item, synthetic
     hole]) there, the monolithic one the full item list */
  let semis = semi_ids(seg);
  let tm_diff =
    Id.Map.merge(
      (id, a, b) =>
        switch (a, b) {
        | (Some(a), Some(b)) =>
          compare(a, b) == 0
            ? None
            : allow_semi_diff && List.mem(id, semis) ? None : Some("value")
        | (Some(_), None) => Some("incr-only")
        | (None, Some(_)) => Some("mono-only")
        | (None, None) => None
        },
      incr.terms,
      mono.terms,
    );
  if (!Id.Map.is_empty(tm_diff)) {
    Id.Map.iter(
      (id, why) =>
        Printf.printf("TERMS DIFF %s: %s\n", Id.to_string(id), why),
      tm_diff,
    );
    fail(
      Printf.sprintf("terms mismatch: %d ids", Id.Map.cardinal(tm_diff)),
    );
  };
};

let full_parity = () => {
  let seg = parse_mod(mod_src);
  let cache = MakeTerm.Incr.mk_cache();
  let incr = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg);
  check(int, "no fallback", 0, MakeTerm.Incr.fell_back^);
  check_parity(seg, incr);
};

/* rewrite the "16" literal in place with a fresh id, preserving the
   physical identity of every untouched piece — the shape of a real
   editor edit after the remold identity restore */
let rec edit_piece = (~needle="16", ~repl="17", p: Piece.t): (Piece.t, bool) =>
  switch (p) {
  | Tile(t) when t.label == [needle] => (
      Tile({
        ...t,
        id: Id.mk(),
        label: [repl],
      }),
      true,
    )
  | Tile(t) =>
    let (children, changed) =
      List.fold_right(
        (seg, (segs, ch)) => {
          let (seg', ch') = edit_seg(~needle, ~repl, seg);
          ([seg', ...segs], ch || ch');
        },
        t.children,
        ([], false),
      );
    changed
      ? (
        Tile({
          ...t,
          children,
        }),
        true,
      )
      : (p, false);
  | p => (p, false)
  }
and edit_seg = (~needle="16", ~repl="17", seg: Segment.t): (Segment.t, bool) => {
  let (pieces, changed) =
    List.fold_right(
      (p, (ps, ch)) => {
        let (p', ch') = edit_piece(~needle, ~repl, p);
        ([p', ...ps], ch || ch');
      },
      seg,
      ([], false),
    );
  changed ? (pieces, true) : (seg, false);
};

let incremental_edit = () => {
  let seg = parse_mod(mod_src);
  let cache = MakeTerm.Incr.mk_cache();
  let _warm = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg);
  let (seg2, edited) = edit_seg(seg);
  check(bool, "edit found the literal", true, edited);
  MakeTerm.Incr.full_analyzed := 0;
  let incr2 = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg2);
  check(int, "one slice reparsed", 1, MakeTerm.Incr.full_analyzed^);
  check(int, "no fallback", 0, MakeTerm.Incr.fell_back^);
  check_parity(seg2, incr2);
};

let term_of_mod_matches = () => {
  let seg = parse_mod(mod_src);
  let t = MakeTerm.Incr.term_of_root(~root=Mod, seg);
  let mono = MakeTerm.go_mod_root(seg);
  check(bool, "term_of_mod ≡ mono term", true, compare(t, mono.term) == 0);
};

/* ---- Phase 2: DefStatics over a Module root ---- */

let settings = CoreSettings.on;
let ctx0 = Builtins.ctx_init(Some(Operators.default_mode));

let sorted_ids = (ids: list(Id.t)): list(string) =>
  List.sort_uniq(compare, List.map(Id.to_string, ids));

/* an error-bearing, richer program: labels, module member using an
   earlier binding, a type error, a trailing member expression */
let mod_src2 = "let x = 1;
type T = Int;
let bad : String = 42;
module M = {
  let y = x + 16
};
x + 1";

let statics_parity = () => {
  let seg = parse_mod(mod_src2);
  let term = MakeTerm.go_mod_root(seg).term;
  let ds = DefStatics.calc(~settings, term);
  let (mono_map, mono_elab) = Statics.mk_unmemoized(settings, ctx0, term);
  check(
    Alcotest.list(string),
    "error-id parity",
    sorted_ids(Statics.Map.error_ids(mono_map)),
    sorted_ids(DefStatics.all_error_ids(ds)),
  );
  check(
    Alcotest.list(string),
    "warning-id parity",
    sorted_ids(Statics.Map.warning_ids(mono_map)),
    sorted_ids(DefStatics.all_warning_ids(ds)),
  );
  switch (DefStatics.whole_elab(ds)) {
  | None => fail("whole_elab: shape gap")
  | Some(graft_elab) =>
    let (v1, _) = Evaluator.evaluate(~env=Builtins.env_init, mono_elab);
    let (v2, _) = Evaluator.evaluate(~env=Builtins.env_init, graft_elab);
    check(bool, "eval parity", true, Exp.fast_equal(v1, v2));
  };
};

let statics_incremental = () => {
  let seg = parse_mod(mod_src2);
  let term = MakeTerm.go_mod_root(seg).term;
  let ds0 = DefStatics.calc(~settings, term);
  let n_items = List.length(ds0.items);
  /* 5 mod items + the exports tail */
  check(int, "6 items", 6, n_items);
  /* body edit inside module M: only that item re-analyzes */
  let (seg2, edited) = edit_seg(seg);
  check(bool, "edit found the literal", true, edited);
  let term2 = MakeTerm.go_mod_root(seg2).term;
  let ds1 = DefStatics.calc(~settings, ~prev=ds0, term2);
  check(int, "1 item re-analyzed", 1, DefStatics.last_analyzed^);
  /* second calc on the SAME term: everything clean */
  let ds2 = DefStatics.calc(~settings, ~prev=ds1, term2);
  check(int, "0 items re-analyzed", 0, DefStatics.last_analyzed^);
  ignore(ds2);
};

/* ---- corpus scale: mega-mod-1k (build_mega.py compose_mod_root) ---- */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | ic =>
    let n = in_channel_length(ic);
    let s = really_input_string(ic, n);
    close_in(ic);
    Some(s);
  | exception _ => None
  };

let corpus = () => {
  let path = "hazel-programs/mega/mega-mod-1k.hz";
  let path = Sys.file_exists(path) ? path : "../" ++ path;
  switch (read_file(path)) {
  | None => fail("mega-mod-1k.hz unreadable")
  | Some(src) =>
    let seg = parse_mod(src);
    let cache = MakeTerm.Incr.mk_cache();
    let incr = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg);
    check(int, "no fallback", 0, MakeTerm.Incr.fell_back^);
    check_parity(seg, incr);
    let term = incr.term;
    let n_items =
      switch (term.term) {
      | Module(items) => List.length(items)
      | _ => (-1)
      };
    Printf.printf("CORPUS mod items: %d\n", n_items);
    check(bool, "many items", true, n_items > 20);
    /* compositional vs monolithic statics at corpus scale */
    let t0 = Sys.time();
    let ds0 = DefStatics.calc(~settings, term);
    Printf.printf(
      "CORPUS cold calc: %.0f ms\n",
      (Sys.time() -. t0) *. 1000.0,
    );
    let t0e = Sys.time();
    switch (DefStatics.whole_elab(ds0)) {
    | None => Printf.printf("CORPUS whole_elab: GAP\n")
    | Some(elab) =>
      Printf.printf(
        "CORPUS whole_elab: %.0f ms\n",
        (Sys.time() -. t0e) *. 1000.0,
      );
      let te = Sys.time();
      let (v, _) = Evaluator.evaluate(~env=Builtins.env_init, elab);
      Printf.printf("CORPUS eval: %.0f ms\n", (Sys.time() -. te) *. 1000.0);
      ignore(v);
    };
    let (mono_map, _) = Statics.mk_unmemoized(settings, ctx0, term);
    check(
      Alcotest.list(string),
      "corpus error-id parity",
      sorted_ids(Statics.Map.error_ids(mono_map)),
      sorted_ids(DefStatics.all_error_ids(ds0)),
    );
    /* one-member body edit: 180 -> 181 deep inside a module member */
    let (seg2, edited) = edit_seg(~needle="180", ~repl="181", seg);
    check(bool, "edit found the literal", true, edited);
    MakeTerm.Incr.full_analyzed := 0;
    let incr2 = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg2);
    check(int, "one slice reparsed", 1, MakeTerm.Incr.full_analyzed^);
    let ds1 = DefStatics.calc(~settings, ~prev=ds0, incr2.term);
    ignore(ds1);
    check(int, "1 item re-analyzed", 1, DefStatics.last_analyzed^);
  };
};

let tests = (
  "ModRoot",
  [
    test_case("probe", `Quick, probe),
    test_case("full parity", `Quick, full_parity),
    test_case("incremental edit", `Quick, incremental_edit),
    test_case("term_of_mod", `Quick, term_of_mod_matches),
    test_case("statics parity", `Quick, statics_parity),
    test_case("statics incremental", `Quick, statics_incremental),
    test_case("corpus mega-mod-1k", `Quick, corpus),
  ],
);
