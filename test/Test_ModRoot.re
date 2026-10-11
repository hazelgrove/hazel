open Alcotest;
open Haz3lcore;
open Language;

/* Mod-root incrementality: go_incr(~root=Mod) ≡ go_mod_root, and a
   one-item edit re-parses one slice */

let mod_src = "let x = 1;
type T = Int;
module M = {
  let y = x + 16
};
x";

let parse_mod = CorpusUtil.seg_of(~root=Mod);

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

let semi_ids = (seg: Segment.t): list(Id.t) =>
  List.filter_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when Tile.label(t) == [";"] => Some(t.id)
      | _ => None
      },
    seg,
  );

let check_parity = (~allow_semi_diff=true, seg: Segment.t, incr: MakeTerm.t) => {
  let mono = MakeTerm.go_mod_root(seg);
  if (compare(incr.term, mono.term) != 0) {
    fail("term mismatch incr vs mono");
  };
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
  /* terms: top-level `;` entries may differ (the per-slice parse records a
     partial MultiHole there, the monolithic one the full item list) */
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
  let fb = MakeTerm.Incr.fell_back^;
  let incr = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg);
  check(int, "no fallback", fb, MakeTerm.Incr.fell_back^);
  check_parity(seg, incr);
};

let edit_seg = (~needle="16", ~repl="17", seg) =>
  CorpusUtil.edit_token(~needle, ~repl, seg);

let incremental_edit = () => {
  let seg = parse_mod(mod_src);
  let cache = MakeTerm.Incr.mk_cache();
  let _warm = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg);
  let (seg2, edited) = edit_seg(seg);
  check(bool, "edit found the literal", true, edited);
  MakeTerm.Incr.full_analyzed := 0;
  let fb = MakeTerm.Incr.fell_back^;
  let incr2 = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg2);
  check(int, "one slice reparsed", 1, MakeTerm.Incr.full_analyzed^);
  check(int, "no fallback", fb, MakeTerm.Incr.fell_back^);
  check_parity(seg2, incr2);
};

let term_of_mod_matches = () => {
  let seg = parse_mod(mod_src);
  let t = MakeTerm.Incr.term_of_root(~root=Mod, seg);
  let mono = MakeTerm.go_mod_root(seg);
  check(bool, "term_of_mod ≡ mono term", true, compare(t, mono.term) == 0);
};

/* a stray line typed above a member: that slice's `;` parses inside the
   member's definition, so slices can't simply concatenate */
let stray_line = () => {
  let src = "let x = 1;\nfoo\nlet y = 2;\nx";
  switch (CorpusUtil.typed_seg(~root=Mod, src)) {
  | None => fail("untypeable mod program")
  | Some(seg) =>
    let incr =
      MakeTerm.Incr.go_incr(~root=Mod, ~cache=MakeTerm.Incr.mk_cache(), seg);
    check_parity(seg, incr);
    MakeTerm.Incr.last := None;
    check(
      bool,
      "term_of_mod ≡ mono term",
      true,
      compare(
        MakeTerm.Incr.term_of_root(~root=Mod, seg),
        MakeTerm.go_mod_root(seg).term,
      )
      == 0,
    );
  };
};

/* Incr's last-result slot serves both roots: each gets its own reading */
let last_slot_keyed_by_root = () => {
  let seg = parse_mod(mod_src);
  let mono = MakeTerm.go_mod_root(seg).term;
  MakeTerm.Incr.last := None;
  let exp_t = MakeTerm.Incr.term_of(seg);
  check(bool, "readings differ by root", false, compare(exp_t, mono) == 0);
  let mod_t = MakeTerm.Incr.term_of_root(~root=Mod, seg);
  check(
    bool,
    "Mod reading after Exp ≡ mono",
    true,
    compare(mod_t, mono) == 0,
  );
  let exp_again = MakeTerm.Incr.term_of(seg);
  check(
    bool,
    "Exp reading after Mod ≡ first Exp reading",
    true,
    compare(exp_again, exp_t) == 0,
  );
};

/* ---- corpus scale: mega-mod-1k (build_mega.py compose_mod_root) ---- */

let corpus = () => {
  let seg = CorpusUtil.mega_seg(~root=Mod, "mega-mod-1k.hz");
  let cache = MakeTerm.Incr.mk_cache();
  let fb = MakeTerm.Incr.fell_back^;
  let incr = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg);
  check(int, "no fallback", fb, MakeTerm.Incr.fell_back^);
  check_parity(seg, incr);
  check(
    bool,
    "many items",
    true,
    switch (incr.term.term) {
    | Module(items) => List.length(items) > 20
    | _ => false
    },
  );
  let (seg2, edited) = edit_seg(~needle="180", ~repl="181", seg);
  check(bool, "edit found the literal", true, edited);
  MakeTerm.Incr.full_analyzed := 0;
  let incr2 = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg2);
  check(int, "one slice reparsed", 1, MakeTerm.Incr.full_analyzed^);
  check_parity(seg2, incr2);
};

let tests = (
  "ModRoot",
  [
    test_case("probe", `Quick, probe),
    test_case("full parity", `Quick, full_parity),
    test_case("incremental edit", `Quick, incremental_edit),
    test_case("term_of_mod", `Quick, term_of_mod_matches),
    test_case("stray line above a member", `Quick, stray_line),
    test_case("last slot keyed by root", `Quick, last_slot_keyed_by_root),
    test_case("corpus mega-mod-1k", `Quick, corpus),
  ],
);
