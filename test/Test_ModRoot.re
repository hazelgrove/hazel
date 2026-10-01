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

/* ---- DefStatics over a Module root ---- */

let settings = CoreSettings.on;
let ctx0 = Builtins.ctx_init(Some(Operators.default_mode));

let sorted_ids = CorpusUtil.sorted_ids;

/* with a type error and a member using an earlier binding */
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
  let (seg2, edited) = edit_seg(seg);
  check(bool, "edit found the literal", true, edited);
  let term2 = MakeTerm.go_mod_root(seg2).term;
  let ds1 = DefStatics.calc(~settings, ~prev=ds0, term2);
  /* the module item (a cheap surrogate) plus the one edited member */
  check(int, "item + 1 member re-analyzed", 2, DefStatics.last_analyzed^);
  let ds2 = DefStatics.calc(~settings, ~prev=ds1, term2);
  check(int, "0 items re-analyzed", 0, DefStatics.last_analyzed^);
  ignore(ds2);
};

/* a recursive type export gets a fresh Rec binder per analysis: a member
   edit that keeps the exports must not re-analyze the module's users */
let recursive_type_export = () => {
  let seg =
    parse_mod(
      "module A = {\n  type L = Nil + Cons(Int, L);\n  let x = 180\n};\nlet u = A.x;\nu",
    );
  let ds0 = DefStatics.calc(~settings, MakeTerm.go_mod_root(seg).term);
  let (seg2, edited) = edit_seg(~needle="180", ~repl="181", seg);
  check(bool, "edit found the literal", true, edited);
  ignore(
    DefStatics.calc(~settings, ~prev=ds0, MakeTerm.go_mod_root(seg2).term),
  );
  check(int, "item + 1 member re-analyzed", 2, DefStatics.last_analyzed^);
};

/* ---- corpus scale: mega-mod-1k (build_mega.py compose_mod_root) ---- */

let corpus = () => {
  switch (CorpusUtil.mega_src("mega-mod-1k.hz")) {
  | None => fail("mega-mod-1k.hz unreadable")
  | Some(src) =>
    let seg = parse_mod(src);
    let cache = MakeTerm.Incr.mk_cache();
    let fb = MakeTerm.Incr.fell_back^;
    let incr = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg);
    check(int, "no fallback", fb, MakeTerm.Incr.fell_back^);
    check_parity(seg, incr);
    let term = incr.term;
    let n_items =
      switch (term.term) {
      | Module(items) => List.length(items)
      | _ => (-1)
      };
    Printf.printf("CORPUS mod items: %d\n", n_items);
    check(bool, "many items", true, n_items > 20);
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
    let (seg2, edited) = edit_seg(~needle="180", ~repl="181", seg);
    check(bool, "edit found the literal", true, edited);
    MakeTerm.Incr.full_analyzed := 0;
    let incr2 = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg2);
    check(int, "one slice reparsed", 1, MakeTerm.Incr.full_analyzed^);
    let ds1 = DefStatics.calc(~settings, ~prev=ds0, incr2.term);
    ignore(ds1);
    check(int, "item + 1 member re-analyzed", 2, DefStatics.last_analyzed^);
  };
};

/* ---- the whole corpus in one `module App = {...}`: a member edit costs
   about one member, not the whole module ---- */
let big_module = () => {
  switch (CorpusUtil.mega_src("mega-mod-1k.hz")) {
  | None => fail("mega-mod-1k.hz unreadable")
  | Some(src) =>
    let seg = parse_mod(src);
    let inner = MakeTerm.go_mod_root(seg).term; /* Module(items) */
    let app = Mod.fresh(ModuleMod(MPat.fresh(Var("App")), inner));
    let term = Exp.fresh(Module([app]));
    let t0 = Sys.time();
    let ds0 = DefStatics.calc(~settings, term);
    let cold = (Sys.time() -. t0) *. 1000.0;
    let (mono_map, _) = Statics.mk_unmemoized(settings, ctx0, term);
    check(
      Alcotest.list(string),
      "big-module error-id parity",
      sorted_ids(Statics.Map.error_ids(mono_map)),
      sorted_ids(DefStatics.all_error_ids(ds0)),
    );
    let (seg2, edited) = edit_seg(~needle="180", ~repl="181", seg);
    check(bool, "edit found the literal", true, edited);
    let inner2 = MakeTerm.go_mod_root(seg2).term;
    let app2 =
      IdTagged.fast_copy(
        Mod.rep_id(app),
        Mod.fresh(
          ModuleMod(
            IdTagged.fast_copy(
              MPat.rep_id(
                switch (app.term) {
                | ModuleMod(mp, _) => mp
                | _ => failwith("app shape")
                },
              ),
              MPat.fresh(Var("App")),
            ),
            inner2,
          ),
        ),
      );
    let term2 =
      IdTagged.fast_copy(Exp.rep_id(term), Exp.fresh(Module([app2])));
    let t1 = Sys.time();
    let ds1 = DefStatics.calc(~settings, ~prev=ds0, term2);
    let incr = (Sys.time() -. t1) *. 1000.0;
    Printf.printf(
      "BIGMOD cold calc: %.0f ms; member edit: %.0f ms, %d analyzed\n",
      cold,
      incr,
      DefStatics.last_analyzed^,
    );
    /* depth 2: App item + WateringTimer member + its format member */
    check(int, "3 analyzed at depth 2", 3, DefStatics.last_analyzed^);
    check(bool, "member edit under half of cold", true, incr < cold /. 2.0);
    ignore(ds1);
  };
};

/* the spine root's elab_term is the whole suffix: incremental eval reuses
   a node whose elab_term is unchanged, so a hollow root would hide edits */
let spine_elab_is_whole_suffix = () => {
  let parse_exp = (src: string): Exp.t =>
    switch (FastParse.of_text(~root=Exp, src)) {
    | Some(seg) => MakeTerm.Incr.term_of(seg)
    | None => fail("exp parse failed")
    };
  let t0 = parse_exp("let junk = 1 in\nlet x = 2 in\nx");
  let ds0 = DefStatics.calc(~settings, t0);
  let top_elab = (ds: DefStatics.t, t: Exp.t) =>
    switch (
      Statics.Map.lookup_exp(Exp.rep_id(DefStatics.strip(t)), ds.merged)
    ) {
    | Some({elab_term, _}) => elab_term
    | None => fail("no root info")
    };
  let whole0 =
    switch (DefStatics.whole_elab(ds0)) {
    | Some(e) => e
    | None => fail("whole_elab")
    };
  check(
    bool,
    "root elab_term is the whole program's elab",
    true,
    Exp.fast_equal(top_elab(ds0, t0), whole0),
  );
  let t1 = parse_exp("let junk = 1 in\nlet x = 3 in\nx");
  let ds1 = DefStatics.calc(~settings, ~prev=ds0, t1);
  check(
    bool,
    "editing item 2 changes the root's elab_term",
    false,
    Exp.fast_equal(top_elab(ds0, t0), top_elab(ds1, t1)),
  );
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
    test_case("statics parity", `Quick, statics_parity),
    test_case("statics incremental", `Quick, statics_incremental),
    test_case(
      "spine elab_term is the whole suffix",
      `Quick,
      spine_elab_is_whole_suffix,
    ),
    test_case("corpus mega-mod-1k", `Quick, corpus),
    test_case("big module (stage D)", `Quick, big_module),
    test_case("recursive type export", `Quick, recursive_type_export),
  ],
);
