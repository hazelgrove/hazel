open Alcotest;
open Haz3lcore;
open Language;

/* MakeTerm.Incr.go_incr's record equals MakeTerm.go's on every field,
   reparses only changed items, and never falls back */

let records_agree = (name: string, a: MakeTerm.t, b: MakeTerm.t): unit => {
  check(bool, name ++ ":term", true, a.term == b.term);
  check(bool, name ++ ":terms", true, Id.Map.equal((==), a.terms, b.terms));
  check(
    bool,
    name ++ ":term_data",
    true,
    Id.Map.equal((==), a.term_data, b.term_data),
  );
  check(
    bool,
    name ++ ":projectors",
    true,
    Id.Map.equal((==), a.projectors, b.projectors),
  );
  check(
    bool,
    name ++ ":projector_list",
    true,
    a.projector_list == b.projector_list,
  );
};

let check_parity = (name: string, seg: Segment.t): MakeTerm.Incr.cache => {
  let cache = MakeTerm.Incr.mk_cache();
  let fb = MakeTerm.Incr.fell_back^;
  let incr_r = MakeTerm.Incr.go_incr(~cache, seg);
  check(int, name ++ ":no fallback", fb, MakeTerm.Incr.fell_back^);
  records_agree(name, MakeTerm.go(seg), incr_r);
  cache;
};

let corpus_case = (file: string, ()) => {
  let seg = CorpusUtil.mega_seg(file);
  let cache = check_parity(file, seg);
  let a0 = MakeTerm.Incr.full_analyzed^;
  let _ = MakeTerm.Incr.go_incr(~cache, seg);
  check(
    int,
    file ++ ": stable rebuild parses nothing",
    a0,
    MakeTerm.Incr.full_analyzed^,
  );
  /* a physically fresh copy of one piece: one item reparses */
  let n = List.length(seg);
  let seg' =
    List.mapi((i, p) => i == n / 2 ? CorpusUtil.copy_piece(p) : p, seg);
  let a1 = MakeTerm.Incr.full_analyzed^;
  let incr_r = MakeTerm.Incr.go_incr(~cache, seg');
  let reparsed = MakeTerm.Incr.full_analyzed^ - a1;
  check(bool, file ++ ": localized reparse", true, reparsed <= 1);
  records_agree(file ++ ":after edit", MakeTerm.go(seg'), incr_r);
};

/* the statics path (term_of) grafts the same item parses */
let check_term_of = (name: string, seg: Segment.t): unit => {
  MakeTerm.Incr.last := None;
  let fb = MakeTerm.Incr.fell_back^;
  let term = MakeTerm.Incr.term_of(seg);
  check(int, name ++ ":term_of no fallback", fb, MakeTerm.Incr.fell_back^);
  check(bool, name ++ ":term_of", true, term == MakeTerm.go(seg).term);
};

let edge_case = ((name, src), ()) => {
  let seg = CorpusUtil.seg_of(src);
  ignore(check_parity(name, seg));
  check_term_of(name, seg);
};

let incomplete_case = ((name, src), ()) =>
  switch (CorpusUtil.typed_seg(src)) {
  | None => fail("untypeable program: " ++ name)
  | Some(seg) =>
    ignore(check_parity(name, seg));
    check_term_of(name, seg);
  };

/* shard masks from one parse don't leak into the next */
let independent = () =>
  switch (CorpusUtil.seg_of("let x = 1 in\nx")) {
  | [Tile(t), ..._] as seg =>
    let fresh = () => MakeTerm.Incr.go_incr(~cache=MakeTerm.Incr.mk_cache());
    let before = fresh((), seg);
    let masks =
      Id.Map.singleton(
        t.id,
        IdTagged.IdTag.{
          present: [0],
          prefixes: [],
        },
      );
    ignore(MakeTerm.go_impl(~masks, seg));
    records_agree("after a masked parse", before, fresh((), seg));
  | _ => fail("no leading tile")
  };

let tests = (
  "MakeTermIncr",
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
    test_case("mega-1k parity", `Quick, corpus_case("mega-1k.hz")),
    test_case("mega-2k parity", `Quick, corpus_case("mega-2k.hz")),
    test_case("mega-4k parity", `Quick, corpus_case("mega-4k.hz")),
    test_case("independent of the previous parse", `Quick, independent),
  ],
);
