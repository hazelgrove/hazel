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

/* the statics path (term_of) grafts the same item parses */
let check_term_of = (name: string, seg: Segment.t): unit => {
  MakeTerm.Incr.last := None;
  let fb = MakeTerm.Incr.fell_back^;
  let term = MakeTerm.Incr.term_of(seg);
  check(int, name ++ ":term_of no fallback", fb, MakeTerm.Incr.fell_back^);
  check(bool, name ++ ":term_of", true, term == MakeTerm.go(seg).term);
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
  check_term_of(file, seg);
  ignore(MakeTerm.Incr.term_of(List.map(p => p, seg)));
  check(
    int,
    file ++ ": term_of a fresh list of the same pieces parses nothing",
    0,
    MakeTerm.Incr.analyzed^,
  );
  let (edited, found) =
    CorpusUtil.edit_token(
      ~at=`Last,
      ~keep_id=true,
      ~needle="9",
      ~repl="8",
      seg,
    );
  check(bool, file ++ ": a 9 to edit", true, found);
  check_term_of(file ++ " edited", edited);
  check(
    int,
    file ++ ": term_of re-parses the edited item and its modules' shells",
    CorpusUtil.parses_for(edited, CorpusUtil.last_tile("8", edited)),
    MakeTerm.Incr.analyzed^,
  );
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

/* the statics path takes an editor's item parses (its go_incr cache):
   after the syntax build parsed an edit, building the term parses
   nothing, and gives the unshared term */
let shared_case = ((root, file), ()) => {
  let seg = CorpusUtil.mega_seg(~root, file);
  let (edited, found) =
    CorpusUtil.edit_token(
      ~at=`First,
      ~keep_id=true,
      ~needle="15",
      ~repl="16",
      seg,
    );
  check(bool, file ++ ": a 15 to edit", true, found);
  let cache = MakeTerm.Incr.mk_cache();
  ignore(MakeTerm.Incr.go_incr(~root, ~cache, seg));
  ignore(MakeTerm.Incr.go_incr(~root, ~cache, edited));
  MakeTerm.Incr.last := None;
  let fb = MakeTerm.Incr.fell_back^;
  let term = MakeTerm.Incr.term_of_root(~root, ~shared=cache, edited);
  check(int, file ++ ": shared parses nothing", 0, MakeTerm.Incr.analyzed^);
  check(int, file ++ ": no fallback", fb, MakeTerm.Incr.fell_back^);
  MakeTerm.Incr.last := None;
  MakeTerm.Incr.term_cache := None;
  let unshared = MakeTerm.Incr.term_of_root(~root, edited);
  check(bool, file ++ ": the unshared term", true, term == unshared);
  /* a cache built at another root goes unused */
  MakeTerm.Incr.last := None;
  MakeTerm.Incr.term_cache := None;
  let other = root == Sort.Mod ? Sort.Exp : Sort.Mod;
  let other_cache = MakeTerm.Incr.mk_cache();
  ignore(MakeTerm.Incr.go_incr(~root=other, ~cache=other_cache, edited));
  MakeTerm.Incr.last := None;
  ignore(MakeTerm.Incr.term_of_root(~root, ~shared=other_cache, edited));
  check(
    bool,
    file ++ ": no reuse across roots",
    true,
    MakeTerm.Incr.analyzed^ > 0,
  );
};

/* with shards missing, semantics parses the completed items itself and
   takes the cache's parses of the rest: the monolithic term */
let shared_incomplete = ((name, src), ()) =>
  switch (CorpusUtil.typed_seg(src)) {
  | None => fail("untypeable program: " ++ name)
  | Some(seg) =>
    let cache = MakeTerm.Incr.mk_cache();
    ignore(MakeTerm.Incr.go_incr(~cache, seg));
    let (completed, masks) = MakeTerm.semantic_segment(~root=Exp, seg);
    MakeTerm.Incr.last := None;
    MakeTerm.Incr.term_cache := None;
    let term =
      MakeTerm.Incr.term_of_root(~masks, ~root=Exp, ~shared=cache, completed);
    check(
      bool,
      name ++ ": the monolithic term",
      true,
      term == MakeTerm.go_impl(~masks, completed).term,
    );
  };

/* a member edit inside a module, at depth 1 and 2: the member and its
   modules' shells parse, nothing else, and the record is go's */
let nested_src = "module A = {\n  let a1 = 11;\n  module B = {\n    let b1 = 21;\n    let b2 = fun x ->\n      let y = x + 22 in\n      y\n  };\n  let m = { let c = 31 };\n  let a2 = 12;\n  let e = 61 in\n  e + 62\n} in\nlet top = 41 in\nA.a1 + top";

let member_edit = ((needle, repl), ()) => {
  let seg = CorpusUtil.seg_of(nested_src);
  let cache = check_parity("nested", seg);
  let (edited, found) =
    CorpusUtil.edit_token(~keep_id=true, ~needle, ~repl, seg);
  check(bool, needle ++ " to edit", true, found);
  let fb = MakeTerm.Incr.fell_back^;
  let a0 = MakeTerm.Incr.full_analyzed^;
  let r = MakeTerm.Incr.go_incr(~cache, edited);
  check(
    int,
    needle ++ ": its member and its modules' shells",
    CorpusUtil.parses_for(edited, CorpusUtil.tile(repl, edited)),
    MakeTerm.Incr.full_analyzed^ - a0,
  );
  check(int, needle ++ ": no fallback", fb, MakeTerm.Incr.fell_back^);
  records_agree(needle ++ " edited", MakeTerm.go(edited), r);
  /* the statics path parses nothing after it */
  MakeTerm.Incr.last := None;
  MakeTerm.Incr.term_cache := None;
  let term = MakeTerm.Incr.term_of_root(~root=Exp, ~shared=cache, edited);
  check(int, needle ++ ": statics shares", 0, MakeTerm.Incr.analyzed^);
  check(bool, needle ++ ": the term", true, term == r.term);
  /* under a new id: nothing of the old token's survives the union */
  let (renewed, _) =
    CorpusUtil.edit_token(~needle=repl, ~repl=needle, edited);
  records_agree(
    needle ++ " renewed",
    MakeTerm.go(renewed),
    MakeTerm.Incr.go_incr(~cache, renewed),
  );
};

/* a parse makes its singleton labeled tuple's ids from its pieces, so two
   parses of the same pieces agree */
let labeled_singletons = () => {
  let seg =
    CorpusUtil.seg_of(
      "let t = (a=1) in\nlet f = fun (b=x) -> x in\nlet ty : (c=Int) = (c=2) in\n[d=3]",
    );
  check(
    bool,
    "two parses agree",
    true,
    MakeTerm.go_impl(seg).term == MakeTerm.go_impl(seg).term,
  );
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
    test_case("a member edit at depth 1", `Quick, member_edit(("11", "13"))),
    test_case("a member edit at depth 2", `Quick, member_edit(("21", "23"))),
    test_case(
      "an edit in a depth-2 member's let-in",
      `Quick,
      member_edit(("22", "24")),
    ),
    test_case(
      "an edit in a let-bound module's member",
      `Quick,
      member_edit(("31", "33")),
    ),
    test_case(
      "an edit after the modules",
      `Quick,
      member_edit(("41", "43")),
    ),
    test_case(
      "a let-chain member's item",
      `Quick,
      member_edit(("61", "63")),
    ),
    test_case(
      "a let-chain member's tail",
      `Quick,
      member_edit(("62", "64")),
    ),
    test_case("labeled singletons", `Quick, labeled_singletons),
    test_case(
      "mega-1k: statics shares the syntax parses",
      `Quick,
      shared_case((Exp, "mega-1k.hz")),
    ),
    test_case(
      "mega-mod-1k: statics shares the syntax parses",
      `Quick,
      shared_case((Mod, "mega-mod-1k.hz")),
    ),
  ]
  @ List.map(
      ((name, src)) =>
        test_case(
          "shared, incomplete: " ++ name,
          `Quick,
          shared_incomplete((name, src)),
        ),
      CorpusUtil.incomplete_programs,
    ),
);
