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
  let slices = MakeTerm.Incr.slices(~root=Mod, seg);
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

/* the top level's separators: its `;`s, and grouts standing in for them */
let sep_ids = (seg: Segment.t): list(Id.t) =>
  semi_ids(seg)
  @ List.filter_map(
      (p: Piece.t) =>
        switch (p) {
        | Grout({shape: Concave, id}) => Some(id)
        | _ => None
        },
      seg,
    );

/* go_incr's record is go_mod_root's, except that top-level separator
   entries in terms may differ: the per-slice parse records a partial
   MultiHole there, the monolithic one the full item list */
let check_parity = (seg: Segment.t, incr: MakeTerm.t) => {
  let mono = MakeTerm.go_mod_root(seg);
  if (compare(incr.term, mono.term) != 0) {
    fail("term mismatch incr vs mono");
  };
  let agree = (~skip=_ => false, what, a, b) => {
    let diff =
      Id.Map.merge(
        (id, a, b) =>
          switch (a, b) {
          | (Some(a), Some(b)) =>
            compare(a, b) == 0 || skip(id) ? None : Some("value")
          | (Some(_), None) => Some("incr-only")
          | (None, Some(_)) => Some("mono-only")
          | (None, None) => None
          },
        a,
        b,
      );
    if (!Id.Map.is_empty(diff)) {
      fail(
        what
        ++ " mismatch at "
        ++ string_of_int(Id.Map.cardinal(diff))
        ++ " ids, e.g. "
        ++ String.concat(
             ", ",
             Id.Map.bindings(diff)
             |> Util.ListUtil.take(5)
             |> List.map(((id, why)) => Id.to_string(id) ++ " " ++ why),
           ),
      );
    };
  };
  agree("term_data", incr.term_data, mono.term_data);
  let seps = sep_ids(seg);
  agree(~skip=id => List.mem(id, seps), "terms", incr.terms, mono.terms);
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
  check(
    int,
    "the member and its module's shell reparsed",
    2,
    MakeTerm.Incr.full_analyzed^,
  );
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
  check(
    int,
    "the member and its module's shell reparsed",
    CorpusUtil.parses_for(~root=Mod, seg2, CorpusUtil.tile("181", seg2)),
    MakeTerm.Incr.full_analyzed^,
  );
  check_parity(seg2, incr2);
};

/* a member opening with a let-in ends at its `;`, so it parses alone: no
   whole-program fallback, before or after an edit inside it (the misread
   cut after its `in`) */
let letin_member = ((name, src), ()) => {
  let seg = parse_mod(src);
  let cache = MakeTerm.Incr.mk_cache();
  let fb = MakeTerm.Incr.fell_back^;
  let incr = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg);
  check(int, name ++ ": no fallback", fb, MakeTerm.Incr.fell_back^);
  check_parity(seg, incr);
  MakeTerm.Incr.last := None;
  ignore(MakeTerm.Incr.term_of_root(~root=Mod, seg));
  check(int, name ++ ": term_of no fallback", fb, MakeTerm.Incr.fell_back^);
  let (seg2, edited) = edit_seg(~needle="1", ~repl="2", seg);
  check(bool, name ++ ": a 1 to edit", true, edited);
  MakeTerm.Incr.full_analyzed := 0;
  let incr2 = MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg2);
  check(
    int,
    name ++ ": its item reparsed, and its let chain's shell",
    CorpusUtil.parses_for(~root=Mod, seg2, CorpusUtil.tile("2", seg2)),
    MakeTerm.Incr.full_analyzed^,
  );
  check(int, name ++ ": no fallback after", fb, MakeTerm.Incr.fell_back^);
  check_parity(seg2, incr2);
};

/* a module body whose `;` became grout: the grout separates the members
   as the `;` did (a Mod-sorted skel), though secondary collection reads
   the body Exp-sorted and the parse takes its skels */
let grout_between_members = () => {
  let rec ungrout = (seg: Segment.t): Segment.t =>
    List.map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when Tile.is_semi(t) =>
          Piece.Grout({
            id: t.id,
            shape: Concave,
          })
        | Tile(t) =>
          Tile({
            ...t,
            children: List.map(ungrout, t.children),
          })
        | p => p
        },
      seg,
    );
  let seg =
    ungrout(
      CorpusUtil.seg_of("module M = {\n  let x = 1;\n  let y = 2\n} in\nM.x"),
    );
  let members = (e: Exp.t) =>
    switch (e.term) {
    | ModuleExp(_, {term: Module(items), _}, _) =>
      List.filter(
        (m: Mod.t) =>
          switch (m.term) {
          | ModLet(_) => true
          | _ => false
          },
        items,
      )
      |> List.length
    | _ => (-1)
    };
  check(int, "two members", 2, members(MakeTerm.go_impl(seg).term));
};

/* a member edit inside a module, at depth 1 and 2, at a Mod root */
let nested_src = "module A = {\n  let a1 = 11;\n  module B = {\n    let b1 = 21;\n    let b2 = fun x ->\n      let y = x + 22 in\n      y\n  };\n  let a2 = 12\n};\nlet top = 41;\nA.a1 + top";

let member_edit = ((needle, repl), ()) => {
  let seg = parse_mod(nested_src);
  let cache = MakeTerm.Incr.mk_cache();
  let fb = MakeTerm.Incr.fell_back^;
  check_parity(seg, MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg));
  let (edited, found) =
    CorpusUtil.edit_token(~keep_id=true, ~needle, ~repl, seg);
  check(bool, needle ++ " to edit", true, found);
  let a0 = MakeTerm.Incr.full_analyzed^;
  let r = MakeTerm.Incr.go_incr(~root=Mod, ~cache, edited);
  check(
    int,
    needle ++ ": its member and its modules' shells",
    CorpusUtil.parses_for(~root=Mod, edited, CorpusUtil.tile(repl, edited)),
    MakeTerm.Incr.full_analyzed^ - a0,
  );
  check(int, needle ++ ": no fallback", fb, MakeTerm.Incr.fell_back^);
  check_parity(edited, r);
};

/* members that are let chains: each chain item parses alone */
let chain_src = "module H = {\n  let a = 1 in\n  let b = a + 51 in\n  module N = {\n    let c = 52;\n    let d = 53 in\n    d + c\n  } in\n  a + b + N.c + 54\n};\nlet x = 55 in\nx + 56;\nH";

let chain_edit = ((needle, repl), ()) => {
  let seg = parse_mod(chain_src);
  let cache = MakeTerm.Incr.mk_cache();
  let fb = MakeTerm.Incr.fell_back^;
  check_parity(seg, MakeTerm.Incr.go_incr(~root=Mod, ~cache, seg));
  let (edited, found) =
    CorpusUtil.edit_token(~keep_id=true, ~needle, ~repl, seg);
  check(bool, needle ++ " to edit", true, found);
  let a0 = MakeTerm.Incr.full_analyzed^;
  let r = MakeTerm.Incr.go_incr(~root=Mod, ~cache, edited);
  check(
    int,
    needle ++ ": its item and the shells around it",
    CorpusUtil.parses_for(~root=Mod, edited, CorpusUtil.tile(repl, edited)),
    MakeTerm.Incr.full_analyzed^ - a0,
  );
  check(int, needle ++ ": no fallback", fb, MakeTerm.Incr.fell_back^);
  check_parity(edited, r);
  let (renewed, _) =
    CorpusUtil.edit_token(~needle=repl, ~repl=needle, edited);
  check_parity(renewed, MakeTerm.Incr.go_incr(~root=Mod, ~cache, renewed));
};

/* the stdlib's sources, where the repo has them */
let stdlib = () =>
  List.iter(
    ((path, src)) => {
      let seg = parse_mod(src);
      let fb = MakeTerm.Incr.fell_back^;
      check_parity(
        seg,
        MakeTerm.Incr.go_incr(
          ~root=Mod,
          ~cache=MakeTerm.Incr.mk_cache(),
          seg,
        ),
      );
      check(int, path ++ ": no fallback", fb, MakeTerm.Incr.fell_back^);
    },
    Test_ItemSpans.stdlib_corpus(),
  );

/* a `;` between two finished members deleted as the editor deletes it
   (a grout stands in): while it is missing, keys typed in one of the pair
   or in another member parse that member's item and its modules' shells,
   never a module whole, the parse is go's, and every outline row keeps
   its id */
let missing_semi = ((root: Sort.t, src, semi, keys), ()) => {
  module MA = Test_ModAudit;
  let z0 = MA.load(~root, src);
  let statics = MA.statics_of(~root, z0);
  let step = (z, a) =>
    switch (
      MA.perform_why(
        ~root,
        ~syntax=CachedSyntax.init(~root, z),
        ~statics,
        z,
        a,
      )
    ) {
    | Ok(z) => z
    | Error(why) => fail(why)
    };
  let at = (needle: string) =>
    switch (MA.point_of(~off=String.length(needle), src, needle)) {
    | Some(pt) => pt
    | None => fail("no " ++ needle)
    };
  let z =
    step(
      step(z0, Move(Point(at(semi), None))),
      Destruct(Local(Left, ByChar)),
    );
  let cache = MakeTerm.Incr.mk_cache();
  /* the editor's frames: the syntax's parse, then semantics' */
  let read = (z: Zipper.t) => {
    let seg = Zipper.unselect_and_zip(z);
    let fb = MakeTerm.Incr.fell_back^;
    let a0 = MakeTerm.Incr.full_analyzed^;
    let r = MakeTerm.Incr.go_incr(~root, ~cache, seg);
    let parsed = MakeTerm.Incr.full_analyzed^ - a0;
    let (completed, masks) = MakeTerm.semantic_segment(~root, seg);
    let term =
      MakeTerm.Incr.term_of_root(~masks, ~root, ~shared=cache, completed);
    check(int, "no fallback", fb, MakeTerm.Incr.fell_back^);
    if (root == Sort.Mod) {
      check_parity(seg, r);
    } else {
      Test_MakeTermIncr.records_agree("go", MakeTerm.go(seg), r);
    };
    (seg, parsed, MA.rows_of(term));
  };
  let (seg, _, rows) = read(z);
  let rec grouted = (seg: Segment.t) =>
    List.exists(
      (p: Piece.t) =>
        switch (p) {
        | Grout({shape: Concave, _}) => true
        | Tile(t) => List.exists(grouted, t.children)
        | _ => false
        },
      seg,
    );
  check(bool, "a grout stands in for the `;`", true, grouted(seg));
  ignore(
    List.fold_left(
      (z, (needle, key, typed)) => {
        /* a move can rebuild the pieces it crosses: read once before */
        let z = step(z, Move(Point(at(needle), None)));
        ignore(read(z));
        let z = step(z, Insert(key));
        let (seg, parsed, rows') = read(z);
        check(
          int,
          typed ++ ": its item and its modules' shells",
          CorpusUtil.parses_for(~root, seg, CorpusUtil.tile(typed, seg)),
          parsed,
        );
        check(
          list(pair(string, string)),
          typed ++ ": every row keeps its id",
          List.map(((k, id)) => (k, Id.to_string(id)), rows),
          List.map(((k, id)) => (k, Id.to_string(id)), rows'),
        );
        z;
      },
      z,
      keys,
    ),
  );
};

let missing_semi_cases = {
  let lit = "module M = {\n  let a = 1;\n  let b = a + 2;\n  test 1 == 1 end;\n  let c = b + 3\n} in\nM.c";
  let top = "let a = 1;\nlet b = a + 2;\ntest 1 == 1 end;\nlet c = b + 3;\nc";
  /* a key in each of the pair, then one in another member */
  [
    (
      "between two definitions, in a literal",
      (
        Sort.Exp,
        lit,
        "let a = 1;",
        [
          ("let a = 1", "5", "15"),
          ("let b = a + 2", "6", "26"),
          ("let c = b + 3", "7", "37"),
        ],
      ),
    ),
    (
      "after a test, in a literal",
      (
        Sort.Exp,
        lit,
        "test 1 == 1 end;",
        [("let c = b + 3", "7", "37"), ("let a = 1", "5", "15")],
      ),
    ),
    (
      "after a test, at a Mod root",
      (
        Sort.Mod,
        top,
        "test 1 == 1 end;",
        [("let c = b + 3", "7", "37"), ("let a = 1", "5", "15")],
      ),
    ),
    (
      "between two definitions, in a literal at a Mod root",
      (
        Sort.Mod,
        "module M = {\n  let a = 1;\n  let b = a + 2;\n  let c = b + 3\n};\nM.c",
        "let a = 1;",
        [("let b = a + 2", "6", "26"), ("let c = b + 3", "7", "37")],
      ),
    ),
  ];
};

let tests = (
  "ModRoot",
  [
    test_case("probe", `Quick, probe),
    test_case("grout between members", `Quick, grout_between_members),
    test_case("full parity", `Quick, full_parity),
    test_case("incremental edit", `Quick, incremental_edit),
    test_case("term_of_mod", `Quick, term_of_mod_matches),
    test_case("stray line above a member", `Quick, stray_line),
    test_case("last slot keyed by root", `Quick, last_slot_keyed_by_root),
    test_case("corpus mega-mod-1k", `Quick, corpus),
    test_case("a member edit at depth 1", `Quick, member_edit(("11", "13"))),
    test_case("a member edit at depth 2", `Quick, member_edit(("21", "23"))),
    test_case(
      "an edit in a depth-2 member's let-in",
      `Quick,
      member_edit(("22", "24")),
    ),
    test_case("an edit after the module", `Quick, member_edit(("41", "43"))),
    test_case("stdlib sources", `Quick, stdlib),
    test_case("a chain's middle item", `Quick, chain_edit(("51", "57"))),
    test_case(
      "a member of a module in a chain",
      `Quick,
      chain_edit(("52", "58")),
    ),
    test_case(
      "a chain in a module in a chain",
      `Quick,
      chain_edit(("53", "59")),
    ),
    test_case("a chain's tail", `Quick, chain_edit(("54", "60"))),
    test_case("a chain at the root", `Quick, chain_edit(("55", "61"))),
    test_case(
      "a chain's tail at the root",
      `Quick,
      chain_edit(("56", "62")),
    ),
  ]
  @ List.map(
      ((name, case)) =>
        test_case("a missing `;`: " ++ name, `Quick, missing_semi(case)),
      missing_semi_cases,
    )
  @ List.map(
      ((name, src)) =>
        test_case(
          "let-in member: " ++ name,
          `Quick,
          letin_member((name, src)),
        ),
      [
        ("let-in member", "let f = let x = 1 in x;\nf"),
        (
          "a fun's let-ins",
          "let g = fun y ->\n  let z = y + 1 in\n  let w = z in\n  w;\ng(0)",
        ),
        (
          "a statement opening with a let-in",
          "let x = 1 in x;\nlet y = 2;\ny",
        ),
        /* the shape of List's enumerate */
        (
          "enumerate",
          "let enumerate : [?] -> [(Int, ?)] = fun xs ->\n  let helper = fun (xs, index) ->\n    case xs\n    | [] => []\n    | x :: xs => (index, x) :: helper(xs, index + 1)\n    end\n  in\n  helper(xs, 0);\nenumerate([2])",
        ),
      ],
    ),
);
