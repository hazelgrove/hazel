open Alcotest;
open Haz3lcore;
open Language;

/* Exact-parity gate for the incremental full parse (MakeTerm.Incr.
   go_incr): on the mega corpus and structure edge cases, the per-item
   composed record must equal MakeTerm.go's on every field, the memo
   must localize reparses, and the exception fallback must not fire.
     bash test/run_node.sh test 'MakeTermIncr' */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | ic =>
    let n = in_channel_length(ic);
    let s = really_input_string(ic, n);
    close_in(ic);
    Some(s);
  | exception _ => None
  };

let corpus_seg = (name: string): option(Segment.t) => {
  let path = "hazel-programs/mega/" ++ name;
  let path = Sys.file_exists(path) ? path : "../hazel-programs/mega/" ++ name;
  Option.bind(read_file(path), src =>
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  );
};

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

let copy_piece = (p: Piece.t): Piece.t =>
  switch (p) {
  | Tile(t) =>
    Tile({
      ...t,
      id: t.id,
    })
  | Grout(g) =>
    Grout({
      ...g,
      id: g.id,
    })
  | Secondary(w) =>
    Secondary({
      ...w,
      id: w.id,
    })
  | Projector(pr) =>
    Projector({
      ...pr,
      id: pr.id,
    })
  };

let corpus_case = (file: string, ()) =>
  switch (corpus_seg(file)) {
  | None => fail("corpus unreadable/unparseable: " ++ file)
  | Some(seg) =>
    let cache = check_parity(file, seg);
    /* identical rebuild: nothing reparses */
    let a0 = MakeTerm.Incr.full_analyzed^;
    let _ = MakeTerm.Incr.go_incr(~cache, seg);
    check(
      int,
      file ++ ": stable rebuild parses nothing",
      a0,
      MakeTerm.Incr.full_analyzed^,
    );
    /* localized change: one item reparses, record stays exact */
    let n = List.length(seg);
    let seg' = List.mapi((i, p) => i == n / 2 ? copy_piece(p) : p, seg);
    let a1 = MakeTerm.Incr.full_analyzed^;
    let incr_r = MakeTerm.Incr.go_incr(~cache, seg');
    let reparsed = MakeTerm.Incr.full_analyzed^ - a1;
    check(bool, file ++ ": localized reparse", true, reparsed <= 1);
    records_agree(file ++ ":after edit", MakeTerm.go(seg'), incr_r);
  };

/* structure edge cases: sequencing, aliases, adoption forms (lists,
   case), top-level operator trees in the tail, comments/blank lines */
let edge_programs = [
  ("two defs", "let a = 1 in\nlet b = 2 in\na + b"),
  ("tail op tree", "let a = 1 in\na + 2 * 3 - 4"),
  ("type alias", "type t = Int in\nlet x: t = 1 in\nx"),
  ("seq semis", "let f = fun x -> x in\nf(1); f(2); f(3)"),
  ("list adoption", "let xs = [1,\n2, 3] in\nxs"),
  (
    "case adoption",
    "let f = fun x ->\ncase x\n| 1 => 2\n| _ => 3\nend in\nf(0)",
  ),
  ("comments", "let a = 1 in\n# note #\nlet b = 2 in\na + b"),
  ("blank lines", "let a = 1 in\n\n\nlet b = 2 in\nb"),
  ("single expr", "1 + 2 * 3"),
  ("trailing lb", "let a = 1 in\na\n"),
  ("tuple top", "let t = (1, 2) in\nt"),
];

let edge_case = ((name, src), ()) =>
  switch (ParsedCorpus.to_segment(~root=Exp, src)) {
  | None => fail("unparseable edge program: " ++ name)
  | Some(seg) => ignore(check_parity(name, seg))
  };

let tests = (
  "MakeTermIncr",
  List.map(
    ((name, src)) => test_case(name, `Quick, edge_case((name, src))),
    edge_programs,
  )
  @ [
    test_case("mega-1k parity", `Quick, corpus_case("mega-1k.hz")),
    test_case("mega-2k parity", `Quick, corpus_case("mega-2k.hz")),
    test_case("mega-4k parity", `Quick, corpus_case("mega-4k.hz")),
  ],
);
