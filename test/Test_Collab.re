open Alcotest;
open Haz3lcore;

/* Collaborative-editing bridge (ScratchCollab, docs/collab-modular.md):
   program <-> items with text leaves, single-leaf replacement, and
   caret <-> text offset. Run: bash test/run_node.sh test 'Collab' */

module C = Web.ScratchCollab;

let parse = (src: string): Segment.t => C.parse(~root=Exp, src);

let tail_id = Id.mk();

let items_of = (src: string): list(C.item) =>
  switch (C.items_of_seg(~tail_id, parse(src))) {
  | Some(items) => items
  | None => failwith("items_of_seg failed: " ++ src)
  };

let summary = (it: C.item): string =>
  Printf.sprintf(
    "%s|%s|%s|%s",
    C.show_kind(it.kind),
    it.lead,
    it.header,
    it.body,
  );

let src = {|# the answer #
let x : Int = 41 in
type T = (Int, Bool) in
let f = fun y ->
  y + 1
in
test f(x) == 42 end;
f(x)|};

let test_items = () => {
  let items = items_of(src);
  check(
    list(string),
    "items",
    [
      "Def|# the answer #|x : Int|41",
      "Type||T|(Int, Bool)",
      "Def||f|fun y ->\n  y + 1",
      "Stmt|||test f(x) == 42 end",
      "Tail|||f(x)",
    ],
    List.map(summary, items),
  );
};

let test_roundtrip = () => {
  let items = items_of(src);
  switch (C.seg_of_items(items)) {
  | None => fail("seg_of_items failed")
  | Some(seg) =>
    switch (C.items_of_seg(~tail_id, seg)) {
    | None => fail("re-itemizing failed")
    | Some(items') =>
      check(
        list(string),
        "same texts",
        List.map(summary, items),
        List.map(summary, items'),
      );
      check(
        list(string),
        "same ids",
        List.map((it: C.item) => Id.to_string(it.id), items),
        List.map((it: C.item) => Id.to_string(it.id), items'),
      );
    }
  };
};

let test_incomplete_leaves = () => {
  /* leaves that don't parse on their own (holes derived, never stored) */
  let items: list(C.item) = [
    {
      id: Id.mk(),
      kind: Def,
      lead: "",
      header: "x",
      body: "1 +",
    },
    {
      id: Id.mk(),
      kind: Def,
      lead: "",
      header: "",
      body: "",
    },
    {
      id: tail_id,
      kind: Tail,
      lead: "",
      header: "",
      body: "x",
    },
  ];
  switch (C.seg_of_items(items)) {
  | None => fail("seg_of_items failed")
  | Some(seg) =>
    check(
      list(string),
      "texts survive",
      List.map(summary, items),
      List.map(summary, Option.get(C.items_of_seg(~tail_id, seg))),
    )
  };
};

/* half-typed code maps to items too, and round-trips */
let test_total = () => {
  let check_rt = (label, src, expected) => {
    let items = items_of(src);
    check(list(string), label, expected, List.map(summary, items));
    let seg = Option.get(C.seg_of_items(items));
    check(
      list(string),
      label ++ " round-trip",
      expected,
      List.map(summary, Option.get(C.items_of_seg(~tail_id, seg))),
    );
  };
  check_rt(
    "stray in",
    "et foo = 42 in foo + 1",
    ["Tail|||et foo = 42 in foo + 1"],
  );
  check_rt(
    "let being typed between defs",
    "let a = 1 in\nlet b = \nlet c = 3 in\nc",
    ["Def||a|1", "Def|let b =|c|3", "Tail|||c"],
  );
  check_rt(
    "partial let before a statement",
    "let a = 1 in\nlet \ntest a == 1 end;\na",
    ["Def||a|1", "Stmt|||let \ntest a == 1 end", "Tail|||a"],
  );
};

/* top-level content spliced next to its neighbours must be regrouted, or
   term construction (Skel) breaks */
let test_lead_regrout = () => {
  let items = items_of("let foo = 42 in foo + 1");
  let seg = Option.get(C.seg_of_items(items));
  let foo = List.hd(items);
  let seg' = C.set_leaf(Def, foo.id, Lead, "0", seg);
  check(
    option(string),
    "lead",
    Some("0"),
    C.leaf_text(Def, foo.id, Lead, seg'),
  );
  /* MakeTerm must not fall over on the result */
  ignore(MakeTerm.go(seg').term);
  let seg'' =
    Option.get(C.seg_of_items(Option.get(C.items_of_seg(~tail_id, seg'))));
  ignore(MakeTerm.go(seg'').term);
  check(
    list(string),
    "round-trip with a lead",
    ["Def|0|foo|42", "Tail|||foo + 1"],
    List.map(summary, Option.get(C.items_of_seg(~tail_id, seg''))),
  );
};

let test_set_leaf = () => {
  let items = items_of(src);
  let seg = Option.get(C.seg_of_items(items));
  let f = List.nth(items, 2);
  let seg' = C.set_leaf(Def, f.id, Body, "fun y -> y * 2", seg);
  check(
    option(string),
    "new body",
    Some("fun y -> y * 2"),
    C.leaf_text(Def, f.id, Body, seg'),
  );
  let seg'' = C.set_leaf(Def, f.id, Header, "g", seg');
  check(
    option(string),
    "new header",
    Some("g"),
    C.leaf_text(Def, f.id, Header, seg''),
  );
  let stmt = List.nth(items, 3);
  let seg3 = C.set_leaf(Stmt, stmt.id, Body, "test true end", seg'');
  let seg4 = C.set_leaf(Tail, tail_id, Body, "g(1)", seg3);
  let items' = Option.get(C.items_of_seg(~tail_id, seg4));
  check(
    list(string),
    "only the edited leaves changed",
    [
      "Def|# the answer #|x : Int|41",
      "Type||T|(Int, Bool)",
      "Def||g|fun y -> y * 2",
      "Stmt|||test true end",
      "Tail|||g(1)",
    ],
    List.map(summary, items'),
  );
  check(
    list(string),
    "ids kept",
    List.map((it: C.item) => Id.to_string(it.id), items),
    List.map((it: C.item) => Id.to_string(it.id), items'),
  );
};

/* every offset maps to a caret whose offset is that offset */
let check_offsets = (~root=Sort.Exp, text: string) => {
  let seg = C.parse(~root, text);
  let printed = C.text_of_seg(seg);
  let z = Zipper.unzip(~direction=Left, seg);
  let n = C.utf16_length(printed);
  for (off in 0 to n) {
    let z' = C.with_caret_at(off, z);
    check(
      int,
      Printf.sprintf("%S @%d", printed, off),
      off,
      C.caret_offset(z'),
    );
  };
};

let test_caret_offsets = () => {
  check_offsets("let x = 1 in x + 22");
  check_offsets("fun y ->\n  y + 1");
  check_offsets("\"héllo\" ++ \"wörld\"");
  check_offsets("1 +");
  check_offsets("(1, (2, 3))");
  check_offsets("case x | 0 => \"a\" | _ => \"b\" end");
  check_offsets(~root=Pat, "(a, b) : (Int, Int)");
};

/* walking ByChar right visits offsets monotonically, ending at the end */
let test_caret_walk = () => {
  let text = "let foo = bar(1, 22) in\nfoo";
  let seg = parse(text);
  let z = ref(Zipper.unzip(~direction=Left, seg));
  let last = ref(0);
  let continue = ref(true);
  while (continue^) {
    let off = C.caret_offset(z^);
    check(bool, Printf.sprintf("monotone at %d", off), true, off >= last^);
    last := off;
    switch (Move.local(ByChar, Right, z^)) {
    | Some(z') => z := z'
    | None => continue := false
    };
  };
  check(int, "ends at end", C.utf16_length(C.text_of_seg(seg)), last^);
};

let splice =
  testable(
    (fmt, s: C.splice) =>
      Format.fprintf(fmt, "{%d,%d,%S}", s.index, s.delete, s.insert),
    (==),
  );

let test_diff = () => {
  check(option(splice), "same", None, C.diff("abc", "abc"));
  check(
    option(splice),
    "insert",
    Some({
      index: 1,
      delete: 0,
      insert: "X",
    }),
    C.diff("ab", "aXb"),
  );
  check(
    option(splice),
    "delete",
    Some({
      index: 1,
      delete: 2,
      insert: "",
    }),
    C.diff("abcd", "ad"),
  );
  /* UTF-16 offsets, cut on code-point boundaries */
  check(
    option(splice),
    "unicode",
    Some({
      index: 3,
      delete: 1,
      insert: "ö",
    }),
    C.diff("héllo", "hélöo"),
  );
  check(
    option(splice),
    "astral",
    Some({
      index: 3,
      delete: 0,
      insert: "!",
    }),
    C.diff("a😀b", "a😀!b"),
  );
};

let test_corpus_roundtrip = () => {
  /* production normalization only: the suite-wide parity check reruns
     the global pass on every remold, which is quadratic-ish here */
  let parity = Zipper.normalize_parity^;
  Zipper.normalize_parity := false;
  switch (CorpusUtil.read_file("hazel-programs/bench/bench-1k.hz")) {
  | None => () /* corpus not reachable from this cwd */
  | Some(text) =>
    switch (C.items_of_seg(~tail_id, parse(text))) {
    | None => fail("bench-1k: not an item chain")
    | Some(items) =>
      let seg = Option.get(C.seg_of_items(items));
      let items' = Option.get(C.items_of_seg(~tail_id, seg));
      check(int, "item count", List.length(items), List.length(items'));
      List.iter2(
        (a: C.item, b: C.item) =>
          check(string, "item " ++ a.header, summary(a), summary(b)),
        items,
        items',
      );
    }
  };
  Zipper.normalize_parity := parity;
};

let tests = (
  "Collab",
  [
    test_case("items of a program", `Quick, test_items),
    test_case("items round-trip", `Quick, test_roundtrip),
    test_case("incomplete leaves", `Quick, test_incomplete_leaves),
    test_case("half-typed programs", `Quick, test_total),
    test_case("lead splice regrouts", `Quick, test_lead_regrout),
    test_case("set one leaf", `Quick, test_set_leaf),
    test_case("caret <-> offset", `Quick, test_caret_offsets),
    test_case("caret walk is monotone", `Quick, test_caret_walk),
    test_case("text diff", `Quick, test_diff),
    test_case("corpus round-trip", `Slow, test_corpus_roundtrip),
  ],
);
