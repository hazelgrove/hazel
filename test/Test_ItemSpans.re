open Alcotest;
open Haz3lcore;
open Poly;

/* ItemSpans at every level of the corpora (module members and definition
   bodies included): slices partition each level, base_col is the line's
   indentation, the top level cuts where Segment.top_items does, and find
   reaches every span. Spans, cores and slots are checked directly on
   hand-written programs */

module S = ItemSpans;

/* the stdlib's sources (the world tree's top-level files), module bodies
   (Mod-rooted), where the repo has them */
let stdlib_corpus = (): list((string, string)) => {
  let dir =
    Stdlib.Sys.file_exists("hazel-programs/world")
      ? "hazel-programs/world" : "../hazel-programs/world";
  switch (Stdlib.Sys.readdir(dir)) {
  | files =>
    files
    |> Array.to_list
    |> List.filter(~f=f => Filename.check_suffix(f, ".hz"))
    |> List.sort(~compare)
    |> List.filter_map(~f=f => {
         let path = Filename.concat(dir, f);
         Option.map(~f=src => (path, src), CorpusUtil.read_file(path));
       })
  | exception _ => []
  };
};

/* Mod-rooted programs whose members open with a let-in, as List's
   enumerate and merge_sort do: a member ends at its `;`, never after an
   `in` */
let letin_members = [
  ("let-in member", "let f = let x = 1 in x;\nf"),
  (
    "enumerate",
    "let enumerate : [?] -> [(Int, ?)] = fun xs ->\n  let helper = fun (xs, index) ->\n    case xs\n    | [] => []\n    | x :: xs => (index, x) :: helper(xs, index + 1)\n    end\n  in\n  helper(xs, 0);\nlet n = 2;\nenumerate([n])",
  ),
  (
    "merge_sort",
    "let sort : ([Int], (Int, Int) -> Bool) -> [Int] = fun (xs, cmp) ->\n  let split = fun xs -> (xs, []) in\n  let merge = fun (l, r) -> l @ r in\n  let merge_sort = fun xs ->\n    case xs\n    | [] => []\n    | _ =>\n      let (left, right) = split(xs) in\n      merge(left, right)\n    end\n  in\n  merge_sort(xs);\nsort([2, 1], fun (a, b) -> a < b)",
  ),
  (
    "a fun's let-in, then a statement",
    "let f = fun x -> let y = x + 100 in y;\nf(1);\nlet z = 2",
  ),
  /* a member that is a let chain, whose items are a level of their own */
  ("a statement opening with a let-in", "let x = 1 in x;\nlet y = 2;\ny"),
  (
    "a module of a let chain",
    "module Holes = {\n  let a = 1 in\n  let b = 2 in\n  a + b\n};\nHoles",
  ),
  (
    "a let chain holding a module",
    "module H = {\n  let a = 1 in\n  module N = {\n    let c = 3\n  } in\n  a + N.c\n};\nH",
  ),
  (
    "a let chain among members",
    "module M = {\n  let x = 1;\n  let a = x in\n  a + 1;\n  let y = 2\n};\nM.y",
  ),
  (
    "a module of let-in members",
    "module M = {\n  let f = let x = 1 in x;\n  let g = fun y ->\n    let z = y in\n    z\n};\nM.f",
  ),
];

/* Exp-rooted shapes beyond the corpus */
let exp_shapes = [
  (
    "comment above a def",
    "# doc #\nlet a = 1 in\n# two #\n# lines #\nlet b = 2 in\na + b",
  ),
  ("same-line items", "let a = 1 in let b = 2 in\na + b"),
  ("same line after a multi-line def", "let a =\n  1\nin let b = 2 in\nb"),
  ("indented program", "  let a = 1 in\n  a"),
  ("a use and a filter", "use M in\nhide x in\nlet a = 1 in\na"),
  ("statements", "let f = fun x -> x in\nf(1);\ntest f(1) == 1 end;\nf(2)"),
  ("an if's else taking a `;`", "let c = true in\nif c then 1 else 2;\n3"),
  (
    "module literals",
    "module M = {\n  let x = 1;\n  module N = {\n    let y = 2\n  };\n  test x == 1 end\n} in\nlet m = { let a = 3 } in\nmodule E = {} in\nmodule S : T = {\n  let z = 4\n} in\nM.x",
  ),
  ("one-line module", "module M = { let x = 1; let y = 2 } in\nM.x"),
  (
    "a module of a let chain",
    "module Holes = {\n  let a = 1 in\n  a\n} in\nHoles",
  ),
  ("trailing comment", "let a = 1 in\na\n# end #"),
  (
    "a function body's lets",
    "let f = fun x ->\n  let a = x in\n  # note #\n  let b = a in\n  b\nin\nf(1)",
  ),
];

/* every piece's line indentation, in one walk */
let indents = (seg: Segment.t): Stdlib.Hashtbl.t(Id.t, int) => {
  let tbl = Stdlib.Hashtbl.create(1024);
  let ind = ref(0);
  let lead = ref(true);
  let rec go = (ps: Segment.t) =>
    List.iter(
      ~f=
        (p: Piece.t) => {
          Stdlib.Hashtbl.replace(tbl, Piece.id(p), ind^);
          switch (p) {
          | Secondary(w) when Secondary.is_linebreak(w) =>
            ind := 0;
            lead := true;
          | Secondary(w) when Secondary.is_space(w) =>
            if (lead^) {
              ind := ind^ + 1;
            }
          | Tile(t) =>
            lead := false;
            List.iter(
              ~f=
                ch => {
                  go(ch);
                  lead := false;
                },
              t.children,
            );
          | _ => lead := false
          };
        },
      ps,
    );
  go(seg);
  tbl;
};

type tally = {
  mutable programs: int,
  mutable levels: int,
  mutable spans: int,
  mutable diffs: list(string),
};

let tally = () => {
  programs: 0,
  levels: 0,
  spans: 0,
  diffs: [],
};

let kind_name = (k: S.kind) =>
  switch (k) {
  | Def => "def"
  | Use => "use"
  | Stmt => "stmt"
  | Tail => "tail"
  };

let sub = (arr: array(Piece.t), a: int, b: int): string =>
  CorpusUtil.text_of(
    Array.to_list(Array.sub(arr, ~pos=a, ~len=max(0, b - a))),
  );

/* one level: its slices partition it, each core lies in its slice, and
   base_col is the indentation of the line the core starts on */
let check_level =
    (
      ~name: string,
      ~indent_of: Stdlib.Hashtbl.t(Id.t, int),
      t: tally,
      lv: S.level,
    ) => {
  t.levels = t.levels + 1;
  let len = Array.length(lv.pieces);
  let n = Array.length(lv.spans);
  t.spans = t.spans + n;
  let diff = what =>
    t.diffs = [
      Printf.sprintf("%s [%s level]: %s", name, Sort.show(lv.sort), what),
      ...t.diffs,
    ];
  Array.iteri(
    ~f=
      (k, sp: S.span) => {
        let lo_ok = k == 0 ? sp.lo == 0 : sp.lo == lv.spans[k - 1].hi;
        let hi_ok = k < n - 1 || sp.hi == len;
        let (c0, c1) = sp.core;
        if (!lo_ok || !hi_ok || !(sp.lo <= c0 && c0 <= c1 && c1 <= sp.hi)) {
          diff(Printf.sprintf("span %d breaks the partition", k));
        };
        let id = Piece.id(lv.pieces[c0]);
        if (!
              Poly.equal(
                Stdlib.Hashtbl.find_opt(indent_of, id),
                Some(sp.base_col),
              )) {
          diff(Printf.sprintf("span %d: base_col %d", k, sp.base_col));
        };
      },
    lv.spans,
  );
};

/* every level of a program: members and definition bodies, recursively */
let check_program = (t: tally, ~name: string, ~root: Sort.t, seg: Segment.t) => {
  t.programs = t.programs + 1;
  let indent_of = indents(seg);
  let rec walk = (lv: S.level) => {
    check_level(~name, ~indent_of, t, lv);
    Array.iteri(
      ~f=
        (k, _) => {
          Option.iter(~f=walk, S.children(lv, k));
          Option.iter(~f=walk, S.block(lv, k));
        },
      lv.spans,
    );
  };
  let top = S.top(~root, seg);
  walk(top);
  /* an expression level cuts no worse than top_items: at its cuts only */
  if (!Sort.equal(root, Sort.Mod)) {
    let cuts =
      List.fold(Segment.top_items(seg), ~init=([], 0), ~f=((acc, at), s) =>
        ([at + List.length(s), ...acc], at + List.length(s))
      )
      |> fst;
    Array.iter(
      ~f=
        (sp: S.span) =>
          if (sp.hi < List.length(seg)
              && !List.mem(cuts, sp.hi, ~equal=Poly.equal)) {
            t.diffs = [name ++ ": a cut top_items lacks", ...t.diffs];
          },
      top.spans,
    );
  };
  /* find reaches every span fold does by its anchor, and by its first
     tile it or the item below anchored there (a let chain's first) */
  ignore(
    S.fold(
      ((), path) => {
        let (lv, k) = Util.ListUtil.last(path);
        let sp = lv.spans[k];
        let lands = (~below, id) =>
          switch (S.find(~root, id, seg)) {
          | Some(p) =>
            let (lv', k') = Util.ListUtil.last(p);
            let here = List.length(p) == List.length(path);
            here
            && phys_equal(lv', lv)
            && k' == k
            || below
            && List.length(p) > List.length(path)
            && Id.equal(lv'.spans[k'].anchor, id)
            && (
              switch (List.nth(p, List.length(path) - 1)) {
              | Some((l, j)) => phys_equal(l, lv) && j == k
              | None => false
              }
            );
          | None => false
          };
        if (!lands(~below=false, sp.anchor)
            || !
                 Option.value_map(
                   sp.first,
                   ~default=true,
                   ~f=lands(~below=true),
                 )) {
          t.diffs = [name ++ ": find misses a span", ...t.diffs];
        };
      },
      (),
      top,
    ),
  );
};

let parse = (~root, src) =>
  switch (CorpusUtil.parse(~root, src)) {
  | Some(seg) => Some(seg)
  | None => CorpusUtil.typed_seg(~root, src)
  };

let report = (what: string, t: tally) => {
  Printf.printf(
    "ItemSpans %s: %d programs, %d levels, %d spans; %d diffs\n",
    what,
    t.programs,
    t.levels,
    t.spans,
    List.length(t.diffs),
  );
  List.iter(~f=d => Printf.printf("DIFF %s\n", d), List.rev(t.diffs));
};

let run = (programs: list((string, Sort.t, string))): tally => {
  let t = tally();
  List.iter(
    ~f=
      ((name, root, src)) =>
        switch (parse(~root, src)) {
        | Some(seg) => check_program(t, ~name, ~root, seg)
        | None => t.diffs = [name ++ ": unparseable", ...t.diffs]
        },
    programs,
  );
  t;
};

let shapes = () => {
  let t =
    run(
      List.map(~f=((n, s)) => (n, Sort.Exp, s), exp_shapes)
      @ List.map(
          ~f=((n, s)) => (n, Sort.Exp, s),
          CorpusUtil.edge_programs,
        )
      @ List.map(
          ~f=((n, s, _)) => (n, Sort.Exp, s),
          Test_HeadlessRows.shapes,
        )
      @ List.map(~f=((n, s)) => (n, Sort.Mod, s), letin_members),
    );
  report("shapes", t);
  check(int, "diffs", 0, List.length(t.diffs));
  /* a member opening with a let-in runs to its `;`, a statement */
  let seg = CorpusUtil.seg_of(~root=Mod, "let x = 1 in x;\nlet y = 2;\ny");
  let top = S.top(~root=Mod, seg);
  check(
    list(string),
    "kinds",
    ["stmt", "def", "stmt"],
    Array.to_list(
      Array.map(~f=(sp: S.span) => kind_name(sp.kind), top.spans),
    ),
  );
  check(
    list(string),
    "slices",
    ["let x = 1 in x;", "\nlet y = 2;", "\ny"],
    List.map(~f=CorpusUtil.text_of, S.slices(top)),
  );
  let stmt = top.spans[0];
  check(
    bool,
    "anchored at its `;`, first at its `let`",
    true,
    Piece.is_semi(top.pieces[snd(stmt.core)])
    && Id.equal(stmt.anchor, Piece.id(top.pieces[snd(stmt.core)]))
    && Option.equal(Id.equal, stmt.first, Some(Piece.id(top.pieces[0]))),
  );
};

/* a member that is a let chain is a level of its items; a definition's
   let chain stays one item */
let chains = () => {
  let kinds = (lv: S.level) =>
    Array.to_list(
      Array.map(~f=(sp: S.span) => kind_name(sp.kind), lv.spans),
    );
  let seg =
    CorpusUtil.seg_of(
      ~root=Mod,
      "module Holes = {\n  let a = 1 in\n  let b = 2 in\n  a + b\n};\nHoles",
    );
  let top = S.top(~root=Mod, seg);
  let members = Option.value_exn(S.children(top, 0));
  check(list(string), "one member", ["stmt"], kinds(members));
  let chain = Option.value_exn(S.children(members, 0));
  check(bool, "an expression level", true, Sort.equal(chain.sort, Sort.Exp));
  check(
    bool,
    "under the member",
    true,
    Poly.equal(chain.owner, Chain(members.spans[0].anchor)),
  );
  check(list(string), "its items", ["def", "def", "tail"], kinds(chain));
  check(
    list(string),
    "its slices",
    ["let a = 1 in", "\n  let b = 2 in", "\n  a + b"],
    List.map(~f=CorpusUtil.text_of, S.slices(chain)),
  );
  check(
    bool,
    "a plain statement has none",
    true,
    Option.is_none(S.children(top, 1)),
  );
  let let_b = chain.spans[1].anchor;
  check(
    int,
    "find reaches a chain's item",
    3,
    Option.value_map(
      S.find(~root=Mod, let_b, seg),
      ~default=0,
      ~f=List.length,
    ),
  );
  /* a module literal in a let chain has its members */
  let seg =
    CorpusUtil.seg_of(
      ~root=Mod,
      "module H = {\n  let a = 1 in\n  module N = {\n    let c = 3\n  } in\n  a + N.c\n};\nH",
    );
  let members = Option.value_exn(S.children(S.top(~root=Mod, seg), 0));
  let chain = Option.value_exn(S.children(members, 0));
  check(list(string), "the chain", ["def", "def", "tail"], kinds(chain));
  check(
    list(string),
    "the module in it",
    ["def"],
    kinds(Option.value_exn(S.children(chain, 1))),
  );
  /* a definition's body is not split: its lets are a block, on demand */
  let seg =
    CorpusUtil.seg_of(
      ~root=Mod,
      "module M = {\n  let summarize = fun xs ->\n    let total = 1 in\n    total;\n  let k = 2\n};\nM.k",
    );
  let members = Option.value_exn(S.children(S.top(~root=Mod, seg), 0));
  check(list(string), "members", ["def", "def"], kinds(members));
  check(
    bool,
    "no child level",
    true,
    Option.is_none(S.children(members, 0)),
  );
  check(
    list(string),
    "its block",
    ["def", "tail"],
    kinds(Option.value_exn(S.block(members, 0))),
  );
};

/* each span as (kind, core, slot, base_col): the slot's text with `|`
   at its core's start and at its terminator's end, and `=` before it when
   it is whole lines */
let layout = (lv: S.level): list(string) =>
  Array.to_list(lv.spans)
  |> List.map(~f=(sp: S.span) => {
       let (lo, term, hi) = sp.slot;
       let (c0, c1) = sp.core;
       Printf.sprintf(
         "%s `%s` %s`%s|%s|%s` %d",
         kind_name(sp.kind),
         sub(lv.pieces, c0, c1),
         Option.is_none(sp.lines) ? "" : "=",
         sub(lv.pieces, lo, c0),
         sub(lv.pieces, c0, term),
         sub(lv.pieces, term, hi),
         sp.base_col,
       );
     });

let direct = () => {
  let spans = (~root=Sort.Exp, src) =>
    layout(S.top(~root, CorpusUtil.seg_of(~root, src)));
  let expect = (name, want, got) => check(list(string), name, want, got);
  expect(
    "comments above go with the item below",
    [
      "def `let a = 1 in` =`# doc #\n|let a = 1 in|\n` 0",
      "def `let b = 2 in` =`# two #\n# lines #\n|let b = 2 in|\n` 0",
      "tail `a + b` =`|a + b|` 0",
    ],
    spans("# doc #\nlet a = 1 in\n# two #\n# lines #\nlet b = 2 in\na + b"),
  );
  expect(
    "items sharing a line are not whole lines",
    [
      "def `let a =\n  1\nin` `|let a =\n  1\nin| ` 0",
      "def `let b = 2 in` `|let b = 2 in|\n` 0",
      "tail `b` =`|b|` 0",
    ],
    spans("let a =\n  1\nin let b = 2 in\nb"),
  );
  expect(
    "indentation",
    ["def `let a = 1 in` =`  |let a = 1 in|\n` 2", "tail `a` =`  |a|` 2"],
    spans("  let a = 1 in\n  a"),
  );
  expect(
    "statements end at their `;`",
    [
      "def `let f = fun x -> x in` =`|let f = fun x -> x in|\n` 0",
      "stmt `f(1)` =`|f(1);|\n` 0",
      "stmt `test f(1) == 1 end` =`|test f(1) == 1 end;|\n` 0",
      "tail `f(2)` =`|f(2)|` 0",
    ],
    spans("let f = fun x -> x in\nf(1);\ntest f(1) == 1 end;\nf(2)"),
  );
  expect(
    "a trailing comment is the tail's",
    [
      "def `let a = 1 in` =`|let a = 1 in|\n` 0",
      "tail `a\n# end #` =`|a|\n` 0",
    ],
    spans("let a = 1 in\na\n# end #"),
  );
  let src = "let x = 1;\nmodule M = {\n  let y = 2;\n  let z = 3\n};\nM.z";
  let top = S.top(~root=Mod, CorpusUtil.seg_of(~root=Mod, src));
  expect(
    "members end at their `;`",
    [
      "def `let x = 1` =`|let x = 1;|\n` 0",
      "def `module M = {\n  let y = 2;\n  let z = 3\n}` =`|module M = {\n  let y = 2;\n  let z = 3\n};|\n` 0",
      "stmt `M.z` =`|M.z|` 0",
    ],
    layout(top),
  );
  expect(
    "a module's members",
    [
      "def `let y = 2` =`  |let y = 2;|\n` 2",
      "def `let z = 3` =`  |let z = 3|\n` 2",
    ],
    layout(Option.value_exn(S.children(top, 1))),
  );
};

let incomplete = () => {
  let t =
    run(
      List.map(
        ~f=((n, s)) => (n, Sort.Exp, s),
        CorpusUtil.incomplete_programs,
      ),
    );
  report("incomplete", t);
  check(int, "diffs", 0, List.length(t.diffs));
};

let corpus = () => {
  let t =
    run(
      CorpusUtil.slide_corpus()
      @ List.map(
          ~f=
            ((f, root)) =>
              (f, root, Option.value_exn(CorpusUtil.mega_src(f))),
          /* Mega 2k and 4k repeat their shapes */
          [("mega-1k.hz", Sort.Exp), ("mega-mod-1k.hz", Sort.Mod)],
        )
      @ List.map(~f=((p, s)) => (p, Sort.Mod, s), stdlib_corpus()),
    );
  report("corpus", t);
  check(int, "diffs", 0, List.length(t.diffs));
};

/* a level is rebuilt only when its pieces change: an edit inside one
   module's member rebuilds the top level and that module's members */
let memo = () => {
  let seg = CorpusUtil.mega_seg(~root=Mod, "mega-mod-1k.hz");
  let all = (seg: Segment.t) => {
    let top = S.top(~root=Mod, seg);
    S.fold((acc, path) => [Util.ListUtil.last(path), ...acc], [], top);
  };
  let before = all(seg);
  let b0 = S.built^;
  let again = all(List.map(~f=p => p, seg));
  check(int, "a fresh list of the same pieces builds nothing", b0, S.built^);
  check(
    bool,
    "the same levels",
    true,
    List.for_all2_exn(
      ~f=((l, _), (l', _)) => phys_equal(l, l'),
      before,
      again,
    ),
  );
  let (edited, found) =
    CorpusUtil.edit_token(
      ~at=`First,
      ~keep_id=true,
      ~needle="180",
      ~repl="181",
      seg,
    );
  check(bool, "a token to edit", true, found);
  let b1 = S.built^;
  let after = all(edited);
  check(int, "the top level and one module's members", 2, S.built^ - b1);
  let levels = l =>
    List.dedup_and_sort(
      ~compare,
      List.map(~f=((lv: S.level, _)) => lv.owner, l),
    );
  check(
    int,
    "same levels",
    List.length(levels(before)),
    List.length(levels(after)),
  );
};

/* the Read division leaves a finished program as written: every level
   (members and let chains, recursively) has the same spans, and the parse
   gives the same record. Programs with an unfinished form are counted
   apart: there Read may cut */
let read_finished = () => {
  let programs =
    CorpusUtil.slide_corpus()
    @ List.map(
        ~f=
          ((f, root)) =>
            (f, root, Option.value_exn(CorpusUtil.mega_src(f))),
        /* Mega 2k and 4k repeat their shapes */
        [("mega-1k.hz", Sort.Exp), ("mega-mod-1k.hz", Sort.Mod)],
      )
    @ List.map(~f=((p, s)) => (p, Sort.Mod, s), stdlib_corpus())
    @ List.filter_map(
        ~f=
          name =>
            Option.map(
              ~f=s => ("stdlib copy " ++ name, Sort.Mod, s),
              ModHarness.stdlib(name),
            ),
        ["List.hz", "Option.hz"],
      )
    @ List.map(~f=((n, s)) => (n, Sort.Mod, s), letin_members)
    @ List.map(~f=((n, s)) => (n, Sort.Exp, s), exp_shapes)
    @ List.map(~f=((n, s)) => (n, Sort.Exp, s), CorpusUtil.edge_programs);
  let levels = ref(0);
  let unfinished = ref(0);
  List.iter(
    ~f=
      ((name, root, src)) =>
        switch (parse(~root, src)) {
        | None => fail(name ++ ": unparseable")
        | Some(seg) when !List.is_empty(Segment.incomplete_tiles_deep(seg)) =>
          incr(unfinished)
        | Some(seg) =>
          let rec same = (w: S.level, r: S.level) => {
            incr(levels);
            check(
              bool,
              name ++ ": the same spans",
              true,
              w.spans == r.spans && w.owner == r.owner && !r.cut,
            );
            Array.iteri(
              ~f=
                (k, _) =>
                  switch (S.children(w, k), S.children(r, k)) {
                  | (Some(w), Some(r)) => same(w, r)
                  | (None, None) => ()
                  | _ => fail(name ++ ": a child level in one division only")
                  },
              w.spans,
            );
          };
          same(S.top(~root, seg), S.top(~ends=Read, ~root, seg));
          let fb = MakeTerm.Incr.fell_back^;
          let read = ends =>
            MakeTerm.Incr.go_incr(
              ~ends,
              ~root,
              ~cache=MakeTerm.Incr.mk_cache(),
              seg,
            );
          Test_MakeTermIncr.records_agree(name, read(Written), read(Read));
          check(int, name ++ ": no fallback", fb, MakeTerm.Incr.fell_back^);
        },
    programs,
  );
  Printf.printf(
    "Read division: %d finished programs, %d levels alike; %d unfinished skipped\n",
    List.length(programs) - unfinished^,
    levels^,
    unfinished^,
  );
};

/* a member typed near the top of List, key by key as the editor types it:
   at the Read division its open form stops at its member line, where
   completion ends it, so the members below keep their parses (no module
   parses whole) and its term no longer holds them; extents (TermData) read
   as at the Written division, so selections and probes don't move */
let read_typing = () => {
  module MA = ModHarness;
  let root = Sort.Mod;
  let src = Option.value_exn(MA.stdlib("List.hz"));
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
  let z =
    step(
      z0,
      Move(
        Point(
          Option.value_exn(
            MA.point_of(
              ~k=0,
              ~off=String.length("    end;"),
              src,
              "    end;",
            ),
          ),
          None,
        ),
      ),
    );
  let cache = MakeTerm.Incr.mk_cache();
  /* List's member level */
  let members = (lv: S.level) =>
    List.init(Array.length(lv.spans), ~f=k => k)
    |> List.find_map(~f=k =>
         Option.is_none(lv.spans[k].body) ? None : S.children(lv, k)
       )
    |> Option.value_exn;
  let typed = "\nlet q = (1 + 2";
  let parses = ref([]);
  ignore(
    List.fold(
      List.init(String.length(typed), ~f=i => i),
      ~init=z,
      ~f=(z, i) => {
        let c = String.sub(typed, ~pos=i, ~len=1);
        let z = step(z, Insert(c));
        let seg = Zipper.unselect_and_zip(z);
        let fb = MakeTerm.Incr.fell_back^;
        let a0 = MakeTerm.Incr.full_analyzed^;
        let r = MakeTerm.Incr.go_incr(~ends=Read, ~root, ~cache, seg);
        parses := [MakeTerm.Incr.full_analyzed^ - a0, ...parses^];
        check(int, c ++ ": no fallback", fb, MakeTerm.Incr.fell_back^);
        let w =
          MakeTerm.Incr.go_incr(~root, ~cache=MakeTerm.Incr.mk_cache(), seg);
        check(
          bool,
          c ++ ": the extents as written",
          true,
          Id.Map.equal(Test_MakeTermIncr.same, w.term_data, r.term_data),
        );
        let wl = members(S.top(~root, seg));
        let rl = members(S.top(~ends=Read, ~root, seg));
        if (!List.is_empty(Segment.incomplete_tiles(wl.seg))) {
          /* an open form: Written runs it to the module's end, Read ends
             it at its member line, as completion does */
          check(bool, c ++ ": cut", true, rl.cut);
          /* the slice typed into, after List.length's */
          let q_text = (lv: S.level) =>
            Util.StringUtil.trim_leading(
              CorpusUtil.text_of(S.slice(lv, 1)),
            );
          check(
            bool,
            c ++ ": Written runs on",
            true,
            String.length(q_text(wl)) > 1000,
          );
          check(
            bool,
            c ++ ": Read stops at the member line",
            false,
            String.contains(String.strip(q_text(rl)), '\n'),
          );
        };
        z;
      },
    ),
  );
  /* once the first open form stands, each key parses its member and
     List's shell */
  let per_key = List.rev(parses^);
  Printf.printf(
    "typing a member near List's top, parses per key at Read: %s\n",
    String.concat(~sep=" ", List.map(~f=string_of_int, per_key)),
  );
  check(
    list(int),
    "the open paren's keys",
    [2, 2, 2, 2],
    Util.ListUtil.sublist(
      (List.length(per_key) - 4, List.length(per_key)),
      per_key,
    ),
  );
};

let tests = (
  "ItemSpans",
  [
    test_case("shapes", `Quick, shapes),
    test_case("let chains", `Quick, chains),
    test_case("spans, cores and slots", `Quick, direct),
    test_case("incomplete programs", `Quick, incomplete),
    test_case("memoized on pieces", `Quick, memo),
    test_case("corpus", `Slow, corpus),
    test_case("Read: a finished program as written", `Slow, read_finished),
    test_case("Read: typing a member near List's top", `Slow, read_typing),
  ],
);
