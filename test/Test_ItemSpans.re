open Alcotest;
open Haz3lcore;

/* ItemSpans against the rules it replaced (Focus and IE below), at every
   level of the corpora (module members and definition bodies included):
   Focus.item_spans for spans and cores, IE's slots for slots and lines,
   Focus.line_indent for base_col, Segment.top_items for the top level's
   cuts. Differences by design are named where they are allowed */

module S = ItemSpans;
module Focus = FocusOracle;
module IE = ItemEditOracle;

/* the stdlib's sources (the world tree's top-level files), module bodies
   (Mod-rooted), where the repo has them */
let stdlib_corpus = (): list((string, string)) => {
  let dir =
    Sys.file_exists("hazel-programs/world")
      ? "hazel-programs/world" : "../hazel-programs/world";
  switch (Sys.readdir(dir)) {
  | files =>
    files
    |> Array.to_list
    |> List.filter(f => Filename.check_suffix(f, ".hz"))
    |> List.sort(compare)
    |> List.filter_map(f => {
         let path = Filename.concat(dir, f);
         Option.map(src => (path, src), CorpusUtil.read_file(path));
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
  /* where Focus did cut after an `in`: a member that is a let chain,
     whose items are a level of their own */
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

/* every piece's line indentation, as Focus.line_indent reads it, in one
   walk */
let indents = (seg: Segment.t): Hashtbl.t(Id.t, int) => {
  let tbl = Hashtbl.create(1024);
  let ind = ref(0);
  let lead = ref(true);
  let rec go = (ps: Segment.t) =>
    List.iter(
      (p: Piece.t) => {
        Hashtbl.replace(tbl, Piece.id(p), ind^);
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
  mutable focus_checks: int,
  mutable slot_checks: int,
  mutable indent_checks: int,
  mutable diffs: list(string),
  /* Focus cut after an `in` at a Mod level: decision 4 */
  mutable mod_in: list(string),
  /* Focus cut after an `…in` tile missing its `in` */
  mutable open_in: list(string),
};

let tally = () => {
  programs: 0,
  levels: 0,
  spans: 0,
  focus_checks: 0,
  slot_checks: 0,
  indent_checks: 0,
  diffs: [],
  mod_in: [],
  open_in: [],
};

let kind_name = (k: S.kind) =>
  switch (k) {
  | Def => "def"
  | Use => "use"
  | Stmt => "stmt"
  | Tail => "tail"
  };

let sub = (arr: array(Piece.t), a: int, b: int): string =>
  CorpusUtil.text_of(Array.to_list(Array.sub(arr, a, max(0, b - a))));

let is_directive = (t: Tile.t) =>
  List.mem(Tile.token(t, 0), ["use", "hide", "eval", "pause", "debug"]);

/* one level against Focus and IE */
let check_level =
    (
      ~name: string,
      ~indent_of: Hashtbl.t(Id.t, int),
      ~nested: bool,
      t: tally,
      lv: S.level,
    ) => {
  t.levels = t.levels + 1;
  let arr = lv.pieces;
  let len = Array.length(arr);
  let at_mod = lv.sort == Sort.Mod;
  let is_members =
    switch (lv.owner) {
    | Members(_) => true
    | Top
    | Chain(_)
    | Block(_) => false
    };
  let n = Array.length(lv.spans);
  t.spans = t.spans + n;
  let diff = what =>
    t.diffs = [
      Printf.sprintf("%s [%s level]: %s", name, Sort.show(lv.sort), what),
      ...t.diffs,
    ];
  /* the slices partition the level */
  Array.iteri(
    (k, sp: S.span) => {
      let lo_ok = k == 0 ? sp.lo == 0 : sp.lo == lv.spans[k - 1].hi;
      let hi_ok = k < n - 1 || sp.hi == len;
      let (c0, c1) = sp.core;
      if (!lo_ok || !hi_ok || !(sp.lo <= c0 && c0 <= c1 && c1 <= sp.hi)) {
        diff(Printf.sprintf("span %d breaks the partition", k));
      };
    },
    lv.spans,
  );
  let sf =
    Array.of_list(Focus.item_spans(~divided_only_tail=nested, lv.seg));
  let m = Array.length(sf);
  let content = (sp: Focus.item_span) => {
    let rec go = i =>
      i < sp.sp_stop && (!Piece.is_secondary(arr[i]) || go(i + 1));
    go(sp.sp_start);
  };
  let hole_only = (sp: Focus.item_span) => {
    let rec go = (i, found) =>
      i >= sp.sp_stop
        ? found
        : (
          switch (arr[i]) {
          | Grout({shape: Convex, _}) when !found => go(i + 1, true)
          | Secondary(_) => go(i + 1, found)
          | _ => false
          }
        );
    go(sp.sp_start, false);
  };
  /* Focus's last span that ItemSpans folds into the one before: comments
     only, or at a Mod level the hole after a last `;` */
  let kept =
    m > 0 && (!content(sf[m - 1]) || at_mod && hole_only(sf[m - 1]))
      ? m - 1 : m;
  let mod_in =
    at_mod
    && Array.exists(
         (sp: Focus.item_span) =>
           sp.sp_kind == IDef
           && (
             switch (Focus.first_tile(arr, sp.sp_start, sp.sp_stop)) {
             | Some(tl) => Tile.ends_with_in(tl)
             | None => false
             }
           ),
         sf,
       );
  let open_in =
    Array.exists(
      (p: Piece.t) =>
        switch (p) {
        | Tile(tl) => Tile.ends_with_in(tl) && !Segment.is_in_tile(p)
        | _ => false
        },
      arr,
    );
  if (mod_in) {
    t.mod_in = [name, ...t.mod_in];
    /* where Focus cut after an `in`, ItemSpans has one member whose let
       chain is a level of its own, checked as one */
    Array.iter(
      (f: Focus.item_span) =>
        switch (Focus.first_tile(arr, f.sp_start, f.sp_stop)) {
        | Some(tl) when f.sp_kind == IDef && Tile.ends_with_in(tl) =>
          let at = i => Piece.id(arr[i]) == tl.id;
          let held =
            List.init(n, k => k)
            |> List.exists(k => {
                 let (c0, c1) = lv.spans[k].core;
                 let rec has = i => i < c1 && (at(i) || has(i + 1));
                 has(c0) && S.chain(lv, k) != None;
               });
          if (!held) {
            diff("an `in` Focus cuts at, in no let chain");
          };
        | _ => ()
        },
      sf,
    );
  } else if (open_in) {
    t.open_in = [name, ...t.open_in];
  } else if (kept != n) {
    diff(Printf.sprintf("%d spans, Focus %d", n, kept));
  } else {
    Array.iteri(
      (k, sp: S.span) => {
        t.focus_checks = t.focus_checks + 1;
        let f = sf[k];
        let ftile = Focus.first_tile(arr, f.sp_start, f.sp_stop);
        let kind: S.kind =
          switch (f.sp_kind, ftile) {
          | (IDef, Some(tl)) when is_directive(tl) => Use
          | (IDef, _) => Def
          | (IStmt, _) => Stmt
          | (ITail, _) => at_mod ? Stmt : Tail
          };
        let anchor =
          switch (f.sp_id) {
          | Some(id) => Some(id)
          | None =>
            let rec go = i =>
              i >= f.sp_stop
                ? None
                : Piece.is_secondary(arr[i])
                    ? go(i + 1) : Some(Piece.id(arr[i]));
            go(f.sp_start);
          };
        let rec skip = i =>
          i < f.sp_stop && Focus.is_edge_ws(arr[i]) ? skip(i + 1) : i;
        let c0 = skip(f.sp_start);
        let rec back = i =>
          i > c0 && Focus.is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
        let c1 = back(Focus.before_semi(arr, f.sp_start, f.sp_stop));
        let first =
          Option.map((tl: Tile.t) => tl.id, Focus.first_tile(arr, c0, c1));
        let row = Printf.sprintf("span %d `%s`", k, sub(arr, c0, c1));
        if (kind != sp.kind) {
          diff(row ++ ": kind " ++ kind_name(sp.kind));
        };
        if (anchor != Some(sp.anchor)) {
          diff(row ++ ": anchor");
        };
        if ((c0, c1) != sp.core) {
          diff(
            Printf.sprintf(
              "%s: core `%s`",
              row,
              sub(arr, fst(sp.core), snd(sp.core)),
            ),
          );
        };
        if (first != sp.first) {
          diff(row ++ ": first tile");
        };
      },
      lv.spans,
    );
    /* slots and lines: IE's slots over Focus's spans */
    let b =
      IE.block(~in_module=at_mod, ~body=is_members, ~bol=lv.bol, sf, lv.seg);
    let line_start = i => i == 0 ? lv.bol : Piece.is_linebreak(arr[i - 1]);
    if (Array.length(b.sl) != n) {
      diff(Printf.sprintf("%d spans, IE %d slots", n, Array.length(b.sl)));
    } else {
      Array.iteri(
        (k, sp: S.span) => {
          t.slot_checks = t.slot_checks + 1;
          let sl = b.sl[k];
          if ((sl.lo, sl.term, sl.hi) != sp.slot) {
            let (lo, term, hi) = sp.slot;
            diff(
              Printf.sprintf(
                "span %d `%s`: slot (%d, %d, %d), IE (%d, %d, %d)",
                k,
                sub(arr, fst(sp.core), snd(sp.core)),
                lo,
                term,
                hi,
                sl.lo,
                sl.term,
                sl.hi,
              ),
            );
          };
          let whole =
            line_start(sl.lo)
            && (Piece.is_linebreak(arr[sl.hi - 1]) || sl.hi == len && lv.eol);
          let want = whole ? Some((sl.lo, sl.hi)) : None;
          if (want != sp.lines) {
            diff(
              Printf.sprintf(
                "span %d `%s`: lines %s, slot (%d, %d)",
                k,
                sub(arr, fst(sp.core), snd(sp.core)),
                switch (sp.lines) {
                | Some((a, z)) => Printf.sprintf("(%d, %d)", a, z)
                | None => "none"
                },
                sl.lo,
                sl.hi,
              ),
            );
          };
          /* a module literal is IE's module definition, but `{}` */
          let empty =
            switch (sp.kind, IE.module_at(b, k)) {
            | (Def, Some(head)) =>
              switch (
                Option.map(
                  List.filter(p => !Piece.is_secondary(p)),
                  Focus.find_def(head, lv.seg),
                )
              ) {
              | Some([Tile(tl)]) => IE.is_empty_body(tl)
              | _ => false
              }
            | _ => false
            };
          let module_def = IE.module_at(b, k) != None && !empty;
          if (module_def != (sp.body != None)) {
            diff(Printf.sprintf("span %d: module literal", k));
          };
        },
        lv.spans,
      );
    };
  };
  /* base_col: the indentation of the line the core starts on */
  Array.iteri(
    (k, sp: S.span) => {
      t.indent_checks = t.indent_checks + 1;
      let id = Piece.id(arr[fst(sp.core)]);
      if (Hashtbl.find_opt(indent_of, id) != Some(sp.base_col)) {
        diff(
          Printf.sprintf(
            "span %d: base_col %d, line_indent %s",
            k,
            sp.base_col,
            switch (Hashtbl.find_opt(indent_of, id)) {
            | Some(i) => string_of_int(i)
            | None => "none"
            },
          ),
        );
      };
    },
    lv.spans,
  );
};

/* every level of a program: members and definition bodies, recursively */
let check_program = (t: tally, ~name: string, ~root: Sort.t, seg: Segment.t) => {
  t.programs = t.programs + 1;
  let indent_of = indents(seg);
  let rec walk = (~nested, lv: S.level) => {
    check_level(~name, ~indent_of, ~nested, t, lv);
    Array.iteri(
      (k, _) => {
        Option.iter(walk(~nested=false), S.children(lv, k));
        Option.iter(walk(~nested=true), S.block(lv, k));
      },
      lv.spans,
    );
  };
  let top = S.top(~root, seg);
  walk(~nested=false, top);
  /* an expression level cuts no worse than top_items: at its cuts only */
  if (root != Sort.Mod) {
    let cuts =
      List.fold_left(
        ((acc, at), s) =>
          ([at + List.length(s), ...acc], at + List.length(s)),
        ([], 0),
        Segment.top_items(seg),
      )
      |> fst;
    Array.iter(
      (sp: S.span) =>
        if (sp.hi < List.length(seg) && !List.mem(sp.hi, cuts)) {
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
            && lv' === lv
            && k' == k
            || below
            && List.length(p) > List.length(path)
            && lv'.spans[k'].anchor == id
            && (
              switch (List.nth_opt(p, List.length(path) - 1)) {
              | Some((l, j)) => l === lv && j == k
              | None => false
              }
            );
          | None => false
          };
        if (!lands(~below=false, sp.anchor)
            || !Option.fold(~none=true, ~some=lands(~below=true), sp.first)) {
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
    "ItemSpans %s: %d programs, %d levels, %d spans; %d Focus, %d IE, %d line_indent checks; %d diffs; Focus cuts after an `in` at a Mod level in %d levels, after an unfinished `…in` in %d\n",
    what,
    t.programs,
    t.levels,
    t.spans,
    t.focus_checks,
    t.slot_checks,
    t.indent_checks,
    List.length(t.diffs),
    List.length(t.mod_in),
    List.length(t.open_in),
  );
  List.iter(d => Printf.printf("DIFF %s\n", d), List.rev(t.diffs));
  List.iter(d => Printf.printf("MOD-IN %s\n", d), List.rev(t.mod_in));
  List.iter(d => Printf.printf("OPEN-IN %s\n", d), List.rev(t.open_in));
};

let run = (programs: list((string, Sort.t, string))): tally => {
  let t = tally();
  List.iter(
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
      List.map(((n, s)) => (n, Sort.Exp, s), exp_shapes)
      @ List.map(((n, s)) => (n, Sort.Exp, s), CorpusUtil.edge_programs)
      @ List.map(
          ((n, s, _)) => (n, Sort.Exp, s),
          Test_HeadlessRows.shapes,
        )
      @ List.map(((n, s)) => (n, Sort.Mod, s), letin_members),
    );
  report("shapes", t);
  check(int, "diffs", 0, List.length(t.diffs));
  check(
    list(string),
    "Focus cuts after an `in` at a Mod level: let chains",
    [
      "a let chain among members",
      "a let chain holding a module",
      "a module of a let chain",
      "a statement opening with a let-in",
    ],
    List.sort_uniq(compare, t.mod_in),
  );
  /* there the item runs to its `;`, a statement */
  let seg = CorpusUtil.seg_of(~root=Mod, "let x = 1 in x;\nlet y = 2;\ny");
  let top = S.top(~root=Mod, seg);
  check(
    list(string),
    "kinds",
    ["stmt", "def", "stmt"],
    Array.to_list(
      Array.map((sp: S.span) => kind_name(sp.kind), top.spans),
    ),
  );
  check(
    list(string),
    "slices",
    ["let x = 1 in x;", "\nlet y = 2;", "\ny"],
    List.map(CorpusUtil.text_of, S.slices(top)),
  );
  let stmt = top.spans[0];
  check(
    bool,
    "anchored at its `;`, first at its `let`",
    true,
    Piece.is_semi(top.pieces[snd(stmt.core)])
    && stmt.anchor == Piece.id(top.pieces[snd(stmt.core)])
    && stmt.first == Some(Piece.id(top.pieces[0])),
  );
};

/* a member that is a let chain is a level of its items; a definition's
   let chain stays one item */
let chains = () => {
  let kinds = (lv: S.level) =>
    Array.to_list(Array.map((sp: S.span) => kind_name(sp.kind), lv.spans));
  let seg =
    CorpusUtil.seg_of(
      ~root=Mod,
      "module Holes = {\n  let a = 1 in\n  let b = 2 in\n  a + b\n};\nHoles",
    );
  let top = S.top(~root=Mod, seg);
  let members = Option.get(S.children(top, 0));
  check(list(string), "one member", ["stmt"], kinds(members));
  let chain = Option.get(S.children(members, 0));
  check(bool, "an expression level", true, chain.sort == Sort.Exp);
  check(
    bool,
    "under the member",
    true,
    chain.owner == Chain(members.spans[0].anchor),
  );
  check(list(string), "its items", ["def", "def", "tail"], kinds(chain));
  check(
    list(string),
    "its slices",
    ["let a = 1 in", "\n  let b = 2 in", "\n  a + b"],
    List.map(CorpusUtil.text_of, S.slices(chain)),
  );
  check(
    bool,
    "a plain statement has none",
    true,
    S.children(top, 1) == None,
  );
  let let_b = chain.spans[1].anchor;
  check(
    int,
    "find reaches a chain's item",
    3,
    Option.fold(~none=0, ~some=List.length, S.find(~root=Mod, let_b, seg)),
  );
  /* a module literal in a let chain has its members */
  let seg =
    CorpusUtil.seg_of(
      ~root=Mod,
      "module H = {\n  let a = 1 in\n  module N = {\n    let c = 3\n  } in\n  a + N.c\n};\nH",
    );
  let members = Option.get(S.children(S.top(~root=Mod, seg), 0));
  let chain = Option.get(S.children(members, 0));
  check(list(string), "the chain", ["def", "def", "tail"], kinds(chain));
  check(
    list(string),
    "the module in it",
    ["def"],
    kinds(Option.get(S.children(chain, 1))),
  );
  /* a definition's body is not split: its lets are a block, on demand */
  let seg =
    CorpusUtil.seg_of(
      ~root=Mod,
      "module M = {\n  let summarize = fun xs ->\n    let total = 1 in\n    total;\n  let k = 2\n};\nM.k",
    );
  let members = Option.get(S.children(S.top(~root=Mod, seg), 0));
  check(list(string), "members", ["def", "def"], kinds(members));
  check(bool, "no child level", true, S.children(members, 0) == None);
  check(
    list(string),
    "its block",
    ["def", "tail"],
    kinds(Option.get(S.block(members, 0))),
  );
};

let incomplete = () => {
  let t =
    run(
      List.map(
        ((n, s)) => (n, Sort.Exp, s),
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
          ((f, root)) => (f, root, Option.get(CorpusUtil.mega_src(f))),
          [
            ("mega-1k.hz", Sort.Exp),
            ("mega-2k.hz", Sort.Exp),
            ("mega-4k.hz", Sort.Exp),
            ("mega-mod-1k.hz", Sort.Mod),
          ],
        )
      @ List.map(((p, s)) => (p, Sort.Mod, s), stdlib_corpus()),
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
  let again = all(List.map(p => p, seg));
  check(int, "a fresh list of the same pieces builds nothing", b0, S.built^);
  check(
    bool,
    "the same levels",
    true,
    List.for_all2(((l, _), (l', _)) => l === l', before, again),
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
    List.sort_uniq(compare, List.map(((lv: S.level, _)) => lv.owner, l));
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
        ((f, root)) => (f, root, Option.get(CorpusUtil.mega_src(f))),
        [
          ("mega-1k.hz", Sort.Exp),
          ("mega-2k.hz", Sort.Exp),
          ("mega-4k.hz", Sort.Exp),
          ("mega-mod-1k.hz", Sort.Mod),
        ],
      )
    @ List.map(((p, s)) => (p, Sort.Mod, s), stdlib_corpus())
    @ List.filter_map(
        name =>
          Option.map(
            s => ("modaudit " ++ name, Sort.Mod, s),
            Test_ModAudit.stdlib(name),
          ),
        ["List.hz", "Option.hz"],
      )
    @ List.map(((n, s)) => (n, Sort.Mod, s), letin_members)
    @ List.map(((n, s)) => (n, Sort.Exp, s), exp_shapes)
    @ List.map(((n, s)) => (n, Sort.Exp, s), CorpusUtil.edge_programs);
  let levels = ref(0);
  let unfinished = ref(0);
  List.iter(
    ((name, root, src)) =>
      switch (parse(~root, src)) {
      | None => fail(name ++ ": unparseable")
      | Some(seg) when Segment.incomplete_tiles_deep(seg) != [] =>
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
  module MA = Test_ModAudit;
  let root = Sort.Mod;
  let src = Option.get(MA.stdlib("List.hz"));
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
          Option.get(
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
    List.init(Array.length(lv.spans), k => k)
    |> List.find_map(k =>
         lv.spans[k].body == None ? None : S.children(lv, k)
       )
    |> Option.get;
  let typed = "\nlet q = (1 + 2";
  let parses = ref([]);
  ignore(
    List.fold_left(
      (z, i) => {
        let c = String.sub(typed, i, 1);
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
          Id.Map.equal((==), w.term_data, r.term_data),
        );
        let wl = members(S.top(~root, seg));
        let rl = members(S.top(~ends=Read, ~root, seg));
        if (Segment.incomplete_tiles(wl.seg) != []) {
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
            String.contains(String.trim(q_text(rl)), '\n'),
          );
        };
        z;
      },
      z,
      List.init(String.length(typed), i => i),
    ),
  );
  /* once the first open form stands, each key parses its member and
     List's shell */
  let per_key = List.rev(parses^);
  Printf.printf(
    "typing a member near List's top, parses per key at Read: %s\n",
    String.concat(" ", List.map(string_of_int, per_key)),
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
    test_case("incomplete programs", `Quick, incomplete),
    test_case("memoized on pieces", `Quick, memo),
    test_case("corpus", `Slow, corpus),
    test_case("Read: a finished program as written", `Slow, read_finished),
    test_case("Read: typing a member near List's top", `Quick, read_typing),
  ],
);
