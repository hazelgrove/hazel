open Alcotest;
open Haz3lcore;

/* EntryPath: the path grammar, resolving and describing both ways, its
   per-level index, and parity with the agent's HighLevelNodeMap on the
   corpora */

module P = EntryPath;
module S = ItemSpans;

let seg_of = (~root=Sort.Exp, src) => CorpusUtil.seg_of(~root, src);

let resolve = (~root=Sort.Exp, seg, s) =>
  switch (P.resolve_string(~root, seg, s)) {
  | Ok(f) => f
  | Error(e) => Alcotest.fail(s ++ ": " ++ P.message(e))
  };

let error = (~root=Sort.Exp, seg, s) =>
  switch (P.resolve_string(~root, seg, s)) {
  | Ok(_) => Alcotest.fail(s ++ ": resolves")
  | Error(e) => P.message(e)
  };

/* the text of the entry a path resolves to, without its edge whitespace
   or `;` */
let core_text = (f: P.found): string => {
  let (lv, k) = Util.ListUtil.last(f.steps);
  let (c0, c1) = lv.spans[k].core;
  String.trim(
    CorpusUtil.text_of(Array.to_list(Array.sub(lv.pieces, c0, c1 - c0))),
  );
};

let starts = (prefix, s) =>
  String.length(s) >= String.length(prefix)
  && String.sub(s, 0, String.length(prefix)) == prefix;

/* the plan's example program */
let example = {|module M = {
  let parse(s) = 1;
  hint "parses ints" test parse("1") == 1 end;
  type t = Int;
  module Inner = { let z = 1 };
  test parse("1") == 1 end;
  parse("2");
  test parse("3") == 1 end
} in
let f(x) =
  let y = x + 1 in
  type p = (Int, Int) in
  module Acc = { let n = 1 } in
  y;
  let (a, b) = (y, y) in
  a * b
in
f(1)|};

/* ---- the grammar ---- */

let grammar = () => {
  let canon = s =>
    switch (P.parse(s)) {
    | Ok(p) => P.to_string(p)
    | Error(e) => Alcotest.fail(e)
    };
  List.iter(
    ((src, want)) => check(string, src, want, canon(src)),
    [
      ("M.parse", "M/parse"),
      ("M/parse", "M/parse"),
      ("M/\"parses ints\"", "M/\"parses ints\""),
      ("M/\"a.b/c#d\"", "M/\"a.b/c#d\""),
      ("M/test#2", "M/test#2"),
      ("f/stmt#1", "f/stmt#1"),
      ("f/let#2", "f/let#2"),
      ("f/=>", "f/=>"),
      ("f/⇒", "f/=>"),
      ("parse#2", "parse#2"),
      ("M.parse:body", "M/parse:body"),
      ("f:pattern", "f:pattern"),
      ("f:definition", "f:definition"),
      ("f:head", "f:head"),
      ("\"h\"#2", "\"h\"#2"),
      (" M . parse ", "M/parse"),
      /* a kind word without `#k` is a name */
      ("stmt", "stmt"),
      ("def#1", "def#1"),
    ],
  );
  List.iter(
    src =>
      check(
        bool,
        src ++ " is refused",
        true,
        Result.is_error(P.parse(src)),
      ),
    ["", "M/", "M//x", "\"open", "x#0", "x#", "x:foo", "x:body/y", "a b"],
  );
};

/* ---- resolving ---- */

let resolving = () => {
  let seg = seg_of(example);
  let at = (path, kind, prefix) => {
    let f = resolve(seg, path);
    check(bool, path ++ ": kind", true, f.kind == kind);
    let text = core_text(f);
    check(bool, path ++ ": " ++ text, true, starts(prefix, text));
  };
  at("M", Module, "module M =");
  at("M.parse", Def, "let parse(s) = 1");
  at("M/parse", Def, "let parse(s) = 1");
  at("M/\"parses ints\"", Test, "hint \"parses ints\"");
  at("M/t", Type, "type t");
  at("M/Inner", Module, "module Inner");
  at("M/Inner/z", Def, "let z");
  at("M/test#1", Test, "hint \"parses ints\"");
  at("M/test#2", Test, "test parse(\"1\")");
  at("M/test#3", Test, "test parse(\"3\")");
  at("M/stmt#1", Stmt, "parse(\"2\")");
  at("f", Def, "let f(x)");
  at("f/y", Def, "let y");
  at("f/p", Type, "type p");
  at("f/Acc", Module, "module Acc");
  at("f/Acc/n", Def, "let n");
  at("f/stmt#1", Stmt, "y");
  at("f/a", Def, "let (a, b)");
  at("f/b", Def, "let (a, b)");
  at("f/=>", Tail, "a * b");
  at("f/⇒", Tail, "a * b");
  at("=>", Tail, "f(1)");
  check(
    bool,
    "f/a and f/b: one entry",
    true,
    resolve(seg, "f/a").id == resolve(seg, "f/b").id,
  );
  let seg' = seg_of("module N = { let c = 1 } in\nuse N in\nc");
  check(bool, "use#1", true, resolve(seg', "use#1").kind == Use);
  /* parts tag the entry */
  let f = resolve(seg, "M.parse:definition");
  check(
    bool,
    "a part keeps the entry",
    true,
    f.id == resolve(seg, "M.parse").id,
  );
  check(bool, "the part", true, f.part == Some(Definition));
  /* the steps are ItemSpans' own levels, as find reaches them */
  let f = resolve(seg, "M/Inner/z");
  check(
    bool,
    "steps as ItemSpans.find",
    true,
    switch (S.find(~root=Exp, f.id, seg)) {
    | Some(steps) =>
      List.for_all2(
        ((l, k), (l', k')) => l === l' && k == k',
        steps,
        f.steps,
      )
    | None => false
    },
  );
};

let errors = () => {
  let seg = seg_of(example);
  let has = (needle, s) =>
    check(
      bool,
      needle ++ " in: " ++ s,
      true,
      CorpusUtil.contains(needle, s),
    );
  let e = error(seg, "M/prase");
  has("No entry \"prase\" in \"M\"", e);
  has("parse, \"parses ints\", t, Inner, test#2, stmt#1, test#3", e);
  let e = error(seg, "f/q");
  has("y, p, Acc, stmt#1, a, b, =>", e);
  has("the program's top level", error(seg, "nope"));
  has("there are only 3", error(seg, "M/test#4"));
  has("has no entries of its own", error(seg, "M/parse/x"));
  has("is a test, which has no pattern", error(seg, "M/test#2:pattern"));
  has("is a type, which has no body", error(seg, "M/t:body"));
  check(
    bool,
    "a let's body",
    true,
    resolve(seg, "f:body").part == Some(Body),
  );
  has("at 2", error(seg, "M/"));
  let seg = seg_of("let x = 1 in\nlet x = 2 in\nlet y = x in\nx + y");
  let e = error(seg, "x");
  has("\"x\" is ambiguous in the program's top level: 2 entries", e);
  has("x#1, x#2", e);
  has("No entry \"x#3\"", error(seg, "x#3"));
  has("x#1, x#2, y, =>", error(seg, "z"));
};

/* `#k` counts in program order within the block */
let program_order = () => {
  let seg =
    seg_of(
      "let x = 1 in\nmodule M = {\n  let x = 2;\n  let y = 3;\n  let x = 4\n} in\nlet x = 5 in\nx",
    );
  let text = s => core_text(resolve(seg, s));
  check(string, "x#1", "let x = 1 in", text("x#1"));
  check(string, "x#2", "let x = 5 in", text("x#2"));
  check(string, "M/x#1", "let x = 2", text("M/x#1"));
  check(string, "M/x#2", "let x = 4", text("M/x#2"));
  check(string, "M/y", "let y = 3", text("M/y"));
};

/* a shared name short of the path's end picks the one entry with the
   rest of the path, as the node map reads `m/x` */
let shared_prefix = () => {
  let seg =
    seg_of(
      "module M = { let x = 1; let z = 1 } in\nmodule M = { let y = 2; let z = 2 } in\nM.y",
    );
  let text = s => core_text(resolve(seg, s));
  check(string, "M/x", "let x = 1", text("M/x"));
  check(string, "M/y", "let y = 2", text("M/y"));
  check(string, "M#2/z", "let z = 2", text("M#2/z"));
  let has = (needle, s) =>
    check(
      bool,
      needle ++ " in: " ++ s,
      true,
      CorpusUtil.contains(needle, s),
    );
  has("\"M\" is ambiguous", error(seg, "M"));
  has("Pick one: M#1, M#2", error(seg, "M/z"));
  has("No entry \"w\" in \"M#1\"", error(seg, "M/w"));
  /* described by the shortest path that resolves */
  let path = s =>
    Option.map(P.to_string, P.describe(~root=Exp, seg, resolve(seg, s).id));
  check(option(string), "M#2/y", Some("M/y"), path("M#2/y"));
  check(option(string), "M#2/z", Some("M#2/z"), path("M#2/z"));
  check(option(string), "M#1", Some("M#1"), path("M#1"));
};

/* a Mod root: its top level is a module body, its last `;`-less
   expression its tail */
let mod_src = "let a = 1;\ntest a == 1 end;\nlet g = let q = 2 in q;\nlet h = 1 in h + a;\na + 1";
let mod_root = () => {
  let root = Sort.Mod;
  let seg = seg_of(~root, mod_src);
  let kind = s => resolve(~root, seg, s).kind;
  check(bool, "a", true, kind("a") == Def);
  check(bool, "test#1", true, kind("test#1") == Test);
  check(bool, "g/q", true, kind("g/q") == Def);
  check(bool, "g/=>", true, kind("g/=>") == Tail);
  /* a member that is a let chain: a statement whose chain is its block */
  check(bool, "stmt#1", true, kind("stmt#1") == Stmt);
  check(bool, "stmt#1/h", true, kind("stmt#1/h") == Def);
  check(bool, "stmt#1/=>", true, kind("stmt#1/=>") == Tail);
  check(string, "=>", "a + 1", core_text(resolve(~root, seg, "=>")));
  /* a module body has no tail: its last `;`-less member is a member */
  let seg =
    seg_of("module M = {\n  let a = 1;\n  test a == 1 end\n} in\nM.a");
  check(bool, "M/test#1", true, resolve(seg, "M/test#1").kind == Test);
};

/* names win over kind words; any pattern name addresses its entry */
let names_src = "let stmt = 1 in\nstmt;\nlet (p, q) : (Int, Int) = (1, 2) in\nlet r :: rs = [1] in\nlet Some(s) = Some(1) in\nlet (t = u) = (t = 1) in\nlet _ = 3 in\nlet g(v, w) = v in\nstmt";
let names = () => {
  let seg = seg_of(names_src);
  let text = s => core_text(resolve(seg, s));
  check(string, "stmt#1 is the let", "let stmt = 1 in", text("stmt#1"));
  /* so the statement's own path picks the let: a known collision */
  let stmt = S.top(~root=Exp, seg).spans[1].anchor;
  check(
    option(string),
    "the statement's path",
    Some("stmt#1"),
    Option.map(P.to_string, P.describe(~root=Exp, seg, stmt)),
  );
  check(bool, "p", true, starts("let (p, q)", text("p")));
  check(bool, "q", true, starts("let (p, q)", text("q")));
  check(
    bool,
    "an ascription binds nothing",
    true,
    Result.is_error(P.resolve_string(~root=Exp, seg, "Int")),
  );
  check(bool, "rs", true, starts("let r :: rs", text("rs")));
  check(bool, "s", true, starts("let Some(s)", text("s")));
  check(bool, "u, not the label t", true, starts("let (t = u)", text("u")));
  check(
    bool,
    "the label t",
    true,
    Result.is_error(P.resolve_string(~root=Exp, seg, "t")),
  );
  check(
    bool,
    "let#6: a nameless def",
    true,
    starts("let _", text("let#6")),
  );
  check(bool, "g", true, starts("let g(v, w)", text("g")));
  check(
    bool,
    "parameters are not entries",
    true,
    Result.is_error(P.resolve_string(~root=Exp, seg, "v")),
  );
};

/* every entry: its canonical path resolves to it, describing it gives
   that path, and the text reads back */
let round_trip = (~root, name, seg) => {
  let all = P.entries(~root, seg);
  List.iter(
    ((id, p)) => {
      let s = P.to_string(p);
      switch (P.parse(s)) {
      | Ok(p') when p' == p => ()
      | _ => Alcotest.fail(name ++ ": " ++ s ++ " does not read back")
      };
      switch (P.resolve(~root, seg, p)) {
      | Ok(f) when f.id == id => ()
      | Ok(_) => Alcotest.fail(name ++ ": " ++ s ++ " resolves elsewhere")
      | Error(e) => Alcotest.fail(name ++ ": " ++ s ++ ": " ++ P.message(e))
      };
      switch (P.describe(~root, seg, id)) {
      | Some(p') when p' == p => ()
      | Some(p') =>
        Alcotest.fail(
          name ++ ": " ++ s ++ " described as " ++ P.to_string(p'),
        )
      | None => Alcotest.fail(name ++ ": " ++ s ++ " not found")
      };
    },
    all,
  );
  List.length(all);
};

let small_round_trips = () => {
  ignore(round_trip(~root=Exp, "example", seg_of(example)));
  ignore(round_trip(~root=Mod, "Mod root", seg_of(~root=Mod, mod_src)));
  List.iter(
    ((name, src)) =>
      ignore(round_trip(~root=Mod, name, seg_of(~root=Mod, src))),
    Test_ItemSpans.letin_members,
  );
  List.iter(
    ((name, src)) => ignore(round_trip(~root=Exp, name, seg_of(src))),
    CorpusUtil.edge_programs,
  );
};

/* ---- corpora ---- */

let read_any = paths => List.find_map(CorpusUtil.read_file, paths);
let here = path => read_any([path, "../" ++ path]);

let corpora = (): list((string, Sort.t, Segment.t)) => {
  let list_src =
    Option.get(here("test/modaudit/List.hz"))
    |> Util.StringUtil.strip_final_newline;
  [
    ("Mega 1k", Sort.Exp, CorpusUtil.mega_seg(~root=Exp, "mega-1k.hz")),
    ("Mega-Mod 1k", Mod, CorpusUtil.mega_seg(~root=Mod, "mega-mod-1k.hz")),
    ("List at a Mod root", Mod, seg_of(~root=Mod, list_src)),
    (
      "List at an Exp root",
      Exp,
      seg_of(~root=Exp, list_src ++ " in\nList.length([1])"),
    ),
  ]
  @ List.filter_map(
      ((path, src)) =>
        CorpusUtil.contains("docs/reference/module", path)
          ? Some((path, Sort.Exp, seg_of(src))) : None,
      CorpusUtil.slide_corpus(),
    );
};

let corpus_round_trips = () =>
  List.iter(
    ((name, root, seg)) => {
      let n = round_trip(~root, name, seg);
      Printf.printf("%s: %d entries round-trip\n", name, n);
    },
    corpora(),
  );

/* every binding the agent's node map addresses: reached by a path with
   the same names to the same id, and by the node map's own path, which is
   then its description, when that takes no `#k`. It reaches further than blocks (lets inside
   parens, case rules, branches), which paths leave to ids; its parent is
   then reached. Printed: those, and the entries only paths reach */
let parity_of = (~root, name, seg) => {
  let z = Zipper.unzip(seg);
  let info_map = CompositionGo.Public.mk_statics(~root, z);
  let nm = Option.get(HighLevelNodeMap.build(z, info_map));
  let names_of = (node: HighLevelNodeMap.node) =>
    List.map(HighLevelNodeMap.id_to_name(nm), node.path);
  let is_ident = n => Token.is_var(n) || Token.is_typ_var(n);
  /* the node map names a pattern by its shape: `(a, b)`, `{wild}` */
  let same_name = (n: string, el: P.elem) =>
    switch (el) {
    | Name(x, _) => n == x || !is_ident(n) && CorpusUtil.contains(x, n)
    | Nth(Def, _) => !is_ident(n)
    | _ => false
    };
  let fail = msg => Alcotest.fail(name ++ ": " ++ msg);
  let reached = ref(0);
  let own = ref(0);
  let missed = ref([]);
  Util.Id.Map.iter(
    (id, node: HighLevelNodeMap.node) => {
      let names = names_of(node);
      let shown = String.concat("/", names);
      switch (P.describe(~root, seg, id)) {
      | None =>
        switch (HighLevelNodeMap.parent_id_of(node)) {
        | Some(parent) when P.describe(~root, seg, parent) == None =>
          fail(shown ++ ": neither it nor its parent is reached")
        | _ => missed := [shown, ...missed^]
        }
      | Some(p) =>
        incr(reached);
        let s = P.to_string(p);
        if (List.length(p.elems) != List.length(names)
            || !List.for_all2(same_name, names, p.elems)) {
          fail(shown ++ " is described as " ++ s);
        };
        switch (P.resolve(~root, seg, p)) {
        | Ok(f) when f.id == id => ()
        | _ => fail(s ++ " does not resolve to " ++ shown)
        };
        if (HighLevelNodeMap.path_to_id_opt(nm, shown) == Some(id)
            && List.for_all(is_ident, names)) {
          incr(own);
          switch (P.resolve_string(~root, seg, shown)) {
          | Ok(f) when f.id == id && s == shown => ()
          | Ok(f) when f.id == id => fail(shown ++ " is described as " ++ s)
          | Ok(_) => fail(shown ++ " resolves elsewhere")
          | Error(e) => fail(shown ++ ": " ++ P.message(e))
          };
        };
      };
    },
    nm,
  );
  let only =
    P.entries(~root, seg)
    |> List.filter(((id, _)) => !Util.Id.Map.mem(id, nm))
    |> List.map(((_, p)) => p);
  let kinds = (ps: list(P.t)) => {
    let kind_of = p =>
      switch (P.resolve(~root, seg, p)) {
      | Ok(f) => P.kind_word(f.kind)
      | Error(_) => "?"
      };
    List.sort_uniq(compare, List.map(kind_of, ps))
    |> List.map(k =>
         Printf.sprintf(
           "%d %s",
           List.length(List.filter(p => kind_of(p) == k, ps)),
           k,
         )
       )
    |> String.concat(", ");
  };
  Printf.printf(
    "%s: node map %d: %d reached (%d by its own path), %d not (inside a parent's non-block body); paths only: %d (%s)\n",
    name,
    Util.Id.Map.cardinal(nm),
    reached^,
    own^,
    List.length(missed^),
    List.length(only),
    kinds(only),
  );
  List.iter(
    s => Printf.printf("  node map only: %s\n", s),
    List.sort_uniq(compare, missed^),
  );
};

let parity = () =>
  List.iter(((name, root, seg)) => parity_of(~root, name, seg), corpora());

/* ItemSpans keeps a member's let chain and a definition's body run while
   their pieces are, but not where they sit: an earlier member's edit moves
   them */
let sub_offsets = () => {
  let root = Sort.Mod;
  let seg =
    seg_of(~root, "let a = 1;\nlet x = 1 in x;\nlet g = let q = 2 in q");
  let space =
    switch (List.find(Piece.is_space, seg)) {
    | Secondary(w) =>
      Piece.Secondary({
        ...w,
        id: Id.mk(),
      })
    | _ => Alcotest.fail("no space")
    };
  let offsets = seg => {
    let top = S.top(~root, seg);
    (
      Option.get(S.chain(top, 1)).offset,
      Option.get(S.block(top, 2)).offset,
    );
  };
  let (chain, block) = offsets(seg);
  let (chain', block') = offsets([List.hd(seg), space, ...List.tl(seg)]);
  check(int, "the chain moves", chain + 1, chain');
  check(int, "the body run moves", block + 1, block');
};

/* ---- cost ---- */

let time = f => {
  let t0 = Sys.time();
  let r = f();
  (r, (Sys.time() -. t0) *. 1000.);
};

/* a level's index is built once, while ItemSpans keeps the level: an edit
   rebuilds those on the edited path */
let index_reuse = () => {
  let root = Sort.Mod;
  let seg = CorpusUtil.mega_seg(~root, "mega-mod-1k.hz");
  let paths = List.map(snd, P.entries(~root, seg));
  let all = seg => List.iter(p => ignore(P.resolve(~root, seg, p)), paths);
  all(seg);
  let b0 = P.built^;
  all(seg);
  check(int, "resolving again builds no index", b0, P.built^);
  let (edited, found) =
    CorpusUtil.edit_token(
      ~at=`First,
      ~keep_id=true,
      ~needle="180",
      ~repl="181",
      seg,
    );
  check(bool, "a token to edit", true, found);
  let b1 = P.built^;
  all(edited);
  let rebuilt = P.built^ - b1;
  Printf.printf("an edit in one member rebuilds %d indexes\n", rebuilt);
  check(bool, "the edited path's levels only", true, rebuilt <= 3);
};

/* resolving every entry's path at 4k, and describing every entry; and the
   node map's lookups for comparison when benchmarking */
let cost = () => {
  let root = Sort.Exp;
  let seg = CorpusUtil.mega_seg(~root, "mega-4k.hz");
  let (entries, cold) = time(() => P.entries(~root, seg));
  let n = List.length(entries);
  let resolve_all = () =>
    List.iter(((_, p)) => ignore(P.resolve(~root, seg, p)), entries);
  let ((), warm) = time(resolve_all);
  let ((), described) =
    time(() =>
      List.iter(((id, _)) => ignore(P.describe(~root, seg, id)), entries)
    );
  let depth =
    List.fold_left(
      (m, (_, p: P.t)) => max(m, List.length(p.elems)),
      0,
      entries,
    );
  /* after an edit deep in the program: the edited path's levels anew */
  let (edited, _) =
    CorpusUtil.edit_token(
      ~at=`Last,
      ~keep_id=true,
      ~needle="1",
      ~repl="2",
      seg,
    );
  let (last_id, last) =
    Util.ListUtil.last(
      List.filter(((_, p: P.t)) => List.length(p.elems) == depth, entries),
    );
  let (_, resolve_after) = time(() => P.resolve(~root, edited, last));
  let (_, describe_after) = time(() => P.describe(~root, edited, last_id));
  Printf.printf(
    "mega-4k: %d entries, depth <= %d; all paths with indexes built %.1f ms; resolve %.4f ms each; describe %.4f ms each; after an edit, the first resolve %.2f ms, the first describe %.2f ms\n",
    n,
    depth,
    cold,
    warm /. float_of_int(n),
    described /. float_of_int(n),
    resolve_after,
    describe_after,
  );
  if (CorpusUtil.bench_enabled) {
    let z = Zipper.unzip(seg);
    let info_map = CompositionGo.Public.mk_statics(~root, z);
    let nm = Option.get(HighLevelNodeMap.build(z, info_map));
    let paths =
      Util.Id.Map.bindings(nm)
      |> List.map(((_, node: HighLevelNodeMap.node)) =>
           String.concat(
             "/",
             List.map(HighLevelNodeMap.id_to_name(nm), node.path),
           )
         );
    let ((), t) =
      time(() =>
        List.iter(
          p => ignore(HighLevelNodeMap.path_to_id_opt(nm, p)),
          paths,
        )
      );
    let ((), mine) =
      time(() =>
        List.iter(p => ignore(P.resolve_string(~root, seg, p)), paths)
      );
    Printf.printf(
      "mega-4k, the node map's %d paths: node map %.4f ms each, EntryPath %.4f ms each\n",
      List.length(paths),
      t /. float_of_int(List.length(paths)),
      mine /. float_of_int(List.length(paths)),
    );
  };
};

let tests = (
  "EntryPath",
  [
    test_case("grammar", `Quick, grammar),
    test_case("resolving", `Quick, resolving),
    test_case("errors", `Quick, errors),
    test_case("program order", `Quick, program_order),
    test_case("a shared name's rest", `Quick, shared_prefix),
    test_case("Mod root", `Quick, mod_root),
    test_case("names", `Quick, names),
    test_case("round trips", `Quick, small_round_trips),
    test_case("sub-level offsets", `Quick, sub_offsets),
    test_case("corpus round trips", `Slow, corpus_round_trips),
    test_case("node map parity", `Slow, parity),
    test_case("indexes reused", `Slow, index_reuse),
    test_case("cost at 4k", `Slow, cost),
  ],
);
