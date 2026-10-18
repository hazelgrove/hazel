open Haz3lcore;

/* programs for tests: the corpora under hazel-programs (the mega programs
   and the slides), small edge cases, parsing, and edits as the editor
   makes them. paths resolve from the repo root (run_node.sh) or one level
   down (dune) */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | ic =>
    let n = in_channel_length(ic);
    let s = really_input_string(ic, n);
    close_in(ic);
    Some(s);
  | exception _ => None
  };

/* HAZEL_BENCH=1: timing the suite, so the runner skips the statics oracle */
let bench_enabled = Sys.getenv_opt("HAZEL_BENCH") == Some("1");

let mega_path = (name: string): string => {
  let path = "hazel-programs/mega/" ++ name;
  Sys.file_exists(path) ? path : "../hazel-programs/mega/" ++ name;
};

let mega_src = (name: string): option(string) =>
  read_file(mega_path(name));

let parse = (~root: Sort.t=Exp, src: string): option(Segment.t) =>
  FastParse.of_text(
    ~materialize=Triggers.invoked_projector,
    ~collect_refractors=true,
    ~root,
    src,
  );

/* as if typed: the text parser keeps unfinished forms and grout */
let typed_seg = (~root: Sort.t=Exp, src: string): option(Segment.t) =>
  Option.map(Zipper.unselect_and_zip, MarkerParse.of_text(~root, src));

/* the fast parser, else the text parser (slow at mega scale); fails the
   test on text neither reads */
let seg_of = (~root: Sort.t=Exp, src: string): Segment.t =>
  switch (parse(~root, src)) {
  | Some(seg) => seg
  | None =>
    switch (typed_seg(~root, src)) {
    | Some(seg) => seg
    | None => Alcotest.fail("unparseable: " ++ src)
    }
  };

/* typed into an empty editor key by key, as the editor reads it */
let typed_zipper = (~root: Sort.t=Exp, src: string): Zipper.t =>
  switch (Parser.to_zipper(~root, src)) {
  | Some(z) => z
  | None => Alcotest.fail("untypeable: " ++ src)
  };

/* a program's text, holes as the text parser's markers */
let text_of = (seg: Segment.t): string =>
  MarkerParse.to_text(Zipper.unzip(seg));

let corpus_seg = (~root: Sort.t=Exp, name: string): option(Segment.t) =>
  Option.bind(mega_src(name), parse(~root));

/* a mega program, failing the test if it doesn't read */
let mega_seg = (~root: Sort.t=Exp, name: string): Segment.t =>
  switch (corpus_seg(~root, name)) {
  | Some(seg) => seg
  | None =>
    Alcotest.fail(
      name
      ++ " unreadable or unparseable: "
      ++ Option.value(FastParse.bail_note^, ~default="?"),
    )
  };

/* every .hz program under hazel-programs outside mega/, as loaded:
   (path, text) */
let slide_corpus = (): list((string, string)) => {
  let rec find = (dir: string): list(string) =>
    switch (Sys.readdir(dir)) {
    | entries =>
      entries
      |> Array.to_list
      |> List.concat_map(entry => {
           let path = Filename.concat(dir, entry);
           switch (Sys.is_directory(path)) {
           | true => entry == "mega" ? [] : find(path)
           | false => Filename.check_suffix(entry, ".hz") ? [path] : []
           | exception _ => []
           };
         })
    | exception _ => []
    };
  let root =
    Sys.file_exists("hazel-programs")
      ? "hazel-programs" : "../hazel-programs";
  find(root)
  |> List.sort(compare)
  |> List.filter_map(path =>
       Option.map(
         src =>
           (
             path,
             src
             |> Util.StringUtil.trim_leading
             |> Util.StringUtil.strip_final_newline,
           ),
         read_file(path),
       )
     );
};

/* complete programs at the edges of per-item slicing: what sits between
   and around items, and lets off the binding-form spine */
let edge_programs = [
  ("two defs", "let a = 1 in\nlet b = 2 in\na + b"),
  ("same-line defs", "let a = 1 in let b = 2 in\na + b"),
  ("tail op tree", "let a = 1 in\na + 2 * 3 - 4"),
  ("continuation", "let a = 1 +\n2 +\n3 in\na"),
  ("type alias", "type t = Int in\nlet x: t = 1 in\nx"),
  ("test item", "let a = 1 in\ntest a == 1 end;\nlet b = a + 1 in\nb"),
  ("seq semis", "let f = fun x -> x in\nf(1); f(2); f(3)"),
  ("list adoption", "let xs = [1,\n2, 3] in\nxs"),
  ("multiline tuple", "let t = (1,\n2,\n3) in\nt"),
  (
    "case adoption",
    "let f = fun x ->\ncase x\n| 1 => 2\n| _ => 3\nend in\nf(0)",
  ),
  ("comments", "let a = 1 in\n# note #\nlet b = 2 in\na + b"),
  ("blank lines", "let a = 1 in\n\n\nlet b = 2 in\nb"),
  ("leading blank", "\nlet a = 1 in\na"),
  ("single expr", "1 + 2 * 3"),
  ("trailing lb", "let a = 1 in\na\n"),
  ("trailing blanks", "let a = 1 in\na\n\n"),
  ("tuple top", "let t = (1, 2) in\nt"),
  ("use", "use X in\nzz"),
  ("theorem", "theorem t = 1 in\nzz"),
  ("let under fun", "fun x ->\nlet y = x in\ny"),
  ("let under else", "if true then 1 else\nlet y = 2 in\ny"),
  ("let under operator", "1 +\nlet y = 2 in\ny"),
  ("let under fun under let", "let a = 1 in\nfun x ->\nlet y = x in\na"),
];

/* mid-typing states: unfinished forms, missing operands, open delimiters */
let incomplete_programs = [
  ("empty definition", "let x = in\nx"),
  ("unfinished last def", "let a = 1 in\nlet b ="),
  ("fun without a body", "let f = fun x -> in\nf(1)"),
  ("missing operand", "let a = 1 in\nlet b = a +\nin b"),
  ("unfinished case", "let a = 1 in\ncase a\n| 1 =>"),
  ("open paren", "let t = (1,\n2 in t"),
  ("open list", "let xs = [1, 2 in xs"),
  ("empty type", "type t = in 1"),
  ("bare keyword", "let a = 1 in\nlet"),
  ("unfinished member", "module M = {\n  let x = 1;\n  let y =\n} in M.x"),
  ("if without else", "let a = 1 in\nif a == 1 then 2"),
  ("def without body", "let a = 1 in\nlet b = 2"),
  /* body holes under use/theorem, or off the binding-form spine */
  ("use", "use X in\nzz"),
  ("theorem", "theorem t = 1 in\nzz"),
  ("operand above a let", "let a = 1 in\nx\nlet b = 2 in\nzz"),
  ("let under fun", "fun x ->\nlet y = x in\nzz"),
  ("let under else", "if true then 1 else\nlet y = 2 in\ny"),
  ("let under operator", "1 +\nlet y = 2 in\ny"),
];

/* retoken [needle] tiles to [repl], leaving untouched pieces physically
   shared as a real edit does: every match, or ~at the first or last in
   document order; under a fresh id, or ~keep_id as typing into a token
   does. returns whether any matched */
let edit_token =
    (
      ~at: [
         | `All
         | `First
         | `Last
       ]=`All,
      ~keep_id=false,
      ~needle: string,
      ~repl: string,
      seg: Segment.t,
    )
    : (Segment.t, bool) => {
  let is_match = (t: Tile.t) => Tile.label(t) == [needle];
  let rec matches = (seg: Segment.t): int =>
    List.fold_left(
      (n, p: Piece.t) =>
        switch (p) {
        | Tile(t) when is_match(t) => n + 1
        | Tile(t) =>
          List.fold_left((n, kid) => n + matches(kid), n, t.children)
        | _ => n
        },
      0,
      seg,
    );
  let chosen =
    switch (at) {
    | `All => (_ => true)
    | `First => (i => i == 0)
    | `Last =>
      let last = matches(seg) - 1;
      (i => i == last);
    };
  let seen = ref(0);
  /* in order, as the match count runs left to right */
  let map_in_order = (f, xs) =>
    List.rev(List.fold_left((acc, x) => [f(x), ...acc], [], xs));
  let shared = (old, xs) => List.for_all2((===), old, xs) ? old : xs;
  let rec go = (seg: Segment.t): Segment.t =>
    shared(seg, map_in_order(piece, seg))
  and piece = (p: Piece.t): Piece.t =>
    switch (p) {
    | Tile(t) when is_match(t) =>
      let i = seen^;
      incr(seen);
      chosen(i)
        ? Tile({
            ...t,
            id: keep_id ? t.id : Id.mk(),
            form: Form.Tok(repl),
          })
        : p;
    | Tile(t) =>
      let children = shared(t.children, map_in_order(go, t.children));
      children === t.children
        ? p
        : Tile({
            ...t,
            children,
          });
    | _ => p
    };
  let seg' = go(seg);
  (seg', seg' !== seg);
};

/* a physically fresh copy, ids kept: a change the incremental layers can
   see, with nothing in it changed */
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

/* the first tile of the one token [label], in document order */
let tile = (label: string, seg: Segment.t): Id.t => {
  let rec find = (seg: Segment.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when Tile.label(t) == [label] => Some(t.id)
        | Tile(t) => List.find_map(find, t.children)
        | _ => None
        },
      seg,
    );
  switch (find(seg)) {
  | Some(id) => id
  | None => Alcotest.fail("no tile " ++ label)
  };
};

/* the last tile of the one token [label], in document order */
let last_tile = (label: string, seg: Segment.t): Id.t => {
  let rec find = (acc, seg: Segment.t) =>
    List.fold_left(
      (acc, p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          List.fold_left(
            find,
            Tile.label(t) == [label] ? Some(t.id) : acc,
            t.children,
          )
        | _ => acc
        },
      acc,
      seg,
    );
  switch (find(None, seg)) {
  | Some(id) => id
  | None => Alcotest.fail("no tile " ++ label)
  };
};

/* the parses an edit at piece [id] costs the per-item parse: its item's,
   and the shell of each enclosing module literal or let-chain member
   (ItemSpans' child levels) */
let parses_for = (~root: Sort.t=Exp, seg: Segment.t, id: Id.t): int => {
  let holds = (ps: Segment.t) => List.mem(id, Segment.IDs.all(ps));
  let rec go = (lv: ItemSpans.level) =>
    switch (
      List.find_opt(
        k => holds(ItemSpans.slice(lv, k)),
        List.init(Array.length(lv.spans), k => k),
      )
    ) {
    | Some(k) =>
      switch (ItemSpans.children(lv, k)) {
      | Some(cl) when holds(cl.seg) => 1 + go(cl)
      | _ => 1
      }
    | None => 1
    };
  go(ItemSpans.top(~root, seg));
};

/* the first outline row labeled [label], depth first */
let row_by_label = (term: Language.Exp.t, label: string): Id.t =>
  switch (
    List.find_map(
      (n: Web.OutlineTree.node) => n.o_label == label ? n.o_id : None,
      Web.OutlineTree.(flatten(of_term(term))),
    )
  ) {
  | Some(id) => id
  | None => Alcotest.fail("no row " ++ label)
  };

/* the outline row at [path] (labels from the top) */
let row = (term: Language.Exp.t, path: list(string)): Id.t => {
  let rec go = (nodes: list(Web.OutlineTree.node), path) =>
    switch (path) {
    | [] => None
    | [l, ...rest] =>
      List.find_opt((n: Web.OutlineTree.node) => n.o_label == l, nodes)
      |> Option.map((n: Web.OutlineTree.node) =>
           rest == [] ? n.o_id : go(n.o_children, rest)
         )
      |> Option.join
    };
  switch (go(Web.OutlineTree.of_term(term), path)) {
  | Some(id) => id
  | None => Alcotest.fail("no row " ++ String.concat("/", path))
  };
};

let contains = (needle: string, hay: string): bool => {
  let (nl, hl) = (String.length(needle), String.length(hay));
  let rec go = i =>
    i + nl <= hl && (String.sub(hay, i, nl) == needle || go(i + 1));
  go(0);
};

/* mega-scale slides skip the super-linear gates (typing parse, roundtrip);
   substring match, as names carry folder prefixes ("Perf / Mega 1k") */
let mega_scale = (name: string): bool => contains("Mega", name);
