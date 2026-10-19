open Alcotest;
open Haz3lcore;
open Language;

/* completion in module members (CanonicalCompletion's member boundaries):
   an unfinished member ends at the next member line, as an unfinished
   definition does at the top level, and gets the `;` a definition gets
   its `in` from */

let settings = CoreSettings.on;

/* [a] applied as the editor applies it; a refused action leaves [z] */
let perform = (~root: Sort.t, z: Zipper.t, a: Action.t): Zipper.t => {
  let statics =
    CachedStatics.init_from_term(
      ~settings,
      ~is_dynamic_term=true,
      MakeTerm.from_zip_for_sem(z, ~root).term,
    );
  switch (
    Perform.go(
      ~settings,
      ~statics,
      ~syntax=CachedSyntax.init(~root, z),
      ~root,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => z
  | Error(_) => z
  };
};

/* [src] read, with the caret at the end of its line [line] */
let at_line = (~root: Sort.t, src: string, line: string): Zipper.t => {
  let rec row = (k, ls) =>
    switch (ls) {
    | [] => fail("no line: " ++ line)
    | [l, ..._] when l == line => k
    | [_, ...ls] => row(k + 1, ls)
    };
  perform(
    ~root,
    Zipper.unzip(CorpusUtil.seg_of(~root, src)),
    Move(
      Point(
        {
          row: row(0, String.split_on_char('\n', src)),
          col: String.length(line),
        },
        None,
      ),
    ),
  );
};

/* the program after each key of a new line typed at [z] */
let type_line = (~root: Sort.t, z: Zipper.t, keys: string): list(Zipper.t) =>
  String.fold_left(
    ((z, acc), c) => {
      let z = perform(~root, z, Insert(String.make(1, c)));
      (z, [z, ...acc]);
    },
    (z, []),
    "\n" ++ keys,
  )
  |> snd
  |> List.rev;

let text = (z: Zipper.t): string =>
  Printer.of_segment(
    ~holes="?",
    ~concave_holes="~",
    Zipper.unselect_and_zip(~erase_buffer=true, z),
  );

/* an outline row as the sidebar shows it: label, kind, its binder's type,
   its error badge, and whether it has rows below (a module's type grows
   with its members, so only a leaf's type is compared) */
type row = {
  label: string,
  kind: Web.OutlineTree.kind,
  ty: string,
  err: bool,
  kids: bool,
};

let kind_name = (k: Web.OutlineTree.kind): string =>
  switch (k) {
  | KModule => "module"
  | KFn => "fn"
  | KConst => "const"
  | KType => "type"
  | KTest => "test"
  | KTests => "tests"
  | KStmt => "stmt"
  | KUse => "use"
  | KTrail => "trail"
  };

/* the rows, each under its parent's label path; an error badges the
   deepest row containing it, as OutlineControl places it */
let rows = (~root: Sort.t, z: Zipper.t): list((string, row)) => {
  module O = Web.OutlineTree;
  let st =
    CachedStatics.init_compositional(~settings, ~stitch=Fun.id, ~root, z);
  let nodes = O.of_term(st.term);
  let ids = O.collect(nodes);
  let badged =
    List.filter_map(
      err =>
        List.find_opt(
          id => List.mem(id, ids),
          switch (Id.Map.find_opt(err, st.info_map)) {
          | Some(info) => [
              err,
              ...Info.within_item(~info_map=st.info_map, info),
            ]
          | None => [err]
          },
        ),
      st.error_ids,
    );
  let ty_of = (n: O.node) =>
    switch (
      Option.bind(n.o_binder, ((b, _)) => Id.Map.find_opt(b, st.info_map))
    ) {
    | Some(InfoPat({ty, _})) => Typ.pretty_print(ty)
    | _ => ""
    };
  let rec go = (parent, ns: list(O.node)) =>
    List.concat_map(
      (n: O.node) =>
        (
          switch (n.o_id) {
          | Some(id) => [
              (
                parent,
                {
                  label: n.o_label,
                  kind: n.o_kind,
                  ty: ty_of(n),
                  err: List.mem(id, badged),
                  kids: n.o_children != [],
                },
              ),
            ]
          | None => []
          }
        )
        @ go(parent ++ "/" ++ n.o_label, n.o_children),
      ns,
    );
  go("", nodes);
};

let show_row = (r: row) =>
  Printf.sprintf(
    "%s%s : %s",
    r.label == "" ? kind_name(r.kind) : r.label,
    r.err ? " [error]" : "",
    r.kids ? "" : r.ty,
  );

/* every row of [before] is in [after] as it was: a named row under the
   same label path, an unnamed one (a statement, a result) among its
   parent's unnamed rows; [after] may have more */
let check_kept = (name: string, before, after) => {
  let key = ((parent, r)) => parent ++ "/" ++ show_row(r);
  let rec take = (k, rs) =>
    switch (rs) {
    | [] => None
    | [r, ...rs] when key(r) == k => Some(rs)
    | [r, ...rs] => Option.map(rs => [r, ...rs], take(k, rs))
    };
  ignore(
    List.fold_left(
      (left, (parent, r: row) as b) =>
        switch (take(key(b), left)) {
        | Some(left) => left
        | None =>
          /* a named row changed in place: say how */
          switch (
            List.find_opt(
              ((p, r': row)) =>
                p == parent && r.label != "" && r'.label == r.label,
              left,
            )
          ) {
          | Some((_, r')) =>
            check(
              string,
              name ++ ": row " ++ r.label,
              show_row(r),
              show_row(r'),
            );
            left;
          | None => fail(name ++ ": lost row " ++ key(b))
          }
        },
      after,
      before,
    ),
  );
};

let labels = (rs: list((string, row))): list(string) =>
  List.map(((_, r)) => show_row(r), rs);

/* the rows under their parents' label paths */
let paths = (rs: list((string, row))): list(string) =>
  List.map(((parent, r)) => parent ++ "/" ++ show_row(r), rs);

/* ---- the Modules slide's shapes ---- */

let counter = "module Counter = {\n  let start = 0;\n  let next = start + 1;\n  let after = next + 1\n} in\nCounter.after";

let geometry = "module Geometry = {\n  type Point = (Int, Int);\n  type Distance = Int;\n  let origin : Point = (0, 0);\n  let manhattan =\n    fun (x, y) : Point -> x + y\n} in\nlet pt : Geometry.Point = (3, 4) in\nlet dist : Geometry.Distance = Geometry.manhattan(pt) in\ndist";

let nested = "module Outer = {\n  module Inner = {\n    type T = Bool;\n    let value = true\n  };\n  let o = 1\n} in\nlet b : Outer.Inner.T = Outer.Inner.value in\nb";

/* a member that is a let chain, between plain members */
let chained = "module H = {\n  let x = 1;\n  let a = 1 in\n  let b = a + x in\n  a + b;\n  let z = 4\n} in\nH.z + H.x";

/* the browser bugs: the member below an unfinished one stays a member */
let unfinished = (name, src, ~after, keys, expect, ()) => {
  let z = at_line(~root=Exp, src, after);
  let z = Util.ListUtil.last(type_line(~root=Exp, z, keys));
  check(list(string), name, expect, labels(rows(~root=Exp, z)));
};

let browser_cases = [
  (
    "Counter: `let q` above `let next = start + 1;`",
    counter,
    "  let start = 0;",
    "let q",
    [
      "Counter : ",
      "start : Int",
      "q : ?",
      "next : Int",
      "after : Int",
      "trail : ",
    ],
  ),
  (
    "Geometry: `let q = (1 + 2` above the `;`-less last member",
    geometry,
    "  let origin : Point = (0, 0);",
    "let q = (1 + 2",
    [
      "Geometry : ",
      "Point : ",
      "Distance : ",
      "origin : Point",
      "q : Int",
      "manhattan : Point -> Int",
      "pt : Geometry.Point",
      "dist : Geometry.Distance",
      "trail : ",
    ],
  ),
  /* finished but for its `;`: the grout between the members separates
     them (a member slice read Mod-sorted) */
  (
    "Geometry: `let q = (1 + 2)` above the `;`-less last member",
    geometry,
    "  let origin : Point = (0, 0);",
    "let q = (1 + 2)",
    [
      "Geometry : ",
      "Point : ",
      "Distance : ",
      "origin : Point",
      "q : Int",
      "manhattan : Point -> Int",
      "pt : Geometry.Point",
      "dist : Geometry.Distance",
      "trail : ",
    ],
  ),
];

/* what each unfinished member reads as: completed on its own line, ended
   with its `;` (printing drops it), the members below it kept */
let sibling_keys = [
  ("`let q`", "let q", "let q = ?;"),
  ("`let q =` (finished, its definition empty)", "let q =", "let q = ?;"),
  ("a trailing operator", "let q = 1 +", "let q = 1 + ?;"),
  ("`fun x` missing `->`", "let q = fun x", "let q = fun x -> ?;"),
  (
    "`if` missing `else`",
    "let q = if true then 1",
    "let q = if true then 1 else ?;",
  ),
  (
    "`if` missing `then`",
    "let q = if true",
    "let q = if true then ? else ?;",
  ),
  (
    "`case` missing `end`",
    "let q = case 1 | 1 => 2",
    "let q = case 1 | 1 => 2 end;",
  ),
  ("`[` missing `]`", "let q = [1, 2", "let q = [1, 2];"),
  ("`type T` missing `=`", "type T", "type T = ?;"),
  ("`module N` missing `=`", "module N", "module N = ?;"),
  ("`test` missing `end`", "test 1 == 1", "test 1 == 1 end;"),
];

/* the completed program's third line: a member typed after Counter's
   first */
let completed_line = (z: Zipper.t): string =>
  switch (
    String.split_on_char(
      '\n',
      Printer.of_segment(
        ~holes="?",
        ~concave_holes="~",
        CanonicalCompletion.complete_items(
          ~sort=Exp,
          MakeTerm.semantic_source(z),
        ).
          completed_seg,
      ),
    )
  ) {
  | [_, _, line, ..._] => line
  | lines => String.concat(" / ", lines)
  };

let sibling = ((name, keys, expect), ()) => {
  let z0 = at_line(~root=Exp, counter, "  let start = 0;");
  let before = rows(~root=Exp, z0);
  let z = Util.ListUtil.last(type_line(~root=Exp, z0, keys));
  /* spacing aside: the printer glues synthesized delimiters */
  let squash = s => String.concat("", String.split_on_char(' ', s));
  check(
    string,
    name ++ ": completed",
    squash(expect),
    squash(completed_line(z)),
  );
  check_kept(name, before, rows(~root=Exp, z));
};

/* an unfinished nested module: its `}` closes before the outer module's
   next member line, which stays the outer module's */
let nested_unfinished = () => {
  let z0 = at_line(~root=Exp, counter, "  let start = 0;");
  let before = rows(~root=Exp, z0);
  let z =
    Util.ListUtil.last(type_line(~root=Exp, z0, "module N = {\nlet z = 2"));
  let after = rows(~root=Exp, z);
  check_kept("a nested module typed", before, after);
  check(
    bool,
    "N holds z: " ++ String.concat(", ", labels(after)),
    true,
    List.exists(
      ((parent, r: row)) => parent == "/Counter/N" && r.label == "z",
      after,
    ),
  );
};

/* ---- feedback preservation: a member typed key by key at a module's
   top, middle and end leaves every other row as it was, but at the keys
   where the program is finished: then it reads as written. `let q =`
   above a let chain (no member head) takes the chain as its definition,
   as the language reads it ---- */

let typing_cases = [
  ("Counter, top", counter, "module Counter = {", []),
  ("Counter, middle", counter, "  let start = 0;", []),
  ("Counter, end", counter, "  let after = next + 1", []),
  ("Geometry, top", geometry, "module Geometry = {", []),
  ("Geometry, middle", geometry, "  type Distance = Int;", []),
  ("Geometry, end", geometry, "    fun (x, y) : Point -> x + y", []),
  ("nested, inner top", nested, "  module Inner = {", []),
  ("nested, inner end", nested, "    let value = true", []),
  ("nested, outer middle", nested, "  };", []),
  ("let chain, before the chain", chained, "  let x = 1;", [7, 8]),
  ("let chain, after the chain", chained, "  a + b;", []),
];

let typed_keys = "let q = (1 + 2";

let preserved = ((name, src, after, finished), ()) => {
  let z0 = at_line(~root=Exp, src, after);
  let before = rows(~root=Exp, z0);
  List.iteri(
    (k, z) => {
      let at = Printf.sprintf("%s, key %d (%S)", name, k, text(z));
      if (List.mem(k, finished)) {
        check(
          int,
          at ++ ": finished, read as written",
          0,
          List.length(
            CanonicalCompletion.complete_items(
              ~sort=Exp,
              MakeTerm.semantic_source(z),
            ).
              shard_records,
          ),
        );
      } else {
        check_kept(at, before, rows(~root=Exp, z));
      };
    },
    type_line(~root=Exp, z0, typed_keys),
  );
};

/* a new link typed into a let-chain member is the chain's: its open `in`
   takes the line below as its body, as at the top level */
let chain_link = () => {
  let z0 = at_line(~root=Exp, chained, "  let a = 1 in");
  let before = rows(~root=Exp, z0);
  let z = Util.ListUtil.last(type_line(~root=Exp, z0, "let c = 2"));
  check_kept("a chain link", before, rows(~root=Exp, z));
  let seg = MakeTerm.semantic_source(z);
  let done_ = CanonicalCompletion.complete_items(~sort=Exp, seg);
  check(
    bool,
    "the link's `in` synthesized, no `;`",
    true,
    List.length(done_.shard_records) == 1,
  );
};

/* ---- a member started on its module's `{` line counts as indented at
   the module's member level, where Enter indents its own-line members: a
   member line below, at-or-left of it, ends it as it ends a member on a
   line of its own, and a line indented past it continues it. The syntax
   parse's cut follows ---- */

let brace_line = (name, (root, src, own, lines), ()) => {
  let typed = src =>
    switch (CorpusUtil.typed_seg(~root, src)) {
    | Some(seg) => seg
    | None => fail("untypeable: " ++ src)
    };
  let seg = typed(src);
  let completed =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="~",
      CanonicalCompletion.complete_items(~sort=root, seg).completed_seg,
    );
  let squash = s => String.concat("", String.split_on_char(' ', s));
  check(
    list(string),
    name ++ ": completed",
    lines,
    Util.ListUtil.take(
      List.length(lines),
      List.map(squash, String.split_on_char('\n', completed)),
    ),
  );
  check(
    list(string),
    name ++ ": the rows of a member on a line of its own",
    labels(rows(~root, Zipper.unzip(typed(own)))),
    labels(rows(~root, Zipper.unzip(seg))),
  );
  /* the member's slice in the syntax parse: its own lines */
  let top = ItemSpans.top(~ends=Read, ~root, seg);
  let members = Option.get(ItemSpans.children(top, 0));
  check(
    int,
    name ++ ": the syntax parse's member ends with its lines",
    List.length(lines),
    List.length(
      String.split_on_char(
        '\n',
        String.trim(CorpusUtil.text_of(ItemSpans.slice(members, 0))),
      ),
    ),
  );
};

let brace_line_cases = [
  (
    "at a Mod root",
    (
      Sort.Mod,
      "module M = {let q = (1\n  let next = 1;\n  let after = 2\n};\nM.next",
      "module M = {\n  let q = (1\n  let next = 1;\n  let after = 2\n};\nM.next",
      ["moduleM={letq=(1);"],
    ),
  ),
  (
    "in an expression",
    (
      Sort.Exp,
      "module M = {let q = (1\n  let next = 1;\n  let after = 2\n} in\nM.next",
      "module M = {\n  let q = (1\n  let next = 1;\n  let after = 2\n} in\nM.next",
      ["moduleM={letq=(1);"],
    ),
  ),
  (
    "a line past the member level continues it",
    (
      Sort.Mod,
      "module M = {let q = (1 +\n    2\n  let next = 1\n};\nM.next",
      "module M = {\n  let q = (1 +\n    2\n  let next = 1\n};\nM.next",
      ["moduleM={letq=(1+", "2);"],
    ),
  ),
  (
    "a line past its start column continues it",
    (
      Sort.Mod,
      "module M = {let q = (1 +\n               2\n  let next = 1\n};\nM.next",
      "module M = {\n  let q = (1 +\n    2\n  let next = 1\n};\nM.next",
      ["moduleM={letq=(1+", "2);"],
    ),
  ),
];

/* a nested module started on the outer `{` line, its `}` deleted: its
   members below, at its member level, stay its own, as when it starts a
   line of its own; the outer module's next member line ends it */
let brace_nested = () => {
  let root = Sort.Mod;
  let deleted = (src: string) => {
    let z0 = Zipper.unzip(CorpusUtil.seg_of(~root, src));
    switch (Test_ModAudit.point_of(~off=3, src, "  };")) {
    | Some(pt) =>
      List.fold_left(
        perform(~root),
        z0,
        [
          Action.Move(Point(pt, None)),
          Action.Destruct(Local(Left, ByChar)),
        ],
      )
    | None => fail("no `}`")
    };
  };
  let on_brace =
    deleted(
      "module M = {module N = {let a = 1;\n    let b = 2\n  };\n  let c = 3\n};\nM.c",
    );
  let own_line =
    deleted(
      "module M = {\n  module N = {let a = 1;\n    let b = 2\n  };\n  let c = 3\n};\nM.c",
    );
  check(
    list(string),
    "N holds a and b",
    [
      "/M : ",
      "/M/N : ",
      "/M/N/a : Int",
      "/M/N/b : Int",
      "/M/c : Int",
      "/stmt : ",
    ],
    paths(rows(~root, on_brace)),
  );
  check(
    list(string),
    "as on a line of its own",
    paths(rows(~root, own_line)),
    paths(rows(~root, on_brace)),
  );
  /* N's `}` closes after b, before the `;` */
  let completed =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="~",
      CanonicalCompletion.complete_items(
        ~sort=root,
        MakeTerm.semantic_source(on_brace),
      ).
        completed_seg,
    );
  check(
    list(string),
    "completed",
    ["module M = {module N = {let a = 1;", "let b = 2}", ";", "let c = 3"],
    Util.ListUtil.take(
      4,
      List.map(String.trim, String.split_on_char('\n', completed)),
    ),
  );
  /* the syntax parse cuts M's members alike: N through its `;`, then c */
  let top =
    ItemSpans.top(~ends=Read, ~root, Zipper.unselect_and_zip(on_brace));
  let members = Option.get(ItemSpans.children(top, 0));
  check(
    list(string),
    "the syntax parse's members",
    ["module N = {let a = 1; let b = 2 ;", "let c = 3"],
    List.map(
      s =>
        String.concat(
          " ",
          List.filter(
            l => l != "",
            List.map(
              String.trim,
              String.split_on_char('\n', CorpusUtil.text_of(s)),
            ),
          ),
        ),
      ItemSpans.slices(members),
    ),
  );
};

/* ---- a Mod root: the same, its members at the top level ---- */

let counter_members = "let start = 0;\nlet next = start + 1;\nlet after = next + 1";

let mod_root = ((name, after), ()) => {
  let z0 = at_line(~root=Mod, counter_members, after);
  let before = rows(~root=Mod, z0);
  List.iteri(
    (k, z) =>
      check_kept(
        Printf.sprintf("%s, key %d (%S)", name, k, text(z)),
        before,
        rows(~root=Mod, z),
      ),
    type_line(~root=Mod, z0, typed_keys),
  );
};

/* ---- deletion round-trips: each delimiter of a member, deleted as the
   editor deletes it, completes back to the program ---- */

let rec tokens = (seg: Segment.t): list(string) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Secondary(_)
      | Grout(_)
      | Projector(_) => []
      | Tile(t) =>
        let rec weave = (ls, chs) =>
          switch (ls, chs) {
          | ([], _) => []
          | ([l, ...ls], []) => [l, ...weave(ls, [])]
          | ([l, ...ls], [c, ...chs]) =>
            [l, ...tokens(c)] @ weave(ls, chs)
          };
        weave(Tile.effective_label(t), t.children);
      },
    seg,
  );

/* every shard of every multi-token tile: its token and the point after
   it */
let targets = (syntax: CachedSyntax.t): list((Token.t, Util.Point.t)) => {
  let acc = ref([]);
  let rec walk = (sg: Segment.t) =>
    List.iter(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          List.iter(walk, t.children);
          if (Tile.arity(t) > 1) {
            switch (Measured.find_shards(~msg="sweep", t, syntax.measured)) {
            | shards =>
              List.iter(
                ((i, m: Measured.measurement)) =>
                  acc := [(Tile.token(t, i), m.last), ...acc^],
                shards,
              )
            | exception _ => ()
            };
          };
        | _ => ()
        },
      sg,
    );
  walk(syntax.segment);
  List.rev(acc^);
};

/* the deletion left the original tokens less the deleted one (or its
   prefix): else the edit itself merged tokens, which no completion of
   the visible state can undo */
let evidence_intact =
    (~replacement: option(string), original, mutated, tok): bool => {
  let rec go = (pre, rest) =>
    switch (rest) {
    | [] => false
    | [x, ...tl] =>
      x == tok
      && List.rev_append(
           pre,
           switch (replacement) {
           | None => tl
           | Some(r) => [r, ...tl]
           },
         )
      == mutated
      || go([x, ...pre], tl)
    };
  go([], original);
};

type outcome = {
  restored: int,
  destroyed: int,
  total: int,
  misses: list(string),
};

/* delete each delimiter (whole, or ~prefix_only its last char), complete
   at the root as statics reads it, compare tokens */
let sweep = (~root: Sort.t, ~prefix_only=false, seg: Segment.t): outcome => {
  let settings = Test_Editing.default_settings;
  let z0 = Zipper.unzip(seg);
  let syntax = CachedSyntax.init(~root, z0);
  let original = tokens(seg);
  let act = (z, a) =>
    Option.bind(z, z =>
      switch (
        Perform.go(
          ~settings,
          ~statics=CachedStatics.empty,
          ~syntax,
          ~root,
          a,
          {
            zipper: z,
            col_target: None,
          },
        )
      ) {
      | Ok(z) => Some(z)
      | Error(_) => None
      | exception _ => None
      }
    );
  targets(syntax)
  |> List.filter(((tok, _)) => !prefix_only || Token.length(tok) > 1)
  |> List.fold_left(
       (o, (tok, pt: Util.Point.t)) => {
         let k = prefix_only ? 1 : Token.length(tok);
         let acts =
           [Action.Move(Point(pt, None))]
           @ List.init(k, _ => Action.Destruct(Local(Left, ByChar)));
         let where = Printf.sprintf("%s at %d:%d", tok, pt.row, pt.col);
         switch (List.fold_left(act, Some(z0), acts)) {
         | None => {
             ...o,
             total: o.total + 1,
             misses: ["INAPPLICABLE " ++ where, ...o.misses],
           }
         | Some(z) =>
           let seg = MakeTerm.semantic_source(z);
           let replacement =
             prefix_only
               ? Some(String.sub(tok, 0, Token.length(tok) - 1)) : None;
           if (!evidence_intact(~replacement, original, tokens(seg), tok)) {
             {
               ...o,
               destroyed: o.destroyed + 1,
               total: o.total + 1,
             };
           } else {
             let got =
               tokens(
                 CanonicalCompletion.complete_items(~sort=root, seg).
                   completed_seg,
               );
             got == original
               ? {
                 ...o,
                 restored: o.restored + 1,
                 total: o.total + 1,
               }
               : {
                 ...o,
                 total: o.total + 1,
                 misses: ["MISS " ++ where, ...o.misses],
               };
           };
         };
       },
       {
         restored: 0,
         destroyed: 0,
         total: 0,
         misses: [],
       },
     );
};

let show = (o: outcome) =>
  Printf.sprintf("%d/%d/%d", o.restored, o.destroyed, o.total);

/* every delimiter kind a member holds */
let kinds = "module M = {\n  type T = Int;\n  let f = fun x -> x + 1;\n  let g = if true then 1 else 2;\n  let h = (1, 2);\n  let l = [1, 2];\n  test 1 == 1 end;\n  let c = case 1 | 1 => 2 | _ => 3 end;\n  module N = {\n    let a = 1;\n    let b = 2\n  };\n  let k = let a = 1 in a + 1;\n  let a = 1 in\n  let b = a + 2 in\n  a + b;\n  let z = 0\n} in\nM.z";

let kinds_members = "type T = Int;\nlet f = fun x -> x + 1;\nlet g = if true then 1 else 2;\nlet h = (1, 2);\nlet l = [1, 2];\ntest 1 == 1 end;\nlet c = case 1 | 1 => 2 | _ => 3 end;\nmodule N = {\n  let a = 1;\n  let b = 2\n};\nlet k = let a = 1 in a + 1;\nlet a = 1 in\nlet b = a + 2 in\na + b;\nlet z = 0";

/* a module laid out as the standard library's are: multi-line members,
   blank lines between them, comments */
let option_shaped = "module Option = {\n  # the type #\n  type t = None + Some(?);\n\n  let map : (Option, ? -> ?) -> ? = fun (opt, f) ->\n    case opt\n    | None => None\n    | Some(x) => Some(f(x))\n    end;\n\n  let to_list : Option -> [?] = fun opt ->\n    if opt == None then [] else [1]\n};\nOption.to_list(None)";

/* the same definitions as a let-in chain: the control */
let option_chain = "# the type #\ntype t = None + Some(?) in\n\nlet map : (Option, ? -> ?) -> ? = fun (opt, f) ->\n  case opt\n  | None => None\n  | Some(x) => Some(f(x))\n  end in\n\nlet to_list : Option -> [?] = fun opt ->\n  if opt == None then [] else [1] in\nto_list(None)";

let option_members = "# the type #\ntype t = None + Some(?);\n\nlet map : (Option, ? -> ?) -> ? = fun (opt, f) ->\n  case opt\n  | None => None\n  | Some(x) => Some(f(x))\n  end;\n\nlet to_list : Option -> [?] = fun opt ->\n  if opt == None then [] else [1]";

/* (name, root, program, full, prefix): restored/destroyed/total, pinned.
   `kinds` misses only `h`'s `=`: `let h (1, 2)` reads as an ap-pattern, as
   at the top level (CompletionScoreboard's tuple-list). The library-style
   module misses what its let-in control misses (a type annotation's `(`,
   `)` or `[`, and `typ` for `type`): general, not member structure */
let round_trip_pins = [
  ("in a module", Sort.Exp, kinds, "54/0/55", "30/0/30"),
  ("at a Mod root", Sort.Mod, kinds_members, "49/0/50", "28/0/28"),
  (
    "in a library-style module",
    Sort.Mod,
    option_shaped,
    "32/5/41",
    "14/0/15",
  ),
  ("library-style members", Sort.Mod, option_members, "27/4/35", "13/0/14"),
  (
    "library-style let-in control",
    Sort.Exp,
    option_chain,
    "31/5/40",
    "16/0/17",
  ),
];

/* a deleted `;` between finished members: the grout it leaves parts them
   (a member slice read Mod-sorted), so no other row changes */
let semi_deleted = ((name, root, src), ()) => {
  let z0 = Zipper.unzip(CorpusUtil.seg_of(~root, src));
  let before = rows(~root, z0);
  let syntax = CachedSyntax.init(~root, z0);
  let rec semis = (sg: Segment.t): list(Id.t) =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when Tile.is_semi(t) => [t.id]
        | Tile(t) => List.concat_map(semis, t.children)
        | _ => []
        },
      sg,
    );
  List.iter(
    id =>
      switch (Measured.find_by_id(id, syntax.measured)) {
      | Some(m) =>
        let z =
          List.fold_left(
            perform(~root),
            z0,
            [
              Action.Move(Point(m.last, None)),
              Action.Destruct(Local(Left, ByChar)),
            ],
          );
        check_kept(
          Printf.sprintf("%s, `;` at %d:%d", name, m.last.row, m.last.col),
          before,
          rows(~root, z),
        );
      | None => ()
      },
    semis(Zipper.unselect_and_zip(z0)),
  );
};

/* a deleted head keyword restores with its pattern read as a pattern:
   annotated, the absorbed `x : T` was read as an expression ascription
   and bound nothing (general, not only members) */
let head_deleted = ((name, root, src, at, k), ()) => {
  let z0 = Zipper.unzip(CorpusUtil.seg_of(~root, src));
  let before = rows(~root, z0);
  let z =
    List.fold_left(
      perform(~root),
      z0,
      [Action.Move(Point(at, None))]
      @ List.init(k, _ => Action.Destruct(Local(Left, ByChar))),
    );
  check_kept(name, before, rows(~root, z));
};

/* a member's deleted `case` restores inside its definition, as a let-in
   definition's does: multi-line exactly; on one line at the definition's
   start, never before the member's `let` */
let case_deleted = ((name, src, at, expect), ()) => {
  let root = Sort.Mod;
  let z0 = Zipper.unzip(CorpusUtil.seg_of(~root, src));
  let z =
    List.fold_left(
      perform(~root),
      z0,
      [Action.Move(Point(at, None))]
      @ List.init(4, _ => Action.Destruct(Local(Left, ByChar))),
    );
  let got =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="~",
      CanonicalCompletion.complete_items(
        ~sort=root,
        MakeTerm.semantic_source(z),
      ).
        completed_seg,
    );
  let squash = s =>
    String.concat("", String.split_on_char(' ', s))
    |> String.split_on_char('\n')
    |> String.concat("");
  check(string, name, squash(expect), squash(got));
};

let round_trip = ((name, root, src, full, prefix), ()) => {
  let seg = CorpusUtil.seg_of(~root, src);
  let f = sweep(~root, seg);
  let p = sweep(~root, ~prefix_only=true, seg);
  List.iter(print_endline, List.rev(f.misses) @ List.rev(p.misses));
  print_endline(
    Printf.sprintf("SCORE %s: full %s, prefix %s", name, show(f), show(p)),
  );
  check(
    string,
    name,
    Printf.sprintf("full %s, prefix %s", full, prefix),
    Printf.sprintf("full %s, prefix %s", show(f), show(p)),
  );
};

/* ---- a finished level reads as written: layout never ends a finished
   member, as it never ends a finished definition at the top level ---- */

let as_written = [
  (
    "a definition on the line below, at the member's indentation",
    Sort.Exp,
    "module M = {\n  let f =\n  fun x -> x;\n  let g = f(1)\n} in\nM.g",
  ),
  ("an operator ending a line", Sort.Mod, "let total = 1 +\n2;\ntotal"),
  (
    "a sum type's variants below it",
    Sort.Mod,
    "type T =\n+ A\n+ B;\nlet t : T = A;\nt",
  ),
  /* MenhirFuzz's case: the fuzz broke the line before the sum's `+` */
  (
    "a sum, its line broken, ending a module",
    Sort.Exp,
    "let m = ({ type U = (); type V =\n+ Gx }) in [1]",
  ),
];

let reads_as_written = ((name, root, src), ()) => {
  let seg = CorpusUtil.seg_of(~root, src);
  let done_ = CanonicalCompletion.complete_items(~sort=root, seg);
  let print = Printer.of_segment(~holes="?", ~concave_holes="~");
  check(
    int,
    name ++ ": nothing synthesized",
    0,
    List.length(done_.shard_records),
  );
  check(
    string,
    name ++ ": as written",
    print(seg),
    print(done_.completed_seg),
  );
  if (root == Sort.Exp) {
    check(
      bool,
      name ++ ": MakeTerm agrees with Menhir",
      true,
      switch (
        Test_MenhirFuzz.make_term_parse(src),
        Test_MenhirFuzz.menhir_parse(src),
      ) {
      | (Some(mk), Some(mh)) => Test_MenhirFuzz.equal_terms(mk, mh)
      | _ => false
      },
    );
  };
};

/* ---- the chips read as semantics does: the display's completion reads
   a program's top level at its root, members an unclosed module literal
   spills into it aside ---- */

let display_cases = [
  (
    "a module typed into a let-in chain",
    Sort.Exp,
    "let start = 0 in\nlet next = start + 1 in\nnext",
    "let start = 0 in",
    "module N = { let z = 2 } in",
  ),
  (
    "a member typed at a Mod root",
    Sort.Mod,
    counter_members,
    "let start = 0;",
    "let q = (1 + 2",
  ),
  (
    "a module typed at a Mod root",
    Sort.Mod,
    counter_members,
    "let start = 0;",
    "module N = { let z = 2 };",
  ),
];

let display_agrees = ((name, root, src, after, keys), ()) =>
  List.iteri(
    (k, z) => {
      let source = MakeTerm.semantic_source(z);
      check(
        list(string),
        Printf.sprintf("%s, key %d (%S)", name, k, text(z)),
        tokens(fst(MakeTerm.semantic_segment(~root, source))),
        tokens(CanonicalCompletion.for_editor(~root, source).completed_seg),
      );
    },
    type_line(~root, at_line(~root, src, after), keys),
  );

/* the caret's chips, as the quiver draws them, at the end of [line]: read
   at the editor's [root], which the code alone doesn't tell when the top
   holds no member-level tile */
let chips_after = ((root, src, line, expect), ()) =>
  check(
    string,
    Sort.show(root) ++ ": " ++ String.escaped(src),
    expect,
    CompletionQuery.chips_at_caret(~root, at_line(~root, src, line))
    |> List.map((i: CanonicalCompletion.insertion) =>
         String.concat(
           " ",
           List.map(
             (d: CanonicalCompletion.delimiter_info) => d.text,
             i.delimiters,
           ),
         )
       )
    |> String.concat(" | "),
  );

let chip_cases = [
  (Sort.Mod, "f(1\nlet y = 2", "f(1", ") ;"),
  (Sort.Exp, "f(1\nlet y = 2", "f(1", ")"),
  (Sort.Mod, "test 1 ==\nlet y = 2", "test 1 ==", "end ;"),
];

/* ---- the corpus sweep, behind HAZEL_COMPLETION_SWEEP=1 (minutes):
   every delimiter of mega-mod-1k, the Modules slide and mega-1k ---- */

let corpus_sweep = () =>
  if (Sys.getenv_opt("HAZEL_COMPLETION_SWEEP") == Some("1")) {
    let slide =
      switch (
        CorpusUtil.read_file("hazel-programs/docs/reference/modules.hz"),
        CorpusUtil.read_file("../hazel-programs/docs/reference/modules.hz"),
      ) {
      | (Some(s), _)
      | (None, Some(s)) => s
      | (None, None) => fail("no Modules slide")
      };
    List.iter(
      ((name, root, seg)) => {
        let f = sweep(~root, seg);
        let p = sweep(~root, ~prefix_only=true, seg);
        List.iter(
          m => print_endline(name ++ " " ++ m),
          List.rev(f.misses) @ List.rev(p.misses),
        );
        print_endline(
          Printf.sprintf(
            "SWEEP %s: full %s, prefix %s",
            name,
            show(f),
            show(p),
          ),
        );
      },
      [
        (
          "mega-mod-1k",
          Sort.Mod,
          CorpusUtil.mega_seg(~root=Mod, "mega-mod-1k.hz"),
        ),
        ("modules slide", Sort.Exp, CorpusUtil.seg_of(slide)),
        ("mega-1k", Sort.Exp, CorpusUtil.mega_seg("mega-1k.hz")),
      ],
    );
  };

/* ---- a member head typed at a member line after a member that isn't
   open on the right starts a member, as after the member's `;`: the
   missing `;` is the grout deleting one leaves, and nothing is
   synthesized ---- */

let counter_unfinished = "module Counter = {\n  let start = 0;\n  let next = (start + 1\n  let after = next + 1\n} in\nCounter.after";

/* the rows are kept from the head keyword on (a prefix of one, `t` of
   `then`, reads as an operator meanwhile, as at the top level); the last
   state reads as [expect], with [synth] shards synthesized (an unfinished
   member's), and the line [marked] holding the missing `;`'s grout */
let member_head = ((name, root, src, after, keys, expect, synth, marked), ()) => {
  let z0 = at_line(~root, src, after);
  let before = rows(~root, z0);
  let zs = type_line(~root, z0, keys);
  let head = String.index(keys, ' ');
  List.iteri(
    (k, z) =>
      if (k >= head) {
        check_kept(
          Printf.sprintf("%s, key %d (%S)", name, k, text(z)),
          before,
          rows(~root, z),
        );
      },
    zs,
  );
  let z = Util.ListUtil.last(zs);
  check(list(string), name ++ ": rows", expect, paths(rows(~root, z)));
  check(
    int,
    name ++ ": synthesized",
    synth,
    List.length(
      CanonicalCompletion.complete_items(
        ~sort=root,
        MakeTerm.semantic_source(z),
      ).
        shard_records,
    ),
  );
  check(
    bool,
    name ++ ": the missing `;` marked",
    true,
    List.mem(
      marked,
      List.map(String.trim, String.split_on_char('\n', text(z))),
    ),
  );
};

let counter_rows = (~root: Sort.t, typed: list(string)) => {
  let (parent, trail) =
    root == Sort.Mod ? ("", []) : ("/Counter", ["/trail : "]);
  (root == Sort.Mod ? [] : ["/Counter : "])
  @ List.map(
      r => parent ++ "/" ++ r,
      ["start : Int", "next : Int", "after : Int"] @ typed,
    )
  @ trail;
};

let member_head_cases = [
  (
    "`let` after the last member",
    Sort.Exp,
    counter,
    "  let after = next + 1",
    "let z = 1",
    counter_rows(~root=Exp, ["z : Int"]),
    0,
    "~let z = 1",
  ),
  (
    "`let` after the last member at a Mod root",
    Sort.Mod,
    counter_members,
    "let after = next + 1",
    "let z = 1",
    counter_rows(~root=Mod, ["z : Int"]),
    0,
    "~let z = 1",
  ),
  (
    "`type` after the last member",
    Sort.Exp,
    counter,
    "  let after = next + 1",
    "type T = Int",
    counter_rows(~root=Exp, ["T : "]),
    0,
    "~type T = Int",
  ),
  (
    "`module` after the last member",
    Sort.Exp,
    counter,
    "  let after = next + 1",
    "module N = {}",
    counter_rows(~root=Exp, ["N : ()"]),
    0,
    "~module N = {}",
  ),
  /* the unfinished member gets its `)` and `;`; z and after are apart by
     the missing `;`'s grout */
  (
    "`let` between members, after an unfinished one",
    Sort.Exp,
    counter_unfinished,
    "  let next = (start + 1",
    "let z = 1",
    [
      "/Counter : ",
      "/Counter/start : Int",
      "/Counter/next : Int",
      "/Counter/z : Int",
      "/Counter/after : Int",
      "/trail : ",
    ],
    2,
    "~let z = 1~",
  ),
];

/* a line indented past the member continues it: the `let` there is an
   expression's, its `in` synthesized, no member */
let member_head_deeper = () => {
  let z0 = at_line(~root=Exp, counter, "  let after = next + 1");
  let z = Util.ListUtil.last(type_line(~root=Exp, z0, "  let z = 1"));
  check(
    bool,
    "no member z",
    false,
    List.exists(((_, r: row)) => r.label == "z", rows(~root=Exp, z)),
  );
  check(
    bool,
    "its `in` synthesized",
    true,
    CanonicalCompletion.complete_items(
      ~sort=Exp,
      MakeTerm.semantic_source(z),
    ).
      shard_records
    != [],
  );
};

/* members typed into a new module, its `}` not yet typed: a `let` at
   its member level is its member */
let new_module_members = () => {
  let z0 = at_line(~root=Exp, counter, "  let start = 0;");
  let before = rows(~root=Exp, z0);
  let z =
    Util.ListUtil.last(
      type_line(~root=Exp, z0, "module N = {\nlet y = 1\n  let z = 2"),
    );
  let after = rows(~root=Exp, z);
  check_kept("a new module typed", before, after);
  check(
    list(string),
    "N holds y and z",
    ["y : Int", "z : Int"],
    List.filter_map(
      ((parent, r: row)) =>
        parent == "/Counter/N" ? Some(show_row(r)) : None,
      after,
    ),
  );
};

let tests = (
  "ModuleCompletion",
  List.map(
    ((name, src, after, keys, expect)) =>
      test_case(name, `Quick, unfinished(name, src, ~after, keys, expect)),
    browser_cases,
  )
  @ List.map(
      ((name, _, _) as c) =>
        test_case("unfinished member: " ++ name, `Quick, sibling(c)),
      sibling_keys,
    )
  @ List.map(
      ((name, _, _, _) as c) =>
        test_case(
          "rows kept typing a member: " ++ name,
          `Quick,
          preserved(c),
        ),
      typing_cases,
    )
  @ [
    test_case("a new let-chain link", `Quick, chain_link),
    test_case("an unfinished nested module", `Quick, nested_unfinished),
  ]
  @ List.map(
      ((name, _, _, _, _, _, _, _) as c) =>
        test_case("a member head: " ++ name, `Quick, member_head(c)),
      member_head_cases,
    )
  @ [
    test_case(
      "a member head on a line past its member",
      `Quick,
      member_head_deeper,
    ),
    test_case("members of a new module", `Quick, new_module_members),
  ]
  @ List.map(
      ((name, c)) =>
        test_case(
          "a member on the `{` line: " ++ name,
          `Quick,
          brace_line(name, c),
        ),
      brace_line_cases,
    )
  @ [
    test_case(
      "a module on the outer `{` line, its `}` deleted",
      `Quick,
      brace_nested,
    ),
  ]
  @ List.map(
      ((name, _) as c) =>
        test_case("rows kept at a Mod root: " ++ name, `Quick, mod_root(c)),
      [
        ("after the first", "let start = 0;"),
        ("after the second", "let next = start + 1;"),
        ("end", "let after = next + 1"),
      ],
    )
  @ List.map(
      ((name, _, _, _, _) as c) =>
        test_case("deletion round-trips " ++ name, `Quick, round_trip(c)),
      round_trip_pins,
    )
  @ List.map(
      ((name, _, _) as c) =>
        test_case(
          "a deleted `;` is quiet " ++ name,
          `Quick,
          semi_deleted(c),
        ),
      [
        ("at a Mod root", Sort.Mod, counter_members),
        ("in a module", Sort.Exp, counter),
        ("in a library-style module", Sort.Mod, option_shaped),
      ],
    )
  @ List.map(
      ((name, _, _, _, _) as c) =>
        test_case(
          "a deleted head keyword: " ++ name,
          `Quick,
          head_deleted(c),
        ),
      [
        (
          "an annotated let-in",
          Sort.Exp,
          "let digit : Int -> Int = fun d -> d in\nlet go = fun k -> digit(k) in\ngo(1)",
          Util.Point.mk(~row=0, ~col=3),
          3,
        ),
        (
          "an annotated member",
          Sort.Mod,
          "let digit : Int -> Int = fun d -> d;\nlet go = fun k -> digit(k);\ngo(1)",
          Util.Point.mk(~row=0, ~col=3),
          3,
        ),
        (
          "a signed module",
          Sort.Exp,
          "module M : {let x : Int} = {let x = 1} in\nlet y = M.x in\ny",
          Util.Point.mk(~row=0, ~col=6),
          6,
        ),
      ],
    )
  @ List.map(
      ((name, _, _, _) as c) =>
        test_case("a deleted `case`: " ++ name, `Quick, case_deleted(c)),
      [
        (
          "multi-line",
          "let f = fun x ->\n  case x\n  | 1 => 2\n  | _ => 3\n  end;\nlet g = 1;\ng",
          Util.Point.mk(~row=1, ~col=6),
          "let f = fun x ->\n  case x\n  | 1 => 2\n  | _ => 3\n  end;\nlet g = 1;\ng",
        ),
        (
          "one line",
          "let f = fun x -> case x | 1 => 2 | _ => 3 end;\nlet g = 1;\ng",
          Util.Point.mk(~row=0, ~col=21),
          "let f = case fun x -> x | 1 => 2 | _ => 3 end;\nlet g = 1;\ng",
        ),
      ],
    )
  @ List.map(
      ((name, _, _) as c) =>
        test_case("reads as written: " ++ name, `Quick, reads_as_written(c)),
      as_written,
    )
  @ List.map(
      ((name, _, _, _, _) as c) =>
        test_case(
          "the display reads as semantics: " ++ name,
          `Quick,
          display_agrees(c),
        ),
      display_cases,
    )
  @ List.map(
      ((root, src, _, _) as c) =>
        test_case(
          "the chips read at the editor's root: "
          ++ Sort.show(root)
          ++ " "
          ++ String.escaped(src),
          `Quick,
          chips_after(c),
        ),
      chip_cases,
    )
  @ [
    test_case("corpus sweep (HAZEL_COMPLETION_SWEEP=1)", `Slow, corpus_sweep),
  ],
);
