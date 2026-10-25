open Alcotest;
open Haz3lcore;

/* the editing kernel's ops and sources on small programs */

module K = EntryEdit;

let seg_of = CorpusUtil.seg_of;
let text_of = CorpusUtil.text_of;

/* the definition head binding [name] */
let head = (name: string, seg: Segment.t): Id.t => {
  let rec find = (seg: Segment.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when SpineNav.is_def_head(t) =>
          switch (
            List.filter(p => !Piece.is_secondary(p), SpineNav.pat_of(t))
          ) {
          | [Tile(n), ..._] when Tile.label(n) == [name] => Some(t.id)
          | _ => List.find_map(find, t.children)
          }
        | Tile(t) => List.find_map(find, t.children)
        | _ => None
        },
      seg,
    );
  switch (find(seg)) {
  | Some(id) => id
  | None => fail("no head " ++ name)
  };
};

/* tile ids, deep, secondary pieces aside */
let rec tile_ids = (seg: Segment.t): list(Id.t) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) => [t.id, ...List.concat_map(tile_ids, t.children)]
      | Grout(g) => [g.id]
      | Projector(pr) => [pr.id, ...tile_ids([pr.syntax])]
      | Secondary(_) => []
      },
    seg,
  );

let run = (~root=Sort.Exp, seg, ops) =>
  switch (K.transaction(~root, seg, ops)) {
  | Ok(c) => c
  | Error(r) => fail("refused: " ++ K.show_refusal(r))
  };

let refused = (~root=Sort.Exp, seg, ops): K.refusal =>
  switch (K.transaction(~root, seg, ops)) {
  | Ok(c) => fail("not refused: " ++ text_of(c.program))
  | Error(r) => r
  };

let expect = (desc, want, c: K.change) =>
  check(string, desc, want, text_of(c.program));

let chain = "let a = 1 in\nlet b = a + 1 in\nb";
let members = "module M = {\n  let x = 1;\n  let y = 2\n} in\nM.y";

let insert = () => {
  let seg = seg_of(chain);
  let a = head("a", seg);
  expect(
    "text after an entry takes its form",
    "let a = 1 in\n\nlet c = 3 in\n\nlet b = a + 1 in\nb",
    run(seg, [Insert(After(a), Text("let c = 3"))]),
  );
  expect(
    "a statement takes `;`, right under the definition it follows",
    "let a = 1 in\ntest a == 1 end;\n\nlet b = a + 1 in\nb",
    run(seg, [Insert(After(a), Text("test a == 1 end"))]),
  );
  expect(
    "a template with a hole for its name",
    {js|let ¿ = ¿ in

let a = 1 in
let b = a + 1 in
b|js},
    run(seg, [Insert(Before(a), Template(Value(None)))]),
  );
  expect(
    "last in the top block: above its result",
    {js|let a = 1 in
let b = a + 1 in

type T = ¿ in

b|js},
    run(seg, [Insert(LastIn(Top), Template(Type(Some("T"))))]),
  );
  let m = seg_of(members);
  expect(
    "into a module: member form, its indentation",
    "module M = {\n  let x = 1;\n  let y = 2;\n\n  let z = 3\n} in\nM.y",
    run(
      m,
      [Insert(LastIn(Members(head("M", m))), Text("let z = 3 in"))],
    ),
  );
  expect(
    "a text's lines take the place's indentation",
    "module M = {\n  let w =\n    0;\n\n  let x = 1;\n  let y = 2\n} in\nM.y",
    run(
      m,
      [Insert(FirstIn(Members(head("M", m))), Text("let w =\n  0"))],
    ),
  );
  let lined = seg_of("let a(x) =\n  x + 1\nin\nlet b = 2 in\nb");
  expect(
    "a definition spanning lines: `in` on a line of its own, blank lines around it",
    "let a(x) =\n  x + 1\nin\n\nlet c(y) =\n  y * 2\nin\n\nlet b = 2 in\nb",
    run(
      lined,
      [Insert(After(head("a", lined)), Text("let c(y) =\n  y * 2"))],
    ),
  );
  let ended = seg_of("let a(x) =\n  x + 1 in\nlet b = 2 in\nb");
  expect(
    "whatever its neighbours do",
    "let a(x) =\n  x + 1 in\n\nlet c(y) =\n  y * 2\nin\n\nlet b = 2 in\nb",
    run(
      ended,
      [Insert(After(head("a", ended)), Text("let c(y) =\n  y * 2\nin"))],
    ),
  );
  let body = seg_of("let f(x) =\n  let a = x in\n  a\nin\nf(1)");
  expect(
    "in a body: its indentation",
    "let f(x) =\n  let g(y) =\n    y\n  in\n  let a = x in\n  a\nin\nf(1)",
    run(
      body,
      [Insert(FirstIn(Body(head("f", body))), Text("let g(y) =\n  y"))],
    ),
  );
  let f = seg_of("let f = fun x -> x in f(1)");
  expect(
    "into a value's body, above its result",
    "let f = fun x -> let y = x in x in f(1)",
    run(f, [Insert(LastIn(Body(head("f", f))), Text("let y = x"))]),
  );
  check(
    bool,
    "nothing goes after a block's result",
    true,
    refused(
      seg,
      [Insert(After(CorpusUtil.last_tile("b", seg)), Text("1"))],
    ).
      reason
    == Result,
  );
  check(
    bool,
    "a token is no entry",
    true,
    refused(seg, [Insert(After(a), Token("q"))]).reason == NotAToken,
  );
};

let copy = () => {
  let seg = seg_of(chain);
  let b = head("b", seg);
  let c = run(seg, [Insert(After(b), Copy(b))]);
  expect(
    "a copy below its entry",
    "let a = 1 in\nlet b = a + 1 in\n\nlet b = a + 1 in\n\nb",
    c,
  );
  let before = tile_ids(seg);
  let added = List.hd(List.hd(c.steps).added);
  check(bool, "the copy's row is fresh", false, List.mem(added, before));
  check(
    int,
    "fresh ids only for the copy",
    List.length(tile_ids(c.program)) - List.length(before),
    List.length(
      List.filter(id => !List.mem(id, before), tile_ids(c.program)),
    ),
  );
};

let replace = () => {
  let seg = seg_of(chain);
  let b = head("b", seg);
  expect(
    "an entry by its head",
    "let a = 1 in\nlet c = 2 in\nb",
    run(seg, [Replace(b, Text("let c = 2"))]),
  );
  let lined = seg_of("let a(x) =\n  x + 1\nin\nlet b = 2 in\nb");
  expect(
    "spanning lines: `in` on a line of its own",
    "let a(x) =\n  x + 1\nin\nlet c(y) =\n  y\nin\nb",
    run(lined, [Replace(head("b", lined), Text("let c(y) =\n  y in"))]),
  );
  let def = Option.get(K.part(~root=Exp, b, Definition, seg));
  expect(
    "a definition",
    "let a = 1 in\nlet b = a * 10 in\nb",
    run(seg, [Replace(def, Text("a * 10"))]),
  );
  /* a definition's doc comment: new code's own replaces it */
  let doc = seg_of("let f(x) =\n  # old #\n  x + 1\nin\nf(1)");
  let body = Option.get(K.part(~root=Exp, head("f", doc), Definition, doc));
  expect(
    "a definition with a comment of its own: it replaces the old",
    "let f(x) =\n  # new #\n  x + 2\nin\nf(1)",
    run(doc, [Replace(body, Text("# new #\nx + 2"))]),
  );
  expect(
    "without one: the old stays",
    "let f(x) =\n  # old #\n  x + 2\nin\nf(1)",
    run(doc, [Replace(body, Text("x + 2"))]),
  );
  let apart = seg_of("let f(x) =\n  # set apart #\n\n  x + 1\nin\nf(1)");
  expect(
    "a comment set apart by a blank line stays",
    "let f(x) =\n  # set apart #\n\n  # new #\n  x + 2\nin\nf(1)",
    run(
      apart,
      [
        Replace(
          Option.get(
            K.part(~root=Exp, head("f", apart), Definition, apart),
          ),
          Text("# new #\nx + 2"),
        ),
      ],
    ),
  );
  let pat = Option.get(K.part(~root=Exp, b, Pattern, seg));
  expect(
    "a pattern",
    "let a = 1 in\nlet (p, q) = a + 1 in\nb",
    run(seg, [Replace(pat, Text("(p, q)"))]),
  );
  expect(
    "a subterm",
    "let a = 1 in\nlet b = 7 + 1 in\nb",
    run(seg, [Replace(CorpusUtil.last_tile("a", seg), Text("7"))]),
  );
  let tail = CorpusUtil.last_tile("b", seg);
  expect(
    "a block's result",
    "let a = 1 in\nlet b = a + 1 in\nb * b",
    run(seg, [Replace(tail, Text("b * b"))]),
  );
  let cs = seg_of("case 1 | 1 => 2 | _ => 3 end");
  let rule =
    switch (cs) {
    | [Tile({children: [kid], _}), ..._] =>
      List.find_map(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) when Tile.is_case_rule(t) => Some(t.id)
          | _ => None
          },
        kid,
      )
      |> Option.get
    | _ => fail("no case")
    };
  expect(
    "a case rule, through its result",
    "case 1 | 0 => 5 | _ => 3 end",
    run(cs, [Replace(rule, Text("| 0 => 5"))]),
  );
  let t = seg_of("type T = Int in 1");
  let tdef = Option.get(K.part(~root=Exp, head("T", t), Definition, t));
  expect(
    "a type definition, read as a type",
    "type T = (Int, Bool) in 1",
    run(t, [Replace(tdef, Text("(Int, Bool)"))]),
  );
};

let token = () => {
  let seg = seg_of(chain);
  let use = CorpusUtil.last_tile("a", seg);
  let c = run(seg, [Replace(use, Token("z"))]);
  expect("a token's new text", "let a = 1 in\nlet b = z + 1 in\nb", c);
  check(
    bool,
    "keeps its id",
    true,
    List.mem(use, tile_ids(c.program))
    && tile_ids(c.program) == tile_ids(seg),
  );
  let m = seg_of("let m = {} in 1");
  let h = head("m", m);
  let name = CorpusUtil.tile("m", m);
  let c =
    run(m, [Replace(name, Token("M")), Replace(h, Token("module"))]);
  expect("a head's keyword re-reads it", "module M = {} in 1", c);
  check(
    bool,
    "the head keeps its id",
    true,
    List.mem(h, tile_ids(c.program)),
  );
  check(bool, "and its name", true, List.mem(name, tile_ids(c.program)));
  let fold = seg_of("let a = 1 in\nlet b = a + 1 in\nb");
  check(
    bool,
    "a multi-token tile takes no token",
    true,
    refused(fold, [Replace(CorpusUtil.tile("+", fold), Token("-"))]).
      reason
    == NotAToken,
  );
};

let delete = () => {
  let seg = seg_of(chain);
  expect(
    "an entry",
    "let b = a + 1 in\nb",
    run(seg, [Delete(head("a", seg))]),
  );
  check(
    bool,
    "a block's result stays",
    true,
    refused(seg, [Delete(CorpusUtil.last_tile("b", seg))]).reason == Result,
  );
};

/* the ids [moved] had, all in [after] (its separator aside) */
let keeps = (moved: Segment.t, after: Segment.t) =>
  List.for_all(id => List.mem(id, tile_ids(after)), tile_ids(moved));

let entry_pieces = (~mod_root, id, seg) =>
  switch (EntryLayout.locate(~mod_root, id, seg)) {
  | Some((b, j, _)) => EntryLayout.bare(b, j)
  | None => fail("no entry")
  };

let move = () => {
  let seg = seg_of(chain);
  let (a, b) = (head("a", seg), head("b", seg));
  let c = run(seg, [Move(a, After(b))]);
  expect("past its neighbour", "let b = a + 1 in\n\nlet a = 1 in\n\nb", c);
  check(
    bool,
    "ids kept",
    true,
    tile_ids(c.program)
    |> List.sort(compare) == (tile_ids(seg) |> List.sort(compare)),
  );
  check(
    bool,
    "its step says where it was",
    true,
    switch (c.steps) {
    | [{moved: [(id, Before(next), After(to_))], _}] =>
      id == a && next == b && to_ == b
    | _ => false
    },
  );
  let three = seg_of("let a = 1 in\nlet b = 2 in\nlet c = 3 in\nc");
  expect(
    "past two",
    "let b = 2 in\nlet c = 3 in\n\nlet a = 1 in\n\nc",
    run(three, [Move(head("a", three), After(head("c", three)))]),
  );
  /* a member out of its module, into the let chain around it, and back */
  let m = seg_of(members);
  let x = head("x", m);
  let x_pieces = entry_pieces(~mod_root=false, x, m);
  let out = run(m, [Move(x, After(head("M", m)))]);
  expect(
    "a member out: `;` to `in`",
    "module M = {\n  let y = 2\n} in\n\nlet x = 1 in\n\nM.y",
    out,
  );
  check(
    bool,
    "a member out keeps its ids",
    true,
    keeps(x_pieces, out.program),
  );
  let back =
    run(
      out.program,
      [Move(x, FirstIn(Members(head("M", out.program))))],
    );
  expect(
    "and back in: `in` to `;`",
    "module M = {\n  let x = 1;\n\n  let y = 2\n} in\n\nM.y",
    back,
  );
  check(bool, "and back keeps them", true, keeps(x_pieces, back.program));
  /* a member spanning lines takes its `in` as the chain's others do */
  let moves_out = a_ends =>
    "let a(x) =\n  x + 1"
    ++ a_ends
    ++ "module M = {\n  let f(y) =\n    y * 2;\n  let z = 1\n} in\nM.z";
  let lined = seg_of(moves_out("\nin\n"));
  let f = head("f", lined);
  let f_pieces = entry_pieces(~mod_root=false, f, lined);
  let out = run(lined, [Move(f, After(head("a", lined)))]);
  expect(
    "a member spanning lines out: `in` on a line of its own",
    "let a(x) =\n  x + 1\nin\n\nlet f(y) =\n  y * 2\nin\n\nmodule M = {\n  let z = 1\n} in\nM.z",
    out,
  );
  check(bool, "it keeps its ids", true, keeps(f_pieces, out.program));
  let back =
    run(
      out.program,
      [Move(f, FirstIn(Members(head("M", out.program))))],
    );
  expect(
    "and back in: its `in` line goes",
    "let a(x) =\n  x + 1\nin\n\nmodule M = {\n  let f(y) =\n    y * 2;\n\n  let z = 1\n} in\nM.z",
    back,
  );
  check(bool, "and back keeps them", true, keeps(f_pieces, back.program));
  let ended = seg_of(moves_out(" in\n"));
  expect(
    "whatever its neighbours do",
    "let a(x) =\n  x + 1 in\n\nlet f(y) =\n  y * 2\nin\n\nmodule M = {\n  let z = 1\n} in\nM.z",
    run(ended, [Move(head("f", ended), After(head("a", ended)))]),
  );
  expect(
    "a copy out likewise",
    "let f(y) =\n  y * 2\nin\n\n" ++ moves_out("\nin\n"),
    run(lined, [Insert(Before(head("a", lined)), Copy(f))]),
  );
  let body =
    seg_of("let g(x) =\n  let a = x in\n  a\nin\n" ++ moves_out("\nin\n"));
  expect(
    "into a body: `in` at its head's indentation",
    "let g(x) =\n  let f(y) =\n    y * 2\n  in\n  let a = x in\n  a\nin\n"
    ++ "let a(x) =\n  x + 1\nin\nmodule M = {\n  let z = 1\n} in\nM.z",
    run(body, [Move(head("f", body), FirstIn(Body(head("g", body))))]),
  );
  let t = seg_of("module M = {\n  let y = 2;\n  test y == 2 end\n} in\nM.y");
  let test = CorpusUtil.tile("==", t);
  let tc = run(t, [Move(test, After(head("M", t)))]);
  expect(
    "a test out: it takes the chain's `;`",
    "module M = {\n  let y = 2\n} in\n\ntest y == 2 end;\n\nM.y",
    tc,
  );
  check(
    bool,
    "a test keeps its ids",
    true,
    List.mem(test, tile_ids(tc.program)),
  );
  let f = seg_of("let a = 1 in\nlet f = fun x -> x in\nf(a)");
  let fa = run(f, [Move(head("a", f), LastIn(Body(head("f", f))))]);
  expect(
    "into a value's body",
    "let f = fun x -> let a = 1 in x in\nf(a)",
    fa,
  );
  check(
    bool,
    "into itself refuses",
    true,
    refused(m, [Move(head("M", m), LastIn(Members(head("M", m))))]).
      reason
    == IntoItself,
  );
};

let transaction = () => {
  let seg = seg_of(chain);
  let a = head("a", seg);
  let r =
    refused(
      seg,
      [
        Insert(After(a), Text("let c = 3")),
        Delete(CorpusUtil.last_tile("b", seg)),
      ],
    );
  check(int, "the refused op", 1, r.op);
  check(bool, "its reason", true, r.reason == Result);
  let c =
    run(
      seg,
      [Insert(After(a), Text("let c = 3")), Delete(head("b", seg))],
    );
  expect("both, in order", "let a = 1 in\n\nlet c = 3 in\n\nb", c);
  check(int, "a step per op", 2, List.length(c.steps));
  check(
    string,
    "each step's program",
    "let a = 1 in\n\nlet c = 3 in\n\nlet b = a + 1 in\nb",
    text_of(List.hd(c.steps).program),
  );
};

/* an edit splices one level: everything else stays physically the same */
let locality = () => {
  let src = "let a = 1 in\nmodule M = {\n  let x = 1;\n  let y = 2\n} in\nlet b = 2 in\nb";
  let seg = seg_of(src);
  let c = run(seg, [Move(head("x", seg), After(head("y", seg)))]);
  let kept = List.filter(p => List.exists(q => q === p, c.program), seg);
  check(
    int,
    "all top-level pieces but the module's",
    List.length(seg) - 1,
    List.length(kept),
  );
};

/* an op landing several entries makes a step per entry, each with its
   program, the last the op's own */
let entrywise = () => {
  let texts = (c: K.change) =>
    List.map((s: K.step) => text_of(s.program), c.steps);
  let counts = (c: K.change) =>
    List.map((s: K.step) => List.length(s.added), c.steps);
  let seg = seg_of(chain);
  let c =
    run(
      seg,
      [
        Insert(
          After(head("a", seg)),
          Text("let c = 3 in\ntest c == 3 end;\nlet d = c + 1"),
        ),
      ],
    );
  check(
    list(string),
    "an insert's entries, one at a time",
    [
      "let a = 1 in\n\nlet c = 3 in\n\nlet b = a + 1 in\nb",
      "let a = 1 in\n\nlet c = 3 in\ntest c == 3 end;\n\nlet b = a + 1 in\nb",
      "let a = 1 in\n\nlet c = 3 in\ntest c == 3 end;\n\nlet d = c + 1 in\n\nlet b = a + 1 in\nb",
    ],
    texts(c),
  );
  check(list(int), "a row each", [1, 1, 1], counts(c));
  let seg = seg_of(members);
  let c =
    run(
      seg,
      [
        Insert(
          LastIn(Members(head("M", seg))),
          Text("let z = 3;\nlet w = 4"),
        ),
      ],
    );
  check(
    list(string),
    "members, one at a time",
    [
      "module M = {\n  let x = 1;\n  let y = 2;\n\n  let z = 3\n} in\nM.y",
      "module M = {\n  let x = 1;\n  let y = 2;\n\n  let z = 3;\n\n  let w = 4\n} in\nM.y",
    ],
    texts(c),
  );
  let seg = seg_of(chain);
  let b = head("b", seg);
  let c = run(seg, [Replace(b, Text("let b = a + 1 in\nlet e = b"))]);
  check(
    list(string),
    "a replace's entries, one at a time",
    [
      "let a = 1 in\nlet b = a + 1 in\nb",
      "let a = 1 in\nlet b = a + 1 in\n\nlet e = b in\nb",
    ],
    texts(c),
  );
  check(
    list(int),
    "the first removes what it replaced",
    [1, 0],
    List.map((s: K.step) => List.length(s.removed), c.steps),
  );
  let seg = seg_of(chain);
  check(
    list(string),
    "a definition, then a statement",
    [
      "let a = 1 in\n\nlet c = 3 in\n\nlet b = a + 1 in\nb",
      "let a = 1 in\n\nlet c = 3 in\ntest c == 3 end;\n\nlet b = a + 1 in\nb",
    ],
    texts(
      run(
        seg,
        [
          Insert(
            After(head("a", seg)),
            Text("let c = 3 in\ntest c == 3 end"),
          ),
        ],
      ),
    ),
  );
  check(
    int,
    "one entry, one step",
    1,
    List.length(
      run(seg, [Insert(After(head("a", seg)), Text("let c = 3"))]).steps,
    ),
  );
};

let tests = (
  "EntryEdit",
  [
    test_case("insert", `Quick, insert),
    test_case("copy", `Quick, copy),
    test_case("replace", `Quick, replace),
    test_case("token", `Quick, token),
    test_case("delete", `Quick, delete),
    test_case("move", `Quick, move),
    test_case("transaction", `Quick, transaction),
    test_case("locality", `Quick, locality),
    test_case("a step per entry", `Quick, entrywise),
  ],
);
