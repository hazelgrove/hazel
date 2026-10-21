open Alcotest;
open Haz3lcore;
open CorpusUtil;

/* outline restructure ops across block kinds, run as the outline runs
   them (ItemEdit.edit), checked on the resulting program */

module R = Web.ItemEdit;

let op = (o, id) => R.Op(o, id);
let inside = (text, id) => R.InsertInside(id, text);

/* [e] on the row labeled [label]: the new program and the row it lands
   on, or None where it's refused. Modules are open as the outline shows
   them, or with ~closed all collapsed: moves step over them */
let edit =
    (~root=Sort.Exp, ~closed=false, e: Id.t => R.t, label, seg: Segment.t)
    : option((Segment.t, option(Id.t))) => {
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  let ctx: R.ctx = {
    mod_root: root == Sort.Mod,
    term,
    info_map: lazy(Language.Statics.Map.empty),
    is_open: id => !closed && Web.OutlineTree.expanded(~folds=[], id, term),
  };
  Result.to_option(R.edit(ctx, e(row_by_label(term, label)), seg));
};

let ok = (desc, result: option('a)): 'a =>
  switch (result) {
  | Some(x) => x
  | None => fail(desc ++ ": refused")
  };

/* the program [src] after [e] on [label]; None where it's refused */
let run = (~root=Sort.Exp, ~closed=?, e, label, src): option(Segment.t) =>
  Option.map(fst, edit(~root, ~closed?, e, label, seg_of(~root, src)));

/* a result against [expected] (None: refused), as the parser prints it */
let expect = (~root=Sort.Exp, desc, expected, got: option(Segment.t)) =>
  check(
    option(string),
    desc,
    Option.map(t => text_of(seg_of(~root, t)), expected),
    Option.map(text_of, got),
  );

/* rows of (what it shows, program, row label, edit, program after) */
let cases = rows =>
  List.iter(
    ((desc, src, label, e, expected)) =>
      expect(desc, expected, run(e, label, src)),
    rows,
  );

/* the outline [src] reads as: labels, a row's children in braces after
   it ("M{x,y}"); unlabeled rows (⇒, `;`) left out */
let outline = (~root=Sort.Exp, src: string): list(string) => {
  let rec labels = (ns: list(Web.OutlineTree.node)) =>
    List.concat_map(
      (n: Web.OutlineTree.node) =>
        switch (n.o_label, labels(n.o_children)) {
        | ("", _) => []
        | (l, []) => [l]
        | (l, kids) => [l ++ "{" ++ String.concat(",", kids) ++ "}"]
        },
      ns,
    );
  labels(
    Web.OutlineTree.of_term(
      MakeTerm.Incr.term_of_root(~root, seg_of(~root, src)),
    ),
  );
};

/* the program's items, one per line, a test among them */
let top_level = () => {
  let (a, b, t, tail) = (
    "let a = 1 in",
    "let b = a + 1 in",
    "test b == 2 end;",
    "b",
  );
  List.iter(
    ((label, o, expected)) =>
      expect(
        label ++ " " ++ Web.OutlineEdit.show_def_op(o),
        Option.map(String.concat("\n"), expected),
        run(op(o), label, String.concat("\n", [a, b, t, tail])),
      ),
    [
      /* an implicit hole, not `?` */
      ("a", NewBelow, Some([a, {js|let new_def = ¿ in|js}, b, t, tail])),
      (
        "a",
        NewTypeBelow,
        Some([a, {js|type NewType = ¿ in|js}, b, t, tail]),
      ),
      (
        "a",
        NewModuleBelow,
        Some([a, "module NewModule = {} in", b, t, tail]),
      ),
      ("a", Duplicate, Some([a, a, b, t, tail])),
      ("a", MoveDown, Some([b, a, t, tail])),
      ("a", MoveUp, None),
      ("b", MoveUp, Some([b, a, t, tail])),
      ("b", Delete, Some([a, t, tail])),
      ("1", MoveUp, Some([a, t, b, tail])),
      ("1", Delete, Some([a, b, tail])),
    ],
  );
};

/* a module's members, the last ending in `;`, and members typed into a
   module's name row (InsertInside: the keyword picks the kind) */
let members = () => {
  let m = "module M = {\n  let a = 1;\n  let b = 2;\n} in\nM.a";
  let e = "module E = {} in 0";
  cases([
    (
      "new below a member is a member",
      m,
      "a",
      op(NewBelow),
      Some(
        {js|module M = {
  let a = 1;
  let new_def = ¿;
  let b = 2;
} in
M.a|js},
      ),
    ),
    (
      "a member duplicated",
      m,
      "a",
      op(Duplicate),
      Some(
        "module M = {\n  let a = 1;\n  let a = 1;\n  let b = 2;\n} in\nM.a",
      ),
    ),
    (
      "the last `;` moves with the last place",
      m,
      "b",
      op(MoveUp),
      Some("module M = {\n  let b = 2;\n  let a = 1;\n} in\nM.a"),
    ),
    (
      "the last member deleted",
      m,
      "b",
      op(Delete),
      Some("module M = {\n  let a = 1;\n} in\nM.a"),
    ),
    (
      "a value typed inside, after the last `;`",
      m,
      "M",
      inside("c"),
      Some(
        {js|module M = {
  let a = 1;
  let b = 2;
  let c = ¿;
} in
M.a|js},
      ),
    ),
    (
      "a module typed inside",
      m,
      "M",
      inside("module N"),
      Some(
        "module M = {\n  let a = 1;\n  let b = 2;\n  module N = {};\n} in\nM.a",
      ),
    ),
    ("a name that can't bind", m, "M", inside("1x"), None),
    (
      "an empty module's first member",
      e,
      "E",
      inside("new_def"),
      Some({js|module E = {
  let new_def = ¿
} in 0|js}),
    ),
    (
      "and its first type",
      e,
      "E",
      inside("type T"),
      Some({js|module E = {
  type T = ¿
} in 0|js}),
    ),
  ]);
  switch (edit(inside("c"), "M", seg_of(m))) {
  | Some((seg, target)) =>
    check(
      bool,
      "the new member is the row it lands on",
      true,
      target == Some(row_by_label(MakeTerm.Incr.term_of(seg), "c")),
    )
  | None => fail("c refused")
  };
};

/* the last member has no `;`: ops that put a member after it add one */
let unterminated_tail = () => {
  let src = "module U = {\n  let a = 1;\n  let z = fun x -> x\n} in U.a";
  cases([
    (
      "a member typed inside",
      src,
      "U",
      inside("new_def"),
      Some(
        {js|module U = {
  let a = 1;
  let z = fun x -> x;
  let new_def = ¿
} in U.a|js},
      ),
    ),
  ]);
  /* U's members as the outline reads them back, also at the top of a
     module-rooted program */
  List.iter(
    ((root, src)) => {
      check(
        list(string),
        "members before",
        ["U{a,z}"],
        outline(~root, src),
      );
      List.iter(
        ((label, o, expected)) =>
          check(
            list(string),
            Web.OutlineEdit.show_def_op(o),
            expected,
            outline(
              ~root,
              text_of(ok(label, run(~root, op(o), label, src))),
            ),
          ),
        [
          ("z", MoveUp, ["U{z,a}"]),
          ("a", MoveDown, ["U{z,a}"]),
          ("z", Duplicate, ["U{a,z,z}"]),
          ("z", NewBelow, ["U{a,z,new_def}"]),
        ],
      );
    },
    [
      (Sort.Exp, src),
      (Mod, "module U = {\n  let a = 1;\n  let z = fun x -> x\n};\nU.a"),
    ],
  );
};

/* Alt↑↓ across module edges: in at the neighbour's end or start, out to
   just above or below; a collapsed module is stepped over */
let across = () => {
  let mv = (~root=Sort.Exp, ~closed=false, ~up, src, label, expected) =>
    check(
      list(string),
      (up ? "up " : "down ") ++ label,
      expected,
      outline(
        ~root,
        text_of(
          ok(
            label,
            run(~root, ~closed, op(up ? MoveUp : MoveDown), label, src),
          ),
        ),
      ),
    );
  /* b up into M and x up out of it: in layout, as text */
  let exp = "let a = 1 in\nmodule M = {\n  let x = 2;\n  let y = 3\n} in\nlet b = 4 in\nb";
  mv(~up=false, exp, "a", ["M{a,x,y}", "b"]);
  mv(~up=false, exp, "y", ["a", "M{x}", "y", "b"]);
  mv(~closed=true, ~up=true, exp, "b", ["a", "b", "M{x,y}"]);
  let md = "let a = 1;\nmodule M = {\n  let x = 2;\n  let y = 3\n};\nlet b = 4;\nb";
  mv(~root=Mod, ~up=true, md, "b", ["a", "M{x,y,b}"]);
  mv(~root=Mod, ~up=false, md, "a", ["M{a,x,y}", "b"]);
  mv(~root=Mod, ~up=true, md, "x", ["a", "x", "M{y}", "b"]);
  mv(~root=Mod, ~up=false, md, "y", ["a", "M{x}", "y", "b"]);
  /* the last member out leaves an empty module (down: in back_in) */
  let md1 = "let a = 1;\nmodule M = {\n  let x = 2\n};\nlet b = 4;\nb";
  mv(~root=Mod, ~up=true, md1, "x", ["a", "x", "M", "b"]);
  /* an empty module takes an item in, or is stepped over when it can't */
  let e = "let a = 1 in module E = {} in let b = 2 in b";
  mv(~up=true, e, "b", ["a", "E{b}"]);
  mv(~up=false, e, "a", ["E{a}", "b"]);
  let c = "let a = 1 in\nmodule E = {\n  # todo #\n} in\nlet b = 2 in\nb";
  mv(~up=true, c, "b", ["a", "b", "E"]);
  /* nested: out of the inner module into the outer one's members, and in */
  let nest = "let a = 1 in\nmodule M = {\n  module N = {\n    let x = 2;\n    let z = 5\n  };\n  let y = 3\n} in\nlet b = 4 in\nb";
  mv(~up=true, nest, "x", ["a", "M{x,N{z},y}", "b"]);
  mv(~up=false, nest, "z", ["a", "M{N{x},z,y}", "b"]);
  mv(~up=true, nest, "y", ["a", "M{N{x,z,y}}", "b"]);
  mv(~up=false, nest, "a", ["M{a,N{x,z},y}", "b"]);
  let nest_mod = "let a = 1;\nmodule M = {\n  module N = {\n    let x = 2;\n    let z = 5\n  };\n  let y = 3\n};\nlet b = 4;\nb";
  mv(~root=Mod, ~up=true, nest_mod, "x", ["a", "M{x,N{z},y}", "b"]);
  mv(~root=Mod, ~up=false, nest_mod, "z", ["a", "M{N{x},z,y}", "b"]);
  mv(~root=Mod, ~up=true, nest_mod, "y", ["a", "M{N{x,z,y}}", "b"]);
  /* out of a module bound by let, in let-in form */
  let v = "let a = 1 in\nlet m = {\n  let x = 2;\n  let y = 3\n} in\nlet b = 4 in\nb";
  mv(~up=true, v, "x", ["a", "x", "m{y}", "b"]);
  mv(~up=false, v, "y", ["a", "m{x}", "y", "b"]);
};

/* a module's last member, moved out, moves back in where it was */
let back_in = () => {
  let src = "let a = 1;\nmodule M = {\n  let x = 2\n};\nlet b = 4;\nb";
  let step = (up, seg) =>
    fst(ok("x", edit(~root=Mod, op(up ? MoveUp : MoveDown), "x", seg)));
  let out = step(false, seg_of(~root=Mod, src));
  expect(
    ~root=Mod,
    "out leaves {}",
    Some("let a = 1;\nmodule M = {};\nlet x = 2;\nlet b = 4;\nb"),
    Some(out),
  );
  check(string, "and back in", src, text_of(step(true, out)));
};

/* in then out again restores the program, and every step makes a term */
let round_trip = () => {
  let src = "module T = {\n  let a = 1;\n  let s = 2\n};\n\nmodule D = {\n  let x = 1\n};\n0";
  let step = (label, e, seg) => {
    let seg = fst(ok(label, edit(~root=Mod, e, label, seg)));
    ignore(MakeTerm.go_mod_root(seg));
    seg;
  };
  let seg = seg_of(~root=Mod, src);
  check(
    string,
    "back where it was",
    src,
    text_of(step("D", op(MoveDown), step("D", op(MoveUp), seg))),
  );
  expect(
    ~root=Mod,
    "deleting a module's last member leaves no bare `;`",
    Some("module T = {\n  let a = 1\n};\n\nmodule D = {\n  let x = 1\n};\n0"),
    Some(step("s", op(Delete), seg)),
  );
};

let fn_body = () => {
  let src = "module N = {\n  let f = fun x ->\n    let y = x + 1 in\n    y * 2;\n} in N.f(1)";
  cases([
    (
      "a flattened insert is a let-in, the member intact",
      src,
      "y",
      op(NewBelow),
      Some(
        {js|module N = {
  let f = fun x ->
    let y = x + 1 in
    let new_def = ¿ in
    y * 2;
} in N.f(1)|js},
      ),
    ),
    ("a nested let can't cross its member head", src, "y", op(MoveUp), None),
    (
      "on one line",
      "let f = fun x -> let y = 1 in y in f(1)",
      "y",
      op(NewBelow),
      Some(
        {js|let f = fun x -> let y = 1 in let new_def = ¿ in y in f(1)|js},
      ),
    ),
  ]);
};

/* a member whose body opens with `let … in`: the member's ops take all
   of it, the inner let's stay inside the body */
let let_body = () => {
  let src = "module M = {\n  let x =\n    let y = 1 in\n    y + 1;\n  let z = 2\n} in M.x";
  check(list(string), "before", ["M{x{y},z}"], outline(src));
  /* the program after [o] on [label], its outline checked */
  let outlined = (label, o, desc, expected) => {
    let t = text_of(ok(desc, run(op(o), label, src)));
    check(list(string), desc, expected, outline(t));
    t;
  };
  let t = outlined("x", MoveDown, "move down", ["M{z,x{y}}"]);
  check(bool, "x moves whole", true, contains("y + 1\n}", t));
  ignore(outlined("z", MoveUp, "move up", ["M{z,x{y}}"]));
  ignore(outlined("x", Duplicate, "duplicate", ["M{x{y},x{y},z}"]));
  ignore(outlined("x", NewBelow, "new below", ["M{x{y},new_def,z}"]));
  let t = outlined("x", Delete, "delete", ["M{z}"]);
  check(bool, "delete takes the body", false, contains("y + 1", t));
  ignore(outlined("y", NewBelow, "inner new below", ["M{x{y,new_def},z}"]));
  ignore(outlined("y", Delete, "inner delete", ["M{x,z}"]));
  expect(
    "the inner let can't leave its member",
    None,
    run(op(MoveUp), "y", src),
  );
  /* a function member's flattened body: the same */
  let fsrc = "module N = {\n  let f = fun x ->\n    let y = x + 1 in\n    y * 2;\n  let g = 0\n} in N.f(1)";
  let t = text_of(ok("f", run(op(MoveDown), "f", fsrc)));
  check(
    bool,
    "f moves whole",
    true,
    contains("let g = 0;", t) && contains("y * 2\n}", t),
  );
};

/* an op a row's own block refuses does nothing: it never falls back to
   the item around the row */
let refused = () => {
  /* label "": the first ⇒ row, f's */
  let f = "let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nf(1)";
  cases([
    ("result row: delete", f, "", op(Delete), None),
    ("result row: duplicate", f, "", op(Duplicate), None),
    ("result row: move up", f, "", op(MoveUp), None),
    ("result row: move down", f, "", op(MoveDown), None),
  ]);
};

/* a module's last test without its `;`, or its only member, is a member
   like the rest, not a result: ops act on it, never on the module */
let last_test = () => {
  let m = "let z = 0 in\nmodule M = {\n  let a = 1;\n  test 1 + 1 == 2 end\n} in\nM.a";
  let swapped = "let z = 0 in\nmodule M = {\n  test 1 + 1 == 2 end;\n  let a = 1\n} in\nM.a";
  let sole = "module M = {\n  test 1 == 1 end\n} in\n0";
  cases([
    (
      "deleted",
      m,
      "1",
      op(Delete),
      Some("let z = 0 in\nmodule M = {\n  let a = 1\n} in\nM.a"),
    ),
    (
      "duplicated",
      m,
      "1",
      op(Duplicate),
      Some(
        "let z = 0 in\nmodule M = {\n  let a = 1;\n  test 1 + 1 == 2 end;\n  test 1 + 1 == 2 end\n} in\nM.a",
      ),
    ),
    ("moved up", m, "1", op(MoveUp), Some(swapped)),
    ("the member above it moved down", m, "a", op(MoveDown), Some(swapped)),
    ("alone: deleted", sole, "1", op(Delete), Some("module M = {} in\n0")),
    (
      "alone: duplicated",
      sole,
      "1",
      op(Duplicate),
      Some("module M = {\n  test 1 == 1 end;\n  test 1 == 1 end\n} in\n0"),
    ),
    (
      "alone: out",
      sole,
      "1",
      op(Outdent),
      Some("module M = {} in\ntest 1 == 1 end;\n0"),
    ),
    (
      "alone: nothing above it",
      "module P = {} in\n" ++ sole,
      "1",
      op(Indent),
      None,
    ),
  ]);
};

/* ops keep the layout: items keep their lines, new ones take their
   neighbours' indentation, comment lines directly above an item go with
   it, and across module edges an item is reindented to where it lands */
let layout = () => {
  let c = "# about a #\nlet a = 1 in\n# about b #\nlet b = 2 in\nb";
  let d = "let a = 1 in\n\n# section #\n\nlet b = 2 in\nb";
  let m = "module M = {\n  let a = 1;\n  let b = 2\n} in\nM.a";
  let f = "let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nf(1)";
  let x = "let a = 1 in\nmodule M = {\n  let x = 2;\n  let y = 3\n} in\nlet b = 4 in\nb";
  cases([
    (
      "comments move with their items",
      c,
      "b",
      op(MoveUp),
      Some("# about b #\nlet b = 2 in\n# about a #\nlet a = 1 in\nb"),
    ),
    (
      "and go when they do",
      c,
      "a",
      op(Delete),
      Some("# about b #\nlet b = 2 in\nb"),
    ),
    (
      "a comment set apart stays",
      d,
      "b",
      op(MoveUp),
      Some("let b = 2 in\n\n# section #\n\nlet a = 1 in\nb"),
    ),
    (
      "member down",
      m,
      "a",
      op(MoveDown),
      Some("module M = {\n  let b = 2;\n  let a = 1\n} in\nM.a"),
    ),
    (
      "first member deleted",
      m,
      "a",
      op(Delete),
      Some("module M = {\n  let b = 2\n} in\nM.a"),
    ),
    (
      "last member deleted",
      m,
      "b",
      op(Delete),
      Some("module M = {\n  let a = 1\n} in\nM.a"),
    ),
    (
      "member duplicated",
      m,
      "a",
      op(Duplicate),
      Some(
        "module M = {\n  let a = 1;\n  let a = 1;\n  let b = 2\n} in\nM.a",
      ),
    ),
    (
      "new member below the last",
      m,
      "b",
      op(NewBelow),
      Some(
        {js|module M = {
  let a = 1;
  let b = 2;
  let new_def = ¿
} in
M.a|js},
      ),
    ),
    (
      "a function body's new let",
      f,
      "y",
      op(NewBelow),
      Some(
        {js|let f = fun x ->
  let y = x + 1 in
  let new_def = ¿ in
  y * 2
in
f(1)|js},
      ),
    ),
    (
      "into a module",
      x,
      "b",
      op(MoveUp),
      Some(
        "let a = 1 in\nmodule M = {\n  let x = 2;\n  let y = 3;\n  let b = 4\n} in\nb",
      ),
    ),
    (
      "out of a module",
      x,
      "x",
      op(MoveUp),
      Some(
        "let a = 1 in\nlet x = 2 in\nmodule M = {\n  let y = 3\n} in\nlet b = 4 in\nb",
      ),
    ),
    (
      "a test out of a module",
      "let a = 1 in\nmodule M = {\n  test a == 1 end;\n  let y = 3\n} in\nlet b = 4 in\nb",
      "1",
      op(MoveUp),
      Some(
        "let a = 1 in\ntest a == 1 end;\nmodule M = {\n  let y = 3\n} in\nlet b = 4 in\nb",
      ),
    ),
    (
      "a test into a module",
      "let a = 1 in\nmodule M = {\n  let y = 3\n} in\ntest a == 1 end;\nlet b = 4 in\nb",
      "1",
      op(MoveUp),
      Some(
        "let a = 1 in\nmodule M = {\n  let y = 3;\n  test a == 1 end\n} in\nlet b = 4 in\nb",
      ),
    ),
    (
      "a function into a module",
      "let f = fun x ->\n  x + 1\nin\nmodule M = {\n  let y = 3\n} in\nM.y",
      "f",
      op(MoveDown),
      Some(
        "module M = {\n  let f = fun x ->\n    x + 1;\n  let y = 3\n} in\nM.y",
      ),
    ),
  ]);
};

/* a module bound by `let` is a module too: its members take member form
   and keep their separators, as in `module M = {…}` */
let value_module = () => {
  let m = "let m = {\n  let a = 1;\n  let b = 2\n} in\nm.a";
  cases([
    (
      "new below a member is a member",
      m,
      "a",
      op(NewBelow),
      Some(
        {js|let m = {
  let a = 1;
  let new_def = ¿;
  let b = 2
} in
m.a|js},
      ),
    ),
    (
      "new below the last member",
      m,
      "b",
      op(NewBelow),
      Some(
        {js|let m = {
  let a = 1;
  let b = 2;
  let new_def = ¿
} in
m.a|js},
      ),
    ),
    (
      "a moved member keeps the separators in place",
      m,
      "a",
      op(MoveDown),
      Some("let m = {\n  let b = 2;\n  let a = 1\n} in\nm.a"),
    ),
    (
      "a duplicated member",
      m,
      "a",
      op(Duplicate),
      Some("let m = {\n  let a = 1;\n  let a = 1;\n  let b = 2\n} in\nm.a"),
    ),
    (
      "the last member deleted",
      m,
      "b",
      op(Delete),
      Some("let m = {\n  let a = 1\n} in\nm.a"),
    ),
    (
      "a member typed inside",
      m,
      "m",
      inside("c"),
      Some({js|let m = {
  let a = 1;
  let b = 2;
  let c = ¿
} in
m.a|js}),
    ),
  ]);
};

/* Alt↑↓ into a module bound by `let`, as into `module M = {…}`: the
   first member going down, the last going up, `{}` taking its only one */
let into_value_module = () => {
  let v = "let a = 1 in\nlet m = {\n  let x = 2;\n  let y = 3\n} in\nlet b = 4 in\nb";
  let e = "let a = 1 in\nlet e = {} in\nlet b = 2 in\nb";
  cases([
    (
      "from above",
      v,
      "a",
      op(MoveDown),
      Some(
        "let m = {\n  let a = 1;\n  let x = 2;\n  let y = 3\n} in\nlet b = 4 in\nb",
      ),
    ),
    (
      "from below",
      v,
      "b",
      op(MoveUp),
      Some(
        "let a = 1 in\nlet m = {\n  let x = 2;\n  let y = 3;\n  let b = 4\n} in\nb",
      ),
    ),
    (
      "with the comment above it",
      "let a = 1 in\nlet m = {\n  let x = 2\n} in\n# about b #\nlet b = 4 in\nb",
      "b",
      op(MoveUp),
      Some(
        "let a = 1 in\nlet m = {\n  let x = 2;\n  # about b #\n  let b = 4\n} in\nb",
      ),
    ),
    (
      "{} from above",
      e,
      "a",
      op(MoveDown),
      Some("let e = {\n  let a = 1\n} in\nlet b = 2 in\nb"),
    ),
    (
      "{} from below",
      e,
      "b",
      op(MoveUp),
      Some("let a = 1 in\nlet e = {\n  let b = 2\n} in\nb"),
    ),
    (
      "a test goes in as a bare member",
      "let a = 1 in\nlet m = {\n  let y = 3\n} in\ntest a == 1 end;\nlet b = 4 in\nb",
      "1",
      op(MoveUp),
      Some(
        "let a = 1 in\nlet m = {\n  let y = 3;\n  test a == 1 end\n} in\nlet b = 4 in\nb",
      ),
    ),
    (
      "an empty signature is stepped over",
      "let a = 1 in\ntype T = {} in\nlet b = 2 in\nb",
      "a",
      op(MoveDown),
      Some("type T = {} in\nlet a = 1 in\nlet b = 2 in\nb"),
    ),
    (
      "a module body inside an if is stepped over",
      "let a = 1 in\nmodule M = if true then {} else {\n  let x = 2\n} in\nlet b = 2 in\nb",
      "a",
      op(MoveDown),
      Some(
        "module M = if true then {} else {\n  let x = 2\n} in\nlet a = 1 in\nlet b = 2 in\nb",
      ),
    ),
  ]);
  expect(
    ~root=Mod,
    "in a module-rooted program, the `;`s stay put",
    Some(
      "let a = 1;\nlet m = {\n  let x = 2;\n  let y = 3;\n  let b = 4\n};\nb",
    ),
    run(
      ~root=Mod,
      op(MoveUp),
      "b",
      "let a = 1;\nlet m = {\n  let x = 2;\n  let y = 3\n};\nlet b = 4;\nb",
    ),
  );
  /* out, then back in where it was */
  List.iter(
    ((root, src, label, up, out)) => {
      let step = (up, seg) =>
        fst(
          ok(label, edit(~root, op(up ? MoveUp : MoveDown), label, seg)),
        );
      let seg = step(up, seg_of(~root, src));
      expect(~root, label ++ " out", Some(out), Some(seg));
      check(string, label ++ " back in", src, text_of(step(!up, seg)));
    },
    [
      (
        Sort.Exp,
        v,
        "x",
        true,
        "let a = 1 in\nlet x = 2 in\nlet m = {\n  let y = 3\n} in\nlet b = 4 in\nb",
      ),
      (
        Exp,
        v,
        "y",
        false,
        "let a = 1 in\nlet m = {\n  let x = 2\n} in\nlet y = 3 in\nlet b = 4 in\nb",
      ),
      (
        Mod,
        "let a = 1;\nlet m = {\n  let x = 2\n};\nlet b = 4;\nb",
        "x",
        false,
        "let a = 1;\nlet m = {};\nlet x = 2;\nlet b = 4;\nb",
      ),
    ],
  );
};

/* `use X in` is a row of its own among the definitions, and an item like
   them: it moves, duplicates and goes like one, so what it scopes over is
   what's below it. A module has no `use` member: a move steps over it */
let use_rows = () => {
  let src = "let a = 1 in\nuse Nat in\nlet b = a + 2 in\nb";
  check(list(string), "its row", ["a", "use Nat", "b"], outline(src));
  let m = "module M = {\n  let x = 1\n} in\n";
  cases([
    (
      "down past b",
      src,
      "use Nat",
      op(MoveDown),
      Some("let a = 1 in\nlet b = a + 2 in\nuse Nat in\nb"),
    ),
    (
      "up past a",
      src,
      "use Nat",
      op(MoveUp),
      Some("use Nat in\nlet a = 1 in\nlet b = a + 2 in\nb"),
    ),
    (
      "b up out of its scope",
      src,
      "b",
      op(MoveUp),
      Some("let a = 1 in\nlet b = a + 2 in\nuse Nat in\nb"),
    ),
    (
      "duplicated",
      src,
      "use Nat",
      op(Duplicate),
      Some("let a = 1 in\nuse Nat in\nuse Nat in\nlet b = a + 2 in\nb"),
    ),
    (
      "deleted",
      src,
      "use Nat",
      op(Delete),
      Some("let a = 1 in\nlet b = a + 2 in\nb"),
    ),
    (
      "a definition below",
      src,
      "use Nat",
      op(NewBelow),
      Some(
        {js|let a = 1 in
use Nat in
let new_def = ¿ in
let b = a + 2 in
b|js},
      ),
    ),
    (
      "a test below",
      src,
      "use Nat",
      op(NewTestBelow),
      Some({js|let a = 1 in
use Nat in
test ¿ end;
let b = a + 2 in
b|js}),
    ),
    (
      "a module stepped over going down",
      "use Nat in\n" ++ m ++ "M.x",
      "use Nat",
      op(MoveDown),
      Some(m ++ "use Nat in\nM.x"),
    ),
    (
      "and going up",
      m ++ "use Nat in\nM.x",
      "use Nat",
      op(MoveUp),
      Some("use Nat in\n" ++ m ++ "M.x"),
    ),
  ]);
  /* in a function body: among its items, never out of it */
  let f = "let f = fun x ->\n  use Nat in\n  let y = x + 1 in\n  y * 2\nin\nf(1)";
  check(list(string), "in a function body", ["f{use Nat,y}"], outline(f));
  cases([
    (
      "down in the body",
      f,
      "use Nat",
      op(MoveDown),
      Some(
        "let f = fun x ->\n  let y = x + 1 in\n  use Nat in\n  y * 2\nin\nf(1)",
      ),
    ),
    ("not out of the body", f, "use Nat", op(MoveUp), None),
  ]);
};

/* `module M : S = {…}` reads and edits like `module M = {…}`: its row has
   its members, and ops on it, on its members and moves in and out work
   the same, keeping the signature */
let signed_module = () => {
  let sg = "type S = {\n  let x : Int\n} in\n";
  let mm = "module M : S = {\n  let x = 1;\n  let y = 2\n} in\n";
  let m = sg ++ mm ++ "let b = 4 in\nM.x";
  check(list(string), "its row", ["S", "M{x,y}", "b"], outline(m));
  let with_members = members =>
    Some(
      sg ++ "module M : S = {\n" ++ members ++ "\n} in\nlet b = 4 in\nM.x",
    );
  cases([
    (
      "new below it",
      m,
      "M",
      op(NewBelow),
      Some(sg ++ mm ++ {js|let new_def = ¿ in
let b = 4 in
M.x|js}),
    ),
    (
      "duplicated",
      m,
      "M",
      op(Duplicate),
      Some(sg ++ mm ++ mm ++ "let b = 4 in\nM.x"),
    ),
    ("moved", m, "M", op(MoveUp), Some(mm ++ sg ++ "let b = 4 in\nM.x")),
    ("deleted", m, "M", op(Delete), Some(sg ++ "let b = 4 in\nM.x")),
    (
      "new below a member",
      m,
      "x",
      op(NewBelow),
      with_members({js|  let x = 1;
  let new_def = ¿;
  let y = 2|js}),
    ),
    (
      "a member moved",
      m,
      "x",
      op(MoveDown),
      with_members("  let y = 2;\n  let x = 1"),
    ),
    (
      "a member duplicated",
      m,
      "x",
      op(Duplicate),
      with_members("  let x = 1;\n  let x = 1;\n  let y = 2"),
    ),
    ("a member deleted", m, "x", op(Delete), with_members("  let y = 2")),
    (
      "a member typed inside",
      m,
      "M",
      inside("z"),
      with_members({js|  let x = 1;
  let y = 2;
  let z = ¿|js}),
    ),
    (
      "b up into it",
      m,
      "b",
      op(MoveUp),
      Some(
        sg
        ++ "module M : S = {\n  let x = 1;\n  let y = 2;\n  let b = 4\n} in\nM.x",
      ),
    ),
    (
      "y down out of it",
      m,
      "y",
      op(MoveDown),
      Some(
        sg
        ++ "module M : S = {\n  let x = 1\n} in\nlet y = 2 in\nlet b = 4 in\nM.x",
      ),
    ),
  ]);
};

/* a module the outline doesn't show (`let _ = {…}`) is stepped over like
   a collapsed one, so a move never lands out of sight; a named one takes
   the item in */
let unshown_module = () => {
  let u = "let a = 1 in\nlet _ = {\n  let x = 2\n} in\nlet b = 4 in\nb";
  check(list(string), "no row", ["a", "b"], outline(u));
  let seg = seg_of(u);
  let term = MakeTerm.Incr.term_of(seg);
  /* each `let`'s head, by its pattern */
  let head = name =>
    switch (
      List.find_map(
        (p: Piece.t) =>
          switch (p) {
          | Tile({children: [pat, ..._], _} as t)
              when String.trim(text_of(pat)) == name =>
            Some(t.id)
          | _ => None
          },
        seg,
      )
    ) {
    | Some(id) => id
    | None => fail("no let " ++ name)
    };
  check(
    pair(bool, bool),
    "expanded: a, not _",
    (true, false),
    (
      Web.OutlineTree.expanded(~folds=[], head("a"), term),
      Web.OutlineTree.expanded(~folds=[], head("_"), term),
    ),
  );
  cases([
    (
      "down past it",
      u,
      "a",
      op(MoveDown),
      Some("let _ = {\n  let x = 2\n} in\nlet a = 1 in\nlet b = 4 in\nb"),
    ),
    (
      "up past it",
      u,
      "b",
      op(MoveUp),
      Some("let a = 1 in\nlet b = 4 in\nlet _ = {\n  let x = 2\n} in\nb"),
    ),
    (
      "a named one takes it in",
      "let a = 1 in\nlet m = {\n  let x = 2\n} in\nlet b = 4 in\nb",
      "a",
      op(MoveDown),
      Some("let m = {\n  let a = 1;\n  let x = 2\n} in\nlet b = 4 in\nb"),
    ),
  ]);
};

/* a new test, `test ¿ end`, lands right after its row in its block's
   form, or above it as a run's first */
let new_tests = () => {
  let run = "let a = 1 in\ntest a == 1 end;\ntest a > 0 end;\na";
  let m = "module M = {\n  let a = 1;\n  test a == 1 end;\n  test a > 0 end\n} in\nM.a";
  cases([
    (
      "after a test in a run",
      run,
      "1",
      op(NewTestBelow),
      Some(
        {js|let a = 1 in
test a == 1 end;
test ¿ end;
test a > 0 end;
a|js},
      ),
    ),
    (
      "after a lone test",
      "let a = 1 in\ntest a == 1 end;\na",
      "1",
      op(NewTestBelow),
      Some({js|let a = 1 in
test a == 1 end;
test ¿ end;
a|js}),
    ),
    (
      "after a definition",
      "let a = 1 in\nlet b = 2 in\nb",
      "a",
      op(NewTestBelow),
      Some({js|let a = 1 in
test ¿ end;
let b = 2 in
b|js}),
    ),
    (
      "a member after a member test",
      m,
      "1",
      op(NewTestBelow),
      Some(
        {js|module M = {
  let a = 1;
  test a == 1 end;
  test ¿ end;
  test a > 0 end
} in
M.a|js},
      ),
    ),
    (
      "after a module's last test, without its `;`",
      m,
      "2",
      op(NewTestBelow),
      Some(
        {js|module M = {
  let a = 1;
  test a == 1 end;
  test a > 0 end;
  test ¿ end
} in
M.a|js},
      ),
    ),
    (
      "a new definition there goes below it too",
      m,
      "2",
      op(NewBelow),
      Some(
        {js|module M = {
  let a = 1;
  test a == 1 end;
  test a > 0 end;
  let new_def = ¿
} in
M.a|js},
      ),
    ),
    (
      "above the trailing expression",
      "let a = 1 in\na",
      "",
      op(NewTestBelow),
      Some({js|let a = 1 in
test ¿ end;
a|js}),
    ),
    (
      "a run's first",
      run,
      "1",
      op(NewTestAbove),
      Some(
        {js|let a = 1 in
test ¿ end;
test a == 1 end;
test a > 0 end;
a|js},
      ),
    ),
    (
      "a member run's first",
      m,
      "1",
      op(NewTestAbove),
      Some(
        {js|module M = {
  let a = 1;
  test ¿ end;
  test a == 1 end;
  test a > 0 end
} in
M.a|js},
      ),
    ),
  ]);
  /* the op lands on the new test's row, numbered in its run */
  List.iter(
    ((desc, src)) =>
      switch (edit(op(NewTestBelow), "2", seg_of(src))) {
      | Some((seg, Some(target))) =>
        let rows =
          Web.OutlineTree.(flatten(of_term(MakeTerm.Incr.term_of(seg))));
        let run =
          switch (
            List.find_opt(
              (n: Web.OutlineTree.node) => n.o_kind == KTests,
              rows,
            )
          ) {
          | Some(n) => n.o_children
          | None => fail(desc ++ ": no run")
          };
        check(
          list(string),
          desc ++ ": the run, renumbered",
          ["1", "2", "3", "4"],
          List.map((n: Web.OutlineTree.node) => n.o_label, run),
        );
        check(
          list(string),
          desc ++ ": the new row is 3",
          ["3"],
          List.filter_map(
            (n: Web.OutlineTree.node) =>
              n.o_id == Some(target) ? Some(n.o_label) : None,
            run,
          ),
        );
      | _ => fail(desc ++ ": refused or no row")
      },
    [
      (
        "top level",
        "let a = 1 in\ntest a == 1 end;\ntest a > 0 end;\ntest a < 2 end;\na",
      ),
      (
        "members",
        "module M = {\n  test 1 == 1 end;\n  test 2 == 2 end;\n  test 3 == 3 end;\n} in\n0",
      ),
    ],
  );
};

/* Tab: an item into the module just above it, as its last member, in
   place a level deeper; ⇧Tab: a module's last member out, just after it.
   Folded or empty, a module takes it all the same */
let nesting = () => {
  let x = "let a = 1 in\nmodule M = {\n  let x = 2;\n  let y = 3\n} in\nlet b = 4 in\nb";
  let into_x = "let a = 1 in\nmodule M = {\n  let x = 2;\n  let y = 3;\n  let b = 4\n} in\nb";
  let e = "let a = 1 in\nmodule E = {} in\nlet b = 2 in\nb";
  cases([
    ("into a module", x, "b", op(Indent), Some(into_x)),
    ("and out again", into_x, "b", op(Outdent), Some(x)),
    (
      "into an empty one",
      e,
      "b",
      op(Indent),
      Some("let a = 1 in\nmodule E = {\n  let b = 2\n} in\nb"),
    ),
    (
      "a lone member out",
      "let a = 1 in\nmodule E = {\n  let b = 2\n} in\nb",
      "b",
      op(Outdent),
      Some(e),
    ),
    ("nothing above", x, "a", op(Indent), None),
    (
      "a type above",
      "type T = Int in\nlet b = 4 in\nb",
      "b",
      op(Indent),
      None,
    ),
    ("a member that isn't last", x, "x", op(Outdent), None),
    ("the top level", x, "a", op(Outdent), None),
    ({js|⇒|js}, x, "", op(Indent), None),
    (
      "a use never goes in",
      "module M = {\n  let x = 2\n} in\nuse Nat in\nM.x",
      "use Nat",
      op(Indent),
      None,
    ),
    (
      "nor into a module the outline doesn't show",
      "let a = 1 in\nlet _ = {\n  let x = 2\n} in\nlet b = 4 in\nb",
      "b",
      op(Indent),
      None,
    ),
    (
      "a module bound by let",
      "let a = 1 in\nlet m = {\n  let x = 2\n} in\nlet b = 4 in\nb",
      "b",
      op(Indent),
      Some("let a = 1 in\nlet m = {\n  let x = 2;\n  let b = 4\n} in\nb"),
    ),
    (
      "a test",
      "let a = 1 in\nmodule M = {\n  let y = 3\n} in\ntest a == 1 end;\nlet b = 4 in\nb",
      "1",
      op(Indent),
      Some(
        "let a = 1 in\nmodule M = {\n  let y = 3;\n  test a == 1 end\n} in\nlet b = 4 in\nb",
      ),
    ),
    (
      "a test out",
      "let a = 1 in\nmodule M = {\n  let y = 3;\n  test a == 1 end\n} in\nlet b = 4 in\nb",
      "1",
      op(Outdent),
      Some(
        "let a = 1 in\nmodule M = {\n  let y = 3\n} in\ntest a == 1 end;\nlet b = 4 in\nb",
      ),
    ),
    (
      "a module into a module",
      "module M = {\n  let y = 3\n} in\nmodule N = {\n  let z = 4\n} in\nM.y",
      "N",
      op(Indent),
      Some(
        "module M = {\n  let y = 3;\n  module N = {\n    let z = 4\n  }\n} in\nM.y",
      ),
    ),
  ]);
  /* a folded module takes it too: the outline unfolds it */
  expect(
    "into a folded module",
    Some(into_x),
    run(~closed=true, op(Indent), "b", x),
  );
  /* a statement: its row is its `;` */
  let stmt = seg_of("module M = {\n  let y = 3\n} in\n1 + 1;\nM.y");
  let term = MakeTerm.Incr.term_of(stmt);
  let semi =
    List.find_map(
      (n: Web.OutlineTree.node) => n.o_kind == KStmt ? n.o_id : None,
      Web.OutlineTree.(flatten(of_term(term))),
    );
  let ctx: R.ctx = {
    mod_root: false,
    term,
    info_map: lazy(Language.Statics.Map.empty),
    is_open: _ => true,
  };
  expect(
    "a statement",
    Some("module M = {\n  let y = 3;\n  1 + 1\n} in\nM.y"),
    Option.bind(semi, id =>
      Result.to_option(R.edit(ctx, op(Indent, id), stmt))
      |> Option.map(fst)
    ),
  );
  /* a module-rooted program's members, and a signed module */
  let md = "let a = 1;\nmodule M = {\n  let x = 2\n};\nlet b = 4;\nb";
  let md_in = "let a = 1;\nmodule M = {\n  let x = 2;\n  let b = 4\n};\nb";
  expect(
    ~root=Mod,
    "module-rooted: in",
    Some(md_in),
    run(~root=Mod, op(Indent), "b", md),
  );
  expect(
    ~root=Mod,
    "module-rooted: out",
    Some(md),
    run(~root=Mod, op(Outdent), "b", md_in),
  );
  let sg = "type S = {\n  let x : Int\n} in\n";
  expect(
    "a signed module",
    Some(sg ++ "module M : S = {\n  let x = 1;\n  let b = 4\n} in\nM.x"),
    run(
      op(Indent),
      "b",
      sg ++ "module M : S = {\n  let x = 1\n} in\nlet b = 4 in\nM.x",
    ),
  );
  /* the row it lands on is the item's */
  switch (edit(op(Indent), "b", seg_of(x))) {
  | Some((seg, target)) =>
    check(
      bool,
      "it lands on b",
      true,
      target == Some(row_by_label(MakeTerm.Incr.term_of(seg), "b")),
    )
  | None => fail("b refused")
  };
};

/* Tab into a value: its body's last local definition, just before the
   result (inside a function's lambda); ⇧Tab out again, just after it */
let nesting_values = () => {
  let f = "let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nlet g = 5 in\nf(g)";
  let f_in = "let f = fun x ->\n  let y = x + 1 in\n  let g = 5 in\n  y * 2\nin\nf(g)";
  cases([
    ("into a function", f, "g", op(Indent), Some(f_in)),
    ("and out again", f_in, "g", op(Outdent), Some(f)),
    (
      "a constant's body gets one",
      "let x = 1 + 2 in\nlet moved = 5 in\nx",
      "moved",
      op(Indent),
      Some("let x = let moved = 5 in 1 + 2 in\nx"),
    ),
    (
      "on its own line when the body is",
      "let x =\n  1 + 2\nin\nlet moved = 5 in\nx",
      "moved",
      op(Indent),
      Some("let x =\n  let moved = 5 in\n  1 + 2\nin\nx"),
    ),
    (
      "a one-line function",
      "let f = fun x -> x * 2 in\nlet g = 5 in\nf(g)",
      "g",
      op(Indent),
      Some("let f = fun x -> let g = 5 in x * 2 in\nf(g)"),
    ),
    (
      "out of a one-line body, onto a line of its own",
      "let x = let moved = 5 in 1 + 2 in\nx",
      "moved",
      op(Outdent),
      Some("let x = 1 + 2 in\nlet moved = 5 in\nx"),
    ),
    (
      "a member value",
      "module M = {\n  let f = fun x ->\n    x;\n  let g = 5\n} in\nM.f(1)",
      "g",
      op(Indent),
      Some(
        "module M = {\n  let f = fun x ->\n    let g = 5 in\n    x\n} in\nM.f(1)",
      ),
    ),
    (
      "out of a member value",
      "module M = {\n  let f = fun x ->\n    let g = 5 in\n    x\n} in\nM.f(1)",
      "g",
      op(Outdent),
      Some(
        "module M = {\n  let f = fun x ->\n    x;\n  let g = 5\n} in\nM.f(1)",
      ),
    ),
    (
      "a curried function: inside its last lambda",
      "let f = fun x -> fun y ->\n  x + y\nin\nlet g = 5 in\ng",
      "g",
      op(Indent),
      Some("let f = fun x -> fun y ->\n  let g = 5 in\n  x + y\nin\ng"),
    ),
    (
      "a function only in parentheses: refused",
      "let f = (fun x -> x) in\nlet g = 5 in\ng",
      "g",
      op(Indent),
      None,
    ),
    (
      "nor a module alias",
      "module N = {} in\nmodule M = N in\nlet b = 2 in\nb",
      "b",
      op(Indent),
      None,
    ),
    (
      "nor across an item with no row",
      "module M = {\n  let x = 1\n} in\nlet _ = 5 in\nlet b = 2 in\nb",
      "b",
      op(Indent),
      None,
    ),
    (
      "not the last local definition",
      "let f = fun x ->\n  let y = 1 in\n  let z = 2 in\n  y\nin\nf",
      "y",
      op(Outdent),
      None,
    ),
    (
      "a test",
      "let f = fun x ->\n  x\nin\ntest f(1) == 1 end;\nf",
      "1",
      op(Indent),
      Some("let f = fun x ->\n  test f(1) == 1 end;\n  x\nin\nf"),
    ),
  ]);
};

/* ---- an op and its inverse give the program back exactly ---- */

/* [steps] (an op and its row's label each) in turn from [src]: the text
   after each, None where one is refused */
let walk = (~root=Sort.Exp, src: string, steps) => {
  let rec go = (seg, steps) =>
    switch (steps) {
    | [] => []
    | [(o, label), ...rest] =>
      switch (edit(~root, op(o), label, seg)) {
      | Some((seg, _)) => [Some(text_of(seg)), ...go(seg, rest)]
      | None => [None]
      }
    };
  go(seg_of(~root, src), steps);
};

/* [o] on [label] makes [mid], and [back] there gives [src] again */
let law = (~root=Sort.Exp, desc, src, (o, back), label, mid) =>
  check(
    list(option(string)),
    desc,
    [Some(mid), Some(src)],
    walk(~root, src, [(o, label), (back, label)]),
  );

/* an item takes the gap above it along (below it, first in its block),
   and brings it back: blank lines stay where they were */
let gaps = () => {
  law(
    ~root=Mod,
    "a last member out and back",
    "module M = {\n  let a = 1;\n\n  let b = 2\n};\nlet c = 3;\nc",
    (MoveDown, MoveUp),
    "b",
    "module M = {\n  let a = 1\n};\n\nlet b = 2;\nlet c = 3;\nc",
  );
  /* blank lines inside a moved item stay blank */
  law(
    ~root=Mod,
    "a module into the module above and out",
    "module A = {\n  let a = 1\n};\n\nmodule B = {\n  let b = 2;\n\n  let c = 3\n};\nA.a",
    (MoveUp, MoveDown),
    "B",
    "module A = {\n  let a = 1;\n\n  module B = {\n    let b = 2;\n\n    let c = 3\n  }\n};\nA.a",
  );
  law(
    ~root=Mod,
    "into a body and out, the gap below it kept",
    "module M = {\n  let f = fun x ->\n    x;\n  let g = 2;\n\n  let h = 3\n};\nM.h",
    (Indent, Outdent),
    "g",
    "module M = {\n  let f = fun x ->\n    let g = 2 in\n    x;\n\n  let h = 3\n};\nM.h",
  );
  /* a blank line holding spaces, as a paste leaves it, comes back so */
  law(
    ~root=Mod,
    "a blank line with spaces on it",
    "module M = {\n  let a = 1;\n  \n  let b = 2\n};\nlet c = 3;\nc",
    (MoveDown, MoveUp),
    "b",
    "module M = {\n  let a = 1\n};\n  \nlet b = 2;\nlet c = 3;\nc",
  );
  /* a module-rooted program's last member: back on a line of its own */
  law(
    ~root=Mod,
    "the last member of a module-rooted program",
    "let start = 0;\nlet next = start + 1;\nlet after = next + 1",
    (Indent, Outdent),
    "after",
    "let start = 0;\nlet next = let after = next + 1 in start + 1",
  );
};

/* a doc comment starts a line: an item with one goes into a one-line
   body on lines of its own, a constant's value on its own line below,
   a function's result after it; and comes back out whole */
let doc_comments = () => {
  let src = "let init = (1, 2) in\n\n# doc for f #\nlet f = fun x -> x in\n\n# doc for g #\nlet g = 2 in\nf";
  law(
    "into a constant",
    src,
    (Indent, Outdent),
    "f",
    "let init =\n  # doc for f #\n  let f = fun x -> x in\n  (1, 2) in\n\n# doc for g #\nlet g = 2 in\nf",
  );
  law(
    "into a function",
    src,
    (Indent, Outdent),
    "g",
    "let init = (1, 2) in\n\n# doc for f #\nlet f = fun x ->\n  # doc for g #\n  let g = 2 in x in\nf",
  );
};

/* an item's first line keeps its offset from its neighbour's: an oddly
   indented one stays so, and one landing by an odd one isn't */
let offsets = () => {
  law(
    ~root=Mod,
    "an oddly indented member out and back",
    "module M = {\n  let a = 1;\n\n    let b = 2\n};\n\nlet c = 3;\nc",
    (MoveDown, MoveUp),
    "b",
    "module M = {\n  let a = 1\n};\n\n  let b = 2;\n\nlet c = 3;\nc",
  );
  law(
    ~root=Mod,
    "out beside its owner, above an oddly indented one",
    "module M = {\n  let f = fun x ->\n    x;\n\n  let g = 1;\n\n    let h = 2\n};\nM.f",
    (Indent, Outdent),
    "g",
    "module M = {\n  let f = fun x ->\n    let g = 1 in\n\n    x;\n\n    let h = 2\n};\nM.f",
  );
};

/* a `;` statement doesn't go into a member's body, where its `;` would
   end the member: Tab refuses it, and no new test goes there. A let-in
   definition's body takes one */
let member_statements = () => {
  expect(
    "a test into a member's function",
    None,
    run(
      op(Indent),
      "1",
      "module M = {\n  let f = fun x ->\n    x;\n  test 1 == 1 end\n} in\nM.f",
    ),
  );
  expect(
    "a new test in a member's body",
    None,
    run(
      op(NewTestBelow),
      "y",
      "module M = {\n  let f = fun x ->\n    let y = x in\n    y;\n  let g = 1\n} in\nM.f",
    ),
  );
  expect(
    "a new test in a let-in definition's body",
    Some(
      {js|module M = {
  let f = fun x ->
    let g = fun z ->
      let y = z in
      test ¿ end;
      y
    in
    g(x);
  let h = 1
} in
M.f|js},
    ),
    run(
      op(NewTestBelow),
      "y",
      "module M = {\n  let f = fun x ->\n    let g = fun z ->\n      let y = z in\n      y\n    in\n    g(x);\n  let h = 1\n} in\nM.f",
    ),
  );
};

/* comments set apart from the items stay on their side: an item leaving
   its owner lands right beside it, and nothing goes into an item across
   such a comment (it couldn't come back to its side) */
let set_apart = () => {
  let src = "module H = {\n  let q = fun x ->\n    x;\n\n  let v = 1;\n\n  # ===== STATE ===== #\n\n  let init = 0\n};\nH.v";
  law(
    ~root=Mod,
    "out beside its owner, above the comment",
    src,
    (Indent, Outdent),
    "v",
    "module H = {\n  let q = fun x ->\n    let v = 1 in\n\n    x;\n\n  # ===== STATE ===== #\n\n  let init = 0\n};\nH.v",
  );
  expect(
    ~root=Mod,
    "no Tab across it",
    None,
    run(~root=Mod, op(Indent), "init", src),
  );
  /* Alt↑ steps over the module instead of going in */
  expect(
    ~root=Mod,
    "no Alt into a module across it",
    Some(
      "let b = 2;\n\n# ===== B ===== #\n\nmodule A = {\n  let a = 1\n};\nA.a",
    ),
    run(
      ~root=Mod,
      op(MoveUp),
      "b",
      "module A = {\n  let a = 1\n};\n\n# ===== B ===== #\n\nlet b = 2;\nA.a",
    ),
  );
};

/* ⌘⌫ on a row the outline refuses to delete leaves its cursor there */
let refused_delete_cursor = (): unit => {
  let seg = seg_of("let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nf(1)");
  let term = MakeTerm.Incr.term_of(seg);
  let rows = Web.OutlineTree.flatten(Web.OutlineTree.of_term(term));
  let path = (n: Web.OutlineTree.node) =>
    Option.bind(n.o_id, id => Web.OutlineTree.label_path(id, term));
  /* f's ⇒ row; the program's own comes after it */
  let result =
    List.find((n: Web.OutlineTree.node) => n.o_kind == KTrail, rows);
  let visible =
    Array.of_list(
      List.map(
        (n: Web.OutlineTree.node): Web.OutlineSidebar.visible_row =>
          {
            r_path: Option.get(path(n)),
            r_node: n,
            r_name: n.o_label,
            r_parent: None,
            r_expanded: n.o_children != [],
          },
        rows,
      ),
    );
  let cursor = ref(path(result));
  let refused = ref(false);
  let on =
    OutlineHarness.handlers(
      ~cursor,
      ~def_op=
        (op, id) => {
          refused := R.apply(op, id, seg) == None;
          Virtual_dom.Vdom.Effect.Ignore;
        },
      (),
    );
  let props = OutlineHarness.props(~cursor=cursor^, ());
  let cmd_backspace = OutlineHarness.keydown(~meta=true, "Backspace");
  ignore(Web.OutlineSidebar.keys(~props, ~on, ~visible, cmd_backspace));
  check(bool, "the delete was tried and refused", true, refused^);
  check(bool, "the cursor stays on the row", true, cursor^ == path(result));
};

let tests = (
  "Restructure",
  [
    test_case("a refused op does nothing", `Quick, refused),
    test_case("a module's last test is a member", `Quick, last_test),
    test_case(
      "a refused delete leaves the outline cursor",
      `Quick,
      refused_delete_cursor,
    ),
    test_case("ops keep the layout", `Quick, layout),
    test_case("top-level ops", `Quick, top_level),
    test_case("new tests", `Quick, new_tests),
    test_case("member ops", `Quick, members),
    test_case("flattened fn-body ops", `Quick, fn_body),
    test_case("a member whose body opens with a let", `Quick, let_body),
    test_case("unterminated last member", `Quick, unterminated_tail),
    test_case("moves across module edges", `Quick, across),
    test_case("a last member out and back in", `Quick, back_in),
    test_case("a module bound by let", `Quick, value_module),
    test_case("moves into a module bound by let", `Quick, into_value_module),
    test_case("in and out again", `Quick, round_trip),
    test_case("use rows", `Quick, use_rows),
    test_case("a module with a signature", `Quick, signed_module),
    test_case("moves step over unshown modules", `Quick, unshown_module),
    test_case("Tab and Shift+Tab across module edges", `Quick, nesting),
    test_case(
      "Tab and Shift+Tab across value bodies",
      `Quick,
      nesting_values,
    ),
    test_case("ops and their inverses keep blank lines", `Quick, gaps),
    test_case("a doc comment goes in and out whole", `Quick, doc_comments),
    test_case("an item keeps its indentation's offset", `Quick, offsets),
    test_case("no statement into a member's body", `Quick, member_statements),
    test_case("comments set apart stay on their side", `Quick, set_apart),
  ],
);
