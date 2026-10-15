open Alcotest;
open Haz3lcore;
open CorpusUtil;

/* outline restructure ops across block kinds, run as the outline runs
   them (ItemEdit.edit), checked on the resulting program */

module R = Web.ItemEdit;

let op = (o, id) => R.Op(o, id);
let inside = (text, id) => R.InsertInside(id, text);

/* [e] on the row labeled [label]: the new program and the row it lands
   on, or None where it's refused. ~closed: moves step over modules */
let edit =
    (~root=Sort.Exp, ~closed=false, e: Id.t => R.t, label, seg: Segment.t)
    : option((Segment.t, option(Id.t))) => {
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  let ctx: R.ctx = {
    mod_root: root == Sort.Mod,
    term,
    info_map: lazy(Language.Statics.Map.empty),
    is_open: _ => !closed,
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
  /* a module's last test without its `;` */
  let m = "let z = 0 in\nmodule M = {\n  let a = 1;\n  test 1 + 1 == 2 end\n} in\nM.a";
  cases([
    ("result row: delete", f, "", op(Delete), None),
    ("result row: duplicate", f, "", op(Duplicate), None),
    ("result row: move up", f, "", op(MoveUp), None),
    ("result row: move down", f, "", op(MoveDown), None),
    ("last test: delete", m, "1", op(Delete), None),
    ("last test: duplicate", m, "1", op(Duplicate), None),
    ("last test: Alt+Up leaves the module put", m, "1", op(MoveUp), None),
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
  let none = _ => Virtual_dom.Vdom.Effect.Ignore;
  let on: Web.OutlineSidebar.handlers = {
    jump: none,
    focus: none,
    toggle: none,
    toggle_run: none,
    toggle_collapse: none,
    zoom_to: none,
    zoom_in: none,
    show_whole: none,
    discard: Virtual_dom.Vdom.Effect.Ignore,
    zoom_out: Virtual_dom.Vdom.Effect.Ignore,
    get_cursor: () => cursor^,
    set_cursor: c => {
      cursor := c;
      Virtual_dom.Vdom.Effect.Ignore;
    },
    focused: Virtual_dom.Vdom.Effect.Ignore,
    leave: Virtual_dom.Vdom.Effect.Ignore,
    edit: {
      current: None,
      get: () => None,
      set: none,
      commit: (_, _) => Virtual_dom.Vdom.Effect.Ignore,
    },
    menu_open: (_, _, _, _) => Virtual_dom.Vdom.Effect.Ignore,
    menu_close: Virtual_dom.Vdom.Effect.Ignore,
    menu_select: none,
    get_menu: () => None,
    get_menu_sel: () => 0,
    def_op: (op, id) => {
      refused := R.apply(op, id, seg) == None;
      Virtual_dom.Vdom.Effect.Ignore;
    },
  };
  let props: Web.OutlineSidebar.props = {
    arrows: true,
    stack_controls: true,
    can_open: _ => true,
    is_collapsed: _ => false,
    header: {
      h_program: "",
      h_trail: [],
      h_open: 0,
      h_parked: 0,
    },
    zoom_root: None,
    cursor: cursor^,
    created: None,
    pinned: [],
    focused_entries: [],
    error_items: [],
    error_subtree: [],
    menu: None,
    menu_sel: 0,
    test_status: _ => None,
  };
  let cmd_backspace =
    Js_of_ocaml.Js.(
      Unsafe.coerce(
        Unsafe.obj([|
          ("key", Unsafe.inject(string("Backspace"))),
          ("code", Unsafe.inject(string("Backspace"))),
          ("metaKey", Unsafe.inject(_true)),
          ("ctrlKey", Unsafe.inject(_false)),
          ("altKey", Unsafe.inject(_false)),
          ("shiftKey", Unsafe.inject(_false)),
        |]),
      )
    );
  ignore(Web.OutlineSidebar.keys(~props, ~on, ~visible, cmd_backspace));
  check(bool, "the delete was tried and refused", true, refused^);
  check(bool, "the cursor stays on the row", true, cursor^ == path(result));
};

let tests = (
  "Restructure",
  [
    test_case("a refused op does nothing", `Quick, refused),
    test_case(
      "a refused delete leaves the outline cursor",
      `Quick,
      refused_delete_cursor,
    ),
    test_case("ops keep the layout", `Quick, layout),
    test_case("top-level ops", `Quick, top_level),
    test_case("member ops", `Quick, members),
    test_case("flattened fn-body ops", `Quick, fn_body),
    test_case("a member whose body opens with a let", `Quick, let_body),
    test_case("unterminated last member", `Quick, unterminated_tail),
    test_case("moves across module edges", `Quick, across),
    test_case("a last member out and back in", `Quick, back_in),
    test_case("a module bound by let", `Quick, value_module),
    test_case("moves into a module bound by let", `Quick, into_value_module),
    test_case("in and out again", `Quick, round_trip),
  ],
);
