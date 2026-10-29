open Alcotest;
open Haz3lcore;
open Language;
open CorpusUtil;

/* outline rename, as typed on the row's line: the binder, its references
   and `M.x` labels change; a capturing rename is refused; a kind keyword
   first changes the item's kind */

let rename = (~root=Sort.Exp, src, label, name) => {
  let seg = seg_of(~root, src);
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  CorpusUtil.rename_row(
    ~root,
    ~info_map,
    ~outline=CorpusUtil.outline_of(~root, seg),
    row_by_label(CorpusUtil.outline_of(~root, seg), label),
    name,
    seg,
  )
  |> Result.map(text_of);
};

let renamed = (~root=Sort.Exp, src, label, name, expected) =>
  switch (rename(~root, src, label, name)) {
  | Ok(txt) =>
    check(
      string,
      label ++ " -> " ++ name,
      text_of(seg_of(~root, expected)),
      txt,
    )
  | Error(why) => fail("refused: " ++ why)
  };

let refused = (~root=Sort.Exp, src, label, name) =>
  switch (rename(~root, src, label, name)) {
  | Ok(txt) => fail("renamed: " ++ txt)
  | Error(_) => ()
  };

let changed = (~root=Sort.Exp, src, label, text, expected) =>
  switch (rename(~root, src, label, text)) {
  | Ok(txt) =>
    check(
      string,
      label ++ " -> " ++ text,
      text_of(seg_of(~root, expected)),
      txt,
    )
  | Error(why) => fail(label ++ " -> " ++ text ++ " refused: " ++ why)
  };

/* refused as typed, and why */
let refused_as = (src, label, text, why) =>
  switch (rename(src, label, text)) {
  | Ok(txt) => fail("renamed: " ++ txt)
  | Error(got) => check(string, label ++ " -> " ++ text, why, got)
  };

let values = () =>
  renamed(
    "let foo = 1 in\nlet bar = foo + 1 in\nbar + foo",
    "foo",
    "baz",
    "let baz = 1 in\nlet bar = baz + 1 in\nbar + baz",
  );

let members = () =>
  renamed(
    "module M = {\n  let x = 1;\n  let y = x + 1\n} in\nM.x + M.y",
    "x",
    "z",
    "module M = {\n  let z = 1;\n  let y = z + 1\n} in\nM.z + M.y",
  );

let types = () =>
  renamed(
    "type T = Int in\nlet a : T = 1 in\na",
    "T",
    "U",
    "type U = Int in\nlet a : U = 1 in\na",
  );

let modules = () =>
  renamed(
    "module M = {\n  let x = 1\n} in\nM.x",
    "M",
    "N",
    "module N = {\n  let x = 1\n} in\nN.x",
  );

/* a module under a signature: its name and `M.x`, the signature kept */
let signed_module = () =>
  renamed(
    "type S = {\n  let x : Int\n} in\nmodule M : S = {\n  let x = 1\n} in\nM.x",
    "M",
    "N",
    "type S = {\n  let x : Int\n} in\nmodule N : S = {\n  let x = 1\n} in\nN.x",
  );

let mod_root = () =>
  renamed(
    ~root=Mod,
    "let x = 1;\nlet y = x + 1;\ny",
    "x",
    "w",
    "let w = 1;\nlet y = w + 1;\ny",
  );

let capture = () => {
  /* the tail's `foo` would now find the other binding */
  refused("let foo = 1 in\nlet bar = foo + 1 in\nbar + foo", "foo", "bar");
  /* an outer `z` used inside would now find the renamed binding */
  refused("let z = 5 in\nlet f = fun y -> y + z in\nf(1)", "f", "z");
};

let names = () => {
  refused("let foo = 1 in foo", "foo", "Foo");
  refused("let foo = 1 in foo", "foo", "let");
  refused("let foo = 1 in foo", "foo", "two words");
  refused("type T = Int in 1", "T", "t");
  /* the wildcard, literals, any form's keyword, qualified names */
  List.iter(
    name => refused("let foo = 1 in foo", "foo", name),
    ["_", "true", "end", "rec", "theorem", "M.x", "$x"],
  );
  renamed("let foo = 1 in foo", "foo", "foo_2'", "let foo_2' = 1 in foo_2'");
};

/* each refusal's reason, short for the narrow outline */
let messages = () => {
  let src = "let foo = 1 in\nlet bar = foo + 1 in\nbar + foo";
  List.iter(
    ((name, why)) => refused_as(src, "foo", name, why),
    [
      ("let", "let is a keyword"),
      ("module", "module is a keyword"),
      ("two words", "names have no spaces"),
      ("M.x", "a name is one identifier"),
      ("$x", "a name is one identifier"),
      ("end", "a name is one identifier"),
      ("Foo", "value names start lowercase"),
      ("bar", "bar is already in use here"),
    ],
  );
  refused_as(
    "type T = Int in 1",
    "T",
    "t",
    "type and module names start with a capital",
  );
  /* an outer name used inside would now find the renamed item */
  refused_as(
    "let z = 5 in\nlet f = fun y -> y + z in\nf(1)",
    "f",
    "z",
    "z is already in use here",
  );
};

/* a kind keyword typed first changes the item's kind: a value's `{…}`
   and a module's, both ways, its uses with it; a definition of only
   holes, any kind's, remade blank; its own kind's keyword is dropped */
let kinds = () => {
  let m = "let m = {\n  let x = 1\n} in\nm.x";
  let mm = "module M = {\n  let x = 1\n} in\nM.x";
  changed(m, "m", "module M", mm);
  changed(mm, "M", "let m", m);
  changed(m, "m", "let n", "let n = {\n  let x = 1\n} in\nn.x");
  changed(mm, "M", "module N", "module N = {\n  let x = 1\n} in\nN.x");
  changed(mm, "M", "module N  ", "module N = {\n  let x = 1\n} in\nN.x");
  changed(
    "module A = {\n  let m = {\n    let x = 1\n  };\n  let y = m.x\n} in\nA.y",
    "m",
    "module M",
    "module A = {\n  module M = {\n    let x = 1\n  };\n  let y = M.x\n} in\nA.y",
  );
  changed(
    ~root=Mod,
    "let m = {\n  let x = 1\n};\nm.x",
    "m",
    "module M",
    "module M = {\n  let x = 1\n};\nM.x",
  );
  changed({js|let x = ¿ in
1|js}, "x", "type T", {js|type T = ¿ in
1|js});
  changed({js|type T = ¿ in
1|js}, "T", "module M", "module M = {} in\n1");
  changed("module M = {} in\n1", "M", "let y", {js|let y = ¿ in
1|js});
  changed({js|let x = ¿ in
x|js}, "x", "module X", "module X = {} in\nX");
  changed(
    ~root=Mod,
    {js|let a = 1;
let x = ¿;
a|js},
    "x",
    "type X",
    {js|let a = 1;
type X = ¿;
a|js},
  );
  refused_as(
    "let x = 1 in\nx",
    "x",
    "module X",
    {js|only a {…} body can be a module|js},
  );
  refused_as("let x = 1 in\nx", "x", "type X", "a type needs a type body");
  refused_as(mm, "M", "type M", "a type needs a type body");
  refused_as(
    "type T = Int in\n1",
    "T",
    "let t",
    "a value needs a value body",
  );
  refused_as(
    m,
    "m",
    "module m",
    "type and module names start with a capital",
  );
  refused_as(mm, "M", "let M", "value names start lowercase");
  refused_as(
    {js|let (a, b) = ¿ in
a|js},
    "a",
    "type A",
    "only a plain name changes kind",
  );
};

/* the item's own ids stay: its row's, its binding token's, and a kept
   definition's */
let kind_ids = () => {
  let seg = seg_of("let m = {\n  let x = 1\n} in\nm.x");
  let term = MakeTerm.Incr.term_of(seg);
  let outline = CorpusUtil.outline_of(seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  let (m, x) = (row_by_label(outline, "m"), row_by_label(outline, "x"));
  let binder =
    switch (Web.OutlineTree.node_of(m, outline)) {
    | Some({o_binder: Some((b, _)), _}) => b
    | _ => fail("no binder")
    };
  switch (CorpusUtil.rename_row(~info_map, ~outline, m, "module M", seg)) {
  | Ok(seg) =>
    let outline = CorpusUtil.outline_of(seg);
    check(bool, "the row's", true, row_by_label(outline, "M") == m);
    check(bool, "a member's", true, row_by_label(outline, "x") == x);
    check(
      bool,
      "the binding token's",
      true,
      switch (Web.OutlineTree.node_of(m, outline)) {
      | Some({o_binder: Some((b, _)), _}) => b == binder
      | _ => false
      },
    );
  | Error(why) => fail("refused: " ++ why)
  };
};

/* a module under a signature is a value under it, as the language allows
   (no errors), and back */
let signed_kinds = () => {
  let s = "type S = {\n  let x : Int\n} in\n";
  let as_value = s ++ "let m : S = {\n  let x = 1\n} in\nm.x";
  changed(
    s ++ "module M : S = {\n  let x = 1\n} in\nM.x",
    "M",
    "let m",
    as_value,
  );
  changed(
    as_value,
    "m",
    "module M",
    s ++ "module M : S = {\n  let x = 1\n} in\nM.x",
  );
  let term = MakeTerm.Incr.term_of(seg_of(as_value));
  check(
    int,
    "the annotation is the language's",
    0,
    List.length(
      Statics.Map.error_ids(
        DefStatics.calc(~settings=CoreSettings.on, term).merged,
      ),
    ),
  );
};

/* a type member used qualified, in an annotation */
let qualified_type = () =>
  renamed(
    "module U = {\n  type Meters = Int;\n  let x : Meters = 1\n} in\nlet d : U.Meters = 5 in\nd",
    "Meters",
    "Feet",
    "module U = {\n  type Feet = Int;\n  let x : Feet = 1\n} in\nlet d : U.Feet = 5 in\nd",
  );

/* qualified uses through nested modules, a let-bound module and at a Mod
   root; another member's name is refused */
let qualified_members = () => {
  renamed(
    "module Outer = {\n  module Inner = {\n    let x = 1\n  };\n  let w = Inner.x\n} in\nOuter.Inner.x",
    "x",
    "z",
    "module Outer = {\n  module Inner = {\n    let z = 1\n  };\n  let w = Inner.z\n} in\nOuter.Inner.z",
  );
  renamed(
    "let m = {\n  let x = 1\n} in\nm.x",
    "x",
    "z",
    "let m = {\n  let z = 1\n} in\nm.z",
  );
  renamed(
    ~root=Mod,
    "module M = {\n  let x = 1\n};\nlet x = M.x;\nx",
    "x",
    "z",
    "module M = {\n  let z = 1\n};\nlet x = M.z;\nx",
  );
  refused_as(
    "module M = {\n  let x = 1;\n  let y = 2\n} in\nM.x + M.y",
    "y",
    "x",
    "x is already in use here",
  );
};

/* a use folded away by a projector is renamed with the rest */
let folded = () => {
  let seg = seg_of("let foo = 1 in\nlet bar = foo + 1 in\nbar");
  let count = ref(0);
  let rec fold = (seg: Segment.t): Segment.t =>
    List.map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when Tile.label(t) == ["foo"] =>
          incr(count);
          count^ == 2 ? Piece.Projector(ProjectorCore.mk(Fold, p, "")) : p;
        | Tile(t) =>
          Piece.Tile({
            ...t,
            children: List.map(fold, t.children),
          })
        | p => p
        },
      seg,
    );
  let seg = fold(seg);
  let term = MakeTerm.Incr.term_of(seg);
  let outline = CorpusUtil.outline_of(seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  let rec inside = (seg: Segment.t): list(string) =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Projector({syntax: Tile(t), _}) => Tile.label(t)
        | Tile(t) => List.concat_map(inside, t.children)
        | _ => []
        },
      seg,
    );
  switch (
    CorpusUtil.rename_row(
      ~info_map,
      ~outline,
      row_by_label(outline, "foo"),
      "baz",
      seg,
    )
  ) {
  | Ok(seg) => check(list(string), "the folded use", ["baz"], inside(seg))
  | Error(why) => fail("refused: " ++ why)
  };
};

let tests = (
  "OutlineRename",
  [
    test_case("values", `Quick, values),
    test_case("module members and M.x", `Quick, members),
    test_case("types", `Quick, types),
    test_case("modules", `Quick, modules),
    test_case("a module with a signature", `Quick, signed_module),
    test_case("module-rooted program", `Quick, mod_root),
    test_case("capture is refused", `Quick, capture),
    test_case("names are checked", `Quick, names),
    test_case("each refusal's reason", `Quick, messages),
    test_case("a keyword changes the kind", `Quick, kinds),
    test_case("a kind change keeps ids", `Quick, kind_ids),
    test_case("a signed module's kind", `Quick, signed_kinds),
    test_case("a qualified type use", `Quick, qualified_type),
    test_case("qualified member uses", `Quick, qualified_members),
    test_case("a folded use", `Quick, folded),
  ],
);
