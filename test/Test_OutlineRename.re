open Alcotest;
open Haz3lcore;
open Language;
open CorpusUtil;

/* outline rename: the binder, its references and `M.x` labels change; a
   capturing rename is refused */

let rename = (~root=Sort.Exp, src, label, name) => {
  let seg = seg_of(~root, src);
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  Web.OutlineRename.rename(
    ~info_map,
    ~term,
    row_by_label(term, label),
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

/* a type member used qualified, in an annotation */
let qualified_type = () =>
  renamed(
    "module U = {\n  type Meters = Int;\n  let x : Meters = 1\n} in\nlet d : U.Meters = 5 in\nd",
    "Meters",
    "Feet",
    "module U = {\n  type Feet = Int;\n  let x : Feet = 1\n} in\nlet d : U.Feet = 5 in\nd",
  );

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
    Web.OutlineRename.rename(
      ~info_map,
      ~term,
      row_by_label(term, "foo"),
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
    test_case("module-rooted program", `Quick, mod_root),
    test_case("capture is refused", `Quick, capture),
    test_case("names are checked", `Quick, names),
    test_case("a qualified type use", `Quick, qualified_type),
    test_case("a folded use", `Quick, folded),
  ],
);
