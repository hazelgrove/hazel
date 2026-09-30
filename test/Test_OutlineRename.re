open Alcotest;
open Haz3lcore;
open Language;

/* outline rename: the binder, its references and `M.x` labels change; a
   capturing rename is refused */

let parse = (~root=Sort.Exp, src: string): Segment.t =>
  switch (FastParse.of_text(~root, src)) {
  | Some(seg) => seg
  | None => failwith("parse failed: " ++ src)
  };

let text_of = (seg: Segment.t): string =>
  seg |> Zipper.unzip |> MarkerParse.to_text;

let term_of = (~root, seg) =>
  root == Sort.Mod
    ? MakeTerm.Incr.term_of_mod(seg) : MakeTerm.Incr.term_of(seg);

let row = (term, label: string): Id.t => {
  let rec find = (ns: list(Web.OutlineTree.node)) =>
    List.fold_left(
      (acc, n: Web.OutlineTree.node) =>
        switch (acc) {
        | Some(_) => acc
        | None => n.o_label == label ? n.o_id : find(n.o_children)
        },
      None,
      ns,
    );
  switch (find(Web.OutlineTree.of_term(term))) {
  | Some(id) => id
  | None => failwith("no row " ++ label)
  };
};

let rename = (~root=Sort.Exp, src, label, name) => {
  let seg = parse(~root, src);
  let term = term_of(~root, seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  Web.OutlineRename.rename(~info_map, ~term, row(term, label), name, seg)
  |> Result.map(text_of);
};

let renamed = (~root=Sort.Exp, src, label, name, expected) =>
  switch (rename(~root, src, label, name)) {
  | Ok(txt) =>
    check(
      string,
      label ++ " -> " ++ name,
      text_of(parse(~root, expected)),
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
  ],
);
