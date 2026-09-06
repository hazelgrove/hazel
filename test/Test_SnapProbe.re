open Alcotest;
open Haz3lcore;

let try_text = (label, text) =>
  test_case(label, `Quick, () =>
    switch (AgentToolResult.segment_of_text(text)) {
    | _ => ()
    | exception e =>
      fail(
        "segment_of_text raised " ++ Printexc.to_string(e) ++ " on: " ++ text,
      )
    }
  );

/* the agent-context view: the program with every top-level definition
   folded, built from per-item statics (the chat's "View Agent Context") */
let try_context = (label, code) =>
  test_case(label, `Quick, () =>
    switch (Parser.to_zipper(~root=Exp, code)) {
    | None => fail("parse failed")
    | Some(z) =>
      let editor = Editor.Model.mk(z, ~root=Exp);
      switch (CompositionView.Public.print(editor, AgentContext.Utils.init())) {
      | s => check(bool, "nonempty", true, String.length(s) > 0)
      | exception e => fail("context view raised " ++ Printexc.to_string(e))
      };
    }
  );

/* the canvas node map from the editor's COMPOSITIONAL record with the
   caret INSIDE a module member (andrew's live crash: the monolithic
   walker descended into the module item's surrogate def, whose
   scaffolding ids the per-item map strips — Not_found in view) */
let try_canvas_nodemap = (label, code, ~caret_token) =>
  test_case(label, `Quick, () =>
    switch (Parser.to_zipper(~root=Exp, code)) {
    | None => fail("parse failed")
    | Some(z) =>
      /* put the caret at the first occurrence of [caret_token] */
      let rec seek = (z, n) =>
        n > 4000
          ? z
          : (
            switch (Zipper.generalized_neighbors(z)) {
            | (_, Some(Piece.Tile(t))) when List.mem(caret_token, t.label) => z
            | _ =>
              switch (Move.by_token(Right, z)) {
              | Some(z') => seek(z', n + 1)
              | None => z
              }
            }
          );
      let z = seek(Move.to_start(z), 0);
      let comp =
        CachedStatics.init_compositional(
          ~settings=Language.CoreSettings.on,
          ~stitch=x => x,
          ~root=Exp,
          z,
        );
      switch (HighLevelNodeMap.build_for(z, comp)) {
      | Some(nm) => check(bool, "nonempty", true, Id.Map.cardinal(nm) > 0)
      | None => fail("node map None")
      | exception e => fail("build_for raised " ++ Printexc.to_string(e))
      };
      /* and the legacy builder must not raise on a per-item map either */
      switch (HighLevelNodeMap.build(z, comp.info_map)) {
      | _ => ()
      | exception e => fail("build raised " ++ Printexc.to_string(e))
      };
    }
  );

let tests = [
  try_canvas_nodemap(
    "canvas node map: caret inside a module member (compositional record)",
    "let x = 1 in\nmodule M = {\n  let a = x;\n  let b = a + 1;\n} in\nM.b",
    ~caret_token="a",
  ),
  try_canvas_nodemap(
    "canvas node map: caret inside a let def (compositional record)",
    "let f = fun n -> n + 1 in\nlet g = f(2) in\ng",
    ~caret_token="n",
  ),
  try_text("whole program", "let x = 1 in\nx + 1\n"),
  try_text("def only (update_definition diff)", "1 + 2"),
  try_text("let without body (defs_exclude_bodies)", "let f = fun x -> x in"),
  try_text("module member", "let name(i: Item): String = \"x\";"),
  try_text("module program", "module M = {\n  let a = 1;\n} in\nM.a\n"),
  try_text("empty", ""),
  try_text("hole", "?"),
  try_context("context: let chain", "let a = 1 in\nlet b = 2 in\na + b"),
  try_context(
    "context: module program",
    "module M = {\n  let a = 1;\n  let b = 2;\n} in\nM.a",
  ),
  try_context("context: dungeon program", Test_MergeProbe.program),
  try_context("context: evolved dungeon program", Test_MergeProbe.evolved),
  try_context("context: empty program", "?"),
];
