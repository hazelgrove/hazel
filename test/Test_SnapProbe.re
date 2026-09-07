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

/* nested-growth trajectory, step 2: are module member functions edges? */
let nested_prog = "module Geo = {\n  type P = (Int, Int);\n  let origin : P = (0, 0);\n  let add(a: P, b: P): P =\n    let (ax, ay) = a in\n    let (bx, by) = b in\n    (ax + bx, ay + by)\n} in\n?";
let canvas_edges_of =
    (comp: bool, code: string): (list(string), list(string)) =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => fail("parse failed")
  | Some(z) =>
    let z = Move.to_end(z);
    let st =
      comp
        ? CachedStatics.init_compositional(
            ~settings=Language.CoreSettings.on,
            ~stitch=x => x,
            ~root=Exp,
            z,
          )
        : CachedStatics.init(
            ~settings=Language.CoreSettings.on,
            ~is_dynamic_term=false,
            ~stitch=x => x,
            ~root=Exp,
            z,
          );
    let g = Web.CanvasGraph.extract(st);
    List.iter(
      (e: Web.CanvasGraph.edge) =>
        print_endline(
          (comp ? "comp " : "mono ")
          ++ e.e_name
          ++ " : "
          ++ e.e_ty
          ++ "  src="
          ++ e.e_src
          ++ " dst="
          ++ e.dst,
        ),
      g.edges,
    );
    (
      List.map((e: Web.CanvasGraph.edge) => e.e_name, g.edges),
      List.map((v: Web.CanvasGraph.value) => v.v_name, g.values),
    );
  };

/* the replay's path: statics of the program BEFORE the member insert, then
   the agent's insert_after, then the (incremental) statics after */
let incremental_member_edges = () => {
  let prog1 = "module Geo = {\n  type P = (Int, Int);\n  let origin : P = (0, 0)\n} in\n?";
  let z1 = Test_AgentTools.mk_zipper(prog1);
  let st1 =
    CachedStatics.init_compositional(
      ~settings=Language.CoreSettings.on,
      ~stitch=x => x,
      ~root=Exp,
      z1,
    );
  let g1 = Web.CanvasGraph.extract(st1);
  print_endline(
    "before: edges "
    ++ String.concat(
         ",",
         List.map((e: Web.CanvasGraph.edge) => e.e_name, g1.edges),
       )
    ++ " values "
    ++ String.concat(
         ",",
         List.map((v: Web.CanvasGraph.value) => v.v_name, g1.values),
       ),
  );
  switch (
    Test_AgentTools.run_agent_action(
      prog1,
      Insert(
        After,
        "Geo/origin",
        "let add(a: P, b: P): P =\n  let (ax, ay) = a in\n  let (bx, by) = b in\n  (ax + bx, ay + by)",
      ),
    )
  ) {
  | Error(e) => fail("insert failed: " ++ Action.Failure.show(e))
  | Ok(z2) =>
    print_endline("after program:\n" ++ Printer.of_zipper(~holes="?", z2));
    let st2 =
      CachedStatics.init_compositional(
        ~settings=Language.CoreSettings.on,
        ~stitch=x => x,
        ~root=Exp,
        z2,
      );
    let g2 = Web.CanvasGraph.extract(st2);
    let edges = List.map((e: Web.CanvasGraph.edge) => e.e_name, g2.edges);
    let values = List.map((v: Web.CanvasGraph.value) => v.v_name, g2.values);
    print_endline(
      "after (incremental): edges "
      ++ String.concat(",", edges)
      ++ " values "
      ++ String.concat(",", values),
    );
    /* and fresh, for comparison */
    let z3 = Test_AgentTools.mk_zipper(Printer.of_zipper(~holes="?", z2));
    let st3 =
      CachedStatics.init_compositional(
        ~settings=Language.CoreSettings.on,
        ~stitch=x => x,
        ~root=Exp,
        z3,
      );
    let g3 = Web.CanvasGraph.extract(st3);
    print_endline(
      "after (fresh): edges "
      ++ String.concat(
           ",",
           List.map((e: Web.CanvasGraph.edge) => e.e_name, g3.edges),
         ),
    );
    check(
      bool,
      "add is an edge after the incremental insert",
      true,
      List.mem("Geo.add", edges),
    );
  };
};

let tests = [
  test_case(
    "incremental member insert: the new function is an edge",
    `Quick,
    incremental_member_edges,
  ),
  test_case(
    "nested module members are edges (mono vs compositional)",
    `Quick,
    () => {
      let (em, vm) = canvas_edges_of(false, nested_prog);
      let (ec, vc) = canvas_edges_of(true, nested_prog);
      print_endline(
        "mono edges: "
        ++ String.concat(",", em)
        ++ " values: "
        ++ String.concat(",", vm),
      );
      print_endline(
        "comp edges: "
        ++ String.concat(",", ec)
        ++ " values: "
        ++ String.concat(",", vc),
      );
      check(list(string), "edges agree", em, ec);
      check(list(string), "values agree", vm, vc);
      check(bool, "add is an edge", true, List.mem("Geo.add", em));
    },
  ),
  test_case(
    "nested local aliases fold bottom-up (Room = (Int, Int, [Tile]))",
    `Quick,
    () => {
      let prog = "module D = {\n  type Tile =\n    + Floor\n    + Wall;\n  type Room = (Int, Int, [Tile]);\n  let width(r: Room): Int =\n    let (w, _, _) = r in\n    w;\n  let nth(ts: [Tile], i: Int): Tile =\n    case ts\n    | [] => Wall\n    | hd :: tl => hd\n    end\n} in\n?";
      switch (Parser.to_zipper(~root=Exp, prog)) {
      | None => fail("parse failed")
      | Some(z) =>
        let st =
          CachedStatics.init_compositional(
            ~settings=Language.CoreSettings.on,
            ~stitch=x => x,
            ~root=Exp,
            Move.to_end(z),
          );
        let g = Web.CanvasGraph.extract(st);
        let edge = n =>
          List.find_opt((e: Web.CanvasGraph.edge) => e.e_name == n, g.edges);
        switch (edge("D.width"), edge("D.nth")) {
        | (Some(w), Some(nth)) =>
          check(string, "width's domain is Room", "D.Room", w.e_src);
          let src =
            List.find_opt(
              (n: Web.CanvasGraph.tynode) => n.key == nth.e_src,
              g.nodes,
            );
          switch (src) {
          | Some(n) =>
            check(
              bool,
              "nth's first part folds to [Tile]",
              true,
              switch (n.parts) {
              | ["[Tile]", ..._] => true
              | _ => false
              },
            )
          | None => fail("no product for nth")
          };
          check(string, "nth's codomain is Tile", "D.Tile", nth.dst);
        | _ => fail("edges missing")
        };
        /* no inlined-body glyph survives as a node */
        let keys = List.map((n: Web.CanvasGraph.tynode) => n.key, g.nodes);
        check(
          bool,
          "no '(Int, Int, [Floor…' glyph",
          false,
          List.exists(
            k =>
              String.length(k) > 12 && String.sub(k, 0, 12) == "(Int, Int, [",
            keys,
          ),
        );
      };
    },
  ),
  test_case(
    "a member typed with a local alias attaches to the alias", `Quick, () =>
    switch (Parser.to_zipper(~root=Exp, nested_prog)) {
    | None => fail("parse failed")
    | Some(z) =>
      let st =
        CachedStatics.init_compositional(
          ~settings=Language.CoreSettings.on,
          ~stitch=x => x,
          ~root=Exp,
          Move.to_end(z),
        );
      let g = Web.CanvasGraph.extract(st);
      switch (
        List.find_opt(
          (e: Web.CanvasGraph.edge) => e.e_name == "Geo.add",
          g.edges,
        )
      ) {
      | None => fail("no Geo.add edge")
      | Some(e) =>
        check(string, "codomain is the alias", "Geo.P", e.dst);
        let src =
          List.find_opt(
            (n: Web.CanvasGraph.tynode) => n.key == e.e_src,
            g.nodes,
          );
        switch (src) {
        | Some(n) =>
          check(list(string), "domain parts", ["Geo.P", "Geo.P"], n.parts)
        | None => fail("no source node")
        };
      };
    }
  ),
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
