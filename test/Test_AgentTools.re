open Alcotest;
open Haz3lcore;
open Language;
open Action;
open Util;

let mk_zipper = (code: string): Zipper.t => {
  switch (Parser.to_zipper(code)) {
  | Some(z) => z
  | None => Alcotest.fail("Failed to parse: " ++ code)
  };
};

let mk_statics = (z: Zipper.t): StaticsBase.Map.t =>
  Statics.mk(
    CoreSettings.on,
    Builtins.ctx_init(Some(Operators.default_mode)),
    MakeTerm.from_zip_for_sem(z).term,
  );

let render_zipper = (z: Zipper.t): string =>
  Printer.of_zipper(~holes="?", ~indent=" ", z);

let run_agent_action = (code: string, a: Action.Structural.t) => {
  let z = mk_zipper(code);
  Perform.go(
    ~statics=CachedStatics.empty,
    ~syntax=CachedSyntax.init(z),
    Structural(a),
    {
      zipper: z,
      col_target: None,
    },
  );
};

/* Initialize bypasses Perform.go since it's no longer a Structural action.
   Test it directly using the same logic as Agent.re's Initialize handler. */
let run_initialize = (code: string, new_code: string) => {
  let z = mk_zipper(code);
  let info_map = mk_statics(z);
  switch (HighLevelNodeMap.build(z, info_map)) {
  | Some(_) =>
    Error(
      Action.Failure.Composition_action_failure(
        "Once a program has let/type alias expressions, you can never use initialize on it ever again.",
      ),
    )
  | None =>
    let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
      Result.of_option(~error, z);
    switch (
      CompositionGo.Local.PerformUtils.introduce(
        Select.all(z),
        new_code,
        return,
      )
    ) {
    | Error(e) => Error(e)
    | Ok(new_z) =>
      let new_statics = mk_statics(new_z);
      let new_errors = ErrorPrint.all(new_statics);
      if (List.length(new_errors) > 0) {
        Error(
          Action.Failure.Composition_action_failure(
            "Static errors: " ++ String.concat(", ", new_errors),
          ),
        );
      } else {
        Ok(Dump.to_zipper(new_z));
      };
    };
  };
};

let check_rendered = (name: string, expected: string, actual: string) => {
  let normalized = s =>
    s
    |> StringUtil.trim_trailing_whitespace
    |> StringUtil.replace(StringUtil.regexp("[\\s]+"), _, " ")
    |> String.trim;
  check(
    testable(Fmt.string, (a, b) =>
      String.equal(normalized(a), normalized(b))
    ),
    name,
    expected,
    actual,
  );
};

let apply_and_render = (code: string, a: Action.Structural.t): string => {
  switch (run_agent_action(code, a)) {
  | Ok(z) => render_zipper(z)
  | Error(err) =>
    Alcotest.fail(
      "Agent action failed: "
      ++ Action.Failure.show(err)
      ++ "\nCode: "
      ++ code,
    )
  };
};

let expect_composition_failure =
    (code: string, a: Action.Structural.t, name: string) => {
  switch (run_agent_action(code, a)) {
  | Ok(_) => Alcotest.fail("Expected failure: " ++ name)
  | Error(Action.Failure.Composition_action_failure(_)) => ()
  | Error(err) =>
    Alcotest.fail(
      "Unexpected failure kind for "
      ++ name
      ++ ": "
      ++ Action.Failure.show(err),
    )
  };
};


let edit_action_tests = (
  "AgentTools.EditActions",
  [
    test_case("initialize replaces program", `Quick, () => {
      switch (run_initialize("?", "let a = 3 in a")) {
      | Ok(z) =>
        check_rendered("initialize", "let a = 3 in a", render_zipper(z))
      | Error(err) =>
        Alcotest.fail("Initialize failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case("initialize rejected on let program", `Quick, () => {
      switch (run_initialize("let a = 1 in a", "let b = 2 in b")) {
      | Ok(_) => Alcotest.fail("Expected failure: initialize on let")
      | Error(Action.Failure.Composition_action_failure(_)) => ()
      | Error(err) =>
        Alcotest.fail(
          "Unexpected failure kind: " ++ Action.Failure.show(err),
        )
      }
    }),
    test_case(
      "update_definition replaces def",
      `Quick,
      () => {
        let result =
          apply_and_render("let a = 1 in a", Update(Definition, "a", "2"));
        check_rendered("update_definition", "let a = 2 in a", result);
      },
    ),
    test_case(
      "update_body replaces body",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Update(Body, "b", "b + 1"),
          );
        check_rendered(
          "update_body",
          "let a = 1 in let b = 2 in b + 1",
          result,
        );
      },
    ),
    test_case(
      "update_pattern renames uses",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a + 1 in b + a",
            Update(Pattern, "a", "x"),
          );
        check_rendered(
          "update_pattern",
          "let x = 1 in let b = x + 1 in b + x",
          result,
        );
      },
    ),
    test_case(
      "update_pattern renames in let def",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x = 3 in let y = x in y",
            Update(Pattern, "x", "z"),
          );
        check_rendered(
          "update_pattern_let_def",
          "let z = 3 in let y = z in y",
          result,
        );
      },
    ),
    test_case(
      "update_binding_clause replaces binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Update(BindingClause, "b", "let b = a + 2 in"),
          );
        check_rendered(
          "update_binding_clause",
          "let a = 1 in let b = a + 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert_before adds binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(Before, "b", "let x = a in"),
          );
        check_rendered(
          "insert_before",
          "let a = 1 in let x = a in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert_after adds binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(After, "a", "let x = a in"),
          );
        check_rendered(
          "insert_after",
          "let a = 1 in let x = a in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "delete_binding_clause removes binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + c",
            Delete(BindingClause, "b"),
          );
        check_rendered(
          "delete_binding_clause",
          "let a = 1 in let c = 3 in a + c",
          result,
        );
      },
    ),
    test_case(
      "delete_body replaces body with hole",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Delete(Body, "b"),
          );
        check_rendered("delete_body", "let a = 1 in let b = 2 in ?", result);
      },
    ),
  ],
);

let build_node_map = (code: string) => {
  let z = mk_zipper(code);
  let info_map = mk_statics(z);
  switch (HighLevelNodeMap.build(z, info_map)) {
  | Some(node_map) => node_map
  | None => Alcotest.fail("Failed to build HighLevelNodeMap")
  };
};

let name_list = (nodes: list(HighLevelNodeMap.node)) =>
  List.map((n: HighLevelNodeMap.node) => n.name, nodes);

let high_level_node_map_tests = (
  "HighLevelNodeMap",
  [
    test_case(
      "top-level nodes from let chain",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in a + b + c");
        let top_level_ids = HighLevelNodeMap.gather_top_level(node_map);
        let top_level_names =
          List.map(
            (id: Id.t) => HighLevelNodeMap.id_to_name(node_map, id),
            top_level_ids,
          )
          |> List.sort(String.compare);
        check(
          list(string),
          "top-level names",
          ["a", "b", "c"],
          top_level_names,
        );
      },
    ),
    test_case(
      "path_to_id resolves simple names",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in a + b + c");
        let id_b = HighLevelNodeMap.path_to_id(node_map, "b");
        check(
          string,
          "path_to_id",
          "b",
          HighLevelNodeMap.id_to_name(node_map, id_b),
        );
      },
    ),
    test_case(
      "children reflect nested lets in def",
      `Quick,
      () => {
        let node_map = build_node_map("let a = let b = 1 in b in a");
        let node_a = HighLevelNodeMap.path_to_node(node_map, "a");
        let child_names =
          HighLevelNodeMap.children_of(node_map, node_a) |> name_list;
        check(list(string), "children names", ["b"], child_names);
      },
    ),
    test_case(
      "siblings include siblings at top level",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in c");
        let node_b = HighLevelNodeMap.path_to_node(node_map, "b");
        let sibling_names =
          HighLevelNodeMap.siblings_of(node_map, node_b) |> name_list;
        check(
          list(string),
          "siblings names",
          ["a", "b", "c"],
          sibling_names,
        );
      },
    ),
  ],
);

let composition_view_print_tests = (
  "CompositionView.print_zipper",
  [
    test_case("hole renders as question mark, not absence", `Quick, () =>
      check_rendered(
        "hole as question mark",
        "let a = 1 in ?",
        "let a = 1 in " |> mk_zipper |> CompositionView.Public.print_zipper,
      )
    ),
    test_case("fold renders as folded, not as invocation", `Quick, () =>
      check_rendered(
        "fold as ellipsis",
        {|let a = ⋱ in a|},
        "let a = ^^fold(1) in a"
        |> mk_zipper
        |> CompositionView.Public.print_zipper,
      )
    ),
  ],
);

let module_node_map_tests = (
  "HighLevelNodeMap.Modules",
  [
    test_case(
      "module items appear as children of parent let",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let m = { let a = 1; let b = 2 } in m.a + m.b",
          );
        let node_m = HighLevelNodeMap.path_to_node(node_map, "m");
        let child_names =
          HighLevelNodeMap.children_of(node_map, node_m) |> name_list;
        check(list(string), "module children", ["a", "b"], child_names);
      },
    ),
    test_case(
      "module items with type alias",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let m = { let a = 1; type T = Int; let b : T = 2 } in m.a + m.b",
          );
        let node_m = HighLevelNodeMap.path_to_node(node_map, "m");
        let child_names =
          HighLevelNodeMap.children_of(node_map, node_m) |> name_list;
        check(
          list(string),
          "module children with type",
          ["a", "T", "b"],
          child_names,
        );
      },
    ),
    test_case(
      "module item path resolution",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let m = { let a = 1; let b = 2 } in m.a + m.b",
          );
        /* Path m/a should resolve to the module item "a" */
        let id_a = HighLevelNodeMap.path_to_id(node_map, "m/a");
        check(
          string,
          "path m/a resolves",
          "a",
          HighLevelNodeMap.id_to_name(node_map, id_a),
        );
      },
    ),
    test_case(
      "module items are siblings of each other",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let m = { let a = 1; let b = 2; let c = 3 } in m.a",
          );
        let node_b = HighLevelNodeMap.path_to_node(node_map, "m/b");
        let sibling_names =
          HighLevelNodeMap.siblings_of(node_map, node_b) |> name_list;
        check(
          list(string),
          "module item siblings",
          ["a", "b", "c"],
          sibling_names,
        );
      },
    ),
  ],
);

let path_extension_tests = (
  "HighLevelNodeMap.PathExtensions",
  [
    test_case(
      "#n resolves to n-th top-level binding",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in a + b + c");
        let id_by_index = HighLevelNodeMap.path_to_id(node_map, "#1");
        check(
          string,
          "#1 = b",
          "b",
          HighLevelNodeMap.id_to_name(node_map, id_by_index),
        );
      },
    ),
    test_case(
      "#0 resolves to first binding",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in a + b + c");
        let id_first = HighLevelNodeMap.path_to_id(node_map, "#0");
        check(
          string,
          "#0 = a",
          "a",
          HighLevelNodeMap.id_to_name(node_map, id_first),
        );
      },
    ),
    test_case(
      "$ resolves to last binding",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in a + b + c");
        let id_last = HighLevelNodeMap.path_to_id(node_map, "$");
        check(
          string,
          "$ = c",
          "c",
          HighLevelNodeMap.id_to_name(node_map, id_last),
        );
      },
    ),
    test_case(
      "#n works for module children",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let m = { let a = 1; let b = 2; let c = 3 } in m.a",
          );
        let id_by_index = HighLevelNodeMap.path_to_id(node_map, "m/#1");
        check(
          string,
          "m/#1 = b",
          "b",
          HighLevelNodeMap.id_to_name(node_map, id_by_index),
        );
      },
    ),
    test_case(
      "$ works for module children",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let m = { let a = 1; let b = 2; let c = 3 } in m.a",
          );
        let id_last = HighLevelNodeMap.path_to_id(node_map, "m/$");
        check(
          string,
          "m/$ = c",
          "c",
          HighLevelNodeMap.id_to_name(node_map, id_last),
        );
      },
    ),
    test_case(
      "mixed name and index path",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let m = { let a = 1; let b = 2 } in m.a",
          );
        /* #0 resolves to m (first top-level), then #1 resolves to b (second child) */
        let id = HighLevelNodeMap.path_to_id(node_map, "#0/#1");
        check(
          string,
          "#0/#1 = b",
          "b",
          HighLevelNodeMap.id_to_name(node_map, id),
        );
      },
    ),
    test_case(
      "update via index path",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Update(Definition, "#1", "42"),
          );
        check_rendered("update via #1", "let a = 1 in let b = 42 in a + b", result);
      },
    ),
  ],
);

let module_edit_action_tests = (
  "AgentTools.ModuleEditActions",
  [
    test_case(
      "update definition in module item",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let a = 1; let b = 2 } in m.a + m.b",
            Update(Definition, "m/a", "42"),
          );
        check_rendered(
          "update module item def",
          "let m = { let a = 42; let b = 2 } in m.a + m.b",
          result,
        );
      },
    ),
    test_case(
      "update definition of type alias in module",
      `Quick,
      () => {
        /* Use a program where changing T doesn't create a type error
           on other items (no `: T` annotation that depends on T). */
        let result =
          apply_and_render(
            "let m = { type T = Int; let a = 1 } in m.a",
            Update(Definition, "m/T", "Bool"),
          );
        check_rendered(
          "update module type def",
          "let m = { type T = Bool; let a = 1 } in m.a",
          result,
        );
      },
    ),
    test_case(
      "update pattern in module item renames",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let a = 1; let b = m.a + 1 } in m.b",
            Update(Pattern, "m/a", "x"),
          );
        /* Pattern update renames the binding within the module.
           Note: use sites via m.a are NOT automatically renamed since
           they use dot-access, not direct variable references. */
        check_rendered(
          "update module item pattern",
          "let m = { let x = 1; let b = m.a + 1 } in m.b",
          result,
        );
      },
    ),
    /* Module edit operations use term-level transformations (TermEdit)
       which manipulate the Module(items) list directly, avoiding the
       sort-context mismatch that occurs with segment-level operations. */
    test_case(
      "delete module item cleanly",
      `Quick,
      () => {
        /* Term-level delete removes the item from the Module(items) list
           and round-trips back to a zipper — no hole left behind. */
        let result =
          apply_and_render(
            "let m = { let a = 1; let b = 2; let c = 3 } in m.a + m.c",
            Delete(BindingClause, "m/b"),
          );
        check_rendered(
          "delete module item",
          "let m = { let a = 1; let c = 3 } in m.a + m.c",
          result,
        );
      },
    ),
    test_case(
      "insert module item before",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let a = 1; let c = 3 } in m.a + m.c",
            Insert(Before, "m/c", "let b = 2"),
          );
        check_rendered(
          "insert module item before",
          "let m = { let a = 1; let b = 2; let c = 3 } in m.a + m.c",
          result,
        );
      },
    ),
    test_case(
      "insert module item after",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let a = 1; let c = 3 } in m.a + m.c",
            Insert(After, "m/a", "let b = 2"),
          );
        check_rendered(
          "insert module item after",
          "let m = { let a = 1; let b = 2; let c = 3 } in m.a + m.c",
          result,
        );
      },
    ),
    test_case(
      "update module item binding clause",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let a = 1; let b = 2; let c = 3 } in m.a + m.c",
            Update(BindingClause, "m/b", "let x = 42"),
          );
        check_rendered(
          "update module item binding clause",
          "let m = { let a = 1; let x = 42; let c = 3 } in m.a + m.c",
          result,
        );
      },
    ),
  ],
);

let edge_case_tests = (
  "AgentTools.EdgeCases",
  [
    test_case(
      "update def in nested let",
      `Quick,
      () => {
        /* a/inner targets inner inside a's definition */
        let result =
          apply_and_render(
            "let a = let inner = 1 in inner in a",
            Update(Definition, "a/inner", "42"),
          );
        check_rendered(
          "nested def update",
          "let a = let inner = 42 in inner in a",
          result,
        );
      },
    ),
    test_case(
      "update def with type alias",
      `Quick,
      () => {
        /* Use a program where x doesn't depend on T's definition
           to avoid type errors when changing T from Int to Bool. */
        let result =
          apply_and_render(
            "type T = Int in let x = 1 in x",
            Update(Definition, "T", "Bool"),
          );
        check_rendered(
          "type alias def update",
          "type T = Bool in let x = 1 in x",
          result,
        );
      },
    ),
    test_case(
      "bad path gives helpful error",
      `Quick,
      () => {
        expect_composition_failure(
          "let a = 1 in let b = 2 in a + b",
          Update(Definition, "nonexistent", "42"),
          "bad path error",
        );
      },
    ),
    test_case(
      "index out of range gives error",
      `Quick,
      () => {
        expect_composition_failure(
          "let a = 1 in let b = 2 in a + b",
          Update(Definition, "#99", "42"),
          "index out of range",
        );
      },
    ),
    test_case(
      "update def preserves surrounding structure",
      `Quick,
      () => {
        /* Updating b's def shouldn't affect a or c */
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Update(Definition, "b", "a * 10"),
          );
        check_rendered(
          "preserve surrounding",
          "let a = 1 in let b = a * 10 in let c = 3 in a + b + c",
          result,
        );
      },
    ),
    test_case(
      "update introduces static error is rejected",
      `Quick,
      () => {
        /* Changing def to wrong type should be caught */
        expect_composition_failure(
          "let a : Int = 1 in a + 1",
          Update(Definition, "a", "true"),
          "type error rejection",
        );
      },
    ),
    test_case(
      "unmatched delimiter gives parse error",
      `Quick,
      () => {
        /* Code with unmatched 'if' delimiter should be caught as parse error */
        expect_composition_failure(
          "let a = 1 in a",
          Update(Definition, "a", "if true then 1"),
          "parse error detection",
        );
      },
    ),
    test_case(
      "invalid token in code gives parse error",
      `Quick,
      () => {
        /* Inserting code that produces an Invalid token should be caught */
        expect_composition_failure(
          "let a = 1 in let b = 2 in a + b",
          Insert(After, "a", "let c = $invalid"),
          "invalid token parse error",
        );
      },
    ),
    test_case(
      "$ path works with edit action",
      `Quick,
      () => {
        /* $ targets the last binding */
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Update(Definition, "$", "0"),
          );
        check_rendered(
          "$ edit",
          "let a = 1 in let b = 2 in let c = 0 in a + b + c",
          result,
        );
      },
    ),
    test_case(
      "#n matches name-based path",
      `Quick,
      () => {
        /* #0 and name "a" should resolve to the same node */
        let node_map =
          build_node_map("let a = 1 in let b = 2 in a + b");
        let id_by_name = HighLevelNodeMap.path_to_id(node_map, "a");
        let id_by_index = HighLevelNodeMap.path_to_id(node_map, "#0");
        check(
          bool,
          "#0 == a",
          true,
          Id.equal(id_by_name, id_by_index),
        );
      },
    ),
    test_case(
      "deeply nested module path",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let outer = let m = { let a = 1; let b = 2 } in m.a in outer",
          );
        let id = HighLevelNodeMap.path_to_id(node_map, "outer/m/a");
        check(
          string,
          "deep module path",
          "a",
          HighLevelNodeMap.id_to_name(node_map, id),
        );
      },
    ),
  ],
);

let string_contains = (needle: string, haystack: string): bool => {
  let needle_len = String.length(needle);
  let haystack_len = String.length(haystack);
  if (needle_len > haystack_len) {
    false;
  } else {
    let found = ref(false);
    for (i in 0 to haystack_len - needle_len) {
      if (String.sub(haystack, i, needle_len) == needle) {
        found := true;
      };
    };
    found^;
  };
};

let run_read_action =
    (code: string, action: CompositionActions.read_action): string => {
  let z = mk_zipper(code);
  switch (CompositionGo.Public.read_dispatch(~action, ~z)) {
  | Ok(content) => content
  | Error(Composition_action_failure(msg)) =>
    Alcotest.fail("Read action failed: " ++ msg)
  | Error(_) => Alcotest.fail("Read action failed with unknown error")
  };
};

let read_action_tests = (
  "AgentTools.ReadActions",
  [
    test_case(
      "get_syntax returns binding clause",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let a = 1 + 2 in let b = 3 in a + b",
            GetSyntax("a"),
          );
        /* Should contain the let binding for a */
        check(bool, "contains let a", true, string_contains("let a", result));
        check(
          bool,
          "contains 1 + 2",
          true,
          string_contains("1 + 2", result),
        );
      },
    ),
    test_case(
      "get_syntax returns nested binding",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let a = let inner = 42 in inner in a",
            GetSyntax("a/inner"),
          );
        check(
          bool,
          "contains inner",
          true,
          string_contains("inner", result),
        );
        check(bool, "contains 42", true, string_contains("42", result));
      },
    ),
    test_case(
      "get_syntax for module child",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let m = { let x = 5; let y = 10 } in m.x",
            GetSyntax("m/x"),
          );
        check(bool, "contains x", true, string_contains("x", result));
        check(bool, "contains 5", true, string_contains("5", result));
      },
    ),
    test_case(
      "get_statics shows types for annotated binding",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let x : Int = 1 + 2 in x",
            GetStatics("x"),
          );
        check(bool, "contains Int", true, string_contains("Int", result));
        check(
          bool,
          "contains path",
          true,
          string_contains("Path: x", result),
        );
      },
    ),
    test_case(
      "get_statics shows error for inconsistent types",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let x : Int = true in x",
            GetStatics("x"),
          );
        /* Should report a type error in the subtree */
        check(
          bool,
          "contains error",
          true,
          string_contains("error", String.lowercase_ascii(result))
          || string_contains("Error", result)
          || string_contains("inconsistent", String.lowercase_ascii(result)),
        );
      },
    ),
    test_case(
      "get_context shows variables in scope",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let x : Int = 5 in let y = x + 1 in y",
            GetContext("y"),
          );
        /* y's context should include x */
        check(
          bool,
          "contains x in context",
          true,
          string_contains("x", result),
        );
        check(
          bool,
          "contains Variables section",
          true,
          string_contains("Variables", result),
        );
      },
    ),
    test_case(
      "get_context shows type aliases",
      `Quick,
      () => {
        let result =
          run_read_action(
            "type Color = Red + Green + Blue in let x : Color = Red in x",
            GetContext("x"),
          );
        check(
          bool,
          "contains Color type alias",
          true,
          string_contains("Color", result),
        );
      },
    ),
    test_case(
      "get_context shows constructors",
      `Quick,
      () => {
        let result =
          run_read_action(
            "type Color = Red + Green + Blue in let x : Color = Red in x",
            GetContext("x"),
          );
        check(
          bool,
          "contains constructors section",
          true,
          string_contains("Constructors", result),
        );
        check(
          bool,
          "contains Red constructor",
          true,
          string_contains("Red", result),
        );
      },
    ),
    test_case(
      "get_statics for type alias binding",
      `Quick,
      () => {
        let result =
          run_read_action(
            "type T = Int in let x : T = 1 in x",
            GetStatics("T"),
          );
        check(
          bool,
          "contains binding name T",
          true,
          string_contains("T", result),
        );
        /* Type alias should not have errors in its subtree */
        check(
          bool,
          "no error in result",
          false,
          string_contains("error", String.lowercase_ascii(result))
          && string_contains("inconsistent", String.lowercase_ascii(result)),
        );
      },
    ),
    test_case(
      "get_syntax with index path",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            GetSyntax("#1"),
          );
        check(bool, "contains b", true, string_contains("b", result));
        check(bool, "contains 2", true, string_contains("2", result));
      },
    ),
  ],
);

let type_annotation_tests = (
  "AgentTools.TypeAnnotation",
  [
    test_case(
      "update type annotation on let binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 5 in x + 1",
            Update(TypeAnnotation, "x", "Float"),
          );
        check_rendered(
          "type annotation updated",
          "let x : Float = 5 in x + 1",
          result,
        );
      },
    ),
    test_case(
      "update type annotation preserves definition and body",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a : Int = 1 in let b : Bool = true in a",
            Update(TypeAnnotation, "a", "Float"),
          );
        check_rendered(
          "preserves other bindings",
          "let a : Float = 1 in let b : Bool = true in a",
          result,
        );
      },
    ),
    test_case(
      "update type annotation on nested binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let inner : Int = 1 in inner in a",
            Update(TypeAnnotation, "a/inner", "Float"),
          );
        check_rendered(
          "nested type annotation",
          "let a = let inner : Float = 1 in inner in a",
          result,
        );
      },
    ),
    test_case(
      "update type annotation fails on unannotated binding",
      `Quick,
      () => {
        expect_composition_failure(
          "let x = 5 in x + 1",
          Update(TypeAnnotation, "x", "Int"),
          "no annotation error",
        );
      },
    ),
    test_case(
      "update type alias definition via TypeAnnotation",
      `Quick,
      () => {
        /* For type aliases, TypeAnnotation targets the type definition itself.
           Use a program where x doesn't depend on T to avoid type errors. */
        let result =
          apply_and_render(
            "type T = Int in let x = 1 in x",
            Update(TypeAnnotation, "T", "Bool"),
          );
        check_rendered(
          "type alias via TypeAnnotation",
          "type T = Bool in let x = 1 in x",
          result,
        );
      },
    ),
  ],
);

let seq_node_map_tests = (
  "HighLevelNodeMap.SeqLines",
  [
    test_case(
      "test expression after let is a sibling node",
      `Quick,
      () => {
        let node_map =
          build_node_map("let x = 1 in test x == 1 end; x");
        let top_names =
          HighLevelNodeMap.gather_top_level(node_map)
          |> List.map((id: Id.t) =>
               HighLevelNodeMap.id_to_name(node_map, id)
             );
        /* gather_top_level returns IDs in source order */
        check(
          list(string),
          "top level includes test",
          ["x", "{test}", "{expr}"],
          top_names,
        );
      },
    ),
    test_case(
      "test expression addressable by index",
      `Quick,
      () => {
        let node_map =
          build_node_map("let x = 1 in test x == 1 end; x");
        /* x is #0, the test is #1, trailing x is #2 */
        let id = HighLevelNodeMap.path_to_id(node_map, "#1");
        check(
          string,
          "#1 resolves to test",
          "{test}",
          HighLevelNodeMap.id_to_name(node_map, id),
        );
      },
    ),
    test_case(
      "multiple tests in sequence",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let x = 1 in test x == 1 end; test x > 0 end; x",
          );
        let top_names =
          HighLevelNodeMap.gather_top_level(node_map)
          |> List.map((id: Id.t) =>
               HighLevelNodeMap.id_to_name(node_map, id)
             );
        check(
          list(string),
          "two tests and trailing expr",
          ["x", "{test}", "{test}", "{expr}"],
          top_names,
        );
      },
    ),
    test_case(
      "test between two lets",
      `Quick,
      () => {
        let node_map =
          build_node_map(
            "let x = 1 in test x == 1 end; let y = x + 1 in y",
          );
        let node_x = HighLevelNodeMap.path_to_node(node_map, "x");
        let sibling_names =
          HighLevelNodeMap.siblings_of(node_map, node_x) |> name_list;
        check(
          list(string),
          "siblings",
          ["x", "{test}", "y"],
          sibling_names,
        );
      },
    ),
    test_case(
      "$ resolves to trailing expression",
      `Quick,
      () => {
        let node_map =
          build_node_map("let x = 1 in test x == 1 end; x");
        let id = HighLevelNodeMap.path_to_id(node_map, "$");
        check(
          string,
          "$ resolves to trailing expr",
          "{expr}",
          HighLevelNodeMap.id_to_name(node_map, id),
        );
      },
    ),
    test_case(
      "update_binding_clause on test expression",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x = 1 in test x == 1 end; x",
            Update(BindingClause, "#1", "test x > 0 end"),
          );
        check_rendered(
          "update test via #1",
          "let x = 1 in test x > 0 end; x",
          result,
        );
      },
    ),
  ],
);

/* === Selector Language Tests === */

let mk_term = (code: string): Exp.t => {
  let z = mk_zipper(code);
  MakeTerm.from_zip_for_sem(z).term;
};

let selector_query = (code: string, sel: string): list(string) => {
  let term = mk_term(code);
  let matches = Selector.query(sel, term);
  List.map(Selector.print_match, matches);
};

let selector_query_unique = (code: string, sel: string): string => {
  let term = mk_term(code);
  switch (Selector.query_unique(sel, term)) {
  | Ok(m) => Selector.print_match(m)
  | Error(e) => "ERROR: " ++ e
  };
};

let selector_tests = (
  "AgentTools.Selectors",
  [
    /* Basic let spine selectors */
    test_case(
      "let x = * selects definition",
      `Quick,
      () => {
        let result = selector_query_unique("let x = 42 in x", "let x = *");
        check(string, "def", "42", result);
      },
    ),
    test_case(
      "let x ... in * selects body",
      `Quick,
      () => {
        let result =
          selector_query_unique("let x = 42 in x + 1", "let x _... in *");
        check(string, "body", "x + 1", result);
      },
    ),
    test_case(
      "let with nested lets selects correct one",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "let a = 1 in let b = 2 in a + b",
            "let b = *",
          );
        check(string, "def of b", "2", result);
      },
    ),
    /* Binder chain */
    test_case(
      "binder chain A/B navigates nested defs",
      `Quick,
      () => {
        let code = "let m = { let x = 42 } in m.x";
        let result = selector_query_unique(code, "m/x = *");
        check(string, "chain def", "42", result);
      },
    ),
    /* If spine */
    test_case(
      "if * selects condition",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "if true then 1 else 0",
            "if *",
          );
        check(string, "cond", "true", result);
      },
    ),
    test_case(
      "if _ then * selects then branch",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "if true then 1 else 0",
            "if _ then *",
          );
        check(string, "then", "1", result);
      },
    ),
    test_case(
      "if _... else * selects else branch",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "if true then 1 else 0",
            "if _... else *",
          );
        check(string, "else", "0", result);
      },
    ),
    /* Descendant search */
    test_case(
      "descendant search finds nested if",
      `Quick,
      () => {
        let code = "let f = fun x -> if x > 0 then x else 0 in f 5";
        let result = selector_query_unique(code, "let f = \\_ if _ then *");
        check(string, "then branch", "x", result);
      },
    ),
    /* Case/match spine */
    test_case(
      "case * selects scrutinee",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "case x | A => 1 | B => 2 end",
            "case *",
          );
        check(string, "scrutinee", "x", result);
      },
    ),
    test_case(
      "| Arm => * selects arm body",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "case x | A => 1 | B => 2 end",
            "| B => *",
          );
        check(string, "arm B body", "2", result);
      },
    ),
    /* No match */
    test_case(
      "no match returns error",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "let x = 1 in x",
            "let y = *",
          );
        check(
          bool,
          "starts with ERROR",
          true,
          String.length(result) > 5
          && String.sub(result, 0, 5) == "ERROR",
        );
      },
    ),
    /* Multiple matches for query mode */
    test_case(
      "query returns multiple matches",
      `Quick,
      () => {
        let code = "let a = 1 in let b = 2 in a + b";
        let results = selector_query(code, "let _ = *");
        check(int, "match count", 2, List.length(results));
      },
    ),
    /* Read action integration tests */
    test_case(
      "Select read action returns focused syntax",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let x = 42 in x + 1",
            Select("let x = *"),
          );
        check(string, "select def", "42", result);
      },
    ),
    test_case(
      "Select with descendant search via read action",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let f = fun x -> if x > 0 then x else 0 in f 5",
            Select("let f = \\_ if _... else *"),
          );
        check(string, "select else", "0", result);
      },
    ),
    test_case(
      "Select with binder chain via read action",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let m = { let x = 42 } in m.x",
            Select("m/x = *"),
          );
        check(string, "chain select", "42", result);
      },
    ),
    test_case(
      "Select returns multiple matches",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let a = 1 in let b = 2 in a + b",
            Select("let _ = *"),
          );
        /* Should have 2 lines, one per match */
        let lines =
          result
          |> String.split_on_char('\n')
          |> List.filter(s => String.length(String.trim(s)) > 0);
        check(int, "line count", 2, List.length(lines));
      },
    ),
    /* Spec Example 1: if inside a function binding */
    test_case(
      "spec ex1: let f = \\_ if * gets condition",
      `Quick,
      () => {
        let code = "let f = fun x -> if x > 0 then x else 0 in f 5";
        let result = selector_query_unique(code, "let f = \\_ if *");
        check(string, "cond", "x > 0", result);
      },
    ),
    test_case(
      "spec ex1: let f = \\_ if _ then * gets then branch",
      `Quick,
      () => {
        let code = "let f = fun x -> if x > 0 then x else 0 in f 5";
        let result =
          selector_query_unique(code, "let f = \\_ if _ then *");
        check(string, "then", "x", result);
      },
    ),
    test_case(
      "spec ex1: * let f selects whole binding",
      `Quick,
      () => {
        let code = "let f = fun x -> if x > 0 then x else 0 in f 5";
        let result = selector_query_unique(code, "* let f");
        /* Should return the whole let f = ... in ... expression */
        let has_let =
          String.length(result) > 5
          && String.sub(result, 0, 5) == "ERROR";
        /* If this errors, we need to implement * before keywords */
        check(bool, "not error", false, has_let);
      },
    ),
    /* Spec Example 2: case arms */
    test_case(
      "spec ex2: case \\_ | Increment => * gets arm body",
      `Quick,
      () => {
        let code =
          "case msg | Increment => count + 1 | Decrement => count - 1 end";
        let result =
          selector_query_unique(code, "| Increment => *");
        check(string, "incr body", "count + 1", result);
      },
    ),
    test_case(
      "spec ex2: | Decrement => * gets second arm",
      `Quick,
      () => {
        let code =
          "case msg | Increment => count + 1 | Decrement => count - 1 end";
        let result =
          selector_query_unique(code, "| Decrement => *");
        check(string, "decr body", "count - 1", result);
      },
    ),
    /* Spec Example 3: module items */
    test_case(
      "spec ex3: module M = \\_ let x = * in module",
      `Quick,
      () => {
        let code = "let m = { let x = 1; let y = 2 } in m.x";
        let result =
          selector_query_unique(code, "m/x = *");
        check(string, "module member x def", "1", result);
      },
    ),
    test_case(
      "spec ex3: module member y via chain",
      `Quick,
      () => {
        let code = "let m = { let x = 1; let y = 2 } in m.y";
        let result =
          selector_query_unique(code, "m/y = *");
        check(string, "module member y def", "2", result);
      },
    ),
    /* Spec Example 4: nested binder chains */
    test_case(
      "spec ex4: A/B chain with nested modules",
      `Quick,
      () => {
        let code =
          "let a = { let x = 1; let b = { let y = 42 } } in a.b.y";
        let result =
          selector_query_unique(code, "a/b/y = *");
        check(string, "nested chain", "42", result);
      },
    ),
    /* name = * without let keyword */
    test_case(
      "name = * works without let keyword",
      `Quick,
      () => {
        let code = "let x = 42 in let y = 99 in x + y";
        let result = selector_query_unique(code, "y = *");
        check(string, "y def", "99", result);
      },
    ),
    /* body selection */
    test_case(
      "name _... in * selects body",
      `Quick,
      () => {
        let code = "let x = 42 in let y = 99 in x + y";
        let result = selector_query_unique(code, "x _... in *");
        check(string, "x body", "let y = 99 in x + y", result);
      },
    ),
  ],
);

/* === Completeness Tests === */

let expect_completeness = (code: string, expected: string) =>
  check(string, "completeness", expected, run_read_action(code, GetCompleteness));

let completeness_tests = (
  "AgentTools.Completeness",
  [
    test_case("complete program", `Quick, () =>
      expect_completeness("let x = 42 in x + 1", "Complete: no unfilled holes.")
    ),
    test_case("expression hole", `Quick, () =>
      expect_completeness(
        "let x = ? in x + 1",
        "Incomplete: 1 unfilled hole(s) (1 expression).",
      )
    ),
    test_case("type hole", `Quick, () =>
      expect_completeness(
        "let x : ? = 42 in x",
        "Incomplete: 1 unfilled hole(s) (1 type).",
      )
    ),
    test_case("multiple holes", `Quick, () =>
      expect_completeness(
        "let x = ? in let y = ? in x + y",
        "Incomplete: 2 unfilled hole(s) (2 expression).",
      )
    ),
  ],
);

/* Programs with sum types, records, case dispatch, and functions */
let sum_type_program =
  "type Color = Red + Green + Blue in\nlet name_of : Color -> String = fun c ->\n  case c\n  | Red => \"red\"\n  | Green => \"green\"\n  | Blue => \"blue\"\n  end\nin\nname_of(Red)";

let record_program =
  "let mk_point = fun x -> fun y -> (x=x, y=y) in\nlet dist = fun p -> p.x * p.x + p.y * p.y in\nlet origin = mk_point(0)(0) in\ndist(origin)";

let complex_program_tests = (
  "AgentTools.ComplexPrograms",
  [
    /* Sum type + case: update a case arm body */
    test_case("update case arm body", `Quick, () => {
      let result =
        apply_and_render(
          sum_type_program,
          Update(Definition, "name_of", "fun c ->\n  case c\n  | Red => \"RED\"\n  | Green => \"green\"\n  | Blue => \"blue\"\n  end"),
        );
      check(bool, "has RED", true, string_contains("RED", result));
    }),
    /* Sum type: read statics on annotated function */
    test_case("statics on sum type function", `Quick, () => {
      let result = run_read_action(sum_type_program, GetStatics("name_of"));
      check(bool, "has String", true, string_contains("String", result));
      check(bool, "has name_of", true, string_contains("name_of", result));
    }),
    /* Sum type: context shows constructors */
    test_case("context has constructors", `Quick, () => {
      let result = run_read_action(sum_type_program, GetContext("name_of"));
      check(bool, "has Red", true, string_contains("Red", result));
      check(bool, "has Green", true, string_contains("Green", result));
      check(bool, "has Blue", true, string_contains("Blue", result));
    }),
    /* Sum type: selector on case arm via pipe */
    test_case("selector on case arm", `Quick, () => {
      let result =
        selector_query_unique(sum_type_program, "name_of = \\_ | Green => *");
      check_rendered("green arm", "\"green\"", result);
    }),
    /* Sum type: completeness */
    test_case("sum type program is complete", `Quick, () =>
      expect_completeness(sum_type_program, "Complete: no unfilled holes.")
    ),
    /* Record: update definition */
    test_case("update record function def", `Quick, () => {
      let result =
        apply_and_render(
          record_program,
          Update(Definition, "origin", "mk_point(1)(1)"),
        );
      check(bool, "has 1)(1)", true, string_contains("1)(1)", result));
    }),
    /* Record: insert new binding */
    test_case("insert after record binding", `Quick, () => {
      let result =
        apply_and_render(
          record_program,
          Insert(After, "dist", "let manhattan = fun p -> p.x + p.y"),
        );
      check(bool, "has manhattan", true, string_contains("manhattan", result));
    }),
    /* Updating type alias without cascading errors */
    test_case("update type alias def", `Quick, () => {
      let result =
        apply_and_render(
          "type T = Int in let x = 5 in x",
          Update(Definition, "T", "Bool"),
        );
      check_rendered("type alias change", "type T = Bool in let x = 5 in x", result);
    }),
    /* Delete a binding clause */
    test_case("delete binding in chain", `Quick, () => {
      let result =
        apply_and_render(
          "let a = 1 in let b = 2 in let c = 3 in c",
          Delete(BindingClause, "b"),
        );
      check_rendered("delete b", "let a = 1 in let c = 3 in c", result);
    }),
    /* Selector with descendant search into function */
    test_case("selector descend into function if", `Quick, () => {
      let code = "let f = fun x -> if x > 0 then x else 0 - x in f(5)";
      let result = selector_query_unique(code, "let f = \\_ if _... else *");
      check_rendered("else branch", "0 - x", result);
    }),
  ],
);

let tests = [
  edit_action_tests,
  high_level_node_map_tests,
  module_node_map_tests,
  path_extension_tests,
  module_edit_action_tests,
  edge_case_tests,
  read_action_tests,
  type_annotation_tests,
  composition_view_print_tests,
  seq_node_map_tests,
  selector_tests,
  completeness_tests,
  complex_program_tests,
];
