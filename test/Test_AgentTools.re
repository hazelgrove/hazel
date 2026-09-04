open Alcotest;
open Haz3lcore;
open Language;
open Action;
open CompositionActions;
open Util_web;

let mk_zipper = (code: string): Zipper.t => {
  switch (Parser.to_zipper(~root=Exp, code)) {
  | Some(z) => z
  | None => Alcotest.fail("Failed to parse: " ++ code)
  };
};

let mk_statics = (z: Zipper.t): StaticsBase.Map.t =>
  fst(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
    ),
  );

let render_zipper = (z: Zipper.t): string =>
  Printer.of_zipper(~holes="?", ~indent=" ", z);

let run_agent_action = (code: string, a: Action.Structural.t) => {
  let z = mk_zipper(code);
  Perform.go(
    ~settings=CoreSettings.on,
    ~statics=CachedStatics.empty,
    ~syntax=CachedSyntax.init(z),
    ~root=Exp,
    Structural(a),
    {
      zipper: z,
      col_target: None,
    },
  );
};

/** Runs a no-path insert_before / insert_after (the InsertAtProgramBoundary
    action). Before → prepend at program start; After → append at program end. */
let run_insert_at_program_boundary =
    (
      code: string,
      direction: Action.Structural.insert_target,
      new_code: string,
    ) => {
  let z = mk_zipper(code);
  let initial_info_map = mk_statics(z);
  let z_at_boundary =
    switch (direction) {
    | Before => Move.to_start(z)
    | After => Move.to_end(z)
    };
  switch (
    CompositionGo.Local.PerformUtils.introduce(
      z_at_boundary,
      "\n" ++ new_code ++ "\n",
    )
  ) {
  | Error(e) => Error(e)
  | Ok(new_z) =>
    let new_statics = mk_statics(new_z);
    let old_errors = ErrorPrint.all(initial_info_map);
    let new_errors = ErrorPrint.all(new_statics);
    if (List.length(new_errors) > List.length(old_errors)) {
      Error(
        Action.Failure.Composition_action_failure(
          "Static errors: " ++ String.concat(", ", new_errors),
        ),
      );
    } else {
      /* Mirror AgentToolCallHandler: boundary inserts normalize like the
         dispatch path. */
      Ok(
        CompositionGo.Local.PerformUtils.normalize_top_level(
          Dump.to_zipper(new_z, ~root=Exp),
        ),
      );
    };
  };
};

/** Test-only string compare for rendered programs: keep [[StringUtil]] minimal;
    normalization lives here, not in shared utils. */
let normalize_rendered_for_compare = (s: string): string => {
  let trim_horizontal_edges_line = (line: string): string => {
    line
    |> StringUtil.replace(StringUtil.regexp("^[\\t \\r]+"), _, "")
    |> StringUtil.replace(StringUtil.regexp("[\\t \\r]+$"), _, "");
  };
  s
  |> StringUtil.replace(StringUtil.regexp("\r\n"), _, "\n")
  |> StringUtil.replace(StringUtil.regexp("\r"), _, "\n")
  |> String.split_on_char('\n')
  |> List.map(trim_horizontal_edges_line)
  |> String.concat("\n")
  |> String.trim
  |> StringUtil.replace(StringUtil.regexp("[\\s]+"), _, " ");
};

let check_rendered = (name: string, expected: string, actual: string) => {
  let normalized = normalize_rendered_for_compare;
  check(
    testable(Fmt.string, (a, b) =>
      String.equal(normalized(a), normalized(b))
    ),
    name,
    expected,
    actual,
  );
};

/** Raw compare: trims only the outer edges, so interior spacing and
    linebreaks (separators, indentation) are actually checked. */
let check_rendered_exact = (name: string, expected: string, actual: string) => {
  check(
    testable(Fmt.string, (a, b) =>
      String.equal(String.trim(a), String.trim(b))
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

/** Run structural actions in order on the zipper produced by each step. */
let apply_chain_render =
    (code: string, actions: list(Action.Structural.t)): string => {
  let rec go = (z: Zipper.t, acts: list(Action.Structural.t)) =>
    switch (acts) {
    | [] => Ok(z)
    | [a, ...rest] =>
      switch (
        Perform.go(
          ~settings=CoreSettings.on,
          ~statics=CachedStatics.empty,
          ~syntax=CachedSyntax.init(z),
          Structural(a),
          {
            zipper: z,
            col_target: None,
          },
          ~root=Exp,
        )
      ) {
      | Ok(z') => go(z', rest)
      | Error(e) => Error(e)
      }
    };
  switch (go(mk_zipper(code), actions)) {
  | Ok(z) => render_zipper(z)
  | Error(err) =>
    Alcotest.fail(
      "Agent chain failed: " ++ Action.Failure.show(err) ++ "\nCode: " ++ code,
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

let expect_any_failure = (code: string, a: Action.Structural.t, name: string) => {
  switch (run_agent_action(code, a)) {
  | Ok(_) => Alcotest.fail("Expected failure: " ++ name)
  | Error(_) => ()
  };
};

/* ============================================================
   1. EDIT ACTION TESTS — basic operations
   ============================================================ */

let edit_action_tests = (
  "AgentTools.EditActions",
  [
    test_case("insert_before (no path) seeds empty program", `Quick, () => {
      switch (run_insert_at_program_boundary("?", Before, "let a = 3 in")) {
      | Ok(z) =>
        check_rendered(
          "insert_before_no_path",
          "let a = 3 in ?",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail(
          "insert_before (no path) failed: " ++ Action.Failure.show(err),
        )
      }
    }),
    test_case(
      "insert_before (no path) prepends to non-empty program", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "let a = 1 in a",
          Before,
          "let x = 0 in",
        )
      ) {
      | Ok(z) =>
        check_rendered(
          "insert_before_no_path_nonempty",
          "let x = 0 in let a = 1 in a",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail(
          "insert_before (no path) failed: " ++ Action.Failure.show(err),
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

/* ============================================================
   2. INSERT_AT_PROGRAM_BOUNDARY — no-path insert_before / insert_after
   ============================================================ */

let insert_at_program_boundary_tests = (
  "AgentTools.InsertAtProgramBoundary",
  [
    test_case(
      "insert_before (no path) on hole produces valid program", `Quick, () => {
      switch (run_insert_at_program_boundary("?", Before, "let x = 5 in")) {
      | Ok(z) =>
        check_rendered(
          "boundary_before_hole",
          "let x = 5 in ?",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("insert_before failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case(
      "insert_before (no path) seeds multi-binding program", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "?",
          Before,
          "let a = 1 in let b = 2 in let c = a + b in",
        )
      ) {
      | Ok(z) =>
        check_rendered(
          "boundary_before_multi",
          "let a = 1 in let b = 2 in let c = a + b in ?",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("insert_before failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case("insert_before (no path) with function value", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "?",
          Before,
          "let f = fun x -> x + 1 in",
        )
      ) {
      | Ok(z) =>
        check_rendered(
          "boundary_before_fun",
          "let f = fun x -> x + 1 in ?",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("insert_before failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case(
      "insert_before (no path) prepends to non-empty program", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "let b = 2 in b",
          Before,
          "let a = 1 in",
        )
      ) {
      | Ok(z) =>
        check_rendered(
          "boundary_before_nonempty",
          "let a = 1 in let b = 2 in b",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("insert_before failed: " ++ Action.Failure.show(err))
      }
    }),
  ],
);

/* ============================================================
   3. UPDATE_DEFINITION — complex definition replacement
   ============================================================ */

let update_definition_tests = (
  "AgentTools.UpdateDefinition",
  [
    test_case(
      "update_definition with complex expression",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a in b",
            Update(Definition, "b", "a + a * 2"),
          );
        check_rendered(
          "update_def_complex",
          "let a = 1 in let b = a + a * 2 in b",
          result,
        );
      },
    ),
    test_case(
      "update_definition with function",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f = 1 in f",
            Update(Definition, "f", "fun x -> x + 1"),
          );
        check_rendered(
          "update_def_function",
          "let f = fun x -> x + 1 in f",
          result,
        );
      },
    ),
    test_case(
      "update_definition first of three bindings",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Update(Definition, "a", "10"),
          );
        check_rendered(
          "update_def_first",
          "let a = 10 in let b = 2 in let c = 3 in a + b + c",
          result,
        );
      },
    ),
    test_case(
      "update_definition last of three bindings",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Update(Definition, "c", "a * b"),
          );
        check_rendered(
          "update_def_last",
          "let a = 1 in let b = 2 in let c = a * b in a + b + c",
          result,
        );
      },
    ),
    test_case(
      "update_definition with tuple",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a",
            Update(Definition, "a", "(1, 2, 3)"),
          );
        check_rendered("update_def_tuple", "let a = (1, 2, 3) in a", result);
      },
    ),
    test_case(
      "update_definition with boolean",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let flag = 1 in flag",
            Update(Definition, "flag", "true"),
          );
        check_rendered("update_def_bool", "let flag = true in flag", result);
      },
    ),
    test_case(
      "update_definition with string",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let msg = 1 in msg",
            Update(Definition, "msg", {|"hello"|}),
          );
        check_rendered(
          "update_def_string",
          {|let msg = "hello" in msg|},
          result,
        );
      },
    ),
    test_case(
      "update_definition with list literal",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let xs = 1 in xs",
            Update(Definition, "xs", "[1, 2, 3]"),
          );
        check_rendered("update_def_list", "let xs = [1, 2, 3] in xs", result);
      },
    ),
  ],
);

/* ============================================================
   4. UPDATE_BODY — edge cases for body replacement
   ============================================================ */

let update_body_tests = (
  "AgentTools.UpdateBody",
  [
    test_case(
      "update_body of first binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Update(Body, "a", "a * 100"),
          );
        check_rendered("update_body_first", "let a = 1 in a * 100", result);
      },
    ),
    test_case(
      "update_body with hole",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Update(Body, "b", "?"),
          );
        check_rendered(
          "update_body_hole",
          "let a = 1 in let b = 2 in ?",
          result,
        );
      },
    ),
    test_case(
      "update_body to complex expression",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in 0",
            Update(Body, "b", "a + b + a * b"),
          );
        check_rendered(
          "update_body_complex",
          "let a = 1 in let b = 2 in a + b + a * b",
          result,
        );
      },
    ),
    test_case(
      "update_body replaces entire remainder",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Update(Body, "a", "a"),
          );
        check_rendered(
          "update_body_replaces_remainder",
          "let a = 1 in a",
          result,
        );
      },
    ),
  ],
);

/* ============================================================
   5. UPDATE_PATTERN — renaming and use-site propagation
   ============================================================ */

let update_pattern_tests = (
  "AgentTools.UpdatePattern",
  [
    test_case(
      "update_pattern with type annotation",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a + 1",
            Update(Pattern, "a", "a : Int"),
          );
        check_rendered(
          "update_pat_annotated",
          "let a : Int = 1 in a + 1",
          result,
        );
      },
    ),
    test_case(
      "update_pattern renames across multiple use sites",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a in let c = a + b in c + a",
            Update(Pattern, "a", "x"),
          );
        check_rendered(
          "update_pat_multi_uses",
          "let x = 1 in let b = x in let c = x + b in c + x",
          result,
        );
      },
    ),
    test_case(
      "update_pattern middle binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a in let c = b in c",
            Update(Pattern, "b", "m"),
          );
        check_rendered(
          "update_pat_middle",
          "let a = 1 in let m = a in let c = m in c",
          result,
        );
      },
    ),
    test_case(
      "update_pattern last binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = a + b in c * 2",
            Update(Pattern, "c", "result"),
          );
        check_rendered(
          "update_pat_last",
          "let a = 1 in let b = 2 in let result = a + b in result * 2",
          result,
        );
      },
    ),
    test_case(
      "update_pattern then update_definition resolves new binding path",
      `Quick,
      () => {
        let result =
          apply_chain_render(
            "let speed = 50 in speed",
            [
              Update(Pattern, "speed", "velocity"),
              Update(Definition, "velocity", "99"),
            ],
          );
        check_rendered(
          "rename_then_update_def",
          "let velocity = 99 in velocity",
          result,
        );
      },
    ),
  ],
);

/* ============================================================
   6. UPDATE_BINDING_CLAUSE — whole-clause replacement
   ============================================================ */

let update_binding_clause_tests = (
  "AgentTools.UpdateBindingClause",
  [
    test_case(
      "update_binding_clause with annotation",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a",
            Update(BindingClause, "a", "let a : Int = 42 in"),
          );
        check_rendered(
          "update_bc_annotation",
          "let a : Int = 42 in a",
          result,
        );
      },
    ),
    test_case(
      "update_binding_clause path t on already-typed single binding",
      `Quick,
      () => {
        let code = "let t : Int = 42 in t";
        let result =
          apply_and_render(
            code,
            Update(
              BindingClause,
              "t",
              "let double : Int -> Int = fun x -> x * 2 in\nlet result = double(5) in",
            ),
          );
        check_rendered(
          "update_bc_t_typed_single",
          "let double : Int -> Int = fun x -> x * 2 in\nlet result = double(5) in t",
          result,
        );
      },
    ),
    test_case(
      "update_binding_clause def-nested t requires double/t not t",
      `Quick,
      () => {
        let code = "let double = let t : Int = 42 in t in double";
        expect_any_failure(
          code,
          Update(BindingClause, "t", "let t : Int = 99 in"),
          "def_nested_path_must_use_outer_slash_t",
        );
        let result =
          apply_and_render(
            code,
            Update(BindingClause, "double/t", "let t : Int = 99 in"),
          );
        check_rendered(
          "update_bc_def_nested_t",
          "let double = let t : Int = 99 in t in double",
          result,
        );
      },
    ),
    test_case(
      "update_binding_clause introduces new binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Update(BindingClause, "a", "let a = 1 in let x = a + 10 in"),
          );
        check_rendered(
          "update_bc_multi",
          "let a = 1 in let x = a + 10 in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "update_binding_clause middle of chain",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Update(BindingClause, "b", "let b : Int = a * 2 in"),
          );
        check_rendered(
          "update_bc_middle",
          "let a = 1 in let b : Int = a * 2 in let c = 3 in a + b + c",
          result,
        );
      },
    ),
  ],
);

/* ============================================================
   7. INSERT — before and after, various positions
   ============================================================ */

let insert_tests = (
  "AgentTools.Insert",
  [
    test_case(
      "insert_before first binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(Before, "a", "let z = 0 in"),
          );
        check_rendered(
          "insert_before_first",
          "let z = 0 in let a = 1 in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert_after last binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(After, "b", "let c = a + b in"),
          );
        check_rendered(
          "insert_after_last",
          "let a = 1 in let b = 2 in let c = a + b in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert_before with annotated binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(Before, "b", "let x : Int = a in"),
          );
        check_rendered(
          "insert_before_annotated",
          "let a = 1 in let x : Int = a in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert_after with function binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a",
            Insert(After, "a", "let f = fun x -> x + a in"),
          );
        check_rendered(
          "insert_after_fun",
          "let a = 1 in let f = fun x -> x + a in a",
          result,
        );
      },
    ),
    test_case(
      "insert_before in three-binding chain",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Insert(Before, "c", "let d = a + b in"),
          );
        check_rendered(
          "insert_before_third",
          "let a = 1 in let b = 2 in let d = a + b in let c = 3 in a + b + c",
          result,
        );
      },
    ),
    test_case(
      "insert_after tool call strips leading indentation",
      `Quick,
      () => {
        let args: API.Json.t =
          `Assoc([
            ("path", `String("b")),
            ("code", `String("  let c = 3 in")),
          ]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="insert_after", ~args)
        ) {
        | Action(EditorAction(a)) =>
          check_rendered_exact(
            "insert_indented_code",
            "let a = 1 in let b = 2 in\n\nlet c = 3 in\n a + b",
            apply_and_render("let a = 1 in let b = 2 in a + b", a),
          )
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "insert_after tool call normalizes CRLF linebreaks",
      `Quick,
      () => {
        let args: API.Json.t =
          `Assoc([
            ("path", `String("b")),
            ("code", `String("let c = 3 in\r\nlet d = 4 in")),
          ]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="insert_after", ~args)
        ) {
        | Action(EditorAction(a)) =>
          check_rendered_exact(
            "insert_crlf_code",
            "let a = 1 in let b = 2 in\n\nlet c = 3 in\n\nlet d = 4 in\n a + b",
            apply_and_render("let a = 1 in let b = 2 in a + b", a),
          )
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case("insert_after last binding keeps line separator", `Quick, () => {
      check_rendered_exact(
        "insert_after_last_separator",
        "let a = 1 in let b = 2 in\n\nlet c = 3 in\n a + b",
        apply_and_render(
          "let a = 1 in let b = 2 in a + b",
          Insert(After, "b", "let c = 3 in"),
        ),
      )
    }),
    test_case("insert_after (no path) appends on its own line", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "let a = 1 in let b = 2 in",
          After,
          "let c = 3 in",
        )
      ) {
      | Ok(z) =>
        check_rendered_exact(
          "boundary_append_separator",
          "let a = 1 in let b = 2 in\n\nlet c = 3 in\n?",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("boundary append failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case("update_body over bare hole keeps `in` separated", `Quick, () => {
      /* The hole grout sits flush against `in`; the replacement code
         must not fuse into `inlet e = ...` */
      check_rendered_exact(
        "update_body_hole_separator",
        "let x = 1 in let e = 2 in x",
        apply_and_render(
          "let x = 1 in",
          Update(Body, "x", "let e = 2 in x"),
        ),
      )
    }),
  ],
);

/* ============================================================
   8. DELETE — binding clause and body deletion
   ============================================================ */

let delete_tests = (
  "AgentTools.Delete",
  [
    test_case(
      "delete_binding_clause first of three",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in b + c",
            Delete(BindingClause, "a"),
          );
        check_rendered(
          "delete_bc_first",
          "let b = 2 in let c = 3 in b + c",
          result,
        );
      },
    ),
    test_case(
      "delete_binding_clause middle of three",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + c",
            Delete(BindingClause, "b"),
          );
        check_rendered(
          "delete_bc_middle",
          "let a = 1 in let c = 3 in a + c",
          result,
        );
      },
    ),
    test_case(
      "delete_binding_clause last of three",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b",
            Delete(BindingClause, "c"),
          );
        check_rendered(
          "delete_bc_last",
          "let a = 1 in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "delete_body first binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Delete(Body, "a"),
          );
        check_rendered("delete_body_first", "let a = 1 in ?", result);
      },
    ),
    test_case(
      "delete_body last binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Delete(Body, "c"),
          );
        check_rendered(
          "delete_body_last",
          "let a = 1 in let b = 2 in let c = 3 in ?",
          result,
        );
      },
    ),
    test_case(
      "delete Definition/Pattern unsupported",
      `Quick,
      () => {
        expect_composition_failure(
          "let a = 1 in a",
          Delete(Definition, "a"),
          "delete_definition",
        );
        expect_composition_failure(
          "let a = 1 in a",
          Delete(Pattern, "a"),
          "delete_pattern",
        );
      },
    ),
  ],
);

/* ============================================================
   9. STATIC ERROR REJECTION — edits that introduce type errors
   ============================================================ */

let static_error_tests = (
  "AgentTools.StaticErrorRejection",
  [
    test_case("update_definition rejects type mismatch", `Quick, () => {
      expect_composition_failure(
        "let a : Int = 1 in a + 1",
        Update(Definition, "a", "true"),
        "type_mismatch_def",
      )
    }),
    test_case("update_definition rejects unbound variable", `Quick, () => {
      expect_composition_failure(
        "let a = 1 in a",
        Update(Definition, "a", "nonexistent_var"),
        "unbound_var_def",
      )
    }),
    test_case("insert_before rejects ill-typed binding", `Quick, () => {
      expect_composition_failure(
        "let a = 1 in a",
        Insert(Before, "a", "let x : Int = true in"),
        "insert_before_type_error",
      )
    }),
    test_case("insert_after rejects ill-typed binding", `Quick, () => {
      expect_composition_failure(
        "let a = 1 in a",
        Insert(After, "a", "let x : String = 42 in"),
        "insert_after_type_error",
      )
    }),
  ],
);

/* ============================================================
   10. NESTED DEFINITIONS — path-based addressing with /
   ============================================================ */

let nested_definition_tests = (
  "AgentTools.NestedDefinitions",
  [
    test_case(
      "update_definition nested child",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b in a",
            Update(Definition, "a/b", "42"),
          );
        check_rendered(
          "update_def_nested",
          "let a = let b = 42 in b in a",
          result,
        );
      },
    ),
    test_case(
      "update_body of nested child",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b + 1 in a",
            Update(Body, "a/b", "b * 2"),
          );
        check_rendered(
          "update_body_nested",
          "let a = let b = 1 in b * 2 in a",
          result,
        );
      },
    ),
    test_case(
      "update_pattern of nested child",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b + 1 in a",
            Update(Pattern, "a/b", "c"),
          );
        check_rendered(
          "update_pat_nested",
          "let a = let c = 1 in c + 1 in a",
          result,
        );
      },
    ),
    test_case(
      "delete nested binding clause",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in let c = 2 in c in a",
            Delete(BindingClause, "a/b"),
          );
        check_rendered(
          "delete_bc_nested",
          "let a = let c = 2 in c in a",
          result,
        );
      },
    ),
    test_case(
      "insert_after into nested scope",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b in a",
            Insert(After, "a/b", "let c = b + 1 in"),
          );
        check_rendered(
          "insert_after_nested",
          "let a = let b = 1 in let c = b + 1 in b in a",
          result,
        );
      },
    ),
    test_case(
      "insert_before into nested scope",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b in a",
            Insert(Before, "a/b", "let z = 0 in"),
          );
        check_rendered(
          "insert_before_nested",
          "let a = let z = 0 in let b = 1 in b in a",
          result,
        );
      },
    ),
  ],
);

/* ============================================================
   11. INVALID PATH HANDLING — nonexistent or ill-formed paths
   ============================================================ */

let invalid_path_tests = (
  "AgentTools.InvalidPaths",
  [
    test_case("update_definition with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Update(Definition, "nonexistent", "2"),
        "nonexistent_path_def",
      )
    }),
    test_case("update_body with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Update(Body, "nonexistent", "2"),
        "nonexistent_path_body",
      )
    }),
    test_case("delete_binding_clause with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Delete(BindingClause, "nonexistent"),
        "nonexistent_path_delete",
      )
    }),
    test_case("insert_before with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Insert(Before, "nonexistent", "let x = 1 in"),
        "nonexistent_path_insert_before",
      )
    }),
    test_case("insert_after with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Insert(After, "nonexistent", "let x = 1 in"),
        "nonexistent_path_insert_after",
      )
    }),
    test_case(
      "deeply repeated path segments fail without applying edit (regression: agent bug 3)",
      `Quick,
      () => {
        let long_path = String.concat("/", List.init(40, _ => "a"));
        expect_any_failure(
          "let a = 1 in a",
          Update(Body, long_path, "2"),
          "long_repeated_path_segments",
        );
      },
    ),
    test_case(
      "failure message lists available paths (diagnostic for agent self-correction)",
      `Quick,
      () => {
      /* When the agent picks a wrong path, it should see the *actual* set
         of paths in the node map so its retry can be correct. */
      switch (
        run_agent_action(
          "let a = 1 in let b = 2 in a + b",
          Delete(BindingClause, "nonexistent"),
        )
      ) {
      | Ok(_) => Alcotest.fail("Expected failure")
      | Error(err) =>
        let msg = Action.Failure.show(err);
        check(
          bool,
          "error mentions 'Available paths'",
          true,
          Util_web.StringUtil.plain_search("Available paths", msg, 0) >= 0,
        );
        check(
          bool,
          "error lists binding 'a'",
          true,
          Util_web.StringUtil.plain_search("a", msg, 0) >= 0,
        );
        check(
          bool,
          "error lists binding 'b'",
          true,
          Util_web.StringUtil.plain_search("b", msg, 0) >= 0,
        );
      }
    }),
    test_case(
      "failure hints fully-qualified nested path when bare name is uniquely nested",
      `Quick,
      () => {
      /* `inner` lives at path `outer/inner` (nested inside outer's def).
         A bare query for `inner` should fail, but the error now includes
         the qualified suggestion so the agent knows the right form. */
      switch (
        run_agent_action(
          "let outer = let inner = 1 in inner in outer",
          Delete(BindingClause, "inner"),
        )
      ) {
      | Ok(_) => Alcotest.fail("Expected failure")
      | Error(err) =>
        let msg = Action.Failure.show(err);
        check(
          bool,
          "error includes 'outer/inner' suggestion",
          true,
          Util_web.StringUtil.plain_search("outer/inner", msg, 0) >= 0,
        );
      }
    }),
  ],
);

/* ============================================================
   12. HIGH LEVEL NODE MAP — tree construction and queries
   ============================================================ */

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
      "duplicate sibling path is ambiguous; #k disambiguates",
      `Quick,
      () => {
        let node_map = build_node_map("let n = 1 in let n = 2 in n");
        check(
          bool,
          "bare duplicate path is ambiguous",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "n") == None,
        );
        let id_n1 = HighLevelNodeMap.path_to_id(node_map, "n#1");
        let node = HighLevelNodeMap.find(node_map, id_n1);
        check(int, "n#1 is sibling_idx 0", 0, node.sibling_idx);
        check(
          bool,
          "path_to_id_opt matches path_to_id on #1",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "n#1") == Some(id_n1),
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
    test_case(
      "path_to_id_opt returns None for missing path",
      `Quick,
      () => {
        let node_map = build_node_map("let a = 1 in a");
        check(
          bool,
          "missing path returns None",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "nonexistent") == None,
        );
      },
    ),
    test_case(
      "path_to_id_opt returns Some for valid path",
      `Quick,
      () => {
        let node_map = build_node_map("let a = 1 in a");
        check(
          bool,
          "valid path returns Some",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "a") != None,
        );
      },
    ),
    test_case(
      "nested path_to_id resolves child",
      `Quick,
      () => {
        let node_map = build_node_map("let a = let b = 1 in b in a");
        let id_b = HighLevelNodeMap.path_to_id(node_map, "a/b");
        check(
          string,
          "nested path_to_id",
          "b",
          HighLevelNodeMap.id_to_name(node_map, id_b),
        );
      },
    ),
    test_case(
      "parent_of returns correct parent",
      `Quick,
      () => {
        let node_map = build_node_map("let a = let b = 1 in b in a");
        let node_b = HighLevelNodeMap.path_to_node(node_map, "a/b");
        switch (HighLevelNodeMap.parent_of(node_map, node_b)) {
        | Some(parent) => check(string, "parent name", "a", parent.name)
        | None => Alcotest.fail("Expected parent for nested node")
        };
      },
    ),
    test_case(
      "parent_of top-level returns None",
      `Quick,
      () => {
        let node_map = build_node_map("let a = 1 in let b = 2 in a + b");
        let node_a = HighLevelNodeMap.path_to_node(node_map, "a");
        check(
          bool,
          "top-level parent is None",
          true,
          HighLevelNodeMap.parent_of(node_map, node_a) == None,
        );
      },
    ),
    test_case(
      "descendants_of returns nested levels",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = let b = let c = 1 in c in b in a");
        let node_a = HighLevelNodeMap.path_to_node(node_map, "a");
        let desc_levels = HighLevelNodeMap.descendants_of(node_map, node_a);
        check(int, "descendants depth", 2, List.length(desc_levels));
      },
    ),
    test_case(
      "closest_valid_path_to_ill_path finds suggestion",
      `Quick,
      () => {
        let node_map =
          build_node_map("let abc = 1 in let def = 2 in abc + def");
        let suggestion =
          HighLevelNodeMap.closest_valid_path_to_ill_path(node_map, "abd");
        check(string, "closest path suggestion", "abc", suggestion);
      },
    ),
    test_case(
      "path_to_id works for typed pattern let t : Int = ...",
      `Quick,
      () => {
        let node_map = build_node_map("let t : Int = 42 in t");
        let id_t = HighLevelNodeMap.path_to_id(node_map, "t");
        check(
          string,
          "typed binding name",
          "t",
          HighLevelNodeMap.id_to_name(node_map, id_t),
        );
      },
    ),
    test_case(
      "body-chain let: inner binding has top-level path name (not outer/inner)",
      `Quick,
      () => {
        let node_map =
          build_node_map("let double = 1 in let t : Int = 42 in t");
        let id_t = HighLevelNodeMap.path_to_id(node_map, "t");
        check(
          string,
          "t is sibling in map, not double/t",
          "t",
          HighLevelNodeMap.id_to_name(node_map, id_t),
        );
        check(
          bool,
          "double/t absent for body-nested sibling chain",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "double/t") == None,
        );
      },
    ),
    test_case(
      "single-segment path does not match def-nested binding; use outer/t",
      `Quick,
      () => {
        /* `let outer = 1 in let inner in body` puts `inner` in the body chain
           (sibling of `outer`), so path `inner` is valid. Nesting must be in
           the *definition*: `let outer = let inner ... in ... in body`. */
        let node_map =
          build_node_map("let double = let t : Int = 42 in t in double");
        check(
          bool,
          "bare t not in map when def-nested",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "t") == None,
        );
        let id_nt = HighLevelNodeMap.path_to_id(node_map, "double/t");
        check(
          string,
          "nested path resolves",
          "t",
          HighLevelNodeMap.id_to_name(node_map, id_nt),
        );
      },
    ),
    test_case(
      "closest path for ill single-segment t prefers outer/t over outer",
      `Quick,
      () => {
        let node_map =
          build_node_map("let double = let t : Int = 42 in t in double");
        let suggestion =
          HighLevelNodeMap.closest_valid_path_to_ill_path(node_map, "t");
        check(string, "suggest nested binding path", "double/t", suggestion);
      },
    ),
    test_case(
      "ascribed let after type chain lands in node map",
      `Quick,
      () => {
        let code = "type Board = Int in let initial_board : Board = 1 in ?";
        let node_map = build_node_map(code);
        check(
          bool,
          "initial_board is in node map",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "initial_board") != None,
        );
        check(
          bool,
          "Board is also in node map",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "Board") != None,
        );
      },
    ),
    test_case(
      "chess-style long type chain + ascribed let with list literal",
      `Quick,
      () => {
        let code = "type Color = + White + Black in type PieceType = + Pawn + Knight + Bishop + Rook + Queen + King in type Piece = (Color, PieceType) in type Square = + Empty + Occupied(Piece) in type Board = [[Square]] in let initial_board : Board = [[Occupied((White, Rook))]] in ?";
        let node_map = build_node_map(code);
        let all_names =
          Id.Map.bindings(node_map)
          |> List.map(((_, n: HighLevelNodeMap.node)) => n.name)
          |> List.sort(String.compare);
        check(
          list(string),
          "all top-level binding names present",
          ["Board", "Color", "Piece", "PieceType", "Square", "initial_board"],
          all_names,
        );
      },
    ),
    test_case(
      "build returns None for non-binding program",
      `Quick,
      () => {
        let z = mk_zipper("1 + 2");
        let info_map = mk_statics(z);
        check(
          bool,
          "no bindings => None",
          true,
          HighLevelNodeMap.build(z, info_map) == None,
        );
      },
    ),
    test_case(
      "deeply nested path resolution (3 levels)",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = let b = let c = 1 in c in b in a");
        let id_c = HighLevelNodeMap.path_to_id(node_map, "a/b/c");
        check(
          string,
          "deep nested path",
          "c",
          HighLevelNodeMap.id_to_name(node_map, id_c),
        );
      },
    ),
    test_case(
      "gather_top_level with type alias",
      `Quick,
      () => {
        let node_map = build_node_map("type T = Int in let a = 1 in a");
        let top_level_ids = HighLevelNodeMap.gather_top_level(node_map);
        let top_level_names =
          List.map(
            (id: Id.t) => HighLevelNodeMap.id_to_name(node_map, id),
            top_level_ids,
          )
          |> List.sort(String.compare);
        check(
          list(string),
          "type alias top-level",
          ["T", "a"],
          top_level_names,
        );
      },
    ),
    test_case(
      "next_sibling_of wraps from last to first",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in a + b + c");
        let node_c = HighLevelNodeMap.path_to_node(node_map, "c");
        switch (HighLevelNodeMap.next_sibling_of(node_c)) {
        | None => Alcotest.fail("expected wrap-around from c to a")
        | Some(id) =>
          check(
            string,
            "wraps to a",
            "a",
            HighLevelNodeMap.id_to_name(node_map, id),
          )
        };
      },
    ),
    test_case(
      "prev_sibling_of wraps from first to last",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in a + b + c");
        let node_a = HighLevelNodeMap.path_to_node(node_map, "a");
        switch (HighLevelNodeMap.prev_sibling_of(node_a)) {
        | None => Alcotest.fail("expected wrap-around from a to c")
        | Some(id) =>
          check(
            string,
            "wraps to c",
            "c",
            HighLevelNodeMap.id_to_name(node_map, id),
          )
        };
      },
    ),
    test_case(
      "next_sibling_of steps forward within chain",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in a + b + c");
        let node_b = HighLevelNodeMap.path_to_node(node_map, "b");
        switch (HighLevelNodeMap.next_sibling_of(node_b)) {
        | None => Alcotest.fail("expected b -> c")
        | Some(id) =>
          check(
            string,
            "b next is c",
            "c",
            HighLevelNodeMap.id_to_name(node_map, id),
          )
        };
      },
    ),
    test_case(
      "prev_sibling_of steps backward within chain",
      `Quick,
      () => {
        let node_map =
          build_node_map("let a = 1 in let b = 2 in let c = 3 in a + b + c");
        let node_b = HighLevelNodeMap.path_to_node(node_map, "b");
        switch (HighLevelNodeMap.prev_sibling_of(node_b)) {
        | None => Alcotest.fail("expected b -> a")
        | Some(id) =>
          check(
            string,
            "b prev is a",
            "a",
            HighLevelNodeMap.id_to_name(node_map, id),
          )
        };
      },
    ),
  ],
);

/* ============================================================
   13. COMPOSITION VIEW — print and fold rendering
   ============================================================ */

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
    test_case("print_zipper preserves simple program", `Quick, () => {
      check_rendered(
        "simple program",
        "let a = 1 in a",
        "let a = 1 in a" |> mk_zipper |> CompositionView.Public.print_zipper,
      )
    }),
    test_case("print_zipper preserves multi-binding", `Quick, () => {
      check_rendered(
        "multi-binding program",
        "let a = 1 in let b = 2 in a + b",
        "let a = 1 in let b = 2 in a + b"
        |> mk_zipper
        |> CompositionView.Public.print_zipper,
      )
    }),
  ],
);

/* ============================================================
   14. COMPOSITION UTILS — tool name -> action parsing
   ============================================================ */

let mk_json_args = (pairs: list((string, string))): API.Json.t => {
  `Assoc(List.map(((k, v)) => (k, `String(v)), pairs));
};

let composition_utils_tests = (
  "CompositionUtils.action_of",
  [
    test_case(
      "parse update_definition tool call",
      `Quick,
      () => {
        let args = mk_json_args([("path", "a"), ("code", "42")]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="update_definition",
            ~args,
          )
        ) {
        | Action(EditorAction(Update(Definition, "a", "42"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse update_body tool call",
      `Quick,
      () => {
        let args = mk_json_args([("path", "b"), ("code", "x + 1")]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="update_body", ~args)
        ) {
        | Action(EditorAction(Update(Body, "b", "x + 1"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse update_pattern tool call",
      `Quick,
      () => {
        let args = mk_json_args([("path", "x"), ("code", "y")]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="update_pattern",
            ~args,
          )
        ) {
        | Action(EditorAction(Update(Pattern, "x", "y"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse update_binding_clause tool call",
      `Quick,
      () => {
        let args =
          mk_json_args([("path", "a"), ("code", "let a : Int = 5 in")]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="update_binding_clause",
            ~args,
          )
        ) {
        | Action(
            EditorAction(Update(BindingClause, "a", "let a : Int = 5 in")),
          ) =>
          ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse insert_after tool call",
      `Quick,
      () => {
        let args = mk_json_args([("path", "a"), ("code", "let b = 2 in")]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="insert_after", ~args)
        ) {
        | Action(EditorAction(Insert(After, "a", "let b = 2 in"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse insert_before tool call",
      `Quick,
      () => {
        let args = mk_json_args([("path", "b"), ("code", "let x = 0 in")]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="insert_before", ~args)
        ) {
        | Action(EditorAction(Insert(Before, "b", "let x = 0 in"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse delete_binding_clause tool call",
      `Quick,
      () => {
        let args = mk_json_args([("path", "b")]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="delete_binding_clause",
            ~args,
          )
        ) {
        | Action(EditorAction(Delete(BindingClause, "b"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse delete_body tool call",
      `Quick,
      () => {
        let args = mk_json_args([("path", "c")]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="delete_body", ~args)
        ) {
        | Action(EditorAction(Delete(Body, "c"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse insert_after (no path) tool call → InsertAtProgramBoundary",
      `Quick,
      () => {
        let args = mk_json_args([("code", "let x = 1 in")]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="insert_after", ~args)
        ) {
        | Action(InsertAtProgramBoundary(After, "let x = 1 in")) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse insert_before (no path) tool call → InsertAtProgramBoundary",
      `Quick,
      () => {
        let args = mk_json_args([("code", "let x = 1 in")]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="insert_before", ~args)
        ) {
        | Action(InsertAtProgramBoundary(Before, "let x = 1 in")) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse insert_after with empty path → InsertAtProgramBoundary",
      `Quick,
      () => {
        let args = mk_json_args([("path", ""), ("code", "let x = 1 in")]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="insert_after", ~args)
        ) {
        | Action(InsertAtProgramBoundary(After, "let x = 1 in")) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse expand tool call",
      `Quick,
      () => {
        let args =
          `Assoc([("paths", `List([`String("a"), `String("b")]))]);
        switch (CompositionUtils.Public.action_of(~tool_name="expand", ~args)) {
        | Action(AgentContextAction(Expand(["a", "b"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse collapse tool call",
      `Quick,
      () => {
        let args = `Assoc([("paths", `List([`String("a")]))]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="collapse", ~args)
        ) {
        | Action(AgentContextAction(Collapse(["a"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse place_probe tool call",
      `Quick,
      () => {
        let args =
          `Assoc([("paths", `List([`String("a"), `String("b")]))]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="place_probe", ~args)
        ) {
        | Action(ProbeAction(PlaceProbe(["a", "b"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse remove_probe tool call",
      `Quick,
      () => {
        let args = `Assoc([("paths", `List([`String("result")]))]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="remove_probe", ~args)
        ) {
        | Action(ProbeAction(RemoveProbe(["result"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse toggle_probe tool call",
      `Quick,
      () => {
        let args = `Assoc([("paths", `List([`String("f")]))]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="toggle_probe", ~args)
        ) {
        | Action(ProbeAction(ToggleProbe(["f"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse place_statics tool call",
      `Quick,
      () => {
        let args =
          `Assoc([("paths", `List([`String("x"), `String("y")]))]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="place_statics", ~args)
        ) {
        | Action(StaticsAction(PlaceStatics(["x", "y"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse remove_statics tool call",
      `Quick,
      () => {
        let args = `Assoc([("paths", `List([`String("foo")]))]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="remove_statics",
            ~args,
          )
        ) {
        | Action(StaticsAction(RemoveStatics(["foo"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse toggle_statics tool call",
      `Quick,
      () => {
        let args = `Assoc([("paths", `List([`String("bar")]))]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="toggle_statics",
            ~args,
          )
        ) {
        | Action(StaticsAction(ToggleStatics(["bar"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse place_syntax_projector tool call",
      `Quick,
      () => {
        let args =
          `Assoc([
            ("kind", `String("slider")),
            ("paths", `List([`String("n"), `String("m")])),
          ]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="place_syntax_projector",
            ~args,
          )
        ) {
        | Action(
            SyntaxProjectorAction(PlaceSyntaxProjector(Slider, ["n", "m"])),
          ) =>
          ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse remove_syntax_projector tool call",
      `Quick,
      () => {
        let args = `Assoc([("paths", `List([`String("x")]))]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="remove_syntax_projector",
            ~args,
          )
        ) {
        | Action(SyntaxProjectorAction(RemoveSyntaxProjector(["x"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse toggle_syntax_projector tool call",
      `Quick,
      () => {
        let args =
          `Assoc([
            ("kind", `String("check")),
            ("paths", `List([`String("flag")])),
          ]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="toggle_syntax_projector",
            ~args,
          )
        ) {
        | Action(
            SyntaxProjectorAction(
              ToggleSyntaxProjector(Checkbox, ["flag"]),
            ),
          ) =>
          ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "syntax projector kind probe returns Failure",
      `Quick,
      () => {
        let args =
          `Assoc([
            ("kind", `String("probe")),
            ("paths", `List([`String("x")])),
          ]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="place_syntax_projector",
            ~args,
          )
        ) {
        | Action(_) => Alcotest.fail("Expected Failure when kind is probe")
        | Failure(_) => ()
        };
      },
    ),
    test_case(
      "unknown tool name returns Failure",
      `Quick,
      () => {
        let args = mk_json_args([]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="does_not_exist",
            ~args,
          )
        ) {
        | Action(_) => Alcotest.fail("Expected Failure for unknown tool")
        | Failure(_) => ()
        };
      },
    ),
    test_case(
      "missing required arg returns Failure",
      `Quick,
      () => {
        let args = mk_json_args([("path", "a")]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="update_definition",
            ~args,
          )
        ) {
        | Action(_) => Alcotest.fail("Expected Failure for missing code arg")
        | Failure(_) => ()
        };
      },
    ),
    test_case(
      "parse set_active_task workbench tool",
      `Quick,
      () => {
        let args = mk_json_args([("title", "My Task")]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="set_active_task",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(SetActiveTask("My Task"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse unset_active_task workbench tool",
      `Quick,
      () => {
        let args = `Assoc([]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="unset_active_task",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(UnsetActiveTask)) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse mark_active_task_complete workbench tool",
      `Quick,
      () => {
        let args = mk_json_args([("summary", "All done")]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="mark_active_task_complete",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(MarkActiveTaskComplete("All done"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse mark_active_task_incomplete workbench tool",
      `Quick,
      () => {
        let args = `Assoc([]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="mark_active_task_incomplete",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(MarkActiveTaskIncomplete)) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse set_active_subtask workbench tool",
      `Quick,
      () => {
        let args = mk_json_args([("title", "Step 1")]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="set_active_subtask",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(SetActiveSubtask("Step 1"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse unset_active_subtask workbench tool",
      `Quick,
      () => {
        let args = `Assoc([]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="unset_active_subtask",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(UnsetActiveSubtask)) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse mark_active_subtask_complete workbench tool",
      `Quick,
      () => {
        let args = mk_json_args([("summary", "Subtask done")]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="mark_active_subtask_complete",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(MarkActiveSubtaskComplete("Subtask done"))) =>
          ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse mark_active_subtask_incomplete workbench tool",
      `Quick,
      () => {
        let args = `Assoc([]);
        switch (
          CompositionUtils.Public.action_of(
            ~tool_name="mark_active_subtask_incomplete",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(MarkActiveSubtaskIncomplete)) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
  ],
);

/* ============================================================
   15. SEQUENTIAL OPERATIONS — multiple edits in sequence
   ============================================================ */

let sequential_operations_tests = (
  "AgentTools.SequentialOperations",
  [
    test_case(
      "insert then update definition",
      `Quick,
      () => {
        let code = "let a = 1 in a";
        let z =
          switch (run_agent_action(code, Insert(After, "a", "let b = 0 in"))) {
          | Ok(z) => z
          | Error(err) =>
            Alcotest.fail("Insert failed: " ++ Action.Failure.show(err))
          };
        let rendered = render_zipper(z);
        check_rendered(
          "after insert",
          "let a = 1 in let b = 0 in a",
          rendered,
        );
        let z2_result =
          Perform.go(
            ~settings=CoreSettings.on,
            ~statics=CachedStatics.empty,
            ~syntax=CachedSyntax.init(z),
            ~root=Exp,
            Structural(Update(Definition, "b", "a + 1")),
            {
              zipper: z,
              col_target: None,
            },
          );
        switch (z2_result) {
        | Ok(z2) =>
          check_rendered(
            "after update",
            "let a = 1 in let b = a + 1 in a",
            render_zipper(z2),
          )
        | Error(err) =>
          Alcotest.fail(
            "Update after insert failed: " ++ Action.Failure.show(err),
          )
        };
      },
    ),
    test_case(
      "update_definition then update_body",
      `Quick,
      () => {
        let code = "let a = 1 in let b = 2 in a + b";
        let z =
          switch (run_agent_action(code, Update(Definition, "a", "10"))) {
          | Ok(z) => z
          | Error(err) =>
            Alcotest.fail(
              "First update failed: " ++ Action.Failure.show(err),
            )
          };
        let z2_result =
          Perform.go(
            ~settings=CoreSettings.on,
            ~statics=CachedStatics.empty,
            ~syntax=CachedSyntax.init(z),
            ~root=Exp,
            Structural(Update(Body, "b", "a * b")),
            {
              zipper: z,
              col_target: None,
            },
          );
        switch (z2_result) {
        | Ok(z2) =>
          check_rendered(
            "sequential def then body",
            "let a = 10 in let b = 2 in a * b",
            render_zipper(z2),
          )
        | Error(err) =>
          Alcotest.fail("Second update failed: " ++ Action.Failure.show(err))
        };
      },
    ),
    test_case(
      "update_body introduces nested recursive helper (regression: agent bug 1 / tools)",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f = fun x -> x in f(3)",
            Update(
              Body,
              "f",
              "let g = fun y -> if y == 0 then 0 else 1 + g(y - 1) in g(3)",
            ),
          );
        check(
          bool,
          "render mentions recursive call g(y - 1)",
          true,
          StringUtil.plain_search("g\\(y - 1\\)", result, 0) >= 0,
        );
        let z = mk_zipper(result);
        let errs = ErrorPrint.all(mk_statics(z));
        check(
          int,
          "still no static errors after edit",
          0,
          List.length(errs),
        );
      },
    ),
    test_case(
      "update_binding_clause then insert_after new binding path resolves (regression: agent bug 2)",
      `Quick,
      () => {
        let result =
          apply_chain_render(
            "let speed = 1 in 2",
            [
              Update(BindingClause, "speed", "let is_winner = true in"),
              Insert(After, "is_winner", "let score = 0 in"),
            ],
          );
        check_rendered(
          "clause_replace_then_insert_after_new_name",
          "let is_winner = true in let score = 0 in 2",
          result,
        );
      },
    ),
    test_case(
      "delete then insert at same position",
      `Quick,
      () => {
        let code = "let a = 1 in let b = 2 in let c = 3 in a + c";
        let z =
          switch (run_agent_action(code, Delete(BindingClause, "b"))) {
          | Ok(z) => z
          | Error(err) =>
            Alcotest.fail("Delete failed: " ++ Action.Failure.show(err))
          };
        let z2_result =
          Perform.go(
            ~settings=CoreSettings.on,
            ~statics=CachedStatics.empty,
            ~syntax=CachedSyntax.init(z),
            ~root=Exp,
            Structural(Insert(Before, "c", "let d = a * 2 in")),
            {
              zipper: z,
              col_target: None,
            },
          );
        switch (z2_result) {
        | Ok(z2) =>
          check_rendered(
            "delete then insert",
            "let a = 1 in let d = a * 2 in let c = 3 in a + c",
            render_zipper(z2),
          )
        | Error(err) =>
          Alcotest.fail(
            "Insert after delete failed: " ++ Action.Failure.show(err),
          )
        };
      },
    ),
    test_case(
      "rename then update definition of renamed binding",
      `Quick,
      () => {
        let code = "let a = 1 in let b = a in b";
        let z =
          switch (run_agent_action(code, Update(Pattern, "a", "x"))) {
          | Ok(z) => z
          | Error(err) =>
            Alcotest.fail("Rename failed: " ++ Action.Failure.show(err))
          };
        check_rendered(
          "after rename",
          "let x = 1 in let b = x in b",
          render_zipper(z),
        );
        let z2_result =
          Perform.go(
            ~settings=CoreSettings.on,
            ~statics=CachedStatics.empty,
            ~syntax=CachedSyntax.init(z),
            ~root=Exp,
            Structural(Update(Definition, "x", "100")),
            {
              zipper: z,
              col_target: None,
            },
          );
        switch (z2_result) {
        | Ok(z2) =>
          check_rendered(
            "update after rename",
            "let x = 100 in let b = x in b",
            render_zipper(z2),
          )
        | Error(err) =>
          Alcotest.fail(
            "Update after rename failed: " ++ Action.Failure.show(err),
          )
        };
      },
    ),
  ],
);

/* ============================================================
   16. TYPE ALIAS OPERATIONS
   ============================================================ */

let type_alias_tests = (
  "AgentTools.TypeAlias",
  [
    test_case(
      "update_definition of type alias",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let a : T = 1 in a",
            Update(Definition, "T", "Bool"),
          );
        check_rendered(
          "update_type_alias_def",
          "type T = Bool in let a : T = 1 in a",
          result,
        );
      },
    ),
    test_case(
      "delete type alias binding clause",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let a = 1 in a",
            Delete(BindingClause, "T"),
          );
        check_rendered("delete_type_alias", "let a = 1 in a", result);
      },
    ),
    test_case(
      "insert_before type alias",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let a : T = 1 in a",
            Insert(Before, "T", "let z = 0 in"),
          );
        check_rendered(
          "insert_before_type_alias",
          "let z = 0 in type T = Int in let a : T = 1 in a",
          result,
        );
      },
    ),
    test_case(
      "insert_after type alias",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let a : T = 1 in a",
            Insert(After, "T", "let z = 0 in"),
          );
        check_rendered(
          "insert_after_type_alias",
          "type T = Int in let z = 0 in let a : T = 1 in a",
          result,
        );
      },
    ),
  ],
);

/* ============================================================
   17. COMPLEX PROGRAMS — functions, recursion, match
   ============================================================ */

let complex_program_tests = (
  "AgentTools.ComplexPrograms",
  [
    test_case(
      "update_definition with if-else",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Update(Definition, "b", "if a > 0 then 10 else 20"),
          );
        check_rendered(
          "update_def_ifelse",
          "let a = 1 in let b = if a > 0 then 10 else 20 in a + b",
          result,
        );
      },
    ),
    test_case(
      "update_definition with let-in expression",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a",
            Update(Definition, "a", "let x = 2 in x + 1"),
          );
        check_rendered(
          "update_def_let_in",
          "let a = let x = 2 in x + 1 in a",
          result,
        );
      },
    ),
    test_case(
      "update_definition with recursive function",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f = fun x -> x in f(5)",
            Update(
              Definition,
              "f",
              "fun x -> if x == 0 then 1 else x * f(x - 1)",
            ),
          );
        check_rendered(
          "update_def_recursive",
          "let f = fun x -> if x == 0 then 1 else x * f(x - 1) in f(5)",
          result,
        );
      },
    ),
    test_case(
      "update_definition with module body (lowercase let)",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = 1 in m",
            Update(Definition, "m", "{ let x = 1; let y = 2 }"),
          );
        check_rendered(
          "update_def_module",
          "let m = { let x = 1; let y = 2 } in m",
          result,
        );
      },
    ),
    test_case("update_definition with capitalized module M", `Quick, () => {
      switch (
        run_agent_action(
          "module M = { let x = 1 } in M.x",
          Update(Definition, "M", "{ let x = 10 }"),
        )
      ) {
      | Ok(z) =>
        let result = render_zipper(z);
        check_rendered(
          "update_def_module_M",
          "module M = { let x = 10 } in M.x",
          result,
        );
      | Error(err) =>
        Alcotest.fail(
          "Agent action failed: "
          ++ Action.Failure.show(err)
          ++ "\nCode: module M = { let x = 1 } in M.x",
        )
      }
    }),
    test_case(
      "delete_binding_clause removes capitalized module M",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "module M = { let x = 1 } in 42",
            Delete(BindingClause, "M"),
          );
        check_rendered("delete_module_M", "42", result);
      },
    ),
    test_case(
      "insert function binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a",
            Insert(After, "a", "let double = fun x -> x * 2 in"),
          );
        check_rendered(
          "insert_function",
          "let a = 1 in let double = fun x -> x * 2 in a",
          result,
        );
      },
    ),
    test_case(
      "update_binding_clause to annotated function",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f = 1 in f",
            Update(
              BindingClause,
              "f",
              "let f : Int -> Int = fun x -> x + 1 in",
            ),
          );
        check_rendered(
          "update_bc_annotated_fun",
          "let f : Int -> Int = fun x -> x + 1 in f",
          result,
        );
      },
    ),
  ],
);

/* ============================================================
   18. AGENT CONTEXT — expand/collapse path management
   ============================================================ */

let agent_context_tests = (
  "AgentContext",
  [
    test_case(
      "init has empty expanded_paths",
      `Quick,
      () => {
        let ctx = AgentContext.Utils.init();
        check(list(string), "init paths", [], ctx.expanded_paths);
      },
    ),
    test_case(
      "add_paths appends paths",
      `Quick,
      () => {
        let ctx = AgentContext.Utils.init();
        let ctx = AgentContext.Utils.add_paths(["a", "b"], ctx);
        check(
          int,
          "paths count after add",
          2,
          List.length(ctx.expanded_paths),
        );
        check(bool, "contains a", true, List.mem("a", ctx.expanded_paths));
        check(bool, "contains b", true, List.mem("b", ctx.expanded_paths));
      },
    ),
    test_case(
      "remove_paths removes specified paths",
      `Quick,
      () => {
        let ctx = AgentContext.Utils.init();
        let ctx = AgentContext.Utils.add_paths(["a", "b", "c"], ctx);
        let ctx = AgentContext.Utils.remove_paths(["b"], ctx);
        check(
          int,
          "paths count after remove",
          2,
          List.length(ctx.expanded_paths),
        );
        check(
          bool,
          "still contains a",
          true,
          List.mem("a", ctx.expanded_paths),
        );
        check(bool, "b removed", false, List.mem("b", ctx.expanded_paths));
      },
    ),
    test_case(
      "expand action adds paths",
      `Quick,
      () => {
        let ctx = AgentContext.Utils.init();
        let ctx = AgentContext.Update.update(Expand(["x", "y"]), ctx);
        check(int, "expanded count", 2, List.length(ctx.expanded_paths));
      },
    ),
    test_case(
      "collapse action removes paths",
      `Quick,
      () => {
        let ctx = AgentContext.Utils.init();
        let ctx = AgentContext.Update.update(Expand(["x", "y", "z"]), ctx);
        let ctx = AgentContext.Update.update(Collapse(["y"]), ctx);
        check(int, "after collapse", 2, List.length(ctx.expanded_paths));
        check(bool, "y removed", false, List.mem("y", ctx.expanded_paths));
      },
    ),
    test_case(
      "freshen_paths removes stale paths",
      `Quick,
      () => {
        let ctx = AgentContext.Utils.init();
        let ctx = AgentContext.Utils.add_paths(["a", "nonexistent"], ctx);
        let node_map = build_node_map("let a = 1 in a");
        let ctx = AgentContext.Utils.freshen_paths(ctx, node_map);
        check(
          int,
          "freshen removes stale",
          1,
          List.length(ctx.expanded_paths),
        );
        check(bool, "a remains", true, List.mem("a", ctx.expanded_paths));
      },
    ),
    test_case(
      "freshen_paths keeps valid module path M",
      `Quick,
      () => {
        let ctx = AgentContext.Utils.init();
        let ctx = AgentContext.Utils.add_paths(["M", "nonexistent"], ctx);
        let node_map = build_node_map("module M = { let x = 1 } in M.x");
        let ctx = AgentContext.Utils.freshen_paths(ctx, node_map);
        check(int, "freshen keeps M", 1, List.length(ctx.expanded_paths));
        check(bool, "M remains", true, List.mem("M", ctx.expanded_paths));
      },
    ),
    test_case(
      "path M resolves for capitalized module",
      `Quick,
      () => {
        let node_map =
          build_node_map("module M = { let x = 1; let y = 2 } in M.x + M.y");
        check(
          bool,
          "path M resolves",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "M") != None,
        );
      },
    ),
  ],
);

/* ============================================================
   19. ERROR PRINT — static error reporting
   ============================================================ */

let error_print_tests = (
  "ErrorPrint",
  [
    test_case(
      "well-typed program has no errors",
      `Quick,
      () => {
        let z = mk_zipper("let a = 1 in a + 1");
        let info_map = mk_statics(z);
        let errors = ErrorPrint.all(info_map);
        check(int, "no errors", 0, List.length(errors));
      },
    ),
    test_case(
      "type mismatch produces errors",
      `Quick,
      () => {
        let z = mk_zipper("let a : Int = true in a");
        let info_map = mk_statics(z);
        let errors = ErrorPrint.all(info_map);
        check(bool, "has errors", true, List.length(errors) > 0);
      },
    ),
    test_case(
      "unbound variable produces errors",
      `Quick,
      () => {
        let z = mk_zipper("let a = xyz in a");
        let info_map = mk_statics(z);
        let errors = ErrorPrint.all(info_map);
        check(bool, "has unbound errors", true, List.length(errors) > 0);
      },
    ),
    test_case(
      "hole-only program has no hard errors",
      `Quick,
      () => {
        let z = mk_zipper("?");
        let info_map = mk_statics(z);
        let errors = ErrorPrint.all(info_map);
        check(int, "hole has no hard errors", 0, List.length(errors));
      },
    ),
    test_case(
      "nested recursive let inside outer function — inner self-reference is bound (regression: agent bug 1)",
      `Quick,
      () => {
        let code = "let f = fun x -> let g = fun y -> if y == 0 then 0 else 1 + g(y - 1) in g(x) in f(3)";
        let z = mk_zipper(code);
        let info_map = mk_statics(z);
        let errors = ErrorPrint.all(info_map);
        check(
          int,
          "no static errors for nested recursive helper",
          0,
          List.length(errors),
        );
      },
    ),
  ],
);

/* ============================================================
   20. TOOL JSON DEFINITIONS — verify structure
   ============================================================ */

let get_tool_name = (tool: API.Json.t): option(string) => {
  switch (API.Json.dot("function", tool)) {
  | Some(func) =>
    switch (API.Json.dot("name", func)) {
    | Some(name_json) => API.Json.str(name_json)
    | None => None
    }
  | None => None
  };
};

let get_tool_description = (tool: API.Json.t): option(string) => {
  switch (API.Json.dot("function", tool)) {
  | Some(func) =>
    switch (API.Json.dot("description", func)) {
    | Some(desc_json) => API.Json.str(desc_json)
    | None => None
    }
  | None => None
  };
};

let tool_json_tests = (
  "ToolJsonDefinitions",
  [
    test_case(
      "all tools have function type and name",
      `Quick,
      () => {
        let tools = CompositionUtils.Public.tools;
        List.iter(
          (tool: API.Json.t) => {
            switch (API.Json.dot("type", tool)) {
            | Some(`String("function")) => ()
            | _ => Alcotest.fail("Tool missing type=function")
            };
            switch (API.Json.dot("function", tool)) {
            | Some(func) =>
              switch (API.Json.dot("name", func)) {
              | Some(`String(_)) => ()
              | _ => Alcotest.fail("Tool function missing name")
              }
            | None => Alcotest.fail("Tool missing function field")
            };
          },
          tools,
        );
      },
    ),
    test_case(
      "all tools have parameters with type=object",
      `Quick,
      () => {
        let tools = CompositionUtils.Public.tools;
        List.iter(
          (tool: API.Json.t) => {
            switch (API.Json.dot("function", tool)) {
            | Some(func) =>
              switch (API.Json.dot("parameters", func)) {
              | Some(params) =>
                switch (API.Json.dot("type", params)) {
                | Some(`String("object")) => ()
                | _ => Alcotest.fail("Parameters missing type=object")
                }
              | None => Alcotest.fail("Function missing parameters")
              }
            | None => Alcotest.fail("Tool missing function")
            }
          },
          tools,
        );
      },
    ),
    test_case(
      "expected tool count matches",
      `Quick,
      () => {
        let tools = CompositionUtils.Public.tools;
        check(int, "tool count", 36, List.length(tools));
      },
    ),
    test_case(
      "get_tool_name extracts name correctly",
      `Quick,
      () => {
        let tools = CompositionUtils.Public.tools;
        let first_tool = List.nth(tools, 0);
        switch (get_tool_name(first_tool)) {
        | Some("expand") => ()
        | Some(name) =>
          Alcotest.fail("Expected 'expand', got '" ++ name ++ "'")
        | None => Alcotest.fail("get_tool_name returned None")
        };
      },
    ),
    test_case(
      "get_tool_description returns non-empty for all tools",
      `Quick,
      () => {
        let tools = CompositionUtils.Public.tools;
        List.iter(
          (tool: API.Json.t) => {
            switch (get_tool_description(tool)) {
            | Some(desc) =>
              check(
                bool,
                "description non-empty",
                true,
                String.length(desc) > 0,
              )
            | None => Alcotest.fail("get_tool_description returned None")
            }
          },
          tools,
        );
      },
    ),
    test_case(
      "all tool names are unique",
      `Quick,
      () => {
        let tools = CompositionUtils.Public.tools;
        let names = List.filter_map(get_tool_name, tools);
        let unique_names = List.sort_uniq(String.compare, names);
        check(
          int,
          "all names unique",
          List.length(names),
          List.length(unique_names),
        );
      },
    ),
    test_case(
      "edit tools have required path parameter",
      `Quick,
      () => {
        /* insert_after / insert_before are intentionally excluded: their
           `path` is optional (omitting it inserts at the program boundary). */
        let edit_tool_names = [
          "update_definition",
          "update_body",
          "update_pattern",
          "update_binding_clause",
          "delete_binding_clause",
          "delete_body",
        ];
        let tools = CompositionUtils.Public.tools;
        List.iter(
          (tool: API.Json.t) => {
            switch (get_tool_name(tool)) {
            | Some(name) when List.mem(name, edit_tool_names) =>
              switch (API.Json.dot("function", tool)) {
              | Some(func) =>
                switch (API.Json.dot("parameters", func)) {
                | Some(params) =>
                  switch (API.Json.dot("required", params)) {
                  | Some(`List(required)) =>
                    let has_path =
                      List.exists(
                        r =>
                          switch (r) {
                          | `String("path") => true
                          | _ => false
                          },
                        required,
                      );
                    check(bool, name ++ " requires path", true, has_path);
                  | _ => Alcotest.fail(name ++ " missing required field")
                  }
                | None => Alcotest.fail(name ++ " missing parameters")
                }
              | None => Alcotest.fail(name ++ " missing function")
              }
            | _ => ()
            }
          },
          tools,
        );
      },
    ),
    test_case(
      "view tools with paths have required paths parameter",
      `Quick,
      () => {
        let probe_tool_names = [
          "place_probe",
          "remove_probe",
          "toggle_probe",
          "place_statics",
          "remove_statics",
          "toggle_statics",
        ];
        let tools = CompositionUtils.Public.tools;
        List.iter(
          (tool: API.Json.t) => {
            switch (get_tool_name(tool)) {
            | Some(name) when List.mem(name, probe_tool_names) =>
              switch (API.Json.dot("function", tool)) {
              | Some(func) =>
                switch (API.Json.dot("parameters", func)) {
                | Some(params) =>
                  switch (API.Json.dot("required", params)) {
                  | Some(`List(required)) =>
                    let has_paths =
                      List.exists(
                        r =>
                          switch (r) {
                          | `String("paths") => true
                          | _ => false
                          },
                        required,
                      );
                    check(bool, name ++ " requires paths", true, has_paths);
                  | _ => Alcotest.fail(name ++ " missing required field")
                  }
                | None => Alcotest.fail(name ++ " missing parameters")
                }
              | None => Alcotest.fail(name ++ " missing function")
              }
            | _ => ()
            }
          },
          tools,
        );
      },
    ),
    test_case(
      "tool registry covers exactly the declared tools",
      `Quick,
      () => {
        let declared =
          List.filter_map(get_tool_name, CompositionUtils.Public.tools);
        let registered = List.map(fst, Web.Agent.ToolUtils.registry);
        List.iter(
          name =>
            check(
              bool,
              "registry entry for " ++ name,
              true,
              List.mem(name, registered),
            ),
          declared,
        );
        List.iter(
          name =>
            check(
              bool,
              "declared tool for registry entry " ++ name,
              true,
              List.mem(name, declared),
            ),
          registered,
        );
      },
    ),
    test_case(
      "tool registry golden: categories and derived mode lists",
      `Quick,
      () => {
        module ToolUtils = Web.Agent.ToolUtils;
        let cat = ToolUtils.category_of_tool;
        check(string, "expand", "View", cat("expand"));
        check(string, "place_probe", "View", cat("place_probe"));
        check(string, "update_definition", "Edit", cat("update_definition"));
        check(
          string,
          "create_new_task",
          "Workbench",
          cat("create_new_task"),
        );
        check(
          string,
          "update_active_task",
          "Other",
          cat("update_active_task"),
        );
        check(string, "delete_task", "Other", cat("delete_task"));
        check(string, "unknown name", "Other", cat("no_such_tool"));
        check(
          list(string),
          "edit tool names",
          [
            "update_definition",
            "update_body",
            "update_pattern",
            "update_binding_clause",
            "delete_binding_clause",
            "delete_body",
            "insert_after",
            "insert_before",
          ],
          ToolUtils.edit_tool_names,
        );
        check(
          list(string),
          "workbench tool names",
          [
            "create_new_task",
            "set_active_task",
            "unset_active_task",
            "set_active_subtask",
            "unset_active_subtask",
            "mark_active_task_complete",
            "mark_active_task_incomplete",
            "mark_active_subtask_complete",
            "mark_active_subtask_incomplete",
            "mark_active_subtask_failed",
            "mark_active_task_failed",
            "add_new_subtask_to_active_task",
            "reorder_subtasks_in_active_task",
          ],
          ToolUtils.workbench_tool_names,
        );
        check(
          list(string),
          "overlay tool names",
          [
            "place_probe",
            "remove_probe",
            "toggle_probe",
            "place_statics",
            "remove_statics",
            "toggle_statics",
            "place_syntax_projector",
            "remove_syntax_projector",
            "toggle_syntax_projector",
          ],
          ToolUtils.overlay_tool_names,
        );
      },
    ),
  ],
);

/* ============================================================
   STATICS REFRACTOR (path-based helpers)
   ============================================================ */

let statics_refractor_tests = (
  "AgentTools.StaticsRefractor",
  [
    test_case(
      "place_statics_at then remove_statics_at clears statics status",
      `Quick,
      () => {
        let code = "let x = 1 in x";
        let z = mk_zipper(code);
        let info_map = mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (HighLevelNodeMap.Public.path_to_id_opt(nm, "x")) {
          | None => Alcotest.fail("expected path x")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.place_statics_at(~syntax, id, info_map, z);
            switch (ProbePerform.probe_status(id, info_map, z2.refractors)) {
            | Statics(_) => ()
            | _ =>
              Alcotest.fail("expected Statics status after place_statics_at")
            };
            let z3 = ProbePerform.remove_statics_at(id, info_map, z2);
            switch (ProbePerform.probe_status(id, info_map, z3.refractors)) {
            | Non => ()
            | _ =>
              Alcotest.fail(
                "expected Non status after remove_statics_at (statics only)",
              )
            };
          }
        };
      },
    ),
    test_case(
      "remove_statics_at does not strip a runtime probe",
      `Quick,
      () => {
        let code = "let x = 1 in x";
        let z = mk_zipper(code);
        let info_map = mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (HighLevelNodeMap.Public.path_to_id_opt(nm, "x")) {
          | None => Alcotest.fail("expected path x")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.add_manual(~syntax, id, info_map, z);
            switch (ProbePerform.probe_status(id, info_map, z2.refractors)) {
            | Manual(_) => ()
            | _ => Alcotest.fail("expected Manual probe after add_manual")
            };
            let z3 = ProbePerform.remove_statics_at(id, info_map, z2);
            switch (ProbePerform.probe_status(id, info_map, z3.refractors)) {
            | Manual(_) => ()
            | _ =>
              Alcotest.fail(
                "remove_statics_at should leave manual probe in place",
              )
            };
          }
        };
      },
    ),
  ],
);

/* ============================================================
   AGENT TOOLS WITH SYNTAX PROJECTORS / LIVELITS IN PROGRAM TEXT
   Projector concrete syntax: ^^<kind>(<exp>) — see Test_ExpToSegment roundtrip tests.
   ============================================================ */

let agent_tools_with_projectors_tests = (
  "AgentTools.WithProjectors",
  [
    test_case(
      "update_definition replaces binding whose definition uses slider projector",
      `Quick,
      () => {
        let code = {|let x = ^^slider(50) in x + 1|};
        let result = apply_and_render(code, Update(Definition, "x", "100"));
        check_rendered(
          "slider projector def replaced",
          "let x = 100 in x + 1",
          result,
        );
      },
    ),
    test_case(
      "update_definition strips slider projector when new def is not an Int literal",
      `Quick,
      () => {
        let code = {|let speed = ^^slider(50) in speed|};
        let result =
          apply_and_render(code, Update(Definition, "speed", "\"hello\""));
        check_rendered(
          "slider stripped for string def",
          "let speed = \"hello\" in speed",
          result,
        );
      },
    ),
    test_case(
      "update_definition replaces checkbox projector binding",
      `Quick,
      () => {
        let code = {|let b = ^^check(true) in b|};
        let result =
          apply_and_render(code, Update(Definition, "b", "false"));
        check_rendered(
          "checkbox projector def replaced",
          "let b = false in b",
          result,
        );
      },
    ),
    test_case(
      "update_body works when a binding uses nested fold + slider projectors",
      `Quick,
      () => {
        let code = {|let n = ^^fold(^^slider(10) + 1) in n|};
        let result = apply_and_render(code, Update(Body, "n", "0"));
        check_rendered(
          "body update with nested projectors",
          "let n = ^^fold(^^slider(10) + 1) in 0",
          result,
        );
      },
    ),
    test_case(
      "place_statics_at on binding with projector in definition",
      `Quick,
      () => {
        let code = {|let v = ^^slider(42) in v|};
        let z = mk_zipper(code);
        let info_map = mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (HighLevelNodeMap.Public.path_to_id_opt(nm, "v")) {
          | None => Alcotest.fail("expected path v")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.place_statics_at(~syntax, id, info_map, z);
            switch (ProbePerform.probe_status(id, info_map, z2.refractors)) {
            | Statics(_) => ()
            | _ =>
              Alcotest.fail(
                "expected Statics after place_statics_at on projector def",
              )
            };
          }
        };
      },
    ),
    test_case(
      "add_manual probe on binding with csv projector wrapper",
      `Quick,
      () => {
        let code = {|let rows = ^^csv([]) in rows|};
        let z = mk_zipper(code);
        let info_map = mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (HighLevelNodeMap.Public.path_to_id_opt(nm, "rows")) {
          | None => Alcotest.fail("expected path rows")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.add_manual(~syntax, id, info_map, z);
            switch (ProbePerform.probe_status(id, info_map, z2.refractors)) {
            | Manual(_) => ()
            | _ =>
              Alcotest.fail(
                "expected Manual probe on binding with csv projector",
              )
            };
          }
        };
      },
    ),
  ],
);

/* ============================================================
   GENERAL TREE — get_refs_to_after_pattern_edit vs get_refs_to
   ============================================================ */

let general_tree_refs_tests = (
  "AgentTools.GeneralTreeRefs",
  [
    test_case(
      "get_refs_to_after_pattern_edit matches get_refs_to when pre/post let info agree",
      `Quick,
      () => {
        let code = "let k = 1 in k + k";
        let z = mk_zipper(code);
        let info_map = mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None => Alcotest.fail("expected node map")
        | Some(nm) =>
          let node = HighLevelNodeMap.path_to_node(nm, "k");
          let co_plain = GeneralTreeUtils.get_refs_to(node.info, info_map);
          let co_hybrid =
            GeneralTreeUtils.get_refs_to_after_pattern_edit(
              ~pre_edit_let_info=node.info,
              ~post_edit_let_info=node.info,
              info_map,
            );
          check(
            string,
            "co_ctx same as plain get_refs_to",
            CoCtx.show(co_plain),
            CoCtx.show(co_hybrid),
          );
        };
      },
    ),
  ],
);

/* ============================================================
   ASCRIBED / EXPLICITLY-TYPED BINDINGS — edit-action regression tests
   ============================================================

   Regression coverage for bindings with type ascriptions
   (`let x : T = v in ...`). These shapes were implicated in a chess-style
   reproducer where the agent claimed `initial_board` wasn't in the node map
   after a chain of `type` aliases ending in an ascribed `let`. The bug did
   not reproduce here — but these tests make sure it stays that way as the
   node-map builder and edit dispatch evolve. */

let ascribed_binding_tests = (
  "AgentTools.AscribedBindings",
  [
    test_case(
      "path_to_id resolves ascribed top-level let",
      `Quick,
      () => {
        let node_map = build_node_map("let x : Int = 1 in x");
        check(
          bool,
          "x present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "x") != None,
        );
      },
    ),
    test_case(
      "path_to_id resolves ascribed let after type alias",
      `Quick,
      () => {
        let node_map = build_node_map("type T = Int in let x : T = 1 in x");
        check(
          bool,
          "x present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "x") != None,
        );
        check(
          bool,
          "T present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "T") != None,
        );
      },
    ),
    test_case(
      "path_to_id resolves ascribed let after chain of type aliases",
      `Quick,
      () => {
        /* Direct analogue of the chess reproducer: long tyalias body chain
           then an ascribed let at the end. */
        let code = "type A = Int in type B = A in type C = B in let x : C = 1 in ?";
        let node_map = build_node_map(code);
        check(
          bool,
          "x present at end of type chain",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "x") != None,
        );
      },
    ),
    test_case(
      "path_to_id resolves ascribed let nested in def (outer/inner)",
      `Quick,
      () => {
        let node_map =
          build_node_map("let outer = let inner : Int = 1 in inner in outer");
        check(
          bool,
          "outer/inner present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "outer/inner") != None,
        );
        check(
          bool,
          "bare inner not ambiguously at top level",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "inner") == None,
        );
      },
    ),
    test_case(
      "delete_binding_clause on simple ascribed let (body preserved, x becomes free)",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 1 in x + 1",
            Delete(BindingClause, "x"),
          );
        check_rendered("delete ascribed x", "x + 1", result);
      },
    ),
    test_case(
      "delete_binding_clause on ascribed let in middle of chain",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b : Int = 2 in let c = 3 in a + b + c",
            Delete(BindingClause, "b"),
          );
        check_rendered(
          "delete ascribed middle b",
          "let a = 1 in let c = 3 in a + b + c",
          result,
        );
      },
    ),
    test_case(
      "delete_binding_clause on ascribed let following type chain",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let x : T = 1 in x",
            Delete(BindingClause, "x"),
          );
        check_rendered(
          "delete x after type chain",
          "type T = Int in x",
          result,
        );
      },
    ),
    test_case(
      "delete_binding_clause on ascribed let with complex list-literal body",
      `Quick,
      () => {
        /* Shape close to chess: ascribed let whose definition is a nested
           list literal of constructor applications. */
        let code = "type Color = + White + Black in type Board = [[Color]] in let initial : Board = [[White, Black], [Black, White]] in ?";
        let result =
          apply_and_render(code, Delete(BindingClause, "initial"));
        check_rendered(
          "delete ascribed let with list-literal def",
          "type Color = + White + Black in type Board = [[Color]] in ?",
          result,
        );
      },
    ),
    test_case(
      "delete_body on ascribed let",
      `Quick,
      () => {
        let result =
          apply_and_render("let x : Int = 1 in x + 2", Delete(Body, "x"));
        check_rendered(
          "delete body of ascribed x",
          "let x : Int = 1 in ?",
          result,
        );
      },
    ),
    test_case(
      "update_definition on ascribed let preserves ascription",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 1 in x + 1",
            Update(Definition, "x", "42"),
          );
        check_rendered(
          "update def of ascribed x",
          "let x : Int = 42 in x + 1",
          result,
        );
      },
    ),
    test_case(
      "update_binding_clause replaces whole let header incl. ascription",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 1 in x",
            Update(BindingClause, "x", "let x : Float = 1.0 in"),
          );
        check_rendered(
          "update binding clause with new ascription",
          "let x : Float = 1.0 in x",
          result,
        );
      },
    ),
    test_case(
      "insert_before ascribed let",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 1 in x",
            Insert(Before, "x", "let y : Int = 2 in"),
          );
        check_rendered(
          "insert ascribed let before ascribed let",
          "let y : Int = 2 in let x : Int = 1 in x",
          result,
        );
      },
    ),
    test_case(
      "insert_after ascribed let",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 1 in x",
            Insert(After, "x", "let y : Int = 2 in"),
          );
        check_rendered(
          "insert ascribed let after ascribed let",
          "let x : Int = 1 in let y : Int = 2 in x",
          result,
        );
      },
    ),
    test_case(
      "chess-style sequence: delete each type alias and the ascribed let",
      `Quick,
      () => {
        /* Matches the live-reproducer setup: 5 type aliases + an ascribed let
           with a deeply nested list-literal body. Deleting every binding in
           turn should end at an all-holes program. */
        let code = "type Color = + White + Black in type PieceType = + Pawn + Knight + Bishop + Rook + Queen + King in type Piece = (Color, PieceType) in type Square = + Empty + Occupied(Piece) in type Board = [[Square]] in let initial_board : Board = [[Occupied((White, Rook))]] in ?";
        let result =
          apply_chain_render(
            code,
            [
              Delete(BindingClause, "Color"),
              Delete(BindingClause, "PieceType"),
              Delete(BindingClause, "Piece"),
              Delete(BindingClause, "Square"),
              Delete(BindingClause, "Board"),
              Delete(BindingClause, "initial_board"),
            ],
          );
        check_rendered("deleting all bindings leaves hole", "?", result);
      },
    ),
    test_case(
      "verbatim chess program: Piece is present in node map",
      `Quick,
      () => {
        /* Exact program from the live-editor repro. `delete_binding_clause Piece`
           reportedly fails with "Path 'Piece' not found in node map" even though
           `type Piece = (Color, PieceType) in` is clearly present. This test
           reproduces the full program verbatim and asserts every top-level
           binding — including `Piece` — is findable via [path_to_id_opt]. */
        let code = "type Color = + White + Black in type PieceType = + Pawn + Knight + Bishop + Rook + Queen + King in type Piece = (Color, PieceType) in type Square = + Empty + Occupied(Piece) in type Board = [[Square]] in let initial_board : Board = [\n    [Occupied((White, Rook)), Occupied((White, Knight)), Occupied((White, Bishop)), Occupied((White, Queen)), Occupied((White, King)), Occupied((White, Bishop)), Occupied((White, Knight)), Occupied((White, Rook))],\n    [Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn))],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn))],\n    [Occupied((Black, Rook)), Occupied((Black, Knight)), Occupied((Black, Bishop)), Occupied((Black, Queen)), Occupied((Black, King)), Occupied((Black, Bishop)), Occupied((Black, Knight)), Occupied((Black, Rook))]\n] in ?";
        let node_map = build_node_map(code);
        check(
          bool,
          "Color present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "Color") != None,
        );
        check(
          bool,
          "PieceType present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "PieceType") != None,
        );
        check(
          bool,
          "Piece present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "Piece") != None,
        );
        check(
          bool,
          "Square present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "Square") != None,
        );
        check(
          bool,
          "Board present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "Board") != None,
        );
        check(
          bool,
          "initial_board present",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "initial_board") != None,
        );
      },
    ),
    test_case(
      "verbatim chess program: delete_binding_clause Piece succeeds",
      `Quick,
      () => {
        /* Single action repro of the exact user flow: send the chess program,
           ask to delete `Piece`. Should succeed (the binding exists). */
        let code = "type Color = + White + Black in type PieceType = + Pawn + Knight + Bishop + Rook + Queen + King in type Piece = (Color, PieceType) in type Square = + Empty + Occupied(Piece) in type Board = [[Square]] in let initial_board : Board = [\n    [Occupied((White, Rook)), Occupied((White, Knight)), Occupied((White, Bishop)), Occupied((White, Queen)), Occupied((White, King)), Occupied((White, Bishop)), Occupied((White, Knight)), Occupied((White, Rook))],\n    [Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn))],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn))],\n    [Occupied((Black, Rook)), Occupied((Black, Knight)), Occupied((Black, Bishop)), Occupied((Black, Queen)), Occupied((Black, King)), Occupied((Black, Bishop)), Occupied((Black, Knight)), Occupied((Black, Rook))]\n] in ?";
        let result = apply_and_render(code, Delete(BindingClause, "Piece"));
        /* We just assert the action didn't raise / didn't produce the "not
           found" failure string. Any non-failure render is acceptable here —
           this test is about proving the binding is discoverable. */
        check(
          bool,
          "delete Piece did not emit 'not found in node map' failure",
          false,
          Util_web.StringUtil.plain_search("not found in node map", result, 0)
          >= 0,
        );
      },
    ),
    test_case(
      "get_diff after Delete(BindingClause) does not raise when path vanishes",
      `Quick,
      () => {
        /* Regression for the live-editor bug the ascribed-binding suite couldn't
           reach: `apply_and_render` only exercises [[Perform.go]], but the
           agent's diff-rendering path calls [[CompositionGo.Local.get_diff]]
           on (old_zipper, new_zipper) AFTER the delete has succeeded. That
           code used to call [[path_to_id]] on the new node map, which
           (correctly) no longer contains the deleted binding — so every
           successful delete surfaced to the agent as a tool-call failure. */
        let code = "type Color = + White + Black in type Piece = (Color, Color) in let x = 1 in x";
        let old_z = mk_zipper(code);
        switch (
          Perform.go(
            ~settings=CoreSettings.on,
            ~statics=CachedStatics.empty,
            ~syntax=CachedSyntax.init(old_z),
            Structural(Delete(BindingClause, "Piece")),
            {
              zipper: old_z,
              col_target: None,
            },
            ~root=Exp,
          )
        ) {
        | Error(e) =>
          Alcotest.fail("Delete itself failed: " ++ Action.Failure.show(e))
        | Ok(new_z) =>
          let syntax = CachedSyntax.init(old_z);
          let diff =
            CompositionGo.Local.get_diff(
              old_z,
              new_z,
              Delete(BindingClause, "Piece"),
              mk_statics,
              syntax,
            );
          switch (diff) {
          | None => Alcotest.fail("get_diff returned None unexpectedly")
          | Some((_old_seg, new_seg)) =>
            check(
              bool,
              "new_segment is None for BindingClause delete",
              true,
              new_seg == None,
            )
          };
        };
      },
    ),
    test_case(
      "get_diff after Delete(BindingClause) on chess program does not raise",
      `Quick,
      () => {
        /* Same bug, full chess program — matches the exact shape the user
           captured in the live editor. */
        let code = "type Color = + White + Black in type PieceType = + Pawn + Knight in type Piece = (Color, PieceType) in type Square = + Empty + Occupied(Piece) in type Board = [[Square]] in let initial_board : Board = [[Occupied((White, Pawn))]] in ?";
        let old_z = mk_zipper(code);
        switch (
          Perform.go(
            ~settings=CoreSettings.on,
            ~statics=CachedStatics.empty,
            ~syntax=CachedSyntax.init(old_z),
            Structural(Delete(BindingClause, "Piece")),
            {
              zipper: old_z,
              col_target: None,
            },
            ~root=Exp,
          )
        ) {
        | Error(e) =>
          Alcotest.fail("Delete itself failed: " ++ Action.Failure.show(e))
        | Ok(new_z) =>
          let syntax = CachedSyntax.init(old_z);
          /* Before fix, this raised:
               Failure "Path \"Piece\" not found in node map ..."
             Now should return Some((old, None)). */
          let diff =
            CompositionGo.Local.get_diff(
              old_z,
              new_z,
              Delete(BindingClause, "Piece"),
              mk_statics,
              syntax,
            );
          check(bool, "diff computation did not raise", true, diff != None);
        };
      },
    ),
    test_case(
      "place_probe on ascribed recursive fib: add_manual + statics rebuild",
      `Quick,
      () => {
        /* Reproduces the live-editor Exception-during-View captured when
           placing a probe on [fib] in a recursive, ascribed let binding. The
           agent-layer call succeeded in the editor and only the view blew up
           (TypeError on reading length of undefined), so this test only
           validates the agent-side invariants:
             1. path [fib] resolves,
             2. add_manual does not raise,
             3. statics can be rebuilt on the probed zipper,
             4. node_map can be rebuilt on the probed zipper.
           If all four hold, the crash lives downstream in view/eval render. */
        let code = {|let fib : Int -> Int = fun n ->
  if n <= 0
    then 0
    else if n == 1
      then 1
      else fib(n - 1) + fib(n - 2)
in
test fib(0) == 0 end;
test fib(1) == 1 end;
test fib(5) == 5 end;
test fib(10) == 55 end;
fib(10)|};
        let z = mk_zipper(code);
        let info_map = mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None =>
          Alcotest.fail("expected high-level node map for fib program")
        | Some(nm) =>
          switch (HighLevelNodeMap.Public.path_to_id_opt(nm, "fib")) {
          | None => Alcotest.fail("expected path fib to resolve")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.add_manual(~syntax, id, info_map, z);
            let info_map2 = mk_statics(z2);
            check(
              bool,
              "statics rebuild on probed fib is non-empty",
              true,
              Id.Map.cardinal(info_map2) > 0,
            );
            switch (HighLevelNodeMap.build(z2, info_map2)) {
            | None =>
              Alcotest.fail("node_map rebuild returned None after probe")
            | Some(_) => ()
            };
          }
        };
      },
    ),
  ],
);

/* ============================================================
   RENAME VALIDATION + AMBIGUOUS PATH DISAMBIGUATION
   ============================================================ */

let contains_str = (~needle: string, haystack: string): bool => {
  let nl = String.length(needle)
  and hl = String.length(haystack);
  let rec go = i =>
    i + nl <= hl && (String.sub(haystack, i, nl) == needle || go(i + 1));
  nl == 0 || go(0);
};

/** Expect a Composition_action_failure whose message mentions every needle. */
let expect_failure_mentioning =
    (
      code: string,
      a: Action.Structural.t,
      needles: list(string),
      name: string,
    ) => {
  switch (run_agent_action(code, a)) {
  | Ok(_) => Alcotest.fail("Expected failure: " ++ name)
  | Error(Action.Failure.Composition_action_failure(msg)) =>
    List.iter(
      needle =>
        check(
          bool,
          name ++ " mentions \"" ++ needle ++ "\" in: " ++ msg,
          true,
          contains_str(~needle, msg),
        ),
      needles,
    )
  | Error(err) =>
    Alcotest.fail(
      "Unexpected failure kind for "
      ++ name
      ++ ": "
      ++ Action.Failure.show(err),
    )
  };
};

let rename_and_path_safety_tests = (
  "AgentTools.RenameAndPathSafety",
  [
    test_case(
      "sanity: normal rename still works",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a + 1 in b + a",
            Update(Pattern, "a", "q"),
          );
        check_rendered(
          "rename_sanity",
          "let q = 1 in let b = q + 1 in b + q",
          result,
        );
      },
    ),
    test_case(
      "rename to a name shadowed later in scope is rejected", `Quick, ()
      /* Renaming x -> y would make the rewritten use of x in [x + y] be
         captured by the inner [let y = 2]; statics would not flag this. */
      =>
        expect_failure_mentioning(
          "let x = 1 in let y = 2 in x + y",
          Update(Pattern, "x", "y"),
          ["already occurs", "\"y\""],
          "rename_to_shadowing_name",
        )
      ),
    test_case(
      "rename to an outer name referenced in scope is rejected", `Quick, ()
      /* Renaming b -> a would make the existing reference to outer [a] in
         [a + b] resolve to the renamed binding instead (capture). */
      =>
        expect_failure_mentioning(
          "let a = 1 in let b = 2 in a + b",
          Update(Pattern, "b", "a"),
          ["already occurs", "\"a\""],
          "rename_captures_outer_reference",
        )
      ),
    test_case("tuple-pattern arity change is rejected", `Quick, ()
      /* Old binds 2 names, new binds 3: old->new use-site mapping is
         ambiguous, so this must hard-error instead of silently leaving
         stale references. Definition is a hole so the pattern itself
         stays statically OK and the arity check is what fires. */
      =>
        expect_failure_mentioning(
          "let (x, y) = ? in x + y",
          Update(Pattern, "(x, y)", "(a, b, c)"),
          ["Cannot rewrite use sites", "binds 2 name(s)", "binds 3"],
          "rename_tuple_arity_mismatch",
        )
      ),
    test_case(
      "naming a hole pattern is allowed (binds 0 names before)",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let ? = 5 in 3",
            Update(Pattern, "{empty pattern hole}", "x"),
          );
        check_rendered("rename_hole_pattern", "let x = 5 in 3", result);
      },
    ),
    test_case(
      "ambiguous path to edit tool yields disambiguation error", `Quick, () =>
      expect_failure_mentioning(
        "let x = 1 in let x = 2 in x",
        Update(Definition, "x", "9"),
        ["ambiguous", "\"x#1\"", "\"x#2\""],
        "ambiguous_path_edit",
      )
    ),
    test_case(
      "#k-disambiguated path targets the later binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x = 1 in let x = 2 in x",
            Update(Definition, "x#2", "9"),
          );
        check_rendered(
          "disambiguated_edit",
          "let x = 1 in let x = 9 in x",
          result,
        );
      },
    ),
    test_case(
      "node map: bare duplicate is None; #k resolves in program order",
      `Quick,
      () => {
        let node_map = build_node_map("let x = 1 in let x = 2 in x");
        check(
          bool,
          "bare shadowed path is None",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "x") == None,
        );
        let id1 = HighLevelNodeMap.path_to_id_opt(node_map, "x#1");
        let id2 = HighLevelNodeMap.path_to_id_opt(node_map, "x#2");
        check(bool, "x#1 resolves", true, id1 != None);
        check(bool, "x#2 resolves", true, id2 != None);
        check(bool, "x#1 and x#2 differ", true, id1 != id2);
        switch (id1, id2) {
        | (Some(id1), Some(id2)) =>
          check(
            int,
            "x#1 is earliest (sibling_idx 0)",
            0,
            HighLevelNodeMap.find(node_map, id1).sibling_idx,
          );
          check(
            int,
            "x#2 is second (sibling_idx 1)",
            1,
            HighLevelNodeMap.find(node_map, id2).sibling_idx,
          );
        | _ => ()
        };
        check(
          bool,
          "out-of-range occurrence is None",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "x#3") == None,
        );
      },
    ),
    test_case(
      "node map: path_to_id raises listing disambiguated paths",
      `Quick,
      () => {
        let node_map = build_node_map("let x = 1 in let x = 2 in x");
        switch (HighLevelNodeMap.path_to_id(node_map, "x")) {
        | _ => Alcotest.fail("expected ambiguity failure for bare \"x\"")
        | exception (Failure(msg)) =>
          check(
            bool,
            "message flags ambiguity: " ++ msg,
            true,
            contains_str(~needle="ambiguous", msg),
          );
          check(
            bool,
            "message lists x#1: " ++ msg,
            true,
            contains_str(~needle="\"x#1\"", msg),
          );
          check(
            bool,
            "message lists x#2: " ++ msg,
            true,
            contains_str(~needle="\"x#2\"", msg),
          );
        };
      },
    ),
    test_case(
      "unique path still resolves without #k",
      `Quick,
      () => {
        let node_map = build_node_map("let a = 1 in let b = 2 in a + b");
        check(
          bool,
          "unique path resolves",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "b") != None,
        );
      },
    ),
  ],
);

/* ============================================================
   PASTE FUNNEL — per-line indentation trim + reserved keywords
   ============================================================ */

let paste_funnel_tests = (
  "AgentTools.PasteFunnel",
  [
    test_case(
      "update_definition strips per-line leading indentation", `Quick, () => {
      check_rendered_exact(
        "update_definition_indented",
        "let a = fun x ->\n  x + 1 in a",
        apply_and_render(
          "let a = 1 in a",
          Update(Definition, "a", "fun x ->\n  x + 1"),
        ),
      )
    }),
    test_case("update_body strips per-line leading indentation", `Quick, () => {
      check_rendered_exact(
        "update_body_indented",
        "let a = 1 in let b = 2 in\na + b",
        apply_and_render(
          "let a = 1 in a",
          Update(Body, "a", "let b = 2 in\n  a + b"),
        ),
      )
    }),
    test_case("update_pattern strips leading indentation", `Quick, () => {
      check_rendered_exact(
        "update_pattern_indented",
        "let b = 1 in b",
        apply_and_render("let a = 1 in a", Update(Pattern, "a", "  b")),
      )
    }),
    test_case(
      "update_binding_clause strips per-line leading indentation", `Quick, () => {
      check_rendered_exact(
        "update_binding_clause_indented",
        "let a =\n  2 in a",
        apply_and_render(
          "let a = 1 in a",
          Update(BindingClause, "a", "let a =\n  2 in"),
        ),
      )
    }),
    test_case("Perform-level insert strips leading indentation", `Quick, () => {
      /* Direct Structural action, bypassing the tool-arg parsing layer:
         the trim must live at the paste funnel, not per tool arm */
      check_rendered_exact(
        "insert_perform_level_indented",
        "let a = 1 in\n\nlet c = 3 in\n a",
        apply_and_render(
          "let a = 1 in a",
          Insert(After, "a", "  let c = 3 in"),
        ),
      )
    }),
    test_case("insert of `let eval = ...` fails naming keyword", `Quick, () => {
      expect_failure_mentioning(
        "let a = 1 in a",
        Insert(After, "a", "let eval = 1 in"),
        [
          "Note: `eval` is a reserved keyword in Hazel and cannot be used as a variable name.",
        ],
        "insert_let_eval",
      )
    }),
    test_case("insert of `let evalStep = ...` succeeds", `Quick, () => {
      check_rendered(
        "insert_let_evalStep",
        "let a = 1 in let evalStep = 1 in a",
        apply_and_render(
          "let a = 1 in a",
          Insert(After, "a", "let evalStep = 1 in"),
        ),
      )
    }),
    test_case("unannotated binding without self-ref succeeds", `Quick, () => {
      check_rendered(
        "insert_unannotated_no_self_ref",
        "let a = 1 in let f = fun n -> n(0) in a",
        apply_and_render(
          "let a = 1 in a",
          Insert(After, "a", "let f = fun n -> n(0) in"),
        ),
      )
    }),
    test_case("unannotated self-reference is accepted", `Quick, () => {
      /* Hazel statics scope the binder over its own definition even
         without a type annotation, so this is not an unbound-name error */
      check_rendered(
        "insert_unannotated_self_ref",
        "let a = 1 in let f = fun n -> f(0) in a",
        apply_and_render(
          "let a = 1 in a",
          Insert(After, "a", "let f = fun n -> f(0) in"),
        ),
      )
    }),
    test_case("annotated recursion succeeds", `Quick, () => {
      check_rendered(
        "insert_annotated_recursion",
        "let a = 1 in let f : Int -> Int = fun n -> if n < 1 then 0 else f(n - 1) in a",
        apply_and_render(
          "let a = 1 in a",
          Insert(
            After,
            "a",
            "let f : Int -> Int = fun n -> if n < 1 then 0 else f(n - 1) in",
          ),
        ),
      )
    }),
    test_case(
      "update_body with annotated `let eval` fails naming keyword", `Quick, () => {
      expect_failure_mentioning(
        "let a = 1 in a",
        Update(Body, "a", "let eval : Int -> Int = fun t -> t in ?"),
        ["`eval` is a reserved keyword"],
        "update_body_let_eval",
      )
    }),
    test_case(
      "keyword note absent when no reserved word is misused", `Quick, () => {
      switch (
        run_agent_action(
          "let a = 1 in a",
          Insert(After, "a", "let b = unboundvar in"),
        )
      ) {
      | Ok(_) => Alcotest.fail("expected static-error failure")
      | Error(Action.Failure.Composition_action_failure(msg)) =>
        check(
          bool,
          "no keyword note in: " ++ msg,
          false,
          contains_str(~needle="reserved keyword", msg),
        )
      | Error(err) =>
        Alcotest.fail("unexpected failure: " ++ Action.Failure.show(err))
      }
    }),
  ],
);

/* ============================================================
   FN-DEFINITION SUGAR — bare-name paths + tool coverage
   `let f(x, y) = ...` binds pattern Ap(Var f, args); the binding is
   addressed by the bare head name, with #k on collisions.
   ============================================================ */

let fn_sugar_tests = (
  "AgentTools.FnSugar",
  [
    test_case(
      "sugared fn is addressed by bare head name",
      `Quick,
      () => {
        let node_map =
          build_node_map("let contains(xs, y) = 1 in contains([], 2)");
        let id = HighLevelNodeMap.path_to_id(node_map, "contains");
        check(
          string,
          "name is bare head",
          "contains",
          HighLevelNodeMap.id_to_name(node_map, id),
        );
        check(
          bool,
          "full rendered pattern is not a path",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "contains((xs, y))")
          == None,
        );
      },
    ),
    test_case(
      "one-param, zero-param, and return-annotated sugar use bare name",
      `Quick,
      () => {
        let nm1 = build_node_map("let inc(x) = x + 1 in inc(1)");
        check(
          bool,
          "one-param path",
          true,
          HighLevelNodeMap.path_to_id_opt(nm1, "inc") != None,
        );
        let nm0 = build_node_map("let f() = 3 in f()");
        check(
          bool,
          "zero-param path",
          true,
          HighLevelNodeMap.path_to_id_opt(nm0, "f") != None,
        );
        let nm_asc = build_node_map("let g(x): Int = x in g(1)");
        check(
          bool,
          "return-annotated path",
          true,
          HighLevelNodeMap.path_to_id_opt(nm_asc, "g") != None,
        );
      },
    ),
    test_case(
      "sugared fn + plain sibling of same name is ambiguous; #k retries",
      `Quick,
      () => {
        let node_map = build_node_map("let f(x) = x in let f = 1 in f");
        check(
          bool,
          "bare shared name is None",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "f") == None,
        );
        check(
          bool,
          "f#1 resolves",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "f#1") != None,
        );
        check(
          bool,
          "f#2 resolves",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "f#2") != None,
        );
        expect_failure_mentioning(
          "let f(x) = x in let f = 1 in f",
          Update(Definition, "f", "9"),
          ["ambiguous", "\"f#1\"", "\"f#2\""],
          "fn_sugar_ambiguous_edit",
        );
      },
    ),
    test_case(
      "#k-disambiguated edit targets the plain sibling, not the fn",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f(x) = x in let f = 1 in f",
            Update(Definition, "f#2", "9"),
          );
        check_rendered(
          "fn_sugar_hash_k_edit",
          "let f(x) = x in let f = 9 in f",
          result,
        );
      },
    ),
    test_case(
      "update_definition replaces RHS of sugared fn",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let inc(x) = x + 1 in inc(3)",
            Update(Definition, "inc", "x + 2"),
          );
        check_rendered(
          "fn_sugar_update_def",
          "let inc(x) = x + 2 in inc(3)",
          result,
        );
      },
    ),
    test_case(
      "update_body on sugared fn",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let inc(x) = x + 1 in inc(3)",
            Update(Body, "inc", "inc(10)"),
          );
        check_rendered(
          "fn_sugar_update_body",
          "let inc(x) = x + 1 in inc(10)",
          result,
        );
      },
    ),
    test_case(
      "update_binding_clause on sugared fn",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let inc(x) = x + 1 in inc(3)",
            Update(BindingClause, "inc", "let inc(x) = x * 2 in"),
          );
        check_rendered(
          "fn_sugar_update_clause",
          "let inc(x) = x * 2 in inc(3)",
          result,
        );
      },
    ),
    test_case(
      "delete_binding_clause on sugared fn",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let inc(x) = x + 1 in a",
            Delete(BindingClause, "inc"),
          );
        check_rendered("fn_sugar_delete_clause", "let a = 1 in a", result);
      },
    ),
    test_case(
      "update_pattern renames fn keeping params; call sites update",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let inc(x) = x + 1 in inc(3) + inc(4)",
            Update(Pattern, "inc", "bump(x)"),
          );
        check_rendered(
          "fn_sugar_rename_fn",
          "let bump(x) = x + 1 in bump(3) + bump(4)",
          result,
        );
      },
    ),
    test_case(
      "update_pattern renames recursive sugared fn incl. recursive call",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let fact(n) = if n < 2 then 1 else n * fact(n - 1) in fact(4)",
            Update(Pattern, "fact", "f(n)"),
          );
        check_rendered(
          "fn_sugar_rename_recursive",
          "let f(n) = if n < 2 then 1 else n * f(n - 1) in f(4)",
          result,
        );
      },
    ),
    test_case(
      "update_pattern renames a param only",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let inc(x) = x + 1 in inc(3)",
            Update(Pattern, "inc", "inc(z)"),
          );
        check_rendered(
          "fn_sugar_rename_param",
          "let inc(z) = z + 1 in inc(3)",
          result,
        );
      },
    ),
    test_case(
      "param rename leaves same-named outer var in body untouched",
      `Quick,
      () => {
        /* body `+ x` refers to the outer x, not the param */
        let result =
          apply_and_render(
            "let x = 1 in let f(x) = x + 1 in f(2) + x",
            Update(Pattern, "f", "f(z)"),
          );
        check_rendered(
          "fn_sugar_param_rename_no_capture",
          "let x = 1 in let f(z) = z + 1 in f(2) + x",
          result,
        );
      },
    ),
    test_case(
      "param-count change on sugared fn is rejected with bound-name counts",
      `Quick,
      ()
      /* old pattern binds inc,x = 2 names; new binds inc,a,b = 3 */
      =>
        expect_failure_mentioning(
          "let inc(x) = x + 1 in inc(3)",
          Update(Pattern, "inc", "inc(a, b)"),
          ["Cannot rewrite use sites", "binds 2 name(s)", "binds 3"],
          "fn_sugar_param_arity_mismatch",
        )
      ),
    test_case(
      "place_probe on sugared fn path",
      `Quick,
      () => {
        let code = "let f(x) = x + 1 in f(2)";
        let z = mk_zipper(code);
        let info_map = mk_statics(z);
        switch (HighLevelNodeMap.build(z, info_map)) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (HighLevelNodeMap.Public.path_to_id_opt(nm, "f")) {
          | None => Alcotest.fail("expected path f")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.add_manual(~syntax, id, info_map, z);
            switch (ProbePerform.probe_status(id, info_map, z2.refractors)) {
            | Manual(_) => ()
            | _ => Alcotest.fail("expected Manual probe on sugared fn")
            };
          }
        };
      },
    ),
    test_case(
      "syntax projector target of sugared fn is its definition",
      `Quick,
      () => {
        let node_map = build_node_map("let f(x) = x + 1 in f(2)");
        check(
          bool,
          "def target resolves",
          true,
          HighLevelNodeMap.path_to_syntax_projector_target_id_opt(
            node_map,
            "f",
          )
          != None,
        );
      },
    ),
  ],
);

/* ============================================================
   VERTICAL WHITESPACE NORMALIZATION (agent edits)
   ============================================================ */

let leading_newline = (s: string): bool =>
  String.length(s) > 0 && s.[0] == '\n';

let trailing_newlines = (s: string): int => {
  let rec go = (i, acc) =>
    i >= 0 && s.[i] == '\n' ? go(i - 1, acc + 1) : acc;
  go(String.length(s) - 1, 0);
};

let contains_sub = (haystack: string, needle: string): bool => {
  let (hl, nl) = (String.length(haystack), String.length(needle));
  let rec go = i =>
    i + nl <= hl && (String.sub(haystack, i, nl) == needle || go(i + 1));
  nl == 0 || go(0);
};

let whitespace_normalization_tests = (
  "WhitespaceNormalization",
  [
    test_case(
      "trailing linebreaks do not accumulate over chained edits",
      `Quick,
      () => {
        /* Live-session symptom: blank lines grew at program end as edits
           near it repeated (each insert's magic-newline wrap left a \n).
           (update_body over a hole adds no linebreaks — space-pad only —
           so the accumulation shape is insert-driven.) */
        let rendered =
          apply_chain_render(
            "let a = 1 in ?",
            [
              Insert(After, "a", "let b = 2 in"),
              Insert(After, "b", "let c = 3 in"),
              Insert(After, "c", "let d = 4 in"),
            ],
          );
        check(
          bool,
          "at most one trailing linebreak",
          true,
          trailing_newlines(rendered) <= 1,
        );
        check_rendered_exact(
          "chained insert_after spacing",
          "let a = 1 in\n\nlet b = 2 in\n\nlet c = 3 in\n\nlet d = 4 in\n ?",
          rendered,
        );
      },
    ),
    test_case("prepend leaves no leading blank line", `Quick, () => {
      switch (run_insert_at_program_boundary("1", Before, "let a = 2 in")) {
      | Ok(z) =>
        let rendered = render_zipper(z);
        check(bool, "no leading newline", false, leading_newline(rendered));
        check_rendered_exact("prepend spacing", "let a = 2 in\n1", rendered);
      | Error(err) =>
        Alcotest.fail("prepend failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case(
      "one blank line between consecutive top-level bindings", `Quick, () => {
      check_rendered_exact(
        "inter-binding blank line",
        "let a = 1 in let b = 2 in\n\nlet c = 3 in\n a + b",
        apply_and_render(
          "let a = 1 in let b = 2 in a + b",
          Insert(After, "b", "let c = 3 in"),
        ),
      )
    }),
    test_case(
      "normalization is idempotent across edits",
      `Quick,
      () => {
        check_rendered_exact(
          "one edit",
          "let a = 5 in\n\nlet b = 2 in a",
          apply_and_render(
            "let a = 1 in\n\nlet b = 2 in a",
            Update(Definition, "a", "5"),
          ),
        );
        check_rendered_exact(
          "two edits, spacing stable",
          "let a = 6 in\n\nlet b = 2 in a",
          apply_chain_render(
            "let a = 1 in\n\nlet b = 2 in a",
            [Update(Definition, "a", "5"), Update(Definition, "a", "6")],
          ),
        );
      },
    ),
    test_case(
      "top-level comment secondaries are preserved",
      `Quick,
      () => {
        let rendered =
          apply_and_render(
            "let a = 1 in\n# note #\nlet b = 2 in a",
            Update(Definition, "a", "5"),
          );
        check(
          bool,
          "comment survives",
          true,
          contains_sub(rendered, "# note #"),
        );
      },
    ),
  ],
);

/* ============================================================
   AGGREGATE ALL TESTS
   ============================================================ */

let tests = [
  whitespace_normalization_tests,
  paste_funnel_tests,
  edit_action_tests,
  insert_at_program_boundary_tests,
  update_definition_tests,
  update_body_tests,
  update_pattern_tests,
  update_binding_clause_tests,
  insert_tests,
  delete_tests,
  static_error_tests,
  nested_definition_tests,
  invalid_path_tests,
  high_level_node_map_tests,
  composition_view_print_tests,
  composition_utils_tests,
  statics_refractor_tests,
  agent_tools_with_projectors_tests,
  general_tree_refs_tests,
  sequential_operations_tests,
  type_alias_tests,
  complex_program_tests,
  agent_context_tests,
  error_print_tests,
  tool_json_tests,
  ascribed_binding_tests,
  rename_and_path_safety_tests,
  fn_sugar_tests,
];
