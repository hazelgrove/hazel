open Alcotest;
open Haz3lcore;
open Language;
open Action;
open CompositionActions;
open Util;

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

/* Check that an edit succeeds but produces a warning */
let expect_warning = (code: string, a: Action.Structural.t, name: string) => {
  switch (run_agent_action(code, a)) {
  | Ok(_) =>
    switch (CompositionGo.Public.last_warning^) {
    | Some(_) => () /* Warning produced, as expected */
    | None => Alcotest.fail("Expected warning but got none: " ++ name)
    }
  | Error(err) =>
    Alcotest.fail(
      "Expected success with warning for "
      ++ name
      ++ ", but got error: "
      ++ Action.Failure.show(err),
    )
  };
};

/* Concise test helper for edit actions */
let edit_test = (name, code, action, expected) =>
  test_case(name, `Quick, () =>
    check_rendered(name, expected, apply_and_render(code, action))
  );

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
    edit_test(
      "update_definition",
      "let a = 1 in a",
      Update(Definition, "a", "2"),
      "let a = 2 in a",
    ),
    /* Replace compound def with simple: trailing secondary of BinOp
       root is empty (lives on rightmost child), must not lose space. */
    edit_test(
      "update_definition: compound to simple",
      "let a = x + 1 in a",
      Update(Definition, "a", "42"),
      "let a = 42 in a",
    ),
    edit_test(
      "update_body",
      "let a = 1 in let b = 2 in a + b",
      Update(Body, "b", "b + 1"),
      "let a = 1 in let b = 2 in b + 1",
    ),
    edit_test(
      "update_pattern renames uses",
      "let a = 1 in let b = a + 1 in b + a",
      Update(Pattern, "a", "x"),
      "let x = 1 in let b = x + 1 in b + x",
    ),
    edit_test(
      "update_pattern renames in def",
      "let x = 3 in let y = x in y",
      Update(Pattern, "x", "z"),
      "let z = 3 in let y = z in y",
    ),
    edit_test(
      "update_binding_clause",
      "let a = 1 in let b = 2 in a + b",
      Update(BindingClause, "b", "let b = a + 2 in"),
      "let a = 1 in let b = a + 2 in a + b",
    ),
    edit_test(
      "insert_before",
      "let a = 1 in let b = 2 in a + b",
      Insert(Before, "b", "let x = a in"),
      "let a = 1 in let x = a in let b = 2 in a + b",
    ),
    edit_test(
      "insert_after",
      "let a = 1 in let b = 2 in a + b",
      Insert(After, "a", "let x = a in"),
      "let a = 1 in let x = a in let b = 2 in a + b",
    ),
    edit_test(
      "delete_binding_clause",
      "let a = 1 in let b = 2 in let c = 3 in a + c",
      Delete(BindingClause, "b"),
      "let a = 1 in let c = 3 in a + c",
    ),
    edit_test(
      "delete_body",
      "let a = 1 in let b = 2 in a + b",
      Delete(Body, "b"),
      "let a = 1 in let b = 2 in ?",
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
    test_case("update_definition warns on type mismatch", `Quick, () => {
      expect_warning(
        "let a : Int = 1 in a + 1",
        Update(Definition, "a", "true"),
        "type_mismatch_def",
      )
    }),
    test_case("update_definition warns on unbound variable", `Quick, () => {
      expect_warning(
        "let a = 1 in a",
        Update(Definition, "a", "nonexistent_var"),
        "unbound_var_def",
      )
    }),
    test_case("insert_before warns on ill-typed binding", `Quick, () => {
      expect_warning(
        "let a = 1 in a",
        Insert(Before, "a", "let x : Int = true in"),
        "insert_before_type_error",
      )
    }),
    test_case("insert_after warns on ill-typed binding", `Quick, () => {
      expect_warning(
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
          Util.StringUtil.plain_search("Available paths", msg, 0) >= 0,
        );
        check(
          bool,
          "error lists binding 'a'",
          true,
          Util.StringUtil.plain_search("a", msg, 0) >= 0,
        );
        check(
          bool,
          "error lists binding 'b'",
          true,
          Util.StringUtil.plain_search("b", msg, 0) >= 0,
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
          Util.StringUtil.plain_search("outer/inner", msg, 0) >= 0,
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
        /* The node map also indexes sequence elements ((k) / [k]);
           this test is about the named bindings. */
        let all_names =
          Id.Map.bindings(node_map)
          |> List.map(((_, n: HighLevelNodeMap.node)) => n.name)
          |> List.filter(name => name != "" && name.[0] != '(' && name.[0] != '[')
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

let module_node_map_tests = (
  "HighLevelNodeMap.Modules",
  [
    test_case(
      "module items appear as children of parent let",
      `Quick,
      () => {
        let node_map =
          build_node_map("let m = { let a = 1; let b = 2 } in m.a + m.b");
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
          build_node_map("let m = { let a = 1; let b = 2 } in m.a + m.b");
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
          build_node_map("let m = { let a = 1; let b = 2 } in m.a");
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
        check_rendered(
          "update via #1",
          "let a = 1 in let b = 42 in a + b",
          result,
        );
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
    edit_test(
      "nested def update",
      "let a = let inner = 1 in inner in a",
      Update(Definition, "a/inner", "42"),
      "let a = let inner = 42 in inner in a",
    ),
    edit_test(
      "type alias def update",
      "type T = Int in let x = 1 in x",
      Update(Definition, "T", "Bool"),
      "type T = Bool in let x = 1 in x",
    ),
    test_case("bad path gives error", `Quick, () =>
      expect_composition_failure(
        "let a = 1 in let b = 2 in a + b",
        Update(Definition, "nonexistent", "42"),
        "bad path",
      )
    ),
    test_case("index out of range", `Quick, () =>
      expect_composition_failure(
        "let a = 1 in let b = 2 in a + b",
        Update(Definition, "#99", "42"),
        "out of range",
      )
    ),
    edit_test(
      "preserves surrounding",
      "let a = 1 in let b = 2 in let c = 3 in a + b + c",
      Update(Definition, "b", "a * 10"),
      "let a = 1 in let b = a * 10 in let c = 3 in a + b + c",
    ),
    test_case("type error warned", `Quick, () =>
      expect_warning(
        "let a : Int = 1 in a + 1",
        Update(Definition, "a", "true"),
        "type error",
      )
    ),
    /* Multi-step refactoring: changing a type alias cascades type errors
       to dependents. Previously this was rejected; now it succeeds with
       a warning so the agent can fix dependents in a follow-up edit. */
    test_case("type alias cascade warned", `Quick, () =>
      expect_warning(
        "type T = Int in let x : T = 5 in x",
        Update(Definition, "T", "Bool"),
        "type alias cascade",
      )
    ),
    edit_test(
      "type alias cascade code correct",
      "type T = Int in let x : T = 5 in x",
      Update(Definition, "T", "Bool"),
      "type T = Bool in let x : T = 5 in x",
    ),
    test_case("unmatched delimiter", `Quick, () =>
      expect_composition_failure(
        "let a = 1 in a",
        Update(Definition, "a", "if true then 1"),
        "parse error",
      )
    ),
    test_case("invalid token warned", `Quick, () =>
      expect_warning(
        "let a = 1 in let b = 2 in a + b",
        Insert(After, "a", "let c = $invalid"),
        "invalid token",
      )
    ),
    edit_test(
      "$ path edit",
      "let a = 1 in let b = 2 in let c = 3 in a + b + c",
      Update(Definition, "$", "0"),
      "let a = 1 in let b = 2 in let c = 0 in a + b + c",
    ),
    test_case(
      "#n matches name-based path",
      `Quick,
      () => {
        /* #0 and name "a" should resolve to the same node */
        let node_map = build_node_map("let a = 1 in let b = 2 in a + b");
        let id_by_name = HighLevelNodeMap.path_to_id(node_map, "a");
        let id_by_index = HighLevelNodeMap.path_to_id(node_map, "#0");
        check(bool, "#0 == a", true, Id.equal(id_by_name, id_by_index));
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

/* Concise test helper for read actions */
let read_test = (name, code, action, expected) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, run_read_action(code, action))
  );

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
        check(
          bool,
          "contains let a",
          true,
          string_contains("let a", result),
        );
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
          run_read_action("let x : Int = 1 + 2 in x", GetStatics("x"));
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
          run_read_action("let x : Int = true in x", GetStatics("x"));
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
    edit_test(
      "update type annotation",
      "let x : Int = 5 in x + 1",
      Update(TypeAnnotation, "x", "Float"),
      "let x : Float = 5 in x + 1",
    ),
    edit_test(
      "preserves other bindings",
      "let a : Int = 1 in let b : Bool = true in a",
      Update(TypeAnnotation, "a", "Float"),
      "let a : Float = 1 in let b : Bool = true in a",
    ),
    edit_test(
      "nested type annotation",
      "let a = let inner : Int = 1 in inner in a",
      Update(TypeAnnotation, "a/inner", "Float"),
      "let a = let inner : Float = 1 in inner in a",
    ),
    test_case("fails on unannotated binding", `Quick, () =>
      expect_composition_failure(
        "let x = 5 in x + 1",
        Update(TypeAnnotation, "x", "Int"),
        "no annotation error",
      )
    ),
    edit_test(
      "type alias via TypeAnnotation",
      "type T = Int in let x = 1 in x",
      Update(TypeAnnotation, "T", "Bool"),
      "type T = Bool in let x = 1 in x",
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
        let node_map = build_node_map("let x = 1 in test x == 1 end; x");
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
        let node_map = build_node_map("let x = 1 in test x == 1 end; x");
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
          build_node_map("let x = 1 in test x == 1 end; test x > 0 end; x");
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
          build_node_map("let x = 1 in test x == 1 end; let y = x + 1 in y");
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
        let node_map = build_node_map("let x = 1 in test x == 1 end; x");
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

/* Check if haystack contains needle as a substring */
let str_contains = (haystack: string, needle: string): bool => {
  let hlen = String.length(haystack);
  let nlen = String.length(needle);
  if (nlen > hlen) {
    false;
  } else {
    let found = ref(false);
    for (i in 0 to hlen - nlen) {
      if (String.sub(haystack, i, nlen) == needle) {
        found := true;
      };
    };
    found^;
  };
};

let mk_term = (code: string): Exp.t => {
  let z = mk_zipper(code);
  MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
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

/* Concise test helpers for common patterns */
let sel_test = (~name, ~code, ~sel, ~expected) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, selector_query_unique(code, sel))
  );

let sel_test_rendered = (~name, ~code, ~sel, ~expected) =>
  test_case(name, `Quick, () =>
    check_rendered(name, expected, selector_query_unique(code, sel))
  );

let if_program = "if true then 1 else 0";
let let_fun_if = "let f = fun x -> if x > 0 then x else 0 in f 5";
let case_program = "case x | A => 1 | B => 2 end";
let case_msg = "case msg | Increment => count + 1 | Decrement => count - 1 end";

/* Realistic program for compositionality testing: module with nested
   functions, case expressions, tests, and a top-level let chain */
let app_program =
  "module App = { "
  ++ "let init = 0; "
  ++ "let update = fun msg -> case msg | Inc => msg + 1 | Dec => msg - 1 | Reset => 0 end; "
  ++ "let view = fun model -> let label = model + 1 in (label, model); "
  ++ "test update(Inc, 0) == 1 end "
  ++ "} in "
  ++ "let result = App.update(Inc, App.init) in "
  ++ "result";

/* Multi-module program for spec section 11.2 */
let multi_module_program =
  "module Types = { "
  ++ "type point = (Int, Int); "
  ++ "type color = (Int, Int, Int) "
  ++ "} in "
  ++ "module Geom = { "
  ++ "let origin = (0, 0); "
  ++ "let translate = fun p -> fun dx -> "
  ++ "let x = p.0 + dx in "
  ++ "let y = p.1 in "
  ++ "(x, y); "
  ++ "module Shapes = { "
  ++ "let circle = fun center -> fun radius -> (center, radius); "
  ++ "let rect = fun tl -> fun br -> (tl, br) "
  ++ "} "
  ++ "} in "
  ++ "module Render = { "
  ++ "let draw = fun shape -> fun color -> "
  ++ "if shape.1 > 0 then color else (0, 0, 0) "
  ++ "} in "
  ++ "let canvas = Render.draw(Geom.Shapes.circle(Geom.origin)(5))((255, 0, 0)) in "
  ++ "canvas";

/* Data processing pipeline for spec section 11.3 */
let pipeline_program =
  "type status = +Active +Inactive +Pending in "
  ++ "let users = [(\"Alice\", Active), (\"Bob\", Inactive), (\"Carol\", Pending)] in "
  ++ "let is_active = fun user -> case user.1 "
  ++ "| Active => true "
  ++ "| Inactive => false "
  ++ "| Pending => false "
  ++ "end in "
  ++ "let count : [?] -> Int = fun xs -> case xs "
  ++ "| [] => 0 "
  ++ "| _ :: tl => 1 + count(tl) "
  ++ "end in "
  ++ "let result = count(users) in "
  ++ "result";

let selector_tests = (
  "AgentTools.Selectors",
  [
    /* Let spine */
    sel_test(
      ~name="let x = %",
      ~code="let x = 42 in x",
      ~sel="let x = %",
      ~expected="42",
    ),
    sel_test(
      ~name="let x _... in %",
      ~code="let x = 42 in x + 1",
      ~sel="let x _... in %",
      ~expected="x + 1",
    ),
    sel_test(
      ~name="let b = % nested",
      ~code="let a = 1 in let b = 2 in a + b",
      ~sel="let b = %",
      ~expected="2",
    ),
    /* Binder chain */
    sel_test(
      ~name="m/x = %",
      ~code="let m = { let x = 42 } in m.x",
      ~sel="m/x = %",
      ~expected="42",
    ),
    /* If spine */
    sel_test(~name="if %", ~code=if_program, ~sel="if %", ~expected="true"),
    sel_test(
      ~name="if _ then %",
      ~code=if_program,
      ~sel="if _ then %",
      ~expected="1",
    ),
    sel_test(
      ~name="if _... else %",
      ~code=if_program,
      ~sel="if _... else %",
      ~expected="0",
    ),
    /* Descendant search */
    sel_test(
      ~name="descend if then",
      ~code=let_fun_if,
      ~sel="let f = \\... if _ then %",
      ~expected="x",
    ),
    /* Case/match spine */
    sel_test(
      ~name="case %",
      ~code=case_program,
      ~sel="case %",
      ~expected="x",
    ),
    sel_test(
      ~name="| B => %",
      ~code=case_program,
      ~sel="| B => %",
      ~expected="2",
    ),
    /* Wildcard arm matching: | _ => * matches any single arm body */
    test_case(
      "| _ => % matches all arm bodies",
      `Quick,
      () => {
        let results = selector_query(case_program, "| _ => %");
        check(int, "match count", 2, List.length(results));
      },
    ),
    sel_test(
      ~name="| _ => % (3 arms)",
      ~code="case x | A => 1 | B => 2 | C => 3 end",
      ~sel="case _... | C => %",
      ~expected="3",
    ),
    /* Wildcard arm with continuation: | _ => <walk> */
    test_case(
      "\\... | _ => % returns all arm bodies via descend",
      `Quick,
      () => {
        let results =
          selector_query(
            "let f = fun x -> case x | A => 1 | B => 2 end in f 0",
            "\\... | _ => %",
          );
        check(int, "match count", 2, List.length(results));
      },
    ),
    /* Ellipsis in arms: | _... <name> => * */
    sel_test(
      ~name="| _... Decrement => %",
      ~code=case_msg,
      ~sel="| _... Decrement => %",
      ~expected="count - 1",
    ),
    /* No match */
    test_case(
      "no match returns error",
      `Quick,
      () => {
        let result = selector_query_unique("let x = 1 in x", "let y = %");
        check(
          bool,
          "starts with ERROR",
          true,
          String.length(result) > 5 && String.sub(result, 0, 5) == "ERROR",
        );
      },
    ),
    /* Multiple matches */
    test_case(
      "query returns multiple matches",
      `Quick,
      () => {
        let results =
          selector_query("let a = 1 in let b = 2 in a + b", "let _ = %");
        check(int, "match count", 2, List.length(results));
      },
    ),
    /* Read action integration */
    read_test(
      "Select def",
      "let x = 42 in x + 1",
      Select("let x = %"),
      "42",
    ),
    read_test(
      "Select descend",
      let_fun_if,
      Select("let f = \\... if _... else %"),
      "0",
    ),
    read_test(
      "Select chain",
      "let m = { let x = 42 } in m.x",
      Select("m/x = %"),
      "42",
    ),
    test_case(
      "Select multiple matches",
      `Quick,
      () => {
        let result =
          run_read_action(
            "let a = 1 in let b = 2 in a + b",
            Select("let _ = %"),
          );
        let lines =
          result
          |> String.split_on_char('\n')
          |> List.filter(s => String.length(String.trim(s)) > 0);
        check(int, "line count", 2, List.length(lines));
      },
    ),
    /* Spec examples */
    sel_test(
      ~name="spec: descend if %",
      ~code=let_fun_if,
      ~sel="let f = \\... if %",
      ~expected="x > 0",
    ),
    sel_test(
      ~name="spec: descend if _ then %",
      ~code=let_fun_if,
      ~sel="let f = \\... if _ then %",
      ~expected="x",
    ),
    test_case(
      "spec: % let f",
      `Quick,
      () => {
        let result = selector_query_unique(let_fun_if, "% let f");
        check(
          bool,
          "not error",
          false,
          String.length(result) > 5 && String.sub(result, 0, 5) == "ERROR",
        );
      },
    ),
    /* Case arms */
    sel_test(
      ~name="spec: | Increment => %",
      ~code=case_msg,
      ~sel="| Increment => %",
      ~expected="count + 1",
    ),
    sel_test(
      ~name="spec: | Decrement => %",
      ~code=case_msg,
      ~sel="| Decrement => %",
      ~expected="count - 1",
    ),
    /* Module items */
    sel_test(
      ~name="spec: m/x = %",
      ~code="let m = { let x = 1; let y = 2 } in m.x",
      ~sel="m/x = %",
      ~expected="1",
    ),
    sel_test(
      ~name="spec: m/y = %",
      ~code="let m = { let x = 1; let y = 2 } in m.y",
      ~sel="m/y = %",
      ~expected="2",
    ),
    /* Nested binder chains */
    sel_test(
      ~name="spec: a/b/y = %",
      ~code="let a = { let x = 1; let b = { let y = 42 } } in a.b.y",
      ~sel="a/b/y = %",
      ~expected="42",
    ),
    /* Bare name */
    sel_test(
      ~name="y = % bare",
      ~code="let x = 42 in let y = 99 in x + y",
      ~sel="y = %",
      ~expected="99",
    ),
    /* Body selection */
    sel_test(
      ~name="x _... in %",
      ~code="let x = 42 in let y = 99 in x + y",
      ~sel="x _... in %",
      ~expected="let y = 99 in x + y",
    ),
    /* === Descend-to-find nested binder === */
    sel_test(
      ~name="\\... let b = % (nested in def)",
      ~code="let a = (let b = 42 in b) in a",
      ~sel="\\... let b = %",
      ~expected="42",
    ),
    test_case(
      "let b = % (NOT found at root)",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "let a = (let b = 42 in b) in a",
            "let b = %",
          );
        /* Verify base error + diagnostics */
        check(
          bool,
          "has error prefix",
          true,
          str_contains(result, "ERROR: No match"),
        );
        check(
          bool,
          "did-you-mean",
          true,
          str_contains(result, "Did you mean: a"),
        );
        check(
          bool,
          "available names",
          true,
          str_contains(result, "Available names: a"),
        );
      },
    ),
    /* === Fun spine tests === */
    sel_test(
      ~name="fun _ -> %",
      ~code="let f = fun x -> x + 1 in f",
      ~sel="let f = \\... fun _ -> %",
      ~expected="x + 1",
    ),
    sel_test(
      ~name="fun x -> %",
      ~code="let f = fun x -> x + 1 in f",
      ~sel="let f = \\... fun x -> %",
      ~expected="x + 1",
    ),
    sel_test(
      ~name="fun ... -> %",
      ~code="let f = fun x -> x + 1 in f",
      ~sel="let f = \\... fun _... -> %",
      ~expected="x + 1",
    ),
    sel_test(
      ~name="descend fun then if",
      ~code="let f = fun x -> if x > 0 then x else 0 in f",
      ~sel="let f = \\... fun _ -> \\... if _... else %",
      ~expected="0",
    ),
    /* === Test keyword tests === */
    sel_test(
      ~name="test %",
      ~code="let x = 1 in test x == 1 end; x",
      ~sel="\\... test %",
      ~expected="x == 1",
    ),
    /* === Colon (type annotation) tests === */
    sel_test(
      ~name="let x : _ = % (annotated)",
      ~code="let x : Int = 42 in x",
      ~sel="let x : _ = %",
      ~expected="42",
    ),
    sel_test(
      ~name="let x = % (annotated, no colon in selector)",
      ~code="let x : Int = 42 in x",
      ~sel="let x = %",
      ~expected="42",
    ),
    /* === FocusTyp tests === */
    /* let x : * focuses on the type annotation itself */
    sel_test(
      ~name="let x : % (focus type annotation)",
      ~code="let x : Int = 42 in x",
      ~sel="let x : %",
      ~expected="Int",
    ),
    /* type T = % focuses on the type definition */
    sel_test(
      ~name="type T = % (focus type def)",
      ~code="type T = Int in let x : T = 42 in x",
      ~sel="type T = %",
      ~expected="Int",
    ),
    /* === List spine tests === */
    sel_test(
      ~name="[ % ... ] first",
      ~code="let xs = [1, 2, 3] in xs",
      ~sel="let xs = \\... [ %",
      ~expected="1",
    ),
    sel_test(
      ~name="[ ... % ] last",
      ~code="let xs = [1, 2, 3] in xs",
      ~sel="let xs = \\... [ _... %",
      ~expected="3",
    ),
    sel_test(
      ~name="[ _ % ... ] second",
      ~code="let xs = [1, 2, 3] in xs",
      ~sel="let xs = \\... [ _ %",
      ~expected="2",
    ),
    /* === Tuple spine tests === */
    sel_test(
      ~name="( % first",
      ~code="let t = (1, 2, 3) in t",
      ~sel="let t = \\... ( %",
      ~expected="1",
    ),
    sel_test(
      ~name="( _... % last",
      ~code="let t = (1, 2, 3) in t",
      ~sel="let t = \\... ( _... %",
      ~expected="3",
    ),
    sel_test(
      ~name="( _ % second",
      ~code="let t = (1, 2, 3) in t",
      ~sel="let t = \\... ( _ %",
      ~expected="2",
    ),
    sel_test(
      ~name="( _ _ % third",
      ~code="let t = (1, 2, 3) in t",
      ~sel="let t = \\... ( _ _ %",
      ~expected="3",
    ),
    /* === Focus-before-keyword tests === */
    test_case(
      "% let x selects whole let",
      `Quick,
      () => {
        let result = selector_query_unique("let x = 42 in x + 1", "% let x");
        /* Should return the entire let expression, not an error */
        check(
          bool,
          "not error",
          false,
          String.length(result) > 5 && String.sub(result, 0, 5) == "ERROR",
        );
        check(bool, "has let", true, string_contains("let", result));
      },
    ),
    test_case(
      "% fun matches whole fun",
      `Quick,
      () => {
        let code = "let f = fun x -> x + 1 in f";
        let result =
          selector_query_unique(code, "let f = \\... % fun _ -> %");
        /* The first * focuses, the second is in the fun spine */
        check(
          bool,
          "not error",
          false,
          String.length(result) > 5 && String.sub(result, 0, 5) == "ERROR",
        );
      },
    ),
    /* === Module expression (ModuleExp) tests === */
    sel_test(
      ~name="module M chain M/x = %",
      ~code="module M = { let x = 42; let y = 99 } in M.x",
      ~sel="M/x = %",
      ~expected="42",
    ),
    sel_test(
      ~name="module M chain M/y = %",
      ~code="module M = { let x = 42; let y = 99 } in M.y",
      ~sel="M/y = %",
      ~expected="99",
    ),
    sel_test(
      ~name="module M = % (def)",
      ~code="module M = { let x = 1 } in M.x",
      ~sel="module M = %",
      ~expected="{ let x = 1 }",
    ),
    sel_test(
      ~name="module M body (M _... in %)",
      ~code="module M = { let x = 1 } in M.x",
      ~sel="M _... in %",
      ~expected="M.x",
    ),
    sel_test(
      ~name="module nested: A/B/x = %",
      ~code="module A = { let z = 0; module B = { let x = 42 } } in A.B.x",
      ~sel="A/B/x = %",
      ~expected="42",
    ),
    /* === Module spine { ... } tests === */
    sel_test(
      ~name="{ % first item",
      ~code="module M = { let x = 1; let y = 2 } in M.x",
      ~sel="M = { %",
      ~expected="let x = 1",
    ),
    sel_test(
      ~name="{ _ % second item",
      ~code="module M = { let x = 1; let y = 2 } in M.x",
      ~sel="M = { _ %",
      ~expected="let y = 2",
    ),
    sel_test(
      ~name="{ _... % last item",
      ~code="module M = { let x = 1; let y = 2; let z = 3 } in M.x",
      ~sel="M = { _... %",
      ~expected="let z = 3",
    ),
    sel_test(
      ~name="{ let x = % named item def",
      ~code="module M = { let x = 42; let y = 99 } in M.x",
      ~sel="M = { let x = %",
      ~expected="42",
    ),
    sel_test(
      ~name="{ _... let y = % skip to named",
      ~code="module M = { let x = 42; let y = 99 } in M.x",
      ~sel="M = { _... let y = %",
      ~expected="99",
    ),
    test_case(
      "{ let _ = % matches all let defs",
      `Quick,
      () => {
        let results =
          selector_query(
            "module M = { let x = 42; let y = 99 } in M.x",
            "M = { _... let _ = %",
          );
        check(int, "match count", 2, List.length(results));
      },
    ),
    sel_test(
      ~name="{ type T = % inside module",
      ~code="module M = { type T = Int; let x = 1 } in M.x",
      ~sel="M = { type T = %",
      ~expected="Int",
    ),
    sel_test(
      ~name="{ module B = % inside module",
      ~code="module A = { module B = { let x = 42 } } in A.B.x",
      ~sel="A = { module B = %",
      ~expected="{ let x = 42 }",
    ),
    sel_test(
      ~name="{ _... let x = % (ellipsis matches first)",
      ~code="module M = { let x = 42 } in M.x",
      ~sel="M = { _... let x = %",
      ~expected="42",
    ),
    sel_test(
      ~name="{ _ _ % third item",
      ~code="module M = { let a = 1; let b = 2; let c = 3 } in M.a",
      ~sel="M = { _ _ %",
      ~expected="let c = 3",
    ),
    /* Descend through regular let chain should find unique match */
    sel_test(
      ~name="descend let chain unique",
      ~code="let x = 1 in let y = x + 1 in y",
      ~sel="\\... let y = %",
      ~expected="x + 1",
    ),
    /* Descend through ModuleExp body to find a let */
    sel_test(
      ~name="descend through module body",
      ~code="module M = { let x = 1 } in let y = M.x + 1 in y",
      ~sel="\\... let y = %",
      ~expected="M.x + 1",
    ),
    /* === ModLet descent: \... let x = * finds ModLet inside Module === */
    sel_test(
      ~name="descend let inside module items",
      ~code="module M = { let x = 42; let y = 99 } in M.x",
      ~sel="\\... let x = %",
      ~expected="42",
    ),
    sel_test(
      ~name="descend let y inside module items",
      ~code="module M = { let x = 42; let y = 99 } in M.x",
      ~sel="\\... let y = %",
      ~expected="99",
    ),
    /* find_all_lets via chain sugar enters def then descends */
    sel_test(
      ~name="chain then descend let inside module",
      ~code="let m = { let a = 1; let b = 2 } in m.a",
      ~sel="m = \\... let a = %",
      ~expected="1",
    ),
    /* descend_all recurses into ModuleMod defs */
    sel_test(
      ~name="descend into nested ModuleMod def",
      ~code="module A = { let z = 0; module B = { let x = 42 } } in A.B.x",
      ~sel="\\... let x = %",
      ~expected="42",
    ),
    /* === Name indexing for shadowed bindings === */
    sel_test(
      ~name="x#0 = % (first binding)",
      ~code="let x = 1 in let x = 2 in x",
      ~sel="x#0 = %",
      ~expected="1",
    ),
    sel_test(
      ~name="x#1 = % (second binding)",
      ~code="let x = 1 in let x = 2 in x",
      ~sel="x#1 = %",
      ~expected="2",
    ),
    sel_test(
      ~name="x#0 _... in % (first body)",
      ~code="let x = 1 in let x = 2 in x",
      ~sel="x#0 _... in %",
      ~expected="let x = 2 in x",
    ),
    sel_test(
      ~name="x#1 _... in % (second body)",
      ~code="let x = 1 in let x = 2 in x",
      ~sel="x#1 _... in %",
      ~expected="x",
    ),
    /* Out-of-range index diagnostic */
    test_case(
      "x#5 out of range",
      `Quick,
      () => {
        let result =
          selector_query_unique("let x = 1 in let x = 2 in x", "x#5 = %");
        check(bool, "has error prefix", true, str_contains(result, "ERROR"));
        check(
          bool,
          "mentions count",
          true,
          str_contains(result, "2 binding(s) named 'x'"),
        );
      },
    ),
    /* Index with let keyword */
    sel_test(
      ~name="let x#0 = % with let keyword",
      ~code="let x = 10 in let x = 20 in x",
      ~sel="let x#1 = %",
      ~expected="20",
    ),
    /* === Shadowed bindings: multi-match with bare name (a = *) === */
    test_case(
      "a = % matches all shadowed bindings",
      `Quick,
      () => {
        let results =
          selector_query("let a = 4 in let a = 4 in let a = 4 in a", "a = %");
        check(int, "match count", 3, List.length(results));
      },
    ),
    test_case(
      "a ... in % matches all shadowed bodies",
      `Quick,
      () => {
        let results =
          selector_query(
            "let a = 4 in let a = 4 in let a = 4 in a",
            "a _... in %",
          );
        check(int, "match count", 3, List.length(results));
      },
    ),
    /* === Chain resolution through shadowed names (a/a) === */
    test_case(
      "a/a finds nested binding through shadow",
      `Quick,
      () => {
        let results =
          selector_query(
            "let a = 4 in let a = (let a = 0 in 4) in let a = 4 in a",
            "a/a = %",
          );
        check(int, "match count", 1, List.length(results));
        check(string, "value", "0", List.hd(results));
      },
    ),
    test_case(
      "a/b chain finds correct nested binding",
      `Quick,
      () => {
        let results =
          selector_query("let a = (let b = 42 in b) in a", "a/b = %");
        check(int, "match count", 1, List.length(results));
        check(string, "value", "42", List.hd(results));
      },
    ),
    test_case(
      "a/a/a three-level chain",
      `Quick,
      () => {
        let results =
          selector_query(
            "let a = (let a = (let a = 99 in a) in a) in a",
            "a/a/a = %",
          );
        check(int, "match count", 1, List.length(results));
        check(string, "value", "99", List.hd(results));
      },
    ),
    /* === Module/type spine uniformity === */
    /* module _ = * : wildcard module name */
    sel_test(
      ~name="module _ = % (wildcard name)",
      ~code="module M = { let x = 1 } in M.x",
      ~sel="module _ = %",
      ~expected="{ let x = 1 }",
    ),
    /* module M _... in * : focus on module body */
    sel_test(
      ~name="module M _... in %",
      ~code="module M = { let x = 1 } in M.x",
      ~sel="module M _... in %",
      ~expected="M.x",
    ),
    /* module _... in * : wildcard name, skip to body */
    sel_test(
      ~name="module _... in %",
      ~code="module M = { let x = 1 } in M.x",
      ~sel="module _... in %",
      ~expected="M.x",
    ),
    /* type T _... in * : skip type def, focus on body */
    sel_test(
      ~name="type T _... in %",
      ~code="type T = Int in let x : T = 42 in x",
      ~sel="type T _... in %",
      ~expected="let x = 42 in x",
    ),
    /* * module M : whole-binding focus */
    sel_test_rendered(
      ~name="% module M (whole binding)",
      ~code="module M = { let x = 1 } in M.x",
      ~sel="% module M",
      ~expected="module M = { let x = 1 } in M.x",
    ),
    /* === Compositionality tests on realistic program === */
    /* Binder chains into module members */
    sel_test(
      ~name="app: App/init = %",
      ~code=app_program,
      ~sel="App/init = %",
      ~expected="0",
    ),
    /* Chain + descend + case scrutinee */
    sel_test(
      ~name="app: App/update \\... case %",
      ~code=app_program,
      ~sel="App/update \\... case %",
      ~expected="msg",
    ),
    /* Chain + descend + named arm */
    sel_test(
      ~name="app: App/update \\... | Inc => %",
      ~code=app_program,
      ~sel="App/update \\... | Inc => %",
      ~expected="msg + 1",
    ),
    /* Chain + descend + wildcard arm: returns ALL arm bodies */
    test_case(
      "app: App/update \\... | _ => % (all arms)",
      `Quick,
      () => {
        let results =
          selector_query(app_program, "App/update \\... | _ => %");
        check(int, "match count", 3, List.length(results));
      },
    ),
    /* Chain + descend + nested let */
    sel_test(
      ~name="app: App/view \\... let label = %",
      ~code=app_program,
      ~sel="App/view \\... let label = %",
      ~expected="model + 1",
    ),
    /* Descend finds all function bodies */
    test_case(
      "app: \\... fun _ -> % (all funs)",
      `Quick,
      () => {
        let results = selector_query(app_program, "\\... fun _ -> %");
        check(
          bool,
          "at least 2 fun bodies",
          true,
          List.length(results) >= 2,
        );
      },
    ),
    /* Module def — check starts with expected prefix */
    test_case(
      "app: module App = % (def)",
      `Quick,
      () => {
        let result = selector_query_unique(app_program, "module App = %");
        check(
          bool,
          "starts with { let init",
          true,
          String.length(result) >= 10
          && String.sub(result, 0, 10) == "{ let init",
        );
      },
    ),
    /* Module body (after in) */
    test_case(
      "app: App _... in % (body)",
      `Quick,
      () => {
        let result = selector_query_unique(app_program, "App _... in %");
        check(
          bool,
          "starts with let result",
          true,
          String.length(result) >= 10
          && String.sub(result, 0, 10) == "let result",
        );
      },
    ),
    /* Whole-binding focus */
    test_case(
      "app: \\... % let result",
      `Quick,
      () => {
        let result = selector_query_unique(app_program, "\\... % let result");
        check(
          bool,
          "starts with let result",
          true,
          String.length(result) >= 10
          && String.sub(result, 0, 10) == "let result",
        );
      },
    ),
    /* === Test spine ellipsis === */
    sel_test(
      ~name="test _... end (ellipsis)",
      ~code="test 1 + 1 == 2 end",
      ~sel="test _... end",
      ~expected="test 1 + 1 == 2 end",
    ),
    sel_test(
      ~name="test _ % (slot then focus)",
      ~code="let x = 1 in test x == 1 end",
      ~sel="\\... test _ %",
      ~expected="x == 1",
    ),
    /* === Diagnostic tests === */
    /* Name not found: suggest similar + list available */
    test_case(
      "diag: name not found with suggestion",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "let foo = 1 in let bar = 2 in foo + bar",
            "let baz = %",
          );
        check(bool, "is error", true, str_contains(result, "ERROR:"));
        check(
          bool,
          "suggests bar",
          true,
          str_contains(result, "Did you mean: bar"),
        );
        check(
          bool,
          "lists available",
          true,
          str_contains(result, "foo") && str_contains(result, "bar"),
        );
      },
    ),
    /* Name not found: no similar names (Levenshtein > 2) */
    test_case(
      "diag: name not found, no suggestion",
      `Quick,
      () => {
        let result = selector_query_unique("let x = 1 in x", "let zzzzz = %");
        check(bool, "is error", true, str_contains(result, "ERROR:"));
        check(
          bool,
          "no did-you-mean",
          false,
          str_contains(result, "Did you mean"),
        );
        check(
          bool,
          "lists available",
          true,
          str_contains(result, "Available names: x"),
        );
      },
    ),
    /* Keyword mismatch: if on a let */
    test_case(
      "diag: keyword mismatch",
      `Quick,
      () => {
        let result = selector_query_unique("let x = 1 in x", "if %");
        check(bool, "is error", true, str_contains(result, "ERROR:"));
        check(
          bool,
          "failed at first step",
          true,
          str_contains(result, "Failed at first step: if"),
        );
      },
    ),
    /* Binder chain: first segment fails */
    test_case(
      "diag: chain first segment fails",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "module App = { let x = 1 } in App.x",
            "Apl/x = %",
          );
        check(bool, "is error", true, str_contains(result, "ERROR:"));
        check(
          bool,
          "suggests App",
          true,
          str_contains(result, "Did you mean: App"),
        );
      },
    ),
    /* Partial match: let keyword matches but name fails */
    test_case(
      "diag: partial match on let",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "let alpha = 1 in let beta = 2 in alpha + beta",
            "let gamma = %",
          );
        check(bool, "is error", true, str_contains(result, "ERROR:"));
        check(
          bool,
          "matched let",
          true,
          str_contains(result, "Matched up to: let"),
        );
        check(
          bool,
          "failed at gamma",
          true,
          str_contains(result, "Failed at: gamma"),
        );
      },
    ),
    /* Module-scoped name diagnostics */
    test_case(
      "diag: module member not found",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            "module M = { let x = 1; let y = 2 } in M.x",
            "M/z = %",
          );
        check(bool, "is error", true, str_contains(result, "ERROR:"));
        check(
          bool,
          "lists available in module",
          true,
          str_contains(result, "x") && str_contains(result, "y"),
        );
      },
    ),
    /* === Double descent === */
    /* \... \... is idempotent: collapsed to single \... in elaboration */
    test_case(
      "double descent is idempotent",
      `Quick,
      () => {
        let code =
          "let f = fun x -> case x | A => 1 | B => 2 end in "
          ++ "let g = fun y -> case y | C => 3 | D => 4 end in "
          ++ "f(g(0))";
        let single = selector_query(code, "\\... | _ => %");
        let double = selector_query(code, "\\... \\... | _ => %");
        check(int, "same count", List.length(single), List.length(double));
      },
    ),
    /* Descend + chain + spine: complex composition */
    test_case(
      "chain + descend + if else + focus",
      `Quick,
      () => {
        let code =
          "module M = { "
          ++ "let f = fun x -> if x > 0 then x + 1 else x - 1 "
          ++ "} in M.f(5)";
        let result = selector_query_unique(code, "M/f \\... if _... else %");
        check(string, "else branch", "x - 1", result);
      },
    ),
    /* InsertAfter via selector + verify result via selector query */
    test_case(
      "selector roundtrip: insert then query",
      `Quick,
      () => {
        let code = "let x = 1 in x + 1";
        switch (
          run_agent_action(code, SelectorInsertAfter("% let x", "let y = 2"))
        ) {
        | Ok(new_z) =>
          let new_term = MakeTerm.from_zip_for_sem(new_z, ~root=Exp).term;
          /* Verify the inserted binding exists */
          let y_results = Selector.query("let y = %", new_term);
          check(int, "y binding found", 1, List.length(y_results));
          /* Verify original still exists */
          let x_results = Selector.query("let x = %", new_term);
          check(int, "x binding still there", 1, List.length(x_results));
        | Error(err) =>
          Alcotest.fail("Insert failed: " ++ Action.Failure.show(err))
        };
      },
    ),
    /* === Chain trailing-slash semantics === */
    /* a/ (trailing slash, single segment) = enter a's def */
    sel_test(
      ~name="a/ % (trailing slash enters def)",
      ~code="let a = 42 in a + 1",
      ~sel="a/ %",
      ~expected="42",
    ),
    /* a (no trailing slash) = atom match (pat + exp references, spec section 6) */
    test_case(
      "a (no slash = atom match)",
      `Quick,
      () => {
        let results = selector_query("let a = 42 in a + 1", "a");
        check(int, "2 matches (pat + ref)", 2, List.length(results));
      },
    ),
    /* A/B/C/ enters all defs */
    sel_test(
      ~name="A/B/C/ % (trailing slash on chain)",
      ~code="let a = { let b = { let c = 99 } } in a.b.c",
      ~sel="a/b/c/ %",
      ~expected="99",
    ),
    /* A/B/C without trailing slash: inside Module(items), bare name
       returns FocusMod — the whole ModLet item, not just the def. */
    sel_test_rendered(
      ~name="A/B/C (no slash, module context = FocusMod)",
      ~code="let a = { let b = { let c = 99 } } in a.b.c",
      ~sel="a/b/c",
      ~expected="let c = 99",
    ),
    /* Bare name at top level = atom match (pat + exp references) */
    test_case(
      "b (no slash, top-level = atom match)",
      `Quick,
      () => {
        let results =
          selector_query("let a = 42 in let b = 99 in a + b", "b");
        check(int, "2 matches (pat + ref)", 2, List.length(results));
      },
    ),
    /* Trailing slash + continuation */
    sel_test(
      ~name="m/ \\... let x = % (trailing slash + descend)",
      ~code="let m = { let x = 42; let y = 99 } in m.x",
      ~sel="m/ \\... let x = %",
      ~expected="42",
    ),
    /* Implicit focus rule: last token is a name → insert % before it */
    sel_test(
      ~name="let x (implicit focus = pattern)",
      ~code="let x = 42 in x + 1",
      ~sel="let x",
      ~expected="x",
    ),
    sel_test(
      ~name="let x = (implicit star on def)",
      ~code="let x = 42 in x + 1",
      ~sel="let x =",
      ~expected="42",
    ),
    sel_test(
      ~name="a/b/ (implicit star on chain trailing slash)",
      ~code="let a = { let b = 42 } in a.b",
      ~sel="a/b/",
      ~expected="42",
    ),
    /* Spaced chain segments: A/ B/ C should equal A/B/C */
    sel_test(
      ~name="A/ B/ C/ % (spaced chain = same as compact)",
      ~code="let a = { let b = { let c = 99 } } in a.b.c",
      ~sel="a/ b/ c/ %",
      ~expected="99",
    ),
    /* * prefix before keyword: focus on whole matched subtree */
    sel_test_rendered(
      ~name="% let x (focus whole let)",
      ~code="let x = 42 in x + 1",
      ~sel="% let x",
      ~expected="let x = 42 in x + 1",
    ),
    sel_test_rendered(
      ~name="\\... % let y (descend + focus whole let)",
      ~code="let x = (let y = 99 in y) in x",
      ~sel="\\... % let y",
      ~expected="let y = 99 in y",
    ),
    /* === Module-internal keyword matching === */
    /* module keyword matches ModuleMod inside Module items */
    sel_test(
      ~name="module B = % inside module items",
      ~code="module A = { let z = 0; module B = { let x = 42 } } in A.B.x",
      ~sel="A/ \\... module B = %",
      ~expected="{ let x = 42 }",
    ),
    /* type keyword matches ModType inside Module items */
    sel_test(
      ~name="type T = % inside module items",
      ~code="module M = { type T = Int; let x = 1 } in M.x",
      ~sel="M/ \\... type T = %",
      ~expected="Int",
    ),
    /* module B = * inside module items → focuses on B's def */
    sel_test(
      ~name="module B = % inside module items",
      ~code="module A = { module B = { let x = 42 } } in A.B.x",
      ~sel="A/ \\... module B = %",
      ~expected="{ let x = 42 }",
    ),
    /* type T inside module items: implicit focus → type %T → FocusTPat */
    sel_test(
      ~name="type T (bare) inside module items = FocusTPat",
      ~code="module M = { type T = Int; let x = 1 } in M.x",
      ~sel="M/ \\... type T",
      ~expected="T",
    ),
    /* === Implicit star with various forms === */
    /* let x = (no *) → implicit * makes it focus on def */
    sel_test(
      ~name="let x = (implicit star on = position)",
      ~code="let x = 42 in x + 1",
      ~sel="let x =",
      ~expected="42",
    ),
    /* \... fun _ -> (implicit star on ->) */
    sel_test(
      ~name="\\... fun _ -> (implicit star)",
      ~code="let f = fun x -> x + 1 in f",
      ~sel="let f = \\... fun _ ->",
      ~expected="x + 1",
    ),
    /* type T = (implicit star) */
    sel_test(
      ~name="type T = (implicit star)",
      ~code="type T = Int in let x : T = 42 in x",
      ~sel="type T =",
      ~expected="Int",
    ),
    /* === Explicit pattern focus === */
    sel_test(
      ~name="let % x = (explicit pattern focus)",
      ~code="let x = 42 in x + 1",
      ~sel="let % x =",
      ~expected="x",
    ),
    sel_test(
      ~name="let % x (explicit pattern focus, terminal)",
      ~code="let x = 42 in x + 1",
      ~sel="let % x",
      ~expected="x",
    ),
    sel_test(
      ~name="let % = (slot-focus pattern)",
      ~code="let x = 42 in x + 1",
      ~sel="let % =",
      ~expected="x",
    ),
    sel_test(
      ~name="fun % x -> (explicit pattern focus)",
      ~code="let f = fun x -> x + 1 in f",
      ~sel="\\... fun % x ->",
      ~expected="x",
    ),
    sel_test(
      ~name="fun % -> (slot-focus pattern)",
      ~code="let f = fun x -> x + 1 in f",
      ~sel="\\... fun % ->",
      ~expected="x",
    ),
    sel_test(
      ~name="| % A => (arm pattern focus)",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="\\... | % A =>",
      ~expected="A",
    ),
    /* === Implicit focus (changed behavior) === */
    sel_test(
      ~name="fun x (implicit focus = parameter pattern)",
      ~code="let f = fun x -> x + 1 in f",
      ~sel="\\... fun x",
      ~expected="x",
    ),
    /* === Unchanged behavior (verify no regression) === */
    sel_test_rendered(
      ~name="% let x (explicit prefix = whole let)",
      ~code="let x = 42 in x + 1",
      ~sel="% let x",
      ~expected="let x = 42 in x + 1",
    ),
    sel_test(
      ~name="let x = % (explicit focus on def unchanged)",
      ~code="let x = 42 in x + 1",
      ~sel="let x = %",
      ~expected="42",
    ),
    sel_test_rendered(
      ~name="type T = (unchanged)",
      ~code="type T = Int in let x : T = 42 in x",
      ~sel="type T =",
      ~expected="Int",
    ),
    /* === Slot-in patterns: let x = _ in === */
    sel_test(
      ~name="let x = _ in % (skip def, focus body)",
      ~code="let x = 42 in x + 1",
      ~sel="let x = _ in %",
      ~expected="x + 1",
    ),
    sel_test(
      ~name="let _ = _ in % (wildcard pattern, skip def, focus body)",
      ~code="let x = 42 in x + 1",
      ~sel="let _ = _ in %",
      ~expected="x + 1",
    ),
    /* === Composition: chain + descend + if === */
    sel_test(
      ~name="M/f \\... if _ then % (chain+descend+if)",
      ~code=
        "module M = { "
        ++ "let f = fun x -> if x > 0 then x + 1 else x - 1 "
        ++ "} in M.f(5)",
      ~sel="M/f \\... if _ then %",
      ~expected="x + 1",
    ),
    /* === Bare name in module context → FocusMod === */
    sel_test_rendered(
      ~name="m/x (bare name in module = FocusMod)",
      ~code="let m = { let x = 42; let y = 99 } in m.x",
      ~sel="m/x",
      ~expected="let x = 42",
    ),
    /* Trailing slash still enters the def (not FocusMod) */
    sel_test(
      ~name="m/x/ (trailing slash = def, not FocusMod)",
      ~code="let m = { let x = 42; let y = 99 } in m.x",
      ~sel="m/x/",
      ~expected="42",
    ),
    /* === Chain equivalence: compact vs spaced === */
    test_case(
      "spaced chain = compact chain",
      `Quick,
      () => {
        let code = "let a = { let b = { let c = 99 } } in a.b.c";
        let compact = selector_query_unique(code, "a/b/c/ %");
        let spaced = selector_query_unique(code, "a/ b/ c/ %");
        check(string, "same result", compact, spaced);
      },
    ),
    /* === module B bare name: implicit focus → module %B → FocusPat === */
    sel_test(
      ~name="module B (bare) inside module = FocusPat",
      ~code="module A = { module B = { let x = 42 } } in A.B.x",
      ~sel="A/ \\... module B",
      ~expected="B",
    ),
    /* === Indexing for non-let binders === */
    /* module#N: disambiguate shadowed module binders */
    sel_test(
      ~name="module M#0 = % (first module)",
      ~code="module M = { let x = 1 } in module M = { let y = 2 } in M.y",
      ~sel="module M#0 = %",
      ~expected="{ let x = 1 }",
    ),
    sel_test(
      ~name="module M#1 = % (second module)",
      ~code="module M = { let x = 1 } in module M = { let y = 2 } in M.y",
      ~sel="module M#1 = %",
      ~expected="{ let y = 2 }",
    ),
    /* type#N: disambiguate shadowed type binders */
    sel_test_rendered(
      ~name="type T#0 = % (first type)",
      ~code="type T = Int in type T = Bool in 42",
      ~sel="type T#0 = %",
      ~expected="Int",
    ),
    sel_test_rendered(
      ~name="type T#1 = % (second type)",
      ~code="type T = Int in type T = Bool in 42",
      ~sel="type T#1 = %",
      ~expected="Bool",
    ),
    /* === ChildIndex: numeric child addressing === */
    /* Let: #0=pat, #1=def, #2=body */
    sel_test(
      ~name="#1 on let = def",
      ~code="let x = 42 in x + 1",
      ~sel="#1",
      ~expected="42",
    ),
    sel_test(
      ~name="#2 on let = body",
      ~code="let x = 42 in x + 1",
      ~sel="#2",
      ~expected="x + 1",
    ),
    sel_test_rendered(
      ~name="#0 on let = pat",
      ~code="let x = 42 in x + 1",
      ~sel="#0",
      ~expected="x",
    ),
    /* BinOp: #0=left, #1=right */
    sel_test(
      ~name="x = #0 (left of binop)",
      ~code="let x = 1 + 2 in x",
      ~sel="x = #0",
      ~expected="1",
    ),
    sel_test(
      ~name="x = #1 (right of binop)",
      ~code="let x = 1 + 2 in x",
      ~sel="x = #1",
      ~expected="2",
    ),
    /* Deep traversal: #1 #0 #0 */
    sel_test(
      ~name="#1 #0 (nested: left of inner binop)",
      ~code="let x = (1 + 2) + 3 in x",
      ~sel="x = #0 #0",
      ~expected="1 + 2",
    ),
    sel_test(
      ~name="x = #0 #0 #0 (deep: left operand of inner +)",
      ~code="let x = (1 + 2) + 3 in x",
      ~sel="x = #0 #0 #0",
      ~expected="1",
    ),
    sel_test(
      ~name="x = #0 #0 #1 (deep: right operand of inner +)",
      ~code="let x = (1 + 2) + 3 in x",
      ~sel="x = #0 #0 #1",
      ~expected="2",
    ),
    /* Parens: #0 enters parens */
    sel_test(
      ~name="x = #0 (enters parens)",
      ~code="let x = (42) in x",
      ~sel="x = #0",
      ~expected="42",
    ),
    /* If: #0=cond, #1=then, #2=else */
    sel_test(
      ~name="#0 on if = cond",
      ~code="if true then 1 else 0",
      ~sel="#0",
      ~expected="true",
    ),
    sel_test(
      ~name="#1 on if = then",
      ~code="if true then 1 else 0",
      ~sel="#1",
      ~expected="1",
    ),
    sel_test(
      ~name="#2 on if = else",
      ~code="if true then 1 else 0",
      ~sel="#2",
      ~expected="0",
    ),
    /* Cross-sort: Pat → Typ via Asc */
    sel_test_rendered(
      ~name="#0 #1 (pat Asc → type annotation)",
      ~code="let x : Int = 42 in x",
      ~sel="#0 #1",
      ~expected="Int",
    ),
    sel_test_rendered(
      ~name="#0 #0 (pat Asc → inner pat)",
      ~code="let x : Int = 42 in x",
      ~sel="#0 #0",
      ~expected="x",
    ),
    /* Cross-sort: Exp → Typ via Asc expression.
       (42 : Int) parses as Parens(Asc(42, Int)): #0 enters Parens, #1 gets type */
    sel_test_rendered(
      ~name="x = #0 #1 (Asc expr → type)",
      ~code="let x = (42 : Int) in x",
      ~sel="x = #0 #1",
      ~expected="Int",
    ),
    /* Tuple: (1, 2, 3) parses as Parens(Tuple(...)). #0 enters Parens,
       then #0/#1/#2 select tuple elements. */
    sel_test(
      ~name="x = #0 #0 (tuple first via Parens)",
      ~code="let x = (1, 2, 3) in x",
      ~sel="x = #0 #0",
      ~expected="1",
    ),
    sel_test(
      ~name="x = #0 #2 (tuple third via Parens)",
      ~code="let x = (1, 2, 3) in x",
      ~sel="x = #0 #2",
      ~expected="3",
    ),
    /* ListLit: #0, #1, #2 = elements (direct, no extra nesting) */
    sel_test(
      ~name="x = #0 (list first)",
      ~code="let x = [10, 20, 30] in x",
      ~sel="x = #0",
      ~expected="10",
    ),
    sel_test(
      ~name="x = #2 (list third)",
      ~code="let x = [10, 20, 30] in x",
      ~sel="x = #2",
      ~expected="30",
    ),
    /* Match: #0=scrut, #1=(rule0 pair), #2=(rule1 pair) */
    sel_test(
      ~name="case #0 (scrutinee)",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="#0",
      ~expected="x",
    ),
    sel_test_rendered(
      ~name="case #1 #0 (rule 0 pat)",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="#1 #0",
      ~expected="A",
    ),
    sel_test(
      ~name="case #1 #1 (rule 0 body)",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="#1 #1",
      ~expected="1",
    ),
    sel_test_rendered(
      ~name="case #2 #0 (rule 1 pat)",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="#2 #0",
      ~expected="B",
    ),
    sel_test(
      ~name="case #2 #1 (rule 1 body)",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="#2 #1",
      ~expected="2",
    ),
    /* Fun: #0=pat, #1=body */
    sel_test_rendered(
      ~name="fun #0 (pat)",
      ~code="fun x -> x + 1",
      ~sel="#0",
      ~expected="x",
    ),
    sel_test(
      ~name="fun #1 (body)",
      ~code="fun x -> x + 1",
      ~sel="#1",
      ~expected="x + 1",
    ),
    /* Module: #0, #1 = items */
    sel_test_rendered(
      ~name="M/ #0 (first module item)",
      ~code="let m = { let x = 1; let y = 2 } in m",
      ~sel="m = #0",
      ~expected="let x = 1",
    ),
    sel_test_rendered(
      ~name="M/ #1 (second module item)",
      ~code="let m = { let x = 1; let y = 2 } in m",
      ~sel="m = #1",
      ~expected="let y = 2",
    ),
    /* ModItem child: #0 on ModLet = pat, #1 = def */
    sel_test(
      ~name="M/ #0 #1 (first item def)",
      ~code="let m = { let x = 42; let y = 99 } in m",
      ~sel="m = #0 #1",
      ~expected="42",
    ),
    sel_test_rendered(
      ~name="M/ #0 #0 (first item pat)",
      ~code="let m = { let x = 42; let y = 99 } in m",
      ~sel="m = #0 #0",
      ~expected="x",
    ),
    /* Mixing named + index: use name to navigate, index for anonymous parts */
    sel_test(
      ~name="named + index: x = #0 (left of +)",
      ~code="let x = 10 + 20 in let y = 30 in x + y",
      ~sel="x = #0",
      ~expected="10",
    ),
    sel_test(
      ~name="named + index: x = #1 (right of +)",
      ~code="let x = 10 + 20 in let y = 30 in x + y",
      ~sel="x = #1",
      ~expected="20",
    ),
    /* Out-of-range index: should produce error */
    test_case(
      "#5 out of range",
      `Quick,
      () => {
        let result = selector_query_unique("let x = 42 in x", "#5");
        check(
          bool,
          "starts with ERROR",
          true,
          String.length(result) >= 5 && String.sub(result, 0, 5) == "ERROR",
        );
      },
    ),
    /* BinOp spine */
    sel_test(
      ~name="_ + % (right operand)",
      ~code="let x = 1 + 2 in x",
      ~sel="x = _ + %",
      ~expected="2",
    ),
    sel_test(
      ~name="% + _ (left operand)",
      ~code="let x = 1 + 2 in x",
      ~sel="x = % + _",
      ~expected="1",
    ),
    sel_test(
      ~name="_ + _ (whole binop)",
      ~code="let x = 1 + 2 in x",
      ~sel="x = _ + _",
      ~expected="1 + 2",
    ),
    sel_test(
      ~name="_ - % (subtraction right)",
      ~code="let x = 10 - 3 in x",
      ~sel="x = _ - %",
      ~expected="3",
    ),
    sel_test(
      ~name="_ && % (boolean and)",
      ~code="let x = true && false in x",
      ~sel="x = _ && %",
      ~expected="false",
    ),
    sel_test(
      ~name="_ == % (equality right)",
      ~code="let x = 1 == 2 in x",
      ~sel="x = _ == %",
      ~expected="2",
    ),
    sel_test(
      ~name="_ ++ % (string concat)",
      ~code={|let x = "a" ++ "b" in x|},
      ~sel="x = _ ++ %",
      ~expected={|"b"|},
    ),
    sel_test(
      ~name="_ :: % (cons right)",
      ~code="let x = 1 :: [2, 3] in x",
      ~sel="x = _ :: %",
      ~expected="[2, 3]",
    ),
    sel_test(
      ~name="% :: _ (cons left)",
      ~code="let x = 1 :: [2, 3] in x",
      ~sel="x = % :: _",
      ~expected="1",
    ),
    /* === Literal/Atom matching === */
    /* Integer literals */
    sel_test(
      ~name="\\... 42 (find int literal)",
      ~code="let x = 42 in x",
      ~sel="\\... 42",
      ~expected="42",
    ),
    sel_test(
      ~name="\\... 99 (find int literal nested)",
      ~code="let x = 42 in let y = 99 in x + y",
      ~sel="\\... 99",
      ~expected="99",
    ),
    /* Boolean literals */
    sel_test(
      ~name="\\... true (find bool literal)",
      ~code="let x = true in x",
      ~sel="\\... true",
      ~expected="true",
    ),
    sel_test(
      ~name="\\... false (find bool literal)",
      ~code="let x = false in x",
      ~sel="\\... false",
      ~expected="false",
    ),
    sel_test(
      ~name="if true: descend find true",
      ~code="if true then 1 else 0",
      ~sel="\\... true",
      ~expected="true",
    ),
    /* Multiple literal matches */
    test_case(
      "\\... 42 finds all occurrences",
      `Quick,
      () => {
        let results =
          selector_query("let x = 42 in let y = 42 in x + y", "\\... 42");
        check(int, "match count", 2, List.length(results));
      },
    ),
    /* Float literals */
    sel_test(
      ~name="\\... 3.14 (find float literal)",
      ~code="let x = 3.14 in x",
      ~sel="\\... 3.14",
      ~expected="3.140000",
    ),
    /* Literal in context: let _ = <literal> */
    sel_test(
      ~name="let x = % still returns literal",
      ~code="let x = 42 in x",
      ~sel="let x = %",
      ~expected="42",
    ),
    /* Literal matching doesn't break name matching */
    sel_test(
      ~name="name matching still works with literals present",
      ~code="let x = 42 in let y = 99 in x + y",
      ~sel="let y = %",
      ~expected="99",
    ),
  ],
);

/* === Canonical Numeric + Deparse Tests === */

/* Helper: parse code, resolve a selector to get a node, generate canonical
   path for that node, verify the deparsed path matches expected string */
let canonical_test = (~name, ~code, ~sel, ~expected_path) =>
  test_case(
    name,
    `Quick,
    () => {
      let root = mk_term(code);
      /* Resolve the selector to get the target node ID */
      switch (Selector.query_unique(sel, root)) {
      | Error(e) => fail(name ++ ": selector failed: " ++ e)
      | Ok(m) =>
        /* Generate canonical numeric path */
        switch (Selector.canonical_numeric(m.focused_id, root)) {
        | None => fail(name ++ ": canonical_numeric returned None")
        | Some(path) =>
          let deparsed = Selector.deparse(path);
          check(string, name ++ " path", expected_path, deparsed);
          /* Roundtrip: resolve the canonical path, verify same ID */
          switch (Selector.query_unique(deparsed, root)) {
          | Error(e) => fail(name ++ ": roundtrip resolve failed: " ++ e)
          | Ok(m2) =>
            check(
              bool,
              name ++ " roundtrip ID",
              true,
              m.focused_id == m2.focused_id,
            )
          };
        }
      };
    },
  );

/* Helper: roundtrip only — verify canonical path resolves to same node */
let canonical_roundtrip = (~name, ~code, ~sel) =>
  test_case(
    name,
    `Quick,
    () => {
      let root = mk_term(code);
      switch (Selector.query_unique(sel, root)) {
      | Error(e) => fail(name ++ ": selector failed: " ++ e)
      | Ok(m) =>
        switch (Selector.canonical_numeric(m.focused_id, root)) {
        | None => fail(name ++ ": canonical_numeric returned None")
        | Some(path) =>
          let deparsed = Selector.deparse(path);
          switch (Selector.query_unique(deparsed, root)) {
          | Error(e) =>
            fail(
              name
              ++ ": roundtrip failed: "
              ++ e
              ++ " (path: "
              ++ deparsed
              ++ ")",
            )
          | Ok(m2) =>
            check(
              bool,
              name ++ " roundtrip",
              true,
              m.focused_id == m2.focused_id,
            )
          };
        }
      };
    },
  );

let canonical_tests = (
  "AgentTools.Canonical",
  [
    /* === Canonical numeric path generation === */
    /* Root node: empty path */
    canonical_test(
      ~name="root = self",
      ~code="42",
      ~sel="%",
      ~expected_path="%",
    ),
    /* Let children */
    canonical_test(
      ~name="let def",
      ~code="let x = 42 in x",
      ~sel="x = %",
      ~expected_path="#1 %",
    ),
    canonical_test(
      ~name="let body",
      ~code="let x = 42 in x + 1",
      ~sel="#2",
      ~expected_path="#2 %",
    ),
    canonical_roundtrip(
      ~name="let pat roundtrip",
      ~code="let x = 42 in x",
      ~sel="#0",
    ),
    /* BinOp children */
    canonical_test(
      ~name="binop left",
      ~code="let x = 1 + 2 in x",
      ~sel="x = #0",
      ~expected_path="#1 #0 %",
    ),
    canonical_test(
      ~name="binop right",
      ~code="let x = 1 + 2 in x",
      ~sel="x = #1",
      ~expected_path="#1 #1 %",
    ),
    /* Deep nested */
    canonical_test(
      ~name="deep left-left",
      ~code="let x = (1 + 2) + 3 in x",
      ~sel="x = #0 #0 #0",
      ~expected_path="#1 #0 #0 #0 %",
    ),
    canonical_test(
      ~name="deep left-right",
      ~code="let x = (1 + 2) + 3 in x",
      ~sel="x = #0 #0 #1",
      ~expected_path="#1 #0 #0 #1 %",
    ),
    /* If */
    canonical_test(
      ~name="if cond",
      ~code="if true then 1 else 0",
      ~sel="if %",
      ~expected_path="#0 %",
    ),
    canonical_test(
      ~name="if then",
      ~code="if true then 1 else 0",
      ~sel="if _ then %",
      ~expected_path="#1 %",
    ),
    canonical_test(
      ~name="if else",
      ~code="if true then 1 else 0",
      ~sel="if _... else %",
      ~expected_path="#2 %",
    ),
    /* Cross-sort: Pat */
    canonical_roundtrip(
      ~name="pat in let",
      ~code="let x = 42 in x",
      ~sel="#0",
    ),
    /* Cross-sort: Typ via pat Asc */
    canonical_roundtrip(
      ~name="type annotation",
      ~code="let x : Int = 42 in x",
      ~sel="#0 #1",
    ),
    /* Tuple elements (via Parens) */
    canonical_test(
      ~name="tuple first",
      ~code="let x = (1, 2, 3) in x",
      ~sel="x = #0 #0",
      ~expected_path="#1 #0 #0 %",
    ),
    canonical_test(
      ~name="tuple third",
      ~code="let x = (1, 2, 3) in x",
      ~sel="x = #0 #2",
      ~expected_path="#1 #0 #2 %",
    ),
    /* List elements */
    canonical_test(
      ~name="list first",
      ~code="let x = [10, 20, 30] in x",
      ~sel="x = #0",
      ~expected_path="#1 #0 %",
    ),
    canonical_test(
      ~name="list third",
      ~code="let x = [10, 20, 30] in x",
      ~sel="x = #2",
      ~expected_path="#1 #2 %",
    ),
    /* Match: scrutinee and rule pairs */
    canonical_test(
      ~name="match scrut",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="case %",
      ~expected_path="#0 %",
    ),
    canonical_roundtrip(
      ~name="match rule0 pat",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="#1 #0",
    ),
    canonical_test(
      ~name="match rule0 body",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="#1 #1",
      ~expected_path="#1 #1 %",
    ),
    canonical_roundtrip(
      ~name="match rule1 pat",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="#2 #0",
    ),
    canonical_test(
      ~name="match rule1 body",
      ~code="case x | A => 1 | B => 2 end",
      ~sel="#2 #1",
      ~expected_path="#2 #1 %",
    ),
    /* Fun */
    canonical_roundtrip(~name="fun pat", ~code="fun x -> x + 1", ~sel="#0"),
    canonical_test(
      ~name="fun body",
      ~code="fun x -> x + 1",
      ~sel="#1",
      ~expected_path="#1 %",
    ),
    /* Module items */
    canonical_roundtrip(
      ~name="module item 0",
      ~code="let m = { let x = 1; let y = 2 } in m",
      ~sel="m = #0",
    ),
    canonical_roundtrip(
      ~name="module item 1",
      ~code="let m = { let x = 1; let y = 2 } in m",
      ~sel="m = #1",
    ),
    /* ModItem children */
    canonical_roundtrip(
      ~name="mod item pat",
      ~code="let m = { let x = 42 } in m",
      ~sel="m = #0 #0",
    ),
    canonical_test(
      ~name="mod item def",
      ~code="let m = { let x = 42 } in m",
      ~sel="m = #0 #1",
      ~expected_path="#1 #0 #1 %",
    ),
    /* Nested let chain */
    canonical_test(
      ~name="nested let: inner def",
      ~code="let x = 1 in let y = 2 in x + y",
      ~sel="y = %",
      ~expected_path="#2 #1 %",
    ),
    canonical_test(
      ~name="nested let: inner body",
      ~code="let x = 1 in let y = 2 in x + y",
      ~sel="#2 #2",
      ~expected_path="#2 #2 %",
    ),
    /* Seq */
    canonical_test(
      ~name="seq first",
      ~code="1; 2",
      ~sel="#0",
      ~expected_path="#0 %",
    ),
    canonical_test(
      ~name="seq second",
      ~code="1; 2",
      ~sel="#1",
      ~expected_path="#1 %",
    ),
    /* === Deparse tests === */
    test_case(
      "deparse: numeric path",
      `Quick,
      () => {
        open Selector;
        let path = [ChildIndex(1), ChildIndex(0), MatchFocus];
        check(string, "deparse", "#1 #0 %", deparse(path));
      },
    ),
    test_case("deparse: just focus", `Quick, () => {
      check(string, "deparse", "%", Selector.deparse([Selector.MatchFocus]))
    }),
    test_case(
      "deparse: named steps",
      `Quick,
      () => {
        open Selector;
        let path = [
          MatchKeyword("let"),
          MatchName("x"),
          MatchDelimiter("="),
          MatchFocus,
        ];
        check(string, "deparse", "let x = %", deparse(path));
      },
    ),
    test_case(
      "deparse: descend + name",
      `Quick,
      () => {
        open Selector;
        let path = [
          DescendInto,
          MatchKeyword("let"),
          MatchName("y"),
          MatchDelimiter("="),
          MatchFocus,
        ];
        check(string, "deparse", "\\... let y = %", deparse(path));
      },
    ),
    test_case(
      "deparse: name index",
      `Quick,
      () => {
        open Selector;
        let path = [
          MatchKeyword("let"),
          MatchNameIndex("x", 1),
          MatchDelimiter("="),
          MatchFocus,
        ];
        check(string, "deparse", "let x#1 = %", deparse(path));
      },
    ),
    test_case(
      "deparse: chain no trailing slash",
      `Quick,
      () => {
        open Selector;
        let path = [
          EnterBinderDef("A"),
          EnterBinderDef("B"),
          MatchName("x"),
          MatchDelimiter("="),
          MatchFocus,
        ];
        check(string, "deparse", "A/B/x = %", deparse(path));
      },
    ),
    test_case(
      "deparse: chain with trailing slash",
      `Quick,
      () => {
        open Selector;
        let path = [
          EnterBinderDef("A"),
          EnterBinderDef("B"),
          MatchKeyword("let"),
          MatchName("y"),
          MatchDelimiter("="),
          MatchFocus,
        ];
        check(string, "deparse", "A/B/ let y = %", deparse(path));
      },
    ),
    test_case(
      "deparse: single enter + name",
      `Quick,
      () => {
        open Selector;
        let path = [
          EnterBinderDef("M"),
          MatchName("x"),
          MatchDelimiter("="),
          MatchFocus,
        ];
        check(string, "deparse", "M/x = %", deparse(path));
      },
    ),
    test_case(
      "deparse: single enter trailing slash",
      `Quick,
      () => {
        open Selector;
        let path = [EnterBinderDef("M"), MatchFocus];
        check(string, "deparse", "M/ %", deparse(path));
      },
    ),
    /* === Named canonical path generation === */
    /* Helper tests: named_canonical produces readable selectors that resolve
       back to the same node as the numeric canonical */
    /* Let: def uses name */
    test_case(
      "named: let def",
      `Quick,
      () => {
        let root = mk_term("let x = 42 in x");
        switch (Selector.query_unique("x = %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "x = %", s);
            /* Roundtrip */
            switch (Selector.query_unique(s, root)) {
            | Error(e) => fail("roundtrip: " ++ e)
            | Ok(m2) =>
              check(bool, "roundtrip ID", true, m.focused_id == m2.focused_id)
            };
          }
        };
      },
    ),
    /* Let: body navigates through */
    test_case(
      "named: nested let body",
      `Quick,
      () => {
        let root = mk_term("let x = 1 in let y = 2 in x + y");
        switch (Selector.query_unique("y = %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "y = %", s);
          }
        };
      },
    ),
    /* If: keyword addressing */
    test_case(
      "named: if cond",
      `Quick,
      () => {
        let root = mk_term("if true then 1 else 0");
        switch (Selector.query_unique("if %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "if %", s);
          }
        };
      },
    ),
    test_case(
      "named: if then",
      `Quick,
      () => {
        let root = mk_term("if true then 1 else 0");
        switch (Selector.query_unique("if _ then %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "if _ then %", s);
          }
        };
      },
    ),
    test_case(
      "named: if else",
      `Quick,
      () => {
        let root = mk_term("if true then 1 else 0");
        switch (Selector.query_unique("if _... else %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "if _... else %", s);
          }
        };
      },
    ),
    /* Fun: keyword addressing */
    test_case(
      "named: fun body",
      `Quick,
      () => {
        let root = mk_term("let f = fun x -> x + 1 in f");
        switch (Selector.query_unique("f = #1", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            /* Fun body is x+1, accessible via f = fun _ -> * */
            check(string, "named path", "f = fun _ -> %", s);
          }
        };
      },
    ),
    /* BinOp inside def: name + index */
    test_case(
      "named: binop left in def",
      `Quick,
      () => {
        let root = mk_term("let x = 1 + 2 in x");
        switch (Selector.query_unique("x = #0", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "x = % + _", s);
          }
        };
      },
    ),
    /* BinOp: right operand named canonical */
    test_case(
      "named: binop right in def",
      `Quick,
      () => {
        let root = mk_term("let x = 1 + 2 in x");
        switch (Selector.query_unique("x = #1", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "x = _ + %", s);
            /* Roundtrip */
            switch (Selector.query_unique(s, root)) {
            | Error(e) => fail("roundtrip: " ++ e)
            | Ok(m2) =>
              check(bool, "roundtrip ID", true, m.focused_id == m2.focused_id)
            };
          }
        };
      },
    ),
    /* Cons: named canonical */
    test_case(
      "named: cons left",
      `Quick,
      () => {
        let root = mk_term("let x = 1 :: [2] in x");
        switch (Selector.query_unique("x = #0", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "x = % :: _", s);
            switch (Selector.query_unique(s, root)) {
            | Error(e) => fail("roundtrip: " ++ e)
            | Ok(m2) =>
              check(bool, "roundtrip ID", true, m.focused_id == m2.focused_id)
            };
          }
        };
      },
    ),
    /* Match: keyword + named rules */
    test_case(
      "named: case scrut",
      `Quick,
      () => {
        let root = mk_term("case x | A => 1 | B => 2 end");
        switch (Selector.query_unique("case %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "case %", s);
          }
        };
      },
    ),
    test_case(
      "named: case rule body",
      `Quick,
      () => {
        let root = mk_term("case x | A => 1 | B => 2 end");
        switch (Selector.query_unique("| B => %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "| _... B => %", s);
          }
        };
      },
    ),
    /* Shadowed names: indexing */
    test_case(
      "named: shadowed name",
      `Quick,
      () => {
        let root = mk_term("let x = 1 in let x = 2 in x");
        switch (Selector.query_unique("x#1 = %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "x#1 = %", s);
          }
        };
      },
    ),
    /* Seq: transparent navigation */
    test_case(
      "named: seq",
      `Quick,
      () => {
        let root = mk_term("let x = 1 in let y = 2 in x + y");
        switch (Selector.query_unique("x = %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named returned None")
          | Some(path) =>
            let s = Selector.deparse(path);
            check(string, "named path", "x = %", s);
            /* Roundtrip */
            switch (Selector.query_unique(s, root)) {
            | Error(e) => fail("roundtrip: " ++ e)
            | Ok(m2) =>
              check(bool, "roundtrip ID", true, m.focused_id == m2.focused_id)
            };
          }
        };
      },
    ),
    /* Numeric vs named comparison: both resolve to same node */
    test_case(
      "numeric vs named: same target",
      `Quick,
      () => {
        let root = mk_term("let x = 42 in x + 1");
        switch (Selector.query_unique("x = %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          let id = m.focused_id;
          let num = Selector.canonical_numeric(id, root);
          let named = Selector.canonical_named(id, root);
          switch (num, named) {
          | (Some(np), Some(nmp)) =>
            let ns = Selector.deparse(np);
            let nms = Selector.deparse(nmp);
            /* Both should resolve to same ID */
            switch (
              Selector.query_unique(ns, root),
              Selector.query_unique(nms, root),
            ) {
            | (Ok(m1), Ok(m2)) =>
              check(bool, "same ID", true, m1.focused_id == m2.focused_id);
              /* Named should be more readable */
              check(string, "numeric", "#1 %", ns);
              check(string, "named", "x = %", nms);
            | _ => fail("roundtrip failed")
            };
          | _ => fail("generation failed")
          };
        };
      },
    ),
    /* === MVU app: canonical roundtrips on a realistic program === */
    /* Helper: verify both canonical forms roundtrip for a given selector */
    test_case(
      "mvu: init def roundtrip",
      `Quick,
      () => {
        let root = mk_term(app_program);
        switch (Selector.query_unique("App/init = %", root)) {
        | Error(e) => fail("sel: " ++ e)
        | Ok(m) =>
          /* Numeric roundtrip */
          switch (Selector.canonical_numeric(m.focused_id, root)) {
          | None => fail("numeric None")
          | Some(np) =>
            let ns = Selector.deparse(np);
            switch (Selector.query_unique(ns, root)) {
            | Error(e) => fail("num roundtrip: " ++ e ++ " (" ++ ns ++ ")")
            | Ok(m2) =>
              check(bool, "num ID", true, m.focused_id == m2.focused_id)
            };
          };
          /* Named roundtrip */
          switch (Selector.canonical_named(m.focused_id, root)) {
          | None => fail("named None")
          | Some(nmp) =>
            let nms = Selector.deparse(nmp);
            switch (Selector.query_unique(nms, root)) {
            | Error(e) => fail("named roundtrip: " ++ e ++ " (" ++ nms ++ ")")
            | Ok(m2) =>
              check(bool, "named ID", true, m.focused_id == m2.focused_id)
            };
          };
        };
      },
    ),
    canonical_roundtrip(
      ~name="mvu: update case scrut",
      ~code=app_program,
      ~sel="App/update \\... case %",
    ),
    canonical_roundtrip(
      ~name="mvu: Inc arm body",
      ~code=app_program,
      ~sel="App/update \\... | Inc => %",
    ),
    canonical_roundtrip(
      ~name="mvu: Dec arm body",
      ~code=app_program,
      ~sel="App/update \\... | Dec => %",
    ),
    canonical_roundtrip(
      ~name="mvu: Reset arm body",
      ~code=app_program,
      ~sel="App/update \\... | Reset => %",
    ),
    canonical_roundtrip(
      ~name="mvu: view label def",
      ~code=app_program,
      ~sel="App/view \\... let label = %",
    ),
    canonical_roundtrip(
      ~name="mvu: result def",
      ~code=app_program,
      ~sel="result = %",
    ),
    /* ChildIndex on MVU: module items */
    sel_test(
      ~name="mvu: App = #0 #1 (init def)",
      ~code=app_program,
      ~sel="App = #0 #1",
      ~expected="0",
    ),
    /* === Spec 11.1: Module spine structural selectors === */
    sel_test_rendered(
      ~name="mvu: App = { % (first item)",
      ~code=app_program,
      ~sel="App = { %",
      ~expected="let init = 0",
    ),
    test_case(
      "mvu: App = { _ % (second item)",
      `Quick,
      () => {
        let result = selector_query_unique(app_program, "App = { _ %");
        check(
          bool,
          "starts with let update",
          true,
          String.length(result) >= 10
          && String.sub(result, 0, 10) == "let update",
        );
      },
    ),
    test_case(
      "mvu: App = { _ _ % (third item)",
      `Quick,
      () => {
        let result = selector_query_unique(app_program, "App = { _ _ %");
        check(
          bool,
          "starts with let view",
          true,
          String.length(result) >= 8
          && String.sub(result, 0, 8) == "let view",
        );
      },
    ),
    /* === Spec 11.2: Multi-module cross-module navigation === */
    /* Type defs inside module accessed via keyword + descent */
    sel_test(
      ~name="mm: \\... type point = %",
      ~code=multi_module_program,
      ~sel="\\... type point = %",
      ~expected="(Int, Int)",
    ),
    sel_test(
      ~name="mm: \\... type color = %",
      ~code=multi_module_program,
      ~sel="\\... type color = %",
      ~expected="(Int, Int, Int)",
    ),
    sel_test(
      ~name="mm: Geom/origin = %",
      ~code=multi_module_program,
      ~sel="Geom/origin = %",
      ~expected="(0, 0)",
    ),
    /* Chain into nested module — check non-error result */
    test_case(
      "mm: Geom/Shapes/circle = % (nested chain)",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            multi_module_program,
            "Geom/Shapes/circle = %",
          );
        check(
          bool,
          "not error",
          true,
          String.length(result) < 5 || String.sub(result, 0, 5) != "ERROR",
        );
      },
    ),
    test_case(
      "mm: Geom/Shapes/rect = % (sibling in nested)",
      `Quick,
      () => {
        let result =
          selector_query_unique(multi_module_program, "Geom/Shapes/rect = %");
        check(
          bool,
          "not error",
          true,
          String.length(result) < 5 || String.sub(result, 0, 5) != "ERROR",
        );
      },
    ),
    test_case(
      "mm: Render/draw = % (different module)",
      `Quick,
      () => {
        let result =
          selector_query_unique(multi_module_program, "Render/draw = %");
        check(
          bool,
          "not error",
          true,
          String.length(result) < 5 || String.sub(result, 0, 5) != "ERROR",
        );
      },
    ),
    /* Spec 11.2: Deep access within definitions */
    sel_test(
      ~name="mm: Geom/translate \\... let x = %",
      ~code=multi_module_program,
      ~sel="Geom/translate \\... let x = %",
      ~expected="p. (0) + dx",
    ),
    sel_test(
      ~name="mm: Geom/translate \\... let y = %",
      ~code=multi_module_program,
      ~sel="Geom/translate \\... let y = %",
      ~expected="p. (1)",
    ),
    sel_test(
      ~name="mm: Render/draw \\... if %",
      ~code=multi_module_program,
      ~sel="Render/draw \\... if %",
      ~expected="shape. (1) > 0",
    ),
    sel_test(
      ~name="mm: Render/draw \\... if _ then %",
      ~code=multi_module_program,
      ~sel="Render/draw \\... if _ then %",
      ~expected="color",
    ),
    sel_test(
      ~name="mm: Render/draw \\... if _... else %",
      ~code=multi_module_program,
      ~sel="Render/draw \\... if _... else %",
      ~expected="(0, 0, 0)",
    ),
    /* Spec 11.2: Module-level operations */
    sel_test(
      ~name="mm: module Types (name pat)",
      ~code=multi_module_program,
      ~sel="module Types",
      ~expected="Types",
    ),
    test_case(
      "mm: module Geom = % (whole body)",
      `Quick,
      () => {
        let result =
          selector_query_unique(multi_module_program, "module Geom = %");
        check(
          bool,
          "not error",
          true,
          String.length(result) < 5 || String.sub(result, 0, 5) != "ERROR",
        );
      },
    ),
    sel_test_rendered(
      ~name="mm: Geom = { % (first item)",
      ~code=multi_module_program,
      ~sel="Geom = { %",
      ~expected="let origin = (0, 0)",
    ),
    /* Spec 11.2: Wildcard queries */
    test_case(
      "mm: \\... fun _ -> % (all funs)",
      `Quick,
      () => {
        let results =
          selector_query(multi_module_program, "\\... fun _ -> %");
        check(
          bool,
          "at least 5 fun bodies",
          true,
          List.length(results) >= 5,
        );
      },
    ),
    test_case(
      "mm: \\... type _ = % (all type defs)",
      `Quick,
      () => {
        let results =
          selector_query(multi_module_program, "\\... type _ = %");
        check(int, "2 type defs", 2, List.length(results));
      },
    ),
    /* === Spec 11.3: Data processing pipeline === */
    sel_test(
      ~name="dp: users/ [ %",
      ~code=pipeline_program,
      ~sel="users/ [ %",
      ~expected="(\"Alice\", Active)",
    ),
    sel_test(
      ~name="dp: users/ [ _ %",
      ~code=pipeline_program,
      ~sel="users/ [ _ %",
      ~expected="(\"Bob\", Inactive)",
    ),
    sel_test(
      ~name="dp: users/ [ _... %",
      ~code=pipeline_program,
      ~sel="users/ [ _... %",
      ~expected="(\"Carol\", Pending)",
    ),
    /* Case arm access */
    sel_test(
      ~name="dp: is_active/ \\... case %",
      ~code=pipeline_program,
      ~sel="is_active/ \\... case %",
      ~expected="user. (1)",
    ),
    sel_test(
      ~name="dp: is_active/ \\... | Active => %",
      ~code=pipeline_program,
      ~sel="is_active/ \\... | Active => %",
      ~expected="true",
    ),
    sel_test(
      ~name="dp: is_active/ \\... | Pending => %",
      ~code=pipeline_program,
      ~sel="is_active/ \\... | Pending => %",
      ~expected="false",
    ),
    test_case(
      "dp: is_active/ \\... | _ => % (all arms)",
      `Quick,
      () => {
        let results =
          selector_query(pipeline_program, "is_active/ \\... | _ => %");
        check(int, "3 arm bodies", 3, List.length(results));
      },
    ),
    /* count base case via descent */
    sel_test(
      ~name="dp: count/ \\... | [] => %",
      ~code=pipeline_program,
      ~sel="count/ \\... | [] => %",
      ~expected="0",
    ),
    /* Cross-cutting */
    test_case(
      "dp: \\... case % (all scrutinees)",
      `Quick,
      () => {
        let results = selector_query(pipeline_program, "\\... case %");
        check(int, "2 scrutinees", 2, List.length(results));
      },
    ),
    test_case(
      "dp: \\... | _ => % (all arm bodies)",
      `Quick,
      () => {
        let results = selector_query(pipeline_program, "\\... | _ => %");
        check(int, "5 arm bodies", 5, List.length(results));
      },
    ),
    test_case(
      "dp: \\... fun _ -> % (all fun bodies)",
      `Quick,
      () => {
        let results = selector_query(pipeline_program, "\\... fun _ -> %");
        check(int, "2 fun bodies", 2, List.length(results));
      },
    ),
    /* Combined chain + module + atom */
    sel_test(
      ~name="combined: M/ \\... 42 (chain + descent + atom)",
      ~code="module M = { let a = 42; let b = 99 } in M.a",
      ~sel="M/ \\... 42",
      ~expected="42",
    ),
  ],
);

/* Geometry program for gap tests */
let geometry_program =
  "module Geometry = { "
  ++ "type Point = (Int, Int); "
  ++ "type Distance = Int; "
  ++ "let origin : Point = (0, 0); "
  ++ "let manhattan = fun (x, y) : Point -> x + y "
  ++ "} in "
  ++ "Geometry.origin";

/* === Gap Tests: implementation targets grouped by feature === */

let gap_tests = (
  "AgentTools.Gaps",
  [
    /* --- Gap 1: Bare name atom matching --- */
    /* Bare name should match variable references (atoms) */
    sel_test(
      ~name="bare name: x matches var ref",
      ~code="let y = 1 in x",
      ~sel="x",
      ~expected="x",
    ),
    test_case(
      "bare name: x matches pat + both refs in x + x",
      `Quick,
      () => {
        let results = selector_query("let x = 1 in x + x", "x");
        check(int, "3 matches (pat + 2 refs)", 3, List.length(results));
      },
    ),
    sel_test(
      ~name="bare name: constructor matches",
      ~code="let x = Active in x",
      ~sel="Active",
      ~expected="Active",
    ),
    /* --- Gap 2: Chain into type/module binders --- */
    /* find_all_binders_named must find type and module binders */
    sel_test(
      ~name="chain: type binder via chain",
      ~code=geometry_program,
      ~sel="Geometry/Point/",
      ~expected="(Int, Int)",
    ),
    sel_test(
      ~name="chain: type binder Distance via chain",
      ~code=geometry_program,
      ~sel="Geometry/Distance/",
      ~expected="Int",
    ),
    sel_test(
      ~name="chain: let binder still works",
      ~code=geometry_program,
      ~sel="Geometry/origin = %",
      ~expected="(0, 0)",
    ),
    /* Module binder inside module via chain */
    sel_test(
      ~name="chain: nested module via chain",
      ~code="module A = { module B = { let x = 42 } } in A.B.x",
      ~sel="A/B/x = %",
      ~expected="42",
    ),
    /* --- Gap 3: Tuple spine access via ( delimiter --- */
    sel_test(
      ~name="tuple: ( % first element",
      ~code="let t = (10, 20, 30) in t",
      ~sel="t/ ( %",
      ~expected="10",
    ),
    sel_test(
      ~name="tuple: ( _ % second element",
      ~code="let t = (10, 20, 30) in t",
      ~sel="t/ ( _ %",
      ~expected="20",
    ),
    sel_test(
      ~name="tuple: ( _ _ % third element",
      ~code="let t = (10, 20, 30) in t",
      ~sel="t/ ( _ _ %",
      ~expected="30",
    ),
    sel_test(
      ~name="tuple: origin = ( after chain",
      ~code=geometry_program,
      ~sel="Geometry/origin = ( %",
      ~expected="0",
    ),
    /* --- Gap 4: Trailing token prefix matching --- */
    /* TODO: post-focus spine matching (e.g., `fun _ -> % +` where `+` confirms
       structure inside the focused body). Deferred — requires architectural
       change to support matching constraints after the focus point. */
    test_case("trailing: fun _ -> % + (prefix match into body)", `Quick, () =>
      Alcotest.skip()
    ),
    /* Simpler version: infix trailing token */
    sel_test(
      ~name="trailing: x = _ + % (right of +)",
      ~code="let x = 1 + 2 in x",
      ~sel="x = _ + %",
      ~expected="2",
    ),
    /* --- Gap 5: % before keyword (whole-form focus) --- */
    test_case(
      "focus-keyword: Geometry/ % let (all lets)",
      `Quick,
      () => {
        let results = selector_query(geometry_program, "Geometry/ % let");
        check(bool, "at least 2 lets", true, List.length(results) >= 2);
      },
    ),
    /* --- Gap 6: Module spine entry via { from walk --- */
    /* \... { should find Module nodes and enter spine */
    test_case(
      "mod-spine-descent: \\... { type (find type in module)",
      `Quick,
      () => {
        let results = selector_query(geometry_program, "\\... { type");
        check(bool, "at least 1 match", true, List.length(results) >= 1);
      },
    ),
    sel_test(
      ~name="mod-spine-descent: \\... { type Point = %",
      ~code=geometry_program,
      ~sel="\\... { type Point = %",
      ~expected="(Int, Int)",
    ),
    sel_test(
      ~name="mod-spine-descent: \\... { _... let origin",
      ~code=geometry_program,
      ~sel="\\... { _... let origin",
      ~expected="origin",
    ),
    /* --- Spec coverage: selectors from 11.2 not yet tested --- */
    /* Chain into type defs (distinct from \... type) */
    sel_test(
      ~name="spec11.2: Types/point = %",
      ~code=multi_module_program,
      ~sel="Types/point = %",
      ~expected="(Int, Int)",
    ),
    sel_test(
      ~name="spec11.2: Types/color = %",
      ~code=multi_module_program,
      ~sel="Types/color = %",
      ~expected="(Int, Int, Int)",
    ),
    /* Deep access: chain + descend + fun body (2 nested funs) */
    test_case(
      "spec11.2: Geom/translate \\... fun _ -> %",
      `Quick,
      () => {
        let results =
          selector_query(
            multi_module_program,
            "Geom/translate \\... fun _ -> %",
          );
        check(int, "2 nested funs", 2, List.length(results));
      },
    ),
    /* Module-level: last item via ellipsis */
    test_case(
      "spec11.2: Geom = { _... % (last item)",
      `Quick,
      () => {
        let result =
          selector_query_unique(multi_module_program, "Geom = { _... %");
        check(
          bool,
          "not error",
          true,
          String.length(result) < 5 || String.sub(result, 0, 5) != "ERROR",
        );
      },
    ),
    /* Item by name inside module spine */
    test_case(
      "spec11.2: Geom = { _... let translate = %",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            multi_module_program,
            "Geom = { _... let translate = %",
          );
        check(
          bool,
          "not error",
          true,
          String.length(result) < 5 || String.sub(result, 0, 5) != "ERROR",
        );
      },
    ),
    /* Descend to nested module */
    test_case(
      "spec11.2: \\... module Shapes = %",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            multi_module_program,
            "\\... module Shapes = %",
          );
        check(
          bool,
          "not error",
          true,
          String.length(result) < 5 || String.sub(result, 0, 5) != "ERROR",
        );
      },
    ),
    /* Chain into nested module + spine */
    test_case(
      "spec11.2: Geom/Shapes = { % (first item)",
      `Quick,
      () => {
        let result =
          selector_query_unique(multi_module_program, "Geom/Shapes = { %");
        check(
          bool,
          "contains let circle",
          true,
          string_contains("let circle", result),
        );
      },
    ),
    test_case(
      "spec11.2: Geom/Shapes = { _ % (second item)",
      `Quick,
      () => {
        let result =
          selector_query_unique(multi_module_program, "Geom/Shapes = { _ %");
        check(
          bool,
          "contains let rect",
          true,
          string_contains("let rect", result),
        );
      },
    ),
    /* Spec 11.3: cons pattern arm — requires compound pattern matching
       in pipe walker (e.g., matching Cons(_, Var("tl")) via spine tokens).
       Skipped until spine-matching within arm patterns is implemented. */
    test_case(
      "spec11.3: count/ \\... | _ :: tl => %",
      `Quick,
      () => {
        let _ = Alcotest.skip();
        let result =
          selector_query_unique(
            pipeline_program,
            "count/ \\... | _ :: tl => %",
          );
        check(
          bool,
          "not error",
          true,
          String.length(result) < 5 || String.sub(result, 0, 5) != "ERROR",
        );
      },
    ),
    /* --- User-reported selectors from geometry_program --- */
    /* Tuple spine with explicit comma separator */
    sel_test(
      ~name="user: Geometry/origin = ( _ , %",
      ~code=geometry_program,
      ~sel="Geometry/origin = ( _ , %",
      ~expected="0",
    ),
    /* Module spine descent: {type % (focus on type name) */
    test_case(
      "user: \\... { type % (type name pat)",
      `Quick,
      () => {
        let results = selector_query(geometry_program, "\\... { type %");
        check(bool, "at least 1 match", true, List.length(results) >= 1);
      },
    ),
    /* Module spine descent: {type Point = % _... */
    sel_test(
      ~name="user: \\... { type Point = % (type def)",
      ~code=geometry_program,
      ~sel="\\... { type Point = %",
      ~expected="(Int, Int)",
    ),
    /* Module spine descent: { _... let _ = % (wildcard let) */
    test_case(
      "user: \\... { _... let _ = % (any let def)",
      `Quick,
      () => {
        let results =
          selector_query(geometry_program, "\\... { _... let _ = %");
        check(bool, "at least 1 match", true, List.length(results) >= 1);
      },
    ),
  ],
);

/* === Selector Edit Tests === */

let selector_edit_tests = (
  "AgentTools.SelectorEdits",
  [
    /* SelectorUpdate: replace the focused subtree with new code */
    edit_test(
      "SelectorUpdate: let x = % -> 99",
      "let x = 42 in x + 1",
      SelectorUpdate("let x = %", "99"),
      "let x = 99 in x + 1",
    ),
    edit_test(
      "SelectorUpdate: if else branch",
      "if true then 1 else 0",
      SelectorUpdate("if _... else %", "42"),
      "if true then 1 else 42",
    ),
    edit_test(
      "SelectorUpdate: nested via descend",
      "let f = fun x -> if x > 0 then x else 0 in f 5",
      SelectorUpdate("\\... if _... else %", "1"),
      "let f = fun x -> if x > 0 then x else 1 in f 5",
    ),
    edit_test(
      "SelectorUpdate: case arm body",
      "let r = case x | A => 1 | B => 2 end in r",
      SelectorUpdate("\\... | B => %", "99"),
      "let r = case x | A => 1 | B => 99 end in r",
    ),
    edit_test(
      "SelectorUpdate: module member def",
      "module M = { let x = 1; let y = 2 } in M.x",
      SelectorUpdate("M/x = %", "42"),
      "module M = { let x = 42; let y = 2 } in M.x",
    ),
    /* SelectorUpdate: FocusMod — replace whole module item */
    edit_test(
      "SelectorUpdate: whole module item (FocusMod)",
      "module M = { let x = 1; let y = 2 } in M.y",
      SelectorUpdate("M/x", "let z = 99"),
      "module M = { let z = 99; let y = 2 } in M.y",
    ),
    /* SelectorDelete: FocusMod — remove module item entirely */
    edit_test(
      "SelectorDelete: module item removal (FocusMod)",
      "module M = { let x = 1; let y = 2 } in M.y",
      SelectorDelete("M/x"),
      "module M = { let y = 2 } in M.y",
    ),
    /* SelectorUpdate: cross-sort (FocusTyp) */
    edit_test(
      "SelectorUpdate: type annotation Int -> Bool",
      "let x : Int = 42 in x",
      SelectorUpdate("let x : %", "Bool"),
      "let x : Bool = 42 in x",
    ),
    edit_test(
      "SelectorUpdate: type def in type alias",
      "type T = Int in let x : T = 42 in x",
      SelectorUpdate("type T = %", "Bool"),
      "type T = Bool in let x : T = 42 in x",
    ),
    /* SelectorDelete: replace focused subtree with hole */
    edit_test(
      "SelectorDelete: let def -> hole",
      "let x = 42 in x + 1",
      SelectorDelete("let x = %"),
      "let x = ? in x + 1",
    ),
    edit_test(
      "SelectorDelete: type annotation -> type hole",
      "let x : Int = 42 in x",
      SelectorDelete("let x : %"),
      "let x : ? = 42 in x",
    ),
    /* SelectorUpdate/Delete: cross-sort (FocusPat) */
    edit_test(
      "SelectorUpdate: pattern via #0",
      "let x = 42 in x",
      SelectorUpdate("#0", "y"),
      "let y = 42 in x",
    ),
    edit_test(
      "SelectorDelete: pattern via #0 -> hole",
      "let x = 42 in x",
      SelectorDelete("#0"),
      "let ? = 42 in x",
    ),
    /* Error cases */
    test_case("SelectorUpdate: no match", `Quick, () => {
      switch (
        run_agent_action("let x = 1 in x", SelectorUpdate("let y = %", "2"))
      ) {
      | Ok(_) => Alcotest.fail("Expected failure: no match")
      | Error(Action.Failure.Composition_action_failure(msg)) =>
        check(
          bool,
          "error mentions no match",
          true,
          String.length(msg) >= 8 && String.sub(msg, 0, 8) == "No match",
        )
      | Error(err) =>
        Alcotest.fail("Unexpected error: " ++ Action.Failure.show(err))
      }
    }),
    test_case("SelectorUpdate: ambiguous match", `Quick, () => {
      switch (
        run_agent_action(
          "let a = 1 in let b = 2 in a + b",
          SelectorUpdate("let _ = %", "0"),
        )
      ) {
      | Ok(_) => Alcotest.fail("Expected failure: ambiguous")
      | Error(Action.Failure.Composition_action_failure(msg)) =>
        check(
          bool,
          "error mentions ambiguous",
          true,
          String.length(msg) >= 9 && String.sub(msg, 0, 9) == "Ambiguous",
        )
      | Error(err) =>
        Alcotest.fail("Unexpected error: " ++ Action.Failure.show(err))
      }
    }),
    /* === SelectorInsert tests === */
    /* InsertAfter: insert let binding after anchor */
    edit_test(
      "SelectorInsertAfter: let after anchor",
      "let x = 1 in x + 1",
      SelectorInsertAfter("% let x", "let y = 2"),
      "let x = 1 in let y = 2 in x + 1",
    ),
    /* InsertBefore: insert let binding before anchor */
    edit_test(
      "SelectorInsertBefore: let before anchor",
      "let x = 1 in x + 1",
      SelectorInsertBefore("% let x", "let y = 2"),
      "let y = 2 in let x = 1 in x + 1",
    ),
    /* InsertAfter in module: insert after a module item */
    edit_test(
      "SelectorInsertAfter: module item",
      "module M = { let x = 1 } in M.x",
      SelectorInsertAfter("M/x = %", "let y = 2"),
      /* Space before ; is from original item's stored after-secondary
         (was space before } in original code). Cosmetic artifact of
         PreserveExact preserving positional whitespace. */
      "module M = { let x = 1 ; let y = 2 } in M.x",
    ),
    /* InsertBefore in module: insert before a module item */
    edit_test(
      "SelectorInsertBefore: module item",
      "module M = { let x = 1 } in M.x",
      SelectorInsertBefore("M/x = %", "let y = 0"),
      "module M = { let y = 0; let x = 1 } in M.x",
    ),
    /* Error: selector no match */
    test_case("SelectorInsertAfter: no match", `Quick, () => {
      switch (
        run_agent_action(
          "let x = 1 in x",
          SelectorInsertAfter("% let z", "let y = 2"),
        )
      ) {
      | Ok(_) => Alcotest.fail("Expected failure: no match")
      | Error(Action.Failure.Composition_action_failure(msg)) =>
        check(
          bool,
          "error mentions no match",
          true,
          str_contains(msg, "No match"),
        )
      | Error(err) =>
        Alcotest.fail("Unexpected error: " ++ Action.Failure.show(err))
      }
    }),
  ],
);

/* === Whitespace / Line Break Preservation Tests === */

let whitespace_tests = (
  "AgentTools.WhitespacePreservation",
  [
    /* --- Existing line breaks preserved through edits --- */
    edit_test(
      "update_definition preserves line breaks",
      "let a = 1\nin a",
      Update(Definition, "a", "42"),
      "let a = 42\nin a",
    ),
    edit_test(
      "update_body preserves line breaks",
      "let a = 1\nin let b = 2\nin a + b",
      Update(Body, "b", "b + 1"),
      "let a = 1\nin let b = 2\nin b + 1",
    ),
    edit_test(
      "update_definition in multi-line let chain",
      "let x = 1\nin let y = 2\nin let z = 3\nin x + y + z",
      Update(Definition, "y", "20"),
      "let x = 1\nin let y = 20\nin let z = 3\nin x + y + z",
    ),
    edit_test(
      "selector_update preserves line breaks in def",
      "let x = 1\nin let y = 2\nin x + y",
      SelectorUpdate("x = %", "99"),
      "let x = 99\nin let y = 2\nin x + y",
    ),
    edit_test(
      "selector_update preserves line breaks in body",
      "let x = 1\nin let y = 2\nin x + y",
      SelectorUpdate("\\... y = %", "20"),
      "let x = 1\nin let y = 20\nin x + y",
    ),
    edit_test(
      "delete_body preserves line breaks",
      "let a = 1\nin let b = 2\nin a + b",
      Delete(Body, "b"),
      "let a = 1\nin let b = 2\nin ?",
    ),
    edit_test(
      "delete_binding preserves surrounding line breaks",
      "let a = 1\nin let b = 2\nin let c = 3\nin a + c",
      Delete(BindingClause, "b"),
      "let a = 1\nin let c = 3\nin a + c",
    ),
    edit_test(
      "selector_delete preserves line breaks",
      "let x = 1\nin let y = 2\nin x + y",
      SelectorDelete("x = %"),
      "let x = ?\nin let y = 2\nin x + y",
    ),
    /* --- New bindings get appropriate line breaks --- */
    edit_test(
      "insert_after adds newline for new binding",
      "let a = 1\nin a + 1",
      Insert(After, "a", "let b = 2"),
      "let a = 1\nin let b = 2\nin a + 1",
    ),
    edit_test(
      "insert_before adds newline for new binding",
      "let a = 1\nin a + 1",
      Insert(Before, "a", "let b = 0"),
      "let b = 0\nin let a = 1\nin a + 1",
    ),
    edit_test(
      "selector_insert_after with line breaks",
      "let x = 1\nin x + 1",
      SelectorInsertAfter("% let x", "let y = 2"),
      "let x = 1\nin let y = 2\nin x + 1",
    ),
    edit_test(
      "selector_insert_before with line breaks",
      "let x = 1\nin x + 1",
      SelectorInsertBefore("% let x", "let y = 2"),
      "let y = 2\nin let x = 1\nin x + 1",
    ),
    /* --- Pattern/type updates preserve line breaks --- */
    edit_test(
      "update_pattern preserves line breaks",
      "let a = 1\nin let b = a + 1\nin b",
      Update(Pattern, "a", "x"),
      "let x = 1\nin let b = x + 1\nin b",
    ),
    /* --- Case arm with line breaks --- */
    edit_test(
      "selector_update in case arm preserves breaks",
      "let r = case x\n| A => 1\n| B => 2\nend\nin r",
      SelectorUpdate("\\... | B => %", "99"),
      "let r = case x\n| A => 1\n| B => 99\nend\nin r",
    ),
  ],
);

/* === GetCanonical Read Action Tests === */

let canonical_read_tests = (
  "AgentTools.GetCanonical",
  [
    test_case(
      "get_canonical for named binding",
      `Quick,
      () => {
        let result =
          run_read_action("let x = 42 in x + 1", GetCanonical("x = %"));
        check(string, "canonical", "numeric: #1 %\nnamed: x = %", result);
      },
    ),
    test_case(
      "get_canonical for nested def",
      `Quick,
      () => {
        let result =
          run_read_action("let x = 1 + 2 in x", GetCanonical("x = #0"));
        check(
          string,
          "canonical",
          "numeric: #1 #0 %\nnamed: x = % + _",
          result,
        );
      },
    ),
    test_case(
      "get_canonical error for bad selector",
      `Quick,
      () => {
        let z = mk_zipper("let x = 42 in x");
        switch (
          CompositionGo.Public.read_dispatch(
            ~action=GetCanonical("nonexistent = %"),
            ~z,
          )
        ) {
        | Error(_) => () /* expected */
        | Ok(s) => Alcotest.fail("Expected error, got: " ++ s)
        };
      },
    ),
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

/* === Completeness Tests === */

let expect_completeness = (code: string, expected: string) =>
  check(
    string,
    "completeness",
    expected,
    run_read_action(code, GetCompleteness),
  );

let completeness_tests = (
  "AgentTools.Completeness",
  [
    test_case("complete program", `Quick, () =>
      expect_completeness(
        "let x = 42 in x + 1",
        "Complete: no unfilled holes.",
      )
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

/* Programs with sum types, records, case dispatch, and functions */
let sum_type_program = "type Color = Red + Green + Blue in\nlet name_of : Color -> String = fun c ->\n  case c\n  | Red => \"red\"\n  | Green => \"green\"\n  | Blue => \"blue\"\n  end\nin\nname_of(Red)";

let record_program = "let mk_point = fun x -> fun y -> (x=x, y=y) in\nlet dist = fun p -> p.x * p.x + p.y * p.y in\nlet origin = mk_point(0)(0) in\ndist(origin)";

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
    test_case(
      "update case arm body",
      `Quick,
      () => {
        let result =
          apply_and_render(
            sum_type_program,
            Update(
              Definition,
              "name_of",
              "fun c ->\n  case c\n  | Red => \"RED\"\n  | Green => \"green\"\n  | Blue => \"blue\"\n  end",
            ),
          );
        check(bool, "has RED", true, string_contains("RED", result));
      },
    ),
    /* Sum type: read statics on annotated function */
    test_case(
      "statics on sum type function",
      `Quick,
      () => {
        let result =
          run_read_action(sum_type_program, GetStatics("name_of"));
        check(bool, "has String", true, string_contains("String", result));
        check(bool, "has name_of", true, string_contains("name_of", result));
      },
    ),
    /* Sum type: context shows constructors */
    test_case(
      "context has constructors",
      `Quick,
      () => {
        let result =
          run_read_action(sum_type_program, GetContext("name_of"));
        check(bool, "has Red", true, string_contains("Red", result));
        check(bool, "has Green", true, string_contains("Green", result));
        check(bool, "has Blue", true, string_contains("Blue", result));
      },
    ),
    /* Sum type: selector on case arm via pipe */
    test_case(
      "selector on case arm",
      `Quick,
      () => {
        let result =
          selector_query_unique(
            sum_type_program,
            "name_of = \\... | Green => %",
          );
        check_rendered("green arm", "\"green\"", result);
      },
    ),
    /* Sum type: completeness */
    test_case("sum type program is complete", `Quick, () =>
      expect_completeness(sum_type_program, "Complete: no unfilled holes.")
    ),
    /* Record: update definition */
    test_case(
      "update record function def",
      `Quick,
      () => {
        let result =
          apply_and_render(
            record_program,
            Update(Definition, "origin", "mk_point(1)(1)"),
          );
        check(bool, "has 1)(1)", true, string_contains("1)(1)", result));
      },
    ),
    /* Record: insert new binding */
    test_case(
      "insert after record binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            record_program,
            Insert(After, "dist", "let manhattan = fun p -> p.x + p.y"),
          );
        check(
          bool,
          "has manhattan",
          true,
          string_contains("manhattan", result),
        );
      },
    ),
    /* Updating type alias without cascading errors */
    test_case(
      "update type alias def",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let x = 5 in x",
            Update(Definition, "T", "Bool"),
          );
        check_rendered(
          "type alias change",
          "type T = Bool in let x = 5 in x",
          result,
        );
      },
    ),
    /* Delete a binding clause */
    test_case(
      "delete binding in chain",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in c",
            Delete(BindingClause, "b"),
          );
        check_rendered("delete b", "let a = 1 in let c = 3 in c", result);
      },
    ),
    /* Selector with descendant search into function */
    test_case(
      "selector descend into function if",
      `Quick,
      () => {
        let code = "let f = fun x -> if x > 0 then x else 0 - x in f(5)";
        let result =
          selector_query_unique(code, "let f = \\... if _... else %");
        check_rendered("else branch", "0 - x", result);
      },
    ),
  ],
);

/* === Case Arm TermEdit Tests === */

/* Helper: find the nth arm body ID from a zipper's term */
let find_arm_body_id_in_zipper = (z: Zipper.t, arm_idx: int): Id.t => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let result = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Match(_, arms) when result^ == None =>
            switch (List.nth_opt(arms, arm_idx)) {
            | Some((_, body)) => result := Some(Exp.rep_id(body))
            | None => ()
            };
            e;
          | _ => continue(e)
          }
        },
      term,
    );
  switch (result^) {
  | Some(id) => id
  | None => Alcotest.fail("No case arm at index " ++ string_of_int(arm_idx))
  };
};

/* Helper: apply a TermEdit case arm operation on code, using shared zipper */
let case_arm_edit =
    (code: string, arm_idx: int, f: (Zipper.t, Id.t) => option(Zipper.t))
    : string => {
  let z = mk_zipper(code);
  let body_id = find_arm_body_id_in_zipper(z, arm_idx);
  switch (f(z, body_id)) {
  | Some(new_z) => render_zipper(new_z)
  | None => Alcotest.fail("Case arm operation returned None")
  };
};

let simple_case = "case x | A => 1 | B => 2 | C => 3 end";
let case_in_let = "let f = fun x -> case x | Some(v) => v | None => 0 end in f";

let case_arm_tests = (
  "AgentTools.CaseArms",
  [
    /* Delete case arms */
    test_case(
      "delete first case arm",
      `Quick,
      () => {
        let result =
          case_arm_edit(simple_case, 0, (z, id) =>
            TermEdit.case_delete_arm(z, id)
          );
        check(bool, "no A", false, string_contains("A", result));
        check(bool, "has B", true, string_contains("B", result));
        check(bool, "has C", true, string_contains("C", result));
      },
    ),
    test_case(
      "delete middle case arm",
      `Quick,
      () => {
        let result =
          case_arm_edit(simple_case, 1, (z, id) =>
            TermEdit.case_delete_arm(z, id)
          );
        check(bool, "has A", true, string_contains("A", result));
        check(bool, "no B", false, string_contains("B", result));
        check(bool, "has C", true, string_contains("C", result));
      },
    ),
    test_case(
      "delete last case arm",
      `Quick,
      () => {
        let result =
          case_arm_edit(simple_case, 2, (z, id) =>
            TermEdit.case_delete_arm(z, id)
          );
        check(bool, "has A", true, string_contains("A", result));
        check(bool, "has B", true, string_contains("B", result));
        check(bool, "no C", false, string_contains("C", result));
      },
    ),
    /* Insert case arms */
    test_case(
      "insert arm after last",
      `Quick,
      () => {
        let result =
          case_arm_edit(simple_case, 2, (z, id) =>
            TermEdit.case_insert_arm(z, id, "D => 4", Direction.Right)
          );
        check(bool, "has D", true, string_contains("D", result));
        check(bool, "has 4", true, string_contains("4", result));
      },
    ),
    test_case(
      "insert arm before first",
      `Quick,
      () => {
        let result =
          case_arm_edit(simple_case, 0, (z, id) =>
            TermEdit.case_insert_arm(z, id, "Z => 0", Direction.Left)
          );
        check(bool, "has Z", true, string_contains("Z", result));
      },
    ),
    test_case(
      "insert arm with pipe prefix",
      `Quick,
      () => {
        let result =
          case_arm_edit(simple_case, 1, (z, id) =>
            TermEdit.case_insert_arm(z, id, "| D => 4", Direction.Right)
          );
        check(bool, "has D", true, string_contains("D", result));
      },
    ),
    /* Update arm body */
    test_case(
      "update arm body",
      `Quick,
      () => {
        let result =
          case_arm_edit(simple_case, 0, (z, id) =>
            TermEdit.case_update_arm_body(z, id, "100")
          );
        check(bool, "has 100", true, string_contains("100", result));
        check(bool, "has B", true, string_contains("B", result));
      },
    ),
    /* Update arm pattern */
    test_case(
      "update arm pattern",
      `Quick,
      () => {
        let result =
          case_arm_edit(simple_case, 1, (z, id) =>
            TermEdit.case_update_arm_pattern(z, id, "D")
          );
        check(bool, "has D", true, string_contains("D", result));
        check(bool, "no B", false, string_contains("| B", result));
      },
    ),
    /* is_case_arm */
    test_case(
      "is_case_arm true for arm body",
      `Quick,
      () => {
        let z = mk_zipper(simple_case);
        let body_id = find_arm_body_id_in_zipper(z, 0);
        check(bool, "is case arm", true, TermEdit.is_case_arm(z, body_id));
      },
    ),
    test_case(
      "is_case_arm false for non-arm",
      `Quick,
      () => {
        let z = mk_zipper("let x = 1 in x");
        let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
        let id = Exp.rep_id(term);
        check(bool, "not case arm", false, TermEdit.is_case_arm(z, id));
      },
    ),
    /* Case arm in let binding */
    test_case(
      "delete arm in let binding",
      `Quick,
      () => {
        let result =
          case_arm_edit(case_in_let, 1, (z, id) =>
            TermEdit.case_delete_arm(z, id)
          );
        check(bool, "no None", false, string_contains("None", result));
        check(bool, "has Some", true, string_contains("Some", result));
      },
    ),
    test_case(
      "insert arm in let binding",
      `Quick,
      () => {
        let result =
          case_arm_edit(case_in_let, 1, (z, id) =>
            TermEdit.case_insert_arm(
              z,
              id,
              "Err(e) => 0 - 1",
              Direction.Right,
            )
          );
        check(bool, "has Err", true, string_contains("Err", result));
      },
    ),
    /* parse_case_arm directly */
    test_case("parse_case_arm simple", `Quick, () => {
      switch (TermEdit.parse_case_arm("Foo => 42")) {
      | Some(_) => ()
      | None => Alcotest.fail("Failed to parse case arm")
      }
    }),
    test_case("parse_case_arm with pipe", `Quick, () => {
      switch (TermEdit.parse_case_arm("| Bar(x) => x + 1")) {
      | Some(_) => ()
      | None => Alcotest.fail("Failed to parse case arm with pipe")
      }
    }),
    /* Case arms in HighLevelNodeMap */
    test_case(
      "case arms appear in node map",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 | C => 3 end in f";
        let node_map = build_node_map(case_code);
        /* Arms should be children of f, named |A, |B, |C */
        check(
          bool,
          "|A exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "f/|A") != None,
        );
        check(
          bool,
          "|B exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "f/|B") != None,
        );
        check(
          bool,
          "|C exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "f/|C") != None,
        );
      },
    ),
    test_case(
      "case arm path resolves correctly",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 end in f";
        let result = run_read_action(case_code, GetSyntax("f/|A"));
        check_rendered("arm A body", "1", result);
      },
    ),
    test_case(
      "case arm path resolves B",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 end in f";
        let result = run_read_action(case_code, GetSyntax("f/|B"));
        check_rendered("arm B body", "2", result);
      },
    ),
    /* Case arm edit via dispatch */
    test_case(
      "update case arm body via dispatch",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 end in f";
        let result =
          apply_and_render(case_code, Update(Body, "f/|A", "100"));
        check(bool, "has 100", true, string_contains("100", result));
        check(bool, "still has B", true, string_contains("B", result));
      },
    ),
    test_case(
      "update case arm pattern via dispatch",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 end in f";
        let result =
          apply_and_render(case_code, Update(Pattern, "f/|B", "C"));
        check(bool, "has C", true, string_contains("C", result));
        check(bool, "no B arm", false, string_contains("| B", result));
      },
    ),
    test_case(
      "delete case arm via dispatch",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 | C => 3 end in f";
        let result =
          apply_and_render(case_code, Delete(BindingClause, "f/|B"));
        check(bool, "no B", false, string_contains("B", result));
        check(bool, "has A", true, string_contains("A", result));
        check(bool, "has C", true, string_contains("C", result));
      },
    ),
    test_case(
      "insert case arm via dispatch",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 end in f";
        let result =
          apply_and_render(case_code, Insert(After, "f/|B", "C => 3"));
        check(bool, "has C", true, string_contains("C", result));
        check(bool, "has 3", true, string_contains("3", result));
      },
    ),
    test_case(
      "insert case arm before via dispatch",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 end in f";
        let result =
          apply_and_render(case_code, Insert(Before, "f/|A", "Z => 0"));
        check(bool, "has Z", true, string_contains("Z", result));
      },
    ),
  ],
);

/* === List Element Tests === */

/* Helper: find nth element ID in the first ListLit in the term */
let find_list_element_id_in_zipper = (z: Zipper.t, idx: int): Id.t => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let result = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | ListLit(elements) when result^ == None =>
            switch (List.nth_opt(elements, idx)) {
            | Some(el) => result := Some(Exp.rep_id(el))
            | None => ()
            };
            e;
          | _ => continue(e)
          }
        },
      term,
    );
  switch (result^) {
  | Some(id) => id
  | None => Alcotest.fail("No list element at index " ++ string_of_int(idx))
  };
};

let list_edit =
    (code: string, idx: int, f: (Zipper.t, Id.t) => option(Zipper.t)): string => {
  let z = mk_zipper(code);
  let el_id = find_list_element_id_in_zipper(z, idx);
  switch (f(z, el_id)) {
  | Some(new_z) => render_zipper(new_z)
  | None => Alcotest.fail("List operation returned None")
  };
};

let list_program = "let xs = [1, 2, 3] in xs";

let list_element_tests = (
  "AgentTools.ListElements",
  [
    /* TermEdit-level operations */
    test_case(
      "delete first list element",
      `Quick,
      () => {
        let result =
          list_edit(list_program, 0, (z, id) =>
            TermEdit.list_delete_element(z, id)
          );
        check(bool, "no 1", false, string_contains("1,", result));
        check(bool, "has 2", true, string_contains("2", result));
        check(bool, "has 3", true, string_contains("3", result));
      },
    ),
    test_case(
      "delete last list element",
      `Quick,
      () => {
        let result =
          list_edit(list_program, 2, (z, id) =>
            TermEdit.list_delete_element(z, id)
          );
        check(bool, "has 1", true, string_contains("1", result));
        check(bool, "has 2", true, string_contains("2", result));
      },
    ),
    test_case(
      "insert list element after",
      `Quick,
      () => {
        let result =
          list_edit(list_program, 2, (z, id) =>
            TermEdit.list_insert_element(z, id, "4", Direction.Right)
          );
        check(bool, "has 4", true, string_contains("4", result));
      },
    ),
    test_case(
      "update list element",
      `Quick,
      () => {
        let result =
          list_edit(list_program, 1, (z, id) =>
            TermEdit.list_update_element(z, id, "99")
          );
        check(bool, "has 99", true, string_contains("99", result));
      },
    ),
    /* Node map indexing */
    test_case(
      "list elements in node map",
      `Quick,
      () => {
        let node_map = build_node_map(list_program);
        check(
          bool,
          "[0] exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "xs/[0]") != None,
        );
        check(
          bool,
          "[1] exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "xs/[1]") != None,
        );
        check(
          bool,
          "[2] exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "xs/[2]") != None,
        );
      },
    ),
    /* Dispatch-level operations */
    test_case(
      "update list element via dispatch",
      `Quick,
      () => {
        let result =
          apply_and_render(list_program, Update(Body, "xs/[1]", "99"));
        check(bool, "has 99", true, string_contains("99", result));
      },
    ),
    test_case(
      "delete list element via dispatch",
      `Quick,
      () => {
        let result =
          apply_and_render(list_program, Delete(BindingClause, "xs/[1]"));
        check(bool, "has 1", true, string_contains("1", result));
        check(bool, "has 3", true, string_contains("3", result));
      },
    ),
    test_case(
      "insert list element via dispatch",
      `Quick,
      () => {
        let result =
          apply_and_render(list_program, Insert(After, "xs/[2]", "4"));
        check(bool, "has 4", true, string_contains("4", result));
      },
    ),
  ],
);

/* === Tuple Element Tests === */

let find_tuple_element_id_in_zipper = (z: Zipper.t, idx: int): Id.t => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let result = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Tuple(elements) when result^ == None =>
            switch (List.nth_opt(elements, idx)) {
            | Some(el) => result := Some(Exp.rep_id(el))
            | None => ()
            };
            e;
          | _ => continue(e)
          }
        },
      term,
    );
  switch (result^) {
  | Some(id) => id
  | None => Alcotest.fail("No tuple element at index " ++ string_of_int(idx))
  };
};

let tuple_edit =
    (code: string, idx: int, f: (Zipper.t, Id.t) => option(Zipper.t)): string => {
  let z = mk_zipper(code);
  let el_id = find_tuple_element_id_in_zipper(z, idx);
  switch (f(z, el_id)) {
  | Some(new_z) => render_zipper(new_z)
  | None => Alcotest.fail("Tuple operation returned None")
  };
};

let tuple_program = "let p = (1, 2, 3) in p";
let labeled_tuple_program = "let p = (x=1, y=2) in p";

let tuple_element_tests = (
  "AgentTools.TupleElements",
  [
    /* TermEdit-level operations */
    test_case(
      "delete tuple element",
      `Quick,
      () => {
        let result =
          tuple_edit(tuple_program, 1, (z, id) =>
            TermEdit.tuple_delete_element(z, id)
          );
        check(bool, "has 1", true, string_contains("1", result));
        check(bool, "has 3", true, string_contains("3", result));
      },
    ),
    test_case(
      "insert tuple element",
      `Quick,
      () => {
        let result =
          tuple_edit(tuple_program, 2, (z, id) =>
            TermEdit.tuple_insert_element(z, id, "4", Direction.Right)
          );
        check(bool, "has 4", true, string_contains("4", result));
      },
    ),
    test_case(
      "update tuple element",
      `Quick,
      () => {
        let result =
          tuple_edit(tuple_program, 0, (z, id) =>
            TermEdit.tuple_update_element(z, id, "99")
          );
        check(bool, "has 99", true, string_contains("99", result));
      },
    ),
    /* Node map indexing */
    test_case(
      "tuple elements in node map",
      `Quick,
      () => {
        let node_map = build_node_map(tuple_program);
        check(
          bool,
          "(0) exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "p/(0)") != None,
        );
        check(
          bool,
          "(1) exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "p/(1)") != None,
        );
        check(
          bool,
          "(2) exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "p/(2)") != None,
        );
      },
    ),
    /* Labeled tuple elements use label names */
    test_case(
      "labeled tuple elements in node map",
      `Quick,
      () => {
        let node_map = build_node_map(labeled_tuple_program);
        check(
          bool,
          "x exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "p/x") != None,
        );
        check(
          bool,
          "y exists",
          true,
          HighLevelNodeMap.path_to_id_opt(node_map, "p/y") != None,
        );
      },
    ),
    /* Dispatch-level operations */
    test_case(
      "update tuple element via dispatch",
      `Quick,
      () => {
        let result =
          apply_and_render(tuple_program, Update(Body, "p/(0)", "99"));
        check(bool, "has 99", true, string_contains("99", result));
      },
    ),
    test_case(
      "delete tuple element via dispatch",
      `Quick,
      () => {
        let result =
          apply_and_render(tuple_program, Delete(BindingClause, "p/(1)"));
        check(bool, "has 1", true, string_contains("1", result));
        check(bool, "has 3", true, string_contains("3", result));
      },
    ),
    test_case(
      "insert tuple element via dispatch",
      `Quick,
      () => {
        let result =
          apply_and_render(tuple_program, Insert(After, "p/(2)", "4"));
        check(bool, "has 4", true, string_contains("4", result));
      },
    ),
  ],
);

/* === Cross-Cutting Path & Error Tests === */

let cross_cutting_tests = (
  "AgentTools.CrossCutting",
  [
    /* Read actions on sequence element paths */
    test_case(
      "GetSyntax on list element path",
      `Quick,
      () => {
        let result = run_read_action(list_program, GetSyntax("xs/[1]"));
        check_rendered("list[1]", "2", result);
      },
    ),
    test_case(
      "GetSyntax on tuple element path",
      `Quick,
      () => {
        let result = run_read_action(tuple_program, GetSyntax("p/(0)"));
        check_rendered("tuple(0)", "1", result);
      },
    ),
    test_case(
      "GetSyntax on labeled tuple element path",
      `Quick,
      () => {
        let result =
          run_read_action(labeled_tuple_program, GetSyntax("p/x"));
        check(bool, "contains 1", true, string_contains("1", result));
      },
    ),
    /* Shadowed names: first binding wins */
    test_case(
      "shadowed name resolves to first binding",
      `Quick,
      () => {
        let shadowed = "let a = 1 in let a = 2 in a";
        /* Bare "a" is ambiguous (dev's #k disambiguation); a#1 = first */
        let result =
          apply_and_render(shadowed, Update(Definition, "a#1", "10"));
        /* Should update the FIRST a (from 1 to 10), second a stays at 2 */
        check(bool, "has 10", true, string_contains("10", result));
        check(bool, "has 2", true, string_contains("2", result));
      },
    ),
    test_case(
      "shadowed name accessible by index",
      `Quick,
      () => {
        let shadowed = "let a = 1 in let a = 2 in a";
        let result =
          apply_and_render(shadowed, Update(Definition, "#1", "20"));
        /* #1 should target the SECOND binding */
        check(bool, "has 1", true, string_contains("1", result));
        check(bool, "has 20", true, string_contains("20", result));
      },
    ),
    /* Inapplicability errors */
    test_case("Update(Pattern) on list element gives clear error", `Quick, () => {
      switch (run_agent_action(list_program, Update(Pattern, "xs/[0]", "x"))) {
      | Ok(_) => Alcotest.fail("Expected failure")
      | Error(Action.Failure.Composition_action_failure(msg)) =>
        check(
          bool,
          "says not applicable",
          true,
          string_contains("not applicable", msg),
        );
        check(
          bool,
          "says list element",
          true,
          string_contains("list element", msg),
        );
      | Error(err) =>
        Alcotest.fail("Unexpected error: " ++ Action.Failure.show(err))
      }
    }),
    test_case("Update(Pattern) on tuple element gives clear error", `Quick, () => {
      switch (run_agent_action(tuple_program, Update(Pattern, "p/(0)", "x"))) {
      | Ok(_) => Alcotest.fail("Expected failure")
      | Error(Action.Failure.Composition_action_failure(msg)) =>
        check(
          bool,
          "says not applicable",
          true,
          string_contains("not applicable", msg),
        );
        check(
          bool,
          "says tuple element",
          true,
          string_contains("tuple element", msg),
        );
      | Error(err) =>
        Alcotest.fail("Unexpected error: " ++ Action.Failure.show(err))
      }
    }),
    test_case(
      "GetStatics on list element path",
      `Quick,
      () => {
        let result = run_read_action(list_program, GetStatics("xs/[1]"));
        check(bool, "mentions type", true, string_contains("type", result));
      },
    ),
    test_case(
      "GetStatics on case arm path",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 end in f";
        let result = run_read_action(case_code, GetStatics("f/|A"));
        check(bool, "mentions type", true, string_contains("type", result));
      },
    ),
    test_case(
      "Update(BindingClause) on case arm gives clear error",
      `Quick,
      () => {
        let case_code = "let f = fun x -> case x | A => 1 | B => 2 end in f";
        switch (
          run_agent_action(
            case_code,
            Update(BindingClause, "f/|A", "| C => 3"),
          )
        ) {
        | Ok(_) => Alcotest.fail("Expected failure")
        | Error(Action.Failure.Composition_action_failure(msg)) =>
          check(
            bool,
            "says not applicable",
            true,
            string_contains("not applicable", msg),
          );
          check(
            bool,
            "says case arm",
            true,
            string_contains("case arm", msg),
          );
        | Error(err) =>
          Alcotest.fail("Unexpected error: " ++ Action.Failure.show(err))
        };
      },
    ),
  ],
);
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
        check(int, "tool count", 47, List.length(tools));
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
            "update_type_annotation",
            "selector_update",
            "selector_delete",
            "selector_insert_after",
            "selector_insert_before",
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
          Util.StringUtil.plain_search("not found in node map", result, 0)
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
      /* Static errors from inserts warn rather than reject (multi-step
         refactoring); the point here is that the warning carries no
         spurious reserved-keyword note. */
      switch (
        run_agent_action(
          "let a = 1 in a",
          Insert(After, "a", "let b = unboundvar in"),
        )
      ) {
      | Ok(_) =>
        switch (CompositionGo.Public.last_warning^) {
        | None => Alcotest.fail("expected a static-error warning")
        | Some(msg) =>
          check(
            bool,
            "no keyword note in: " ++ msg,
            false,
            contains_str(~needle="reserved keyword", msg),
          )
        }
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
  module_node_map_tests,
  path_extension_tests,
  module_edit_action_tests,
  edge_case_tests,
  read_action_tests,
  type_annotation_tests,
  composition_view_print_tests,
  seq_node_map_tests,
  selector_tests,
  canonical_tests,
  canonical_read_tests,
  selector_edit_tests,
  gap_tests,
  whitespace_tests,
  completeness_tests,
  composition_utils_tests,
  statics_refractor_tests,
  agent_tools_with_projectors_tests,
  general_tree_refs_tests,
  sequential_operations_tests,
  type_alias_tests,
  complex_program_tests,
  case_arm_tests,
  list_element_tests,
  tuple_element_tests,
  cross_cutting_tests,
  agent_context_tests,
  error_print_tests,
  tool_json_tests,
  ascribed_binding_tests,
  rename_and_path_safety_tests,
  fn_sugar_tests,
];
