open Alcotest;
open Haz3lcore;
open Language;
open Action;
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
        Ok(Materialize.all(new_z, ~root=Exp));
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

/* ============================================================
   2. INITIALIZE — edge cases and static error rejection
   ============================================================ */

let initialize_tests = (
  "AgentTools.Initialize",
  [
    test_case("initialize with hole produces valid program", `Quick, () => {
      switch (run_initialize("?", "let x = 5 in x + 1")) {
      | Ok(z) =>
        check_rendered(
          "initialize_hole",
          "let x = 5 in x + 1",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("Initialize failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case("initialize with expression replaces it", `Quick, () => {
      switch (run_initialize("42", "let a = 1 in let b = 2 in a + b")) {
      | Ok(z) =>
        check_rendered(
          "initialize_expr",
          "let a = 1 in let b = 2 in a + b",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("Initialize failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case("initialize rejected on type alias program", `Quick, () => {
      switch (run_initialize("type T = Int in 0", "let x = 1 in x")) {
      | Ok(_) => Alcotest.fail("Expected failure: initialize on type alias")
      | Error(Action.Failure.Composition_action_failure(_)) => ()
      | Error(err) =>
        Alcotest.fail(
          "Unexpected failure kind: " ++ Action.Failure.show(err),
        )
      }
    }),
    test_case("initialize multi-binding program", `Quick, () => {
      switch (
        run_initialize("?", "let a = 1 in let b = 2 in let c = a + b in c")
      ) {
      | Ok(z) =>
        check_rendered(
          "initialize_multi",
          "let a = 1 in let b = 2 in let c = a + b in c",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("Initialize failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case("initialize with function value", `Quick, () => {
      switch (run_initialize("?", "let f = fun x -> x + 1 in f(3)")) {
      | Ok(z) =>
        check_rendered(
          "initialize_fun",
          "let f = fun x -> x + 1 in f(3)",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("Initialize failed: " ++ Action.Failure.show(err))
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
      "parse initialize tool call",
      `Quick,
      () => {
        let args = mk_json_args([("code", "let x = 1 in x")]);
        switch (
          CompositionUtils.Public.action_of(~tool_name="initialize", ~args)
        ) {
        | Action(Initialize("let x = 1 in x")) => ()
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
        check(int, "tool count", 25, List.length(tools));
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
        let edit_tool_names = [
          "update_definition",
          "update_body",
          "update_pattern",
          "update_binding_clause",
          "delete_binding_clause",
          "delete_body",
          "insert_after",
          "insert_before",
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
      "probe tools have required paths parameter",
      `Quick,
      () => {
        let probe_tool_names = [
          "place_probe",
          "remove_probe",
          "toggle_probe",
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
  ],
);

/* ============================================================
   AGGREGATE ALL TESTS
   ============================================================ */

let tests = [
  edit_action_tests,
  initialize_tests,
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
  sequential_operations_tests,
  type_alias_tests,
  complex_program_tests,
  agent_context_tests,
  error_print_tests,
  tool_json_tests,
];
