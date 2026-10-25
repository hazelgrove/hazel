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

/* literal text only: indentation is materialized as space pieces, so
   re-deriving it at print time (~indent) would double-count */
let render_zipper = (z: Zipper.t): string =>
  Printer.of_zipper(~holes="?", z);

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

/** Runs an insert at the top level ("" path): the code at the
    program's start (Before) or end (After; at a Mod root, members after
    the last, above a final expression). */
let run_insert_at_program_boundary =
    (
      ~root=Sort.Exp,
      code: string,
      place: Action.Structural.place,
      new_code: string,
    ) => {
  let z =
    root == Sort.Exp
      ? mk_zipper(code) : Zipper.unzip(CorpusUtil.seg_of(~root, code));
  Perform.go(
    ~settings=CoreSettings.on,
    ~statics=CachedStatics.empty,
    ~syntax=CachedSyntax.init(~root, z),
    ~root,
    Structural(Insert(place, new_code)),
    {
      zipper: z,
      col_target: None,
    },
  );
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

/* a reply on [code] with no calls yet, on the kernel */
let reply_on = (~root=Sort.Exp, code: string): CompositionGo.Reply.t => {
  let seg =
    root == Sort.Exp
      ? Zipper.unselect_and_zip(mk_zipper(code))
      : CorpusUtil.seg_of(~root, code);
  let statics_of =
    CompositionGo.statics_of(
      ~settings=CoreSettings.on,
      ~root,
      ~probe_ids=Id.Map.empty,
    );
  CompositionGo.Reply.start(
    ~root,
    ~statics=lazy(statics_of(seg)),
    ~statics_of,
    seg,
  );
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
    test_case("insert first in the top level seeds empty program", `Quick, () => {
      switch (
        run_insert_at_program_boundary("?", Before(""), "let a = 3 in")
      ) {
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
      "insert first in the top level prepends to non-empty program", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "let a = 1 in a",
          Before(""),
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
      "replace :definition replaces def",
      `Quick,
      () => {
        let result =
          apply_and_render("let a = 1 in a", Replace("a:definition", "2"));
        check_rendered("update_definition", "let a = 2 in a", result);
      },
    ),
    test_case(
      "replace => replaces the result",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Replace("=>", "b + 1"),
          );
        check_rendered(
          "update_body",
          "let a = 1 in let b = 2 in b + 1",
          result,
        );
      },
    ),
    test_case(
      "rename renames uses",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a + 1 in b + a",
            Rename("a", None, "x"),
          );
        check_rendered(
          "update_pattern",
          "let x = 1 in let b = x + 1 in b + x",
          result,
        );
      },
    ),
    test_case(
      "rename renames in let def",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x = 3 in let y = x in y",
            Rename("x", None, "z"),
          );
        check_rendered(
          "update_pattern_let_def",
          "let z = 3 in let y = z in y",
          result,
        );
      },
    ),
    test_case(
      "replace replaces binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Replace("b", "let b = a + 2 in"),
          );
        check_rendered(
          "update_binding_clause",
          "let a = 1 in let b = a + 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert before adds binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(Before("b"), "let x = a in"),
          );
        check_rendered(
          "insert_before",
          "let a = 1 in let x = a in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert after adds binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(After("a"), "let x = a in"),
          );
        check_rendered(
          "insert_after",
          "let a = 1 in let x = a in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "delete removes binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + c",
            Delete("b"),
          );
        check_rendered(
          "delete_binding_clause",
          "let a = 1 in let c = 3 in a + c",
          result,
        );
      },
    ),
  ],
);

/* ============================================================
   2. INSERT_AT_PROGRAM_BOUNDARY — insert first or last in the top level
   ============================================================ */

let insert_at_program_boundary_tests = (
  "AgentTools.InsertAtProgramBoundary",
  [
    test_case(
      "insert first in the top level on hole produces valid program",
      `Quick,
      () => {
      switch (
        run_insert_at_program_boundary("?", Before(""), "let x = 5 in")
      ) {
      | Ok(z) =>
        check_rendered(
          "boundary_before_hole",
          "let x = 5 in ?",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("insert failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case(
      "insert first in the top level seeds multi-binding program", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "?",
          Before(""),
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
        Alcotest.fail("insert failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case("insert first in the top level with function value", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "?",
          Before(""),
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
        Alcotest.fail("insert failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case(
      "insert first in the top level prepends to non-empty program", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "let b = 2 in b",
          Before(""),
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
        Alcotest.fail("insert failed: " ++ Action.Failure.show(err))
      }
    }),
  ],
);

/* ============================================================
   3. REPLACE :definition — complex definition replacement
   ============================================================ */

let replace_definition_tests = (
  "AgentTools.ReplaceDefinition",
  [
    test_case(
      "X:definition code starting `let X =` again is refused",
      `Quick,
      () => {
        /* round 2's E3: the whole tail chain written as joined's
           definition */
        let seg =
          Option.get(
            EntryLayout.parse(
              Option.get(CorpusUtil.mega_src("mega-1k.hz")),
            ),
          );
        let statics_of =
          CompositionGo.statics_of(
            ~settings=CoreSettings.on,
            ~root=Exp,
            ~probe_ids=Id.Map.empty,
          );
        let mega =
          CompositionGo.Reply.start(
            ~root=Exp,
            ~statics=lazy(statics_of(seg)),
            ~statics_of,
            seg,
          );
        let code' = "let joined = string_join(\" \", body) in\nlet tight = string_replace(\" -- \", joined, \"--\") in\nlet res = string_replace(\"--\", tight, \": \") in\nstring_trim(res)";
        switch (
          CompositionGo.Reply.add(
            mega,
            ~index=0,
            Replace("LogCleaner/clean_entry/joined:definition", code'),
          )
        ) {
        | Ok(_) => Alcotest.fail("replaced")
        | Error(e) =>
          check(
            string,
            "refusal",
            "`LogCleaner/clean_entry/joined:definition` is what follows `=`; your code starts with `let joined =` again. To change only clean_entry's final expression, replace `LogCleaner/clean_entry/=>`.",
            e,
          )
        };
        switch (
          CompositionGo.Reply.add(
            reply_on("type T = Int in let x: T = 1 in x"),
            ~index=0,
            Replace("T:definition", "type T = Bool"),
          )
        ) {
        | Ok(_) => Alcotest.fail("replaced T")
        | Error(e) =>
          check(
            string,
            "a type, at the top",
            "`T:definition` is what follows `=`; your code starts with `type T =` again.",
            e,
          )
        };
      },
    ),
    test_case(
      "replace :definition with complex expression",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a in b",
            Replace("b:definition", "a + a * 2"),
          );
        check_rendered(
          "update_def_complex",
          "let a = 1 in let b = a + a * 2 in b",
          result,
        );
      },
    ),
    test_case(
      "replace :definition with code spanning lines: on lines of its own",
      `Quick,
      () => {
        check_rendered_exact(
          "one line to many",
          "let scale(k: Int): Int -> Int =\n  case k\n  | 0 => fun sc -> sc\n  | _ => fun sc -> sc * k\n  end\nin\nscale(2)(3)",
          apply_and_render(
            "let scale(k: Int): Int -> Int = fun sc -> sc in\nscale(2)(3)",
            Replace(
              "scale:definition",
              "case k\n| 0 => fun sc -> sc\n| _ => fun sc -> sc * k\nend",
            ),
          ),
        );
        check_rendered_exact(
          "many lines to one",
          "let f(x) = x * 2 in\nf(1)",
          apply_and_render(
            "let f(x) =\n  x + 1\nin\nf(1)",
            Replace("f:definition", "x * 2"),
          ),
        );
      },
    ),
    test_case(
      "replace :definition with function",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f = 1 in f",
            Replace("f:definition", "fun x -> x + 1"),
          );
        check_rendered(
          "update_def_function",
          "let f = fun x -> x + 1 in f",
          result,
        );
      },
    ),
    test_case(
      "replace :definition first of three bindings",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Replace("a:definition", "10"),
          );
        check_rendered(
          "update_def_first",
          "let a = 10 in let b = 2 in let c = 3 in a + b + c",
          result,
        );
      },
    ),
    test_case(
      "replace :definition last of three bindings",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Replace("c:definition", "a * b"),
          );
        check_rendered(
          "update_def_last",
          "let a = 1 in let b = 2 in let c = a * b in a + b + c",
          result,
        );
      },
    ),
    test_case(
      "replace :definition with tuple",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a",
            Replace("a:definition", "(1, 2, 3)"),
          );
        check_rendered("update_def_tuple", "let a = (1, 2, 3) in a", result);
      },
    ),
    test_case(
      "replace :definition with boolean",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let flag = 1 in flag",
            Replace("flag:definition", "true"),
          );
        check_rendered("update_def_bool", "let flag = true in flag", result);
      },
    ),
    test_case(
      "replace :definition with string",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let msg = 1 in msg",
            Replace("msg:definition", {|"hello"|}),
          );
        check_rendered(
          "update_def_string",
          {|let msg = "hello" in msg|},
          result,
        );
      },
    ),
    test_case(
      "replace :definition with list literal",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let xs = 1 in xs",
            Replace("xs:definition", "[1, 2, 3]"),
          );
        check_rendered("update_def_list", "let xs = [1, 2, 3] in xs", result);
      },
    ),
  ],
);

/* ============================================================
   4. PARTS — :definition and the result =>
   ============================================================ */

let body_part_tests = (
  "AgentTools.Parts",
  [
    test_case("named function edits still reject real type errors", `Quick, () => {
      expect_any_failure(
        "let f(x: Int) = x in 0",
        Replace("f:definition", "true + 1"),
        "invalid function definition",
      )
    }),
    test_case(
      "named function added after an annotated constant",
      `Quick,
      () => {
        let code = "type Model = Int in type Action = Int in let init : Model = 0 in ?";
        let result =
          apply_and_render(
            code,
            Insert(
              After("init"),
              "let counter_update(m: Model, a: Action) = m + a",
            ),
          );
        check_rendered(
          "counter update",
          "type Model = Int in type Action = Int in let init : Model = 0 in let counter_update(m: Model, a: Action) = m + a in ?",
          result,
        );
      },
    ),
    test_case(":definition of a sugared function is its body", `Quick, () =>
      check_rendered(
        "sugared_definition",
        "let f(x) = x * 2 in f(2)",
        apply_and_render(
          "let f(x) = x + 1 in f(2)",
          Replace("f:definition", "x * 2"),
        ),
      )
    ),
    test_case(
      ":body isn't a part: the refusal names :definition and /=>", `Quick, () =>
      switch (
        run_agent_action(
          "let f = fun x -> x + 1 in f(2)",
          Replace("f:body", "x * 2"),
        )
      ) {
      | Error(Action.Failure.Composition_action_failure(msg)) =>
        check(
          string,
          "refusal",
          "`:body` isn't a part. Use `f:definition` (for `let f(x) = e` that is `e`), or `f/=>` for the result of a block of definitions.",
          msg,
        )
      | _ => Alcotest.fail("expected a refusal")
      }
    ),
    test_case(
      "/=> replaces a definition's result, its local definitions kept",
      `Quick,
      () =>
      check_rendered(
        "local_result",
        "let f(x) = let y = x + 1 in y * 3 in f(2)",
        apply_and_render(
          "let f(x) = let y = x + 1 in y * 2 in f(2)",
          Replace("f/=>", "y * 3"),
        ),
      )
    ),
  ],
);

/* ============================================================
   5. RENAME — renaming and use-site propagation
   ============================================================ */

let rename_tests = (
  "AgentTools.Rename",
  [
    test_case(
      "replace :pattern adds a type annotation",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a + 1",
            Replace("a:pattern", "a : Int"),
          );
        check_rendered(
          "update_pat_annotated",
          "let a : Int = 1 in a + 1",
          result,
        );
      },
    ),
    test_case(
      "rename renames a funlet head, call sites follow",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f(x: Int): Int = x + 1 in f(2) + f(3)",
            Rename("f", None, "g"),
          );
        check_rendered(
          "update_pat_funlet_rename",
          "let g(x: Int): Int = x + 1 in g(2) + g(3)",
          result,
        );
      },
    ),
    test_case(
      "rename renames a funlet member helper (internal uses follow)",
      `Quick,
      () => {
        /* externally-referenced members (m.g) are rejected by design;
           internal helpers rename with their in-module use sites */
        let result =
          apply_and_render(
            "let m = {\n  let helper(x) = x + 1;\n  let g(y) = helper(y)\n} in m.g(2)",
            Rename("m/helper", None, "aux"),
          );
        check_rendered(
          "update_pat_funlet_member_rename",
          "let m = {\n  let aux(x) = x + 1;\n  let g(y) = aux(y)\n} in m.g(2)",
          result,
        );
      },
    ),
    test_case(
      "rename renames across multiple use sites",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a in let c = a + b in c + a",
            Rename("a", None, "x"),
          );
        check_rendered(
          "update_pat_multi_uses",
          "let x = 1 in let b = x in let c = x + b in c + x",
          result,
        );
      },
    ),
    test_case(
      "rename middle binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a in let c = b in c",
            Rename("b", None, "m"),
          );
        check_rendered(
          "update_pat_middle",
          "let a = 1 in let m = a in let c = m in c",
          result,
        );
      },
    ),
    test_case(
      "rename last binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = a + b in c * 2",
            Rename("c", None, "result"),
          );
        check_rendered(
          "update_pat_last",
          "let a = 1 in let b = 2 in let result = a + b in result * 2",
          result,
        );
      },
    ),
    test_case(
      "rename then replace :definition resolves new binding path",
      `Quick,
      () => {
        let result =
          apply_chain_render(
            "let speed = 50 in speed",
            [
              Rename("speed", None, "velocity"),
              Replace("velocity:definition", "99"),
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
   6. REPLACE — whole-entry replacement
   ============================================================ */

let replace_entry_tests = (
  "AgentTools.ReplaceEntry",
  [
    test_case(
      "replace with annotation",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a",
            Replace("a", "let a : Int = 42 in"),
          );
        check_rendered(
          "update_bc_annotation",
          "let a : Int = 42 in a",
          result,
        );
      },
    ),
    test_case(
      "replace path t on already-typed single binding",
      `Quick,
      () => {
        let code = "let t : Int = 42 in 0";
        let result =
          apply_and_render(
            code,
            Replace(
              "t",
              "let double : Int -> Int = fun x -> x * 2 in\nlet result = double(5) in",
            ),
          );
        check_rendered(
          "update_bc_t_typed_single",
          "let double : Int -> Int = fun x -> x * 2 in\nlet result = double(5) in 0",
          result,
        );
      },
    ),
    test_case(
      "replace def-nested t requires double/t not t",
      `Quick,
      () => {
        let code = "let double = let t : Int = 42 in t in double";
        expect_any_failure(
          code,
          Replace("t", "let t : Int = 99 in"),
          "def_nested_path_must_use_outer_slash_t",
        );
        let result =
          apply_and_render(code, Replace("double/t", "let t : Int = 99 in"));
        check_rendered(
          "update_bc_def_nested_t",
          "let double = let t : Int = 99 in t in double",
          result,
        );
      },
    ),
    test_case(
      "replace introduces new binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Replace("a", "let a = 1 in let x = a + 10 in"),
          );
        check_rendered(
          "update_bc_multi",
          "let a = 1 in let x = a + 10 in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "replace middle of chain",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Replace("b", "let b : Int = a * 2 in"),
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
      "insert before first binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(Before("a"), "let z = 0 in"),
          );
        check_rendered(
          "insert_before_first",
          "let z = 0 in let a = 1 in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert after last binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(After("b"), "let c = a + b in"),
          );
        check_rendered(
          "insert_after_last",
          "let a = 1 in let b = 2 in let c = a + b in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert before with annotated binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(Before("b"), "let x : Int = a in"),
          );
        check_rendered(
          "insert_before_annotated",
          "let a = 1 in let x : Int = a in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "insert after with function binding",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a",
            Insert(After("a"), "let f = fun x -> x + a in"),
          );
        check_rendered(
          "insert_after_fun",
          "let a = 1 in let f = fun x -> x + a in a",
          result,
        );
      },
    ),
    test_case(
      "insert before in three-binding chain",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b + c",
            Insert(Before("c"), "let d = a + b in"),
          );
        check_rendered(
          "insert_before_third",
          "let a = 1 in let b = 2 in let d = a + b in let c = 3 in a + b + c",
          result,
        );
      },
    ),
    test_case(
      "insert after tool call strips leading indentation",
      `Quick,
      () => {
        let args: API.Json.t =
          `Assoc([
            ("after", `String("b")),
            ("code", `String("  let c = 3 in")),
          ]);
        switch (CompositionUtils.action_of(~tool_name="insert", ~args)) {
        | Action(EditorAction(a)) =>
          check_rendered_exact(
            "insert_indented_code",
            "let a = 1 in let b = 2 in let c = 3 in a + b",
            apply_and_render("let a = 1 in let b = 2 in a + b", a),
          )
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "insert after tool call normalizes CRLF linebreaks",
      `Quick,
      () => {
        let args: API.Json.t =
          `Assoc([
            ("after", `String("b")),
            ("code", `String("let c =\r\n  3")),
          ]);
        switch (CompositionUtils.action_of(~tool_name="insert", ~args)) {
        | Action(EditorAction(a)) =>
          check_rendered_exact(
            "insert_crlf_code",
            "let a = 1 in let b = 2 in let c =\n  3\nin a + b",
            apply_and_render("let a = 1 in let b = 2 in a + b", a),
          )
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case("insert after last binding keeps line separator", `Quick, () => {
      check_rendered_exact(
        "insert_after_last_separator",
        "let a = 1 in let b = 2 in let c = 3 in a + b",
        apply_and_render(
          "let a = 1 in let b = 2 in a + b",
          Insert(After("b"), "let c = 3 in"),
        ),
      )
    }),
    test_case(
      "insert last in the top level appends on its own line", `Quick, () => {
      switch (
        run_insert_at_program_boundary(
          "let a = 1 in let b = 2 in",
          After(""),
          "let c = 3 in",
        )
      ) {
      | Ok(z) =>
        check_rendered_exact(
          "boundary_append_separator",
          "let a = 1 in let b = 2 in let c = 3 in ?",
          render_zipper(z),
        )
      | Error(err) =>
        Alcotest.fail("boundary append failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case("replace => over a bare hole keeps `in` separated", `Quick, () => {
      /* The hole grout sits flush against `in`; the replacement code
         must not fuse into `inx` */
      check_rendered_exact(
        "result_hole_separator",
        "let x = 1 in x",
        apply_and_render("let x = 1 in ?", Replace("=>", "x")),
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
      "delete first of three",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in b + c",
            Delete("a"),
          );
        check_rendered(
          "delete_bc_first",
          "let b = 2 in let c = 3 in b + c",
          result,
        );
      },
    ),
    test_case(
      "delete middle of three",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + c",
            Delete("b"),
          );
        check_rendered(
          "delete_bc_middle",
          "let a = 1 in let c = 3 in a + c",
          result,
        );
      },
    ),
    test_case(
      "delete last of three",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in let c = 3 in a + b",
            Delete("c"),
          );
        check_rendered(
          "delete_bc_last",
          "let a = 1 in let b = 2 in a + b",
          result,
        );
      },
    ),
    test_case(
      "replace :definition with ? clears a sugared function's body",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f(x) = x + 1 in let b = 2 in f(b)",
            Replace("f:definition", "?"),
          );
        check_rendered(
          "clear_body",
          "let f(x) = ? in let b = 2 in f(b)",
          result,
        );
      },
    ),
    test_case(
      "delete takes no part",
      `Quick,
      () => {
        expect_composition_failure(
          "let a = 1 in a",
          Delete("a:definition"),
          "delete_definition",
        );
        expect_composition_failure(
          "let a = 1 in a",
          Delete("a:pattern"),
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
    test_case("replace :definition rejects type mismatch", `Quick, () => {
      expect_composition_failure(
        "let a : Int = 1 in a + 1",
        Replace("a:definition", "true"),
        "type_mismatch_def",
      )
    }),
    test_case("replace :definition rejects unbound variable", `Quick, () => {
      expect_composition_failure(
        "let a = 1 in a",
        Replace("a:definition", "nonexistent_var"),
        "unbound_var_def",
      )
    }),
    test_case("insert before rejects ill-typed binding", `Quick, () => {
      expect_composition_failure(
        "let a = 1 in a",
        Insert(Before("a"), "let x : Int = true in"),
        "insert_before_type_error",
      )
    }),
    test_case("insert after rejects ill-typed binding", `Quick, () => {
      expect_composition_failure(
        "let a = 1 in a",
        Insert(After("a"), "let x : String = 42 in"),
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
      "replace :definition nested child",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b in a",
            Replace("a/b:definition", "42"),
          );
        check_rendered(
          "update_def_nested",
          "let a = let b = 42 in b in a",
          result,
        );
      },
    ),
    test_case(
      "replace a nested block's result",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b + 1 in a",
            Replace("a/=>", "b * 2"),
          );
        check_rendered(
          "update_body_nested",
          "let a = let b = 1 in b * 2 in a",
          result,
        );
      },
    ),
    test_case(
      "rename of nested child",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b + 1 in a",
            Rename("a/b", None, "c"),
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
            Delete("a/b"),
          );
        check_rendered(
          "delete_bc_nested",
          "let a = let c = 2 in c in a",
          result,
        );
      },
    ),
    test_case(
      "insert after into nested scope",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b in a",
            Insert(After("a/b"), "let c = b + 1 in"),
          );
        check_rendered(
          "insert_after_nested",
          "let a = let b = 1 in let c = b + 1 in b in a",
          result,
        );
      },
    ),
    test_case(
      "insert before into nested scope",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = let b = 1 in b in a",
            Insert(Before("a/b"), "let z = 0 in"),
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
    test_case("replace :definition with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Replace("nonexistent:definition", "2"),
        "nonexistent_path_def",
      )
    }),
    test_case("replace :pattern with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Replace("nonexistent:pattern", "2"),
        "nonexistent_path_body",
      )
    }),
    test_case("delete with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Delete("nonexistent"),
        "nonexistent_path_delete",
      )
    }),
    test_case("insert before with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Insert(Before("nonexistent"), "let x = 1 in"),
        "nonexistent_path_insert_before",
      )
    }),
    test_case("insert after with nonexistent path fails", `Quick, () => {
      expect_any_failure(
        "let a = 1 in a",
        Insert(After("nonexistent"), "let x = 1 in"),
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
          Replace(long_path ++ ":definition", "2"),
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
          Delete("nonexistent"),
        )
      ) {
      | Ok(_) => Alcotest.fail("Expected failure")
      | Error(err) =>
        let msg = Action.Failure.show(err);
        check(
          bool,
          "error lists the entries there",
          true,
          Util.StringUtil.plain_search("Its entries:", msg, 0) >= 0,
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
          Delete("inner"),
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
   12. PATHS — the agent's paths, EntryPath's
   ============================================================ */

/* a program's outline: what its paths name */
let outline_of = (z: Zipper.t): Outline.t =>
  Outline.of_program(~root=Exp, Zipper.unselect_and_zip(z));
let build_node_map = (code: string): Outline.t =>
  outline_of(mk_zipper(code));
let path_id = (o: Outline.t, path: string): option(Id.t) =>
  switch (Outline.resolve(o, path)) {
  | Ok(e) => Some(e.id)
  | Error(_) => None
  };

/* what [path] names in [code]: its entry's first name, else its path */
let resolved = (code: string, path: string): option(string) => {
  let seg = Zipper.unselect_and_zip(mk_zipper(code));
  switch (EntryPath.resolve_string(~root=Exp, seg, path)) {
  | Ok(f) =>
    let (lv, k) = ListUtil.last(f.steps);
    switch (EntryPath.entry(lv, k).binders) {
    | [(x, _), ..._] => Some(x)
    | [] =>
      Option.map(
        EntryPath.to_string,
        EntryPath.describe(~root=Exp, seg, f.id),
      )
    };
  | Error(_) => None
  };
};

/* every entry's canonical path, in program order */
let paths_of = (code: string): list(string) => {
  let seg = Zipper.unselect_and_zip(mk_zipper(code));
  List.map(
    ((_, p)) => EntryPath.to_string(p),
    EntryPath.entries(~root=Exp, seg),
  );
};

let path_error = (code: string, path: string): string =>
  switch (run_agent_action(code, Delete(path))) {
  | Error(Composition_action_failure(msg)) => msg
  | Error(e) => Action.Failure.show(e)
  | Ok(_) => Alcotest.fail("resolved " ++ path)
  };

let high_level_node_map_tests = (
  "AgentTools.Paths",
  [
    test_case("a let chain's entries", `Quick, () =>
      check(
        list(string),
        "paths",
        ["a", "b", "c", "=>"],
        paths_of("let a = 1 in let b = 2 in let c = 3 in a + b + c"),
      )
    ),
    test_case("a name resolves", `Quick, () =>
      check(
        option(string),
        "b",
        Some("b"),
        resolved("let a = 1 in let b = 2 in let c = 3 in a + b + c", "b"),
      )
    ),
    test_case(
      "a duplicate name is ambiguous; #k picks in program order",
      `Quick,
      () => {
        let code = "let n = 1 in let n = 2 in n";
        check(option(string), "bare n", None, resolved(code, "n"));
        check(option(string), "n#1", Some("n"), resolved(code, "n#1"));
        check(list(string), "paths", ["n#1", "n#2", "=>"], paths_of(code));
      },
    ),
    test_case("lets in a definition are under it", `Quick, () =>
      check(
        list(string),
        "paths",
        ["a", "a/b", "a/=>", "=>"],
        paths_of("let a = let b = 1 in b in a"),
      )
    ),
    test_case("a missing name resolves to nothing", `Quick, () =>
      check(
        option(string),
        "missing",
        None,
        resolved("let a = 1 in a", "x"),
      )
    ),
    test_case(
      "nested names resolve, 3 deep",
      `Quick,
      () => {
        let code = "let a = let b = let c = 1 in c in b in a";
        check(option(string), "a/b", Some("b"), resolved(code, "a/b"));
        check(option(string), "a/b/c", Some("c"), resolved(code, "a/b/c"));
        check(option(string), "a.b.c", Some("c"), resolved(code, "a.b.c"));
      },
    ),
    test_case("a typed binding by its name", `Quick, () =>
      check(
        option(string),
        "t",
        Some("t"),
        resolved("let t : Int = 42 in t", "t"),
      )
    ),
    test_case(
      "a body chain's lets are siblings, not nested",
      `Quick,
      () => {
        let code = "let double = 1 in let t : Int = 42 in t";
        check(option(string), "t", Some("t"), resolved(code, "t"));
        check(option(string), "double/t", None, resolved(code, "double/t"));
      },
    ),
    test_case(
      "a definition's lets need their outer name",
      `Quick,
      () => {
        let code = "let double = let t : Int = 42 in t in double";
        check(option(string), "bare t", None, resolved(code, "t"));
        check(
          option(string),
          "double/t",
          Some("t"),
          resolved(code, "double/t"),
        );
      },
    ),
    test_case(
      "a bare nested name suggests its path",
      `Quick,
      () => {
        let msg =
          path_error("let double = let t : Int = 42 in t in double", "t");
        check(
          bool,
          "suggests double/t: " ++ msg,
          true,
          StringUtil.plain_search("Did you mean `double/t`", msg, 0) >= 0,
        );
      },
    ),
    test_case(
      "a missing name lists the block's entries",
      `Quick,
      () => {
        let msg =
          path_error("let abc = 1 in let def = 2 in abc + def", "abd");
        check(
          bool,
          "lists abc: " ++ msg,
          true,
          StringUtil.plain_search("Its entries: abc, def", msg, 0) >= 0,
        );
      },
    ),
    test_case(
      "types and lets after a type chain",
      `Quick,
      () => {
        check(
          list(string),
          "paths",
          ["Board", "initial_board", "=>"],
          paths_of("type Board = Int in let initial_board : Board = 1 in ?"),
        );
        check(
          list(string),
          "chess",
          [
            "Color",
            "PieceType",
            "Piece",
            "Square",
            "Board",
            "initial_board",
            "=>",
          ],
          paths_of(
            "type Color = + White + Black in type PieceType = + Pawn + Knight + Bishop + Rook + Queen + King in type Piece = (Color, PieceType) in type Square = + Empty + Occupied(Piece) in type Board = [[Square]] in let initial_board : Board = [[Occupied((White, Rook))]] in ?",
          ),
        );
      },
    ),
    test_case("a program without bindings has just its result", `Quick, () =>
      check(list(string), "paths", ["=>"], paths_of("1 + 2"))
    ),
  ],
);

/* ============================================================
   13. COMPOSITION VIEW — print and fold rendering
   ============================================================ */

let composition_view_print_tests = {
  let print = code =>
    CompositionView.print_segment(Zipper.unselect_and_zip(mk_zipper(code)));
  (
    "CompositionView.print_segment",
    [
      test_case("hole renders as question mark, not absence", `Quick, () =>
        check_rendered(
          "hole as question mark",
          "let a = 1 in ?",
          print("let a = 1 in "),
        )
      ),
      test_case("a fold prints as its invocation", `Quick, () =>
        check_rendered(
          "fold as invocation",
          {|let a = ^^fold(1) in a|},
          print("let a = ^^fold(1) in a"),
        )
      ),
      test_case("a program prints as written", `Quick, () =>
        check_rendered(
          "multi-binding program",
          "let a = 1 in let b = 2 in a + b",
          print("let a = 1 in let b = 2 in a + b"),
        )
      ),
    ],
  );
};

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
      "parse the edit verbs",
      `Quick,
      () => {
        let parse = (tool_name, pairs) =>
          switch (
            CompositionUtils.action_of(~tool_name, ~args=mk_json_args(pairs))
          ) {
          | Action(EditorAction(a)) => Ok(a)
          | Action(_) => Error("wrong action variant")
          | DocsRequest(_) => Error("DocsRequest")
          | Failure(msg) => Error(msg)
          };
        let is = (what, want: Action.Structural.t, got) =>
          check(
            bool,
            what,
            true,
            switch (got) {
            | Ok(a) => a == want
            | Error(_) => false
            },
          );
        is(
          "insert after",
          Insert(After("a"), "let b = 2"),
          parse("insert", [("after", "a"), ("code", "let b = 2")]),
        );
        is(
          "insert first_in the top level",
          Insert(FirstIn(""), "let x = 1"),
          parse("insert", [("first_in", ""), ("code", "let x = 1")]),
        );
        is(
          "insert last_in a module",
          Insert(LastIn("M"), "let x = 1"),
          parse("insert", [("last_in", "M"), ("code", "let x = 1")]),
        );
        is(
          "replace a part",
          Replace("a:definition", "42"),
          parse("replace", [("target", "a:definition"), ("code", "42")]),
        );
        is("delete", Delete("b"), parse("delete", [("target", "b")]));
        is(
          "move",
          Move("a", Before("b")),
          parse("move", [("target", "a"), ("before", "b")]),
        );
        is(
          "rename",
          Rename("x", None, "y"),
          parse("rename", [("target", "x"), ("to", "y")]),
        );
        check(
          bool,
          "insert takes exactly one place",
          true,
          Result.is_error(
            parse(
              "insert",
              [("after", "a"), ("before", "b"), ("code", "let c = 3")],
            ),
          )
          && Result.is_error(parse("insert", [("code", "let c = 3")])),
        );
        List.iter(
          old =>
            check(
              bool,
              old ++ " is gone",
              true,
              Result.is_error(parse(old, [("path", "a"), ("code", "1")])),
            ),
          [
            "update_definition",
            "update_body",
            "update_pattern",
            "update_binding_clause",
            "delete_binding_clause",
            "delete_body",
            "insert_after",
            "insert_before",
            "insert_test",
          ],
        );
      },
    ),
    test_case(
      "parse open and close",
      `Quick,
      () => {
        let targets = `Assoc([("targets", `List([`String("a")]))]);
        switch (
          CompositionUtils.action_of(~tool_name="open", ~args=targets),
          CompositionUtils.action_of(~tool_name="close", ~args=targets),
          CompositionUtils.action_of(
            ~tool_name="close",
            ~args=`Assoc([("all", `Bool(true))]),
          ),
        ) {
        | (
            Action(AgentContextAction(OpenPaths(["a"]))),
            Action(AgentContextAction(ClosePaths(["a"]))),
            Action(AgentContextAction(CloseAllPaths)),
          ) =>
          ()
        | _ => Alcotest.fail("open/close parse")
        };
      },
    ),
    test_case(
      "parse place_probe tool call",
      `Quick,
      () => {
        let args =
          `Assoc([("paths", `List([`String("a"), `String("b")]))]);
        switch (CompositionUtils.action_of(~tool_name="place_probe", ~args)) {
        | Action(ProbeAction(PlaceProbe(["a", "b"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse remove_probe tool call",
      `Quick,
      () => {
        let args = `Assoc([("paths", `List([`String("result")]))]);
        switch (CompositionUtils.action_of(~tool_name="remove_probe", ~args)) {
        | Action(ProbeAction(RemoveProbe(["result"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
        | Failure(msg) => Alcotest.fail("Failed to parse: " ++ msg)
        };
      },
    ),
    test_case(
      "parse toggle_probe tool call",
      `Quick,
      () => {
        let args = `Assoc([("paths", `List([`String("f")]))]);
        switch (CompositionUtils.action_of(~tool_name="toggle_probe", ~args)) {
        | Action(ProbeAction(ToggleProbe(["f"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
        switch (CompositionUtils.action_of(~tool_name="place_statics", ~args)) {
        | Action(StaticsAction(PlaceStatics(["x", "y"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(~tool_name="remove_statics", ~args)
        ) {
        | Action(StaticsAction(RemoveStatics(["foo"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(~tool_name="toggle_statics", ~args)
        ) {
        | Action(StaticsAction(ToggleStatics(["bar"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(
            ~tool_name="place_syntax_projector",
            ~args,
          )
        ) {
        | Action(
            SyntaxProjectorAction(PlaceSyntaxProjector(Slider, ["n", "m"])),
          ) =>
          ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(
            ~tool_name="remove_syntax_projector",
            ~args,
          )
        ) {
        | Action(SyntaxProjectorAction(RemoveSyntaxProjector(["x"]))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(
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
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(
            ~tool_name="place_syntax_projector",
            ~args,
          )
        ) {
        | Action(_) => Alcotest.fail("Expected Failure when kind is probe")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(~tool_name="does_not_exist", ~args)
        ) {
        | Action(_) => Alcotest.fail("Expected Failure for unknown tool")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
        | Failure(_) => ()
        };
      },
    ),
    test_case(
      "missing required arg returns Failure",
      `Quick,
      () => {
        let args = mk_json_args([("target", "a")]);
        switch (CompositionUtils.action_of(~tool_name="replace", ~args)) {
        | Action(_) => Alcotest.fail("Expected Failure for missing code arg")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(~tool_name="set_active_task", ~args)
        ) {
        | Action(WorkbenchAction(SetActiveTask("My Task"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(~tool_name="unset_active_task", ~args)
        ) {
        | Action(WorkbenchAction(UnsetActiveTask)) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(
            ~tool_name="mark_active_task_complete",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(MarkActiveTaskComplete("All done"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(
            ~tool_name="mark_active_task_incomplete",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(MarkActiveTaskIncomplete)) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(~tool_name="set_active_subtask", ~args)
        ) {
        | Action(WorkbenchAction(SetActiveSubtask("Step 1"))) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(~tool_name="unset_active_subtask", ~args)
        ) {
        | Action(WorkbenchAction(UnsetActiveSubtask)) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(
            ~tool_name="mark_active_subtask_complete",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(MarkActiveSubtaskComplete("Subtask done"))) =>
          ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          CompositionUtils.action_of(
            ~tool_name="mark_active_subtask_incomplete",
            ~args,
          )
        ) {
        | Action(WorkbenchAction(MarkActiveSubtaskIncomplete)) => ()
        | Action(_) => Alcotest.fail("Parsed to wrong action variant")
        | DocsRequest(_) => Alcotest.fail("Parsed to DocsRequest")
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
          switch (
            run_agent_action(code, Insert(After("a"), "let b = 0 in"))
          ) {
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
            Structural(Replace("b:definition", "a + 1")),
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
      "replace :definition, then =>",
      `Quick,
      () => {
        let code = "let a = 1 in let b = 2 in a + b";
        let z =
          switch (run_agent_action(code, Replace("a:definition", "10"))) {
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
            Structural(Replace("=>", "a * b")),
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
      "replace :definition introduces nested recursive helper (regression: agent bug 1 / tools)",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f = fun x -> x in f(3)",
            Replace(
              "f:definition",
              "fun x -> let g = fun y -> if y == 0 then 0 else 1 + g(y - 1) in g(3)",
            ),
          );
        check(
          bool,
          "render mentions recursive call g(y - 1)",
          true,
          StringUtil.plain_search("g(y - 1)", result, 0) >= 0,
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
      "insert after a module member whose body is a case, with a case",
      `Quick,
      () => {
        let prog = "module M = {\n  let a(i: Int): Int =\n    case i\n    | 0 => 1\n    | _ => 2\n    end\n} in\n1";
        let code = "let b(i: Int): Int =\n  case i\n  | 0 => 1\n  | _ => 2\n  end";
        switch (
          try(run_agent_action(prog, Insert(After("M/a"), code))) {
          | exn => Alcotest.fail("raised: " ++ Printexc.to_string(exn))
          }
        ) {
        | Ok(_) => ()
        | Error(err) =>
          Alcotest.fail("refused: " ++ Action.Failure.show(err))
        };
      },
    ),
    test_case(
      "insert two case members after a case member",
      `Quick,
      () => {
        let prog = "module M = {\n  let a(i: Int): Int =\n    case i\n    | 0 => 1\n    | _ => 2\n    end\n} in\n1";
        let code = "let b(i: Int): Int =\n  case i\n  | 0 => 1\n  | _ => 2\n  end;\nlet c(i: Int): Int =\n  case i\n  | 0 => 1\n  | _ => 2\n  end";
        switch (
          try(run_agent_action(prog, Insert(After("M/a"), code))) {
          | exn => Alcotest.fail("raised: " ++ Printexc.to_string(exn))
          }
        ) {
        | Ok(_) => ()
        | Error(err) =>
          Alcotest.fail("refused: " ++ Action.Failure.show(err))
        };
      },
    ),
    test_case(
      "constructor patterns over a module ADT in a nested module",
      `Quick,
      () => {
        let prog = "module Outer = {\n  module M = {\n    type T =\n      + A(Int)\n      + B;\n    \n    let a(i: T): Int =\n      case i\n      | A(n) => n\n      | B => 0\n      end\n  };\n  \n  let z = 1\n} in\n1";
        let code = "let heal(i: T): Int =\n  case i\n  | A(n) => n\n  | B => 0\n  end;\nlet blast(i: T): Int =\n  case i\n  | A(_) => 0\n  | B => 0\n  end";
        switch (
          try(run_agent_action(prog, Insert(After("Outer/M/a"), code))) {
          | exn => Alcotest.fail("raised: " ++ Printexc.to_string(exn))
          }
        ) {
        | Ok(_) => ()
        | Error(err) =>
          Alcotest.fail("refused: " ++ Action.Failure.show(err))
        };
      },
    ),
    test_case(
      "insert after a module member with TWO case members (dungeon run: Failure nth / Exp patterns)",
      `Quick,
      () => {
        let prog = {js|module Creatures = {
  module Items = {
    type Item =
      + Potion(Int)
      + Bomb(Int)
      + Torch;

    let name(i: Item): String =
      case i
      | Potion(n) => "potion(" ++ Show.int(n) ++ ")"
      | Bomb(n) => "bomb(" ++ Show.int(n) ++ ")"
      | Torch => "torch"
      end
  };
  let z = 1
} in
1|js};
        let code = {js|let heal_amount(i: Item): Int =
  case i
  | Potion(n) => n
  | Bomb(_) => 0
  | Torch => 0
  end;
let blast_amount(i: Item): Int =
  case i
  | Potion(_) => 0
  | Bomb(n) => n
  | Torch => 0
  end|js};
        switch (
          try(
            run_agent_action(
              prog,
              Insert(After("Creatures/Items/name"), code),
            )
          ) {
          | exn => Alcotest.fail("raised: " ++ Printexc.to_string(exn))
          }
        ) {
        | Ok(_) => ()
        | Error(err) =>
          Alcotest.fail("refused: " ++ Action.Failure.show(err))
        };
      },
    ),
    test_case(
      "insert after a nested module member does not raise (dungeon run: Failure nth)",
      `Quick,
      () => {
        let code = "module Creatures = {\n  module Items = {\n    let name = fun i -> \"sword\";\n    let has = fun (i, c) -> true\n  };\n  let label = fun c -> \"x\"\n} in\n1";
        switch (
          run_agent_action(
            code,
            Insert(
              After("Creatures/Items/name"),
              "let weight = fun i -> 3;",
            ),
          )
        ) {
        | Ok(_) => ()
        | Error(err) =>
          Alcotest.fail(
            "insert_after member failed: " ++ Action.Failure.show(err),
          )
        };
      },
    ),
    test_case(
      "replace then insert after new binding path resolves (regression: agent bug 2)",
      `Quick,
      () => {
        let result =
          apply_chain_render(
            "let speed = 1 in 2",
            [
              Replace("speed", "let is_winner = true in"),
              Insert(After("is_winner"), "let score = 0 in"),
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
          switch (run_agent_action(code, Delete("b"))) {
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
            Structural(Insert(Before("c"), "let d = a * 2 in")),
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
          switch (run_agent_action(code, Rename("a", None, "x"))) {
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
            Structural(Replace("x:definition", "100")),
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
      "replace :definition of type alias",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let a = 1 in a",
            Replace("T:definition", "Bool"),
          );
        check_rendered(
          "update_type_alias_def",
          "type T = Bool in let a = 1 in a",
          result,
        );
      },
    ),
    test_case(
      "delete type alias binding clause",
      `Quick,
      () => {
        let result =
          apply_and_render("type T = Int in let a = 1 in a", Delete("T"));
        check_rendered("delete_type_alias", "let a = 1 in a", result);
      },
    ),
    test_case(
      "insert before type alias",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let a : T = 1 in a",
            Insert(Before("T"), "let z = 0 in"),
          );
        check_rendered(
          "insert_before_type_alias",
          "let z = 0 in type T = Int in let a : T = 1 in a",
          result,
        );
      },
    ),
    test_case(
      "insert after type alias",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let a : T = 1 in a",
            Insert(After("T"), "let z = 0 in"),
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
      "replace :definition with if-else",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Replace("b:definition", "if a > 0 then 10 else 20"),
          );
        check_rendered(
          "update_def_ifelse",
          "let a = 1 in let b = if a > 0 then 10 else 20 in a + b",
          result,
        );
      },
    ),
    test_case(
      "replace :definition with let-in expression",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in a",
            Replace("a:definition", "let x = 2 in x + 1"),
          );
        check_rendered(
          "update_def_let_in",
          "let a = let x = 2 in x + 1 in a",
          result,
        );
      },
    ),
    test_case(
      "replace :definition with recursive function",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f = fun x -> x in f(5)",
            Replace(
              "f:definition",
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
      "replace :definition with module body (lowercase let)",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = 1 in m",
            Replace("m:definition", "{ let x = 1; let y = 2 }"),
          );
        check_rendered(
          "update_def_module",
          "let m = { let x = 1; let y = 2 } in m",
          result,
        );
      },
    ),
    test_case("replace :definition with capitalized module M", `Quick, () => {
      switch (
        run_agent_action(
          "module M = { let x = 1 } in M.x",
          Replace("M:definition", "{ let x = 10 }"),
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
      "delete removes capitalized module M",
      `Quick,
      () => {
        let result =
          apply_and_render("module M = { let x = 1 } in 42", Delete("M"));
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
            Insert(After("a"), "let double = fun x -> x * 2 in"),
          );
        check_rendered(
          "insert_function",
          "let a = 1 in let double = fun x -> x * 2 in a",
          result,
        );
      },
    ),
    test_case(
      "replace to annotated function",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let f = 1 in f",
            Replace("f", "let f : Int -> Int = fun x -> x + 1 in"),
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
   18. AGENT CONTEXT — node-map paths the view relies on
   ============================================================ */

let agent_context_tests = (
  "AgentContext",
  [
    test_case(
      "path M resolves for capitalized module",
      `Quick,
      () => {
        let node_map =
          build_node_map("module M = { let x = 1; let y = 2 } in M.x + M.y");
        check(bool, "path M resolves", true, path_id(node_map, "M") != None);
      },
    ),
  ],
);

/* ============================================================
   19. ERROR PRINT — static error reporting
   ============================================================ */

/* a module naming itself in its own body: the free constructor and the
   Dot on it say why, under either analysis */
let self_reference_errors = () => {
  let want = "`RunningSums` isn't in scope inside its own body; use `running_max` directly";
  let mentions = (needle, e) => CorpusUtil.contains(needle, e);
  let body = "module RunningSums = {\n  let running_max = 3;\n  let twice = RunningSums.running_max * 2\n}";
  List.iter(
    ((root: Sort.t, code)) => {
      let term =
        switch (Parser.to_zipper(~root, code)) {
        | Some(z) => MakeTerm.from_zip_for_sem(z, ~root).term
        | None => fail("Failed to parse: " ++ code)
        };
      List.iter(
        ((what, info_map)) => {
          let what = Sort.show(root) ++ " " ++ what;
          let errors = ErrorPrint.all(info_map);
          check(
            bool,
            what ++ ": says why",
            true,
            List.exists(mentions(want), errors),
          );
          check(
            list(string),
            what ++ ": nothing else",
            [],
            List.filter(e => !mentions(want, e), errors),
          );
        },
        [
          (
            "monolithic",
            fst(Statics.mk(CoreSettings.on, DefStatics.ctx0, term)),
          ),
          (
            "DefStatics",
            DefStatics.calc(~settings=CoreSettings.on, term).merged,
          ),
        ],
      );
    },
    [
      (Exp, body ++ " in\nRunningSums.twice"),
      (Mod, body ++ ";\nRunningSums.twice"),
    ],
  );
  let errors = ErrorPrint.all(mk_statics(mk_zipper("let y = Nope.x in y")));
  check(
    bool,
    "another module's name stays undefined",
    true,
    List.exists(mentions("Constructor is not defined"), errors),
  );
};

/* a tuple type prints parenthesized wherever it stands, but a
   constructor's argument keeps only the constructor's own parens */
let printed_parens = () => {
  let says = (code, expected) =>
    check(
      bool,
      code ++ " says " ++ expected,
      true,
      List.exists(
        CorpusUtil.contains(expected),
        ErrorPrint.all(mk_statics(mk_zipper(code))),
      ),
    );
  says(
    "type Point = (Int, Int) in let p : (Point, Bool) = 1 in p",
    "Expecting type (Point, Bool) but got inconsistent type Int",
  );
  says(
    "let x : Int = ((1, true), 2) in x",
    "inconsistent type ((Int, Bool), Int)",
  );
  says(
    "let f : (Int, Int) -> Int = 1 in f",
    "Expecting type (Int, Int) -> Int",
  );
  says(
    "let f : Int -> (Int, Bool) = 1 in f",
    "Expecting type Int -> (Int, Bool)",
  );
  says(
    "type S = + A(Int, Bool) + B in let x : Int = A(1, true) in x",
    "inconsistent type + A(Int, Bool) + B",
  );
  says(
    "type S = + A(Int, Bool) + B in let x : Int = A in x",
    "inconsistent type (Int, Bool) -> S",
  );
  says("let x : () = 1 in x", "Expecting type () but");
  says("let x : Int = (?, ?) in x", "inconsistent type (?, ?)");
  says(
    "type S = + A(Int, Bool) + B in let f = fun (A(x, y): S) -> x in f",
    "fun (A(x, y)) -> x",
  );
};

let error_print_tests = (
  "ErrorPrint",
  [
    test_case("tuple types print parenthesized", `Quick, printed_parens),
    test_case(
      "a module naming itself in its own body",
      `Quick,
      self_reference_errors,
    ),
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
        let tools = CompositionUtils.tools;
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
        let tools = CompositionUtils.tools;
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
        let tools = CompositionUtils.tools;
        /* 33 while DocPacks is empty (read_docs is not offered) */
        check(int, "tool count", 33, List.length(tools));
      },
    ),
    test_case(
      "get_tool_name extracts name correctly",
      `Quick,
      () => {
        let tools = CompositionUtils.tools;
        let first_tool = List.nth(tools, 0);
        switch (get_tool_name(first_tool)) {
        | Some("open") => ()
        | Some(name) =>
          Alcotest.fail("Expected 'open', got '" ++ name ++ "'")
        | None => Alcotest.fail("get_tool_name returned None")
        };
      },
    ),
    test_case(
      "get_tool_description returns non-empty for all tools",
      `Quick,
      () => {
        let tools = CompositionUtils.tools;
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
        let tools = CompositionUtils.tools;
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
      "edit tools with a target require it",
      `Quick,
      () => {
        /* insert takes a place instead */
        let edit_tool_names = ["replace", "delete", "move", "rename"];
        let tools = CompositionUtils.tools;
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
                    let has_target =
                      List.exists(
                        r =>
                          switch (r) {
                          | `String("target") => true
                          | _ => false
                          },
                        required,
                      );
                    check(bool, name ++ " requires target", true, has_target);
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
        let tools = CompositionUtils.tools;
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
        let declared = List.filter_map(get_tool_name, CompositionUtils.tools);
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
        check(string, "open", "View", cat("open"));
        check(string, "place_probe", "View", cat("place_probe"));
        check(string, "replace", "Edit", cat("replace"));
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
          ["insert", "replace", "delete", "move", "rename"],
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
        switch (Some(outline_of(z))) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (path_id(nm, "x")) {
          | None => Alcotest.fail("expected path x")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.place_statics_at(~syntax, id, info_map, z);
            switch (ProbeTargets.probe_status(id, info_map, z2.refractors)) {
            | Statics(_) => ()
            | _ =>
              Alcotest.fail("expected Statics status after place_statics_at")
            };
            let z3 = ProbePerform.remove_statics_at(id, info_map, z2);
            switch (ProbeTargets.probe_status(id, info_map, z3.refractors)) {
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
        switch (Some(outline_of(z))) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (path_id(nm, "x")) {
          | None => Alcotest.fail("expected path x")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.add_manual(~syntax, id, info_map, z);
            switch (ProbeTargets.probe_status(id, info_map, z2.refractors)) {
            | Manual(_) => ()
            | _ => Alcotest.fail("expected Manual probe after add_manual")
            };
            let z3 = ProbePerform.remove_statics_at(id, info_map, z2);
            switch (ProbeTargets.probe_status(id, info_map, z3.refractors)) {
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
      "replace :definition replaces binding whose definition uses slider projector",
      `Quick,
      () => {
        let code = {|let x = ^^slider(50) in x + 1|};
        let result = apply_and_render(code, Replace("x:definition", "100"));
        check_rendered(
          "slider projector def replaced",
          "let x = 100 in x + 1",
          result,
        );
      },
    ),
    test_case(
      "replace :definition strips slider projector when new def is not an Int literal",
      `Quick,
      () => {
        let code = {|let speed = ^^slider(50) in speed|};
        let result =
          apply_and_render(code, Replace("speed:definition", "\"hello\""));
        check_rendered(
          "slider stripped for string def",
          "let speed = \"hello\" in speed",
          result,
        );
      },
    ),
    test_case(
      "replace :definition replaces checkbox projector binding",
      `Quick,
      () => {
        let code = {|let b = ^^check(true) in b|};
        let result =
          apply_and_render(code, Replace("b:definition", "false"));
        check_rendered(
          "checkbox projector def replaced",
          "let b = false in b",
          result,
        );
      },
    ),
    test_case(
      "replace => works when a binding uses nested fold + slider projectors",
      `Quick,
      () => {
        let code = {|let n = ^^fold(^^slider(10) + 1) in n|};
        let result = apply_and_render(code, Replace("=>", "0"));
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
        switch (Some(outline_of(z))) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (path_id(nm, "v")) {
          | None => Alcotest.fail("expected path v")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.place_statics_at(~syntax, id, info_map, z);
            switch (ProbeTargets.probe_status(id, info_map, z2.refractors)) {
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
        switch (Some(outline_of(z))) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (path_id(nm, "rows")) {
          | None => Alcotest.fail("expected path rows")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.add_manual(~syntax, id, info_map, z);
            switch (ProbeTargets.probe_status(id, info_map, z2.refractors)) {
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
        check(bool, "x present", true, path_id(node_map, "x") != None);
      },
    ),
    test_case(
      "path_to_id resolves ascribed let after type alias",
      `Quick,
      () => {
        let node_map = build_node_map("type T = Int in let x : T = 1 in x");
        check(bool, "x present", true, path_id(node_map, "x") != None);
        check(bool, "T present", true, path_id(node_map, "T") != None);
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
          path_id(node_map, "x") != None,
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
          path_id(node_map, "outer/inner") != None,
        );
        check(
          bool,
          "bare inner not ambiguously at top level",
          true,
          path_id(node_map, "inner") == None,
        );
      },
    ),
    test_case(
      "delete on simple ascribed let (body preserved)",
      `Quick,
      () => {
        let result = apply_and_render("let x : Int = 1 in 2", Delete("x"));
        check_rendered("delete ascribed x", "2", result);
      },
    ),
    test_case(
      "delete on ascribed let in middle of chain",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b : Int = 2 in let c = 3 in a + c",
            Delete("b"),
          );
        check_rendered(
          "delete ascribed middle b",
          "let a = 1 in let c = 3 in a + c",
          result,
        );
      },
    ),
    test_case(
      "delete on ascribed let following type chain",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "type T = Int in let x : T = 1 in 0",
            Delete("x"),
          );
        check_rendered(
          "delete x after type chain",
          "type T = Int in 0",
          result,
        );
      },
    ),
    test_case(
      "delete on ascribed let with complex list-literal body",
      `Quick,
      () => {
        /* Shape close to chess: ascribed let whose definition is a nested
           list literal of constructor applications. */
        let code = "type Color = + White + Black in type Board = [[Color]] in let initial : Board = [[White, Black], [Black, White]] in ?";
        let result = apply_and_render(code, Delete("initial"));
        check_rendered(
          "delete ascribed let with list-literal def",
          "type Color = + White + Black in type Board = [[Color]] in ?",
          result,
        );
      },
    ),
    test_case(
      "replace => after an ascribed let",
      `Quick,
      () => {
        let result =
          apply_and_render("let x : Int = 1 in x + 2", Replace("=>", "?"));
        check_rendered(
          "delete body of ascribed x",
          "let x : Int = 1 in ?",
          result,
        );
      },
    ),
    test_case(
      "replace :definition on ascribed let preserves ascription",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 1 in x + 1",
            Replace("x:definition", "42"),
          );
        check_rendered(
          "update def of ascribed x",
          "let x : Int = 42 in x + 1",
          result,
        );
      },
    ),
    test_case(
      "replace replaces whole let header incl. ascription",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 1 in x",
            Replace("x", "let x : Float = 1.0 in"),
          );
        check_rendered(
          "update binding clause with new ascription",
          "let x : Float = 1.0 in x",
          result,
        );
      },
    ),
    test_case(
      "insert before ascribed let",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 1 in x",
            Insert(Before("x"), "let y : Int = 2 in"),
          );
        check_rendered(
          "insert ascribed let before ascribed let",
          "let y : Int = 2 in let x : Int = 1 in x",
          result,
        );
      },
    ),
    test_case(
      "insert after ascribed let",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let x : Int = 1 in x",
            Insert(After("x"), "let y : Int = 2 in"),
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
           turn, users first, should end at an all-holes program. */
        let code = "type Color = + White + Black in type PieceType = + Pawn + Knight + Bishop + Rook + Queen + King in type Piece = (Color, PieceType) in type Square = + Empty + Occupied(Piece) in type Board = [[Square]] in let initial_board : Board = [[Occupied((White, Rook))]] in ?";
        let result =
          apply_chain_render(
            code,
            [
              Delete("initial_board"),
              Delete("Board"),
              Delete("Square"),
              Delete("Piece"),
              Delete("PieceType"),
              Delete("Color"),
            ],
          );
        check_rendered("deleting all bindings leaves hole", "?", result);
      },
    ),
    test_case(
      "verbatim chess program: Piece is present in node map",
      `Quick,
      () => {
        /* Exact program from the live-editor repro. `delete Piece`
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
          path_id(node_map, "Color") != None,
        );
        check(
          bool,
          "PieceType present",
          true,
          path_id(node_map, "PieceType") != None,
        );
        check(
          bool,
          "Piece present",
          true,
          path_id(node_map, "Piece") != None,
        );
        check(
          bool,
          "Square present",
          true,
          path_id(node_map, "Square") != None,
        );
        check(
          bool,
          "Board present",
          true,
          path_id(node_map, "Board") != None,
        );
        check(
          bool,
          "initial_board present",
          true,
          path_id(node_map, "initial_board") != None,
        );
      },
    ),
    test_case(
      "verbatim chess program: delete Piece finds it, and its use",
      `Quick,
      () => {
        /* Single action repro of the exact user flow: send the chess program,
           ask to delete `Piece`. Should succeed (the binding exists). */
        let code = "type Color = + White + Black in type PieceType = + Pawn + Knight + Bishop + Rook + Queen + King in type Piece = (Color, PieceType) in type Square = + Empty + Occupied(Piece) in type Board = [[Square]] in let initial_board : Board = [\n    [Occupied((White, Rook)), Occupied((White, Knight)), Occupied((White, Bishop)), Occupied((White, Queen)), Occupied((White, King)), Occupied((White, Bishop)), Occupied((White, Knight)), Occupied((White, Rook))],\n    [Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn)), Occupied((White, Pawn))],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],\n    [Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn)), Occupied((Black, Pawn))],\n    [Occupied((Black, Rook)), Occupied((Black, Knight)), Occupied((Black, Bishop)), Occupied((Black, Queen)), Occupied((Black, King)), Occupied((Black, Bishop)), Occupied((Black, Knight)), Occupied((Black, Rook))]\n] in ?";
        /* the binding is found; Square still uses it, so the delete is
           refused naming that use */
        switch (run_agent_action(code, Delete("Piece"))) {
        | Error(Composition_action_failure(msg)) =>
          check(
            bool,
            "names its use: " ++ msg,
            true,
            Util.StringUtil.plain_search("used at Square", msg, 0) >= 0,
          )
        | Ok(_) => Alcotest.fail("deleted a type still in use")
        | Error(e) => Alcotest.fail(Action.Failure.show(e))
        };
      },
    ),
    test_case(
      "a delete's diff is its entry before, and nothing after",
      `Quick,
      () => {
        let code = "type Color = + White + Black in type Piece = (Color, Color) in let x = 1 in x";
        switch (
          CompositionGo.Reply.add(reply_on(code), ~index=0, Delete("Piece"))
        ) {
        | Error(e) => Alcotest.fail("delete failed: " ++ e)
        | Ok(r) =>
          let m = List.hd(CompositionGo.Reply.made(r));
          check(
            option(pair(string, option(string))),
            "old entry, no new one",
            Some(("type Piece = (Color, Color) in", None)),
            CompositionGo.diff(~root=Exp, m),
          );
        };
      },
    ),
    test_case(
      "deleting a type still used is refused, naming its uses",
      `Quick,
      () => {
        /* the chess program the live editor captured: Square uses Piece */
        let code = "type Color = + White + Black in type PieceType = + Pawn + Knight in type Piece = (Color, PieceType) in type Square = + Empty + Occupied(Piece) in type Board = [[Square]] in let initial_board : Board = [[Occupied((White, Pawn))]] in ?";
        switch (run_agent_action(code, Delete("Piece"))) {
        | Ok(_) => Alcotest.fail("deleted a type still in use")
        | Error(Composition_action_failure(msg)) =>
          check(
            bool,
            "names the use: " ++ msg,
            true,
            Util.StringUtil.plain_search("Piece, used at Square", msg, 0) >= 0,
          )
        | Error(e) => Alcotest.fail(Action.Failure.show(e))
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
        switch (Some(outline_of(z))) {
        | None =>
          Alcotest.fail("expected high-level node map for fib program")
        | Some(nm) =>
          switch (path_id(nm, "fib")) {
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
            switch (Some(outline_of(z2))) {
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
      "rename refuses what has no name, and a non-name",
      `Quick,
      () => {
        expect_failure_mentioning(
          "let a = 1 in test a == 1 end; a",
          Rename("test#1", None, "b"),
          ["is a test", "only a definition, type or module"],
          "rename_test",
        );
        expect_failure_mentioning(
          "let ? = 5 in 3",
          Rename("let#1", None, "x"),
          ["binds no name"],
          "rename_binderless",
        );
        expect_failure_mentioning(
          "let a = 1 in a",
          Rename("a", None, "B"),
          ["is not a value's name"],
          "rename_non_name",
        );
      },
    ),
    test_case(
      "code that copies the view's `#› … #` comments is refused",
      `Quick,
      () => {
        let why = {js|`#› … #` comments are added by Hazel|js};
        expect_failure_mentioning(
          "let a = 1 in a",
          Insert(After("a"), {js|let b = a + 1  #› 1 use #|js}),
          [why],
          "insert_view_comment",
        );
        expect_failure_mentioning(
          "let a = 1 in a",
          Replace("a:definition", {js|2  #› ≡ 2 #|js}),
          [why],
          "replace_view_comment",
        );
        check_rendered(
          "a program comment still goes in",
          "let a = # one # 1 in a",
          apply_and_render(
            "let a = 1 in a",
            Replace("a:definition", "# one # 1"),
          ),
        );
      },
    ),
    test_case(
      "sanity: normal rename still works",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let b = a + 1 in b + a",
            Rename("a", None, "q"),
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
          Rename("x", None, "y"),
          ["Not renamed to y", "change what they refer to"],
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
          Rename("b", None, "a"),
          ["Not renamed to a", "change what they refer to"],
          "rename_captures_outer_reference",
        )
      ),
    test_case(
      "tuple-pattern shape change leaving uses unbound is rejected", `Quick, ()
      /* not a rename: the pattern is replaced, and the error check finds
         x and y unbound */
      =>
        expect_failure_mentioning(
          "let (x, y) = ? in x + y",
          Replace("x:pattern", "(a, b, c)"),
          ["static errors", "Variable x is not bound"],
          "rename_tuple_arity_mismatch",
        )
      ),
    test_case(
      "naming a hole pattern is allowed (binds 0 names before)",
      `Quick,
      () => {
        let result =
          apply_and_render("let ? = 5 in 3", Replace("let#1:pattern", "x"));
        check_rendered("rename_hole_pattern", "let x = 5 in 3", result);
      },
    ),
    test_case(
      "ambiguous path to edit tool yields disambiguation error", `Quick, () =>
      expect_failure_mentioning(
        "let x = 1 in let x = 2 in x",
        Replace("x:definition", "9"),
        ["ambiguous", "x#1", "x#2"],
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
            Replace("x#2:definition", "9"),
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
          path_id(node_map, "x") == None,
        );
        let id1 = path_id(node_map, "x#1");
        let id2 = path_id(node_map, "x#2");
        check(bool, "x#1 resolves", true, id1 != None);
        check(bool, "x#2 resolves", true, id2 != None);
        check(bool, "x#1 and x#2 differ", true, id1 != id2);
        let order =
          List.map(
            (e: Outline.entry) => Some(e.id),
            Outline.all(node_map),
          );
        check(
          list(option(testable(Fmt.of_to_string(Id.str3), Id.equal))),
          "x#1, then x#2, in program order",
          [id1, id2],
          List.filter(id => id == id1 || id == id2, order),
        );
        check(
          bool,
          "out-of-range occurrence is None",
          true,
          path_id(node_map, "x#3") == None,
        );
      },
    ),
    test_case(
      "an ambiguous path's error lists the disambiguated paths",
      `Quick,
      () => {
        let msg = path_error("let x = 1 in let x = 2 in x", "x");
        List.iter(
          needle =>
            check(
              bool,
              "mentions " ++ needle ++ ": " ++ msg,
              true,
              contains_str(~needle, msg),
            ),
          ["ambiguous", "x#1", "x#2"],
        );
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
          path_id(node_map, "b") != None,
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
      "replace :definition strips then re-indents canonically", `Quick, () => {
      check_rendered_exact(
        "update_definition_indented",
        "let a = fun x ->\n  x + 1\nin a",
        apply_and_render(
          "let a = 1 in a",
          Replace("a:definition", "fun x ->\n  x + 1"),
        ),
      )
    }),
    test_case("replace keeps the code's own indentation", `Quick, () => {
      /* formatting is local: new code keeps its lines' relative
         indentation, at its place's */
      check_rendered_exact(
        "replace_indented",
        "let a = 1 in let b = 2 in\n  a + b",
        apply_and_render(
          "let a = 1 in a",
          Replace("=>", "let b = 2 in\n  a + b"),
        ),
      )
    }),
    test_case("rename strips leading indentation", `Quick, () => {
      check_rendered_exact(
        "update_pattern_indented",
        "let b = 1 in b",
        apply_and_render("let a = 1 in a", Rename("a", None, "  b")),
      )
    }),
    test_case("replace strips then re-indents canonically", `Quick, () => {
      check_rendered_exact(
        "update_binding_clause_indented",
        "let a =\n  2\nin a",
        apply_and_render("let a = 1 in a", Replace("a", "let a =\n  2 in")),
      )
    }),
    test_case("Perform-level insert strips leading indentation", `Quick, () => {
      /* Direct Structural action, bypassing the tool-arg parsing layer:
         the trim must live at the paste funnel, not per tool arm */
      check_rendered_exact(
        "insert_perform_level_indented",
        "let a = 1 in let c = 3 in a",
        apply_and_render(
          "let a = 1 in a",
          Insert(After("a"), "  let c = 3 in"),
        ),
      )
    }),
    test_case("insert of `let eval = ...` fails naming keyword", `Quick, () => {
      expect_failure_mentioning(
        "let a = 1 in a",
        Insert(After("a"), "let eval = 1 in"),
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
          Insert(After("a"), "let evalStep = 1 in"),
        ),
      )
    }),
    test_case("unannotated binding without self-ref succeeds", `Quick, () => {
      check_rendered(
        "insert_unannotated_no_self_ref",
        "let a = 1 in let f = fun n -> n(0) in a",
        apply_and_render(
          "let a = 1 in a",
          Insert(After("a"), "let f = fun n -> n(0) in"),
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
          Insert(After("a"), "let f = fun n -> f(0) in"),
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
            After("a"),
            "let f : Int -> Int = fun n -> if n < 1 then 0 else f(n - 1) in",
          ),
        ),
      )
    }),
    test_case(
      "replace => with annotated `let eval` fails naming keyword", `Quick, () => {
      expect_failure_mentioning(
        "let a = 1 in a",
        Replace("=>", "let eval : Int -> Int = fun t -> t in ?"),
        ["`eval` is a reserved keyword"],
        "update_body_let_eval",
      )
    }),
    test_case(
      "reserved word inside a string literal is not rejected", `Quick, () => {
      /* the binder text scan alone fires on `let eval` INSIDE the
         string; the structural witness (an incomplete tile the word
         actually leads) keeps legitimate code insertable */
      check_rendered(
        "insert_reserved_in_string",
        "let a = 1 in let msg = \"let eval\" in a",
        apply_and_render(
          "let a = 1 in a",
          Insert(After("a"), "let msg = \"let eval\" in"),
        ),
      )
    }),
    test_case(
      "keyword note absent when no reserved word is misused", `Quick, () => {
      switch (
        run_agent_action(
          "let a = 1 in a",
          Insert(After("a"), "let b = unboundvar in"),
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
        let code = "let contains(xs, y) = 1 in contains([], 2)";
        let node_map = build_node_map(code);
        check(
          option(string),
          "name is bare head",
          Some("contains"),
          resolved(code, "contains"),
        );
        check(
          bool,
          "full rendered pattern is not a path",
          true,
          path_id(node_map, "contains((xs, y))") == None,
        );
      },
    ),
    test_case(
      "one-param, zero-param, and return-annotated sugar use bare name",
      `Quick,
      () => {
        let nm1 = build_node_map("let inc(x) = x + 1 in inc(1)");
        check(bool, "one-param path", true, path_id(nm1, "inc") != None);
        let nm0 = build_node_map("let f() = 3 in f()");
        check(bool, "zero-param path", true, path_id(nm0, "f") != None);
        let nm_asc = build_node_map("let g(x): Int = x in g(1)");
        check(
          bool,
          "return-annotated path",
          true,
          path_id(nm_asc, "g") != None,
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
          path_id(node_map, "f") == None,
        );
        check(bool, "f#1 resolves", true, path_id(node_map, "f#1") != None);
        check(bool, "f#2 resolves", true, path_id(node_map, "f#2") != None);
        expect_failure_mentioning(
          "let f(x) = x in let f = 1 in f",
          Replace("f:definition", "9"),
          ["ambiguous", "f#1", "f#2"],
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
            Replace("f#2:definition", "9"),
          );
        check_rendered(
          "fn_sugar_hash_k_edit",
          "let f(x) = x in let f = 9 in f",
          result,
        );
      },
    ),
    test_case(
      "replace :definition replaces RHS of sugared fn",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let inc(x) = x + 1 in inc(3)",
            Replace("inc:definition", "x + 2"),
          );
        check_rendered(
          "fn_sugar_update_def",
          "let inc(x) = x + 2 in inc(3)",
          result,
        );
      },
    ),
    test_case(
      "replace => after a sugared fn",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let inc(x) = x + 1 in inc(3)",
            Replace("=>", "inc(10)"),
          );
        check_rendered(
          "fn_sugar_update_body",
          "let inc(x) = x + 1 in inc(10)",
          result,
        );
      },
    ),
    test_case(
      "replace on sugared fn",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let inc(x) = x + 1 in inc(3)",
            Replace("inc", "let inc(x) = x * 2 in"),
          );
        check_rendered(
          "fn_sugar_update_clause",
          "let inc(x) = x * 2 in inc(3)",
          result,
        );
      },
    ),
    test_case(
      "delete on sugared fn",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let a = 1 in let inc(x) = x + 1 in a",
            Delete("inc"),
          );
        check_rendered("fn_sugar_delete_clause", "let a = 1 in a", result);
      },
    ),
    test_case(
      "rename renames fn keeping params; call sites update",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let inc(x) = x + 1 in inc(3) + inc(4)",
            Rename("inc", None, "bump"),
          );
        check_rendered(
          "fn_sugar_rename_fn",
          "let bump(x) = x + 1 in bump(3) + bump(4)",
          result,
        );
      },
    ),
    test_case(
      "rename renames recursive sugared fn incl. recursive call",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let fact(n) = if n < 2 then 1 else n * fact(n - 1) in fact(4)",
            Rename("fact", None, "f"),
          );
        check_rendered(
          "fn_sugar_rename_recursive",
          "let f(n) = if n < 2 then 1 else n * f(n - 1) in f(4)",
          result,
        );
      },
    ),
    test_case("a pattern replace leaving uses unbound is refused", `Quick, () =>
      expect_composition_failure(
        "let inc(x) = x + 1 in inc(3)",
        Replace("inc:pattern", "inc(z)"),
        "param_replace_unbinds",
      )
    ),
    test_case(
      "param-count change on sugared fn is rejected with bound-name counts",
      `Quick,
      ()
      /* old pattern binds inc,x = 2 names; new binds inc,a,b = 3 */
      =>
        expect_failure_mentioning(
          "let inc(x) = x + 1 in inc(3)",
          Replace("inc:pattern", "inc(a, b)"),
          ["static errors", "Variable x is not bound"],
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
        switch (Some(outline_of(z))) {
        | None => Alcotest.fail("expected high-level node map")
        | Some(nm) =>
          switch (path_id(nm, "f")) {
          | None => Alcotest.fail("expected path f")
          | Some(id) =>
            let syntax = CachedSyntax.init(z);
            let z2 = ProbePerform.add_manual(~syntax, id, info_map, z);
            switch (ProbeTargets.probe_status(id, info_map, z2.refractors)) {
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
        let seg =
          Zipper.unselect_and_zip(mk_zipper("let f(x) = x + 1 in f(2)"));
        check(
          bool,
          "def target resolves",
          true,
          Web.AgentToolCallHandler.definition_id(~root=Exp, seg, "f") != None,
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
           (replacing a hole adds no linebreaks — space-pad only —
           so the accumulation shape is insert-driven.) */
        let rendered =
          apply_chain_render(
            "let a = 1 in ?",
            [
              Insert(After("a"), "let b = 2 in"),
              Insert(After("b"), "let c = 3 in"),
              Insert(After("c"), "let d = 4 in"),
            ],
          );
        check(
          bool,
          "at most one trailing linebreak",
          true,
          trailing_newlines(rendered) <= 1,
        );
        /* a one-line chain stays on its line */
        check_rendered_exact(
          "chained insert_after spacing",
          "let a = 1 in let b = 2 in let c = 3 in let d = 4 in ?",
          rendered,
        );
      },
    ),
    test_case("prepend leaves no leading blank line", `Quick, () => {
      switch (
        run_insert_at_program_boundary("1", Before(""), "let a = 2 in")
      ) {
      | Ok(z) =>
        let rendered = render_zipper(z);
        check(bool, "no leading newline", false, leading_newline(rendered));
        check_rendered_exact(
          "prepend spacing",
          "let a = 2 in\n\n1",
          rendered,
        );
      | Error(err) =>
        Alcotest.fail("prepend failed: " ++ Action.Failure.show(err))
      }
    }),
    test_case(
      "a new binding takes its neighbours' spacing",
      `Quick,
      () => {
        check_rendered_exact(
          "inline chain",
          "let a = 1 in let b = 2 in let c = 3 in a + b",
          apply_and_render(
            "let a = 1 in let b = 2 in a + b",
            Insert(After("b"), "let c = 3 in"),
          ),
        );
        check_rendered_exact(
          "blank lines between",
          "let a = 1 in\n\nlet b = 2 in\n\nlet c = 3 in\n\na + b",
          apply_and_render(
            "let a = 1 in\n\nlet b = 2 in\n\na + b",
            Insert(After("b"), "let c = 3 in"),
          ),
        );
      },
    ),
    test_case(
      "normalization is idempotent across edits",
      `Quick,
      () => {
        check_rendered_exact(
          "one edit",
          "let a = 5 in\n\nlet b = 2 in a",
          apply_and_render(
            "let a = 1 in\n\nlet b = 2 in a",
            Replace("a:definition", "5"),
          ),
        );
        check_rendered_exact(
          "two edits, spacing stable",
          "let a = 6 in\n\nlet b = 2 in a",
          apply_chain_render(
            "let a = 1 in\n\nlet b = 2 in a",
            [Replace("a:definition", "5"), Replace("a:definition", "6")],
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
            Replace("a:definition", "5"),
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

/* Module MEMBER operations: paths descend into module literals
   ("m/x"). The node map chains through the statics expansion
   (wrapper Let/TyAlias keyed by Mod item ids), so the same eight tools
   work at member granularity. */
/* a livelit definition: its `^name` binds, its literal's members are
   paths as a module's */
let livelit_src = "let ^pct = {\n  type T = Int;\n  let init = 50\n} in\n^pct(1)";

let module_member_tests = (
  "AgentTools.ModuleMembers",
  [
    test_case("a livelit's member: replace :definition", `Quick, () =>
      check_rendered_exact(
        "livelit_member_def",
        "let ^pct = {\n  type T = Int;\n  let init = 75\n} in\n^pct(1)",
        apply_and_render(livelit_src, Replace("^pct/init:definition", "75")),
      )
    ),
    test_case("a livelit's members: insert last_in", `Quick, () =>
      check_rendered_exact(
        "livelit_member_insert",
        "let ^pct = {\n  type T = Int;\n  let init = 50;\n\n  let step = 5\n} in\n^pct(1)",
        apply_and_render(
          livelit_src,
          Insert(LastIn("^pct"), "let step = 5"),
        ),
      )
    ),
    test_case(
      "replace :definition on a member",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let y = 2 } in m",
            Replace("m/x:definition", "5"),
          );
        check_rendered(
          "member_update_def",
          "let m = { let x = 5; let y = 2 } in m",
          result,
        );
      },
    ),
    test_case(
      "replace :definition on a type member",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { type T = Int; let x = 1 } in m",
            Replace("m/T:definition", "Bool"),
          );
        check_rendered(
          "member_update_type_def",
          "let m = { type T = Bool; let x = 1 } in m",
          result,
        );
      },
    ),
    test_case(
      "replace :definition on a nested inner let",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = let inner = 1 in inner; let y = 2 } in m",
            Replace("m/x/inner:definition", "7"),
          );
        check_rendered(
          "member_nested_inner",
          "let m = { let x = let inner = 7 in inner; let y = 2 } in m",
          result,
        );
      },
    ),
    test_case(
      "insert after a middle member",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let y = 2 } in m",
            Insert(After("m/x"), "let z = 9"),
          );
        check_rendered(
          "member_insert_after",
          "let m = { let x = 1; let z = 9; let y = 2 } in m",
          result,
        );
      },
    ),
    test_case(
      "insert after the last member",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let y = 2 } in m",
            Insert(After("m/y"), "let z = 9"),
          );
        check_rendered(
          "member_insert_after_last",
          "let m = { let x = 1; let y = 2; let z = 9 } in m",
          result,
        );
      },
    ),
    test_case(
      "insert before the first member",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let y = 2 } in m",
            Insert(Before("m/x"), "let z = 9"),
          );
        check_rendered(
          "member_insert_before_first",
          "let m = { let z = 9; let x = 1; let y = 2 } in m",
          result,
        );
      },
    ),
    test_case(
      "delete a middle member cleans its separator",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let y = 2; let z = 3 } in m",
            Delete("m/y"),
          );
        check_rendered(
          "member_delete_middle",
          "let m = { let x = 1; let z = 3 } in m",
          result,
        );
      },
    ),
    test_case(
      "delete the last member cleans its separator",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let y = 2 } in m",
            Delete("m/y"),
          );
        check_rendered(
          "member_delete_last",
          "let m = { let x = 1 } in m",
          result,
        );
      },
    ),
    test_case(
      "delete the first member cleans its separator",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let y = 2 } in m",
            Delete("m/x"),
          );
        check_rendered(
          "member_delete_first",
          "let m = { let y = 2 } in m",
          result,
        );
      },
    ),
    test_case(
      "replace replaces a whole member",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let y = 2 } in m",
            Replace("m/x", "let w = 8"),
          );
        check_rendered(
          "member_update_clause",
          "let m = { let w = 8; let y = 2 } in m",
          result,
        );
      },
    ),
    test_case(
      "rename renames member use sites",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let y = x + 2 } in m",
            Rename("m/x", None, "base"),
          );
        check_rendered(
          "member_rename",
          "let m = { let base = 1; let y = base + 2 } in m",
          result,
        );
      },
    ),
    test_case(
      "replace :definition on a member",
      `Quick,
      () => {
        check_rendered(
          "member_body",
          "let m = { let x = 3; let y = 2 } in m",
          apply_and_render(
            "let m = { let x = 1; let y = 2 } in m",
            Replace("m/x:definition", "3"),
          ),
        );
        check_rendered(
          "member_fun_body",
          "let m = { let f = fun n -> n * 2; let y = 2 } in m",
          apply_and_render(
            "let m = { let f = fun n -> n + 1; let y = 2 } in m",
            Replace("m/f:definition", "fun n -> n * 2"),
          ),
        );
      },
    ),
    test_case(
      "member blank-line policy on multiline modules",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = {\n  let x = 1;\n  let y = 2\n} in m",
            Insert(After("m/x"), "let z = 9"),
          );
        /* a new member is parted from its neighbours by blank lines,
           whatever theirs */
        check_rendered_exact(
          "member_blank_lines",
          "let m = {\n  let x = 1;\n\n  let z = 9;\n\n  let y = 2\n} in m",
          result,
        );
      },
    ),
    test_case(
      "renaming a member used outside relabels its M.x",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "module M = { let x = 1; let y = 2 } in M.x",
            Rename("M/x", None, "z"),
          );
        check_rendered(
          "member_rename_external",
          "module M = { let z = 1; let y = 2 } in M.z",
          result,
        );
      },
    ),
    test_case(
      "oversized chunk on the fast path succeeds",
      `Quick,
      () => {
        /* The size cap guards only the quadratic fallback; Menhir-
           parseable chunks of any size take the linear fast path. */
        let big =
          String.concat(" + ", List.init(400, i => string_of_int(i)));
        switch (
          run_agent_action("let a = 1 in ?", Replace("a:definition", big))
        ) {
        | Ok(_) => ()
        | Error(e) =>
          Alcotest.fail(
            "fast path should take oversized parseable chunks: "
            ++ Action.Failure.show(e),
          )
        };
      },
    ),
    test_case(
      "an unreadable chunk is rejected with the parser's note",
      `Quick,
      () => {
        /* Bare comma tuple: Menhir-rejected even with hole completion.
           New code is read by the batch parser only */
        let big = String.concat(", ", List.init(900, i => string_of_int(i)));
        switch (
          run_agent_action("let a = 1 in ?", Replace("a:definition", big))
        ) {
        | Ok(_) => Alcotest.fail("expected oversized fallback to be rejected")
        | Error(Action.Failure.Composition_action_failure(msg)) =>
          check(
            bool,
            "chunk_rejection_names_the_parser",
            true,
            StringUtil.match(StringUtil.regexp("does not parse"), msg),
          )
        | Error(e) =>
          Alcotest.fail(
            "unexpected failure kind: " ++ Action.Failure.show(e),
          )
        };
      },
    ),
    test_case(
      "member paths disambiguate with #k",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { let x = 1; let x = 2 } in m",
            Replace("m/x#2:definition", "5"),
          );
        check_rendered(
          "member_hash_k",
          "let m = { let x = 1; let x = 5 } in m",
          result,
        );
      },
    ),
    test_case(
      "a module opening with a test keeps its member paths",
      `Quick,
      () => {
        let result =
          apply_and_render(
            "let m = { test 1 == 1 end; let x = 1 } in m",
            Replace("m/x:definition", "5"),
          );
        check_rendered(
          "member_after_leading_test",
          "let m = { test 1 == 1 end; let x = 5 } in m",
          result,
        );
      },
    ),
  ],
);

/* The assistant on a Mod-rooted program (stdlib files, Mega-Mod): its top
   level is a module body, so top-level bindings are members too. */
let mod_root_program = "module M = {\n  let a = 1;\n  let b = a + 1;\n  type T = Int;\n  let c : T = b\n};\nlet a = 5;\nlet d : M.T = M.a + a;\nd";

let run_at_mod_root = (code: string, a: Action.Structural.t) => {
  let z = Zipper.unzip(CorpusUtil.seg_of(~root=Mod, code));
  Perform.go(
    ~settings=CoreSettings.on,
    ~statics=CachedStatics.empty,
    ~syntax=CachedSyntax.init(~root=Mod, z),
    ~root=Mod,
    Structural(a),
    {
      zipper: z,
      col_target: None,
    },
  );
};

let apply_at_mod_root = (~code=mod_root_program, a: Action.Structural.t) =>
  switch (run_at_mod_root(code, a)) {
  | Ok(z) => render_zipper(z)
  | Error(err) =>
    Alcotest.fail("Agent action failed: " ++ Action.Failure.show(err))
  };

/* [mod_root_program] with [line] replaced by [by] (a list: none deletes) */
let mod_root_with = (line: string, by: list(string)): string =>
  String.split_on_char('\n', mod_root_program)
  |> List.concat_map(l => l == line ? by : [l])
  |> String.concat("\n");

let mod_root_tests = (
  "AgentTools.ModRoot",
  [
    test_case("replace :definition on a member", `Quick, () =>
      check_rendered(
        "mod_root_update_member_def",
        mod_root_with("  let b = a + 1;", ["  let b = a + 2;"]),
        apply_at_mod_root(Replace("M/b:definition", "a + 2")),
      )
    ),
    test_case("replace :definition on a top-level binding", `Quick, () =>
      check_rendered(
        "mod_root_update_top_def",
        mod_root_with("let a = 5;", ["let a = 6;"]),
        apply_at_mod_root(Replace("a:definition", "6")),
      )
    ),
    test_case("replace on a member", `Quick, () =>
      check_rendered(
        "mod_root_update_clause",
        mod_root_with("  let c : T = b", ["  let c : T = 7"]),
        apply_at_mod_root(Replace("M/c", "let c : T = 7")),
      )
    ),
    test_case("replace :definition on a top-level binding", `Quick, () =>
      check_rendered(
        "mod_root_body",
        mod_root_with("let a = 5;", ["let a = 1;"]),
        apply_at_mod_root(Replace("a:definition", "1")),
      )
    ),
    test_case(
      "insert after a member and a top-level binding",
      `Quick,
      () => {
        check_rendered(
          "mod_root_insert_after_member",
          mod_root_with("  let a = 1;", ["  let a = 1;", "  let z = 3;"]),
          apply_at_mod_root(Insert(After("M/a"), "let z = 3")),
        );
        check_rendered(
          "mod_root_insert_after_top",
          mod_root_with("let a = 5;", ["let a = 5;", "let z = 3;"]),
          apply_at_mod_root(Insert(After("a"), "let z = 3")),
        );
      },
    ),
    test_case("insert before the first top-level binding", `Quick, () =>
      check_rendered(
        "mod_root_insert_before_top",
        "let z = 3;\n" ++ mod_root_program,
        apply_at_mod_root(Insert(Before("M"), "let z = 3")),
      )
    ),
    test_case(
      "a test inserted among members, and last in the top level",
      `Quick,
      () => {
        check_rendered(
          "mod_root_insert_test_member",
          mod_root_with(
            "  let b = a + 1;",
            ["  let b = a + 1;", "  test b == 2 end;"],
          ),
          apply_at_mod_root(Insert(After("M/b"), "test b == 2 end")),
        );
        /* after the last binding, before the trailing expression */
        check_rendered(
          "mod_root_insert_test_no_path",
          mod_root_with(
            "let d : M.T = M.a + a;",
            ["let d : M.T = M.a + a;", "test d == 6 end;"],
          ),
          apply_at_mod_root(Insert(LastIn(""), "test d == 6 end")),
        );
      },
    ),
    test_case(
      "insert first and last in the top level place members",
      `Quick,
      () => {
        let boundary = (d, code) =>
          switch (
            run_insert_at_program_boundary(
              ~root=Mod,
              mod_root_program,
              d,
              code,
            )
          ) {
          | Ok(z) => render_zipper(z)
          | Error(err) =>
            Alcotest.fail(
              "boundary insert failed: " ++ Action.Failure.show(err),
            )
          };
        /* after the last member, before the trailing expression */
        check_rendered(
          "mod_root_boundary_after",
          mod_root_with(
            "let d : M.T = M.a + a;",
            ["let d : M.T = M.a + a;", "let z = 3;"],
          ),
          boundary(After(""), "let z = 3"),
        );
        check_rendered(
          "mod_root_boundary_after_several",
          mod_root_with(
            "let d : M.T = M.a + a;",
            ["let d : M.T = M.a + a;", "let y = 2;", "let z = y + 1;"],
          ),
          boundary(After(""), "let y = 2;\nlet z = y + 1;"),
        );
        check_rendered(
          "mod_root_boundary_before",
          "let z = 3;\n" ++ mod_root_program,
          boundary(Before(""), "let z = 3"),
        );
      },
    ),
    test_case(
      "delete on a member and a top level",
      `Quick,
      () => {
        /* both are used: a delete leaving uses dangling is refused */
        let refused = (path, at) =>
          switch (run_at_mod_root(mod_root_program, Delete(path))) {
          | Error(Action.Failure.Composition_action_failure(msg)) =>
            check(
              bool,
              "refused naming " ++ at ++ ": " ++ msg,
              true,
              StringUtil.match(StringUtil.regexp("used at " ++ at), msg),
            )
          | Ok(z) =>
            Alcotest.fail("deleted " ++ path ++ ": " ++ render_zipper(z))
          | Error(e) => Alcotest.fail(Action.Failure.show(e))
          };
        refused("M/b", "M/c");
        refused("a", "d");
        check_rendered(
          "mod_root_delete_member",
          "module M = {\n  let a = 1;\n  let c = a\n};\nM.c",
          apply_at_mod_root(
            ~code=
              "module M = {\n  let a = 1;\n  let b = a + 1;\n  let c = a\n};\nM.c",
            Delete("M/b"),
          ),
        );
      },
    ),
    test_case("rename on a member relabels M.a", `Quick, () =>
      check_rendered(
        "mod_root_rename_member",
        mod_root_program
        |> StringUtil.replace(
             StringUtil.regexp("let a = 1"),
             _,
             "let aa = 1",
           )
        |> StringUtil.replace(StringUtil.regexp("= a [+] 1"), _, "= aa + 1")
        |> StringUtil.replace(StringUtil.regexp("M[.]a"), _, "M.aa"),
        apply_at_mod_root(Rename("M/a", None, "aa")),
      )
    ),
    test_case("rename on a top-level binding leaves M.a", `Quick, () =>
      check_rendered(
        "mod_root_rename_top",
        mod_root_with("let a = 5;", ["let z = 5;"])
        |> StringUtil.replace(StringUtil.regexp("M[.]a [+] a"), _, "M.a + z"),
        apply_at_mod_root(Rename("a", None, "z")),
      )
    ),
  ],
);

/* rename of a module member: its `M.x` labels follow, and a name
   another member of the module has is refused */
let member_rename_tests = (
  "AgentTools.MemberRename",
  [
    test_case("a livelit's member", `Quick, () =>
      check_rendered_exact(
        "livelit_member_rename",
        "let ^pct = {\n  type T = Int;\n  let start = 50\n} in\n^pct(1)",
        apply_and_render(livelit_src, Rename("^pct/init", None, "start")),
      )
    ),
    /* statics reads no binding in `let ^pct`, so its uses are unknown */
    test_case("the livelit itself is refused", `Quick, () =>
      switch (run_agent_action(livelit_src, Rename("^pct", None, "^pc"))) {
      | Error(Action.Failure.Composition_action_failure(_)) => ()
      | Ok(z) => Alcotest.fail("renamed: " ++ render_zipper(z))
      | Error(e) => Alcotest.fail("unexpected: " ++ Action.Failure.show(e))
      }
    ),
    test_case("a nested member's Outer.Inner.x", `Quick, () =>
      check_rendered(
        "nested_member_rename",
        "module Outer = { module Inner = { let y = 1 }; let w = 1 } in Outer.Inner.y",
        apply_and_render(
          "module Outer = { module Inner = { let x = 1 }; let w = 1 } in Outer.Inner.x",
          Rename("Outer/Inner/x", None, "y"),
        ),
      )
    ),
    test_case("a let-bound module's m.x", `Quick, () =>
      check_rendered(
        "let_module_member_rename",
        "let m = { let z = 1; let y = 2 } in m.z + m.y",
        apply_and_render(
          "let m = { let x = 1; let y = 2 } in m.x + m.y",
          Rename("m/x", None, "z"),
        ),
      )
    ),
    test_case("a function member's M.f(…)", `Quick, () =>
      check_rendered(
        "fn_member_rename",
        "module M = { let g(n) = n + 1 } in M.g(2)",
        apply_and_render(
          "module M = { let f(n) = n + 1 } in M.f(2)",
          Rename("M/f", None, "g"),
        ),
      )
    ),
    test_case("another member's name is refused", `Quick, () =>
      switch (
        run_agent_action(
          "module M = { let x = 1; let y = 2 } in M.x + M.y",
          Rename("M/y", None, "x"),
        )
      ) {
      | Error(Action.Failure.Composition_action_failure(msg)) =>
        check(
          bool,
          "member_clash_refused",
          true,
          StringUtil.match(StringUtil.regexp("Not renamed to x"), msg),
        )
      | Ok(z) => Alcotest.fail("renamed: " ++ render_zipper(z))
      | Error(e) => Alcotest.fail("unexpected: " ++ Action.Failure.show(e))
      }
    ),
    test_case("a member delete leaves no lone `;`", `Quick, () =>
      check_rendered_exact(
        "member_delete_multiline",
        "let m = {\n  let x = 1;\n\n  let z = 3\n} in m",
        apply_and_render(
          "let m = {\n  let x = 1;\n  let y = 2;\n  let z = 3\n} in m",
          Delete("m/y"),
        ),
      )
    ),
  ],
);

/* tests: inserted after their function or after its last test; at the
   end, last in the top level, above the result */
let test_entry_tests = (
  "AgentTools.Tests",
  [
    test_case("after the binding at path", `Quick, () =>
      check_rendered_exact(
        "after_binding",
        "let a = 1 in\ntest a == 1 end;\n\nlet b = 2 in\na + b",
        apply_and_render(
          "let a = 1 in\nlet b = 2 in\na + b",
          Insert(After("a"), "test a == 1 end"),
        ),
      )
    ),
    test_case("after the last of its tests, in order", `Quick, () =>
      check_rendered_exact(
        "after_run",
        "let a = 1 in\ntest a == 1 end;\ntest a > 0 end;\ntest a < 2 end;\ntest a != 3 end;\n\nlet b = 2 in\nb",
        apply_chain_render(
          "let a = 1 in\ntest a == 1 end;\ntest a > 0 end;\nlet b = 2 in\nb",
          [
            Insert(After("test#2"), "test a < 2 end"),
            Insert(After("test#3"), "test a != 3 end"),
          ],
        ),
      )
    ),
    test_case("in a function body, one line each", `Quick, () =>
      check_rendered_exact(
        "fn_body",
        "let f = fun x ->\n  let y = x + 1 in\n  test y > x end;\n  test y != x end;\n  y * 2\nin\nf(1)",
        apply_chain_render(
          "let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nf(1)",
          [
            Insert(After("f/y"), "test y > x end"),
            Insert(After("f/test#1"), "test y != x end"),
          ],
        ),
      )
    ),
    test_case(
      "last in the top level: above the result",
      `Quick,
      () => {
        check_rendered_exact(
          "no_path",
          "let a = 1 in\n\ntest a == 1 end;\ntest a > 0 end;\n\na",
          apply_and_render(
            "let a = 1 in\n\ntest a == 1 end;\na",
            Insert(LastIn(""), "test a > 0 end"),
          ),
        );
        check_rendered_exact(
          "no_path_no_bindings",
          "test 1 + 1 == 2 end;\n\n1 + 1",
          apply_and_render(
            "1 + 1",
            Insert(LastIn(""), "test 1 + 1 == 2 end"),
          ),
        );
      },
    ),
    test_case(
      "among a module's members",
      `Quick,
      () => {
        check_rendered(
          "member",
          "let m = { let x = 1; test x == 1 end; test x > 0 end; let y = 2 } in m",
          apply_and_render(
            "let m = {\n  let x = 1;\n  test x == 1 end;\n  let y = 2\n} in m",
            Insert(After("m/test#1"), "test x > 0 end"),
          ),
        );
        check_rendered(
          "member_last",
          "let m = { let x = 1; test x == 1 end; test x > 0 end } in m",
          apply_and_render(
            "let m = { let x = 1; test x == 1 end } in m",
            Insert(LastIn("m"), "test x > 0 end"),
          ),
        );
        check_rendered(
          "member_after_leading_test",
          "let m = { test 1 == 1 end; let x = 1; test x > 0 end } in m",
          apply_and_render(
            "let m = { test 1 == 1 end; let x = 1 } in m",
            Insert(After("m/x"), "test x > 0 end"),
          ),
        );
      },
    ),
    test_case("a trailing `;` is dropped", `Quick, () =>
      check_rendered_exact(
        "wrapped",
        "let a = 1 in\ntest a == 1 end;\n\na",
        apply_and_render(
          "let a = 1 in\na",
          Insert(After("a"), "test a == 1 end;"),
        ),
      )
    ),
    test_case("a bad path errors with the paths there are", `Quick, () =>
      switch (
        run_agent_action(
          "let a = 1 in\na",
          Insert(After("nope"), "test a == 1 end"),
        )
      ) {
      | Ok(_) => Alcotest.fail("expected a bad path to be rejected")
      | Error(Action.Failure.Composition_action_failure(msg)) =>
        check(
          bool,
          "names the path and the available ones",
          true,
          StringUtil.match(StringUtil.regexp("No entry \"nope\""), msg)
          && StringUtil.match(StringUtil.regexp("Its entries: a"), msg),
        )
      | Error(e) =>
        Alcotest.fail("unexpected failure kind: " ++ Action.Failure.show(e))
      }
    ),
    test_case("a test that isn't a Bool is rejected", `Quick, () =>
      expect_composition_failure(
        "let a = 1 in\na",
        Insert(After("a"), "test a + 1 end"),
        "non-Bool test",
      )
    ),
  ],
);

let tests = [
  whitespace_normalization_tests,
  paste_funnel_tests,
  edit_action_tests,
  insert_at_program_boundary_tests,
  replace_definition_tests,
  body_part_tests,
  rename_tests,
  replace_entry_tests,
  module_member_tests,
  mod_root_tests,
  member_rename_tests,
  insert_tests,
  test_entry_tests,
  delete_tests,
  static_error_tests,
  nested_definition_tests,
  invalid_path_tests,
  high_level_node_map_tests,
  composition_view_print_tests,
  composition_utils_tests,
  statics_refractor_tests,
  agent_tools_with_projectors_tests,
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
