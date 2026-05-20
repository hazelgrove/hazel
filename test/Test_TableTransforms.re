open Alcotest;
open Haz3lcore;
open Language;

let testable_exp =
  testable(
    Fmt.using(Exp.show, Fmt.string),
    Equality.(
      equality({
        ...syntactic_settings,
        ignore_parens: true,
        ignore_function_names: true,
      }).
        exp
    ),
  );

/* Stand-in base expression: transforms are applied to this */
let table = IdTagged.FreshGrammar.Exp.var("table");

let assert_transforms = (transforms, expected_str) => {
  let result = TableTransforms.apply_transforms(table, transforms);
  let expected =
    switch (Parser.to_term(expected_str, ~root=Sort.Exp)) {
    | Some(exp) => exp
    | None => fail("Failed to parse expected: " ++ expected_str)
    };
  check(testable_exp, expected_str, expected, result);
};

let mk_table = (rows: list(list((string, Exp.t)))): Exp.t =>
  IdTagged.FreshGrammar.Exp.(
    list_lit(
      List.map(
        fields =>
          parens(
            tuple(List.map(((l, v)) => tup_label(label(l), v), fields)),
          ),
        rows,
      ),
    )
  );

/* --- parse_table tests --- */

let parse_table_tests = {
  module G = IdTagged.FreshGrammar.Exp;
  [
    test_case(
      "parse_table: simple 2-column table",
      `Quick,
      () => {
        let tbl =
          mk_table([
            [("x", G.int(1)), ("y", G.int(2))],
            [("x", G.int(3)), ("y", G.int(4))],
          ]);
        switch (TableCore.parse_table(tbl)) {
        | Some((headers, rows)) =>
          check(
            list(option(string)),
            "headers",
            [Some("x"), Some("y")],
            headers,
          );
          check(int, "row count", 2, List.length(rows));
          check(int, "col count row 0", 2, List.length(List.nth(rows, 0)));
          check(int, "col count row 1", 2, List.length(List.nth(rows, 1)));
        | None => fail("Expected Some table data")
        };
      },
    ),
    test_case(
      "parse_table: empty list returns None",
      `Quick,
      () => {
        let tbl = G.list_lit([]);
        check(
          bool,
          "empty list",
          true,
          Option.is_none(TableCore.parse_table(tbl)),
        );
      },
    ),
    test_case(
      "parse_table: non-list returns None",
      `Quick,
      () => {
        let not_a_table = G.int(42);
        check(
          bool,
          "int",
          true,
          Option.is_none(TableCore.parse_table(not_a_table)),
        );
      },
    ),
    test_case(
      "parse_table: mismatched headers returns None",
      `Quick,
      () => {
        let tbl =
          mk_table([
            [("x", G.int(1)), ("y", G.int(2))],
            [("a", G.int(3)), ("b", G.int(4))],
          ]);
        check(
          bool,
          "mismatched",
          true,
          Option.is_none(TableCore.parse_table(tbl)),
        );
      },
    ),
    test_case(
      "parse_table: single row table",
      `Quick,
      () => {
        let tbl =
          mk_table([[("name", G.string("Alice")), ("age", G.int(30))]]);
        switch (TableCore.parse_table(tbl)) {
        | Some((headers, rows)) =>
          check(
            list(option(string)),
            "headers",
            [Some("name"), Some("age")],
            headers,
          );
          check(int, "row count", 1, List.length(rows));
        | None => fail("Expected Some table data")
        };
      },
    ),
  ];
};

/* --- Transform tests: apply to `table` and compare result expressions --- */

let transform_tests = [
  test_case("drop_column", `Quick, () =>
    assert_transforms(
      [TableTransforms.drop_column("col")],
      {|table |> map(_, omit_labels(_, `col`))|},
    )
  ),
  test_case("rename_column", `Quick, () =>
    assert_transforms(
      [TableTransforms.rename_column("old_col", "new_col")],
      {|table |> map(_, fun r -> (omit_labels(r, `old_col`)) ... (new_col=r.old_col))|},
    )
  ),
  test_case("add_column", `Quick, () =>
    assert_transforms(
      [TableTransforms.add_column()],
      {|table |> map(_, fun r -> r ... (?=?))|},
    )
  ),
  test_case("convert_column", `Quick, () =>
    assert_transforms(
      [TableTransforms.convert_column("price", "string_of_int")],
      {|table |> map(_, fun r -> r ... (price=string_of_int(r.price)))|},
    )
  ),
  test_case("clear_column", `Quick, () =>
    assert_transforms(
      [TableTransforms.clear_column("col")],
      {|table |> map(_, fun r -> r ... (col=?))|},
    )
  ),
  test_case("noop_column", `Quick, () =>
    assert_transforms(
      [TableTransforms.noop_column("col")],
      {|table |> map(_, fun r -> r ... (col=r.col))|},
    )
  ),
  test_case("group_by_column", `Quick, () =>
    assert_transforms(
      [TableTransforms.group_by_column("category")],
      {|table |> group_on_key(_, fun row -> row.category)|},
    )
  ),
  test_case("filter_by_column (equals)", `Quick, () =>
    assert_transforms(
      [TableTransforms.filter_by_column(Poly(Equals), "score")],
      {|table |> filter(_, fun row -> row.score == ?)|},
    )
  ),
  test_case("custom_filter (open predicate)", `Quick, () =>
    assert_transforms(
      [TableTransforms.custom_filter()],
      {|table |> filter(_, fun row -> ?)|},
    )
  ),
  test_case("string_match_filter", `Quick, () =>
    assert_transforms(
      [TableTransforms.string_match_filter("name")],
      {|table |> filter(_, fun row -> string_match(?, row.name))|},
    )
  ),
  test_case("drop_nones_column", `Quick, () =>
    assert_transforms(
      [TableTransforms.drop_nones_column("optional_col")],
      {|table |> filter_map(_, fun row -> option_map(row.optional_col, fun v -> row ... (optional_col=v)))|},
    )
  ),
  test_case("provide_default_column", `Quick, () =>
    assert_transforms(
      [TableTransforms.provide_default_column("opt")],
      {|table |> map(_, fun row -> row ... (opt=case row.opt | None => ?| Some(v) => v end))|},
    )
  ),
];

/* --- sort_column tests --- */

let sort_tests = [
  test_case(
    "sort_column: Int ascending",
    `Quick,
    () => {
      let int_ty = Typ.fresh(Atom(Int));
      switch (TableTransforms.sort_column(Some(int_ty), "age", false)) {
      | Some(transforms) =>
        assert_transforms(
          transforms,
          {|table |> sort(fun (r1, r2) -> int_compare(r1.age, r2.age), _)|},
        )
      | None => fail("Expected Some transforms for Int sort")
      };
    },
  ),
  test_case(
    "sort_column: Int descending wraps comparator in invert_ord",
    `Quick,
    () => {
      let int_ty = Typ.fresh(Atom(Int));
      switch (TableTransforms.sort_column(Some(int_ty), "age", true)) {
      | Some(transforms) =>
        assert_transforms(
          transforms,
          {|table |> sort(fun (r1, r2) -> invert_ord(int_compare(r1.age, r2.age)), _)|},
        )
      | None => fail("Expected Some transforms for descending sort")
      };
    },
  ),
  test_case(
    "sort_column: Float ascending",
    `Quick,
    () => {
      let float_ty = Typ.fresh(Atom(Float));
      switch (TableTransforms.sort_column(Some(float_ty), "price", false)) {
      | Some(transforms) =>
        assert_transforms(
          transforms,
          {|table |> sort(fun (r1, r2) -> float_compare(r1.price, r2.price), _)|},
        )
      | None => fail("Expected Some transforms for Float sort")
      };
    },
  ),
  test_case(
    "sort_column: Bool returns None",
    `Quick,
    () => {
      let bool_ty = Typ.fresh(Atom(Bool));
      check(
        bool,
        "no sort for Bool",
        true,
        Option.is_none(
          TableTransforms.sort_column(Some(bool_ty), "flag", false),
        ),
      );
    },
  ),
  test_case("sort_column: None type returns None", `Quick, () =>
    check(
      bool,
      "no sort without type",
      true,
      Option.is_none(TableTransforms.sort_column(None, "col", false)),
    )
  ),
];

/* --- Type utility tests --- */

let mk_list_ty = (cols: list((string, Typ.t))) =>
  IdTagged.FreshGrammar.Typ.(
    list(prod(List.map(((l, t)) => tup_label(label(l), t), cols)))
  );

let type_utility_tests = {
  module T = IdTagged.FreshGrammar.Typ;
  [
    test_case(
      "get_column_type_from_ty: finds column type",
      `Quick,
      () => {
        let ty = mk_list_ty([("x", T.int()), ("y", T.float())]);
        switch (TableTransforms.get_column_type_from_ty(ty, "y")) {
        | Some(col_ty) =>
          check(
            bool,
            "is Float",
            true,
            switch (col_ty.term) {
            | Atom(Float) => true
            | _ => false
            },
          )
        | None => fail("Expected Some column type")
        };
      },
    ),
    test_case(
      "get_column_type_from_ty: missing column returns None",
      `Quick,
      () => {
        let ty = mk_list_ty([("x", T.int())]);
        check(
          bool,
          "missing col",
          true,
          Option.is_none(TableTransforms.get_column_type_from_ty(ty, "z")),
        );
      },
    ),
    test_case(
      "get_column_type_from_ty: non-list type returns None",
      `Quick,
      () => {
        let ty = Typ.fresh(Atom(Int));
        check(
          bool,
          "non-list",
          true,
          Option.is_none(TableTransforms.get_column_type_from_ty(ty, "x")),
        );
      },
    ),
    test_case(
      "get_columns: extracts column names",
      `Quick,
      () => {
        let ty =
          mk_list_ty([("a", T.int()), ("b", T.string()), ("c", T.bool())]);
        switch (TableTransforms.get_columns(ty)) {
        | Some(cols) =>
          check(list(string), "column names", ["a", "b", "c"], cols)
        | None => fail("Expected Some columns")
        };
      },
    ),
    test_case(
      "get_columns: non-list returns None",
      `Quick,
      () => {
        let ty = Typ.fresh(Atom(Int));
        check(
          bool,
          "non-list",
          true,
          Option.is_none(TableTransforms.get_columns(ty)),
        );
      },
    ),
    test_case(
      "can_move_column: boundary checks",
      `Quick,
      () => {
        let cols = Some(["a", "b", "c"]);
        check(
          bool,
          "a cannot move left",
          false,
          TableTransforms.can_move_column(cols, "a", true),
        );
        check(
          bool,
          "a can move right",
          true,
          TableTransforms.can_move_column(cols, "a", false),
        );
        check(
          bool,
          "c can move left",
          true,
          TableTransforms.can_move_column(cols, "c", true),
        );
        check(
          bool,
          "c cannot move right",
          false,
          TableTransforms.can_move_column(cols, "c", false),
        );
        check(
          bool,
          "b can move left",
          true,
          TableTransforms.can_move_column(cols, "b", true),
        );
        check(
          bool,
          "b can move right",
          true,
          TableTransforms.can_move_column(cols, "b", false),
        );
        check(
          bool,
          "unknown col cannot move",
          false,
          TableTransforms.can_move_column(cols, "z", true),
        );
        check(
          bool,
          "None columns cannot move",
          false,
          TableTransforms.can_move_column(None, "a", true),
        );
      },
    ),
    test_case(
      "move_column: swaps adjacent columns",
      `Quick,
      () => {
        let ty =
          mk_list_ty([
            ("a", T.int()),
            ("b", T.float()),
            ("c", T.string()),
          ]);
        switch (TableTransforms.move_column(Some(ty), "b", true)) {
        | Some(t) =>
          assert_transforms(
            [t],
            {|table |> map(_, select_labels(_, `b`, `a`, `c`))|},
          )
        | None => fail("Expected Some transform for move")
        };
      },
    ),
    test_case(
      "move_column: out of bounds returns None",
      `Quick,
      () => {
        let ty = mk_list_ty([("a", T.int()), ("b", T.float())]);
        check(
          bool,
          "a cannot move left",
          true,
          Option.is_none(TableTransforms.move_column(Some(ty), "a", true)),
        );
        check(
          bool,
          "b cannot move right",
          true,
          Option.is_none(TableTransforms.move_column(Some(ty), "b", false)),
        );
      },
    ),
  ];
};

/* --- TableProj.error tests --- */

let mk_info = (elaborated: option(Exp.t)): ProjectorBase.info => {
  id: Id.invalid,
  syntax: [],
  statics: None,
  dynamics: None,
  elaborated,
  utility: ProjectorInfo.utility,
};

let table_proj_error_tests = {
  module G = IdTagged.FreshGrammar.Exp;
  let good_table = mk_table([[("x", G.int(1))], [("x", G.int(2))]]);
  let model =
    switch (TableProj.M.init(Exp(good_table))) {
    | Some(m) => m
    | None => fail("TableProj.init should succeed on a valid table")
    };
  [
    test_case(
      "TableProj.error: None when elaborated is a valid table", `Quick, () =>
      check(
        bool,
        "no error",
        true,
        Option.is_none(
          TableProj.M.error(model, mk_info(Some(good_table))),
        ),
      )
    ),
    test_case(
      "TableProj.error: Some when elaborated is a non-table", `Quick, () =>
      switch (TableProj.M.error(model, mk_info(Some(G.int(42))))) {
      | Some(err) =>
        check(
          bool,
          "error has non-empty message",
          true,
          String.length(err.message) > 0,
        )
      | None => fail("Expected error for non-table elaborated form")
      }
    ),
    test_case(
      "TableProj.error: Some when elaborated is a list of unlabeled tuples",
      `Quick,
      () => {
        /* Without labels the list fails parse_table (headers all None) */
        let unlabeled =
          G.list_lit([G.parens(G.tuple([G.int(1), G.int(2)]))]);
        check(
          bool,
          "has error",
          true,
          Option.is_some(
            TableProj.M.error(model, mk_info(Some(unlabeled))),
          ),
        );
      },
    ),
  ];
};

/* --- packed_renderer round-trip tests --- */

let packed_renderer_tests = {
  module G = IdTagged.FreshGrammar.Exp;
  let renderer = RichProbe.pack_renderer((module TableRenderer), "table");
  let good_table = mk_table([[("x", G.int(1))]]);
  /* Round-trip a packed_model body through the renderer's sexp serializer
     to get back a typed TableRenderer.model. */
  let extract_model = (pm: RichProbe.packed_model): TableRenderer.model =>
    pm |> renderer.sexp_of_model_payload |> TableRenderer.model_of_sexp;
  /* Build a packed_action targeting this renderer from a typed action. */
  let pack_action = (a: TableRenderer.action): RichProbe.packed_action =>
    a |> TableRenderer.sexp_of_action |> renderer.action_payload_of_sexp;
  [
    test_case("packed_renderer: id is preserved", `Quick, () =>
      check(string, "id", "table", renderer.id)
    ),
    test_case(
      "packed_renderer: can_handle accepts a labeled table", `Quick, () =>
      check(bool, "accepts", true, renderer.can_handle(Sort.Exp, good_table))
    ),
    test_case("packed_renderer: can_handle rejects a non-table", `Quick, () =>
      check(
        bool,
        "rejects",
        false,
        renderer.can_handle(Sort.Exp, G.int(42)),
      )
    ),
    test_case(
      "packed_renderer: ShowMenu then CloseMenu round-trip",
      `Quick,
      () => {
        let m0 =
          switch (renderer.init_model(Sort.Exp, good_table)) {
          | Some(pm) => pm
          | None => fail("init_model should succeed on a valid table")
          };
        check(
          bool,
          "initial menu_state is None",
          true,
          extract_model(m0).menu_state == None,
        );
        let m1 = renderer.update_model(m0, pack_action(ShowMenu(1)));
        switch (extract_model(m1).menu_state) {
        | Some((1, Some({selected_idx: 0, path: []}))) => ()
        | _ =>
          fail(
            "Expected menu_state = Some((1, Menu.opened)) after ShowMenu(1)",
          )
        };
        let m2 = renderer.update_model(m1, pack_action(CloseMenu));
        check(
          bool,
          "menu_state is None after CloseMenu",
          true,
          extract_model(m2).menu_state == None,
        );
      },
    ),
  ];
};

let tests = (
  "TableTransforms",
  parse_table_tests
  @ transform_tests
  @ sort_tests
  @ type_utility_tests
  @ table_proj_error_tests
  @ packed_renderer_tests,
);
