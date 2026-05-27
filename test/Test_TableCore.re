/* Tests for TableCore.parse_table.
 *
 * These exercise the full pipeline (parse -> elaborate -> parse_table) rather
 * than testing parse_table on hand-built syntax, because the shapes
 * parse_table needs to recognize (auto-labeled tuples, reordered labels,
 * rows wrapped in Asc by the elaborator's meet-type bridging) are produced
 * by elaboration. Pure-syntax tests for TableProj/transforms live in
 * Test_TableTransforms.re. */

open Alcotest;
open Haz3lcore;

let parse_exp = (s: string) =>
  switch (Parser.to_term(s, ~root=Language.Sort.Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };

let dhexp_of_uexp = u => {
  let (_, elab) =
    Language.Statics.mk(
      Language.CoreSettings.on,
      Language.Builtins.ctx_init(Some(Int)),
      u,
    );
  elab;
};

/* Find the first ListLit sub-expression in an elaborated program */
let find_list_lit = (exp: Language.Exp.t): option(Language.Exp.t) => {
  module M = {
    exception Found(Language.Exp.t);
  };
  switch (
    Language.Exp.map_term(
      ~f_exp=
        (cont, e) =>
          switch (e.term) {
          | ListLit(_) => raise(M.Found(e))
          | _ => cont(e)
          },
      exp,
    )
  ) {
  | exception (M.Found(x)) => Some(x)
  | _ => None
  };
};

let elaborate_and_find_list = (program_str: string): Language.Exp.t => {
  let uexp = parse_exp(program_str);
  let elaborated = dhexp_of_uexp(uexp);
  switch (find_list_lit(elaborated)) {
  | Some(e) => e
  | None => Alcotest.fail("No ListLit found in elaborated program")
  };
};

let assert_table_headers =
    (msg: string, expected_headers: list(string), exp: Language.Exp.t) =>
  switch (TableCore.parse_table(exp)) {
  | Some((headers, _rows)) =>
    let header_strs = List.filter_map(Fun.id, headers);
    Alcotest.(check(list(string), msg, expected_headers, header_strs));
  | None => Alcotest.fail(msg ++ ": parse_table returned None")
  };

let assert_table_shape =
    (
      msg: string,
      expected_headers: list(string),
      expected_row_count: int,
      exp: Language.Exp.t,
    ) => {
  assert_table_headers(msg ++ " headers", expected_headers, exp);
  switch (TableCore.parse_table(exp)) {
  | Some((_headers, rows)) =>
    Alcotest.(
      check(int, msg ++ " row count", expected_row_count, List.length(rows))
    )
  | None => Alcotest.fail(msg ++ ": parse_table returned None")
  };
};

let tests = (
  "TableCore.parse_table",
  [
    test_case(
      "Auto-labeled list: labels introduced from type",
      `Quick,
      () => {
        let exp =
          elaborate_and_find_list(
            {|type Row = (name=String, age=Int) in
              let t : [Row] = [("Alice", 12), ("Bob", 17)] in t|},
          );
        assert_table_shape("auto-labeled table", ["name", "age"], 2, exp);
      },
    ),
    test_case(
      "Reordered labels: elaboration reorders to match type",
      `Quick,
      () => {
        let exp =
          elaborate_and_find_list(
            {|type Row = (name=String, score=Int) in
              let t : [Row] = [(score=100, name="Alice")] in t|},
          );
        assert_table_headers(
          "reordered labels match type order",
          ["name", "score"],
          exp,
        );
      },
    ),
    test_case(
      "Mixed labels: some explicit, some auto",
      `Quick,
      () => {
        let exp =
          elaborate_and_find_list(
            {|type Row = (a=Int, b=String, c=Bool) in
              let t : [Row] = [(b="hi", 1, true)] in t|},
          );
        assert_table_headers("mixed labels resolved", ["a", "b", "c"], exp);
      },
    ),
    test_case(
      "Type alias: labels resolve through alias",
      `Quick,
      () => {
        let exp =
          elaborate_and_find_list(
            {|type GradebookEntry = (name=String, age=Int, quiz1=Int, quiz2=Int, final=Int) in
              let gradebook : [GradebookEntry] = [
                ("Alice", 12, 8, 9, 87),
                ("Bob", 17, 6, 8, 85)
              ] in gradebook|},
          );
        assert_table_shape(
          "gradebook with type alias",
          ["name", "age", "quiz1", "quiz2", "final"],
          2,
          exp,
        );
      },
    ),
    test_case(
      "Row with empty hole in one column still parses as table",
      `Quick,
      () => {
        /* Elaboration wraps the hole row in Asc(Parens(Tuple), Prod) because
         * its syn type differs from the list's meet type. normalize_row in
         * TableCore must push that ascription through so the row's labeled
         * structure is recognizable. */
        let exp =
          elaborate_and_find_list(
            {|type Row = (name=String, age=Int, score=Int) in
              let t : [Row] = [
                ("Alice", 12, ?),
                ("Bob", 17, 9)
              ] in t|},
          );
        assert_table_shape(
          "row with hole",
          ["name", "age", "score"],
          2,
          exp,
        );
      },
    ),
  ],
);
