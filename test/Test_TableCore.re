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

/* --- Splice-cell tests --- */

let parse_segment_exn = (s: string): Segment.t =>
  switch (Parser.to_segment(s, ~root=Language.Sort.Exp)) {
  | Some(seg) => seg
  | None => Alcotest.fail("Failed to parse segment: " ++ s)
  };

let splice_of_segment_exn = (seg: Segment.t): Segment.t =>
  switch (TableCore.splice_table_cells(seg)) {
  | Some(spliced) => spliced
  | None => Alcotest.fail("splice_table_cells declined")
  };

let seg_text = (seg: Segment.t): string =>
  Printer.of_segment(~holes="?", ~concave_holes="~", seg);

let splice_texts = (seg: Segment.t): list(string) =>
  Segment.direct_splices(seg)
  |> List.map((s: Haz3lcore.Base.splice) => seg_text(s.content));

/* Wrap the cells of the first list literal nested anywhere in the
 * segment (test surrogate for projecting a table inside a program). */
let rec splice_first_list = (seg: Segment.t): Segment.t =>
  List.map(
    (p: Haz3lcore.Base.piece) =>
      switch (p) {
      | Tile({label: ["[", "]"], _}) => (
          switch (TableCore.splice_table_cells([p])) {
          | Some([spliced]) => spliced
          | _ => p
          }: Haz3lcore.Base.piece
        )
      | Tile(t) =>
        Tile({
          ...t,
          children: List.map(splice_first_list, t.children),
        })
      | p => p
      },
    seg,
  );

let splice_tests = [
  test_case(
    "splice_table_cells wraps each unlabeled cell value",
    `Quick,
    () => {
      let seg = parse_segment_exn({|[("Alice", 12), ("Bob", 17)]|});
      let spliced = splice_of_segment_exn(seg);
      Alcotest.(
        check(
          list(string),
          "one splice per cell",
          [{|"Alice"|}, "12", {|"Bob"|}, "17"],
          splice_texts(spliced),
        )
      );
      Alcotest.(
        check(
          string,
          "printed text unchanged (splices are transparent)",
          seg_text(seg),
          seg_text(spliced),
        )
      );
    },
  ),
  test_case(
    "splice_table_cells leaves label prefixes outside the splices",
    `Quick,
    () => {
      let seg = parse_segment_exn({|[(name="Alice", age=12)]|});
      let spliced = splice_of_segment_exn(seg);
      Alcotest.(
        check(
          list(string),
          "splices wrap only the values",
          [{|"Alice"|}, "12"],
          splice_texts(spliced),
        )
      );
      /* The spliced syntax still parses as a labeled table, with each
       * cell mapped to its splice. */
      switch (MakeTerm.for_projection(spliced)) {
      | Some(Exp(exp)) =>
        switch (TableCore.parse_table(exp)) {
        | Some((headers, [row])) =>
          Alcotest.(
            check(
              list(option(string)),
              "headers",
              [Some("name"), Some("age")],
              headers,
            )
          );
          Alcotest.(
            check(
              int,
              "each cell carries a splice",
              2,
              row |> List.filter_map(TableCore.first_splice_id) |> List.length,
            )
          );
        | _ => Alcotest.fail("spliced syntax no longer parses as a table")
        }
      | _ => Alcotest.fail("spliced syntax no longer parses as a term")
      };
    },
  ),
  test_case(
    "splice_table_cells is idempotent",
    `Quick,
    () => {
      let seg = parse_segment_exn({|[(name="Alice", age=12)]|});
      let spliced = splice_of_segment_exn(seg);
      let respliced = splice_of_segment_exn(spliced);
      let ids = (seg: Segment.t) =>
        Segment.direct_splices(seg)
        |> List.map((s: Haz3lcore.Base.splice) => s.id);
      Alcotest.(
        check(
          bool,
          "re-running adds no splices and keeps ids",
          true,
          ids(spliced) == ids(respliced),
        )
      );
    },
  ),
  test_case(
    "elaboration carries cell splices through auto-labeling",
    `Quick,
    () => {
      let seg =
        parse_segment_exn(
          {|type Row = (name=String, age=Int) in
            let t : [Row] = [("Alice", 12), ("Bob", 17)] in t|},
        )
        |> splice_first_list;
      let exp =
        switch (MakeTerm.for_projection(seg)) {
        | Some(Exp(exp)) => exp
        | _ => Alcotest.fail("program did not parse as an expression")
        };
      let elaborated = dhexp_of_uexp(exp);
      let list_exp =
        switch (find_list_lit(elaborated)) {
        | Some(e) => e
        | None => Alcotest.fail("no ListLit in elaborated program")
        };
      assert_table_shape(
        "auto-labeled spliced table",
        ["name", "age"],
        2,
        list_exp,
      );
      switch (TableCore.parse_table(list_exp)) {
      | Some((_, rows)) =>
        Alcotest.(
          check(
            int,
            "every elaborated cell carries a splice id",
            4,
            rows
            |> List.concat
            |> List.filter_map(TableCore.first_splice_id)
            |> List.length,
          )
        )
      | None => Alcotest.fail("parse_table failed on spliced elaboration")
      };
    },
  ),
  test_case(
    "cell splices follow elaboration's label reordering",
    `Quick,
    () => {
      let seg =
        parse_segment_exn(
          {|type Row = (name=String, score=Int) in
            let t : [Row] = [(score=100, name="Alice")] in t|},
        )
        |> splice_first_list;
      /* Splices in source order: 100 first, then "Alice". */
      let by_content =
        Segment.direct_splices(seg)
        |> List.map((s: Haz3lcore.Base.splice) =>
             (seg_text(s.content), s.id)
           );
      let exp =
        switch (MakeTerm.for_projection(seg)) {
        | Some(Exp(exp)) => exp
        | _ => Alcotest.fail("program did not parse as an expression")
        };
      let elaborated = dhexp_of_uexp(exp);
      let list_exp =
        switch (find_list_lit(elaborated)) {
        | Some(e) => e
        | None => Alcotest.fail("no ListLit in elaborated program")
        };
      switch (TableCore.parse_table(list_exp)) {
      | Some((headers, [row])) =>
        let header_strs = List.filter_map(Fun.id, headers);
        Alcotest.(
          check(
            list(string),
            "headers reordered to type order",
            ["name", "score"],
            header_strs,
          )
        );
        let cell_ids = List.map(TableCore.first_splice_id, row);
        Alcotest.(
          check(
            list(option(string)),
            "cells map to the splices of their (reordered) values",
            [Some({|"Alice"|}), Some("100")],
            List.map(
              id =>
                Option.bind(id, id =>
                  by_content
                  |> List.find_opt(((_, sid)) => sid == id)
                  |> Option.map(fst)
                ),
              cell_ids,
            ),
          )
        );
      | _ => Alcotest.fail("parse_table failed on spliced elaboration")
      };
    },
  ),
];

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
  ]
  @ splice_tests,
);
