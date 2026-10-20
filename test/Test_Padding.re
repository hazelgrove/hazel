open Alcotest;
open Haz3lcore;
open Language;

/* Syntax and statics of a program on the same ids, as the editor has them. */
let padding = (~pad_cols=0., program: string): Padding.t =>
  switch (Parser.to_zipper(~root=Exp, program)) {
  | None => fail("Failed to parse: " ++ program)
  | Some(z) =>
    let syntax = CachedSyntax.init(z);
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let statics =
      CachedStatics.init_from_term(
        ~settings=CoreSettings.on,
        ~is_dynamic_term=false,
        term,
      );
    Padding.of_blocks(
      ~pad_cols,
      ~measured=syntax.measured,
      ~term_data=syntax.term_data,
      statics.type_blocks,
    );
  };

let block_row = testable(Padding.pp_block_row, (==));
let gap = testable(Padding.pp_gap, (==));
let leading = testable(Padding.pp_leading, (==));

/* Ids are random, so a block is found by its level and head. */
let rows_of = (p: Padding.t, level: int, head: Typ.head) =>
  switch (
    List.find_opt(
      (b: Padding.block) =>
        b.block.level == level && Typ.equal_head(b.block.head, head),
      p.blocks,
    )
  ) {
  | Some(b) => b.rows
  | None => fail(Printf.sprintf("no block at level %d", level))
  };

let edge = (col, rank): Padding.edge => {
  col,
  rank,
};

let row = (row, left, right, rank): Padding.block_row => {
  row,
  left,
  right,
  rank,
};

let gap_ = (row, before, hi, demand): Padding.gap => {
  row,
  before,
  hi,
  demand,
};

let lead = (above, below, demand): Padding.leading => {
  above,
  below,
  demand,
};

/* [fun x -> x + 1]: List over Arrow over an Unknown pattern and an Int body */
let test_nested_ranks =
  test_case(
    "of_blocks: shared edges rank from the inside out",
    `Quick,
    () => {
      let p = padding("[fun x -> x + 1]");
      check(
        list(block_row),
        "list: three blocks deep on its row",
        [row(0, edge(0, 1), edge(16, 1), 3)],
        rows_of(p, 0, List),
      );
      check(
        list(block_row),
        "arrow: closes with the Int body at col 15",
        [row(0, edge(1, 1), edge(15, 2), 2)],
        rows_of(p, 1, Arrow),
      );
      check(
        list(block_row),
        "pattern x",
        [row(0, edge(5, 1), edge(6, 1), 1)],
        rows_of(p, 2, Unknown),
      );
      check(
        list(block_row),
        "body x + 1",
        [row(0, edge(10, 1), edge(15, 1), 1)],
        rows_of(p, 2, Int),
      );
    },
  );

let test_gaps =
  test_case(
    "of_blocks: a gap's demand counts the edges at its ends", `Quick, () =>
    check(
      list(gap),
      "[fun x -> x + 1]",
      [
        gap_(0, None, 0, 1),
        gap_(0, Some(1), 1, 1),
        gap_(0, Some(4), 5, 1),
        gap_(0, Some(6), 7, 1),
        gap_(0, Some(9), 10, 1),
        gap_(0, Some(15), 15, 2),
      ],
      padding("[fun x -> x + 1]").gaps,
    )
  );

let test_leading =
  test_case(
    "of_blocks: a bottom over a top asks for both their ranks",
    `Quick,
    () => {
      let p = padding("(1,\n true)");
      check(
        list(block_row),
        "tuple spans both rows, two deep on each; true shares its row-1 left edge",
        [
          row(0, edge(0, 1), edge(3, 1), 2),
          row(1, edge(1, 2), edge(6, 1), 2),
        ],
        rows_of(p, 0, Tuple),
      );
      check(list(leading), "between the rows", [lead(0, 1, 2)], p.leading);
    },
  );

/* `1`'s block ends on row 0 above the `p` on row 1, which starts no block
   of its own (p's type is the let's), yet is in the way. */
let test_leading_over_text =
  test_case(
    "of_blocks: a bottom over plain text asks for its own rank", `Quick, () =>
    check(
      list(leading),
      "let p = (1, true) in / p under the 1",
      [lead(0, 1, 1)],
      padding("let p = (1, true) in\n         p").leading,
    )
  );

/* The same with the `p` one column left of the `1`. */
let test_pads_stand_out =
  test_case(
    "of_blocks: an edge's pads stand out of its columns",
    `Quick,
    () => {
      let program = "let p = (1, true) in\n        p";
      check(list(leading), "a column apart", [], padding(program).leading);
      check(
        list(leading),
        "a pad wider than the column",
        [lead(0, 1, 1)],
        padding(~pad_cols=1.25, program).leading,
      );
    },
  );

/* The tuple opens at the end of row 0 and fills row 1, so its top on row 1
   runs under `w`, two deep against one. */
let test_widening =
  test_case(
    "of_blocks: a block widening on its next row has a top edge there",
    `Quick,
    () =>
    check(
      list(leading),
      "let w = ( / 1, 2) in 0",
      [lead(0, 1, 3)],
      padding("let w = (\n 1, 2) in 0").leading,
    )
  );

/* The list fills the end of row 0 and closes early on row 1, so its bottom
   on row 0 runs over the `+`, two deep. */
let test_narrowing =
  test_case(
    "of_blocks: a block narrowing on its next row has a bottom edge there",
    `Quick,
    () =>
    check(
      list(leading),
      "let y = [1, / 2] in 3 + 4",
      [lead(0, 1, 2)],
      padding("let y = [1,\n 2] in 3 + 4").leading,
    )
  );

let test_blank_row =
  test_case("of_blocks: edges face each other across a blank row", `Quick, () =>
    check(
      list(leading),
      "(1, / / 2): the tuple's halves, two deep each",
      [lead(0, 2, 4)],
      padding("(1,\n\n 2)").leading,
    )
  );

let tests = (
  "Padding",
  [
    test_nested_ranks,
    test_gaps,
    test_leading,
    test_leading_over_text,
    test_pads_stand_out,
    test_widening,
    test_narrowing,
    test_blank_row,
  ],
);
