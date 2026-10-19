open Alcotest;
open Haz3lcore;
open Language;

/* Syntax and statics of a program on the same ids, as the editor has them. */
let padding = (program: string): Padding.t =>
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
      check(
        list(leading),
        "between the rows",
        [
          {
            above: 0,
            demand: 2,
          },
        ],
        p.leading,
      );
    },
  );

let tests = ("Padding", [test_nested_ranks, test_gaps, test_leading]);
