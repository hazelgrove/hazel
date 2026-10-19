/* The room a set of nested blocks asks of the layout, in units of one pad.

   A block (TypeBlocks) occupies a run of columns on each of its rows.
   Blocks nested inside one another share edges, so an edge has a rank: how
   many edges, this block's own included, meet at that boundary from this
   block inward. The outermost of three blocks closing at one column has
   rank 3 and is drawn three pads out. Likewise a block's box on a row
   stands as many pads above and below the row as blocks nest inside it
   there, its own included.

   A gap is the space between two neighboring fragments of text on a row,
   or between a row's start and its first fragment; its demand is the number
   of block edges at its ends. The demand between two rows is the most edges
   that have to fit between them at any column: the bottoms of blocks ending
   on the upper row and the tops of blocks starting on the lower one, each
   against the other row's text and against each other where they overlap. */

[@deriving show({with_path: false})]
type edge = {
  col: int,
  rank: int,
};

/* `rank` is the vertical one. */
[@deriving show({with_path: false})]
type block_row = {
  row: int,
  left: edge,
  right: edge,
  rank: int,
};

[@deriving show({with_path: false})]
type block = {
  block: Language.TypeBlocks.block,
  rows: list(block_row),
};

/* `before` is the column the preceding fragment ends at, None at a row's
   start; `hi` is where the next fragment begins. */
[@deriving show({with_path: false})]
type gap = {
  row: int,
  before: option(int),
  hi: int,
  demand: int,
};

[@deriving show({with_path: false})]
type leading = {
  above: int,
  demand: int,
};

/* Gaps and leading only where there is demand, in row then column order. */
[@deriving show({with_path: false})]
type t = {
  blocks: list(block),
  gaps: list(gap),
  leading: list(leading),
};

let of_blocks:
  (
    ~measured: Measured.t,
    ~term_data: TermData.t,
    list(Language.TypeBlocks.block)
  ) =>
  t;
