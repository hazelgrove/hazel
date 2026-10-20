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
   of block edges at its ends. A block's top or bottom edge on a row, less
   the block's own next row where it carries on, faces the nearest text or
   edge in its columns on the rows beyond, which may differ along it. The
   demand between a row and one it faces something on is the edge's rank,
   plus the deepest edge's it meets there. */

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

/* Between the text of rows `above` and `below`. */
[@deriving show({with_path: false})]
type leading = {
  above: int,
  below: int,
  demand: int,
};

/* Gaps and leading only where there is demand, in row then column order. */
[@deriving show({with_path: false})]
type t = {
  blocks: list(block),
  gaps: list(gap),
  leading: list(leading),
};

/* `pad_cols` is a pad in columns: how far an edge stands out of its block's
   columns per rank, so edges a row apart meet where their pads do. */
let of_blocks:
  (
    ~pad_cols: float,
    ~measured: Measured.t,
    ~term_data: TermData.t,
    list(Language.TypeBlocks.block)
  ) =>
  t;
