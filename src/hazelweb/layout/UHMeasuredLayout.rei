open Pretty;

type t = MeasuredLayout.t(UHAnnot.t);

let mk: Layout.t(UHAnnot.t) => t;

let caret_position_of_path:
  (CursorPath_common.t, t) => option(MeasuredPosition.t);

type path_position = (CursorPath_common.rev_t, MeasuredPosition.t);

/**
 * `first_path_in_row(r, m)` returns the first path encountered
 * during left-to-right traversal of row `r` of `m`
 */
let first_path_in_row:
  (
    ~rev_steps: CursorPath_common.rev_steps=?,
    ~indent: int=?,
    ~start: MeasuredPosition.t=?,
    int,
    t
  ) =>
  option(path_position);

/**
 * `last_path_in_row(r, m)` returns the first path encountered
 * during right-to-left traversal of row `r` of `m`
 */
let last_path_in_row:
  (
    ~rev_steps: CursorPath_common.rev_steps=?,
    ~start: MeasuredPosition.t=?,
    int,
    t
  ) =>
  option(path_position);

/**
 * `prev_path_within_row(from, m)` returns the next encountered path
 * during in right-to-left traversal of row `from.row` starting at
 * (but not including) `from.col`
 */
let prev_path_within_row: (MeasuredPosition.t, t) => option(path_position);

/**
 * `next_path_within_row(from, m)` returns the next encountered path
 * during left-to-right traversal of row `from.row` starting at
 * (but not including) `from.col`
 */
let next_path_within_row: (MeasuredPosition.t, t) => option(path_position);

/**
 * `nearest_path_within_row(from, m)` returns the path nearest to
 * `from` (including any path positioned at `from`)
 */
let nearest_path_within_row: (MeasuredPosition.t, t) => option(path_position);
