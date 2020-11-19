open Pretty;

[@deriving sexp]
type t = MeasuredLayout.t(UHAnnot.t);
type with_splices = (t, SpliceMap.t(t));
type with_offset = MeasuredLayout.with_offset(UHAnnot.t);

let mk: Layout.t(UHAnnot.t) => t;

let caret_position_of_path:
  (CursorPath.t, with_splices) =>
  option((MeasuredPosition.t, option((MetaVar.t, SpliceName.t))));

type path_position = (CursorPath.rev_t, MeasuredPosition.t);

/**
 * `first_path_in_row(r, m)` returns the first path encountered
 * during left-to-right traversal of row `r` of `m`. Returns `None`
 * if row `r` is outside of `m`.
 */
let first_path_in_row:
  (
    ~rev_steps: CursorPath.rev_steps=?,
    ~indent: int=?,
    ~start: MeasuredPosition.t=?,
    int,
    t
  ) =>
  option(path_position);

/**
 * `last_path_in_row(r, m)` returns the first path encountered
 * during right-to-left traversal of row `r` of `m`. Returns `None`
 * if row `r` is outside of `m`.
 */
let last_path_in_row:
  (
    ~rev_steps: CursorPath.rev_steps=?,
    ~indent: int=?,
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
