/**
 * The associated structure of a `Seq.t`.
 */
[@deriving sexp]
type t('op) =
  // `Placeholder(n)` refers to the `n`th operand in the corresponding seq
  | Placeholder(int)
  | BinOp(ErrStatus.t, 'op, t('op), t('op));

[@deriving sexp]
type range = (int, int);

let leftmost_op: t('op) => option('op);
let rightmost_op: t('op) => option('op);

let size: t(_) => int;

let leftmost_tm_index: t(_) => int;
let rightmost_tm_index: t(_) => int;

let range: t(_) => range;

/**
 * `subskel_rooted_at_op(n, skel)` returns the subskel rooted
 * at the operator with index `n`.
 */
let subskel_rooted_at_op: (OpIndex.t, t('op)) => t('op);

let mk_skel_str: (Seq.t('operand, 'op), 'op => string) => string;
