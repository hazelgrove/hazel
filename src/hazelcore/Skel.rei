/**
 * The associated structure of a `Seq.t`.
 */
[@deriving (sexp, show)]
type t('op) =
  // `Placeholder(n)` refers to the `n`th operand in the corresponding seq
  | Placeholder(int)
  | BinOp(ErrStatus.t, 'op, t('op), t('op));

let leftmost_tm_index: t(_) => int;
let rightmost_tm_index: t(_) => int;

let mk: ('op => int, 'op => Associativity.t, Seq.t('operand, 'op)) => t('op);
