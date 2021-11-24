/**
 * The associated structure of a `Seq.t`.
 */
[@deriving sexp]
type t('op) =
  // `Placeholder(n)` refers to the `n`th operand in the corresponding seq
  | Placeholder(int)
  | BinOp(int, ErrStatus.t, 'op, t('op), t('op));

let leftmost_tm_index: t(_) => int;
let rightmost_tm_index: t(_) => int;

let repair_binop_numbering: (t('op), int) => t('op);

let mk: ('op => int, 'op => Associativity.t, Seq.t('operand, 'op)) => t('op);

let get_root_num: t(_) => int;
