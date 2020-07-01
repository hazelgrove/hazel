[@deriving sexp]
type t('op) =
  | Placeholder(int)
  | BinOp(ErrStatus.t, 'op, t('op), t('op));

[@deriving sexp]
type range = (int, int);

let leftmost_op: t('a) => option('a);

let rightmost_op: t('a) => option('a);

let size: t('a) => int;

let leftmost_tm_index: t('a) => int;

let rightmost_tm_index: t('a) => int;

let range: t('a) => range;

// return bool is for internal use,
// indicates whether the accompanying
// skel is the target subskel
let _subskel_rooted_at_op: (OpIndex.t, t('op)) => (bool, t('op));

let subskel_rooted_at_op: (OpIndex.t, t('op)) => t('op);

let range_of_subskel_rooted_at_op: (OpIndex.t, t('a)) => range;

let mk_skel_str':
  (
    'op => string,
    Seq.t('operand, 'op),
    ref(int),
    Sexplib.Std.Hashtbl.t(int, 'operand)
  ) =>
  string;

let mk_skel_str: (Seq.t('operand, 'op), 'op => string) => string;
